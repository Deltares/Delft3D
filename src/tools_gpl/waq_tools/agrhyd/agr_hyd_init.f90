!!  Copyright (C)  Stichting Deltares, 2021-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine agr_hyd_init(input_hyd, ipnt  , ipnt_h    , ipnt_q   , ipnt_vdf, &
                             ipnt_b   , ipnt_v, output_hyd, l_regular, l_expand, &
                             l_lenlen )

      ! function : initialise aggregation, time independent data

      ! global declarations

      use m_logger_helper, only : stop_with_error
      use m_hydmod
      use m_aggregate_waqgeom
      use aggregation, only : aggregate_extended, AGGREGATION_TYPE_ACCUMULATE, AGGREGATION_TYPE_WEIGHTED_AVERAGE

      implicit none

      ! declaration of the arguments

      type(t_hydrodynamics)          :: input_hyd                            ! description of the input hydrodynamics
      integer              :: ipnt(input_hyd%noseg)                ! aggregation pointer segments
      integer              :: ipnt_h(input_hyd%nmax,input_hyd%mmax)! aggregation pointer in the horizontal
      integer              :: ipnt_q(input_hyd%noq)                ! aggregation pointer exchanges
      integer              :: ipnt_vdf(*)                          ! aggregation pointer used for minimum vertical diffusion
      integer              :: ipnt_b(*)                            ! aggregation pointer for boundaries
      integer              :: ipnt_v(input_hyd%nolay)              ! vertical aggregation pointer
      type(t_hydrodynamics)          :: output_hyd                           ! description of the output hydrodynamics
      logical              :: l_regular                            ! regular aggregartion option
      logical              :: l_expand                             ! expand to full matrix
      logical              :: l_lenlen                             ! take length from length

      ! local declarations

      integer             :: m,n                    ! loop counter grid
      integer             :: m_new,n_new            ! indices on new grid
      integer             :: noq_new                ! new number of exchanges
      integer             :: ilay                   ! loop counter layers
      integer             :: newlay                 ! index layers
      integer             :: iseg, isegl            ! segment indices
      integer             :: iseg_new               ! segment indices
      integer             :: iwast                  ! index wasteloads
      integer             :: i_wasteload            ! index in collection
      type(t_wasteload)   :: wasteload              ! one wasteload description
      integer             :: no_sect                ! number of boundary sections
      integer             :: i_sect                 ! index of boudary section
      integer             :: iret                   ! index of added boudary section (for checking result)
      real                :: rdumar(1)              ! dummy array
      integer             :: ierr_alloc             !
      integer             :: ierror                 !
      logical             :: success
      integer, allocatable:: apnt(:)                ! general work array aggregation
      real, allocatable   :: rwork(:)               ! general work array aggregation
      character(len=255)  :: message                ! temporary variable for writing log messages.


      ! copy geometry type
      output_hyd%geometry = input_hyd%geometry

      ! lga, only in non-regular
      ! only in structured cases!!

      if (output_hyd%geometry .eq. HYD_GEOM_CURVI) then
         if ( .not. l_regular ) then
            output_hyd%mmax = input_hyd%mmax
            output_hyd%nmax = input_hyd%nmax
            output_hyd%kmax = input_hyd%kmax
            allocate(output_hyd%lgrid(output_hyd%nmax,output_hyd%mmax),stat=ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error allocating memory' ; call stop_with_error() ; endif
            do m = 1 , input_hyd%mmax
               do n = 1 , input_hyd%nmax
                  if ( input_hyd%lgrid(n,m) .eq. 0 ) then
                     output_hyd%lgrid(n,m) = 0
                  elseif ( input_hyd%lgrid(n,m) .lt. 0 ) then
                     output_hyd%lgrid(n,m) = -ipnt_b(-input_hyd%lgrid(n,m))
                  else
                     output_hyd%lgrid(n,m) = ipnt(input_hyd%lgrid(n,m))
                  endif
               enddo
            enddo
            output_hyd%nolay = maxval(ipnt_v)
            if ( .not. l_expand ) then
               output_hyd%nosegl= maxval(ipnt(1:input_hyd%nosegl))
               output_hyd%noseg = maxval(ipnt)
            else
               output_hyd%nosegl= output_hyd%mmax*output_hyd%nmax
               output_hyd%noseg = output_hyd%nosegl*output_hyd%nolay
            endif
         else
            output_hyd%kmax  = input_hyd%kmax
            output_hyd%nolay = maxval(ipnt_v)
            output_hyd%noseg = output_hyd%nolay*output_hyd%nosegl
         endif
      end if

      ! only in unstructured cases!!
      if (output_hyd%geometry .eq. HYD_GEOM_UNSTRUC) then
          call realloc (apnt, size(ipnt_h, 2))
          apnt = ipnt_h(1,:)
          success = aggregate_ugrid_geometry(input_hyd%waqgeom,output_hyd%waqgeom,input_hyd%edge_type,output_hyd%edge_type,apnt)
          if ( .not. success ) then
             write(message, *) 'There was and error when aggregating the grid! agrhyd will stop.'
             call mess(LEVEL_ERROR, trim(message))
             call stop_with_error()
          endif
          output_hyd%kmax  = input_hyd%kmax
          output_hyd%nolay = maxval(ipnt_v)
          output_hyd%nosegl= maxval(ipnt_h)
          output_hyd%noseg = output_hyd%nolay*output_hyd%nosegl
          output_hyd%crs = input_hyd%crs
          output_hyd%openbndsect_coll%current_size = 0
          output_hyd%openbndsect_coll%maxsize = 0
          output_hyd%openbndsect_coll%openbndsect_pnts => null()
          no_sect = input_hyd%openbndsect_coll%current_size
          do i_sect = 1, no_sect
             iret = output_hyd%openbndsect_coll%add(input_hyd%openbndsect_coll%openbndsect_pnts(i_sect))
             if ( iret .ne. i_sect ) then ; write(*,*) ' error copying boundary section data' ; call stop_with_error() ; endif
          end do
      endif

      ! pointers, allocate to the max

      if ( l_regular ) then
         allocate(output_hyd%ipoint(4,input_hyd%noq),stat=ierr_alloc)
         if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error allocating memory' ; call stop_with_error() ; endif
         output_hyd%ipoint = 0
         call agr_poi_reg ( ipnt, ipnt_b, input_hyd, output_hyd, ipnt_q)
      elseif ( l_expand ) then
         noq_new = (3*output_hyd%nolay-1)*output_hyd%nosegl
         allocate(output_hyd%ipoint(4,noq_new),stat=ierr_alloc)
         if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error allocating memory' ; call stop_with_error() ; endif
         output_hyd%ipoint = 0
         call agr_poi_exp ( ipnt, ipnt_b, input_hyd, output_hyd, ipnt_q)
      else
         allocate(output_hyd%ipoint(4,input_hyd%noq),stat=ierr_alloc)
         if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error allocating memory' ; call stop_with_error() ; endif
         output_hyd%ipoint = 0
         call agr_poi( ipnt             , input_hyd%noq    , &
                      input_hyd%noq1   , input_hyd%noq2   , &
                      input_hyd%noq3   , input_hyd%ipoint , &
                      output_hyd%noq   , output_hyd%noq1  , &
                      output_hyd%noq2  , output_hyd%noq3  , &
                      output_hyd%ipoint, ipnt_q           , &
                      ipnt_b           )
         if (output_hyd%geometry .eq. HYD_GEOM_UNSTRUC .and. output_hyd%file_dwq%name .ne. ' ') then
            output_hyd%ipoint(3:4,:) = 0
         endif
      endif

      ! horizontal surface, always greater then 0.0, aggregate one layer then copy to all new layers

      allocate(output_hyd%surf(output_hyd%noseg),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error allocating memory' ; call stop_with_error() ; endif
      call aggregate_extended( input_hyd%nosegl, output_hyd%nosegl, &
                  1               , 1                , &
                  1               , 1                , &
                  1               , 0                , &
                  0               , 1                , &
                  ipnt            , AGGREGATION_TYPE_ACCUMULATE     , &
                  input_hyd%surf  , rdumar           , &
                  rdumar          , output_hyd%surf  )
      do iseg = 1 , output_hyd%nosegl
         output_hyd%surf(iseg) = max(1.0,output_hyd%surf(iseg))
         do ilay = 2 , output_hyd%nolay
            isegl = (ilay-1)*output_hyd%nosegl + iseg
            output_hyd%surf(isegl) = output_hyd%surf(iseg)
         enddo
      enddo

!     allocate(output_hyd%displen(2,input_hyd%noq),stat=ierr_alloc)
      allocate(output_hyd%displen(2,output_hyd%noq),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error allocating memory' ; call stop_with_error() ; endif
      call agr_len( input_hyd        , output_hyd       , &
                   ipnt_h           , ipnt_q           , &
                   l_expand         , l_lenlen         )

      ! depth

      allocate(output_hyd%depth(output_hyd%nosegl),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error allocating memory' ; call stop_with_error() ; endif
      allocate(rwork(output_hyd%nosegl),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error allocating memory' ; call stop_with_error() ; endif
      call aggregate_extended( input_hyd%nosegl, output_hyd%nosegl, &
                  1               , 1                , &
                  1               , 1                , &
                  1               , 1                , &
                  1               , 1                , &
                  ipnt            , AGGREGATION_TYPE_WEIGHTED_AVERAGE      , &
                  input_hyd%depth , input_hyd%surf   , &
                  rwork           , output_hyd%depth )
      deallocate(rwork,stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error deallocating memory' ; call stop_with_error() ; endif

      ! attributes

      allocate(output_hyd%attributes(output_hyd%noseg),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error allocating memory' ; call stop_with_error() ; endif
      call agr_atr(input_hyd , ipnt, output_hyd)

      ! allocate the time dependent arrays

      allocate(output_hyd%volume(output_hyd%noseg))
      allocate(output_hyd%area(output_hyd%noq))
      allocate(output_hyd%flow(output_hyd%noq))
      if ( input_hyd%sal_present ) then
         output_hyd%sal_present = .true.
         allocate(output_hyd%sal(output_hyd%noseg))
      else
         output_hyd%sal_present = .false.
      endif
      if ( input_hyd%tem_present ) then
         output_hyd%tem_present = .true.
         allocate(output_hyd%tem(output_hyd%noseg))
      else
         output_hyd%tem_present = .false.
      endif
      if ( input_hyd%tau_present ) then
         output_hyd%tau_present = .true.
         allocate(output_hyd%tau(output_hyd%noseg))
      else
         output_hyd%tau_present = .false.
      endif
      if ( input_hyd%vdf_present ) then
         output_hyd%vdf_present = .true.
         allocate(output_hyd%vdf(output_hyd%noseg))
      else
         output_hyd%vdf_present = .false.
      endif
!     allocate(hyd%wasteflow(hyd%wasteload_coll%actual_size))

      ! cco information
      if (output_hyd%geometry .eq. HYD_GEOM_CURVI) then

         if ( .not. l_regular ) then
            allocate(output_hyd%xdepth(output_hyd%nmax,output_hyd%mmax),stat=ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error allocating memory xdepth' ; call stop_with_error() ; endif
            allocate(output_hyd%ydepth(output_hyd%nmax,output_hyd%mmax),stat=ierr_alloc)
            if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error allocating memory ydepth' ; call stop_with_error() ; endif
            output_hyd%xdepth = input_hyd%xdepth
            output_hyd%ydepth = input_hyd%ydepth
         endif
      endif

      ! flow layers

      allocate(output_hyd%hyd_layers(output_hyd%kmax),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error allocating memory' ; call stop_with_error() ; endif
      do ilay = 1 , output_hyd%kmax
         output_hyd%hyd_layers(ilay) = input_hyd%hyd_layers(ilay)
      enddo

      ! waq layers

      allocate(output_hyd%waq_layers(output_hyd%nolay),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error allocating memory' ; call stop_with_error() ; endif
      output_hyd%waq_layers = 0
      do ilay = 1 , input_hyd%nolay
         newlay = ipnt_v(ilay)
         output_hyd%waq_layers(newlay) = output_hyd%waq_layers(newlay) + input_hyd%waq_layers(ilay)
      enddo

      ! wasteloads

      output_hyd%wasteload_coll%current_size = 0
      output_hyd%wasteload_coll%maxsize = 0
      output_hyd%wasteload_coll%l_seconds = input_hyd%wasteload_coll%L_seconds

      do iwast = 1 , input_hyd%wasteload_coll%current_size

         if ( l_regular ) then
            n           = input_hyd%wasteload_coll%wasteload_pnts(iwast)%n
            m           = input_hyd%wasteload_coll%wasteload_pnts(iwast)%m
            iseg_new    = ipnt_h(n,m)
            n_new       = mod(iseg_new-1,output_hyd%nmax) + 1
            m_new       = (iseg_new-1)/output_hyd%nmax + 1
            wasteload%n = n_new
            wasteload%m = m_new
         else
            wasteload%n    = input_hyd%wasteload_coll%wasteload_pnts(iwast)%n
            wasteload%m    = input_hyd%wasteload_coll%wasteload_pnts(iwast)%m
         endif

         if ( input_hyd%wasteload_coll%wasteload_pnts(iwast)%k .gt. 0 ) then
            wasteload%k    = ipnt_v(input_hyd%wasteload_coll%wasteload_pnts(iwast)%k)
         else
            wasteload%k    = input_hyd%wasteload_coll%wasteload_pnts(iwast)%k
         endif
         wasteload%name = input_hyd%wasteload_coll%wasteload_pnts(iwast)%name
         wasteload%waqtype = input_hyd%wasteload_coll%wasteload_pnts(iwast)%waqtype
         wasteload%type = input_hyd%wasteload_coll%wasteload_pnts(iwast)%type

         ! add to wasteload collection

         i_wasteload = output_hyd%wasteload_coll%add(wasteload)

      enddo
      if ( output_hyd%wasteload_coll%current_size .gt. 0 ) then
         ierror = input_hyd%wasteload_data%copy(output_hyd%wasteload_data)
         if ( ierror .ne. 0 ) then ; write(*,*) ' error copying wasteload data' ; call stop_with_error() ; endif
      endif

      ! some things

      output_hyd%description(1) = input_hyd%description(1)
      output_hyd%description(2) = input_hyd%description(2)
      output_hyd%description(3) = 'aggregated by agrhyd'
      output_hyd%hyd_ref        = input_hyd%hyd_ref
      output_hyd%hyd_start      = input_hyd%hyd_start
      output_hyd%hyd_stop       = input_hyd%hyd_stop
      output_hyd%hyd_step       = input_hyd%hyd_step
      output_hyd%cnv_ref        = input_hyd%cnv_ref
      output_hyd%cnv_start      = input_hyd%cnv_start
      output_hyd%cnv_stop       = input_hyd%cnv_stop
      output_hyd%cnv_step       = input_hyd%cnv_step
      output_hyd%time_ref       = input_hyd%time_ref
      output_hyd%file_com%name  = input_hyd%file_com%name

      return
      end
