!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!

      subroutine read_src(file_src, num_layers, wasteload_coll, wasteload_data, time_in_seconds)

      ! read a src file
      use m_logger_helper, only : stop_with_error, get_log_unit_number
      use m_waq_file                   ! module contains everything for the files
      use m_hydmod                   ! module contains everything for the hydrodynamic description
      use rd_token       ! tokenized reading

      implicit none

      ! declaration of the arguments

      type(t_file)                       :: file_src               ! aggregation-file
      integer                                :: num_layers                  ! number of layers
      type(t_wasteload_coll)                 :: wasteload_coll         ! the wasteloads
      type(t_data_block)      , intent(inout)  :: wasteload_data         ! wasteload_data

      ! local declarations

      integer                                :: i                      ! loop counter
      integer                                :: ilay                   ! loop counter
      integer                                :: iwaste                 ! waste index
      integer                                :: i_waste                ! waste index
      integer                                :: i_flow                 ! flow index
      integer                                :: ibrk                   ! breakpoint index
      integer                                :: nobrk_waste            ! number of breakpoints
      integer                                :: nowast2                ! number of wasteloads in file
      integer                                :: iopt_time              ! option time dependent input
      integer                                :: no_param               ! number of parameters/itim
      integer                                :: no_waste               ! number of wasteloads
      integer                                :: no_flow                ! number of flows
      integer                                :: lunrep                 ! unit number report file
      integer                                :: int                    ! integer token from input
      real                                   :: reel                   ! real token from input
      integer                                :: itype                  ! token type found
      integer                                :: ierr                   ! error indication
      integer                                :: ierr_alloc             ! error indication
      real, allocatable                      :: flow_data(:,:,:)       ! array with the flows from file
      logical                                :: time_in_seconds        ! Whether the time is given in seconds or not
      character(len=20)                      :: string                 ! String token
      character(len=40)                      :: ctime                  ! time token
      integer*8                              :: iday                   ! iday
      integer*8                              :: ihour                  ! ihour
      integer*8                              :: imin                   ! imin
      integer*8                              :: isec                   ! isec
      integer*8                              :: itime                  ! time in seconds
      integer*8, parameter :: I8_1000000 = 1000000, I8_10000 = 10000, I8_100 = 100


      call get_log_unit_number(lunrep)

      ! count how many wasteload flows we expect in the file (uniform loads have num_layers flows)

      no_flow  = 0
      no_waste = wasteload_coll%current_size
      do i = 1 , no_waste
         if ( wasteload_coll%wasteload_pnts(i)%k .eq. 0 ) then
            no_flow = no_flow + num_layers
         else
            no_flow = no_flow + 1
         endif
      enddo

      call file_src%open()
      ilun    = 0
      ilun(1) = file_src%unit
      lch (1) = file_src%name
      npos   = 1000
      cchar  = ';'
      ierr = 0

      ! option time dependent sources
      !
      ! Note:
      ! The time can be given in ddhhmmss format or in seconds
      ! If the latter, this information must be included in the
      ! output file too (since the time is not interpreted,
      ! that is the only issue)

      time_in_seconds = .false.
      if (gettoken( string, int, reel, itype, ierr) .ne. 0) then
         write(lunrep,*) ' error reading sources file'
         goto 200
      endif

      if ( itype .eq. 1) then
          if (string .eq. 'SECONDS' .or. string .eq. 'seconds') then
              time_in_seconds = .true.
              if ( gettoken ( iopt_time, ierr) .ne. 0 ) then
                 write(lunrep,*) ' error reading sources file'
                 write(lunrep,*) ' expected integer with option time dependent sources'
                 goto 200
              endif
          else
              write(lunrep,*) ' error reading sources file'
              write(lunrep,*) ' string at the beginning of the file should be either ''SECONDS'' or ''seconds'''
              goto 200
          endif
      else if ( itype .eq. 2) then
          iopt_time = int
      else
          write(lunrep,*) ' error reading sources file'
          write(lunrep,*) ' expected integer with option time dependent sources or a ''SECONDS'' or ''seconds'' string'
          goto 200
      endif
      wasteload_coll%l_seconds = time_in_seconds


      ! option block function

      if ( gettoken( wasteload_data%function_type, ierr) .ne. 0 ) then
         write(lunrep,*) ' error reading sources file'
         write(lunrep,*) ' expected integer with option block function'
         goto 200
      endif

      ! number of sources(flows), check with no_flow

      if ( gettoken( nowast2, ierr) .ne. 0 ) then
         write(lunrep,*) ' error reading sources file'
         write(lunrep,*) ' expected integer with number of sources'
         goto 200
      endif
      if ( nowast2 .ne. no_flow ) then
         if ( ierr .eq. 0 ) ierr = ierr + 1
         write(lunrep,*) ' number of wasteload flows in src file does not match hyd file'
         write(lunrep,*) ' src file:', nowast2
         write(lunrep,*) ' hyd file:', no_flow
         goto 200
      endif

      ! index numbers for waste loads, not used sequential input expected

      do iwaste = 1 , no_flow
         if ( gettoken( int, ierr) .ne. 0 ) then
            write(lunrep,*) ' error reading sources file'
            write(lunrep,*) ' expected integer with index of source:',iwaste
            goto 200
         endif
      enddo

      ! number of breakpoints

      if ( gettoken( nobrk_waste, ierr) .ne. 0 ) then
         write(lunrep,*) ' error reading sources file'
         write(lunrep,*) ' expected integer with number of breakpoints'
         goto 200
      endif
      wasteload_data%num_breakpoints = nobrk_waste

      ! allocate arrays

      no_param = 1
      wasteload_data%num_locations   = no_waste
      wasteload_data%num_spatial_parameters = no_param
      allocate(wasteload_data%times(nobrk_waste), &
               wasteload_data%values(no_param,no_waste,nobrk_waste), &
               flow_data(no_param,no_flow,nobrk_waste), &
               stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then
         ierr = ierr + 1
         write(lunrep,*) ' error allocating data arrays wasteloads'
         write(lunrep,*) ' number of breakpoints:',nobrk_waste
         write(lunrep,*) ' number of wasteloads :',no_waste
         write(lunrep,*) ' number of flows      :',no_flow
         goto 200
      endif

      ! set options

      wasteload_data%subject         = SUBJECT_WASTE
      wasteload_data%function_type        = FUNCTYPE_BLOCK
      wasteload_data%igrid          =  1
      wasteload_data%is_external          = .FALSE.
      wasteload_data%iorder          = ORDER_PARAM_LOC
      wasteload_data%is_parameter_pointered = .FALSE.
      wasteload_data%are_locations_default    = .FALSE.
      wasteload_data%are_locations_pointered   = .FALSE.
      wasteload_data%is_scaled          = .FALSE.
      wasteload_data%need_parameters_scaling    = .FALSE.
      wasteload_data%need_location_scaling      = .FALSE.


      ! two scale factors

      do i = 1 , 2
         if ( gettoken( reel, ierr) .ne. 0 ) then
            write(lunrep,*) ' error reading sources file'
            write(lunrep,*) ' expected real with scale factor'
            goto 200
         endif
      enddo

      ! loop over the breakpoints

      do ibrk = 1 , nobrk_waste

         ! read integer time as character to avoid overflow on ddhhmmss format

         if ( gettoken( itime, ierr) .ne. 0 ) then
            write(lunrep,*) ' error reading sources file'
            write(lunrep,*) ' expected integer with breakpoint'
            goto 200
         endif

         ! convert to seconds if needed using integer*8

         if ( .not. time_in_seconds ) then
            iday  = itime/I8_1000000
            ihour = mod(itime,I8_1000000)/I8_10000
            imin  = mod(itime,I8_10000)/I8_100
            isec  = mod(itime,I8_100)
            itime = iday*86400 + ihour*3600 + imin*60 + isec
         endif

         ! now make an integer

         wasteload_data%times(ibrk) = itime
         if ( ierr .ne. 0 ) then
            write(lunrep,*) ' error reading sources file'
            write(lunrep,*) ' expected integer with breakpoint'
            goto 200
         endif

         ! loop over the wasteloads read flow and dummy concentration

         do iwaste = 1 , no_flow
            if ( gettoken( flow_data(1,iwaste,ibrk), ierr) .ne. 0 ) then
               write(lunrep,*) ' error reading sources file'
               write(lunrep,*) ' expected real with wasteload flow'
               goto 200
            endif
            if ( gettoken( reel, ierr) .ne. 0 ) then
               write(lunrep,*) ' error reading sources file'
               write(lunrep,*) ' expected real with concentration 1.0'
               goto 200
            endif

         enddo

      enddo

      ! cummulate the flow for uniform wasteloads

      wasteload_data%values = 0.0
      do ibrk = 1 , nobrk_waste
         i_flow = 0
         do ilay = 1 , num_layers
            do i_waste = 1 , no_waste
               if ( ilay .eq. 1 .or. wasteload_coll%wasteload_pnts(i_waste)%k .eq. 0 ) then
                  i_flow = i_flow + 1
                  wasteload_data%values(1,i_waste,ibrk) = wasteload_data%values(1,i_waste,ibrk) + flow_data(1,i_flow,ibrk)
               endif
            enddo
         enddo
      enddo

      deallocate(flow_data)

  200 continue
      if ( ierr .ne. 0 ) then
         call stop_with_error()
      endif

      ! time always in seconds

      time_in_seconds = .true.

      close(file_src%unit)
      file_src%status = FILE_STAT_UNOPENED

      return
      end
