!!  Copyright (C)  Stichting Deltares, 2021-2023.
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

      subroutine read_src(file_src, nolay, wasteload_coll, wasteload_data, time_in_seconds)

      ! function : read a src file

      ! global declarations

      use m_srstop
      use m_monsys
      use filmod                   ! module contains everything for the files
      use hydmod                   ! module contains everything for the hydrodynamic description
      implicit none

      ! declaration of the arguments

      type(t_dlwqfile)                       :: file_src               ! aggregation-file
      integer                                :: nolay                  ! number of layers
      type(t_wasteload_coll)                 :: wasteload_coll         ! the wasteloads
      type(t_dlwqdata)      , intent(inout)  :: wasteload_data         ! wasteload_data

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
      integer                                :: ierr                   ! error indication
      integer                                :: ierr_alloc             ! error indication
      real, allocatable                      :: flow_data(:,:,:)       ! array with the flows from file
      type(inputfilestack)                   :: inpfil                 ! input file strucure with include stack
      logical                                :: time_in_seconds        ! Whether the time is given in seconds or not
      character(len=20)                      :: string                 ! String token
      character(len=40)                      :: ctime                  ! time token
      integer*8                              :: iday                   ! iday
      integer*8                              :: ihour                  ! ihour
      integer*8                              :: imin                   ! imin
      integer*8                              :: isec                   ! isec
      integer*8                              :: itime                  ! time in seconds


      call getmlu(lunrep)

      ! count how many wasteload flows we expect in the file (uniform loads have nolay flows)

      no_flow  = 0
      no_waste = wasteload_coll%cursize
      do i = 1 , no_waste
         if ( wasteload_coll%wasteload_pnts(i)%k .eq. 0 ) then
            no_flow = no_flow + nolay
         else
            no_flow = no_flow + 1
         endif
      enddo

      call dlwqfile_open(file_src)
      inpfil%inplun(1) = file_src%unit_nr
      inpfil%finame(1) = file_src%name
      inpfil%cchar  = ';'
      inpfil%iposr  = 0
      inpfil%npos   = 256
      inpfil%token_used = .true.
      inpfil%inputf = 1
      inpfil%nrepeat= 0

      ! option time dependent sources
      !
      ! Note:
      ! The time can be given in ddhhmmss format or in seconds
      ! If the latter, this information must be included in the
      ! output file too (since the time is not interpreted,
      ! that is the only issue)

      time_in_seconds = .false.
      call dlwq_read_token( inpfil, string, ierr)
      if ( string .eq. 'SECONDS' .or. string .eq. 'seconds' ) then
          time_in_seconds = .true.
          call dlwq_read_token( inpfil, iopt_time, ierr)
      else
          read( string, *, iostat = ierr ) iopt_time
      endif

      if ( ierr .ne. 0 ) then
         write(lunrep,*) ' error reading sources file'
         write(lunrep,*) ' expected integer with option time dependent sources'
         goto 200
      endif

      ! option block function

      call dlwq_read_token( inpfil, wasteload_data%functype, ierr)
      if ( ierr .ne. 0 ) then
         write(lunrep,*) ' error reading sources file'
         write(lunrep,*) ' expected integer with option block function'
         goto 200
      endif

      ! number of sources(flows), check with no_flow

      call dlwq_read_token( inpfil, nowast2, ierr)
      if ( ierr .ne. 0 ) then
         write(lunrep,*) ' error reading sources file'
         write(lunrep,*) ' expected integer with number of sources'
         goto 200
      endif
      if ( nowast2 .ne. no_flow ) then
         if ( ierr .eq. 0 ) ierr = ierr + 1
         write(lunrep,*) ' number of wasteload flows in src file does not match hyd file'
         write(lunrep,*) ' src file: ', nowast2
         write(lunrep,*) ' hyd file: ', no_flow
         goto 200
      endif

      ! index numbers for waste loads, not used sequential input expected

      do iwaste = 1 , no_flow
         call dlwq_read_token( inpfil, int, ierr)
         if ( ierr .ne. 0 ) then
            write(lunrep,*) ' error reading sources file'
            write(lunrep,*) ' expected integer with index of source:',iwaste
            goto 200
         endif
      enddo

      ! number of breakpoints

      call dlwq_read_token( inpfil, nobrk_waste, ierr)
      if ( ierr .ne. 0 ) then
         write(lunrep,*) ' error reading sources file'
         write(lunrep,*) ' expected integer with number of breakpoints'
         goto 200
      endif
      wasteload_data%no_brk = nobrk_waste

      ! allocate arrays

      no_param = 1
      wasteload_data%no_loc   = no_waste
      wasteload_data%no_param = no_param
      allocate(wasteload_data%times(nobrk_waste), &
               wasteload_data%values(no_param,no_waste,nobrk_waste), &
               flow_data(no_param,no_flow,nobrk_waste), &
               stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then
         ierr = ierr + 1
         write(lunrep,*) ' error allocating data arrays wasteloads'
         write(lunrep,*) ' number of brakpoints :',nobrk_waste
         write(lunrep,*) ' number of wasteloads :',no_waste
         write(lunrep,*) ' number of flows      :',no_flow
         goto 200
      endif

      ! two scale factors

      do i = 1 , 2
         call dlwq_read_token( inpfil, reel, ierr)
         if ( ierr .ne. 0 ) then
            write(lunrep,*) ' error reading sources file'
            write(lunrep,*) ' expected real with scale factor'
            goto 200
         endif
      enddo

      ! loop over the breakpoints

      do ibrk = 1 , nobrk_waste

         ! read integer time as character to avoid overflow on ddhhmmss format

         call dlwq_read_token( inpfil, ctime, ierr)
         if ( ierr .ne. 0 ) then
            write(lunrep,*) ' error reading sources file'
            write(lunrep,*) ' expected integer with breakpoint'
            goto 200
         endif

         ! convert to seconds if needed using integer*8

         if ( .not. time_in_seconds ) then
            read( ctime, * , iostat = ierr ) itime
            if ( ierr .ne. 0 ) then
               write(lunrep,*) ' error reading sources file'
               write(lunrep,*) ' expected integer with breakpoint'
               goto 200
            endif
            iday  = itime/1000000
            ihour = mod(itime,1000000)/10000
            imin  = mod(itime,10000)/100
            isec  = mod(itime,100)
            itime = iday*86400 + ihour*3600 + imin*60 + isec
            write(ctime,'(i40)') itime
         endif

         ! now make an integer

         read( ctime, * , iostat = ierr ) wasteload_data%times(ibrk)
         if ( ierr .ne. 0 ) then
            write(lunrep,*) ' error reading sources file'
            write(lunrep,*) ' expected integer with breakpoint'
            goto 200
         endif

         ! loop over the wasteloads read flow and dummy concentration

         do iwaste = 1 , no_flow
            call dlwq_read_token( inpfil, flow_data(1,iwaste,ibrk), ierr)
            if ( ierr .ne. 0 ) then
               write(lunrep,*) ' error reading sources file'
               write(lunrep,*) ' expected real with wasteload flow'
               goto 200
            endif
            call dlwq_read_token( inpfil, reel, ierr)
            if ( ierr .ne. 0 ) then
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
         do ilay = 1 , nolay
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
         call srstop(1)
      endif

      ! time always in seconds

      time_in_seconds = .true.

      close(file_src%unit_nr)
      file_src%status = FILE_STAT_UNOPENED

      return
      end
