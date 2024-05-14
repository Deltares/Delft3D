!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
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

      subroutine read_src_tmp(file_src_tmp, nolay, wasteload_coll, wasteload_data)

      ! read a src file
      use m_logger, only : terminate_execution, get_log_unit_number
      use m_waq_file                   ! module contains everything for the files
      use m_hydmod                   ! module contains everything for the hydrodynamic description
      use rd_token       ! tokenized reading

      implicit none

      ! declaration of the arguments

      type(t_file)                       :: file_src_tmp           ! tmp sources file
      integer                                :: nolay                  ! number of layers
      type(t_wasteload_coll)                 :: wasteload_coll         ! the wasteloads
      type(t_data_block)      , intent(inout)  :: wasteload_data         ! wasteload_data
      type(t_data_block)                       :: wasteload_data_tmp     ! wasteload_data backup to append

      ! local declarations

      integer                                :: i                      ! loop counter
      integer                                :: ilay                   ! loop counter
      integer                                :: iwaste                 ! waste index
      integer                                :: i_waste                ! waste index
      integer                                :: i_flow                 ! flow index
      integer                                :: ibrk                   ! breakpoint index
      integer                                :: nobrk_waste            ! number of breakpoints
      integer                                :: nobrk_waste_tmp        ! number of breakpoints in tmp file
      integer                                :: nowast2                ! number of wasteloads in file
      integer                                :: iopt_time              ! option time dependent input
      integer                                :: no_param               ! number of parameters/itim
      integer                                :: no_waste               ! number of wasteloads
      integer                                :: no_flow                ! number of flows
      integer                                :: first_itime_current    ! time of first break point where we are going to patch before
      integer                                :: lunrep                 ! unit number report file
      integer                                :: int                    ! integer token from input
      real                                   :: reel                   ! real token from input
      integer                                :: ierr                   ! error indication
      integer                                :: ierr_alloc             ! error indication
      real, allocatable                      :: flow_data(:,:,:)       ! array with the flows from file
      character(len=255)                     :: ctoken                 ! line buffer input file

      call get_log_unit_number(lunrep)

      no_waste = wasteload_coll%current_size
      if(no_waste.eq.0) return
      
      if(wasteload_data%num_breakpoints .gt. 0) then
         first_itime_current = wasteload_data%times(1)
      else
         first_itime_current = 2000000000
      endif
      
      no_flow  = 0
      do i = 1 , no_waste
         if ( wasteload_coll%wasteload_pnts(i)%k .eq. 0 ) then
            no_flow = no_flow + nolay
         else
            no_flow = no_flow + 1
         endif
      enddo

      call file_src_tmp%open()
      ilun    = 0
      ilun(1) = file_src_tmp%unit
      lch (1) = file_src_tmp%name
      npos   = 1000
      cchar  = ';'

      ! first count the number of breakpoints in the file before first_time_current
      nobrk_waste_tmp = 0
      do
         if( gettoken( int, ierr).ne.0) then
            exit
         end if
         if(int.ge.first_itime_current) then
            exit
         end if
         ! dummy read the data
         do iwaste = 1 , no_flow
            do i = 1 , 3
               if ( gettoken( int, ierr) .ne. 0 ) then
                  write(lunrep,*) ' error reading sources file'
                  write(lunrep,*) ' expected integer for isrc, iseg, ilay'
                  goto 200
               endif
            enddo
            if ( gettoken( reel, ierr) .ne. 0 ) then
               write(lunrep,*) ' error reading sources file'
               write(lunrep,*) ' expected real with concentration 1.0'
               goto 200
            endif
         enddo
         nobrk_waste_tmp = nobrk_waste_tmp + 1
      enddo
      ierr = 0
      rewind(file_src_tmp%unit)

      ! then allocate number of break points + current number of breakpoints in wasteload_data_tmp
      nobrk_waste = nobrk_waste_tmp + wasteload_data%num_breakpoints
      wasteload_data_tmp%num_breakpoints = nobrk_waste
      no_param = 1
      wasteload_data_tmp%num_locations   = no_waste
      wasteload_data_tmp%num_parameters = no_param

      ! allocate arrays
      allocate(wasteload_data_tmp%times(nobrk_waste), &
               wasteload_data_tmp%values(no_param,no_waste,nobrk_waste), &
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
      wasteload_data_tmp%subject         = SUBJECT_WASTE
      wasteload_data_tmp%function_type        = FUNCTYPE_BLOCK
      wasteload_data_tmp%is_external          = .FALSE.
      wasteload_data_tmp%iorder          = ORDER_PARAM_LOC
      wasteload_data_tmp%is_parameter_pointered = .FALSE.
      wasteload_data_tmp%are_locations_default    = .FALSE.
      wasteload_data_tmp%are_locations_pointered   = .FALSE.
      wasteload_data_tmp%is_scaled          = .FALSE.
      wasteload_data_tmp%need_parameters_scaling    = .FALSE.
      wasteload_data_tmp%need_location_scaling      = .FALSE.

      ! read the data from the tmp-file into wasteload_data_tmp
      do ibrk = 1 , nobrk_waste_tmp
         if ( gettoken( wasteload_data_tmp%times(ibrk), ierr) .ne. 0 ) then
            write(lunrep,*) ' error reading sources file'
            write(lunrep,*) ' expected integer with breakpoint'
            goto 200
         endif
         do iwaste = 1 , no_flow
            do i = 1 , 3
               if ( gettoken( int, ierr) .ne. 0 ) then
                  write(lunrep,*) ' error reading sources file'
                  write(lunrep,*) ' expected integer for isrc, iseg, ilay'
                  goto 200
               endif
            enddo
            if ( gettoken( flow_data(1,iwaste,ibrk), ierr) .ne. 0 ) then
               write(lunrep,*) ' error reading sources file'
               write(lunrep,*) ' expected real with wasteload flow'
               goto 200
            endif
         enddo
      enddo

      ! cummulate the flow for uniform wasteloads

      wasteload_data_tmp%values = 0.0
      do ibrk = 1 , nobrk_waste_tmp
         i_flow = 0
         do i_waste = 1 , no_waste
            if ( wasteload_coll%wasteload_pnts(i_waste)%k .eq. 0 ) then
               do ilay = 1 , nolay
                  i_flow = i_flow + 1
                  wasteload_data_tmp%values(1,i_waste,ibrk) = wasteload_data_tmp%values(1,i_waste,ibrk) + flow_data(1,i_flow,ibrk)
               enddo
            else
               i_flow = i_flow + 1
               wasteload_data_tmp%values(1,i_waste,ibrk) = wasteload_data_tmp%values(1,i_waste,ibrk) + flow_data(1,i_flow,ibrk)
            endif
         enddo
      enddo
      
      ! add the data that was already there into wasteload_data_tmp, and return it as wasteload_data
      do ibrk = 1 , wasteload_data%num_breakpoints
         wasteload_data_tmp%times(ibrk+nobrk_waste_tmp) = wasteload_data%times(ibrk)
         do i_waste = 1 , no_waste
            wasteload_data_tmp%values(1,i_waste,ibrk+nobrk_waste_tmp) = wasteload_data%values(1,i_waste,ibrk)
         end do
      enddo
      
      ierr = wasteload_data_tmp%copy(wasteload_data)
      if ( ierr .ne. 0 ) then
         write(*,*) ' error copying wasteload data'
         call terminate_execution(1)
      endif

      deallocate(flow_data)

  200 continue
      if ( ierr .ne. 0 ) then
         call terminate_execution(1)
      endif

      close(file_src_tmp%unit)
      file_src_tmp%status = FILE_STAT_UNOPENED

      end subroutine read_src_tmp
