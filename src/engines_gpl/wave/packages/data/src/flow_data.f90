module flow_data
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
!!--description-----------------------------------------------------------------
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use precision_basics
!
integer                                 , save :: num_subdomains        = 0
integer                                 , save :: num_muddomains        = 0
logical                                 , save :: flow_data_initialized = .false.
character(256), dimension(:),allocatable, save :: runids
character(256), dimension(:),allocatable, save :: mudids
!
!
contains
!
!
!===============================================================================
subroutine flow_init (mode, it01, tscale)
!
! Initializes communication with flow computation
! Fills flow_data parameters
!
   use swan_input    ! Needed when running stand_alone
   use sync_flowwave
   use wave_data
   use string_module, only: str_token, count_words
   !
   implicit none
!
! Global variables
!
   integer, intent(in) :: mode
   integer             :: it01
   real                :: tscale
!
! Local variables
!
   integer                          :: idom
   real(hp)                         :: d_tscale
   real                             :: dummy     = -999.0
   logical                          :: mud
   character(80), dimension(100)    :: subdom_names
   character(256)                   :: filnam
!
!! executable statements -------------------------------------------------------
!
   mud          = .false.
   subdom_names = ' '
   if (swan_run%flowgridfile/=' ') then
      !
      ! Running in Delft3D FM mode
      ! mode may be stand_alone or not
      !
      if (swan_run%useflowdata .or. swan_run%swwav) then
         num_subdomains = checkcomfiles(swan_run%flowgridfile)
         if (num_subdomains == 1) then
            write(*,'(3a)') '*** MESSAGE: Using data from NetCDF file "', trim(swan_run%flowgridfile), '".'
         else
            write(*,'(a,i0,a)') '*** MESSAGE: Using data from ', num_subdomains, ' NetCDF files'
            write(*,'(2a)')     '             Base name: ', trim(swan_run%flowgridfile)
            write(*,'(a,i4.4)')  '             Partition range: 0000 - ', num_subdomains-1
         endif
         flow_data_initialized = .true.
      endif
   elseif (mode==stand_alone) then
      !
      ! Running in Delft3D 4 stand alone mode
      !
      if (swan_run%useflowdata .or. swan_run%swwav) then
         if (swan_run%comfile == ' ') then
            swan_run%comfile = swan_run%casl
            num_subdomains = 1
         else
            num_subdomains = count_words(swan_run%comfile)
         endif
         !
         allocate (runids(num_subdomains))
         write(*,'(a,i0,a)') '*** MESSAGE: Using data from the following FLOW domain(s):'
         do idom = 1,num_subdomains
            call str_token(swan_run%comfile,runids(idom))
            write(*,'(13x,a)') trim(runids(idom))
         enddo
         !
         ! Obtain time settings from first com file
         !
         write(filnam,'(2a)') 'com-',trim(runids(1))
         call get_params(tscale, dummy, filnam)
         flow_data_initialized = .true.
      endif
   else ! mode/=stand_alone .and. swan_run%flowgridfile==' '
      !
      ! Running in Delft3D 4 coupled mode
      !
      write (*,'(a)') '  Waiting for initialisation from FLOW'
      num_subdomains = wave_from_flow_init(subdom_names, it01, d_tscale, mud)
      tscale         = real(d_tscale,sp)
      if (num_subdomains < 1) then
         write(*,'(a)') '*** ERROR: Delftio initialization WAVE side failed'
         call wavestop(1, '           Is file ''dioconfig.ini'' present?')
      endif
      allocate (runids(num_subdomains))
      write(*,'(a,i0,a)') '*** MESSAGE: Connected to the following ',num_subdomains,' FLOW domain(s):'
      do idom = 1,num_subdomains
         runids(idom) = subdom_names(idom)
         write(*,'(13x,a)') trim(runids(idom))
      enddo
      call wave_to_flow_status(flow_wave_comm_result_ok, mud)
      !
      if (mode == flow_mud_online) then
         mud          = .true.
         subdom_names = ' '
         write (*,'(a)') '  Waiting for initialisation from MUD layer'
         num_muddomains = wave_from_flow_init(subdom_names, it01, d_tscale, mud)
         if (num_muddomains < 1) then
            write(*,'(a)') '*** ERROR: Delftio initialization WAVE side failed'
            call wavestop(1, '           Is file ''dioconfig.ini'' present?')
         endif
         allocate (mudids(num_muddomains))
         write(*,'(a,i0,a)') '*** MESSAGE: Connected to the following ',num_muddomains,' MUD domain(s):'
         do idom = 1,num_muddomains
            mudids(idom) = subdom_names(idom)
            write(*,'(13x,a)') trim(mudids(idom))
         enddo
         if (num_muddomains /= 1) then
            write(*,'(a)') '*** ERROR: Interaction with Fluid Mud is currently only possible for one domain'
            call wave_to_flow_status(flow_wave_comm_error, mud)
            call wavestop(1, '*** ERROR: Interaction with Fluid Mud is currently only possible for one domain')
         endif
         if (num_muddomains /= num_subdomains) then
            write(*,'(a,2(i0,a))') '*** ERROR: number of mud domains (', num_muddomains, &
                & ') is not equal to the number of water domains (',num_subdomains,').'
            call wave_to_flow_status(flow_wave_comm_error, mud)
            call wavestop(1, 'Number of mud domains is not equal to the number of water domains')
         endif
         call wave_to_flow_status(flow_wave_comm_result_ok, mud)
      endif
      flow_data_initialized = .true.
   endif
end subroutine flow_init
!
!
!===============================================================================
subroutine deallocate_flow_data ()
   integer :: ierr
   deallocate (runids, stat=ierr)
   if (num_muddomains > 0) then
      deallocate (mudids, stat=ierr)
   endif
end subroutine deallocate_flow_data
!
!
!===============================================================================
function checkcomfiles(basename) result(numDomains)
   !
   ! Return
   integer :: numDomains
   !
   ! Parameters
   character(*), intent(in) :: basename
   !
   ! Locals
   integer :: partitionlocation
   integer :: retval
   logical :: ex
   character(300) :: filnam
   !
   ! Body
   retval = 0
   inquire (file = trim(basename), exist = ex)
   if (ex) then
      retval = 1
   else
      partitionlocation = index(basename, '_com.nc')
      if (partitionlocation == 0) then
          call wavestop(1, 'The NetCDF com file specified does not contain the substring "_com.nc"')
      endif
      do
         write(filnam,'(a,a,i4.4,a)') basename(:partitionlocation-1), '_', retval, trim(basename(partitionlocation:))
         inquire (file = trim(filnam), exist = ex)
         if (ex) then
            ! Found an existing com-file
            retval = retval + 1
         else
            ! Not an existing com-file (anymore)
            exit
         endif
      enddo
      if (retval == 0) then
          write(*,'(2a)') '*** ERROR: File does not exist:', trim(basename)
          write(*,'(2a)') '           And partition variant does not exist:', trim(filnam)
          call wavestop(1, 'No com-file to connect to')
      endif
   endif
   numDomains = retval
end function checkcomfiles

end module flow_data
