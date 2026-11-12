program waves_main
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2026.                                
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use wave_main, only: wave_main_init, wave_main_step, wave_main_finish
   use precision
   !
   ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
   ! Activate the following line
   ! See also statements below
   !
   ! use ifcore
   ! 
   implicit none
!
! Global variables
!
!
! Local variables
!
   integer                                      :: arg
   integer                                      :: i
   integer                                      :: ierr
   integer                                      :: mode_in
   integer                                      :: online_in
   integer                                      :: mtdim
   integer                                      :: retval
   real(hp)                                     :: stepsize
   logical                                      :: mode_read
   logical                                      :: mdw_file_read
   character(256)                               :: tmpchar
   character(256)                               :: mdw_file     ! filename mdw file

   !
   ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
   ! Activate the following line
   ! See also statements below
   !
   ! INTEGER*4 OLD_FPE_FLAGS, NEW_FPE_FLAGS
!
!! executable statements -----------------------------------------------
!
   ! To raise floating-point invalid, divide-by-zero, and overflow exceptions:
   ! Activate the following two lines
   ! See also use statement above
   !
   ! NEW_FPE_FLAGS = FPE_M_TRAP_OVF + FPE_M_TRAP_DIV0 + FPE_M_TRAP_INV
   ! OLD_FPE_FLAGS = FOR_SET_FPE (NEW_FPE_FLAGS)
   !
   ! ====================================================================================
   ! INIT
   ! ====================================================================================
   !
   mode_in = stand_alone
   online_in = ONLINE_NONE
   select case(COMMAND_ARGUMENT_COUNT())
   case (1)
       call getarg(1,mdw_file)
   case (2)
       mode_read     = .false.
       mdw_file_read = .false.
       do arg=1,2
           call getarg(arg,tmpchar)
           if (.not. mode_read) then
               if (trim(tmpchar)(1:9) == '--online=') then
                   select case (trim(tmpchar)(10:))
                   case ('none')
                       mode_in   = stand_alone
                       online_in = ONLINE_NONE
                   case ('delftio')
                       mode_in   = flow_online
                       online_in = ONLINE_DELFTIO
                   case ('dimr')
                       mode_in   = flow_online
                       online_in = ONLINE_DIMR
                   case ('precice')
                       online_in = ONLINE_PRECICE
                   case default
                       call wavestop(1, "Invalid command line argument: " // trim(tmpchar))
                   end select
                   mode_read = .true.
               else
                   read(tmpchar,*,iostat=ierr) mode_in
                   if (ierr /= 0) then
                       mdw_file = trim(tmpchar)
                       mdw_file_read = .true.
                   else
                       mode_read = .true.
                   end if
               end if
           else
               if (mdw_file_read) then
                   call wavestop(1, "Invalid command line argument: either '" // trim(tmpchar) // "' or '" // trim(mdw_file) // "' must be a mode specification")
               end if
               mdw_file = tmpchar
           end if
       end do
   case default
      call usage()
      call wavestop(1, "Wrong number of command line arguments")
   endselect
   call small(mdw_file,len(mdw_file))

   do i=1,len(mdw_file)
      if (ichar(mdw_file(i:i)) == 0) mdw_file(i:i) = ' '
   enddo
   !
   retval = wave_main_init(mode_in, online_in, mdw_file)
   !
   ! ====================================================================================
   ! RUN
   ! ====================================================================================
   !
   stepsize = -1.0_hp
   retval = wave_main_step(stepsize)
   !
   ! ====================================================================================
   ! FINALIZE
   ! ====================================================================================
   !
   retval = wave_main_finish()
end program waves_main
