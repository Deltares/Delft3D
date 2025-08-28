program waves_main
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
   integer                                      :: i
   integer                                      :: ierr
   integer                                      :: mode_in
   integer                                      :: mtdim
   integer                                      :: retval
   real(hp)                                     :: stepsize
   character(20)                                :: tmpchar
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
   mode_in = 0
   select case(COMMAND_ARGUMENT_COUNT())
   case (1)
      call getarg(1,mdw_file)
   case (2)
      call getarg(1,mdw_file)
      call getarg(2,tmpchar)
      read(tmpchar,*,iostat=ierr) mode_in
   case default
      call usage()
      call wavestop(1, "Wrong number of command line arguments")
   endselect
   call small(mdw_file,len(mdw_file))

   do i=1,len(mdw_file)
      if (ichar(mdw_file(i:i)) == 0) mdw_file(i:i) = ' '
   enddo
   !
   retval = wave_main_init(mode_in, mdw_file)
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
