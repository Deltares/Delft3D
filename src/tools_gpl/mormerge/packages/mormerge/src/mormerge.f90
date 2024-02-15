program mormerge
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
!!--description-----------------------------------------------------------------
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use mormerge_version_module
implicit none
!
! Local constants
!
integer, parameter :: numarg = 6
!
! Local variables
!
integer                                   :: i
integer                                   :: iindex
integer                                   :: windex
integer                                   :: rindex
character(256), dimension(:), allocatable :: arguments
character(256)                            :: full_version   ! by calling getfullversionstring_MORMERGE, the version number is visible with the what command
!
!! executable statements -------------------------------------------------------
!
call getfullversionstring_mormerge(full_version)
!
if (COMMAND_ARGUMENT_COUNT() /= numarg) then
   call printUsage()
   stop 
endif
allocate(arguments(0:COMMAND_ARGUMENT_COUNT()))
!
iindex = 0
windex = 0
rindex = 0
do i=0,numarg
   !
   ! Read and scan arguments
   !
   call getarg(i,arguments(i))
   if (arguments(i) == "-i") then
      iindex = i+1
   endif
   if (arguments(i) == "-w") then
      windex = i+1
   endif
   if (arguments(i) == "-r") then
      rindex = i+1
   endif
enddo
!
! Checks
!
if (iindex == 0) then
   write(*,'(a)') "ERROR: inputfilename not specified"
   call printUsage()
   stop 
endif
if (windex == 0) then
   write(*,'(a)') "ERROR: workdir not specified"
   call printUsage()
   stop 
endif
if (rindex == 0) then
   write(*,'(a)') "ERROR: runid not specified"
   call printUsage()
   stop 
endif
!
! Continue
!
call merge(arguments(iindex), arguments(windex), arguments(rindex))
deallocate(arguments)
end program mormerge



subroutine printUsage()
   implicit none
   write(*,'(a)') "Usage:"
   write(*,'(a)') "mormerge.exe -i <inputfilename> -w <workdir> -r <runid>"
endsubroutine printUsage
