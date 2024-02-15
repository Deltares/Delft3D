!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

!> make directory (also for linux)
 subroutine makedir(dirname)
   use string_module, only: get_dirsep
#ifdef __INTEL_COMPILER
   use ifport
#endif
    implicit none
    character(len=*), intent(in) :: dirname

    character(len=256)           :: command
    integer                      :: istat
    logical                      :: l_exist
    integer                      :: i
    character(len=256)           :: dirnamewin

!    write(6,"('Creating directory ', A128)") trim(dirname)

#ifdef __INTEL_COMPILER
    inquire(directory = trim(dirname), exist = l_exist)
#else
    ! GNU
    inquire(file = trim(dirname)//get_dirsep()//".", exist = l_exist)
#endif
    if (l_exist) then
       return
    end if

    if ( get_dirsep().eq.'/' ) then
!     linux
      command = "mkdir -p "//trim(dirname)
    else
!     windows
       dirnamewin = trim(dirname)
       do i = 1,len(dirnamewin)
          if( dirnamewin(i:i) == '/' ) dirnamewin(i:i) = '\'
       enddo
       command = "mkdir "//trim(dirnamewin)
       ! call iosDirMAKE(dirname)
    end if

    istat = system(command)
    ! Fortran2008, not available before Intel 15:
    ! call execute_command_line(command)

    return
 end subroutine
