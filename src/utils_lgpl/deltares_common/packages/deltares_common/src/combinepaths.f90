subroutine combinepaths (firstname, secondname)
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
!!--description-----------------------------------------------------------------
! This routine appends the secondname to the path part of firstname, and returns
! it as an updated version of secondname. Both firstname and secondname may
! initially contain absolute, relative or no path information.
!
! RESTRICTION: The combined path/file length should be less than 256 characters.
!
! Examples:
!
! firstname             secondname IN       secondname OUT
! file1.txt             file2.txt           file2.txt
! dir\file1.txt         file2.txt           dir\file2.txt
! dir/file1.txt         file2.txt           dir/file2.txt
! c:\dir\file1.txt      file2.txt           c:\dir\file2.txt
! //dir/file1.txt       file2.txt           //dir/file2.txt
! c:\dir\file1.txt      ..\dir2\file2.txt   c:\dir\..\dir2\file2.txt
! //dir/file1.txt       ../dir2/file2.txt   //dir/../dir2/file2.txt
! c:\dir\file1.txt      d:\dir2\file2.txt   d:\dir2\file2.txt
! //dir/file1.txt       //dir2/file2.txt    //dir2/file2.txt
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    character(*), intent(in)  :: firstname
    character(*)              :: secondname
!
! Local variables
!
    integer        :: ipos
    character(256) :: newname
!
!! executable statements -------------------------------------------------------
!
    secondname = adjustl(secondname)
    if (len_trim(secondname) > 2) then
       if (secondname(2:2) == ':') then
          !
          ! secondname contains an absolute Windows path
          ! don't append it to firstname!
          !
          return
       elseif (secondname(1:2) == '//') then
          !
          ! secondname contains an absolute Linux/UNIX path
          ! don't append it to firstname!
          !
          return
       endif
    endif
    !
    ! secondname contains file name or relative path.
    ! firstname may contain absolute, relative or no path.
    !
    ipos = index(firstname, '/', BACK=.true.)
    ipos = max(ipos,index(firstname, char(92), BACK=.true.)) ! char(92)==backslash
    !
    ! ipos = 0: no separator in firstname; 
    ! no path in firstname to add to secondname; return
    !
    if (ipos == 0) return
    newname = ' '
    write(newname,'(2a)') firstname(:ipos), trim(secondname)
    secondname = newname
end subroutine combinepaths
