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
!!--description-----------------------------------------------------------------
! This routine calls the c routine getcwd.c to obtain the current 
! working directorty (cwd). Code based on :https://stackoverflow.com/questions/30279228/is-there-an-alternative-to-getcwd-in-fortran-2003-2008.
!-------------------------------------------------------------------------------
  
  
  module cwd
    use iso_c_binding, only: C_INT, C_CHAR, C_NULL_CHAR
    implicit none
    private
    public :: getCWD

    interface
        function getCWDHelper(str, len) bind(C, name="getCWDHelper")
            use iso_c_binding, only: C_INT, C_CHAR
            integer(kind=C_INT) :: getCWDHelper
            character(kind=C_CHAR), intent(out) :: str(*)
            integer(kind=C_INT), value :: len
        end function getCWDHelper
    end interface

contains

    ! Writes the current working directory path into str.
    ! Returns 0 on success, or 1 on error.
    function getCWD(str)
        integer :: getCWD
        character(*), intent(out) :: str

        integer :: i, length
        character(len=len(str), kind=C_CHAR) :: str_copy

        ! Call the C helper, passing the length as the correct int kind
        getCWD = getCWDHelper(str_copy, len(str_copy, kind=C_INT))

        if (getCWD /= 0) then
            str = '' ! Error, clear the string
            return
        end if

        ! Copy the C_CHAR string to the output string,
        ! removing the C_NULL_CHAR and clearing the rest.
        length = index(str_copy, C_NULL_CHAR) - 1
        do i = 1, length
            str(i:i) = char(ichar(str_copy(i:i)))
        end do
        str(length+1:) = ''
    end function getCWD

end module
