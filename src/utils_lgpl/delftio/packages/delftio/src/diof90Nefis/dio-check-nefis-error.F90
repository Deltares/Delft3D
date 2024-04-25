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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!
!!! dio-check-nefis-error: check the return value of a Nefis call
!!!
!!! (c) Deltares, apr 2007
!!!
!!! Stef.Hummel@deltares.nl
!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine checkNefisError(success, nefisRetVal, errString)

    ! arguments
    logical, intent(inout)          :: success     ! All over success of nefis-actions in calling function
    integer, intent(in)             :: nefisRetVal ! return code of last nefis-action
    character(len=*), intent(inout) :: errString   ! Error string for (concatenation of) nefis error(s)

    ! external
    integer, external :: NefErr

    ! locals
    integer :: resOfErrorCall
    character(len=1024) :: nefErrorString

    if ( nefisRetVal .ne. 0 ) then
        success = .false.
        resOfErrorCall = NefErr(0, nefErrorString)
        if ( resOfErrorCall == 0 ) then
            write(errString, '(A,'','',I7,'': '',A)') trim(errString), nefisRetVal, trim(nefErrorString)
        else
            write(errString, '(A,I7)') trim(errString), nefisRetVal
        endif
    endif
   
end subroutine checkNefisError


