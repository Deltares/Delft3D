!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2025.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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
!-------------------------------------------------------------------------------
module dhydrology_error
   implicit none
   
   integer, parameter :: DHYD_NOERR         = 0   !< Success code.
   integer, parameter :: DHYD_GENERICERROR  = 1   !< Error without further details.   
   
   contains
   
   !> Returns a human-readable error string for a given integer error code.
   function dhyd_strerror(ierr) result(errorstring)
      use MessageHandling, only: msgbuf
      
      integer,          intent(in)  :: ierr        !< The error code for which the error string should be returned.
      character(len=:), allocatable :: errorstring !< The string variable in which the error text will be put (already trimmed).
      
      select case (ierr)
      case (DHYD_NOERR)
         errorstring = 'No error'
      case (DHYD_GENERICERROR)
         errorstring = 'Generic error'
      case default
         write (msgbuf, '(a,i0,a)') 'Unknown error (', ierr, ')'
         errorstring = trim(msgbuf)
      end select
   end function dhyd_strerror

end module dhydrology_error