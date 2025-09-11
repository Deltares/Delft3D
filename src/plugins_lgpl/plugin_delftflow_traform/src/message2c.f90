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
subroutine message2c(error_message, error_message_c)
   use iso_c_binding, only: c_char, c_int, C_NULL_CHAR
   implicit none
   integer(c_int), parameter :: MAXSTRINGLEN = 257
   
   character(len=*), intent(in) :: error_message
   character(kind=c_char), intent(out) :: error_message_c(MAXSTRINGLEN) 
   !
   integer :: i
   !
   do i=1,len(error_message)
      error_message_c(i) = error_message(i:i)
   enddo
   error_message_c(len(error_message)+1) = C_NULL_CHAR
   !
end subroutine message2c