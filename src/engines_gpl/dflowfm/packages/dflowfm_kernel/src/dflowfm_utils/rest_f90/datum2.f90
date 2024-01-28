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

      SUBROUTINE DATUM2(DATE)
      use unstruc_display, only : jadatetime
      use string_module, only: get_dirsep
      implicit none
      integer :: iyear, month, iday, ihour, minute, isecnd
      CHARACTER DATE*20

!              1  4  7   11 14 17

      if (jadatetime == 0) then
         DATE = get_dirsep()
      else
         DATE = '_yymmddhhmmss'//get_dirsep()

         call dateandtimenow(iyear, month, iday, ihour, minute, isecnd)

         WRITE(DATE(2:3),'(I2.2)') IYEAR - 2000
         WRITE(DATE(4:5),'(I2.2)') month
         WRITE(DATE(6:7),'(I2.2)') iday
         WRITE(DATE(8:9),'(I2.2)') Ihour
         WRITE(DATE(10:11),'(I2.2)') minute
         WRITE(DATE(12:13),'(I2.2)') isecnd
      endif
      RETURN
      END
