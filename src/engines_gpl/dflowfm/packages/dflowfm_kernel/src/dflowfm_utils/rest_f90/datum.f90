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

      SUBROUTINE DATUM(DATE)
      implicit none
      integer :: iyear, month, iday, ihour, minute, isecnd
      CHARACTER DATE*20
!              1  4  7   11 14 17
      DATE = 'hh:mm:ss, dd-mm-yyyy'

      call dateandtimenow(iyear, month, iday, ihour, minute, isecnd)

      WRITE(DATE( 1:2 ),'(I2.2)') IHOUR
      WRITE(DATE( 4:5 ),'(I2.2)') MINUTE
      WRITE(DATE( 7:8 ),'(I2.2)') ISECND
      WRITE(DATE(11:12),'(I2.2)') IDAY
      WRITE(DATE(14:15),'(I2.2)') MONTH
      WRITE(DATE(17:20),'(I4)') IYEAR
      RETURN
      END
