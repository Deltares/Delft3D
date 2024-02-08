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

      SUBROUTINE HITEXT(IVAL,X,Y)
      implicit none
      integer :: ival
      integer :: l
      integer :: ncolnow
      double precision :: x
      double precision :: y
!     INTEGER grafisch scherm in current color
      CHARACTER TEX*8
      COMMON /COLNOW/ NCOLNOW
      IF (NCOLNOW .GE. 0) THEN
         IF (abs(IVAL) < 100) THEN
            WRITE(TEX,'(I3)') IVAL
         ELSE IF (abs(IVAL) < 10000) THEN
            WRITE(TEX,'(I5)') IVAL
         ELSE
            WRITE(TEX,'(I8)') IVAL
         ENDIF
         L = len_trim(TEX)
         CALL DRAWTEXT(real(X),real(Y), TEX(1:L))
      ENDIF
      RETURN
      END
