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

      SUBROUTINE MAKEF(XR,YR,MMAX,NMAX) ! naar rekenvlak SUBROUTINE MAKEF
      USE M_SFERIC
      USE M_MISSING
      implicit none
      integer :: mmax,nmax
      DOUBLE PRECISION :: XR(MMAX,NMAX), YR(MMAX,NMAX), FI2
      integer :: i,j
      DO I = 1,MMAX
         DO J = 1,NMAX
            IF (XR(I,J) .NE. DXYMIS) THEN
               FI2     = DG2RD*YR(I,J)
               YR(I,J) = ( 1D0 + SIN(FI2) ) / COS(FI2)
               YR(I,J) = LOG(YR(I,J))
               XR(I,J) = DG2RD*XR(I,J)
            ENDIF
         ENDDO
      ENDDO
      RETURN
      END SUBROUTINE MAKEF
