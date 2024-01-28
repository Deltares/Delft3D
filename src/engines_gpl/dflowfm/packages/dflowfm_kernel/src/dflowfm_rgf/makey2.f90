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

      SUBROUTINE MAKEY2(XR,YR,XO,YO,MMAX,NMAX)  ! Voor tekenen bij JSFERIC SUBROUTINE MAKEY2
      USE M_SFERIC
      USE M_MISSING
      implicit none
      DOUBLE PRECISION :: XR(MMAX,NMAX), YR(MMAX,NMAX), &
                          XO(MMAX,NMAX), YO(MMAX,NMAX), FI2
      integer :: mmax,nmax
      integer :: i,j



      DO I = 1,MMAX
         DO J = 1,NMAX
            IF (XR(I,J) .NE. XYMIS) THEN
               FI2     = ATAN(SINH(YR(I,J) ) )
               YO(I,J) = RD2DG*FI2
               XO(I,J) = RD2DG*XR(I,J)
            ELSE
               XO(I,J) = XYMIS
               YO(I,J) = XYMIS
            ENDIF
         ENDDO
      ENDDO
      RETURN
      END SUBROUTINE MAKEY2
