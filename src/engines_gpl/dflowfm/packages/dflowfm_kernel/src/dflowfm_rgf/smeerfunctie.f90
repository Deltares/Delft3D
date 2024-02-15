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

      SUBROUTINE SMEERFUNCTIE(I,J,MP,NP,FR,IN,JN)
      implicit none
      integer :: i, j, mp, np, in, jn
      double precision :: fr

      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      double precision :: pi, phi, fri, frj
      PI = ACOS(-1d0)

      IF (I .EQ. MP) THEN
         PHI = 0
      ELSE IF (I .GT. MP .AND. I .LT. MB(4) ) THEN
         PHI = PI*dble(I - MP)/dble( MB(4) - MP )
      ELSE IF (I .LT. MP .AND. I .GT. MB(3)) THEN
         PHI = PI*dble(MP - I)/dble( MP - MB(3) )
      ELSE
         PHI = PI
      ENDIF
      FRI = (1 + COS(PHI) ) / 2

      IF (J .EQ. NP) THEN
         PHI = 0
      ELSE IF (J .GT. NP .AND. J .LT. NB(4) ) THEN
         PHI = PI*dble(J - NP)/dble( NB(4) - NP )
      ELSE IF (J .LT. NP .AND. J .GT. NB(3)) THEN
         PHI = PI*dble(NP - J)/dble( NP - NB(3) )
      ELSE
         PHI = PI
      ENDIF
      FRJ = (1 + COS(PHI) ) / 2

      IF (IN .EQ. 1 .AND. JN .EQ. 1) THEN
         FR = SQRT(FRI*FRJ)
      ELSE IF (JN .EQ. 1) THEN
         FR = FRJ
      ELSE IF (IN .EQ. 1) THEN
         FR = FRI
      ENDIF

      RETURN
      END subroutine smeerfunctie
