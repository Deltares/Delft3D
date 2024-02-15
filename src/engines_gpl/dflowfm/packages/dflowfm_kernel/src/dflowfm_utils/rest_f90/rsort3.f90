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

      SUBROUTINE RSORT3(X, Y, Z, N)
      implicit none
      integer :: j
      integer :: j1
      integer :: k0
      integer :: kk
      integer :: lk
      integer :: n
      integer :: nm
      double precision :: temp

      double precision :: X(N), Y(N), Z(N)
      IF (N .EQ. 0) RETURN
      LK = N / 2
      K0 = LK
      KK = K0

20    J  = 2 * KK
      J1 = J + 1

30    IF (   J1 .LE. N ) THEN

         IF ( X(J) .LT. X(J1) )  J  = J1

      ENDIF

      IF ( X(KK) .LT. X(J) ) THEN

         TEMP  = X(J)
         X(J)  = X(KK)
         X(KK)  = TEMP

         TEMP  = Y(J)
         Y(J)  = Y(KK)
         Y(KK)  = TEMP

         TEMP  = Z(J)
         Z(J)  = Z(KK)
         Z(KK)  = TEMP

         IF ( J .LE. LK ) THEN
            KK = J
            GOTO 20
         ENDIF

      ENDIF

      K0 = K0 - 1

      IF ( K0 .NE. 0 ) THEN
         KK  = K0
         J  = 2 * KK
         J1 = J + 1
         GOTO 30
      ENDIF

      NM = N

65    TEMP  = X(1)
      X(1)  = X(NM)
      X(NM) = TEMP

      TEMP  = Y(1)
      Y(1)  = Y(NM)
      Y(NM) = TEMP

      TEMP  = Z(1)
      Z(1)  = Z(NM)
      Z(NM) = TEMP

      NM = NM - 1
      IF ( NM .EQ. 1 ) RETURN

      KK  = 1

70    J  = 2 * KK
      J1 = J + 1

      IF (    J .GT. NM ) GOTO 65

      IF ( J1 .LE. NM .AND.  X(J) .LT. X(J1) )  J = J1

      IF ( X(KK) .GE. X(J) ) GOTO 65

      TEMP  = X(J)
      X(J)  = X(KK)
      X(KK)  = TEMP

      TEMP  = Y(J)
      Y(J)  = Y(KK)
      Y(KK)  = TEMP

      TEMP  = Z(J)
      Z(J)  = Z(KK)
      Z(KK)  = TEMP

      KK = J

      GOTO 70

      END
