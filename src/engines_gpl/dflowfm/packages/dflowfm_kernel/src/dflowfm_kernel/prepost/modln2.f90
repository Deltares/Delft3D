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

      SUBROUTINE MODLN2( X, Y, Z, MMAX, NUMPI, MP, XP, YP, NPUT)
      USE M_MISSING
      implicit none
!     WIJZIG AANTAL PUNTEN OP EEN ENKELE LIJN
!     DELETE , NPUT = -2
!     OF INSERT, NPUT = -1
!     DELETE ENTIRE LINE, -3
!     DELETE ALL EXCEPT SELECTED LINE, -4
      double precision :: X(MMAX), Y(MMAX), Z(MMAX)
      integer :: MMAX, NUMPI, MP, nput
      double precision :: XP, YP, ZP
      integer :: i
      integer :: istart
      integer :: j
      integer :: k
      integer :: jstart, jend

      ZP = DMISS  ! set Z-value of newly inserted points to DMISS

      IF (NPUT .EQ. -2) THEN
!        DELETE PUNT
         IF (MP .EQ. 0) THEN
            CALL OKAY(0)
         ELSE IF (NUMPI .EQ. 2) THEN
!           LAATSTE TWEE PUNTEN VAN EEN SPLINE, DUS DELETE DE HELE SPLIN
            do I = 1,MMAX
               X(I) = 0
               Y(I) = 0
               Z(I) = 0
            end do
            NUMPI = 0
         ELSE
!           EEN WILLEKEURIG ANDER PUNT
            NUMPI = NUMPI - 1
            do j = MP,NUMPI
               X(J) = X(J+1)
               Y(J) = Y(J+1)
               Z(J) = Z(J+1)
         end do
         ENDIF
      ELSE IF (NPUT .EQ. -1) THEN
!        ADD PUNT
         IF (NUMPI .LT. MMAX) THEN
            IF (MP .NE. 0) THEN
!              EEN NIEUW PUNT OP EEN BESTAANDE SPLINE TUSSENVOEGEN
               NUMPI = NUMPI + 1
               do J = NUMPI,MP+2,-1
                  X(J) = X(J-1)
                  Y(J) = Y(J-1)
                  Z(J) = Z(J-1)
end do
               MP    = MP + 1
               X(MP) = XP
               Y(MP) = YP
               Z(MP) = ZP
               CALL OKAY(0)
            ELSE
               NUMPI = NUMPI + 1
               MP    = NUMPI
               X(MP) = XP
               Y(MP) = YP
               Z(MP) = ZP
               CALL OKAY(0)
            ENDIF
         ELSE
            CALL OKAY(0)
         ENDIF
      ELSE IF (NPUT .EQ. -3 .or. NPUT.EQ.-4) THEN
         IF ( NPUT .EQ. -3 ) THEN
   !        DELETE ENTIRE LINE
            K = MP
      40    CONTINUE
            IF (K .LE. MMAX) THEN
               IF (X(K) .NE. dmiss) THEN
                  X(K) = dmiss
                  K    = K + 1
                  GOTO 40
               ENDIF
            ENDIF

            K = MP - 1
      50    CONTINUE
            IF (K .GE. 1) THEN
               IF (X(K) .NE. dmiss) THEN
                  X(K) = dmiss
                  K    = K - 1
                  GOTO 50
               ENDIF
            ENDIF
         ELSE IF ( NPUT .EQ. -4 ) THEN
!           DELETE ALL EXCEPT SELECTED LINE

!           get start and end indices of the line
            call get_polstartend(MMAX, X, Y, MP, jstart, jend)

!           delete leading part in array
            if ( jstart.gt.1 ) then
               x(1:jstart-1) = DMISS
            end if

!           delete trailing part in array
            if ( jend.lt.MMAX) then
               x(jend+1:MMAX) = DMISS
            end if
         END IF

         K = 0
         ISTART = 0
         do I = 1,NUMPI
            IF (X(I) .NE. dmiss) THEN
               K      = K + 1
               X(K)   = X(I)
               Y(K)   = Y(I)
               Z(K)   = Z(I)
               ISTART = 1
            ELSE IF (ISTART .EQ. 1) THEN
               K      = K + 1
               X(K)   = X(I)
               Y(K)   = Y(I)
               Z(K)   = Z(I)
               ISTART = 0
            ENDIF
end do
         NUMPI = K
         ! If numpi points to a dmiss element at tail, decrement it.
         if (k > 0 .and. istart == 0) then
            numpi = numpi - 1
         end if
         mp = 0 ! Reset active point (subsequent "insert" will continue at tail of last polyline)
      ENDIF
      CALL DISPNODE(MP)
      RETURN
      END SUBROUTINE MODLN2
