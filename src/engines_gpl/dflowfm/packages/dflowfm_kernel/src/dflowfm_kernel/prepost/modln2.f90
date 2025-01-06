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

module m_modln2

implicit none

private

public :: modln2

contains

      subroutine MODLN2(X, Y, Z, MMAX, NUMPI, MP, XP, YP, NPUT)
         use m_get_polstartend, only: get_polstartend
         use precision, only: dp
         use m_missing, only: dmiss
         use m_okay
         use m_dispnode
!     WIJZIG AANTAL PUNTEN OP EEN ENKELE LIJN
!     DELETE , NPUT = -2
!     OF INSERT, NPUT = -1
!     DELETE ENTIRE LINE, -3
!     DELETE ALL EXCEPT SELECTED LINE, -4
         integer :: MMAX, NUMPI, MP, nput
         real(kind=dp) :: X(MMAX), Y(MMAX), Z(MMAX)
         real(kind=dp) :: XP, YP, ZP
         integer :: i
         integer :: istart
         integer :: j
         integer :: k
         integer :: jstart, jend

         ZP = DMISS ! set Z-value of newly inserted points to DMISS

         if (NPUT == -2) then
!        DELETE PUNT
            if (MP == 0) then
               call OKAY(0)
            else if (NUMPI == 2) then
!           LAATSTE TWEE PUNTEN VAN EEN SPLINE, DUS DELETE DE HELE SPLIN
               do I = 1, MMAX
                  X(I) = 0
                  Y(I) = 0
                  Z(I) = 0
               end do
               NUMPI = 0
            else
!           EEN WILLEKEURIG ANDER PUNT
               NUMPI = NUMPI - 1
               do j = MP, NUMPI
                  X(J) = X(J + 1)
                  Y(J) = Y(J + 1)
                  Z(J) = Z(J + 1)
               end do
            end if
         else if (NPUT == -1) then
!        ADD PUNT
            if (NUMPI < MMAX) then
               if (MP /= 0) then
!              EEN NIEUW PUNT OP EEN BESTAANDE SPLINE TUSSENVOEGEN
                  NUMPI = NUMPI + 1
                  do J = NUMPI, MP + 2, -1
                     X(J) = X(J - 1)
                     Y(J) = Y(J - 1)
                     Z(J) = Z(J - 1)
                  end do
                  MP = MP + 1
                  X(MP) = XP
                  Y(MP) = YP
                  Z(MP) = ZP
                  call OKAY(0)
               else
                  NUMPI = NUMPI + 1
                  MP = NUMPI
                  X(MP) = XP
                  Y(MP) = YP
                  Z(MP) = ZP
                  call OKAY(0)
               end if
            else
               call OKAY(0)
            end if
         else if (NPUT == -3 .or. NPUT == -4) then
            if (NPUT == -3) then
               !        DELETE ENTIRE LINE
               K = MP
40             continue
               if (K <= MMAX) then
                  if (X(K) /= dmiss) then
                     X(K) = dmiss
                     K = K + 1
                     goto 40
                  end if
               end if

               K = MP - 1
50             continue
               if (K >= 1) then
                  if (X(K) /= dmiss) then
                     X(K) = dmiss
                     K = K - 1
                     goto 50
                  end if
               end if
            else if (NPUT == -4) then
!           DELETE ALL EXCEPT SELECTED LINE

!           get start and end indices of the line
               call get_polstartend(MMAX, X, Y, MP, jstart, jend)

!           delete leading part in array
               if (jstart > 1) then
                  x(1:jstart - 1) = DMISS
               end if

!           delete trailing part in array
               if (jend < MMAX) then
                  x(jend + 1:MMAX) = DMISS
               end if
            end if

            K = 0
            ISTART = 0
            do I = 1, NUMPI
               if (X(I) /= dmiss) then
                  K = K + 1
                  X(K) = X(I)
                  Y(K) = Y(I)
                  Z(K) = Z(I)
                  ISTART = 1
               else if (ISTART == 1) then
                  K = K + 1
                  X(K) = X(I)
                  Y(K) = Y(I)
                  Z(K) = Z(I)
                  ISTART = 0
               end if
            end do
            NUMPI = K
            ! If numpi points to a dmiss element at tail, decrement it.
            if (k > 0 .and. istart == 0) then
               numpi = numpi - 1
            end if
            mp = 0 ! Reset active point (subsequent "insert" will continue at tail of last polyline)
         end if
         call DISPNODE(MP)
         return
      end subroutine MODLN2

end module m_modln2
