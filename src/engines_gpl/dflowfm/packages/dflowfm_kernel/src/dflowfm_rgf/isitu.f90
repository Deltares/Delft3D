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
module m_isitu
   implicit none
contains
   subroutine ISITU()
      use precision, only: dp
      use m_grid, only: ijc, ijyes, mc, nc, mmax, nmax, xc, yc
      use m_missing, only: dxymis

!C     IJYES, WELKE CELLEN DOEN MEE 1 OF 0
!C     IJC  , CODE VAN PUNT, ZIE FIGUUR
!C                            0  14   3  13
!C     9  4  3               14  12  10   2
!C     6  1  2                4  10  10   2
!C     8  5  7               11   1   1  12
!C
!C     ALS IK NOG EENS TIJD HEB, ZAL IK DE NUMMERING MOOIER MAKEN
!C

      real(kind=dp) :: x1, x2, x3, x4, x5, x6, x7, x8, x9
      integer :: i, j, i1, i2, i3, i4, IJYES2, IJYES3, IJYES4, jaunconnected

      if (allocated(ijc)) then
         deallocate (ijc)
      end if
      if (allocated(ijyes)) then
         deallocate (ijyes)
      end if
      allocate (ijc(mmax, nmax), ijyes(mmax, nmax))

5     continue
      ijc = 0; ijyes = 0

      do I = 1, MC - 1
         do J = 1, NC - 1
            X1 = Xc(I, J)
            X2 = Xc(I + 1, J)
            X3 = Xc(I + 1, J + 1)
            X4 = Xc(I, J + 1)
            if (X1 /= dXYMIS .and. X2 /= dXYMIS .and. &
                X3 /= dXYMIS .and. X4 /= dXYMIS) IJYES(I, J) = 1
         end do
      end do

      do I = 1, MC
         do J = 1, NC
            X1 = Xc(I, J)
            if (I /= 1) X6 = Xc(I - 1, J)
            if (I /= MC) X2 = Xc(I + 1, J)
            if (J /= 1) X5 = Xc(I, J - 1)
            if (J /= NC) X4 = Xc(I, J + 1)
            if (I /= MC .and. J /= NC) X3 = Xc(I + 1, J + 1)
            if (I /= 1 .and. J /= 1) X8 = Xc(I - 1, J - 1)
            if (I /= 1 .and. J /= NC) X9 = Xc(I - 1, J + 1)
            if (I /= MC .and. J /= 1) X7 = Xc(I + 1, J - 1)
!           POSITIE BENOEMEN
            if (X1 == dXYMIS) then
               IJC(I, J) = 0
            else if (I == 1) then
!              LINKS
               if (J == 1) then
                  IJC(I, J) = 11
               else if (J == NC) then
                  IJC(I, J) = 14
               else if (X5 == dXYMIS) then
                  IJC(I, J) = 11
               else if (X4 == dXYMIS) then
                  IJC(I, J) = 14
               else
                  IJC(I, J) = 4
               end if
            else if (I == MC) then
!              RECHTS
               if (J == 1) then
                  IJC(I, J) = 12
               else if (J == NC) then
                  IJC(I, J) = 13
               else if (X5 == dXYMIS) then
                  IJC(I, J) = 12
               else if (X4 == dXYMIS) then
                  IJC(I, J) = 13
               else
                  IJC(I, J) = 2
               end if
            else if (J == 1) then
!              ONDER
               if (X6 == dXYMIS) then
                  IJC(I, J) = 11
               else if (X2 == dXYMIS) then
                  IJC(I, J) = 12
               else
                  IJC(I, J) = 1
               end if
            else if (J == NC) then
!              BOVEN
               if (X6 == dXYMIS) then
                  IJC(I, J) = 14
               else if (X2 == dXYMIS) then
                  IJC(I, J) = 13
               else
                  IJC(I, J) = 3
               end if
            else
               I1 = IJYES(I, J)
               I2 = IJYES(I - 1, J)
               I3 = IJYES(I - 1, J - 1)
               I4 = IJYES(I, J - 1)
               if (I1 == 1 .and. I2 == 1 .and. I3 == 1 .and. I4 == 1) then
                  IJC(I, J) = 10
               else if (I1 == 0 .and. I2 == 1 .and. I3 == 1 .and. I4 == 1) then
                  IJC(I, J) = 11
               else if (I1 == 1 .and. I2 == 0 .and. I3 == 1 .and. I4 == 1) then
                  IJC(I, J) = 12
               else if (I1 == 1 .and. I2 == 1 .and. I3 == 0 .and. I4 == 1) then
                  IJC(I, J) = 13
               else if (I1 == 1 .and. I2 == 1 .and. I3 == 1 .and. I4 == 0) then
                  IJC(I, J) = 14
               else if (I1 == 1 .and. I2 == 1 .and. I3 == 0 .and. I4 == 0) then
                  IJC(I, J) = 1
               else if (I1 == 1 .and. I2 == 0 .and. I3 == 0 .and. I4 == 1) then
                  IJC(I, J) = 4
               else if (I1 == 0 .and. I2 == 0 .and. I3 == 1 .and. I4 == 1) then
                  IJC(I, J) = 3
               else if (I1 == 0 .and. I2 == 1 .and. I3 == 1 .and. I4 == 0) then
                  IJC(I, J) = 2
               else if (I1 == 1 .and. I2 == 0 .and. I3 == 0 .and. I4 == 0) then
                  IJC(I, J) = 11
               else if (I1 == 0 .and. I2 == 1 .and. I3 == 0 .and. I4 == 0) then
                  IJC(I, J) = 12
               else if (I1 == 0 .and. I2 == 0 .and. I3 == 1 .and. I4 == 0) then
                  IJC(I, J) = 13
               else if (I1 == 0 .and. I2 == 0 .and. I3 == 0 .and. I4 == 1) then
                  IJC(I, J) = 14
               end if
            end if
         end do
      end do

      JAUNCONNECTED = 0
      do I = 2, MC
         do J = 2, NC
            X1 = Xc(I, J)
            if (X1 /= dXYMIS) then
!              ALS ER EEN PUNT IS, MAAR GEEN VAN DE OMLIGGENDE CELLEN IS
!              GESLOTEN
               if (IJYES(I, J) == 0) then
                  IJYES2 = IJYES(I - 1, J)
                  IJYES3 = IJYES(I - 1, J - 1)
                  IJYES4 = IJYES(I, J - 1)
                  if (IJYES2 == 0 .and. IJYES3 == 0 .and. &
                      IJYES4 == 0) then
                     !    WRITE(MDIA,*)'UNCONNECTED POINT AT ',I,J,' DELETED'
                     JAUNCONNECTED = 1
                     Xc(I, J) = dXYMIS
                     Yc(I, J) = dXYMIS
                  end if
               end if
            end if
         end do
      end do

      J = 1
      do I = 2, MC
         X1 = Xc(I, J)
         if (X1 /= dXYMIS) then
            if (IJYES(I - 1, J) == 0 .and. IJYES(I, J) == 0) then
               !  WRITE(MDIA,*)'UNCONNECTED POINT AT ',I,J,' DELETED'
               Xc(I, J) = dXYMIS
               Yc(I, J) = dXYMIS
            end if
         end if
      end do
      I = 1
      do J = 2, NC
         X1 = Xc(I, J)
         if (X1 /= dXYMIS) then
            if (IJYES(I, J - 1) == 0 .and. IJYES(I, J) == 0) then
               !   WRITE(MDIA,*)'UNCONNECTED POINT AT ',I,J,' DELETED'
               Xc(I, J) = dXYMIS
               Yc(I, J) = dXYMIS
            end if
         end if
      end do
      J = 1
      I = 1
      if (IJYES(I, J) == 0 .and. Xc(I, J) /= dXYMIS) then
         ! WRITE(MDIA,*)'UNCONNECTED POINT AT ',I,J,' DELETED'
         Xc(I, J) = dXYMIS
         Yc(I, J) = dXYMIS
      end if
      if (JAUNCONNECTED == 1) then
         goto 5
      end if

      return
   end subroutine ISITU
end module m_isitu
