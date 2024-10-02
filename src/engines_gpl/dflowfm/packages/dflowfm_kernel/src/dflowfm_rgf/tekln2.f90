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
module m_tekln2
   implicit none
contains
      subroutine TEKLN2(X, Y, mmax, nmax, M1, N1, M2, N2, NCOL)
!     TEKEN EEN LIJN IN GRID (MET CIRKELS ROND DE UITEINDEN)
         use m_missing, only: xymis
         use m_cirr
         use m_set_col
         use m_movabs
         use m_lnabs

         integer :: mmax, nmax, m1, n1, m2, n2, ncol
         double precision :: X(MMAX, NMAX), Y(MMAX, NMAX)

         integer :: istart, i, j, in, jn

         call SETCOL(NCOL)
         ISTART = 0
         if (M1 /= 0) call CIRR(X(M1, N1), Y(M1, N1), NCOL)
         if (M2 /= 0) call CIRR(X(M2, N2), Y(M2, N2), NCOL)
         if (M1 /= 0 .and. M2 /= 0) then
            IN = sign(1, M2 - M1)
            JN = sign(1, N2 - N1)
            do I = M1, M2, IN
               do J = N1, N2, JN
                  if (X(I, J) /= XYMIS) then
                     if (ISTART == 0) then
                        call MOVABS(X(I, J), Y(I, J))
                        ISTART = 1
                     else
                        call LNABS(X(I, J), Y(I, J))
                     end if
                  else
                     ISTART = 0
                  end if
               end do
            end do
         end if
         return
      end subroutine tekln2
end module m_tekln2
