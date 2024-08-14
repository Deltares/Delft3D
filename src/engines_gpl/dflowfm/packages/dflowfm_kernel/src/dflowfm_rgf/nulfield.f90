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

      subroutine NULFIELD(X, Y, mmax, nmax)
         use m_missing
         implicit none
         integer :: mmax, nmax
         double precision :: X(MMAX, NMAX), Y(MMAX, NMAX)
         integer :: MB, NB, MB2, NB2, NPT, NPT2, NPUTO, ITYPE
         common / BLOK / MB(6), NB(6), MB2(6), NB2(6), NPT, NPT2, NPUTO, ITYPE

         integer :: i, j

         do I = MB(3), MB(4)
            do J = NB(3), NB(4)
               X(I, J) = XYMIS
               Y(I, J) = 0d0
            end do
         end do
         return
      end subroutine nulfield
