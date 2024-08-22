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

      subroutine POSITIVEBLOK()
         implicit none
         integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
         common / BLOK / MB(6), NB(6), MB2(6), NB2(6), NPT, NPT2, NPUTO, ITYPE

         integer :: mh, nh, m1, n1, m2, n2, i
         if (NPT <= 1) return

!     IF (ITYPE .EQ. 1) THEN
         if (MB(2) < MB(1)) then
            MH = MB(1)
            MB(1) = MB(2)
            MB(2) = MH
         end if
         if (NB(2) < NB(1)) then
            NH = NB(1)
            NB(1) = NB(2)
            NB(2) = NH
         end if
!     ENDIF

         M1 = MB(1)
         N1 = NB(1)
         M2 = MB(2)
         N2 = NB(2)
         do I = 1, NPT
            M1 = min(MB(I), M1)
            N1 = min(NB(I), N1)
            M2 = max(MB(I), M2)
            N2 = max(NB(I), N2)
         end do
         MB(3) = M1
         NB(3) = N1
         MB(4) = M2
         NB(4) = N2
         return
      end subroutine positiveblok
