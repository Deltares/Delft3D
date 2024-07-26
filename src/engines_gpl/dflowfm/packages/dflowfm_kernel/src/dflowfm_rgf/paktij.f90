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

     subroutine PAKTIJ(T, mmax, nmax, TH, imax, I1, I2, J1, J2, NUM)
        implicit none
!     Haal lijn uit array en geef aantal niet nul NUM
        integer :: mmax, nmax, imax, i1, i2, j1, j2, num
        double precision :: T(MMAX, NMAX), TH(IMAX)
        integer :: i, j, k, ji1
        TH = 0d0
        K = 0
        JI1 = 0
        do I = I1, I2
           do J = J1, J2
              if (T(I, J) /= 0) then
                 K = K + 1
                 TH(K) = T(I, J)
              end if
           end do
        end do
        NUM = K
        return
     end subroutine paktij
