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

      subroutine insertsamples(L1, L2)
         use precision, only: dp
         use m_samples
         use m_gridsettings, only: mfac
         implicit none
         integer :: L1, L2
         integer :: k
         real(kind=dp) :: aa, bb

         do k = 1, mfac
            ns = ns + 1
            call increasesam(ns)
            aa = dble(k) / dble(mfac + 1); bb = 1d0 - aa
            xs(ns) = bb * xs(L1) + aa * xs(L2)
            ys(ns) = bb * ys(L1) + aa * ys(L2)
            zs(ns) = bb * zs(L1) + aa * zs(L2)
         end do

!     user is editing samples: mark samples as unstructured
         MXSAM = 0
         MYSAM = 0
         IPSTAT = IPSTAT_NOTOK

      end subroutine insertsamples
