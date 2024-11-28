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

module m_arcinfo
   use precision, only: dp
   implicit none
   private
   real(kind=dp), allocatable, public :: D(:, :)
   integer, public :: MCa = 0, NCa
   real(kind=dp), public :: X0 = 0.0_dp, Y0 = 0.0_dp, DXa = 1.0_dp, DYa = 1.0_dp, RMIS = -999.0_dp
   integer, public :: MAXARCTILE = 9000 * 4500 ! if arc is larger, tile the arc data
   integer, public :: MAXSAMARC = 2000 * 1500 ! if arc is larger, do not make samples xs,ys,zs,ns, but keep d(mca,nca)
end module m_arcinfo
