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

!> Constructs the set of crossed flow links for a single path on the
!! current *network* geometry. (Used for thin dams.)
!!
!! Input is a path with path coordinates in xp,yp.
!! Output path contains additional link numbers in kn and edge
!! coordinates in xk,yk.
!!
!! \see crspath_on_flowgeom, thindams_on_netgeom
subroutine crspath_on_netgeom(path)
   use m_crspath
   use network_data
   implicit none
   type(tcrspath), intent(inout) :: path !< Cross section path that must be imposed on network geometry.

   integer :: L, isactive
   double precision :: xza, yza, xzb, yzb

   path%lnx = 0 ! Reset link administration for this path.

!   Loop across all net links
   do L = 1, numl
      call get_link_neighboringcellcoords(L, isactive, xza, yza, xzb, yzb)
      if (isactive /= 1) cycle

      call crspath_on_singlelink(path, L, xk(kn(1, L)), yk(kn(1, L)), xk(kn(2, L)), yk(kn(2, L)), xza, yza, xzb, yzb, 1)
   end do
end subroutine crspath_on_netgeom
