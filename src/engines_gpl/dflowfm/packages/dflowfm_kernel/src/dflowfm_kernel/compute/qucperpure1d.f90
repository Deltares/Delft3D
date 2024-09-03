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

 double precision function QucPerpure1D(n12, L) ! sum of (Q*uc cell centre upwind normal) at side n12 of link L
    use m_flow ! advect the cell center velocities (dimension: m4/s2)
    use m_flowgeom ! leaving the cell = +
    implicit none

    integer, intent(in) :: L !< link number
    integer, intent(in) :: n12 !< index of the node to be processed: 1 (from node) or 2 (to node)

    ! locals
    logical :: process1D !< process node as 1D
    integer :: k12 !< node to be processed

    integer :: LL !< index counting the links connected to k12
    integer :: L2 !< signed link number of link LL of node k12 (positive if link points to node, negative if link points away from node)
    integer :: L2a !< link number of link LL of node k12
    integer :: L2s !< sign of L2

    double precision :: cs !< cosine of link direction (+1 for link in positive x-direction)
    double precision :: sn !< sine of link direction (+1 for link in positive y-direction)
    double precision :: ucin !< representative velocity transported along link

    if (kcu(L) == -1) then
       QucPerpure1D = 0d0
       return
    end if

    k12 = ln(n12, L)
    QucPerpure1D = 0d0
    cs = csu(L)
    sn = snu(L)
    process1D = jaPure1D > 0
    if (jaJunction1D == 0 .and. nd(k12)%lnx > 2) process1D = .false.

    do LL = 1, nd(k12)%lnx ! loop over all attached links
       L2 = nd(k12)%ln(LL)
       L2a = abs(L2)
       L2s = sign(1, L2)

       ! distinguish between vectorial treatment of momentum and pure 1D approach
       if (process1D) then
          ucin = u1Du(L2a)
       else
          ucin = ucxu(L2a) * cs + ucyu(L2a) * sn
       end if

       ! L2s * qa(L2a) > 0d0 for inflowing: positive flow to this node, or negative flow from this node
       ! L2s * qa(L2a) < 0d0 for outflowing: positive flow from this node, or negative flow to this node
       QucPerpure1D = QucPerpure1D - L2s * qa(L2a) * (ucin - u1(L))
    end do

 end function QucPerpure1D
