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

!> assign indices to the nodes of the cell neighboring cell kcell in kdir direction
module m_assignij
use m_checkvalidnode, only: checkvalidnode

implicit none

private

public :: assignij

contains

subroutine assignij(kcell, kdir, kneighbor, ic, jc)
!---------------------------------------------------------
!  kdir: direction of neighboring cell
!    1 : -i, left
!    2 : -j, bottom
!    3 : +i, right
!    4 : +j, top
!---------------------------------------------------------
   use m_netw
   use m_grid
   use unstruc_messages
   use m_missing
   use m_find_common_node

   integer :: kcell !< cell number
   integer :: kdir !< direction
   integer :: kneighbor !< neighboring cell number

   integer, dimension(*) :: ic, jc ! indices (i,j) of the nodes

   integer :: L
   integer :: kself, kkneighbor, kkself
   integer :: Lneighbor1, Lneighbor3
   integer :: k1, k2, icnew1, icnew2, jcnew1, jcnew2

   integer, dimension(4), parameter :: Di = (/-1, 0, 1, 0/)
   integer, dimension(4), parameter :: Dj = (/0, -1, 0, 1/)

   integer :: icell, jcell, nodes(4)
   integer :: ilink
   integer :: node1, node2, othernode
! integer, parameter                :: IMISS = -999999

   logical :: lconflict

!---------------------------------------------------------
! find the active link based on node indices
!---------------------------------------------------------
   nodes = netcell(kcell)%nod
   icell = minval(ic(nodes))
   jcell = minval(jc(nodes))

   select case (kdir)
   case (1)
      node1 = ijc(icell, jcell)
      node2 = ijc(icell, jcell + 1)
   case (2)
      node1 = ijc(icell, jcell)
      node2 = ijc(icell + 1, jcell)
   case (3)
      node1 = ijc(icell + 1, jcell)
      node2 = ijc(icell + 1, jcell + 1)
   case (4)
      node1 = ijc(icell + 1, jcell + 1)
      node2 = ijc(icell, jcell + 1)
   end select

   do ilink = 1, nmk(node1)
      L = nod(node1)%lin(ilink)
      othernode = kn(1, L) + kn(2, L) - node1
      if (othernode == node2) exit
   end do

   if (lnn(L) /= 2 .or. node2 /= othernode) then
      kneighbor = 0
      return
   end if

!---------------------------------------------------------
! find neighboring cell kneighbor for link L w.r.t. cell k
!---------------------------------------------------------
   kneighbor = lne(1, L) + lne(2, L) - kcell

   if (netcell(kneighbor)%n /= 4 .or. cellmask(kneighbor) == 0) then ! only consider quads
      cellmask(kneighbor) = 0
      kneighbor = 0
      return
   end if

!---------------------------------------------------------
! find active link kkself w.r.t. neighboring cell
!---------------------------------------------------------
   kkself = minloc(abs(netcell(kneighbor)%lin - L), 1)

!---------------------------------------------------------
! Find links numbers in neighboring cell
!---------------------------------------------------------
   kkneighbor = 1 + mod(kkself, 4)
   Lneighbor1 = netcell(kneighbor)%lin(kkneighbor)

   kkneighbor = 1 + mod(kkself + 2, 4)
   Lneighbor3 = netcell(kneighbor)%lin(kkneighbor)

!---------------------------------------------------------
! determine new node indices
!---------------------------------------------------------
   call find_common_node(L, Lneighbor1, kself)
   k1 = sum(kn(1:2, Lneighbor1)) - kself
   icnew1 = ic(kself) + Di(kdir)
   jcnew1 = jc(kself) + Dj(kdir)

   call find_common_node(L, Lneighbor3, kself)
   k2 = sum(kn(1:2, Lneighbor3)) - kself
   icnew2 = ic(kself) + Di(kdir)
   jcnew2 = jc(kself) + Dj(kdir)

!---------------------------------------------------------
! check for conflicts
!---------------------------------------------------------
   if ((ic(k1) /= IMISS .and. ic(k1) /= icnew1) .or. &
       (jc(k1) /= IMISS .and. jc(k1) /= jcnew1) .or. &
       (ic(k2) /= IMISS .and. ic(k2) /= icnew2) .or. &
       (jc(k2) /= IMISS .and. jc(k2) /= jcnew2)) then
      lconflict = .true.
   else
      lconflict = .false.
   end if

   if (.not. lconflict) call checkvalidnode(k1, icnew1, jcnew1, lconflict)

   if (.not. lconflict) call checkvalidnode(k2, icnew2, jcnew2, lconflict)

   if (.not. lconflict) then
!---------------------------------------------------------
! no conflicts: assign indices
!---------------------------------------------------------
      ic(k1) = icnew1
      jc(k1) = jcnew1
      ic(k2) = icnew2
      jc(k2) = jcnew2

      ijc(icnew1, jcnew1) = k1
      ijc(icnew2, jcnew2) = k2
   else
!---------------------------------------------------------
! conflicts: do not assign indices and  deactive neighbor
!---------------------------------------------------------
      cellmask(kneighbor) = 0
      kneighbor = 0
   end if

end subroutine

end module m_assignij
