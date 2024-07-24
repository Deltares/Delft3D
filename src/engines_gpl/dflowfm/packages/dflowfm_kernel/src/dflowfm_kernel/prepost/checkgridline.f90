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

!> check if a connection from node1 to node2 exists
subroutine checkgridline(node1, node2, lconflict)

   use m_netw
   use m_grid
   use m_missing

   implicit none

   integer, intent(in) :: node1, node2 !< nodes

   logical, intent(out) :: lconflict !< .false. if connected, .true. otherwise

   integer :: ilink, link, othernode1

   logical :: doit ! determines whether the link neighbors a quad (.true.) or not (.false.)

! integer, parameter   :: IMISS = -999999

   lconflict = .true. ! .false. if the (i,j)-connection is a valid connection

   do ilink = 1, nmk(node1)
      link = nod(node1)%lin(ilink)
      othernode1 = kn(1, link) + kn(2, link) - node1

! select links adjacent to at least one quad only
      doit = .false.
      if (lnn(link) > 0) doit = (netcell(lne(1, link))%n == 4)
      if (lnn(link) > 1) doit = doit .or. (netcell(lne(2, link))%n == 4)

      if (doit) then
         if (othernode1 == node2) then ! valid connection
            lconflict = .false.
            return
         end if
      end if
   end do

end subroutine checkgridline
