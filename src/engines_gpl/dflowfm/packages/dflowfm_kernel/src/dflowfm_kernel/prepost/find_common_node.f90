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

!> return common node of links L1 and L2
subroutine find_common_node(L1, L2, node)

   use m_netw
   use m_missing

   implicit none

   integer, intent(in) :: L1, L2 !< links
   integer, intent(out) :: node !< common node

   integer, dimension(4) :: a ! dummy array with nodes of L1 and L2
! integer, parameter    :: IMISS = -999999

   a(1:2) = kn(1:2, L1)
   a(3:4) = kn(1:2, L2)

   do
      node = IMISS

      if (a(1) == a(3) .or. a(1) == a(4)) node = a(1)
      if (a(2) == a(3) .or. a(2) == a(4)) node = a(2)

      if (node /= IMISS) exit

      write (6, *) 'find_common_node: no common node found'
      exit
   end do

end subroutine find_common_node
