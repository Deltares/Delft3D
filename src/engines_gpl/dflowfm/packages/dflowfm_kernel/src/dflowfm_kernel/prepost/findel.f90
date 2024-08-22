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

  !> Finds the net link number between two net nodes.
  subroutine FINDEL(K1, K2, L1)
     use m_netw
     implicit none
     integer, intent(in) :: K1, K2 !< The two net node numbers between which a net link is searched for.
     integer, intent(out) :: L1 !< The shared netlink between nodes k1 and k2, or 0 when not found.

     integer :: l2
     integer :: n1
     integer :: n2

     do N1 = 1, NMK(K1)
        L1 = NOD(K1)%LIN(N1)
        do N2 = 1, NMK(K2)
           L2 = NOD(K2)%LIN(N2)
           if (L1 == L2) return
        end do
     end do
     L1 = 0
     return
  end subroutine FINDEL
