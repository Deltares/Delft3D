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

  !   call nextcel(np,La,npb,k1b,k2b,Lb)
  subroutine nextcel(np, LL, npa, k1a, k2a, La) ! give face, link and nodes, opposite to plakrand LL of cel np
     use m_netw
     implicit none
     integer :: LL, np, La, npa, k1a, k2a

     La = 0; npa = 0; k1a = 0; k2a = 0

     if (np == 0) return

     if (lne(1, LL) == np) then ! find cell behind current np, eindplaat
        npa = lne(2, LL)
     else if (lne(2, LL) == np) then
        npa = lne(1, LL)
     end if

     if (npa == 0) return

     call tegenovernodesandlink(npa, LL, k1a, k2a, La)

  end subroutine nextcel
