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

module m_isquadadjacenttoline

implicit none

private

public :: isquadadjacenttoline

contains

  subroutine isquadadjacenttoline(L1, n, L2)
     use precision, only: dp
     use m_adjacent, only: adjacent
     use m_netw

     integer :: L1, n, L2

     integer :: ja
     integer :: l
     integer :: ll
     integer :: k1k, k2k

     real(kind=dp) :: x1, y1, x2, y2, x3, y3, x4, y4

     L2 = 0
     x1 = xk(kn(1, L1)); y1 = yk(kn(1, L1))
     x2 = xk(kn(2, L1)); y2 = yk(kn(2, L1))
     do ll = 1, 4
        L = netcell(n)%lin(LL)
        x3 = xk(kn(1, L)); y3 = yk(kn(1, L))
        x4 = xk(kn(2, L)); y4 = yk(kn(2, L))
        call adjacent(x1, y1, x2, y2, x3, y3, x4, y4, ja, k1k, k2k)
        if (ja == 1) then
           L2 = L
           return
        end if
     end do
  end subroutine isquadadjacenttoline

end module m_isquadadjacenttoline
