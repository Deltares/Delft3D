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

module m_islinkadjacenttolink

implicit none

private

public :: islinkadjacenttolink

contains

  subroutine islinkadjacenttolink(L1, L2, ja, k1k, k2k)

     use m_adjacent, only: adjacent
     use m_netw
     use m_sferic, only: jsferic, jasfer3D
     use m_missing, only: dxymis
     use geometry_module, only: dcosphi
     use precision, only: dp

     integer :: L1, L2, ja, k1k, k2k

     real(kind=dp) :: x1, y1, x2, y2, x3, y3, x4, y4
     real(kind=dp) :: cosphi

     x1 = xk(kn(1, L1)); y1 = yk(kn(1, L1))
     x2 = xk(kn(2, L1)); y2 = yk(kn(2, L1))
     x3 = xk(kn(1, L2)); y3 = yk(kn(1, L2))
     x4 = xk(kn(2, L2)); y4 = yk(kn(2, L2))
     call adjacent(x1, y1, x2, y2, x3, y3, x4, y4, ja, k1k, k2k)
     ! Links are close to eachother, now also check whether they're almost parallel
     if (ja == 1) then
        cosphi = dcosphi(x1, y1, x2, y2, x3, y3, x4, y4, jsferic, jasfer3D, dxymis)
        if (abs(cosphi) > .9d0 .and. abs(cosphi) <= 1d0) then
           ja = 1
           if (k1k > 0) k1k = kn(k1k, L2)
           if (k2k > 0) k2k = kn(k2k, L2)
        else
           ja = 0
        end if
     end if
  end subroutine islinkadjacenttolink

end module m_islinkadjacenttolink
