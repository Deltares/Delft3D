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

module m_addclosed_2d_walls

implicit none

private

public :: addclosed_2D_walls

contains

 subroutine addclosed_2D_walls()
    use m_flowgeom
    use m_flow
    use m_missing

    implicit none

    integer :: n, k1
    double precision :: bl1, aa1, hh1

    do n = 1, mxwalls
       k1 = walls(1, n)
       bl1 = walls(13, n)
       aa1 = walls(17, n)
       hh1 = s1(k1) - bl1
       a1(k1) = a1(k1) + aa1
       if (hh1 > 0d0) then
          vol1(k1) = vol1(k1) + aa1 * hh1
       end if
    end do

 end subroutine addclosed_2D_walls

end module m_addclosed_2d_walls
