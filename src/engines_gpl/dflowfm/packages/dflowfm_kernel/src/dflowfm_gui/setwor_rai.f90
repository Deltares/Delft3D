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

module m_setwor_rai

implicit none

contains

 subroutine setwor_rai(xs1, ys1, xs2, ys2, xw1, yw1, xw2, yw2)
    use m_setwor
    use m_raaitek
    use m_view_port
    implicit none
    real :: xs1, ys1, xs2, ys2
    double precision :: xw1, yw1, xw2, yw2
    call viewport(xs1, ys1, xs2, ys2)
    call setwor(xw1, yw1, xw2, yw2)
    xs1m = xs1
    ys1m = ys1
    xs2m = xs2
    ys2m = ys2
    xw1m = xw1
    yw1m = yw1
    xw2m = xw2
    yw2m = yw2
 end subroutine setwor_rai

end module m_setwor_rai
