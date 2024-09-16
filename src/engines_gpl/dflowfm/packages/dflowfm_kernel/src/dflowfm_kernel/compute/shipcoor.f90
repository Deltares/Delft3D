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
module m_shipcoor
   implicit none
contains
   subroutine shipcoor(n, sx1, sy1, sx2, sy2) ! get absolute shipcoordinates in sx2, sy2), input sx1, sy1 : ( 1, -1) = (bow  , portside )
      use m_ship !                                                             (-1,  1) = (stern, starboard)
      double precision :: sx1, sx2, sy1, sy2, css, sns
      integer :: n
      css = cos(shi(n)); sns = sin(shi(n))
      sx2 = shx(n) + sx1 * shL(n) * css - sy1 * shb(n) * sns ! square ship
      sy2 = shy(n) + sx1 * shL(n) * sns + sy1 * shb(n) * css
   end subroutine shipcoor
end module m_shipcoor
