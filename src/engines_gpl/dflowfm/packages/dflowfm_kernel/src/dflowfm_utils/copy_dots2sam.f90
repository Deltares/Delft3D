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

!  copy dots to samples
module m_copy_dots2sam

implicit none

private

public :: copy_dots2sam

contains

   subroutine copy_dots2sam()
      use m_samples
      use m_plotdots

      integer :: i

      if (numdots < 1) return

      call increasesam(Ns + numdots)

      do i = 1, numdots
         Ns = Ns + 1
         xs(Ns) = xdots(i)
         ys(Ns) = ydots(i)
         zs(Ns) = zdots(i)
      end do

!     clear dots
      numdots = 0

      return
   end subroutine copy_dots2sam

end module m_copy_dots2sam
