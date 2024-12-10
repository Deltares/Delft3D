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

module m_reaobs2stat

   implicit none

   private

   public :: reaobs2stat

contains

   subroutine reaobs2stat(mobs, mout) ! convert d3d obs file to model independent
      use precision, only: dp
      use m_grid
      implicit none
      integer :: mobs, mout
      real(kind=dp) :: xce, yce
      character(len=132) :: rec
      character(len=20) :: name
      integer :: m, n

10    read (mobs, '(a)', end=999) rec

      read (rec(1:), '(a)') name
      read (rec(21:), *) m, n

      xce = 0.25d0 * (xc(m - 1, n) + xc(m - 1, n - 1) + xc(m, n) + xc(m, n - 1))
      yce = 0.25d0 * (yc(m - 1, n) + yc(m - 1, n - 1) + yc(m, n) + yc(m, n - 1))

      write (mout, *) xce, yce, name

      goto 10

999   call doclose(mobs)
      call doclose(mout)

   end subroutine reaobs2stat

end module m_reaobs2stat
