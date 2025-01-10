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

!>    move a whole spline
module m_movespline

   implicit none

   private

   public :: movespline

contains

   subroutine movespline(ispline, inode, xp, yp)
      use precision, only: dp
      use m_splines

      integer, intent(in) :: ispline !< spline number
      integer, intent(in) :: inode !< spline control point
      real(kind=dp), intent(in) :: xp, yp !< new active spline control point (np) coordinates

      real(kind=dp) :: dx, dy

      integer :: num

      call nump(ispline, num)

      if (ispline > 0 .and. ispline <= maxspl .and. inode > 0 .and. inode <= num) then
         dx = xp - xsp(ispline, inode)
         dy = yp - ysp(ispline, inode)
         xsp(ispline, 1:maxsplen) = xsp(ispline, 1:maxsplen) + dx
         ysp(ispline, 1:maxsplen) = ysp(ispline, 1:maxsplen) + dy
      end if
      return
   end subroutine movespline

end module m_movespline
