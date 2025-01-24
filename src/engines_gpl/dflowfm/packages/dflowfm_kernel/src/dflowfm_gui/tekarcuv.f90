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

module m_tekarcuv

   implicit none

contains

   subroutine tekarcuv(vfac, met)
      use precision, only: dp
      use m_arrowsxy
      use M_arcuv
      use m_htext
      use m_set_col
      implicit none
      real(kind=dp) :: vfac
      integer :: met

      integer :: mx, nx, i, j

      mx = size(arcuv, 2)
      nx = size(arcuv, 3)
      do i = 1, mx
         do j = 1, nx
            call setcol(221)
            if (met == 6) then
               call arrowsxy(arcuv(1, i, j), arcuv(2, i, j), arcuv(3, i, j), arcuv(4, i, j), 50 * VFAC)
            else
               call htext(arcuv(3, i, j), arcuv(1, i, j), arcuv(2, i, j))
            end if
         end do
      end do
   end subroutine tekarcuv

end module m_tekarcuv
