 !----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

!> Wrapper around cellmask_from_polygon_set that uses OpenMP to parallelize the loop over all points if not in MPI mode
module m_pol_to_cellmask
   use precision, only: dp
   use m_cellmask_from_polygon_set, only: cellmask_from_polygon_set_init, cellmask_from_polygon_set_cleanup, cellmask_from_polygon_set

   implicit none

   private

   public :: pol_to_cellmask

contains

   subroutine pol_to_cellmask()
      use network_data, only: cellmask, nump1d2d, npl, nump, xzw, yzw, xpl, ypl, zpl
      use m_alloc, only: realloc

      integer :: k

      if (NPL == 0) then
         return
      end if

      call realloc(cellmask, nump1d2d, keepexisting=.false., fill=0)

      call cellmask_from_polygon_set_init(NPL, xpl, ypl, zpl)

      !> Dynamic scheduling in case of unequal work, chunksize guided
      !$OMP PARALLEL DO SCHEDULE(GUIDED)
      do k = 1, nump
         cellmask(k) = cellmask_from_polygon_set(xzw(k), yzw(k))
      end do
      !$OMP END PARALLEL DO

      call cellmask_from_polygon_set_cleanup()

   end subroutine pol_to_cellmask

end module m_pol_to_cellmask
