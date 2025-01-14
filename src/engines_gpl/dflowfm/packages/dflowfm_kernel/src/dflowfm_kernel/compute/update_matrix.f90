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

module m_update_matrix

   implicit none

   private

   public :: update_matrix

contains

!> update overlapping ghost-parts of matrix
   subroutine update_matrix(ierror)
      use m_flowgeom, only: ndx, lnx
      use m_reduce, only: bbr, ccr, ddr, lv2
      use m_partitioninfo, only: workmatbd, workmatc, numsend_sall, isendlist_sall, numsend_u, isendlist_u, &
                                 itype_sall, itype_u, numghost_sall, ighostlist_sall, numghost_u, ighostlist_u, update_ghosts
      use m_alloc

      integer, intent(out) :: ierror !< error (1) or not (0)

      integer :: i, k, L

      ierror = 0

!   allocate if necessary
      call realloc(workmatbd, (/2, Ndx/), keepExisting=.true., fill=0d0)
      call realloc(workmatc, (/2, Lnx/), keepExisting=.true., fill=0d0)

!   fill work arrays
      do i = 1, numsend_sall
         k = isendlist_sall(i)
         workmatbd(1, k) = bbr(k)
         workmatbd(2, k) = ddr(k)
      end do

      do i = 1, numsend_u
         L = isendlist_u(i)
         workmatc(1, L) = ccr(Lv2(L))
         workmatc(2, L) = 1d0 ! used to "undo" orientation correction in update_ghosts(ITYPE_U,...)
      end do

!   update work arrays
      call update_ghosts(ITYPE_SALL, 2, Ndx, workmatbd, ierror)
      call update_ghosts(ITYPE_U, 2, Lnx, workmatc, ierror)

!   copy from work array
      do i = 1, numghost_sall
         k = ighostlist_sall(i)
         bbr(k) = workmatbd(1, k)
         ddr(k) = workmatbd(2, k)
      end do

      do i = 1, numghost_u
         L = abs(ighostlist_u(i))
         ccr(Lv2(L)) = workmatc(1, L) * workmatc(2, L)
      end do

      return
   end subroutine update_matrix

end module m_update_matrix
