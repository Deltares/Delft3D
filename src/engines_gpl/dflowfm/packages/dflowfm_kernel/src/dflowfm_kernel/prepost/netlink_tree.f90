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

!! Initialise net link based kd-tree for trachytopes or calibration
subroutine netlink_tree(phase)
   use precision, only: dp

   use network_data, only: numl, xk, yk, kn
   use kdtree2Factory
   use m_missing
   use m_sferic, only: jsferic, jasfer3D
   use geometry_module, only: half
   implicit none

   integer :: L, k1, k2, ierror
   integer, intent(in) :: phase

   real(kind=dp), dimension(:), allocatable :: xuL !< xu points on net-links
   real(kind=dp), dimension(:), allocatable :: yuL !< yu points on net-links

   if (phase == 0) then
      !   allocation step
      allocate (xuL(numL), yuL(numL))
      xuL = DMISS
      yuL = DMISS
      do L = 1, numL
         k1 = kn(1, L)
         k2 = kn(2, L)
         call half(xk(k1), yk(k1), xk(k2), yk(k2), xuL(L), yuL(L), jsferic, jasfer3D)
      end do

      !   build kdtree
      call build_kdtree(treeglob, numL, xuL, yuL, ierror, jsferic, dmiss)
      call realloc_results_kdtree(treeglob, 1) ! safety

      if (allocated(xuL)) deallocate (xuL)
      if (allocated(yuL)) deallocate (yuL)

   end if

   if (phase == 1) then
      !   deallocation step
      if (treeglob%itreestat /= ITREE_EMPTY) call delete_kdtree2(treeglob)
   end if

end subroutine
