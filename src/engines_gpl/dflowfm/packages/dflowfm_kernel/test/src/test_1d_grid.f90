!!  Copyright (C)  Stichting Deltares, 2012-2026.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

module test_1d_grid
   use assertions_gtest, only: f90_expect_eq, f90_expect_true
   implicit none(type, external)
   private
contains

!> test for network in Figure B.1, renumbered branches
!$gtest
   subroutine test_start_end_nodes_of_branches_A() bind(C)
      use gridgeom, only: ggeo_get_start_end_nodes_of_branches
      integer, parameter :: nBranches = 3
      integer, parameter :: nNodes = 13
      integer :: ierr
      integer :: test_nodebranchidx(nNodes)
      integer :: gpFirst(nBranches), gpLast(nBranches)

      test_nodebranchidx(1:5) = 1
      test_nodebranchidx(6:9) = 2
      test_nodebranchidx(10:13) = 3
      ierr = ggeo_get_start_end_nodes_of_branches(test_nodebranchidx, gpFirst, gpLast)
      call f90_expect_eq(ierr, 0, 'Ordered branch indices should be accepted')
      call f90_expect_true(gpFirst == [1, 6, 10], 'check gpFirst')
      call f90_expect_true(gpLast == [5, 9, 13], 'check gpLast')

   end subroutine test_start_end_nodes_of_branches_A

!> Verify that branch indices not stored in increasing order are rejected.
!$gtest
   subroutine test_start_end_nodes_reject_unordered_branches() bind(C)
      use gridgeom, only: ggeo_get_start_end_nodes_of_branches
      integer, parameter :: nBranches = 3
      integer, parameter :: nNodes = 13
      integer :: ierr
      integer :: test_nodebranchidx(nNodes)
      integer :: gpFirst(nBranches), gpLast(nBranches)

      test_nodebranchidx(1:5) = 1
      test_nodebranchidx(6:9) = 3
      test_nodebranchidx(10:13) = 2
      ierr = ggeo_get_start_end_nodes_of_branches(test_nodebranchidx, gpFirst, gpLast)
      call f90_expect_eq(ierr, -1, 'Unordered branch indices should be rejected')

   end subroutine test_start_end_nodes_reject_unordered_branches

end module test_1d_grid
