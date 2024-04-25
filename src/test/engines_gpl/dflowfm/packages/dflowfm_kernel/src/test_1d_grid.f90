!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
use ftnunit
implicit none
private
public :: tests_1d_grid
contains

subroutine tests_1d_grid
   call test(test_start_end_nodes_of_branches_A, 'test start end nodes of branches A: renumbered branches')
   !call test(test_start_end_nodes_of_branches_B, 'test start end nodes of branches B: original numbering branches')
end subroutine tests_1d_grid

!> test for network in Figure B.1, renumbered branches
subroutine test_start_end_nodes_of_branches_A
   use gridgeom, only : ggeo_get_start_end_nodes_of_branches
   integer, parameter :: nBranches =  3
   integer, parameter :: nNodes    = 13
   integer :: ierr
   integer :: nodebranchidx(nNodes)
   integer :: gpFirst(nBranches), gpLast(nBranches)

   nodebranchidx(1:5) = 1
   nodebranchidx(6:9) = 2
   nodebranchidx(10:13) = 3
   ierr = ggeo_get_start_end_nodes_of_branches(nodebranchidx, gpFirst, gpLast)
   call assert_equal(gpFirst, (/1, 6, 10/), 'check gpFirst')
   call assert_equal(gpLast, (/5, 9, 13/), 'check gpLast')

end subroutine test_start_end_nodes_of_branches_A

!> test for network in Figure B.1, original numbering branches
!! test disabled: to much routines depends on this order
subroutine test_start_end_nodes_of_branches_B
   use gridgeom, only : ggeo_get_start_end_nodes_of_branches
   integer, parameter :: nBranches =  3
   integer, parameter :: nNodes    = 13
   integer :: ierr
   integer :: nodebranchidx(nNodes)
   integer :: gpFirst(nBranches), gpLast(nBranches)

   nodebranchidx(1:5) = 1
   nodebranchidx(6:9) = 3
   nodebranchidx(10:13) = 2
   ierr = ggeo_get_start_end_nodes_of_branches(nodebranchidx, gpFirst, gpLast)
   call assert_equal(gpFirst, (/1, 10, 6/), 'check gpFirst')
   call assert_equal(gpLast, (/5, 13, 9/), 'check gpLast')

end subroutine test_start_end_nodes_of_branches_B

end module test_1d_grid
