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
module test_observations
   use assertions_gtest, only: f90_assert_eq, f90_expect_eq, f90_expect_streq, f90_expect_true, &
                               f90_expect_relative_near
   use precision, only: dp

   implicit none(type, external)
   private
   real(kind=dp), parameter :: eps = 1.0e-6_dp

contains
!
!
!==============================================================================
!
!
!==============================================================================
   !$gtest
   subroutine test_read_obs_points() bind(C)
      use m_observations
      use m_partitioninfo, only: jampi
      use m_network, only: realloc, fill_hashtable
      use unstruc_channel_flow, only: network
      use m_save_ugrid_state, only: meshgeom1d
      use m_resetfullflowmodel, only: resetfullflowmodel
      !
      ! Locals
      integer, parameter :: N_OBS_POINTS = 4
      integer :: i
      real(kind=dp), dimension(2, N_OBS_POINTS), parameter :: refdata = reshape([ &
                                                                                1.0_dp, 0.0_dp, 145964.0_dp, 427274.0_dp, &
                                                                                0.0_dp, 32.0_dp, 145852.0_dp, 427142.0_dp], [2, N_OBS_POINTS])
      character(len=40), dimension(N_OBS_POINTS) :: refnames
      !
      refnames(1) = 'TestLocation1'
      refnames(2) = 'TestLocation_xy1'
      refnames(3) = 'TestLocation2'
      refnames(4) = 'TestLocation_xy2'
      !
      ! Body
      jampi = 0
      call resetfullflowmodel()
      call realloc(network%brs)
      network%brs%count = 2
      network%brs%branch(1)%id = 'Lei3'
      network%brs%branch(2)%id = 'Lei11'
      call fill_hashtable(network%brs)
      allocate (meshgeom1d%ngeopointx(4), meshgeom1d%ngeopointy(4))
      allocate (meshgeom1d%nbranchgeometrynodes(2), meshgeom1d%nbranchlengths(2))
      meshgeom1d%ngeopointx = [0.0_dp, 100.0_dp, 0.0_dp, 0.0_dp]
      meshgeom1d%ngeopointy = [0.0_dp, 0.0_dp, 0.0_dp, 100.0_dp]
      meshgeom1d%nbranchgeometrynodes = [2, 2]
      meshgeom1d%nbranchlengths = [100.0_dp, 100.0_dp]
      call loadObservations("observations/points/ObservationPoints_2.ini", 0)
      deallocate (meshgeom1d%ngeopointx, meshgeom1d%ngeopointy)
      deallocate (meshgeom1d%nbranchgeometrynodes, meshgeom1d%nbranchlengths)
      if (.not. f90_assert_eq(numobs, N_OBS_POINTS, 'Number of observation points incorrect')) then
         return
      end if
      call f90_expect_true(allocated(xobs), 'xobs is allocated')
      if (allocated(xobs)) then
         do i = 1, N_OBS_POINTS
            call f90_expect_relative_near(xobs(i), refdata(1, i), eps, 'x-coordinate of observation points incorrect')
            call f90_expect_relative_near(yobs(i), refdata(2, i), eps, 'y-coordinate of observation points incorrect')
            call f90_expect_streq(trim(namobs(i)), trim(refnames(i)), "Observation point name incorrect")
         end do
      end if
   end subroutine test_read_obs_points
!
!
!==============================================================================
   !$gtest
   subroutine test_snap_obs_points() bind(C)
      use m_alloc, only: realloc
      use m_flowgeom, only: tnode, nd, ndx, ndxi, ndx2d, xz, yz, ln, lnx
      use m_globalparameters, only: INDTP_1D
      use m_network_helpers, only: t_grid_helper
      use m_obs_on_flowgeom, only: obs_on_flowgeom
      use m_observations, only: addObservation, kobs, lobs
      use m_partitioninfo, only: jampi
      use network_data, only: nump, netcell, xk, yk, xzw, yzw
      use unstruc_channel_flow, only: network

      type(t_grid_helper) :: grid
      type(tnode), dimension(:), allocatable :: saved_nodes
      integer :: cell_index, branch_index
      integer, dimension(2, 2) :: cells
      real(kind=dp), dimension(2), parameter :: centers = [5.0_dp, 15.0_dp]

      ! Two parallel 1D branches. Reuse square cells as their point-in-cell search polygons.
      jampi = 0
      call grid%make_square_grid(0.0_dp, 0.0_dp, 10.0_dp, rows=2, columns=2)
      if (.not. f90_assert_eq(nump, 4, 'Number of snapping cells')) then
         return
      end if
      ndx = nump
      ndxi = ndx
      ndx2d = 0
      xz = xzw(1:ndx)
      yz = yzw(1:ndx)
      call move_alloc(nd, saved_nodes)
      allocate (nd(ndx))
      do cell_index = 1, ndx
         nd(cell_index)%x = xk(netcell(cell_index)%nod)
         nd(cell_index)%y = yk(netcell(cell_index)%nod)
         nd(cell_index)%lnx = 1
      end do

      allocate (network%brs%branch(2), network%obs%OPnt(2))
      network%brs%count = 2
      network%brs%size = 2
      network%obs%count = 2
      network%obs%size = 2
      lnx = 2
      call realloc(ln, [2, lnx], keepExisting=.false.)
      do branch_index = 1, 2
         cells(1, branch_index) = minloc(abs(xz - centers(1)) + abs(yz - centers(branch_index)), dim=1)
         cells(2, branch_index) = minloc(abs(xz - centers(2)) + abs(yz - centers(branch_index)), dim=1)
         ln(:, branch_index) = cells(:, branch_index)
         associate (branch => network%brs%branch(branch_index))
            branch%gridPointsCount = 2
            branch%gridPointsChainages = [0.0_dp, 10.0_dp]
            branch%gridPointsSeqCount = 1
            branch%k1gridPointsSeq = [1]
            branch%k2gridPointsSeq = [2]
            branch%grd = cells(:, branch_index)
            branch%uPointsCount = 1
            branch%uPointsChainages = [5.0_dp]
            branch%isGPLastAtBranchEnd = .true.
         end associate
         network%obs%OPnt(branch_index)%branchIdx = branch_index
      end do

      network%obs%OPnt%chainage = [8.0_dp, 2.0_dp]
      ! Give branch stations conflicting XY positions so the test distinguishes the two lookup paths.
      call addObservation(7.0_dp, 5.0_dp, 'lower branch', loctype=INDTP_1D, iOP=1)
      call addObservation(13.0_dp, 15.0_dp, 'upper branch', loctype=INDTP_1D, iOP=2)
      call addObservation(6.0_dp, 6.0_dp, 'lower XY', loctype=INDTP_1D)
      call addObservation(14.0_dp, 14.0_dp, 'upper XY', loctype=INDTP_1D)

      call obs_on_flowgeom(0)
      call move_alloc(saved_nodes, nd)

      call f90_expect_eq(kobs(1), cells(2, 1), 'Lower branch: after the link midpoint')
      call f90_expect_eq(kobs(2), cells(1, 2), 'Upper branch: before the link midpoint')
      call f90_expect_eq(kobs(3), cells(1, 1), 'Lower XY: enclosing cell')
      call f90_expect_eq(kobs(4), cells(2, 2), 'Upper XY: enclosing cell')
      call f90_expect_eq(lobs(3), 1, 'Lower XY: nearest link')
      call f90_expect_eq(lobs(4), 2, 'Upper XY: nearest link')
   end subroutine test_snap_obs_points

end module test_observations
