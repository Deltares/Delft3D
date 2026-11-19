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
module chdir_mod
   implicit none

   interface
      integer function c_chdir(path) bind(C, name="chdir")
         use iso_c_binding
         character(kind=c_char) :: path(*)
      end function
   end interface

contains

   subroutine chdir(path, err)
      use iso_c_binding
      character(*) :: path
      integer, optional, intent(out) :: err
      integer :: loc_err

      loc_err = c_chdir(path//c_null_char)

      if (present(err)) err = loc_err
   end subroutine
end module chdir_mod

module test_observations
   use assertions_gtest, only: f90_assert_eq, f90_assert_true, f90_expect_eq, f90_expect_streq, f90_expect_true, &
                               f90_expect_relative_near
   use precision

   implicit none(type, external)
   private
   real(fp), parameter :: eps = 1.0e-6_fp

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
   subroutine test_read_snapped_obs_points() bind(C)
      use gridoperations
      use m_cell_geometry, only: xz, yz
      use m_commandline_option, only: iarg_usecaching
      use m_netw
      use m_observations
      use unstruc_model
      use m_partitioninfo, only: jampi
      use ifport
      use m_flow_modelinit, only: flow_modelinit
      use m_resetfullflowmodel, only: resetfullflowmodel
      !
      ! Locals
      integer, parameter :: N_OBS_POINTS = 4
      integer :: i
      integer :: istat
      logical :: success_
      integer, dimension(N_OBS_POINTS), parameter :: ref_k = [522, 1043, 1565, 1304]
      real(kind=dp), dimension(2, N_OBS_POINTS), parameter :: refdata = reshape([ &
                                                                                100000.0_dp, 25000.0_dp, 0.0_dp, 50000.0_dp, &
                                                                                0.0_dp, 100000.0_dp, 100000.0_dp, 75000.0_dp], [2, N_OBS_POINTS])
      character(len=40), dimension(N_OBS_POINTS) :: refnames
      character(len=40) :: mdufile
      !
      refnames(1) = 'TestLocation1'
      refnames(2) = 'TestLocation_xy1'
      refnames(3) = 'TestLocation2'
      refnames(4) = 'TestLocation_xy2'
      !
      ! Body
      jampi = 0
      iarg_usecaching = 0
      kmax = 2
      lmax = 2
      call resetFullFlowModel()
      call increaseNetw(kmax, lmax)
      !
      success_ = CHANGEDIRQQ("observations/snapped")
      if (.not. f90_assert_true(success_, 'Could not change to directory ''observations/snapped''.')) then
         return
      end if

      mdufile = 'Flow1d.mdu'
      call loadModel(mdufile)
      istat = flow_modelinit()

      success_ = CHANGEDIRQQ("../..")
      if (.not. f90_assert_true(success_, 'Could not restore the working directory from ''observations/snapped''.')) then
         return
      end if
      if (.not. f90_assert_eq(istat, 0, 'Could not initialize flow model.')) then
         return
      end if
      !
      do i = 1, N_OBS_POINTS
         call f90_expect_eq(kobs(i), ref_k(i), 'index of snapped observation points incorrect')
         call f90_expect_relative_near(xz(kobs(i)), refdata(1, i), eps, 'x-coordinate of snapped observation points incorrect')
         call f90_expect_relative_near(yz(kobs(i)), refdata(2, i), eps, 'y-coordinate of snapped observation points incorrect')
         call f90_expect_streq(trim(namobs(i)), trim(refnames(i)), "Snapped observation point name incorrect")
      end do
   end subroutine test_read_snapped_obs_points

end module test_observations
