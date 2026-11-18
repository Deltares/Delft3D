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
module test_obserCrossSections
   use assertions_gtest, only: f90_assert_eq, f90_assert_true, f90_expect_eq, f90_expect_streq
   use test_relative_assertions, only: expect_relative_near
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
   subroutine test_read_snapped_observ_crs() bind(C)
      use gridoperations
      use m_commandline_option, only: iarg_usecaching
      use m_flowgeom, only: xu, yu
      use m_netw
      use m_monitoring_crosssections
      use unstruc_model
      use m_partitioninfo, only: jampi
      use network_data, only: numk
      use ifport
      use m_flow_modelinit, only: flow_modelinit
      use m_resetfullflowmodel, only: resetfullflowmodel
      !
      ! Locals
      integer, parameter :: N_Observ_Crs = 3
      integer :: i
      integer :: istat
      logical :: success
      real(kind=dp), dimension(2), parameter :: refcoord1 = [1.458360454017425e5_dp, 4.271440320425944e5_dp]
      real(kind=dp), dimension(2, 3), parameter :: refcoord2 = reshape([ &
         145883.0_dp, 427375.0_dp, 145897.0_dp, 427379.0_dp, 145939.0_dp, 427382.0_dp], [2, 3])
      real(kind=dp), dimension(2), parameter :: refcoord3 = [1.458105097560976e5_dp, 4.271732395492436e5_dp]
      integer, dimension(N_Observ_Crs), parameter :: ref_lnx = [1, 2, 1]
      integer, parameter :: ref_L1 = 3, ref_L3 = 11
      integer, dimension(2), parameter :: ref_L2 = [29, 38]
      real(kind=dp), dimension(2), parameter :: refdata1 = [145839.15_dp, 427141.25_dp]
      real(kind=dp), dimension(2, 2), parameter :: refdata2 = reshape([ &
         145888.15_dp, 427376.85_dp, 145928.7_dp, 427385.8_dp], [2, 2])
      real(kind=dp), dimension(2), parameter :: refdata3 = [145802.8_dp, 427187.0_dp]
      character(len=40), dimension(N_Observ_Crs) :: refnames
      character(len=40) :: mdufile

      refnames(1) = 'ObservCross1'
      refnames(2) = 'ObservCross_xy2'
      refnames(3) = 'ObservCross2'
      !
      ! Body
      jampi = 0
      iarg_usecaching = 0
      kmax = 2
      lmax = 2
      numk = 0
      !call inidat()
      call increaseNetw(kmax, lmax)

      call resetFullFlowModel()

      !
      success = CHANGEDIRQQ("observations/cross_sections_snapped")
      if (.not. f90_assert_true(success, 'Could not change to directory ''observations/cross_sections_snapped''.')) then
         return
      end if
      mdufile = 'FlowFM.mdu'
      call loadModel(mdufile)
      istat = flow_modelinit()
      success = CHANGEDIRQQ("../..")
      if (.not. f90_assert_true(success, 'Could not restore the working directory from ''observations/cross_sections_snapped''.')) then
         return
      end if
      if (.not. f90_assert_eq(istat, 0, 'Could not initialize flow model.')) then
         return
      end if
      !
      ! first observCrs
      call f90_expect_streq(trim(crs(1)%name), trim(refnames(1)), 'Name of the first observation cross section incorrect')
      call expect_relative_near(crs(1)%path%xp(1), refcoord1(1), eps, 'x-coordinate of the first observation cross section incorrect')
      call expect_relative_near(crs(1)%path%yp(1), refcoord1(2), eps, 'y-coordinate of the first observation cross section incorrect')
      call f90_expect_eq(crs(1)%path%lnx, ref_lnx(1), 'Number of snapped flowlinks of the first observation cross section incorrect')
      call f90_expect_eq(crs(1)%path%ln(1), ref_L1, 'Index of snapped flowlink of the first observation cross section incorrect')
      call expect_relative_near(xu(crs(1)%path%ln(1)), refdata1(1), eps, 'x-coordinate of snapped flowlink of the first observation cross section incorrect')
      call expect_relative_near(yu(crs(1)%path%ln(1)), refdata1(2), eps, 'y-coordinate of snapped flowlink of the first observation cross section incorrect')

      ! second observCrs
      call f90_expect_streq(trim(crs(2)%name), trim(refnames(2)), 'Name of the second observation cross section incorrect')
      do i = 1, 3
         call expect_relative_near(crs(2)%path%xp(i), refcoord2(1, i), eps, 'x-coordinate of the second observation cross section incorrect')
         call expect_relative_near(crs(2)%path%yp(i), refcoord2(2, i), eps, 'y-coordinate of the second observation cross section incorrect')
      end do
      call f90_expect_eq(crs(2)%path%lnx, ref_lnx(2), 'Number of snapped flowlinks of the second observation cross section incorrect')
      do i = 1, 2
         call f90_expect_eq(crs(2)%path%ln(i), ref_L2(i), 'Index of snapped flowlink of the second observation cross section incorrect')
         call expect_relative_near(xu(crs(2)%path%ln(i)), refdata2(1, i), eps, 'x-coordinate of snapped flowlink of the second observation cross section incorrect')
         call expect_relative_near(yu(crs(2)%path%ln(i)), refdata2(2, i), eps, 'y-coordinate of snapped flowlink of the second observation cross section incorrect')
      end do
      ! third observCrs
      call f90_expect_streq(trim(crs(3)%name), trim(refnames(3)), 'Name of the third observation cross section incorrect')
      call expect_relative_near(crs(3)%path%xp(1), refcoord3(1), eps, 'x-coordinate of the third observation cross section incorrect')
      call expect_relative_near(crs(3)%path%yp(1), refcoord3(2), eps, 'y-coordinate of the third observation cross section incorrect')
      call f90_expect_eq(crs(3)%path%lnx, ref_lnx(3), 'Number of snapped flowlinks of the third observation cross section incorrect')
      call f90_expect_eq(crs(3)%path%ln(1), ref_L3, 'Index of snapped flowlink of the third observation cross section incorrect')
      call expect_relative_near(xu(crs(3)%path%ln(1)), refdata3(1), eps, 'x-coordinate of snapped flowlink of the third observation cross section incorrect')
      call expect_relative_near(yu(crs(3)%path%ln(1)), refdata3(2), eps, 'y-coordinate of snapped flowlink of the third observation cross section incorrect')

   end subroutine test_read_snapped_observ_crs

end module test_obserCrossSections
