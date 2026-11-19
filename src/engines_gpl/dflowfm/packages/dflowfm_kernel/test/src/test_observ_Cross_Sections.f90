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
   use assertions_gtest, only: f90_assert_eq, f90_expect_eq, f90_expect_streq, f90_expect_near
   use precision, only: dp

   implicit none(type, external)
   private
   real(kind=dp), parameter :: eps = 1.0e-9_dp

contains
   !$gtest
   subroutine test_read_snapped_observ_crs() bind(C)
      use m_alloc, only: realloc
      use m_branch, only: fill_hashtable
      use m_crosssections_on_flowgeom, only: crosssections_on_flowgeom
      use m_flowgeom, only: ln, lnx, lnxi, lnx1d, ln2lne, kcu, ndx, xz, yz, wu
      use m_monitoring_crosssections, only: loadObservCrossSections, crs, ncrs
      use m_partitioninfo, only: jampi
      use m_save_ugrid_state, only: meshgeom1d
      use network_data, only: numl, LINK_1D
      use unstruc_channel_flow, only: network

      integer :: branch_index, saved_numl

      ! Two parallel branches, one link each. The polyline crosses both links.
      jampi = 0
      ndx = 4
      lnx = 2
      lnxi = lnx
      lnx1d = lnx
      xz = [0.0_dp, 10.0_dp, 0.0_dp, 10.0_dp]
      yz = [0.0_dp, 0.0_dp, 10.0_dp, 10.0_dp]
      call realloc(ln, [2, lnx], keepExisting=.false.)
      ln = reshape([1, 2, 3, 4], shape(ln))
      ln2lne = [1, 2]
      call realloc(kcu, lnx, fill=LINK_1D, keepExisting=.false.)
      call realloc(wu, lnx, fill=2.0_dp, keepExisting=.false.)

      allocate (network%brs%branch(2))
      network%brs%count = 2
      network%brs%size = 2
      network%brs%branch(1)%id = 'lower'
      network%brs%branch(2)%id = 'upper'
      do branch_index = 1, 2
         associate (branch => network%brs%branch(branch_index))
            branch%length = 10.0_dp
            branch%gridPointsChainages = [0.0_dp, 10.0_dp]
            branch%gridPointsSeqCount = 1
            branch%k1gridPointsSeq = [1]
            branch%k2gridPointsSeq = [2]
            branch%lin = [branch_index]
         end associate
      end do
      call fill_hashtable(network%brs)

      allocate (meshgeom1d%ngeopointx(4), meshgeom1d%ngeopointy(4))
      allocate (meshgeom1d%nbranchgeometrynodes(2), meshgeom1d%nbranchlengths(2))
      meshgeom1d%ngeopointx = xz
      meshgeom1d%ngeopointy = yz
      meshgeom1d%nbranchgeometrynodes = [2, 2]
      meshgeom1d%nbranchlengths = [10.0_dp, 10.0_dp]
      call loadObservCrossSections('observations/cross_sections_snapped/observcrs.ini', 0)
      deallocate (meshgeom1d%ngeopointx, meshgeom1d%ngeopointy)
      deallocate (meshgeom1d%nbranchgeometrynodes, meshgeom1d%nbranchlengths)
      if (.not. f90_assert_eq(ncrs, 3, 'Number of monitoring cross sections read')) then
         return
      end if

      saved_numl = numl
      numl = lnx
      call crosssections_on_flowgeom()
      numl = saved_numl

      call check_branch_cross_section(1, 'ObservCross1', 1, 2.0_dp, 0.0_dp)
      call check_branch_cross_section(3, 'ObservCross2', 2, 8.0_dp, 10.0_dp)
      call f90_expect_streq(trim(crs(2)%name), 'ObservCross_xy2', 'Polyline cross-section name')
      associate (path => crs(2)%path)
         if (.not. f90_assert_eq(path%np, 3, 'Number of polyline vertices')) then
            return
         end if
         call f90_expect_near(path%xp(1:3), [5.0_dp, 5.0_dp, 5.0_dp], eps, 'Polyline x coordinates')
         call f90_expect_near(path%yp(1:3), [-2.0_dp, 5.0_dp, 12.0_dp], eps, 'Polyline y coordinates')
         if (.not. f90_assert_eq(path%lnx, 2, 'Number of links crossed by the polyline')) then
            return
         end if
         call f90_expect_eq(path%ln(path%iperm(1)), 1, 'First flow link crossed by the polyline')
         call f90_expect_eq(path%ln(path%iperm(2)), 2, 'Second flow link crossed by the polyline')
      end associate
   end subroutine test_read_snapped_observ_crs

   subroutine check_branch_cross_section(section_index, name, link_index, x, y)
      use m_monitoring_crosssections, only: crs

      integer, intent(in) :: section_index, link_index
      character(len=*), intent(in) :: name
      real(kind=dp), intent(in) :: x, y

      call f90_expect_streq(trim(crs(section_index)%name), name, 'Branch cross-section name')
      associate (path => crs(section_index)%path)
         if (.not. f90_assert_eq(path%np, 1, 'Number of coordinates for a branch cross section')) then
            return
         end if
         call f90_expect_near(path%xp(1), x, eps, 'Branch cross-section x coordinate')
         call f90_expect_near(path%yp(1), y, eps, 'Branch cross-section y coordinate')
         if (.not. f90_assert_eq(path%lnx, 1, 'Number of links for a branch cross section')) then
            return
         end if
         call f90_expect_eq(path%ln(1), link_index, 'Snapped link for a branch cross section')
      end associate
   end subroutine check_branch_cross_section

end module test_obserCrossSections
