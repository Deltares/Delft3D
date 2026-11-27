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
module test_ini_Field_1dField
   use assertions_gtest, only: f90_expect_true, f90_expect_relative_near
   use precision, only: dp
   use m_flow, only: s1, hs
   use m_flowgeom, only: bl, ndx, ndxi, ndx2d
   use m_branch, only: t_branch
   use unstruc_channel_flow, only: network
   implicit none(type, external)
   private

   real(kind=dp), parameter :: eps = 1.0e-6_dp

contains

   subroutine setup_field(number_of_branches, bed_levels, number_of_2d_cells)
      use m_alloc, only: realloc
      use unstruc_inifields, only: specified_water_1dfield, water_global_quantity_1dfield

      integer, intent(in) :: number_of_branches, number_of_2d_cells
      real(kind=dp), dimension(:), intent(in) :: bed_levels

      ndx = size(bed_levels)
      ndxi = ndx
      ndx2d = number_of_2d_cells
      call realloc(bl, ndx, keepExisting=.false.)
      call realloc(hs, ndx, fill=0.0_dp, keepExisting=.false.)
      call realloc(s1, ndx, keepExisting=.false.)
      bl = bed_levels
      s1 = bed_levels
      network%brs%count = number_of_branches
      if (number_of_branches > 0) then
         allocate (network%brs%branch(number_of_branches))
         network%brs%size = number_of_branches
      end if
      if (allocated(specified_water_1dfield)) then
         deallocate (specified_water_1dfield)
      end if
      water_global_quantity_1dfield = ''
   end subroutine setup_field

   subroutine set_branch(branch, id, first_cell, chainages)
      type(t_branch), intent(inout) :: branch
      character(len=*), intent(in) :: id
      integer, intent(in) :: first_cell
      real(kind=dp), dimension(:), intent(in) :: chainages
      integer :: point_index

      branch%id = id
      branch%gridPointsCount = size(chainages)
      branch%gridPointsChainages = chainages
      branch%gridPointsSeqCount = 1
      branch%k1gridPointsSeq = [1]
      branch%k2gridPointsSeq = [size(chainages)]
      branch%grd = [(first_cell + point_index - 1, point_index=1, size(chainages))]
   end subroutine set_branch

   ! Only the 1dField data file is read. Construct its enclosing [Initial] block in memory.
   subroutine apply_field(test_dir, quantity, data_file)
      use fm_external_forcings_init, only: init_spatial_fields
      use properties, only: prop_set
      use tree_structures, only: tree_data, tree_create, tree_destroy

      character(len=*), intent(in) :: test_dir, quantity, data_file
      type(tree_data), pointer :: block
      logical :: success

      call tree_create('Initial', block)
      call prop_set(block, '', 'quantity', quantity)
      call prop_set(block, '', 'forcingFile', 'initial_fields/'//test_dir//'/'//data_file)
      call prop_set(block, '', 'forcingFileType', '1dField')
      success = init_spatial_fields(block, '', 'test initial field', 'Initial')
      call tree_destroy(block)
      call f90_expect_true(success, 'Could not apply '//quantity//' from '//test_dir//'/'//data_file)
   end subroutine apply_field

   !$gtest
   subroutine test_iniField1dField() bind(C)
      use m_branch, only: fill_hashtable
      use unstruc_inifields, only: finalize_1dfield_global_values

      call setup_field(3, [0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp], 0)
      call set_branch(network%brs%branch(1), '88_5', 1, [0.0_dp, 300.0_dp, 400.0_dp, 925.0_dp, 1350.0_dp, 1500.0_dp])
      call set_branch(network%brs%branch(2), 'N:TW-0307_1_204', 7, [0.0_dp, 100.0_dp])
      call set_branch(network%brs%branch(3), 'unspecified', 9, [0.0_dp])
      call fill_hashtable(network%brs)

      call apply_field('1d_field', 'initialWaterLevel', 'IniCondition.ini')
      call finalize_1dfield_global_values()

      call f90_expect_relative_near(s1(1:6), [8.0_dp, 8.0_dp, 8.5_dp, 10.0_dp, 11.0_dp, 11.0_dp], eps, &
                                    'Interpolate and hold the endpoint values outside the sample range')
      call f90_expect_relative_near(s1(7:8), [5.0_dp, 5.0_dp], eps, 'Uniform branch value')
      call f90_expect_relative_near(s1(9), 10.0_dp, eps, 'Global fallback on an unspecified branch')
   end subroutine test_iniField1dField

   subroutine check_branch_interpolation(test_dir, quantity, data_file, expected_depths, expected_levels)
      use m_branch, only: fill_hashtable
      use unstruc_inifields, only: finalize_1dfield_global_values

      character(len=*), intent(in) :: test_dir, quantity, data_file
      real(kind=dp), dimension(:), intent(in) :: expected_depths, expected_levels

      call setup_field(5, [-2.0_dp, -2.0_dp, -2.0_dp, -2.0_dp, -2.0_dp, -2.0_dp, -2.0_dp], 0)
      call set_branch(network%brs%branch(1), 'Channel001', 1, [0.0_dp])
      call set_branch(network%brs%branch(2), 'Channel002', 2, [0.0_dp])
      call set_branch(network%brs%branch(3), 'Channel003', 3, [0.0_dp, 785.394_dp, 1570.788_dp])
      call set_branch(network%brs%branch(4), 'Channel004', 6, [392.697_dp])
      call set_branch(network%brs%branch(5), 'Channel005', 7, [1178.091_dp])
      call fill_hashtable(network%brs)

      call apply_field(test_dir, quantity, data_file)
      call finalize_1dfield_global_values()

      call f90_expect_relative_near(s1, expected_levels, eps, 'Initial water levels')
      call f90_expect_relative_near(hs, expected_depths, eps, 'Initial water depths')
   end subroutine check_branch_interpolation

   !$gtest
   subroutine test_iniField1dField_waterdepth() bind(C)
      call check_branch_interpolation('water_depth', 'initialWaterDepth', 'InitialWaterDepth.ini', &
                                      [5.0_dp, 7.0_dp, 8.0_dp, 6.5_dp, 5.0_dp, 7.25_dp, 5.75_dp], &
                                      [3.0_dp, 5.0_dp, 6.0_dp, 4.5_dp, 3.0_dp, 5.25_dp, 3.75_dp])
   end subroutine test_iniField1dField_waterdepth

   !$gtest
   subroutine test_iniField1dField_waterlevel() bind(C)
      call check_branch_interpolation('water_level', 'initialWaterLevel', 'InitialWaterLevel.ini', &
                                      [7.0_dp, 9.0_dp, 10.0_dp, 8.5_dp, 7.0_dp, 9.25_dp, 7.75_dp], &
                                      [5.0_dp, 7.0_dp, 8.0_dp, 6.5_dp, 5.0_dp, 7.25_dp, 5.75_dp])
   end subroutine test_iniField1dField_waterlevel

   !$gtest
   subroutine test_iniField1dField_waterdepth_cross_sections() bind(C)
      use m_alloc, only: realloc
      use m_crosssections, only: CS_TABULATED, t_CSType, setParsCross
      use m_flowgeom, only: bob, bob0, kcu, ln, lnx1d, lnx1db, lnxi
      use m_oned_functions, only: setbobs_1d
      use network_data, only: LINK_1D
      use unstruc_inifields, only: finalize_1dfield_global_values

      real(kind=dp), parameter :: global_water_depth = 5.0_dp
      real(kind=dp), parameter :: cross_section_lowest_level = -13.12345_dp
      real(kind=dp), parameter :: first_branch_shift = 1000.0_dp
      type(t_CSType), target :: definition
      integer :: link_index

      ! Two independent 1D links use the same profile, one with a vertical shift.
      call setup_field(0, [0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp], 0)
      lnx1d = 2
      lnxi = lnx1d
      lnx1db = lnx1d
      call realloc(ln, [2, lnx1d], keepExisting=.false.)
      ln = reshape([1, 2, 3, 4], shape(ln))
      call realloc(kcu, lnx1d, fill=LINK_1D, keepExisting=.false.)
      call realloc(bob, [2, lnx1d], keepExisting=.false.)
      call realloc(bob0, [2, lnx1d], keepExisting=.false.)

      definition%crossType = CS_TABULATED
      definition%closed = .false.
      definition%height = [cross_section_lowest_level, 2.0_dp]
      definition%flowWidth = [1.0_dp, 2.0_dp]
      definition%levelsCount = size(definition%height)
      allocate (definition%groundlayer)
      allocate (network%crs%cross(lnx1d), network%adm%line2cross(lnx1d, 3))
      network%crs%count = lnx1d
      network%crs%size = lnx1d
      network%crs%cross%shift = [first_branch_shift, 0.0_dp]
      do link_index = 1, lnx1d
         network%crs%cross(link_index)%tabDef => definition
         call setParsCross(definition, network%crs%cross(link_index))
         network%adm%line2cross(link_index, :)%c1 = link_index
         network%adm%line2cross(link_index, :)%c2 = link_index
         network%adm%line2cross(link_index, :)%f = 1.0_dp
      end do
      call setbobs_1d()
      do link_index = 1, lnx1d
         nullify (network%crs%cross(link_index)%tabDef)
      end do
      deallocate (definition%groundlayer)

      call apply_field('water_depth_cross_sections', 'initialWaterDepth', 'InitialWaterDepth.ini')
      call finalize_1dfield_global_values()

      call f90_expect_relative_near(s1(1:2), &
                                    [cross_section_lowest_level + first_branch_shift + global_water_depth, &
                                     cross_section_lowest_level + first_branch_shift + global_water_depth], &
                                    eps, 'Water level on the shifted cross-section link')
      call f90_expect_relative_near(s1(3:4), &
                                    [cross_section_lowest_level + global_water_depth, cross_section_lowest_level + global_water_depth], &
                                    eps, 'Water level on the unshifted cross-section link')
   end subroutine test_iniField1dField_waterdepth_cross_sections

   subroutine check_water_depth_and_level(depth_dir, level_dir, level_first, expected_levels)
      use m_branch, only: fill_hashtable
      use unstruc_inifields, only: finalize_1dfield_global_values

      character(len=*), intent(in) :: depth_dir, level_dir
      logical, intent(in) :: level_first
      real(kind=dp), dimension(2), intent(in) :: expected_levels
      real(kind=dp), dimension(2), parameter :: bed_levels = [-2.0_dp, -2.0_dp]

      call setup_field(2, bed_levels, 0)
      call set_branch(network%brs%branch(1), 'Channel_1D_1', 1, [0.0_dp])
      call set_branch(network%brs%branch(2), 'Channel_1D_2', 2, [0.0_dp])
      call fill_hashtable(network%brs)

      if (level_first) then
         call apply_field(level_dir, 'initialWaterLevel', 'InitialWaterLevel.ini')
         call apply_field(depth_dir, 'initialWaterDepth', 'InitialWaterDepth.ini')
      else
         call apply_field(depth_dir, 'initialWaterDepth', 'InitialWaterDepth.ini')
         call apply_field(level_dir, 'initialWaterLevel', 'InitialWaterLevel.ini')
      end if
      call finalize_1dfield_global_values()

      call f90_expect_relative_near(bl, bed_levels, eps, 'Initial fields must not change the bed')
      call f90_expect_relative_near(s1, expected_levels, eps, 'Initial water levels on each branch')
      call f90_expect_relative_near(hs, expected_levels - bed_levels, eps, 'Initial water depths on each branch')
   end subroutine check_water_depth_and_level

   !$gtest
   subroutine test_water_depth_and_level_no_globals() bind(C)
      call check_water_depth_and_level('water_depth_global_level', 'water_depth_level_no_globals', .false., [900.0_dp, 18.0_dp])
   end subroutine test_water_depth_and_level_no_globals

   !$gtest
   subroutine test_water_depth_and_level_ignore_globals() bind(C)
      call check_water_depth_and_level('water_depth_level_ignore_globals', 'water_depth_level_ignore_globals', &
                                       .false., [900.0_dp, 18.0_dp])
   end subroutine test_water_depth_and_level_ignore_globals

   !$gtest
   subroutine test_water_depth_and_global_level() bind(C)
      call check_water_depth_and_level('water_depth_global_level', 'water_depth_global_level', .false., [100.0_dp, 18.0_dp])
   end subroutine test_water_depth_and_global_level

   !$gtest
   subroutine test_water_global_level_and_depth() bind(C)
      call check_water_depth_and_level('water_depth_global_level', 'water_depth_global_level', .true., [100.0_dp, 18.0_dp])
   end subroutine test_water_global_level_and_depth

   !$gtest
   subroutine test_water_only_depths() bind(C)
      call check_water_depth_and_level('water_only_depths', 'water_only_depths', .true., [298.0_dp, 398.0_dp])
   end subroutine test_water_only_depths

   !$gtest
   subroutine test_iniField1d_waterlevel_2d_points_not_set() bind(C)
      use unstruc_inifields, only: finalize_1dfield_global_values

      call setup_field(0, [-2.0_dp, -2.0_dp], 1)
      s1(1) = 4.0_dp
      hs(1) = 6.0_dp

      call apply_field('with_2d', 'initialWaterLevel', 'InitialWaterLevel.ini')
      call finalize_1dfield_global_values()

      call f90_expect_relative_near(s1, [4.0_dp, 1.0_dp], eps, 'The global 1D level must leave the 2D cell unchanged')
      call f90_expect_relative_near(hs, [6.0_dp, 3.0_dp], eps, 'The global 1D level must leave the 2D depth unchanged')
   end subroutine test_iniField1d_waterlevel_2d_points_not_set

end module test_ini_Field_1dField
