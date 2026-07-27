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

program tests_aggregate_waqgeom
    !!  tests_aggregate_waqgeom.f90
    !!  Runs unit tests for tests_aggregate_waqgeom

   use m_waq_precision
   use m_aggregate_waqgeom, only: aggregate_ugrid_layers_interfaces
   use m_ug_meshgeom
   use io_ugrid, only: LAYERTYPE_OCEANSIGMA, &
                       LAYERTYPE_Z, &
                       LAYERTYPE_OCEAN_SIGMA_Z
   use m_alloc
   use ftnunit, only: runtests_init, &
                      runtests, &
                      runtests_final, &
                      assert_comparable, &
                      test, &
                      assert_true

   implicit none
   character(len=200) :: cmd_arg
   integer :: iargc
   real(kind=real_wp), parameter :: tolerance = 0.0001

   type(t_ug_meshgeom) :: input_2d !< 2D model (sigma with only one layer).
   type(t_ug_meshgeom) :: input_bt_s !< Bottom to top layers and interfaces to be aggregated (sigma).
   type(t_ug_meshgeom) :: input_bt_z !< Bottom to top layers and interfaces to be aggregated (z).
   type(t_ug_meshgeom) :: input_bt_zs !< Bottom to top layers and interfaces to be aggregated (z-sigma).
   type(t_ug_meshgeom) :: input_tb_s !< Top to bottom layers and interfaces to be aggregated (sigma).
   type(t_ug_meshgeom) :: input_tb_z !< Top to bottom layers and interfaces to be aggregated (z).
   type(t_ug_meshgeom) :: input_tb_zs !< Top to bottom layers and interfaces to be aggregated (z-sigma).
   integer, dimension(7, 20) :: layer_mapping_table !< Mapping table flow cells -> waq cells.

   ! Determine the number of command line arguments
   iargc = command_argument_count()
   call prepare_tests()
   call runtests_init()

   ! Run the test specified in the argument, if no argument run all tests
   if (iargc > 0) then
      call get_command_argument(1, cmd_arg)

      select case (trim(cmd_arg))
      case ('tests_aggregate_ugrid_layers_interfaces')
         write (*, *) "Running "//cmd_arg
         call runtests(call_test_aggregate_ugrid_layers_interfaces)
      end select
   else
      write (*, *) "No test specified, running all tests"
      call runtests(call_test_aggregate_ugrid_layers_interfaces)
   end if

   call runtests_final()

contains

   subroutine prepare_tests
      ! prepare_tests
      !     Routine to start the testing
      !
      ! Note:
      !     This routine merely takes care that the unit tests are indeed run
      integer :: lunrun, i

      open (newunit=lunrun, file='ftnunit.run')
      write (lunrun, '(a)') 'ALL'
      close (lunrun)

      ! Prepare the inputs
      ! ==================

      ! Three 3D cases with different layering
      ! --------------------------------------

      ! 3D 20 sigma layers (top to bottom)
      input_tb_s%num_layers = 20
      input_tb_s%numtopsig = -1
      input_tb_s%layertype = LAYERTYPE_OCEANSIGMA
      call reallocP(input_tb_s%layer_zs, 20)
      input_tb_s%layer_zs = [-0.025, -0.075, -0.125, -0.175, -0.225, -0.275, -0.325, -0.375, -0.425, -0.475, &
                             -0.525, -0.575, -0.625, -0.675, -0.725, -0.775, -0.825, -0.875, -0.925, -0.975]
      call reallocP(input_tb_s%interface_zs, 21)
      input_tb_s%interface_zs = [0.0, -0.05, -0.10, -0.15, -0.20, -0.25, -0.30, -0.35, -0.40, -0.45, -0.50, -0.55, &
                                 -0.60, -0.65, -0.70, -0.75, -0.80, -0.85, -0.90, -0.95, -1.0]

      ! 3D 20 z-layers (top to bottom)
      input_tb_z%num_layers = 20
      input_tb_z%numtopsig = -1
      input_tb_z%layertype = LAYERTYPE_Z
      call reallocP(input_tb_z%layer_zs, 20)
      input_tb_z%layer_zs = [-0.0275, -0.2825, -0.5375, -0.7925, -1.0475, -1.3025, -1.5575, -1.8125, -2.0675, -2.3225, &
                             -2.5775, -2.8325, -3.0875, -3.3425, -3.5975, -3.8525, -4.1075, -4.3625, -4.6175, -4.8725]
      call reallocP(input_tb_z%interface_zs, 21)
      input_tb_z%interface_zs = [0.1, -0.155, -0.41, -0.665, -0.92, -1.175, -1.43, -1.685, -1.94, -2.195, -2.45, &
                                 -2.705, -2.96, -3.215, -3.47, -3.725, -3.98, -4.235, -4.49, -4.745, -5.0]

      ! 3D 12/8 z-sigma layers (top to bottom)
      input_tb_zs%num_layers = 20
      input_tb_zs%numtopsig = 8
      input_tb_zs%layertype = LAYERTYPE_OCEAN_SIGMA_Z
      call reallocP(input_tb_zs%layer_zs, 20)
      input_tb_zs%layer_zs = [-0.0625, -0.1875, -0.3125, -0.4375, -0.5625, -0.6875, -0.8125, -0.9375, -2.0675, -2.3225, &
                              -2.5775, -2.8325, -3.0875, -3.3425, -3.5975, -3.8525, -4.1075, -4.3625, -4.6175, -4.8725]
      call reallocP(input_tb_zs%interface_zs, 21)
      input_tb_zs%interface_zs = [0.0, -0.125, -0.25, -0.375, -0.5, -0.625, -0.75, -0.875, -1.94, -2.195, -2.45, &
                                  -2.705, -2.96, -3.215, -3.47, -3.725, -3.98, -4.235, -4.49, -4.745, -5.0]

      ! Also created these cases with the layering reversed
      ! ---------------------------------------------------

      ! 3D 20 sigma layers (bottom to top)
      input_bt_s%num_layers = 20
      input_bt_s%numtopsig = -1
      input_bt_s%layertype = LAYERTYPE_OCEANSIGMA
      call reallocP(input_bt_s%layer_zs, 20)
      do i = 1, input_bt_s%num_layers
         input_bt_s%layer_zs(i) = input_tb_s%layer_zs(input_bt_s%num_layers - i + 1)
      end do
      call reallocP(input_bt_s%interface_zs, 21)
      do i = 1, input_bt_s%num_layers + 1
         input_bt_s%interface_zs(i) = input_tb_s%interface_zs(input_bt_s%num_layers - i + 2)
      end do

      ! 3D 20 z-layers (bottom to top)
      input_bt_z%num_layers = 20
      input_bt_z%numtopsig = -1
      input_bt_z%layertype = LAYERTYPE_Z
      call reallocP(input_bt_z%layer_zs, 20)
      do i = 1, input_bt_z%num_layers
         input_bt_z%layer_zs(i) = input_tb_z%layer_zs(input_bt_z%num_layers - i + 1)
      end do
      call reallocP(input_bt_z%interface_zs, 21)
      do i = 1, input_bt_z%num_layers + 1
         input_bt_z%interface_zs(i) = input_tb_z%interface_zs(input_bt_z%num_layers - i + 2)
      end do

      ! 3D 12/8 z-sigma layers (bottom to top)
      input_bt_zs%num_layers = 20
      input_bt_zs%numtopsig = 8
      input_bt_zs%layertype = LAYERTYPE_OCEAN_SIGMA_Z
      call reallocP(input_bt_zs%layer_zs, 20)
      do i = 1, input_bt_zs%num_layers
         input_bt_zs%layer_zs(i) = input_tb_zs%layer_zs(input_bt_zs%num_layers - i + 1)
      end do
      call reallocP(input_bt_zs%interface_zs, 21)
      do i = 1, input_bt_zs%num_layers + 1
         input_bt_zs%interface_zs(i) = input_tb_zs%interface_zs(input_bt_zs%num_layers - i + 2)
      end do

      ! Trivial 2D model (3D model with a single layer)
      ! -----------------------------------------------
      input_2d%num_layers = 1
      input_2d%numtopsig = -1
      input_2d%layertype = LAYERTYPE_OCEANSIGMA
      call reallocP(input_2d%layer_zs, 1)
      input_2d%layer_zs = [-0.5]
      call reallocP(input_2d%interface_zs, 2)
      input_2d%interface_zs = [0.0, -1.0]

      ! The layer mapping tables
      ! ------------------------

      ! The top to bottom cases should be aggregated in three different ways, where the layer mapping table is always
      ! defined from top to bottom. The bottom to top case is only tested with option 3).
      ! 1) no aggregation of layers (just copy current data)
      layer_mapping_table(1, :) = [(i, i=1, 20)]
      ! 2) 3D -> 2D layers
      layer_mapping_table(2, :) = 1
      ! 3) 20 -> 8 layers (where z and sigma layers are not combined when %layertype = LAYERTYPE_OCEAN_SIGMA_Z)
      layer_mapping_table(3, :) = [1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8]

      ! Also test layer mapping tables with errors:
      ! 4) layer mapping doesn't start with 1
      layer_mapping_table(4, :) = [8, 8, 8, 7, 7, 7, 6, 6, 6, 6, 5, 5, 4, 4, 3, 3, 2, 2, 1, 1]
      ! 5) layer mapping is increasing by more than one
      layer_mapping_table(5, :) = [1, 1, 3, 2, 3, 3, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8]
      ! 6) layer mapping is decreasing
      layer_mapping_table(6, :) = [1, 1, 2, 1, 2, 3, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8]
      ! 7) 20 -> 8 layers where z and sigma layers are combined (this should fail when %layertype = LAYERTYPE_OCEAN_SIGMA_Z)
      layer_mapping_table(7, :) = [1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8]

   end subroutine prepare_tests

   subroutine show_result
      ! show_result
      !     Start the browser to show the result
      call system('ftnunit.html')
   end subroutine show_result

   subroutine call_test_aggregate_ugrid_layers_interfaces
      call test(test_aggregate_ugrid_layers_interfaces, 'Test aggregation of ugrid layers and interfaces')
   end subroutine

   subroutine test_aggregate_ugrid_layers_interfaces()
      type(t_ug_meshgeom), dimension(14) :: output !< Aggregated layers and interfaces.
      type(t_ug_meshgeom), dimension(14) :: expected_output !< Aggregated layers and interfaces.
      logical :: success !< Result status, true if successful.
      logical :: is_equal !< Result status, true if successful.
      integer :: i

      ! Test with no aggregation
      ! ------------------------

      ! Sigma-layers without aggregation
      expected_output(1) = input_tb_s
      success = aggregate_ugrid_layers_interfaces(input_tb_s, output(1), layer_mapping_table(1, :))
      is_equal = compare_ugrid_layers_interfaces(output(1), expected_output(1), tolerance)
      call assert_true(success .and. is_equal, &
                       'Error in aggregation of layers and interfaces for sigma-layers without aggregation.')

      ! Z-layers without aggregation
      expected_output(2) = input_tb_z
      success = aggregate_ugrid_layers_interfaces(input_tb_z, output(2), layer_mapping_table(1, :))
      is_equal = compare_ugrid_layers_interfaces(output(2), expected_output(2), tolerance)
      call assert_true(success .and. is_equal, &
                       'Error in aggregation of layers and interfaces for z-layers without aggregation.')

      ! Z-sigma-layers without aggregation
      expected_output(3) = input_tb_zs
      success = aggregate_ugrid_layers_interfaces(input_tb_zs, output(3), layer_mapping_table(1, :))
      is_equal = compare_ugrid_layers_interfaces(output(3), expected_output(3), tolerance)
      call assert_true(success .and. is_equal, &
                       'Error in aggregation of layers and interfaces for z-sigma-layers without aggregation.')

      ! Test 3D to 2D
      ! =============

      ! Sigma-layers 3D to 2D
      expected_output(4)%num_layers = 0
      success = aggregate_ugrid_layers_interfaces(input_tb_s, output(4), layer_mapping_table(2, :))
      is_equal = compare_ugrid_layers_interfaces(output(4), expected_output(4), tolerance)
      call assert_true(success .and. is_equal, &
                       'Error in aggregation of layers and interfaces for sigma-layers without aggregation.')

      ! Z-layers 3D to 2D
      expected_output(5)%num_layers = 0
      success = aggregate_ugrid_layers_interfaces(input_tb_z, output(5), layer_mapping_table(2, :))
      is_equal = compare_ugrid_layers_interfaces(output(5), expected_output(5), tolerance)
      call assert_true(success .and. is_equal, &
                       'Error in aggregation of layers and interfaces for z-layers without aggregation.')

      ! Z-sigma-layers 3D to 2D
      expected_output(6)%num_layers = 0
      success = aggregate_ugrid_layers_interfaces(input_tb_zs, output(6), layer_mapping_table(2, :))
      is_equal = compare_ugrid_layers_interfaces(output(6), expected_output(6), tolerance)
      call assert_true(success .and. is_equal, &
                       'Error in aggregation of layers and interfaces for z-sigma-layers without aggregation.')

      ! Proper layer mapping table for all three layer types (top to bottom)
      ! --------------------------------------------------------------------

      ! Sigma-layers
      expected_output(7)%num_layers = 8
      expected_output(7)%numtopsig = -1
      expected_output(7)%layertype = LAYERTYPE_OCEANSIGMA
      call reallocP(expected_output(7)%layer_zs, 8)
      expected_output(7)%layer_zs = [-0.05, -0.15, -0.25, -0.35, -0.475, -0.625, -0.775, -0.925]
      call reallocP(expected_output(7)%interface_zs, 9)
      expected_output(7)%interface_zs = [0.0, -0.10, -0.20, -0.30, -0.40, -0.55, -0.70, -0.85, -1.0]
      success = aggregate_ugrid_layers_interfaces(input_tb_s, output(7), layer_mapping_table(3, :))
      is_equal = compare_ugrid_layers_interfaces(output(7), expected_output(7), tolerance)
      call assert_true(success .and. is_equal, &
                       'Error in aggregation of layers and interfaces for sigma-layers with a 20 to 8 layer aggregation.')

      ! Z-layers
      expected_output(8)%num_layers = 8
      expected_output(8)%numtopsig = -1
      expected_output(8)%layertype = LAYERTYPE_Z
      call reallocP(expected_output(8)%layer_zs, 8)
      expected_output(8)%layer_zs = [-0.155, -0.665, -1.175, -1.685, -2.3225, -3.0875, -3.8525, -4.6175]
      call reallocP(expected_output(8)%interface_zs, 9)
      expected_output(8)%interface_zs = [0.1, -0.41, -0.92, -1.43, -1.94, -2.705, -3.47, -4.235, -5.0]
      success = aggregate_ugrid_layers_interfaces(input_tb_z, output(8), layer_mapping_table(3, :))
      is_equal = compare_ugrid_layers_interfaces(output(8), expected_output(8), tolerance)
      call assert_true(success .and. is_equal, &
                       'Error in aggregation of layers and interfaces for z-layers with a 20 to 8 layer aggregation.')

      ! Z-sigma-layers
      expected_output(9)%num_layers = 8
      expected_output(9)%numtopsig = 4
      expected_output(9)%layertype = LAYERTYPE_OCEAN_SIGMA_Z
      call reallocP(expected_output(9)%layer_zs, 8)
      expected_output(9)%layer_zs = [-0.125, -0.375, -0.625, -0.875, -2.3225, -3.0875, -3.8525, -4.6175]
      call reallocP(expected_output(9)%interface_zs, 9)
      expected_output(9)%interface_zs = [0.0, -0.25, -0.5, -0.75, -1.94, -2.705, -3.47, -4.235, -5.0]
      success = aggregate_ugrid_layers_interfaces(input_tb_zs, output(9), layer_mapping_table(3, :))
      is_equal = compare_ugrid_layers_interfaces(output(9), expected_output(9), tolerance)
      call assert_true(success .and. is_equal, &
                       'Error in aggregation of layers and interfaces for z-sigma-layers with a 20 to 8 layer aggregation.')

      ! Proper layer mapping table for all three layer types (bottom to top)
      ! --------------------------------------------------------------------

      ! Sigma-layers
      expected_output(10)%num_layers = 8
      expected_output(10)%numtopsig = -1
      expected_output(10)%layertype = LAYERTYPE_OCEANSIGMA
      call reallocP(expected_output(10)%layer_zs, 8)
      expected_output(10)%layer_zs = [-0.925, -0.775, -0.625, -0.475, -0.35, -0.25, -0.15, -0.05]
      call reallocP(expected_output(10)%interface_zs, 9)
      expected_output(10)%interface_zs = [-1.0, -0.85, -0.70, -0.55, -0.40, -0.30, -0.20, -0.10, 0.0]
      success = aggregate_ugrid_layers_interfaces(input_bt_s, output(10), layer_mapping_table(3, :))
      is_equal = compare_ugrid_layers_interfaces(output(10), expected_output(10), tolerance)
      call assert_true(success .and. is_equal, &
                       'Error in aggregation of layers and interfaces for sigma-layers with a 20 to 8 layer aggregation.')

      ! Z-layers
      expected_output(11)%num_layers = 8
      expected_output(11)%numtopsig = -1
      expected_output(11)%layertype = LAYERTYPE_Z
      call reallocP(expected_output(11)%layer_zs, 8)
      expected_output(11)%layer_zs = [-4.6175, -3.8525, -3.0875, -2.3225, -1.685, -1.175, -0.665, -0.155]
      call reallocP(expected_output(11)%interface_zs, 9)
      expected_output(11)%interface_zs = [-5.0, -4.235, -3.47, -2.705, -1.94, -1.43, -0.92, -0.41, 0.1]
      success = aggregate_ugrid_layers_interfaces(input_bt_z, output(11), layer_mapping_table(3, :))
      is_equal = compare_ugrid_layers_interfaces(output(11), expected_output(11), tolerance)
      call assert_true(success .and. is_equal, &
                       'Error in aggregation of layers and interfaces for z-layers with a 20 to 8 layer aggregation.')

      ! Z-sigma-layers
      expected_output(12)%num_layers = 8
      expected_output(12)%numtopsig = 4
      expected_output(12)%layertype = LAYERTYPE_OCEAN_SIGMA_Z
      call reallocP(expected_output(12)%layer_zs, 8)
      expected_output(12)%layer_zs = [-4.6175, -3.8525, -3.0875, -2.3225, -0.875, -0.625, -0.375, -0.125]
      call reallocP(expected_output(12)%interface_zs, 9)
      expected_output(12)%interface_zs = [-5.0, -4.235, -3.47, -2.705, -1.94, -0.75, -0.5, -0.25, 0.0]
      success = aggregate_ugrid_layers_interfaces(input_bt_zs, output(12), layer_mapping_table(3, :))
      is_equal = compare_ugrid_layers_interfaces(output(12), expected_output(12), tolerance)
      call assert_true(success .and. is_equal, &
                       'Error in aggregation of layers and interfaces for z-sigma-layers with a 20 to 8 layer aggregation.')

      ! Test trivial 2D case
      ! --------------------
      expected_output(13)%num_layers = 0
      success = aggregate_ugrid_layers_interfaces(input_2d, output(13), [1])
      is_equal = compare_ugrid_layers_interfaces(output(13), expected_output(13), tolerance)
      call assert_true(success .and. is_equal, 'Error in trivial aggregation of 2D model.')

      ! Layer mapping table validity testing
      ! ------------------------------------
      ! output(14) is a dummy, because output is not expected

      ! Layer mapping table is too short
      success = aggregate_ugrid_layers_interfaces(input_tb_s, output(14), [(i, i=1, 19)])
      call assert_true(.not. success, 'No error when layer mapping table is too short.')

      ! Layer mapping table is too long
      success = aggregate_ugrid_layers_interfaces(input_tb_s, output(14), [(i, i=1, 21)])
      call assert_true(.not. success, 'No error when layer mapping table is too long.')

      ! Layer mapping doesn't start with 1
      success = aggregate_ugrid_layers_interfaces(input_tb_s, output(14), layer_mapping_table(4, :))
      call assert_true(.not. success, 'No error when layer mapping table does not start with one.')

      ! Layer mapping is increasing by more than one
      success = aggregate_ugrid_layers_interfaces(input_tb_s, output(14), layer_mapping_table(5, :))
      call assert_true(.not. success, 'No error when layer mapping table increases with a step of more than one.')

      ! Layer mapping is decreasing
      success = aggregate_ugrid_layers_interfaces(input_tb_s, output(14), layer_mapping_table(6, :))
      call assert_true(.not. success, 'No error when layer mapping table has a decreasing step.')

      ! Z-sigma-layers  with a invalid layer mapping
      success = aggregate_ugrid_layers_interfaces(input_tb_zs, output(14), layer_mapping_table(7, :))
      call assert_true(.not. success, 'No error when merging Z and sigma layers in z-sigma-layers model.')

      return
   end subroutine

   function compare_ugrid_layers_interfaces(ugrid1, ugrid2, tolerance) result(is_equal)
      type(t_ug_meshgeom), intent(in) :: ugrid1, ugrid2
      real(kind=real_wp), intent(in) :: tolerance
      logical :: is_equal
      integer :: i

      is_equal = .true. ! Assume they are equal

      if (ugrid1%num_layers /= ugrid2%num_layers) then
         is_equal = .false.
         return
      end if
      if (ugrid1%numtopsig /= ugrid2%numtopsig) then
         is_equal = .false.
         return
      end if
      if (ugrid1%layertype /= ugrid2%layertype) then
         is_equal = .false.
         return
      end if

      if (associated(ugrid1%layer_zs) /= associated(ugrid2%layer_zs)) then
         is_equal = .false.
         return
      end if
      if (associated(ugrid1%layer_zs)) then
         if (size(ugrid1%layer_zs) /= size(ugrid2%layer_zs)) then
            is_equal = .false.
            return
         end if
         do i = 1, size(ugrid1%layer_zs)
            if (abs(ugrid1%layer_zs(i) - ugrid2%layer_zs(i)) > tolerance) then
               is_equal = .false.
               return
            end if
         end do
      end if

      if (associated(ugrid1%interface_zs) /= associated(ugrid2%interface_zs)) then
         is_equal = .false.
         return
      end if
      if (associated(ugrid1%interface_zs)) then
         if (size(ugrid1%interface_zs) /= size(ugrid2%interface_zs)) then
            is_equal = .false.
            return
         end if
         do i = 1, size(ugrid1%interface_zs)
            if (abs(ugrid1%interface_zs(i) - ugrid2%interface_zs(i)) > tolerance) then
               is_equal = .false.
               return
            end if
         end do
      end if
   end function compare_ugrid_layers_interfaces
end program
