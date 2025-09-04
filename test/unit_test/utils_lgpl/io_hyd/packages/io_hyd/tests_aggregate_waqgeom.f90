!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
      integer :: lunrun

      open (newunit=lunrun, file='ftnunit.run')
      write (lunrun, '(a)') 'ALL'
      close (lunrun)
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
      type(t_ug_meshgeom) :: input_s !< The layers and interfaces to be aggregated (sigma).
      type(t_ug_meshgeom) :: input_z !< The layers and interfaces to be aggregated (z).
      type(t_ug_meshgeom) :: input_zs !< The layers and interfaces to be aggregated (z-sigma).
      type(t_ug_meshgeom) :: output !< Aggregated layers and interfaces.
      type(t_ug_meshgeom) :: expected_output !< Aggregated layers and interfaces.
      integer, dimension(20) :: layer_mapping_table_1 !< Mapping table flow cells -> waq cells.
      integer, dimension(20) :: layer_mapping_table_2 !< Mapping table flow cells -> waq cells.
      integer, dimension(20) :: layer_mapping_table_3 !< Mapping table flow cells -> waq cells.
      integer, dimension(20) :: layer_mapping_table_4 !< Mapping table flow cells -> waq cells.
      logical :: success !< Result status, true if successful.
      logical :: is_equal !< Result status, true if successful.
      integer :: i

      ! The tests should be implemented for 3 cases with different layering:
      ! 1) 3D 20 sigma layers
      input_s%num_layers = 20
      input_s%numtopsig = -1
      input_s%layertype = LAYERTYPE_OCEANSIGMA
      call reallocP(input_s%layer_zs, 20)
      input_s%layer_zs = [-0.975, -0.925, -0.875, -0.825, -0.775, -0.725, -0.675, -0.625, -0.575, -0.525, &
                          -0.475, -0.425, -0.375, -0.325, -0.275, -0.225, -0.175, -0.125, -0.075, -0.025]
      call reallocP(input_s%interface_zs, 21)
      input_s%interface_zs = [-1.0, -0.95, -0.90, -0.85, -0.80, -0.75, -0.70, -0.65, -0.60, -0.55, -0.50, &
                              -0.45, -0.40, -0.35, -0.30, -0.25, -0.20, -0.15, -0.10, -0.05, 0.0]

      ! 2) 3D 20 z-layers
      input_z%num_layers = 20
      input_z%numtopsig = -1
      input_z%layertype = LAYERTYPE_Z
      call reallocP(input_z%layer_zs, 20)
      input_z%layer_zs = [-4.8725, -4.6175, -4.3625, -4.1075, -3.8525, -3.5975, -3.3425, -3.0875, -2.8325, -2.5775, &
                          -2.3225, -2.0675, -1.8125, -1.5575, -1.3025, -1.0475, -0.7925, -0.5375, -0.2825, -0.0275]
      call reallocP(input_z%interface_zs, 21)
      input_z%interface_zs = [-5.0, -4.745, -4.49, -4.235, -3.98, -3.725, -3.47, -3.215, -2.96, -2.705, -2.45, &
                              -2.195, -1.94, -1.685, -1.43, -1.175, -0.92, -0.665, -0.41, -0.155, 0.1]

      ! 3) 3D 12/8 z-sigma layers
      input_zs%num_layers = 20
      input_zs%numtopsig = 8
      input_zs%layertype = LAYERTYPE_OCEAN_SIGMA_Z
      call reallocP(input_zs%layer_zs, 20)
      input_zs%layer_zs = [-4.8725, -4.6175, -4.3625, -4.1075, -3.8525, -3.5975, -3.3425, -3.0875, -2.8325, -2.5775, &
                           -2.3225, -2.0675, -0.9375, -0.8125, -0.6875, -0.5625, -0.4375, -0.3125, -0.1875, -0.0625]
      call reallocP(input_zs%interface_zs, 21)
      input_zs%interface_zs = [-5.0, -4.745, -4.49, -4.235, -3.98, -3.725, -3.47, -3.215, -2.96, -2.705, -2.45, &
                               -2.195, -1.94, -0.875, -0.75, -0.625, -0.5, -0.375, -0.25, -0.125, 0.0]

      ! The cases should be aggregated in 4 different ways:
      ! 1) no aggregation of layers (just copy current data)
      layer_mapping_table_1 = [(i, i=1, 20)]
      ! 2) 3D -> 2D layers
      layer_mapping_table_2 = 1
      ! 3) 20 -> 8 layers where z and sigma layers are not combined
      layer_mapping_table_3 = [1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8]
      ! 4) 20 -> 8 layers where z and sigma layers are combined (this should fail when %layertype = LAYERTYPE_OCEAN_SIGMA_Z)
      layer_mapping_table_4 = [1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8]

      ! in total this results in 12 test cases, of which one is expected to fail (assert_true(.not.success, 'failure expected'))

      ! Sigma-layers without aggregation
      expected_output = input_s
      success = aggregate_ugrid_layers_interfaces(input_s, output, layer_mapping_table_1)
      is_equal = compare_ugrid_layers_interfaces(output, expected_output, tolerance)
      call assert_true(success .and. is_equal, &
                       'Error in aggregation of layers and interfaces for sigma-layers without aggregation.')

      ! Z-layers without aggregation
      expected_output = input_z
      success = aggregate_ugrid_layers_interfaces(input_z, output, layer_mapping_table_1)
      is_equal = compare_ugrid_layers_interfaces(output, expected_output, tolerance)
      call assert_true(success .and. is_equal, &
                       'Error in aggregation of layers and interfaces for z-layers without aggregation.')

      ! Z-sigma-layers without aggregation
      expected_output = input_zs
      success = aggregate_ugrid_layers_interfaces(input_zs, output, layer_mapping_table_1)
      is_equal = compare_ugrid_layers_interfaces(output, expected_output, tolerance)
      call assert_true(success .and. is_equal, &
                       'Error in aggregation of layers and interfaces for z-sigma-layers without aggregation.')

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
