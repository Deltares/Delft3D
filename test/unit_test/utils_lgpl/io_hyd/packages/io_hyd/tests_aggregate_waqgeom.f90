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
      type(t_ug_meshgeom) :: input !< The layers and interfaces to be aggregated.
      type(t_ug_meshgeom) :: output !< Aggregated layers and interfaces.
      integer, dimension(:), allocatable :: layer_mapping_table !< Mapping table flow cells -> waq cells.
      logical :: success = .false. !< Result status, true if successful.
      
      ! The tests should be implemented for 3 cases with different layering:
      ! 1) 3D 20 sigma layers
      ! 2) 3D 20 z-layers
      ! 3) 3D 10/10 z-sigma layers
      
      ! The cases should be aggregated in 4 different ways:
      ! 1) no aggregation of layers (just copy current data)
      ! 2) 3D -> 2D layers
      ! 3) 20 -> 8 layers where z and sigma layers are combined (this should fail %layertype = LAYERTYPE_OCEAN_SIGMA_Z)
      ! 4) 20 -> 8 layers where z and sigma layers are not combined
      
      ! in total this results in 12 test cases, of which one is expected to fail
      
      success = aggregate_ugrid_layers_interfaces(input, output, layer_mapping_table)

      call assert_true(success, 'Layers and interfaces were not aggregated as expected.')
      return
   end subroutine

end program
