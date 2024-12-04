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

program tests_tmode
    !!  Tests_tmode.f90
    !!  Runs unit tests for the TMODE process routine

   use m_waq_precision
   use ftnunit, only: runtests_init, runtests, runtests_final, assert_comparable, test
   use m_tempermode

   implicit none
   character(len=200) :: cmd_arg
   integer :: iargc
   real(kind=real_wp), parameter :: tolerance = 0.0001

   ! Administrative arrays and variables
   integer, parameter :: num_process_parameters = 8
   integer, parameter :: num_cells = 9
   integer, parameter :: noflux = 0
   integer, parameter :: num_exchanges_u_dir = 0
   integer, parameter :: num_exchanges_v_dir = 0
   integer, parameter :: num_exchanges_z_dir = 0
   integer, parameter :: num_exchanges_bottom_dir = 0
   integer :: ipoint(num_process_parameters)
   integer :: increm(num_process_parameters)
   integer :: iexpnt(num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + num_exchanges_bottom_dir) ! Exchanges play no role
   integer :: iknmrk(num_cells)

   real(kind=real_wp) :: process_space_real(num_process_parameters*num_cells)
   real(kind=real_wp) :: fl(noflux * num_cells)

   ! Determine the number of command line arguments
   iargc = command_argument_count()
   call prepare_tests()
   call runtests_init()

   ! Set the administration - the same for all tests and it does not change much
   call fill_administration

   ! Run the test specified in the argument, if no argument run all tests
   if (iargc > 0) then
      call get_command_argument(1, cmd_arg)

      select case (trim(cmd_arg))
      case ('test_tmode')
         call test(test_tmode, 'Modelled temperature conversion and limitation')
      case default
         write (*, *) "Unknown test case: ", trim(cmd_arg)
      end select
   else
      write (*, *) "No test specified, running all tests"
         call test(test_tmode, 'Modelled temperature conversion and limitation')
   end if
   
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

   subroutine fill_administration
      integer :: i

      increm = num_process_parameters
      do i = 1, num_process_parameters
         ipoint(i) = i
      end do

      iknmrk = 1 ! Single layer, active
   end subroutine fill_administration

   ! Fill the process parameters with their default values
   ! (Taken from the processes configuration)
   !
   subroutine fill_process_space_real_defaults
      process_space_real = -999.000
   end subroutine fill_process_space_real_defaults

   ! Test the 'Modelled temperature conversion and limitation'
   ! Combination of:
   ! - three switch options for iswtmp
   ! - three cases of ttemp being lower than mintemp, between mintemp and maxtemp, or greater than maxtemp
   ! results in nine different cases, tested in nine cells together
   !
   subroutine test_tmode
      real(kind=real_wp) :: mtemp(num_cells) !             1 in  modelled temperature                                           [oC]
      real(kind=real_wp) :: tmpnat(num_cells) !            2 in  natural temperature of ambient water                           [oC]
      integer(kind=int_wp) :: iswtmp(num_cells) !          3 in  switch if modelled temperature is total or excess temeperature [-]
      real(kind=real_wp) :: mintemp(num_cells) !           4 in  minimum total temperature to be used by processes              [oC]
      real(kind=real_wp) :: maxtemp(num_cells) !           5 in  minimum total temperature to be used by processes              [oC]
      real(kind=real_wp) :: expected_ttemp(num_cells) !    6 out total temperature to be used by processes                      [oC]
      real(kind=real_wp) :: expected_etemp(num_cells) !    7 out excess temperature to be used by processes                     [oC]
      real(kind=real_wp) :: expected_tmpnatp1(num_cells) ! 8 out natural temperature plus one                                   [oC]
      
      integer(kind=int_wp) :: ip(num_process_parameters)
      integer(kind=int_wp) :: i
      character(len=1) :: label      

      mtemp  = [ -2.0, 25.0, 105.0, -17.0, 10.0, 90.0, -17.0, 10.0,  90.0] 
      tmpnat = [ 15.0, 15.0,  15.0,  15.0, 15.0, 15.0,  -2.0, 25.0, 105.0] 
      iswtmp = [  0.0,  0.0,   0.0,   1.0,  1.0,  1.0,   2.0,  2.0,   2.0]
      mintemp = 5.0
      maxtemp = 40.0

      expected_ttemp = [  5.0, 25.0,  40.0,   5.0, 25.0, 40.0,   5.0, 25.0, 40.0] 
      expected_etemp = [-17.0, 10.0,  90.0, -17.0, 10.0, 90.0, -17.0, 10.0, 90.0]  
      expected_tmpnatp1 = [ 16.0, 16.0, 16.0,  16.0, 16.0, 16.0,  -1.0, 26.0, 106.0] 

      call fill_process_space_real_defaults

      ! Arrange
      ip = ipoint
      do i = 1, num_cells
         process_space_real(ip(1)) = mtemp(i)
         process_space_real(ip(2)) = tmpnat(i)
         process_space_real(ip(3)) = iswtmp(i)
         process_space_real(ip(4)) = mintemp(i)
         process_space_real(ip(5)) = maxtemp(i)
         ip = ip + increm
      end do
      
      ! Act
      call tmode(process_space_real, fl, ipoint, increm, num_cells, noflux, iexpnt, iknmrk, &
                 num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir)

      ! Assert
      ip = ipoint
      do i = 1, num_cells
         write(label, '(I1)') i
         call assert_comparable(process_space_real(ip(6)), expected_ttemp(i), tolerance, 'Total temperature case '//label)
         call assert_comparable(process_space_real(ip(7)), expected_etemp(i), tolerance, 'Excess temperature case '//label)
         call assert_comparable(process_space_real(ip(8)), expected_tmpnatp1(i), tolerance, 'Natural temperature plus one case '//label)
         ip = ip + increm
      end do
   end subroutine test_tmode
end program tests_tmode

