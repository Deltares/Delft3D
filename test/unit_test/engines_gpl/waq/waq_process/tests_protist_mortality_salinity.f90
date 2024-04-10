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

program tests_protist_mortality_salinity
    !!  Tests_protist_mortality_salinity.f90
    !!  Runs unit tests for protist_mortality_salinity

    use m_waq_precision
    use ftnunit, only: runtests_init, runtests, runtests_final, assert_comparable, test
    use m_protist_mortality_salinity, only: calculate_process_in_segment, input_protist_mortality_salinity, output_protist_mortality_salinity

    implicit none
    character(len=200) :: cmd_arg
    integer :: iargc, getarg
    real(kind=real_wp), parameter :: tolerance = 0.0001

    ! Determine the number of command line arguments
    iargc = command_argument_count()
    call prepare_tests()
    call runtests_init()

    ! Run the test specified in the argument, if no argument run all tests
    if (iargc > 0) then
        call get_command_argument(1, cmd_arg)

        select case (trim(cmd_arg))
        case('test_protist_mort_salt__influence_salinity')
            write(*,*) "Running test_protist_mort_salt__influence_salinity"
            call runtests(call_test_protist_mort_salt__influence_salinity)
        case ('test_protist_mort_salt__influence_sigmoid_gradient')
            write(*,*) "Running test_protist_mort_salt__influence_sigmoid_gradient"
            call runtests(call_test_protist_mort_salt__influence_sigmoid_gradient)
        case ('test_protist_mort_salt__influence_sigmoid_shift')
            write(*,*) "Running test_protist_mort_salt__influence_sigmoid_shift"
            call runtests(call_test_protist_mort_salt__influence_sigmoid_shift)
        case ('test_protist_mort_salt__influence_rate_low_salinity')
            write(*,*) "Running test_protist_mort_salt__influence_rate_low_salinity"
            call runtests(call_test_protist_mort_salt__influence_rate_low_salinity)
        case ('test_protist_mort_salt__influence_rate_high_salinity')
            write(*,*) "Running test_protist_mort_salt__influence_rate_high_salinity"
            call runtests(call_test_protist_mort_salt__influence_rate_high_salinity)
        end select
    else
        write(*,*) "No test specified, running all tests"
        call runtests(call_test_protist_mort_salt__influence_salinity)
        call runtests(call_test_protist_mort_salt__influence_sigmoid_gradient)
        call runtests(call_test_protist_mort_salt__influence_sigmoid_shift)
        call runtests(call_test_protist_mort_salt__influence_rate_low_salinity)
        call runtests(call_test_protist_mort_salt__influence_rate_high_salinity)
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

    subroutine call_test_protist_mort_salt__influence_salinity
        call test(test_protist_mort_salt__influence_salinity, 'Dependency of protist mortality on salinity.')
    end subroutine call_test_protist_mort_salt__influence_salinity

    subroutine call_test_protist_mort_salt__influence_sigmoid_gradient
        call test(test_protist_mort_salt__influence_sigmoid_gradient, 'Dependency of protist mortality on gradient of sigmoid curve.')
    end subroutine call_test_protist_mort_salt__influence_sigmoid_gradient

    subroutine call_test_protist_mort_salt__influence_sigmoid_shift
        call test(test_protist_mort_salt__influence_sigmoid_shift, 'Dependency of protist mortality on shift value of sigmoid.')
    end subroutine call_test_protist_mort_salt__influence_sigmoid_shift

    subroutine call_test_protist_mort_salt__influence_rate_low_salinity
        call test(test_protist_mort_salt__influence_rate_low_salinity, 'Dependency of protist mortality on rate value for low salinity.')
    end subroutine call_test_protist_mort_salt__influence_rate_low_salinity

    subroutine call_test_protist_mort_salt__influence_rate_high_salinity
        call test(test_protist_mort_salt__influence_rate_high_salinity, 'Dependency of protist mortality on rate value for high salinity.')
    end subroutine call_test_protist_mort_salt__influence_rate_high_salinity

    subroutine test_protist_mort_salt__influence_salinity
        !< Local variables
        type(input_protist_mortality_salinity)  :: iv1, iv2
        type(output_protist_mortality_salinity) :: ov1, ov2

        ! Arrange
        iv1 = input_protist_mortality_salinity(20.0, 0.55, 20.0, 0.08, 0.16)
        iv2 = input_protist_mortality_salinity(17.0, 0.55, 20.0, 0.08, 0.16)

        ! Act
        ov1 = calculate_process_in_segment(iv1)
        ov2 = calculate_process_in_segment(iv2)

        ! Assert
        call assert_comparable(ov1%mortality, 0.120000, tolerance, 'Validate value of mortality with salinity equal to 20 ppt.')
        call assert_comparable(ov2%mortality, 0.147111, tolerance, 'Validate value of mortality with salinity equal to 17 ppt.')
    end subroutine test_protist_mort_salt__influence_salinity

    subroutine test_protist_mort_salt__influence_sigmoid_gradient
        !< Local variables
        type(input_protist_mortality_salinity)  :: iv1, iv2
        type(output_protist_mortality_salinity) :: ov1, ov2

        ! Arrange
        iv1 = input_protist_mortality_salinity(18.0, 0.3, 20.0, 0.08, 0.16)
        iv2 = input_protist_mortality_salinity(18.0, 1.7, 20.0, 0.08, 0.16)

        ! Act
        ov1 = calculate_process_in_segment(iv1)
        ov2 = calculate_process_in_segment(iv2)

        ! Assert
        call assert_comparable(ov1%mortality, 0.131653, tolerance, 'Validate value of mortality with sigmoid gradient b1 = 0.825.')
        call assert_comparable(ov2%mortality, 0.157416, tolerance, 'Validate value of mortality with sigmoid gradient b1 = 1.650.')
    end subroutine test_protist_mort_salt__influence_sigmoid_gradient

    subroutine test_protist_mort_salt__influence_sigmoid_shift
        !< Local variables
        type(input_protist_mortality_salinity)  :: iv1, iv2
        type(output_protist_mortality_salinity) :: ov1, ov2

        ! Arrange
        iv1 = input_protist_mortality_salinity(30.0, 0.55, 30.0, 0.08, 0.16)
        iv2 = input_protist_mortality_salinity(10.0, 0.55, 10.0, 0.08, 0.16)

        ! Act
        ov1 = calculate_process_in_segment(iv1)
        ov2 = calculate_process_in_segment(iv2)

        ! Assert
        call assert_comparable(ov1%mortality, 0.120000, tolerance, 'Validate value of mortality with sigmoid shift b2 = 30.')
        call assert_comparable(ov2%mortality, 0.120000, tolerance, 'Validate value of mortality with sigmoid shift b2 = 10.')
    end subroutine test_protist_mort_salt__influence_sigmoid_shift

    subroutine test_protist_mort_salt__influence_rate_low_salinity
        !< Local variables
        type(input_protist_mortality_salinity)  :: iv1, iv2
        type(output_protist_mortality_salinity) :: ov1, ov2

        ! Arrange
        iv1 = input_protist_mortality_salinity(2.0, 0.55, 20.0, 0.08, 0.19)
        iv2 = input_protist_mortality_salinity(2.0, 0.55, 20.0, 0.08, 0.13)

        ! Act
        ov1 = calculate_process_in_segment(iv1)
        ov2 = calculate_process_in_segment(iv2)

        ! Assert
        call assert_comparable(ov1%mortality, 0.1900, tolerance, 'Validate value of mortality for low salinity m1 = 0.08.')
        call assert_comparable(ov2%mortality, 0.1300, tolerance, 'Validate value of mortality for low salinity m1 = 0.05.')
    end subroutine test_protist_mort_salt__influence_rate_low_salinity

    subroutine test_protist_mort_salt__influence_rate_high_salinity
        !< Local variables
        type(input_protist_mortality_salinity)  :: iv1, iv2
        type(output_protist_mortality_salinity) :: ov1, ov2

        ! Arrange
        iv1 = input_protist_mortality_salinity(35.0, 0.55, 20.0, 0.08, 0.16)
        iv2 = input_protist_mortality_salinity(35.0, 0.55, 20.0, 0.05, 0.16)

        ! Act
        ov1 = calculate_process_in_segment(iv1)
        ov2 = calculate_process_in_segment(iv2)

        ! Assert
        call assert_comparable(ov1%mortality, 0.080021, tolerance, 'Validate value of mortality for high salinity m2 = 0.19.')
        call assert_comparable(ov2%mortality, 0.050029, tolerance, 'Validate value of mortality for high salinity m2 = 0.19.')
    end subroutine test_protist_mort_salt__influence_rate_high_salinity

end program tests_protist_mortality_salinity