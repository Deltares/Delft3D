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

program test_read_boundary_concentrations
    !! tests_read_boundary_concentrations.f90 --
    !!     Run unit tests for routines in Delft3D-WAQ

    use m_waq_precision

    use m_delwaq1_data, only : npos, ilun, lch
    use date_time_utils, only : system_time_factor_seconds, base_julian_time
    use ftnunit, only : runtests_init, runtests, runtests_final, assert_true, assert_equal, assert_files_comparable, &
            test, prepare_tests, show_result
    use m_string_utils, only : remove_duplicates
    use boundary_conditions, only : read_boundary_concentrations
    use m_error_status

    implicit none
    character(len = 200) :: cmd_arg
    integer :: iargc, getarg

    type :: bc_fixture
        integer(kind = int_wp) :: nosys, nobnd, nobtyp, output_verbose_level, ierr2
        character(255) :: bc_file, wrk_file, lchar(50)
        character(20), dimension(:), allocatable :: bc_ids
        character(20), dimension(:), allocatable :: bc_types
        character(20), dimension(:), allocatable :: sname
        real(kind = dp) :: drar(10) !< not used but required as argument of read_boundary_concentrations
        character(200) :: car(20)
        integer(kind = int_wp) :: iar(20)
        real(kind = real_wp) :: rar(20)
        logical :: is_date_format, is_yyddhh_format
        integer(kind = int_wp) :: iimax = 2500000
        integer(kind = int_wp) :: irmax = 10000000
        integer(kind = int_wp) :: icmax = 1000000
        integer(kind = int_wp) :: iwidth = 150
        integer(kind = int_wp) :: lun(50)
        type(error_status) :: status
    end type bc_fixture

    ! Set-up
    call setup_all()
    ! Determine the number of command line arguments
    iargc = command_argument_count()

    call prepare_tests
    call runtests_init

    ! Run a specific test based on the first argument
    if (iargc > 0) then
        call get_command_argument(1, cmd_arg)

        select case (trim(cmd_arg))
        case('test_dlwq5a_1_item_1_conc_1_const')
            write(*, *) "Running test_dlwq5a_1_item_1_conc_1_const"
            call runtests(call_test_dlwq5a_1_item_1_conc_1_const)
        case ('test_dlwq5a_1_item_2_conc_2_const')
            write(*, *) "Running test_dlwq5a_1_item_2_conc_2_const"
            call runtests(call_test_dlwq5a_1_item_2_conc_2_const)
        case ('test_dlwq5a_1_item_1_conc_1_tseries')
            write(*, *) "Running test_dlwq5a_1_item_1_conc_1_tseries"
            call runtests(call_test_dlwq5a_1_item_1_conc_1_tseries)
        case ('test_dlwq5a_1_item_2_conc_1_tseries')
            write(*, *) "Running test_dlwq5a_1_item_2_conc_1_tseries"
            call runtests(call_test_dlwq5a_1_item_2_conc_1_tseries)
        end select
    else
        write(*, *) "No test specified, running all tests"
        call runtests(call_test_dlwq5a_1_item_1_conc_1_const)
        call runtests(call_test_dlwq5a_1_item_2_conc_2_const)
        call runtests(call_test_dlwq5a_1_item_1_conc_1_tseries)
        call runtests(call_test_dlwq5a_1_item_2_conc_1_tseries)
    end if

    call teardown_all()

    call runtests_final

contains

    subroutine setup_all
        use time_module, only : julian_with_leapyears
        !< Steps required before running the first test case.
        system_time_factor_seconds = 1 ! global variable declared in dlwq0t_data
        base_julian_time = julian_with_leapyears(20220101, 0)
    end subroutine setup_all

    subroutine teardown_all
        !< Steps required after having run the last test case.
    end subroutine teardown_all

    subroutine setup_case(this, substance_names, bc_types, case_name)
        !< Steps required before running each test case.
        type(bc_fixture), intent(inout) :: this

        character(*), intent(in) :: substance_names(:) !< names of substances in BC's
        character(*), intent(in) :: bc_types(:)        !< Types of the BC's for each consecutive BC id. Therefore,
        !! there can be repetitions, e.g. ['Sea', 'River', 'River'].
        character(*), intent(in) :: case_name

        character(200) :: data_path
        integer(kind = int_wp) :: j

        ! global variables declared in m_sysn, loaded inside m_delwaq1_data
        call get_environment_variable("DATA_PATH", data_path)
        write(*, *) data_path
        this%nosys = size(substance_names)
        this%nobnd = size(bc_types)
        this%nobtyp = size(remove_duplicates(bc_types))

        this%lun = (/14, 15, 16, 17, 18, 19, 20, 21, 22, 23, &
                24, 25, 26, 27, 28, 29, 30, 31, 32, 33, &
                34, 35, 36, 37, 38, 39, 40, 41, 42, 43, &
                44, 45, 46, 47, 48, 49, 50, 51, 52, 53, &
                54, 55, 56, 57, 58, -1, -1, -1, -1, -1/)
        !! Allocate variables
        allocate(this%sname(this%nosys))
        allocate(this%bc_ids(this%nobnd))
        allocate(this%bc_types(this%nobtyp))

        !! Assign fixture variables
        do j = 1, this%nobnd
            write(this%bc_ids(j), '(I0)') j
        end do
        this%bc_types = remove_duplicates(bc_types)
        this%sname = substance_names
        this%ierr2 = 0
        this%output_verbose_level = 9

        !! Assign global variables
        ! global variable declared in rd_token, loaded inside m_delwaq1_data
        npos = 150
        ! global variables declared in m_delwaq1_data

        call this%status%initialize(0, 0, 0)
        this%lchar(14) = trim(data_path) // '/' // trim(case_name) // '.wrk'
        this%lchar(26) = trim(data_path) // '/' // trim(case_name) // '.inc'
        this%is_date_format = .true.
        this%is_yyddhh_format = .false.
        ! lun:  global variable declared in m_delwaq1_data
        ilun(1) = this%lun(26)   ! ilun: global variable declared in rd_token
        lch(1) = this%lchar(26)  ! lch:  global variable declared in rd_token

        !! Open input file
        open (this%lun(26), file = this%lchar(26), status = 'old')
    end subroutine setup_case

    subroutine teardown_case(fx)
        !< Steps required after running each test case.
        type(bc_fixture), intent(inout) :: fx                 !< Fixture containing all local variables

        if (allocated(fx%sname)) then
            deallocate(fx%sname)
        end if
        if (allocated(fx%bc_types)) then
            deallocate(fx%bc_types)
        end if
        if (allocated(fx%bc_ids)) then
            deallocate(fx%bc_ids)
        end if
    end subroutine teardown_case

    subroutine call_test_dlwq5a_1_item_1_conc_1_const
        call test(test_dlwq5a_1_item_1_conc_1_const, &
                'One bc item, concentration of one substance and one data constant should be ok.')
    end subroutine call_test_dlwq5a_1_item_1_conc_1_const

    subroutine call_test_dlwq5a_1_item_2_conc_2_const
        call test(test_dlwq5a_1_item_2_conc_2_const, &
                'One bc item, concentration of two substances and two respective data constants should be ok.')
    end subroutine call_test_dlwq5a_1_item_2_conc_2_const

    subroutine call_test_dlwq5a_1_item_1_conc_1_tseries
        call test(test_dlwq5a_1_item_1_conc_1_tseries, &
                'One bc item, concentration of one substance and time series should be ok.')
    end subroutine call_test_dlwq5a_1_item_1_conc_1_tseries

    subroutine call_test_dlwq5a_1_item_2_conc_1_tseries
        call test(test_dlwq5a_1_item_2_conc_1_tseries, &
                'One bc item, concentration of two substances given time series of one column (using USEFOR) ' // &
                        'should be ok.')
    end subroutine call_test_dlwq5a_1_item_2_conc_1_tseries


    subroutine parse_bc(fixture)
        type(bc_fixture), intent(inout) :: fixture

        call read_boundary_concentrations(fixture%lun, fixture%lchar, 14, fixture%iwidth, fixture%icmax, &
                fixture%car, fixture%iimax, fixture%iar, fixture%irmax, fixture%rar, &
                fixture%sname, fixture%bc_ids, fixture%bc_types(1:fixture%nobtyp), fixture%nobnd, fixture%nosys, &
                fixture%nobtyp, fixture%drar, fixture%is_date_format, fixture%is_yyddhh_format, fixture%output_verbose_level, &
                fixture%ierr2, fixture%status)
    end subroutine parse_bc

    subroutine test_dlwq5a_1_item_1_conc_1_const
        !< Test defining one item, and one concentration defined by one constant
        type(bc_fixture) :: fx       !< fixture containing all local variables

        ! Arrange
        call setup_case(fx, ['OXY     ', 'CBOD5   ', 'Salinity'], ['River', 'Sea  ', 'River'], 'bc_1_item_1_conc_1_const')

        ! Act
        call parse_bc(fx)

        ! Assert
        call assert_true(fx%ierr2 == 0, 'Validate no error in this subroutine.')
        call assert_true(fx%status%ierr == 0, 'Validate no error in total.')
        call assert_true(fx%status%iwar == 0, 'Validate no warning.')
        call assert_equal(fx%iar(1), -2, 'Validate index of boundary type.')
        call assert_equal(fx%car(1), 'Sea', 'Validate name of boundary type.')
        call assert_equal(fx%car(3), 'OXY', 'Validate name of substance in boundary condition.')
        call assert_true(fx%rar(1) == 3.0, 'Validate value of boundary condition.')

        ! Tear down
        call teardown_case(fx)
    end subroutine test_dlwq5a_1_item_1_conc_1_const

    subroutine test_dlwq5a_1_item_2_conc_2_const
        !< Test defining one item, and one concentration defined by one constant
        type(bc_fixture) :: fx       !< fixture containing all local variables

        ! Arrange
        call setup_case(fx, ['OXY     ', 'CBOD5   ', 'Salinity'], ['River', 'Sea  ', 'River'], 'bc_1_item_2_conc_2_const')

        ! Act
        call parse_bc(fx)

        ! Assert
        call assert_true(fx%ierr2 == 0, 'Validate no error in this subroutine.')
        call assert_true(fx%status%ierr == 0, 'Validate no error in total.')
        call assert_true(fx%status%iwar == 0, 'Validate no warning.')
        call assert_equal(fx%iar(1), -2, 'Validate index of "Sea" boundary type.')
        call assert_equal(fx%iar(2), 1, 'Validate index of "OXY" in substance names array.')
        call assert_equal(fx%iar(3), 3, 'Validate index of "Salinity" in substance names array.')
        call assert_equal(fx%car(1), 'Sea', 'Validate name of boundary type.')
        call assert_equal(fx%car(3), 'OXY', 'Validate name of first substance in boundary condition.')
        call assert_equal(fx%car(4), 'Salinity', 'Validate name of second substance in boundary condition.')
        call assert_true(fx%rar(1) == 3.0, 'Validate value of boundary condition for first substance.')
        call assert_true(fx%rar(2) == 35.0, 'Validate value of boundary condition for second substance.')

        ! Tear down
        call teardown_case(fx)
    end subroutine test_dlwq5a_1_item_2_conc_2_const

    subroutine test_dlwq5a_1_item_1_conc_1_tseries

        !< Test defining one bc item, and one concentration defined by a time series
        type(bc_fixture) :: fx       !< fixture containing all local variables

        ! Arrange
        call setup_case(fx, ['Salinity', 'CBOD5   ', 'OXY     '], ['Sea  ', 'River', 'River'], 'bc_1_item_1_conc_1_tseries')

        ! Act
        call parse_bc(fx)

        ! Assert
        call assert_true(fx%ierr2 == 0, 'Validate no error in this subroutine.')
        call assert_true(fx%status%ierr == 0, 'Validate no error in total.')
        call assert_true(fx%status%iwar == 0, 'Validate no warning.')
        call assert_equal(fx%iar(1), -1, 'Validate index of "Sea" boundary type.')
        call assert_equal(fx%iar(2), 3, 'Validate index of "OXY" in substance names array.')
        call assert_equal(fx%iar(4), 86400, 'Validate time duration of first block in time series.')
        call assert_equal(fx%car(1), 'Sea', 'Validate name of boundary type.')
        call assert_equal(fx%car(3), 'OXY', 'Validate name of first substance in boundary condition.')
        call assert_true(fx%rar(1) == 5.0, 'Validate value of boundary condition for first block in time series.')
        call assert_true(fx%rar(2) == 4.0, 'Validate value of boundary condition for second block in time series.')

        ! Tear down
        call teardown_case(fx)
    end subroutine test_dlwq5a_1_item_1_conc_1_tseries

    subroutine test_dlwq5a_1_item_2_conc_1_tseries


        !! Test defining one bc item, and concentration of two substances employing USEFOR to be defined by a time
        !! series of one column.
        type(bc_fixture) :: fx       !< fixture containing all local variables

        ! Arrange
        call setup_case(fx, ['Salinity', 'OXY     ', 'CBOD5   '], ['Sea  ', 'River', 'River'], 'bc_1_item_2_conc_1_tseries')

        ! Act
        call parse_bc(fx)

        ! Assert
        call assert_true(fx%ierr2 == 0, 'Validate no error in this subroutine.')
        call assert_true(fx%status%ierr == 0, 'Validate no error in total.')
        call assert_true(fx%status%iwar == 0, 'Validate no warning.')
        call assert_equal(fx%iar(1), -1, 'Validate index of "Sea" boundary type.')
        call assert_equal(fx%iar(2), 2, 'Validate index of "OXY" in substance names array.')
        call assert_equal(fx%iar(5), 86400, 'Validate time duration of first block in time series.')
        call assert_equal(fx%car(1), 'Sea', 'Validate name of boundary type.')
        call assert_equal(fx%car(3), 'OXY', 'Validate name of first substance in boundary condition.')
        call assert_equal(fx%car(4), 'Salinity', 'Validate name of second substance in boundary condition.')
        call assert_equal(fx%car(5), &
                'Salinity', 'Validate name of first substance in boundary condition after substitutions (USEFOR).')
        call assert_equal(fx%car(6), &
                'Salinity', 'Validate name of second substance in boundary condition after substitutions (USEFOR).')
        call assert_true(fx%rar(1) == 5.0, 'Validate value of boundary condition for first block in time series.')
        call assert_true(fx%rar(2) == 4.0, 'Validate value of boundary condition for second block in time series.')
        call assert_true(fx%rar(3) == 5.0, &
                'Validate value of boundary condition for first substance in first block of time series.')
        call assert_true(fx%rar(4) == 5.0, &
                'Validate value of boundary condition for second substance in first block of time series.')
        call assert_true(fx%rar(5) == 4.0, &
                'Validate value of boundary condition for first substance in second block of time series.')
        call assert_true(fx%rar(6) == 4.0, &
                'Validate value of boundary condition for second substance in second block of time series.')

        ! Tear down
        call teardown_case(fx)
    end subroutine test_dlwq5a_1_item_2_conc_1_tseries

end program test_read_boundary_concentrations

