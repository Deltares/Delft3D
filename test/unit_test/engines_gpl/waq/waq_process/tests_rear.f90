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

program tests_rear
    !!  Tests_rear.f90
    !!  Runs unit tests for the REAR process routine y

    use m_waq_precision
    use ftnunit, only: runtests_init, runtests, runtests_final, assert_comparable, test
    use m_rear

    implicit none
    character(len=200) :: cmd_arg
    integer :: iargc
    real(kind=real_wp), parameter :: tolerance = 0.0001

    ! Administrative arrays and variables
    integer, parameter :: num_process_parameters = 28
    integer, parameter :: num_cells = 2                  ! Tests concern two cells: one at the surface, one not
    integer, parameter :: noflux = 1
    integer, parameter :: num_exchanges_u_dir = 0
    integer, parameter :: num_exchanges_v_dir = 0
    integer, parameter :: num_exchanges_z_dir = 0
    integer, parameter :: num_exchanges_bottom_dir = 0
    integer            :: ipoint(num_process_parameters)
    integer            :: increm(num_process_parameters)
    integer            :: iexpnt(num_exchanges_u_dir+num_exchanges_v_dir+num_exchanges_z_dir+num_exchanges_bottom_dir)  ! Exchanges play no role
    integer            :: iknmrk(num_cells)  ! One cell only

    real(kind=real_wp) :: process_space_real(num_process_parameters+2) ! Accommodate for the output parameters - two cells
    real(kind=real_wp) :: fl(noflux*num_cells)

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
        case('test_rear_simple')
            call test(test_rear_simple, 'Simple reaeration formulae')

        case default
            write(*,*) "Unknown test case: " ,trim(cmd_arg)
        end select

    else
        write(*,*) "No test specified, running all tests"

        call test(test_rear_simple, 'Simple reaeration formulae')
    endif
    
    continue

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

        increm = 0
        do i = 1,num_process_parameters
            ipoint(i) = i
        enddo

        ! Correct for the output parameters - two cells, so both take two elements
        ipoint(28) = ipoint(28) + 1
        increm(27) = 1
        increm(28) = 1

        iknmrk(1) = 11   ! Surface, active
        iknmrk(2) = 21   ! Below the surface, active

    end subroutine fill_administration

    ! Fill the process parameters with their default values
    ! (Taken from the processes configuration)
    !
    subroutine fill_process_space_real_defaults

        process_space_real(1) =       -999.000      ! OXY        x Dissolved Oxygen                                       (g/m3)
        process_space_real(2) =       -999.000      ! Depth      x depth of segment                                       (m)
        process_space_real(3) =        15.0000      ! Temp       x ambient water temperature                              (oC)
        process_space_real(4) =       -999.000      ! Velocity   x horizontal flow velocity                               (m/s)
        process_space_real(5) =        3.00000      ! VWind      x wind speed                                             (m/s)
        process_space_real(6) =        1.00000      ! SWRear     x switch for oxygen reaeration formulation (1-13)        (-)
        process_space_real(7) =        1.00000      ! KLRear     x reaeration transfer coefficient                        (m/d)
        process_space_real(8) =        1.01600      ! TCRear     x temperature coefficient for rearation                  (-)
        process_space_real(9) =       -999.000      ! DELT       x timestep for processes                                 (d)
        process_space_real(10) =      -999.000      ! SaturOXY   x saturation concentration                               (gO2/m3)
        process_space_real(11) =       35.0000      ! Salinity   x Salinity                                               (g/kg)
        process_space_real(12) =      -999.000      ! TotalDepth x total depth water column                               (m)
        process_space_real(13) =       0.00000      ! fcover     x fraction of water surface covered <0-1>                (-)
        process_space_real(14) =       1000.00      ! KLRearMax  x maximum KLREAR oxygen for temp. correction             (m/d)
        process_space_real(15) =      0.200000      ! KLRearMin  x minimum rearation transfer coefficient oxygen          (m/d)
        process_space_real(16) =       0.00000      ! Rain       x rainfall rate                                          (mm/h)
        process_space_real(17) =       1.66000      ! coefAOxy   x gas transfer Oxy coefficient transmission              (m/d)
        process_space_real(18) =      0.260000      ! coefB1Oxy  x gas transfer O2 coefficient wind scale 1               (-)
        process_space_real(19) =       1.00000      ! coefB2Oxy  x gas transfer O2 coefficient wind scale 2               (-)
        process_space_real(20) =      0.660000      ! coefC1Oxy  x gas transfer O2 coefficient rain scale 1               (-)
        process_space_real(21) =       1.00000      ! coefC2Oxy  x gas transfer O2 coefficient rain scale 2               (-)
        process_space_real(22) =       1800.06      ! coefD1Oxy  x fresh water coefficient1 for Schmidt nr Oxy            (-)
        process_space_real(23) =       120.100      ! coefD2Oxy  x fresh water coefficient2 for Schmidt nr Oxy            (-)
        process_space_real(24) =       3.78180      ! coefD3Oxy  x fresh water coefficient3 for Schmidt nr Oxy            (-)
        process_space_real(25) =      0.476080E-01  ! coefD4Oxy  x fresh water coefficient4 for Schmidt nr Oxy            (-)
        process_space_real(26) =       0.00000      ! coefD5Oxy  x fresh water coefficient5 for Schmidt nr Oxy            (-)

        ! Output
        process_space_real(27) =      -999.0        ! KLREAR       reaeration coefficient cell 1                          (m/d)
        process_space_real(28) =      -999.0        ! KLREAR       reaeration coefficient cell 2                          (m/d)
        process_space_real(29) =      -999.0        ! SATPEC       percentage of saturation cell 1                        (-)
        process_space_real(30) =      -999.0        ! SATPEC       percentage of saturation cell 2                        (-)

    end subroutine fill_process_space_real_defaults

    ! Test the simple options of the REAR process routine:
    ! - IFREAR = 0, 1, 2, 3, 4, 5, 6, 7, 9
    ! - These require a flow velocity, a temperature, a depth and a wind velocity
    !
    subroutine test_rear_simple
        integer       :: ifrear

        real(kind=real_wp) :: expected_rear(0:9 ) = [1.0,       0.1,     5.4516e-2, 8.6379e-2, 8.6379e-2, &
                                                     4.7248e-2, 0.26082, 1.0223,      -999.0,    0.4332     ]
        real(kind=real_wp) :: expected_satur(0:9) = 8.0 / 10.0 * 100.0

        call fill_process_space_real_defaults

        ! Set flow and wind velocity and depth to distinguishable values
        process_space_real(1)  =  8.0       ! Oxygen
        process_space_real(2)  = 10.0       ! Depth
        process_space_real(3)  = 20.0       ! Temperature
        process_space_real(4)  =  0.5       ! Flow velocity
        process_space_real(5)  = 12.0       ! Wind velocity
        process_space_real(9)  =  0.02      ! Time step in days, about half an hour
        process_space_real(10) = 10.0       ! Saturation concentration

        process_space_real(12)  = process_space_real(2)  ! Total depth, used by the simplest options

        do ifrear = 0,9
            if ( ifrear == 8 ) then
                cycle
            endif

            process_space_real(6) = ifrear

            fl = 0.0_real_wp

            call rear( process_space_real, fl, ipoint, increm, num_cells, &
                       noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                       num_exchanges_z_dir, num_exchanges_bottom_dir )

            ! Check that there is no flux for cell 2 - below the surface

            call assert_comparable(fl(2), 0.0_real_wp, tolerance, 'Reaeration flux for cell below surface')

            ! Check the values

            call assert_comparable(process_space_real(27), expected_rear(ifrear),  tolerance, 'Reaeration coefficient at surface')
            call assert_comparable(process_space_real(28), -999.0_real_wp, tolerance, 'Reaeration coefficient below surface')
            call assert_comparable(process_space_real(29), expected_satur(ifrear), tolerance, 'Saturation percentage at surface')
            call assert_comparable(process_space_real(30), expected_satur(ifrear), tolerance, 'Saturation percentage below surface')
        enddo

    end subroutine test_rear_simple

end program tests_rear
