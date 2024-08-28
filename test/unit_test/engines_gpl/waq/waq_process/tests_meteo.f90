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

program tests_meteo
    !!  Tests_meteo.f90
    !!  Runs unit tests for the METEO process routine

    use m_waq_precision
    use ftnunit, only: runtests_init, runtests, runtests_final, assert_comparable, test
    use m_waqmeteo

    implicit none
    character(len=200) :: cmd_arg
    integer :: iargc
    real(kind=real_wp), parameter :: tolerance = 0.0001

    ! Administrative arrays and variables
    integer, parameter :: num_process_parameters = 102
    integer, parameter :: num_cells = 1
    integer, parameter :: noflux = 0
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
        case('test_meteo_nearest')
            call test(test_meteo_nearest, 'Tests for mearest-neighbour interpolation meteo data')
        case('test_meteo_linear')
            call test(test_meteo_linear, 'Tests for linear interpolation meteo data')
        case('test_meteo_quadratic')
            call test(test_meteo_quadratic, 'Tests for quadratic interpolation meteo data')

        case default
            write(*,*) "Unknown test case: " ,trim(cmd_arg)
        end select

    else
        write(*,*) "No test specified, running all tests"

        call test(test_meteo_nearest, 'Tests for mearest-neighbour interpolation meteo data')
        call test(test_meteo_linear, 'Tests for linear interpolation meteo data')
        call test(test_meteo_quadratic, 'Tests for quadratic interpolation meteo data')
    endif

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

        iknmrk(1) = 11   ! Surface, active

    end subroutine fill_administration

    ! Fill the process parameters:
    ! - Two "meteo" stations
    ! - Use the seven quantities for specific interpolation patterns
    !
    subroutine fill_process_space_real_defaults

        process_space_real( 1) =      1.00000   ! Rad_1          x Global radiation at station 1                          (W/m2)
        process_space_real( 2) =      0.00000   ! VWind_1        x Wind velocity at staion 1                              (m/s)
        process_space_real( 3) =      1.00000   ! WinDir_1       x wind direction (pair 1)                                (degrees)
        process_space_real( 4) =      1.00000   ! RelHum_1       x Relative air humidity at station 1                     (%)
        process_space_real( 5) =     10.00000   ! AirTemp_1      x Air temperature at station 1                           (oC)
        process_space_real( 6) =      1.00000   ! AirPres_1      x Air pressure at station 1                              (mbar)
        process_space_real( 7) =     -1.00000   ! Cloud_1        x Cloudiness at station 1                                (%)
        process_space_real( 8) =      1.00000   ! Rad_2          x Global radiation at station 2                          (W/m2)
        process_space_real( 9) =      2.00000   ! VWind_2        x Wind velocity at staion 2                              (m/s)
        process_space_real(10) =      0.00000   ! WinDir_2       x wind direction (pair 2)                                (degrees)
        process_space_real(11) =      2.00000   ! RelHum_2       x Relative air humidity at station 2                     (%)
        process_space_real(12) =      1.00000   ! AirTemp_2      x Air temperature at station 2                           (oC)
        process_space_real(13) =     -1.00000   ! AirPres_2      x Air pressure at station 2                              (mbar)
        process_space_real(14) =      1.00000   ! Cloud_2        x Cloudiness at station 2                                (%)

        ! Unused ...
        process_space_real(15) =      0.00000   ! Rad_3          x Global radiation at station 3                          (W/m2)
        process_space_real(16) =      0.00000   ! VWind_3        x Wind velocity at staion 3                              (m/s)
        process_space_real(17) =      0.00000   ! WinDir_3       x wind direction (pair 3)                                (degrees)
        process_space_real(18) =      0.00000   ! RelHum_3       x Relative air humidity at station 3                     (%)
        process_space_real(19) =      0.00000   ! AirTemp_3      x Air temperature at station 3                           (oC)
        process_space_real(20) =      0.00000   ! AirPres_3      x Air pressure at station 3                              (mbar)
        process_space_real(21) =      0.00000   ! Cloud_3        x Cloudiness at station 3                                (%)
        process_space_real(22) =      0.00000   ! Rad_4          x Global radiation at station 4                          (W/m2)
        process_space_real(23) =      0.00000   ! VWind_4        x Wind velocity at staion 4                              (m/s)
        process_space_real(24) =      0.00000   ! WinDir_4       x wind direction (pair 4)                                (degrees)
        process_space_real(25) =      0.00000   ! RelHum_4       x Relative air humidity at station 4                     (%)
        process_space_real(26) =      0.00000   ! AirTemp_4      x Air temperature at station 4                           (oC)
        process_space_real(27) =      0.00000   ! AirPres_4      x Air pressure at station 4                              (mbar)
        process_space_real(28) =      0.00000   ! Cloud_4        x Cloudiness at station 4                                (%)
        process_space_real(29) =      0.00000   ! Rad_5          x Global radiation at station 5                          (W/m2)
        process_space_real(30) =      0.00000   ! VWind_5        x Wind velocity at staion 5                              (m/s)
        process_space_real(31) =      0.00000   ! WinDir_5       x wind direction (pair 5)                                (degrees)
        process_space_real(32) =      0.00000   ! RelHum_5       x Relative air humidity at station 5                     (%)
        process_space_real(33) =      0.00000   ! AirTemp_5      x Air temperature at station 5                           (oC)
        process_space_real(34) =      0.00000   ! AirPres_5      x Air pressure at station 5                              (mbar)
        process_space_real(35) =      0.00000   ! Cloud_5        x Cloudiness at station 5                                (%)
        process_space_real(36) =      0.00000   ! Rad_6          x Global radiation at station 6                          (W/m2)
        process_space_real(37) =      0.00000   ! VWind_6        x Wind velocity at staion 6                              (m/s)
        process_space_real(38) =      0.00000   ! WinDir_6       x wind direction (pair 6)                                (degrees)
        process_space_real(39) =      0.00000   ! RelHum_6       x Relative air humidity at station 6                     (%)
        process_space_real(40) =      0.00000   ! AirTemp_6      x Air temperature at station 6                           (oC)
        process_space_real(41) =      0.00000   ! AirPres_6      x Air pressure at station 6                              (mbar)
        process_space_real(42) =      0.00000   ! Cloud_6        x Cloudiness at station 6                                (%)
        process_space_real(43) =      0.00000   ! Rad_7          x Global radiation at station 7                          (W/m2)
        process_space_real(44) =      0.00000   ! VWind_7        x Wind velocity at staion 7                              (m/s)
        process_space_real(45) =      0.00000   ! WinDir_7       x wind direction (pair 7)                                (degrees)
        process_space_real(46) =      0.00000   ! RelHum_7       x Relative air humidity at station 7                     (%)
        process_space_real(47) =      0.00000   ! AirTemp_7      x Air temperature at station 7                           (oC)
        process_space_real(48) =      0.00000   ! AirPres_7      x Air pressure at station 7                              (mbar)
        process_space_real(49) =      0.00000   ! Cloud_7        x Cloudiness at station 7                                (%)
        process_space_real(50) =      0.00000   ! Rad_8          x Global radiation at station 8                          (W/m2)
        process_space_real(51) =      0.00000   ! VWind_8        x Wind velocity at staion 8                              (m/s)
        process_space_real(52) =      0.00000   ! WinDir_8       x wind direction (pair 8)                                (degrees)
        process_space_real(53) =      0.00000   ! RelHum_8       x Relative air humidity at station 8                     (%)
        process_space_real(54) =      0.00000   ! AirTemp_8      x Air temperature at station 8                           (oC)
        process_space_real(55) =      0.00000   ! AirPres_8      x Air pressure at station 8                              (mbar)
        process_space_real(56) =      0.00000   ! Cloud_8        x Cloudiness at station 8                                (%)
        process_space_real(57) =      0.00000   ! Rad_9          x Global radiation at station 9                          (W/m2)
        process_space_real(58) =      0.00000   ! VWind_9        x Wind velocity at staion 9                              (m/s)
        process_space_real(59) =      0.00000   ! WinDir_9       x wind direction (pair 9)                                (degrees)
        process_space_real(60) =      0.00000   ! RelHum_9       x Relative air humidity at station 9                     (%)
        process_space_real(61) =      0.00000   ! AirTemp_9      x Air temperature at station 9                           (oC)
        process_space_real(62) =      0.00000   ! AirPres_9      x Air pressure at station 9                              (mbar)
        process_space_real(63) =      0.00000   ! Cloud_9        x Cloudiness at station 9                                (%)
        process_space_real(64) =      0.00000   ! Rad_10         x Global radiation at station 10                         (W/m2)
        process_space_real(65) =      0.00000   ! VWind_10       x Wind velocity at staion 10                             (m/s)
        process_space_real(66) =      0.00000   ! WinDir_10      x wind direction (pair 10)                               (degrees)
        process_space_real(67) =      0.00000   ! RelHum_10      x Relative air humidity at station 10                    (%)
        process_space_real(68) =      0.00000   ! AirTemp_10     x Air temperature at station 10                          (oC)
        process_space_real(69) =      0.00000   ! AirPres_10     x Air pressure at station 10                             (mbar)
        process_space_real(70) =      0.00000   ! Cloud_10       x  Cloudiness at station 10                              (%)

        ! Coordinates
        process_space_real(71) =    -10.00000   ! XMeteo1        x X-coordinate (metric) of station 1                     (m)
        process_space_real(72) =      0.00000   ! YMeteo1        x Y-coordinate (metric) of station 1                     (m)
        process_space_real(73) =     20.00000   ! XMeteo2        x X-coordinate (metric) of station 2                     (m)
        process_space_real(74) =      0.00000   ! YMeteo2        x Y-coordinate (metric) of station 2                     (m)

        ! Unused
        process_space_real(75) =      0.00000   ! XMeteo3        x X-coordinate (metric) of station 3                     (m)
        process_space_real(76) =      0.00000   ! YMeteo3        x Y-coordinate (metric) of station 3                     (m)
        process_space_real(77) =      0.00000   ! XMeteo4        x X-coordinate (metric) of station 4                     (m)
        process_space_real(78) =      0.00000   ! YMeteo4        x Y-coordinate (metric) of station 4                     (m)
        process_space_real(79) =      0.00000   ! XMeteo5        x X-coordinate (metric) of station 5                     (m)
        process_space_real(80) =      0.00000   ! YMeteo5        x Y-coordinate (metric) of station 5                     (m)
        process_space_real(81) =      0.00000   ! XMeteo6        x X-coordinate (metric) of station 6                     (m)
        process_space_real(82) =      0.00000   ! YMeteo6        x Y-coordinate (metric) of station 6                     (m)
        process_space_real(83) =      0.00000   ! XMeteo7        x X-coordinate (metric) of station 7                     (m)
        process_space_real(84) =      0.00000   ! YMeteo7        x Y-coordinate (metric) of station 7                     (m)
        process_space_real(85) =      0.00000   ! XMeteo8        x X-coordinate (metric) of station 8                     (m)
        process_space_real(86) =      0.00000   ! YMeteo8        x Y-coordinate (metric) of station 8                     (m)
        process_space_real(87) =      0.00000   ! XMeteo9        x X-coordinate (metric) of station 9                     (m)
        process_space_real(88) =      0.00000   ! YMeteo9        x Y-coordinate (metric) of station 9                     (m)
        process_space_real(89) =      0.00000   ! XMeteo10       x X-coordinate (metric) of station 10                    (m)
        process_space_real(90) =      0.00000   ! YMeteo10       x Y-coordinate (metric) of station 10                    (m)
        process_space_real(91) =      1.00000   ! XYScaleFac     x Mult for meteo to segment coordinates                  (-)
                                                ! Note: the weighting factor is calculated as max(1/d, 1), so scale the coordinates!

        ! Changed per test
        process_space_real(92) =      2.00000   ! NoMeteoSta     x Number of Meteo stations in calculation                (-)
        process_space_real(93) =      1.00000   ! MeteoClcSW     x Option (1=nearest; 2= lin dist 3=squared dist )        (-)

        process_space_real(94) =      0.00000   ! XSeg           x X-coordinate of DELWAQ segment (metric)                (m)
        process_space_real(95) =      0.00000   ! YSeg           x Y-coordinate of DELWAQ segment (metric)                (m)

        ! Output
        process_space_real(96) =      -999.0    ! RadSW          x Short wave radiation reaching water                    (W/m2)
        process_space_real(97) =      -999.0    ! VWind          x wind speed                                             (m/s)
        process_space_real(98) =      -999.0    ! WinDir         x Wind direction at location                             (degrees)
        process_space_real(99) =      -999.0    ! RelHumAir      x Relative air humidity                                  (%)
        process_space_real(100)=      -999.0    ! TempAir        x Air temperature                                        (oC)
        process_space_real(101)=      -999.0    ! PAtm           x Air pressure                                           (mbar)
        process_space_real(102)=      -999.0    ! Cloud          x Cloud coverage                                         (%)

    end subroutine fill_process_space_real_defaults

    ! Test the nearest-neighbour options of the METEO process routine:
    !
    subroutine test_meteo_nearest
                                                             !Rad   VWind  WinDir  Relhum  AirTemp  AirPres  Cloud
        real(kind=real_wp) :: expected_meteo(7,2) = reshape( [1.0,  0.0,   1.0,    1.0,    10.0,     1.0,     -1.0,        &
                                                              1.0,  2.0,   0.0,    2.0,     1.0,    -1.0,      1.0], [7,2] )
        integer            :: i

        call fill_process_space_real_defaults

        process_space_real(92) =  2.0  ! Two stations
        process_space_real(93) =  1.0  ! Nearest neighbour

        ! Nearest neighbour: first nearest to first, then to second

        process_space_real(94) =  0.0
        process_space_real(95) =  0.0

        call meteo( process_space_real, fl, ipoint, increm, num_cells, &
                    noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                    num_exchanges_z_dir, num_exchanges_bottom_dir )

        do i = 1,7
            call assert_comparable( process_space_real(95+i), expected_meteo(i,1), tolerance, 'Meteo output')
        enddo

        process_space_real(94) = 15.0
        process_space_real(95) =  0.0

        call meteo( process_space_real, fl, ipoint, increm, num_cells, &
                    noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                    num_exchanges_z_dir, num_exchanges_bottom_dir )

        do i = 1,7
            write(*,*) "B", i
            call assert_comparable( process_space_real(95+i), expected_meteo(i,2), tolerance, 'Meteo output')
        enddo

    end subroutine test_meteo_nearest

    ! Test the linear options of the METEO process routine:
    !
    subroutine test_meteo_linear
                                                  !Note: wind direction is NOT averaged!
                                                  !Rad    VWind      WinDir     Relhum      AirTemp    AirPres   Cloud
        real(kind=real_wp) :: expected_meteo(7) = [1.0,   0.6666667, 1.0,       1.3333333,  7.0000000, 0.333333, -0.333333]
        integer            :: i

        call fill_process_space_real_defaults

        process_space_real(92) =  2.0  ! Two stations
        process_space_real(93) =  2.0  ! Linear

        call meteo( process_space_real, fl, ipoint, increm, num_cells, &
                    noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                    num_exchanges_z_dir, num_exchanges_bottom_dir )

        do i = 1,7
            call assert_comparable( process_space_real(95+i), expected_meteo(i), tolerance, 'Meteo output')
        enddo

    end subroutine test_meteo_linear

    ! Test the quadratic options of the METEO process routine:
    !
    subroutine test_meteo_quadratic
                                                  !Note: wind direction is NOT averaged!
                                                  !Rad    VWind      WinDir     Relhum     AirTemp   AirPres  Cloud
        real(kind=real_wp) :: expected_meteo(7) = [1.0,   0.4000000, 1.0,       1.200000,  8.200000, 0.60000, -0.60000]
        integer            :: i

        call fill_process_space_real_defaults

        process_space_real(92) =  2.0  ! Two stations
        process_space_real(93) =  3.0  ! Quadratic

        call meteo( process_space_real, fl, ipoint, increm, num_cells, &
                    noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                    num_exchanges_z_dir, num_exchanges_bottom_dir )

        do i = 1,7
            call assert_comparable( process_space_real(95+i), expected_meteo(i), tolerance, 'Meteo output')
        enddo

    end subroutine test_meteo_quadratic

end program tests_meteo
