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

program tests_debgrz_computations
    !!  tests_debgrz_computations.f90
    !!  Runs unit tests for debgrz_computations

    use m_waq_precision
    use ftnunit, only: runtests_init, runtests, runtests_final, assert_comparable, test
    use m_debgrz_computations, only: get_maximum_conversion_coeff, &
                                     temperature_dependent_rate, &
                                     calculate_uptake, &
                                     calculate_rescale_factors, &
                                     rescale_non_food_vars, &
                                     rescale_food_arrays, &
                                     calculate_defaecation, &
                                     calculate_energy_reserve_dynamics, &
                                     calculate_maintenance, &
                                     calculate_growth, &
                                     get_maturity_fractions, &
                                     get_gsi, &
                                     calculate_maturity_and_reproduction_energies, &
                                     calculate_spawning, &
                                     calculate_respiration, &
                                     calculate_shell_formation_fluxes, &
                                     calculate_mortality


    implicit none
    character(len=200) :: cmd_arg
    integer :: iargc
    real(kind=real_wp), parameter :: tolerance = 0.00001

    ! Determine the number of command line arguments
    iargc = command_argument_count()
    call prepare_tests()
    call runtests_init()

    ! Run the test specified in the argument, if no argument run all tests
    if (iargc > 0) then
        call get_command_argument(1, cmd_arg)

        select case (trim(cmd_arg))
        case('test_debgrz_temperature_dependent_rate')
            write(*,*) "Running test_debgrz_temperature_dependent_rate"
            call runtests(run_test_debgrz_temperature_dependent_rate)
        case('test_debgrz_calculate_uptake')
            write(*,*) "Running test_debgrz_calculate_uptake"
            call runtests(run_test_debgrz_calculate_uptake)
        case('test_debgrz_get_maximum_conversion_coeff')
            write(*,*) "Running test_get_maximum_conversion_coeff"
            call runtests(run_test_debgrz_get_maximum_conversion_coeff)
        case('test_debgrz_calculate_rescale_factors')
            write(*,*) "Running test_debgrz_calculate_rescale_factors"
            call runtests(run_test_debgrz_calculate_rescale_factors)
        case('test_debgrz_rescale_non_food_vars')
            write(*,*) "Running test_debgrz_calculate_rescale_factors"
            call runtests(run_test_debgrz_rescale_non_food_vars)
        case('test_debgrz_rescale_food_arrays')
            write(*,*) "Running test_rescale_food_arrays"
            call runtests(run_test_debgrz_rescale_food_arrays)
        case('test_debgrz_calculate_defaecation')
            write(*,*) "Running test_debgrz_calculate_defaecation"
            call runtests(run_test_debgrz_calculate_defaecation)
        case('test_debgrz_calculate_energy_reserve_dynamics')
            write(*,*) "Running test_debgrz_calculate_energy_reserve_dynamics"
            call runtests(run_test_debgrz_calculate_energy_reserve_dynamics)
        case('test_debgrz_calculate_maintenance')
            write(*,*) "Running test_debgrz_calculate_maintenance"
            call runtests(run_test_debgrz_calculate_maintenance)
        case('test_debgrz_calculate_growth')
            write(*,*) "Running test_debgrz_calculate_growth"
            call runtests(run_test_debgrz_calculate_growth)
        case('test_debgrz_get_maturity_fractions')
            write(*,*) "Running test_debgrz_get_maturity_fractions"
            call runtests(run_test_debgrz_get_maturity_fractions)
        case('test_debgrz_get_gsi')
            write(*,*) "Running test_debgrz_get_gsi"
            call runtests(run_test_debgrz_get_gsi)
        case('test_debgrz_calculate_maturity_and_reproduction_energies')
            write(*,*) "Running test_debgrz_calculate_maturity_and_reproduction_energies"
            call runtests(run_test_debgrz_calculate_maturity_and_reproduction_energies)
        case('test_debgrz_calculate_spawning')
            write(*,*) "Running test_debgrz_calculate_spawning"
            call runtests(run_test_debgrz_calculate_spawning)
        case('test_debgrz_calculate_respiration')
            write(*,*) "Running test_debgrz_calculate_respiration"
            call runtests(run_test_debgrz_calculate_respiration)
        case('test_debgrz_calculate_shell_formation_fluxes')
            write(*,*) "Running test_debgrz_calculate_shell_formation_fluxes"
            call runtests(run_test_debgrz_calculate_shell_formation_fluxes)
        case('test_debgrz_calculate_mortality')
            write(*,*) "Running test_debgrz_calculate_mortality"
            call runtests(run_test_debgrz_calculate_mortality)
        end select
    else
        write(*,*) "No test specified, running all tests"
        call run_test_debgrz_temperature_dependent_rate()
        call run_test_debgrz_calculate_uptake()
        call run_test_debgrz_get_maximum_conversion_coeff()
        call run_test_debgrz_calculate_rescale_factors()
        call run_test_debgrz_rescale_non_food_vars()
        call run_test_debgrz_rescale_food_arrays()
        call run_test_debgrz_calculate_defaecation()
        call run_test_debgrz_calculate_energy_reserve_dynamics()
        call run_test_debgrz_calculate_maintenance()
        call run_test_debgrz_calculate_growth()
        call run_test_debgrz_get_maturity_fractions()
        call run_test_debgrz_get_gsi()
        call run_test_debgrz_calculate_maturity_and_reproduction_energies()
        call run_test_debgrz_calculate_spawning()
        call run_test_debgrz_calculate_respiration()
        call run_test_debgrz_calculate_shell_formation_fluxes()
        call run_test_debgrz_calculate_mortality()
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

    subroutine run_test_debgrz_temperature_dependent_rate()
        call test(test_debgrz_temperature_dependent_rate, 'Arrhenius temperature dependent rate.')
    end subroutine run_test_debgrz_temperature_dependent_rate

    subroutine run_test_debgrz_calculate_uptake()
        call test(test_debgrz_calculate_uptake, ' Calculation of uptake')
    end subroutine run_test_debgrz_calculate_uptake

    subroutine run_test_debgrz_get_maximum_conversion_coeff()
        call test(test_debgrz_get_maximum_conversion_coeff, 'Get maximum conversion coefficient for costs for growth.')
    end subroutine run_test_debgrz_get_maximum_conversion_coeff

    subroutine run_test_debgrz_calculate_rescale_factors()
        call test(test_debgrz_calculate_rescale_factors, 'Calculation of rescale factors.')
    end subroutine run_test_debgrz_calculate_rescale_factors

    subroutine run_test_debgrz_rescale_non_food_vars()
        call test(test_debgrz_rescale_non_food_vars, 'Rescaling non food arrays variables.')
    end subroutine run_test_debgrz_rescale_non_food_vars

    subroutine run_test_debgrz_rescale_food_arrays()
        call test(test_debgrz_rescale_food_arrays, 'Rescaling food arrays.')
    end subroutine run_test_debgrz_rescale_food_arrays

    subroutine run_test_debgrz_calculate_defaecation()
        call test(test_debgrz_calculate_defaecation, 'Calculation of defecation')
    end subroutine run_test_debgrz_calculate_defaecation

    subroutine run_test_debgrz_calculate_energy_reserve_dynamics()
        call test(test_debgrz_calculate_energy_reserve_dynamics, 'Calculation of energy reserve dynamics')
    end subroutine run_test_debgrz_calculate_energy_reserve_dynamics

    subroutine run_test_debgrz_calculate_maintenance()
        call test(test_debgrz_calculate_maintenance, 'Calculation of Maintenance per individual')
    end subroutine run_test_debgrz_calculate_maintenance

    subroutine run_test_debgrz_calculate_growth()
        call test(test_debgrz_calculate_growth, 'Calculation of Growth per individual')
    end subroutine run_test_debgrz_calculate_growth

    subroutine run_test_debgrz_get_maturity_fractions()
        call test(test_debgrz_get_maturity_fractions, 'Calculation of juvenile and adult fractions')
    end subroutine run_test_debgrz_get_maturity_fractions

    subroutine run_test_debgrz_get_gsi()
        call test(test_debgrz_get_gsi, 'Calculation of gonadosomatic index (gsi)')
    end subroutine run_test_debgrz_get_gsi

    subroutine run_test_debgrz_calculate_maturity_and_reproduction_energies()
        call test(test_debgrz_calculate_maturity_and_reproduction_energies, 'Calculation of energies associated to maturity and reproduction')
    end subroutine run_test_debgrz_calculate_maturity_and_reproduction_energies

    subroutine run_test_debgrz_calculate_spawning()
        call test(test_debgrz_calculate_spawning, 'Calculation of spawning')
    end subroutine run_test_debgrz_calculate_spawning

    subroutine run_test_debgrz_calculate_respiration()
        call test(test_debgrz_calculate_respiration, 'Calculation of Respiration')
    end subroutine run_test_debgrz_calculate_respiration

    subroutine run_test_debgrz_calculate_shell_formation_fluxes()
        call test(test_debgrz_calculate_shell_formation_fluxes, 'Calculation of shell formation fluxes')
    end subroutine run_test_debgrz_calculate_shell_formation_fluxes

    subroutine run_test_debgrz_calculate_mortality()
        call test(test_debgrz_calculate_mortality, 'Calculation of mortality')
    end subroutine run_test_debgrz_calculate_mortality

    subroutine test_debgrz_temperature_dependent_rate()
        !< Local variables
        real(kind = real_wp) :: temp1, temp2, temp3 !< temperature
        real(kind = real_wp) :: ta1,   ta2          !< arrhenius_temp_K
        real(kind = real_wp) :: tal1,  tal2         !< temp_decrease_rate_at_lower_bound
        real(kind = real_wp) :: tah1,  tah2         !< temp_decrease_rate_at_upper_bound
        real(kind = real_wp) :: th1,   th2          !< temp_upper_tolerance
        real(kind = real_wp) :: tl1,   tl2          !< temp_lower_tolerance
        real(kind = real_wp) :: kt1, kt2, kt3, kt4, kt5, kt6 !< temperature dependent rate values

        ! Arrange
        temp1 = 5.0
        temp2 = 25.0
        ta1 = 5800.0
        ta2 = 2000.0
        tal1 = 45430.0
        tal2 = 60000.0
        tah1 = 31376.0
        tah2 = 50000.0
        th1 = 296.0
        th2 = 300.0
        tl1 = 275.0
        tl2 = 280.0

        ! Act
        kt1 = temperature_dependent_rate(temp2, ta1, tal1, tah1, th1, tl1)
        kt2 = temperature_dependent_rate(temp1, ta2, tal1, tah1, th1, tl2)
        kt3 = temperature_dependent_rate(temp1, ta1, tal2, tah1, th1, tl1)
        kt4 = temperature_dependent_rate(temp2, ta1, tal1, tah2, th1, tl1)
        kt5 = temperature_dependent_rate(temp2, ta1, tal1, tah1, th2, tl1)
        kt6 = temperature_dependent_rate(temp1, ta1, tal1, tah1, th1, tl2)

        ! Assert
        call assert_comparable(kt1, 0.614073581, tolerance, 'Validate temperature dependent rate: common values.')
        call assert_comparable(kt2, 0.219765004, tolerance, 'Validate temperature dependent rate: effect of ta.')
        call assert_comparable(kt3, 0.419487218, tolerance, 'Validate temperature dependent rate: effect of tal.')
        call assert_comparable(kt4, 0.399609783, tolerance, 'Validate temperature dependent rate: effect of tah.')
        call assert_comparable(kt5, 1.008649558, tolerance, 'Validate temperature dependent rate: effect of th.')
        call assert_comparable(kt6, 0.109155922, tolerance, 'Validate temperature dependent rate: effect of tl.')
    end subroutine test_debgrz_temperature_dependent_rate

    subroutine test_debgrz_calculate_uptake()
        integer(kind=int_wp) :: food_count         !< Number of food types
        integer(kind=int_wp) :: is_food_benthic(4) !< Is food type benthic (1) or pelagic (0)

        real(kind=real_wp) :: cfood(4)     !< Carbon foods
        real(kind=real_wp) :: fffood(4)    !< Faecal fraction of grazers
        real(kind=real_wp) :: ccfood(4)    !< Stoichiometry ratio of carbon to food unit
        real(kind=real_wp) :: ncfood(4)    !< Nitrogen foods
        real(kind=real_wp) :: pcfood(4)    !< Phosphorus foods
        real(kind=real_wp) :: sicfood(4)   !< Silicon foods
        real(kind=real_wp) :: pref(4)      !< Preference of DEB grazers
        real(kind=real_wp) :: suspension   !< DEB species preference for suspension over deposit feeding
        real(kind=real_wp) :: xk           !< Halfrate const food uptake Sup fdr               [gC/m3]
        real(kind=real_wp) :: minfood      !< Minimum amount of food for DEB species
        real(kind=real_wp) :: totaldepth   !< Depth of entire water column to which cell belongs   [m]
        real(kind=real_wp) :: conv_j_gc    !< Conversion factor from energy into mass           [gC/J]
        real(kind=real_wp) :: jxm_l2       !< Max ingestion rate of DEB species              [J/cm2/d]
        real(kind=real_wp) :: kappa_i      !< Ingestion efficiency (pseudofaeces production)       [-]
        real(kind=real_wp) :: tim          !< Total inorganic matter                          [gDM/m3]
        real(kind=real_wp) :: yk           !< Halfrate const TIM                               [gC/m3]
        real(kind=real_wp) :: kT           !< Temperature_dependent_rate
        real(kind=real_wp) :: v            !< Individual volume                              [cm3/ind]
        real(kind=real_wp) :: dens_m2      !< Density derived from Vtot per m2
        real(kind=real_wp) :: delt         !< Timestep for processes                               [d]
        real(kind=real_wp) :: depth        !< Depth of segment/cell                                [m]
        real(kind=real_wp) :: faecal_fraction !< Global value for faecal fraction

        real(kind=real_wp) :: dfil(4)      !< Daily filtration rate for each food type [gC/ind/d]

        real(kind=real_wp) :: c_filtr      !< Daily filtration rate for carbon     [gC/ind/d]
        real(kind=real_wp) :: n_filtr      !< Daily filtration rate for nitrogen   [gN/ind/d]
        real(kind=real_wp) :: p_filtr      !< Daily filtration rate for phosphorus [gP/ind/d]
        real(kind=real_wp) :: si_filtr     !< Daily filtration rate for silicon   [gSi/ind/d]
        real(kind=real_wp) :: food_pelagic !< Food for pelagic organisms
        real(kind=real_wp) :: food_benthic !< Food for benthic organisms

        ! Arrange
        food_count = 4
        is_food_benthic = [1, 1, 0, 0]
        cfood   = [0.1, 0.2, 0.3, 0.4]
        fffood  = [0.1, 0.2, 0.3, 0.4]
        ccfood  = [0.1, 0.2, 0.3, 0.4]
        ncfood  = [0.4, 0.1, 0.2, 0.3]
        pcfood  = [0.3, 0.4, 0.1, 0.2]
        sicfood = [0.2, 0.3, 0.4, 0.1]
        pref    = [0.1, 0.2, 0.3, 0.4]
        suspension = 0.8
        xk         = 0.7
        minfood    = 0.001
        totaldepth = 1.3
        conv_j_gc  = 0.3e-5
        jxm_l2     = 0.54
        kappa_i    = 0.25
        tim        = 0.03
        yk         = 0.6
        kT         = 0.123
        v          = 0.04
        dens_m2    = 0.33
        delt       = 1e-2
        depth      = 0.3
        dfil = [0.1, 0.2, 0.3, 0.4]

        ! Act
        call calculate_uptake(food_count, is_food_benthic, cfood, fffood, pref, suspension, xk, &
                              minfood, dfil, totaldepth, ccfood, ncfood, pcfood, sicfood, conv_j_gc, &
                              jxm_l2, kappa_i, tim, yk, c_filtr, n_filtr, p_filtr, si_filtr, kT, &
                              v, dens_m2, delt, depth, faecal_fraction, food_pelagic, food_benthic)

        ! Assert
        call assert_comparable(faecal_fraction,    0.3272,   tolerance, 'Validate faecal_fraction')
        call assert_comparable(cfood(1),             0.01,   tolerance, 'Validate Carbon foods(1)')
        call assert_comparable(cfood(2),             0.04,   tolerance, 'Validate Carbon foods(2)')
        call assert_comparable(cfood(3),             0.09,   tolerance, 'Validate Carbon foods(3)')
        call assert_comparable(cfood(4),             0.16,   tolerance, 'Validate Carbon foods(4)')
        call assert_comparable(dfil(1),     0.2842146E-09,   tolerance, 'Validate Daily filtration(1)')
        call assert_comparable(dfil(2),     0.1136858E-08,   tolerance, 'Validate Daily filtration(2)')
        call assert_comparable(dfil(3),     0.1068218E-07,   tolerance, 'Validate Daily filtration(3)')
        call assert_comparable(dfil(4),     0.1899054E-07,   tolerance, 'Validate Daily filtration(4)')
        call assert_comparable(c_filtr,     0.1105666E-07,   tolerance, 'Validate Daily filtration carbon')
        call assert_comparable(n_filtr,     0.8060969E-08,   tolerance, 'Validate Daily filtration nitrogen')
        call assert_comparable(p_filtr,     0.5406333E-08,   tolerance, 'Validate Daily filtration phosphorus')
        call assert_comparable(si_filtr,    0.6569826E-08,   tolerance, 'Validate Daily filtration silicon')
        call assert_comparable(food_pelagic,         0.25,   tolerance, 'Validate Food for pelagic organisms')
        call assert_comparable(food_benthic,         0.05,   tolerance, 'Validate Food for benthic organisms')

    end subroutine test_debgrz_calculate_uptake

    subroutine test_debgrz_get_maximum_conversion_coeff()
        real(kind=real_wp) :: conv_cm3_gc_1, conv_cm3_gc_2
        real(kind=real_wp) :: conv_j_gc
        real(kind=real_wp) :: eg_l3
        real(kind=real_wp) :: max_coeff_1, max_coeff_2

        !Arrange
        conv_cm3_gc_1 = 1.0
        conv_cm3_gc_2 = 10.0
        conv_j_gc = 2.0
        eg_l3 = 3.0

        !Act
        max_coeff_1 = get_maximum_conversion_coeff(conv_cm3_gc_1, conv_j_gc, eg_l3)
        max_coeff_2 = get_maximum_conversion_coeff(conv_cm3_gc_2, conv_j_gc, eg_l3)

        !Assert
        call assert_comparable(max_coeff_1, 1.0, tolerance, 'Validate maximum conversion coeff: value unchanged.')
        call assert_comparable(max_coeff_2, 6.0, tolerance, 'Validate maximum conversion coeff: value changed.')
    end subroutine test_debgrz_get_maximum_conversion_coeff

    subroutine test_debgrz_calculate_rescale_factors()
        integer(kind=int_wp) :: switchv1_0, switchv1_1       !< Use ISO-morphs (0) or V1-morphs (1)            [-]
        integer(kind=int_wp) :: benths_0, benths_1           !< Switch to use benthic or pelagic for DEB species
                                                             !< 0 = do not use benthic = use pelagic;
                                                             !< 1 = use benthic
        real(kind=real_wp)   :: shape                        !< Shape coefficient                              [-]
        real(kind=real_wp)   :: length_ini                   !< Initial Individual Length                 [gWW/m2]
        real(kind=real_wp)   :: vtot                         !< Structural biomass grazer pop.    [gC/m3 or gC/m2]
        real(kind=real_wp)   :: conv_cm3_gc1, conv_cm3_gc2   !< Conversion factor from cm3 into gC        [gC/cm3]
        real(kind=real_wp)   :: dens_ini1, dens_ini2         !< Initial number of grazer individuals           [-]
        real(kind=real_wp)   :: depth                        !< Depth of segment                               [m]
        real(kind=real_wp)   :: vd1, vd2                     !< Reference volume                             [cm3]
        real(kind=real_wp)   :: dens1, dens2, dens3, dens4   !< Density derived from Vtot(unit dep on BENTHS)
        real(kind=real_wp)   :: depth_factor1, depth_factor2 !< Factor to rescale depth


        !Arrange
        switchv1_0 = 0
        switchv1_1 = 1
        benths_0   = 0
        benths_1   = 1
        depth = 2.0
        shape = 3.0
        length_ini = 2.0
        vd1 = 123.456
        vd2 = 654.321
        dens_ini1 = 5.4321
        dens_ini2 = -5.4321
        vtot = 6048.0
        conv_cm3_gc1 = 2.0
        conv_cm3_gc2 = -2.0
        dens1 = 2
        dens2 = 2
        dens3 = 2
        dens4 = 2

        !Act
        call calculate_rescale_factors(switchv1_0, benths_0, shape, length_ini, vtot, conv_cm3_gc1, &
                                       dens_ini1, depth, vd1, dens1, depth_factor1)
        call calculate_rescale_factors(switchv1_1, benths_1, shape, length_ini, vtot, conv_cm3_gc1, &
                                       dens_ini1, depth, vd2, dens2, depth_factor2)
        call calculate_rescale_factors(switchv1_0, benths_0, shape, length_ini, vtot, conv_cm3_gc1, &
                                       dens_ini2, depth, vd1, dens3, depth_factor1)
        call calculate_rescale_factors(switchv1_1, benths_1, shape, length_ini, vtot, conv_cm3_gc2, &
                                       dens_ini1, depth, vd2, dens4, depth_factor2)

        !Assert
        call assert_comparable(depth_factor1, 2.0, tolerance, 'Validate depth_factor == depth when benths == 0.')
        call assert_comparable(depth_factor2, 1.0, tolerance, 'Validate depth_factor == 1.0 when benths /= 0.')
        call assert_comparable(vd1, 123.456, tolerance, 'Validate vd == unmodified when switchv1 /= 1.')
        call assert_comparable(vd2, 216.0, tolerance, 'Validate vd == (shape*length_ini)**3 when switchv1 /= 0.')
        call assert_comparable(dens1, dens_ini1, tolerance, 'Validate dens == dens_ini when switchv1/=1 and dens_ini > tiny')
        call assert_comparable(dens2, (vtot/conv_cm3_gc1)/vd2, tolerance, 'Validate dens == (vtot/conv_cm3_gc)/vd when switchv1==1 and (vtot/conv_cm3_gc)/vd > tiny')
        call assert_comparable(dens3, tiny(dens3), tolerance, 'Validate dens == tiny when switchv1==1 and (vtot/conv_cm3_gc)/vd < tiny')
        call assert_comparable(dens4, tiny(dens4), tolerance, 'Validate dens == tiny when switchv1/=1 and dens_ini < tiny')

    end subroutine test_debgrz_calculate_rescale_factors

    subroutine test_debgrz_rescale_non_food_vars()
        integer(kind=int_wp) :: switchv1_0, switchv1_1 !<

        real(kind=real_wp)   :: depth_factor !<
        real(kind=real_wp)   :: dens1        !< Density derived from Vtot(unit dep on BENTHS)
        real(kind=real_wp)   :: conv_cm3_gc  !< Conversion factor from cm3 into gC        [gC/cm3]
        real(kind=real_wp)   :: conv_j_gc    !< Conversion factor from energy into mass     [gC/J]
        real(kind=real_wp)   :: shape        !< Shape coefficient                              [-]
        real(kind=real_wp)   :: em_l3        !< Maximum storage density of DEB species     [J/cm3]
        real(kind=real_wp)   :: vd           !< Reference volume                             [cm3]
        real(kind=real_wp)   :: vtot         !< Structural biomass grazer pop.    [gC/m3 or gC/m2]
        real(kind=real_wp)   :: etot1        !< Structural biomass grazer pop.    [gC/m3 or gC/m2]
        real(kind=real_wp)   :: rtot1        !< Reproductional storage grazer pop.[gC/m3 or gC/m2]
        real(kind=real_wp)   :: dens_m2      !< Density derived from Vtot per m2
        real(kind=real_wp)   :: v_m2         !< Population structural biomass             [gC/m2]
        real(kind=real_wp)   :: e_m2         !< Population energy biomass                 [gC/m2]
        real(kind=real_wp)   :: r_m2         !< Population gonadal (reproductive) biomass [gC/m2]
        real(kind=real_wp)   :: v1, v3       !< Individual volume    [cm3/ind]
        real(kind=real_wp)   :: e1           !< Individual energy      [J/ind]
        real(kind=real_wp)   :: r1           !< Individual gonads      [J/ind]
        real(kind=real_wp)   :: length       !< Individual Length         [cm]
        real(kind=real_wp)   :: e_scaled     !< Scaled energy density      [-]

        ! Arrange
        switchv1_0 = 0
        switchv1_1 = 1
        depth_factor = 10
        dens1 = 5
        conv_cm3_gc = 2
        conv_j_gc = 4
        shape = 10
        em_l3 = 0.25
        vd = 0.123
        vtot = 100.0
        etot1 = 200.0
        rtot1 = 300.0

        !Act
        call rescale_non_food_vars(switchv1_0, depth_factor, dens1, conv_cm3_gc, conv_j_gc, shape, em_l3, &
                                   vd, vtot, etot1, rtot1, dens_m2, v_m2, e_m2, r_m2, v1, e1, r1, length, e_scaled)
        call rescale_non_food_vars(switchv1_1, depth_factor, dens1, conv_cm3_gc, conv_j_gc, shape, em_l3, &
                                   vd, vtot, etot1, rtot1, dens_m2, v_m2, e_m2, r_m2, v3, e1, r1, length, e_scaled)

        !Assert
        call assert_comparable(v_m2,  1000.0, tolerance, 'Validate v_m2')
        call assert_comparable(e_m2,  2000.0, tolerance, 'Validate e_m2')
        call assert_comparable(r_m2,  3000.0, tolerance, 'Validate r_m2')
        call assert_comparable(dens_m2, 50.0, tolerance, 'Validate dens_m2')

        call assert_comparable(v1,      10.0, tolerance, 'Validate v')
        call assert_comparable(e1,      10.0, tolerance, 'Validate e')
        call assert_comparable(r1,      15.0, tolerance, 'Validate r')
        call assert_comparable(v3,     0.123, tolerance, 'Validate v if switchv1==1')
        call assert_comparable(length,   0.049731898, tolerance, 'Validate length')
        call assert_comparable(e_scaled, 325.203252, tolerance, 'Validate e_scaled')



    end subroutine test_debgrz_rescale_non_food_vars

    subroutine test_debgrz_rescale_food_arrays()
        real(kind=real_wp)   :: area       !< Area of the cell
        integer(kind=int_wp) :: food_count !< Number of food types
        integer(kind=int_wp) :: benfood(3) !< Benthic foods (true/false)
        real(kind=real_wp)   :: cfood(3)   !< Carbon foods

        ! Arrange
        area = 2
        food_count = 3
        benfood = [0, 1, 1]
        cfood = [10, 30, -30]

        !Act
        call rescale_food_arrays(area, food_count, benfood, cfood)

        !Assert
        call assert_comparable(cfood(1), 10.0, tolerance, 'Validate cfood no scaling when benfood==0')
        call assert_comparable(cfood(2), 15.0, tolerance, 'Validate cfood scaling when benfood==1')
        call assert_comparable(cfood(3),  0.0, tolerance, 'Validate cfood >0')

    end subroutine test_debgrz_rescale_food_arrays

    subroutine test_debgrz_calculate_defaecation()
        real(kind=real_wp) :: kappa_i                !< Ingestion efficiency (pseudofaeces production)  [-]
        real(kind=real_wp) :: kappa_a                !< Assimilation efficiency                         [-]
        real(kind=real_wp) :: tn                     !< N:C ratio grazers                           [gN/gC]
        real(kind=real_wp) :: tp                     !< P:C ratio grazers                           [gP/gC]
        real(kind=real_wp) :: conv_j_gc              !< Conversion factor from energy into mass      [gC/J]
        real(kind=real_wp) :: c_filtr1, c_filtr2     !< Daily filtration rate for carbon         [gC/ind/d]
        real(kind=real_wp) :: n_filtr                !< Daily filtration rate for nitrogen       [gN/ind/d]
        real(kind=real_wp) :: p_filtr                !< Daily filtration rate for phosphorus     [gP/ind/d]
        real(kind=real_wp) :: si_filtr               !< Daily filtration rate for silicon       [gSi/ind/d]
        real(kind=real_wp) :: faecal_fraction        !< Faecal fraction
        real(kind=real_wp) :: c_defaec1, c_defaec2   !< Daily defaecation rate for carbon        [gC/ind/d]
        real(kind=real_wp) :: n_defaec1, n_defaec2   !< Daily defaecation rate for nitrogen      [gN/ind/d]
        real(kind=real_wp) :: p_defaec1, p_defaec2   !< Daily defaecation rate for phosphorus    [gP/ind/d]
        real(kind=real_wp) :: si_defaec1, si_defaec2 !< Si loss by defaecation in carbon equivalents [gC/d]
        real(kind=real_wp) :: pa1, pa2               !< Energy reserves                           [J/ind/d]

        ! Arrange
        kappa_i = 0.4
        kappa_a = 0.8
        tn = 2.0
        tp = 2.5
        conv_j_gc = 0.1
        c_filtr1 = 20.0
        c_filtr2 = -20.0
        n_filtr = 2.0
        p_filtr = 1.0
        si_filtr = 12.0
        faecal_fraction = 0.5

        ! Act
        call calculate_defaecation(kappa_i, kappa_a, tn, tp, conv_j_gc, c_filtr1, n_filtr, p_filtr, si_filtr, &
                                faecal_fraction, c_defaec1, n_defaec1, p_defaec1, si_defaec1, pa1)
        call calculate_defaecation(kappa_i, kappa_a, tn, tp, conv_j_gc, c_filtr2, n_filtr, p_filtr, si_filtr, &
                                faecal_fraction, c_defaec2, n_defaec2, p_defaec2, si_defaec2, pa2)

        ! Assert
        call assert_comparable(c_defaec1,  19.872, tolerance, 'Validate c_defaec if c_filtr > 0')
        call assert_comparable(n_defaec1,   1.744, tolerance, 'Validate n_defaec if c_filtr > 0')
        call assert_comparable(p_defaec1,   0.680, tolerance, 'Validate p_defaec if c_filtr > 0')
        call assert_comparable(si_defaec1, 12.000, tolerance, 'Validate si_defaec if c_filtr >0')
        call assert_comparable(pa1,         1.280, tolerance, 'Validate pa if c_filtr >0')

        call assert_comparable(c_defaec2,  0.0, tolerance, 'Validate c_defaec if c_filtr < 0')
        call assert_comparable(n_defaec2,  0.0, tolerance, 'Validate n_defaec if c_filtr < 0')
        call assert_comparable(p_defaec2,  0.0, tolerance, 'Validate p_defaec if c_filtr < 0')
        call assert_comparable(si_defaec2, 0.0, tolerance, 'Validate si_defaec if c_filtr < 0')
        call assert_comparable(pa2,        0.0, tolerance, 'Validate pa if c_filtr < 0')

    end subroutine test_debgrz_calculate_defaecation

    subroutine test_debgrz_calculate_energy_reserve_dynamics()
        real(kind=real_wp) :: jxm_l2        !< Max ingestion rate of DEB species           [J/cm2/d]
        real(kind=real_wp) :: kappa_a       !< Assimilation efficiency                           [-]
        real(kind=real_wp) :: eg_l3         !< Volume-spec costs for growth of DEB species   [J/cm3]
        real(kind=real_wp) :: em_l3         !< Maximum storage density of DEB species        [J/cm3]
        real(kind=real_wp) :: pm_l3         !< Respiration rate constant of DEB species        [J/d]
        real(kind=real_wp) :: kappa         !< Fraction of util.energy spent on maint&growth     [-]
        real(kind=real_wp) :: delt1, delt2  !< Timestep for processes                            [d]
        real(kind=real_wp) :: kT            !< Temperature_dependent_rate
        real(kind=real_wp) :: v             !< Individual volume                           [cm3/ind]
        real(kind=real_wp) :: e1, e2        !< Individual energy                             [J/ind]
        real(kind=real_wp) :: pc1, pc2, pc3 !< Energy reserve dynamics per individual      [J/ind/d]

        ! Arrange
        jxm_l2  = 1.0
        kappa_a = 2.0
        eg_l3   = 12.0
        em_l3   = 4.0
        pm_l3   = 0.123
        kappa   = 0.5
        delt1   = 0.1
        delt2   = 100000
        kT      = 0.1
        v       = 0.027
        e1      = 2.0
        e2      = -2.0

        ! Act
        call calculate_energy_reserve_dynamics(jxm_l2, kappa_a, eg_l3, em_l3, pm_l3, kappa, delt1, kT, v, e1, pc1)
        call calculate_energy_reserve_dynamics(jxm_l2, kappa_a, eg_l3, em_l3, pm_l3, kappa, delt2, kT, v, e1, pc2)
        call calculate_energy_reserve_dynamics(jxm_l2, kappa_a, eg_l3, em_l3, pm_l3, kappa, delt1, kT, v, e2, pc3)

        ! Assert
        call assert_comparable(pc1, 8.20727E-02, tolerance, 'Validate Energy reserve dynamics per individual normal formula')
        call assert_comparable(pc2,      2.0e-5, tolerance, 'Validate Energy reserve dynamics per individual at most e/delt')
        call assert_comparable(pc3,         0.0, tolerance, 'Validate Energy reserve dynamics per individual >= 0 ')
    end subroutine test_debgrz_calculate_energy_reserve_dynamics

    subroutine test_debgrz_calculate_maintenance()
        real(kind=real_wp) :: pm_l3  !< Respiration rate constant of DEB species [J/d]
        real(kind=real_wp) :: v      !< Individual volume                    [cm3/ind]
        real(kind=real_wp) :: kt     !< Temperature-dependent rate
        real(kind=real_wp) :: pm     !< Maintenance per individual           [J/ind/d]

        ! Arrange
        pm_l3 = 2
        v     = 3
        kt    = 5

        ! Act
        call calculate_maintenance(pm_l3, v, kt, pm)

        ! Assert
        call assert_comparable(pm, 30.0, tolerance, 'Validate Maintenance per individual.')

    end subroutine test_debgrz_calculate_maintenance

    subroutine test_debgrz_calculate_growth()
        real(kind=real_wp) :: kappa         !< Fraction of util.energy spent on maint&growth  [-]
        real(kind=real_wp) :: pc            !< Energy reserve dynamics per individual   [J/ind/d]
        real(kind=real_wp) :: conv_cm3_gc   !< Conversion factor from cm3 into gC        [gC/cm3]
        real(kind=real_wp) :: conv_j_gc     !< Conversion factor from energy into mass     [gC/J]
        real(kind=real_wp) :: eg_l3         !< Volume-spec costs for growth of DEB species[J/cm3]
        real(kind=real_wp) :: delt1, delt2  !< Timestep for processes                         [d]
        real(kind=real_wp) :: v             !< Individual volume                        [cm3/ind]
        real(kind=real_wp) :: pm1, pm2, pm3 !< Maintenance per individual               [J/ind/d]
        real(kind=real_wp) :: pv1, pv2, pv3 !< Overhead costs per volume                [J/ind/d]
        real(kind=real_wp) :: pg1, pg2, pg3 !< Energy costs for growth                  [J/ind/d]
        real(kind=real_wp) :: kappa_g       !< Overhead costs for growth                      [-]

        ! Arrange
        kappa       = 0.5
        pc          = 10.0
        conv_cm3_gc = 0.12
        conv_j_gc   = 0.3
        eg_l3       = 2.0
        delt1       = 0.1
        delt2       = 0.01
        v           = 0.25
        pm1         = 0.123
        pm2         = 10.0
        pm3         = 10.0

        ! Act
        call calculate_growth(kappa, pc, conv_cm3_gc, conv_j_gc, eg_l3, delt1, v, pm1, pv1, pg1, kappa_g)
        call calculate_growth(kappa, pc, conv_cm3_gc, conv_j_gc, eg_l3, delt2, v, pm2, pv2, pg2, kappa_g)
        call calculate_growth(kappa, pc, conv_cm3_gc, conv_j_gc, eg_l3, delt1, v, pm3, pv3, pg3, kappa_g)

        ! Assert
        call assert_comparable(pm1, 0.1230, tolerance, 'Validate pm: Maintenance per individual, pg > 0')
        call assert_comparable(pv1, 0.9754, tolerance, 'Validate pv: Overhead costs per volume, pg > 0')
        call assert_comparable(pg1, 4.8770, tolerance, 'Validate pg: Energy costs for growth, pg > 0')
        call assert_comparable(kappa_g, 0.2, tolerance, 'Validate kappa_g: Overhead costs for growth.')

        call assert_comparable(pm2,   14.0, tolerance, 'Validate pm: Maintenance per individual, pg < 0, no max adjustment')
        call assert_comparable(pv2,   -9.0, tolerance, 'Validate pv: Overhead costs per volume, pg < 0, no max adjustment')
        call assert_comparable(pg2,   -5.0, tolerance, 'Validate pg: Energy costs for growth, pg < 0, no min adjustment')

        call assert_comparable(pm3,   6.0000, tolerance, 'Validate pm: Maintenance per individual, pg > 0, max adjustment')
        call assert_comparable(pv3,   -1.000, tolerance, 'Validate pv: Overhead costs per volume, pg > 0, max adjustment')
        call assert_comparable(pg3, -0.55556, tolerance, 'Validate pg: Energy costs for growth, pg > 0, min adjustment')

    end subroutine test_debgrz_calculate_growth

    subroutine test_debgrz_get_maturity_fractions()
        integer(kind=int_wp) :: switchv1_0, switchv1_1    !< Use ISO-morphs (0) or V1-morphs (1)         [-]

        real(kind=real_wp)   :: v1, v2                    !< Individual volume                     [cm3/ind]
        real(kind=real_wp)   :: vp                        !< Volume at start of reproductive stage [cm3/ind]
        real(kind=real_wp)   :: fjuv1, fjuv2, fjuv3       !< Juvenile fraction of the population         [-]
        real(kind=real_wp)   :: fadult1, fadult2, fadult3 !< Adult fraction of the population            [-]

        ! Arrange
        switchv1_0 = 0
        switchv1_1 = 1
        v1 = 20.0
        v2 = 90.0
        vp = 80.0

        ! Act
        call get_maturity_fractions(switchv1_1, v1, vp, fjuv1, fadult1)
        call get_maturity_fractions(switchv1_0, v1, vp, fjuv2, fadult2)
        call get_maturity_fractions(switchv1_0, v2, vp, fjuv3, fadult3)

        ! Assert
        call assert_comparable(fjuv1,   0.8, tolerance, 'Validate juvenile fraction: switchv1 == 0')
        call assert_comparable(fadult1, 0.2, tolerance, 'Validate adult fraction: switchv1 == 0')
        call assert_comparable(fjuv2,   1.0, tolerance, 'Validate juvenile fraction: switchv1 == 0, v < vp')
        call assert_comparable(fadult2, 0.0, tolerance, 'Validate adult fraction: switchv1 == 0, v < vp')
        call assert_comparable(fjuv3,   0.0, tolerance, 'Validate juvenile fraction: switchv1 == 0, v >= vp')
        call assert_comparable(fadult3, 1.0, tolerance, 'Validate adult fraction: switchv1 == 0, v >= vp')

    end subroutine test_debgrz_get_maturity_fractions

    subroutine test_debgrz_get_gsi()
        integer(kind=int_wp) :: switchv1_0, switchv1_1 !< Use ISO-morphs (0) or V1-morphs (1)            [-]

        real(kind=real_wp)   :: vp1, vp2               !< Volume at start of reproductive stage [cm3/ind]
        real(kind=real_wp)   :: v                      !< Individual volume                     [cm3/ind]
        real(kind=real_wp)   :: e                      !< Individual energy                       [J/ind]
        real(kind=real_wp)   :: r                      !< Individual gonads                       [J/ind]
        real(kind=real_wp)   :: fadult                 !< Adult fraction of the population            [-]
        real(kind=real_wp)   :: conv_j_gc              !< Conversion factor from energy into mass  [gC/J]
        real(kind=real_wp)   :: conv_cm3_gc            !< Conversion factor from cm3 into gC     [gC/cm3]
        real(kind=real_wp)   :: gsi1, gsi2, gsi3, gsi4 !< Gonadosomatic Index                         [-]

        ! Arrange
        switchv1_0 = 0
        switchv1_1 = 1
        vp1 = 20.0
        vp2 = 10.0
        v = 15.0
        e = 5.0
        r = 12.0
        fadult = 0.75
        conv_j_gc = 0.123e-3
        conv_cm3_gc = 0.456e-3

        ! Act
        gsi1 = get_gsi(switchv1_0, vp1, v, e, r, fadult, conv_j_gc, conv_cm3_gc)
        gsi2 = get_gsi(switchv1_0, vp2, v, e, r, fadult, conv_j_gc, conv_cm3_gc)
        gsi3 = get_gsi(switchv1_1, vp1, v, e, r, fadult, conv_j_gc, conv_cm3_gc)
        gsi4 = get_gsi(switchv1_1, vp2, v, e, r, fadult, conv_j_gc, conv_cm3_gc)

        ! Assert
        call assert_comparable(gsi1,       0.0, tolerance, 'Validate Gonadosomatic Index gsi: switchv1==0, vp<v')
        call assert_comparable(gsi2, 0.2088507, tolerance, 'Validate Gonadosomatic Index gsi: switchv1==0, vp>v')
        call assert_comparable(gsi3, 0.2088507, tolerance, 'Validate Gonadosomatic Index gsi: switchv1==1, vp<v')
        call assert_comparable(gsi4, 0.2088507, tolerance, 'Validate Gonadosomatic Index gsi: switchv1==1, vp>v')
    end subroutine test_debgrz_get_gsi

    subroutine test_debgrz_calculate_maturity_and_reproduction_energies()
        real(kind=real_wp) :: vp          !< Volume at start of reproductive stage        [cm3]
        real(kind=real_wp) :: kappa       !< Fraction of util.energy spent on maint&growth  [-]
        real(kind=real_wp) :: pm_l3       !< Respiration rate constant of DEB species     [J/d]
        real(kind=real_wp) :: kappar      !< Fraction of repro.energy spent on              [-]
        real(kind=real_wp) :: delt        !< Timestep for processes                         [d]
        real(kind=real_wp) :: v           !< Individual volume                     [cm3/ind]
        real(kind=real_wp) :: r           !< Individual gonads                          [J/ind]
        real(kind=real_wp) :: pc1, pc2    !< Energy reserve dynamics per individual   [J/ind/d]
        real(kind=real_wp) :: kT          !< Temperature_dependent_rate
        real(kind=real_wp) :: fadult      !< Adult fraction of the population               [-]
        real(kind=real_wp) :: fjuv        !< Juvenile fraction of the population            [-]
        real(kind=real_wp) :: pjj1, pjj2  !< Maturity maintenance juveniles           [J/ind/d]
        real(kind=real_wp) :: prj1, prj2  !< Maturity development                     [J/ind/d]
        real(kind=real_wp) :: pja1, pja2  !< Maturity maintenance adults              [J/ind/d]
        real(kind=real_wp) :: pra1, pra2  !< Specific energy for reproduction         [J/ind/d]
        real(kind=real_wp) :: pr1, pr2    !< Energy for reproduction                  [J/ind/d]

        ! Arrange
        vp = 10
        kappa = 0.8
        pm_l3 = 0.123e-3
        kappar = 0.6
        delt = 0.01
        v = 15.0
        r  = 12.0
        pc1 = 0.321
        pc2 = 0.321e-3
        kT = 0.456
        fadult = 0.25
        fjuv = 0.75
        ! Act
        call calculate_maturity_and_reproduction_energies(vp, kappa, pm_l3, kappar, delt, v, r, pc1, kT, fadult, fjuv, &
                                                            pjj1, prj1, pja1, pra1, pr1)
        call calculate_maturity_and_reproduction_energies(vp, kappa, pm_l3, kappar, delt, v, r, pc2, kT, fadult, fjuv, &
                                                            pjj2, prj2, pja2, pra2, pr2)

        ! Assert
        call assert_comparable(pjj1, 0.1577475E-03, tolerance, 'Validate Maturity maintenance juveniles pjj: pra > 0')
        call assert_comparable(prj1, 0.4799225E-01, tolerance, 'Validate Maturity development prj: pra > 0')
        call assert_comparable(pja1, 0.3505500E-04, tolerance, 'Validate Maturity maintenance adults pja: pra > 0')
        call assert_comparable(pra1, 0.1601494E-01, tolerance, 'Validate Specific energy for reproduction pra: pra > 0')
        call assert_comparable(pr1,  0.9608967E-02, tolerance, 'Validate Energy for reproduction pr:  pra > 0')

        call assert_comparable(pjj2,  0.4815000E-04, tolerance, 'Validate Maturity maintenance juveniles pjj: pra <= 0')
        call assert_comparable(prj2,            0.0, tolerance, 'Validate Maturity development prj: pra <= 0')!!!
        call assert_comparable(pja2,  0.4265700E-04, tolerance, 'Validate Maturity maintenance adults pja: pra <= 0')
        call assert_comparable(pra2, -0.1900500E-04, tolerance, 'Validate Specific energy for reproduction pra: pra <= 0')
        call assert_comparable(pr2,  -0.2660700E-04, tolerance, 'Validate Energy for reproduction pr:  pra <= 0')

    end subroutine test_debgrz_calculate_maturity_and_reproduction_energies

    subroutine test_debgrz_calculate_spawning()
        real(kind=real_wp) :: gsi                    !< Gonadosomatic Index                            [-]
        real(kind=real_wp) :: gsi_upper1, gsi_upper2 !< Minimum GSI for spawning                       [-]
        real(kind=real_wp) :: gsi_lower1, gsi_lower2 !< Minimum GSI while spawning                     [-]
        real(kind=real_wp) :: temp                   !< Ambient water temperature                     [oC]
        real(kind=real_wp) :: minsptemp1, minsptemp2 !< Minimum temperature for spawning              [oC]
        real(kind=real_wp) :: r                      !< Individual gonads                          [J/ind]
        real(kind=real_wp) :: rspawn                 !< Spawning rate                                  [-]
        real(kind=real_wp) :: delt                   !< Timestep for processes                         [d]
        real(kind=real_wp) :: tn                     !< N:C ratio grazers                          [gN/gC]
        real(kind=real_wp) :: tp                     !< P:C ratio grazers                          [gP/gC]
        real(kind=real_wp) :: pr                     !< Energy for reproduction                  [J/ind/d]
        real(kind=real_wp) :: dospawn(3)             !< Indication of spawning                         [-]
        real(kind=real_wp) :: dspw(3)                !< Delta energy for carbon for spawning     [J/ind/d]
        real(kind=real_wp) :: dnspw(3)               !< Delta energy for nitrogen for spawning   [J/ind/d]
        real(kind=real_wp) :: dpspw(3)               !< Delta energy for phosphorus for spawning [J/ind/d]

        ! Arrange
        gsi = 15.0
        gsi_upper1 = 10.0
        gsi_upper2 = 20.0
        gsi_lower1 = 5.0
        gsi_lower2 = 17.0
        temp = 12.0
        minsptemp1 = 10.0
        minsptemp2 = 15.0
        r = 0.123
        rspawn = 0.456
        delt = 0.01
        tn = 0.3
        tp = 0.4
        pr = 0.321
        dospawn = [0.0, 0.0, 1.0]

        ! Act
        call calculate_spawning(gsi, gsi_lower1, gsi_upper1, temp, minsptemp1, r, rspawn, delt, tn, tp, &
                                pr, dospawn(1), dspw(1), dnspw(1), dpspw(1))
        call calculate_spawning(gsi, gsi_lower1, gsi_upper2, temp, minsptemp1, r, rspawn, delt, tn, tp, &
                                pr, dospawn(2), dspw(2), dnspw(2), dpspw(2))
        call calculate_spawning(gsi, gsi_lower2, gsi_upper1, temp, minsptemp1, r, rspawn, delt, tn, tp, &
                                pr, dospawn(3), dspw(3), dnspw(3), dpspw(3))

        ! Assert
        call assert_comparable(dospawn(1),       1.0, tolerance, 'Validate Indication of spawning dospawn: dospawn changed to 1')
        call assert_comparable(dspw(1),    0.3770880, tolerance, 'Validate Delta energy for carbon for spawning: dospawn changed to 1')
        call assert_comparable(dnspw(1),   0.1131264, tolerance, 'Validate Delta energy for nitrogen for spawning: dospawn changed to 1')
        call assert_comparable(dpspw(1),   0.1508352, tolerance, 'Validate Delta energy for phosphorus for spawning: dospawn changed to 1')

        call assert_comparable(dospawn(2),       0.0, tolerance, 'Validate Indication of spawning dospawn: dospawn unchanged at 1, gsi < gsi_lower')
        call assert_comparable(dspw(2),          0.0, tolerance, 'Validate Delta energy for carbon for spawning: dospawn unchanged at 1, gsi < gsi_lower')
        call assert_comparable(dnspw(2),         0.0, tolerance, 'Validate Delta energy for nitrogen for spawning: dospawn unchanged at 1, gsi < gsi_lower')
        call assert_comparable(dpspw(2),         0.0, tolerance, 'Validate Delta energy for phosphorus for spawning: dospawn unchanged at 1, gsi < gsi_lower')

        call assert_comparable(dospawn(3),       0.0, tolerance, 'Validate Indication of spawning dospawn: dospawn changed to 0')
        call assert_comparable(dspw(3),          0.0, tolerance, 'Validate Delta energy for carbon for spawning: dospawn changed to 0')
        call assert_comparable(dnspw(3),         0.0, tolerance, 'Validate Delta energy for nitrogen for spawning: dospawn changed to 0')
        call assert_comparable(dpspw(3),         0.0, tolerance, 'Validate Delta energy for phosphorus for spawning: dospawn changed to 0')

    end subroutine test_debgrz_calculate_spawning

    subroutine test_debgrz_calculate_respiration()
        real(kind=real_wp) :: pm             !< Maintenance per individual             [J/ind/d]
        real(kind=real_wp) :: pja            !< Maturity maintenance adults            [J/ind/d]
        real(kind=real_wp) :: pjj            !< Maturity maintenance juveniles         [J/ind/d]
        real(kind=real_wp) :: prj            !< Maturity development                   [J/ind/d]
        real(kind=real_wp) :: kappa_g        !< Overhead costs for growth                    [-]
        real(kind=real_wp) :: pg1, pg2       !< Energy costs for growth                [J/ind/d]
        real(kind=real_wp) :: pra1, pra2     !< Specific energy for reproduction       [J/ind/d]
        real(kind=real_wp) :: kappar         !< Fraction of repro.energy spent on            [-]
        real(kind=real_wp) :: tn             !< N:C ratio grazers                        [gN/gC]
        real(kind=real_wp) :: tp             !< P:C ratio grazers                        [gP/gC]
        real(kind=real_wp) :: dres1, dres2   !< Respiration per individual (carbon)
        real(kind=real_wp) :: dnres1, dnres2 !< Respiration per individual (nitrogen)
        real(kind=real_wp) :: dpres1, dpres2 !< Respiration per individual (phosphorus)
        real(kind=real_wp) :: ddis           !< Total maintenance cost (see calculate_shell_formation_fluxes)
        real(kind=real_wp) :: pomm           !< Energy flux ot organic shell matrix (ignored for now)

        ! Arrange
        pm      = 1.0
        pja     = 2.0
        pjj     = 4.0
        prj     = 8.0
        kappa_g = 0.5
        pg1     = 100.0
        pra1    = 1000.0
        pg2     = -100.0
        pra2    = -1000.0
        kappar  = 0.25
        tn      = 0.11
        tp      = 0.33

        ddis    = pm  + pja + pjj + prj
        pomm    = 0.0

        ! Act
        call calculate_respiration(ddis, pomm, kappa_g, pg1, pra1, kappar, tn, tp, dres1, dnres1, dpres1)
        call calculate_respiration(ddis, pomm, kappa_g, pg2, pra2, kappar, tn, tp, dres2, dnres2, dpres2)

        ! Assert
        call assert_comparable(dres1,  815.00, tolerance, 'Validate dres: Respiration per individual (carbon), pg > 0, pra > 0')
        call assert_comparable(dnres1,  89.65, tolerance, 'Validate dres: Respiration per individual (nitrogen), pg > 0, pra > 0')
        call assert_comparable(dpres1, 268.95, tolerance, 'Validate dres: Respiration per individual (phosphorus), pg > 0, pra > 0')

        call assert_comparable(dres2,   15.00, tolerance, 'Validate dres: Respiration per individual (carbon), pg < 0, pra < 0')
        call assert_comparable(dnres2,   1.65, tolerance, 'Validate dres: Respiration per individual (nitrogen), pg < 0, pra < 0')
        call assert_comparable(dpres2,   4.95, tolerance, 'Validate dres: Respiration per individual (phosphorus), pg < 0, pra < 0')
    end subroutine test_debgrz_calculate_respiration

    subroutine test_debgrz_calculate_shell_formation_fluxes()
        real(kind=real_wp) :: pv                      !< Overhead costs per volume                      [J/ind/d]
        real(kind=real_wp) :: fpgrosmo1, fpgrosmo2    !< Growth-based contribution to shell matrix            [-]
        real(kind=real_wp) :: fpdissmo1, fpdissmo2,   &
                              fpdissmo3               !< Dissipation-based contribution to shell matrix       [-]
        real(kind=real_wp) :: pomm1, pomm2, pomm3,    &
                              pomm4, pomm5            !< Energy flux to organic shell matrix            [J/ind/d]
        real(kind=real_wp) :: pca1, pca2, pca3,       &
                              pca4, pca5              !< Energy flux to calcification of shell matrix   [J/ind/d]
        real(kind=real_wp) :: ycacosmo1,              &
                              ycacosmo2, ycacosmo3    !< Yield coefficient CaCO3 deposition on matrix         [-]
        real(kind=real_wp) :: ddis1, ddis2, ddis3,    &
                              ddis4, ddis5            !< Dissipation flux (not the same as respiration!)[J/ind/d]
        real(kind=real_wp) :: pm, pja, pjj, prj       !< Maintenance costs                              [J/ind/d]

        ! Arrange - with DELWAQ-843 the meaning of frsmosmi changes
        pv        = 2.0
        fpgrosmo1 = 3.0
        ycacosmo1 = 1.0 / 5.0
        ycacosmo2 = 0.0
        fpgrosmo2 = -3.0
        ycacosmo3 = 0.0   ! Should not lead to division by zero!

        ! pm, etc. chosen so that we retain the old values - before DELWAQ-843
        pm        = 0.0
        pja       = 0.0
        pjj       = 0.0
        prj       = 0.0

        fpdissmo1 = 1.0
        fpdissmo2 = 1.0
        fpdissmo3 = 1.0

        ! Act
        call calculate_shell_formation_fluxes(pm, pja, pjj, prj, pv, fpgrosmo1, fpdissmo1, ycacosmo1, ddis1, pomm1, pca1)
        call calculate_shell_formation_fluxes(pm, pja, pjj, prj, pv, fpgrosmo1, fpdissmo2, ycacosmo2, ddis2, pomm2, pca2)
        call calculate_shell_formation_fluxes(pm, pja, pjj, prj, pv, fpgrosmo2, fpdissmo3, ycacosmo1, ddis3, pomm3, pca3)

        pm        = 1.0
        pja       = 1.0
        pjj       = 1.0
        prj       = 1.0

        call calculate_shell_formation_fluxes(pm, pja, pjj, prj, pv, fpgrosmo1, fpdissmo3, ycacosmo3, ddis4, pomm4, pca4)
        call calculate_shell_formation_fluxes(pm, pja, pjj, prj, pv, fpgrosmo1, fpdissmo3, ycacosmo1, ddis5, pomm5, pca5)


        ! Assert
        call assert_comparable(ddis1,  0.0, tolerance, 'Validate ddis: Energy flux to organic shell matrix, pomm > 0, pca > 0')
        call assert_comparable(pomm1,  6.0, tolerance, 'Validate pomm: Energy flux to organic shell matrix, pomm > 0, pca > 0')
        call assert_comparable(pca1,  30.0, tolerance, 'Validate pca:  Energy flux to calcification of shell matrix, pomm > 0, pca >0')

        call assert_comparable(ddis2,  0.0, tolerance, 'Validate ddis: Energy flux to organic shell matrix, pomm > 0, pca > 0')
        call assert_comparable(pomm2,  6.0, tolerance, 'Validate pomm: Energy flux to organic shell matrix, pomm > 0, pca == 0')
        call assert_comparable(pca2,   0.0, tolerance, 'Validate pca:  Energy flux to calcification of shell matrix, pomm > 0, pca == 0')

        call assert_comparable(ddis3,  0.0, tolerance, 'Validate ddis: Energy flux to organic shell matrix, pomm > 0, pca > 0')
        call assert_comparable(pomm3,  0.0, tolerance, 'Validate pomm: Energy flux to organic shell matrix, pomm == 0')
        call assert_comparable(pca3,   0.0, tolerance, 'Validate pca:  Energy flux to calcification of shell matrix, pomm == 0')

        call assert_comparable(ddis4,  4.0, tolerance, 'Validate ddis: Energy flux to organic shell matrix, diss > 0, pomm > 0, pca == 0')
        call assert_comparable(pomm4, 10.0, tolerance, 'Validate pomm: Energy flux to organic shell matrix, diss > 0, pomm > 0, pca == 0')
        call assert_comparable(pca4,   0.0, tolerance, 'Validate pca:  Energy flux to calcification of shell matrix, diss > 0, pomm > 0, pca == 0')

        call assert_comparable(ddis5,  4.0, tolerance, 'Validate ddis: Energy flux to organic shell matrix, diss > 0, pomm > 0, pca > 0')
        call assert_comparable(pomm5, 10.0, tolerance, 'Validate pomm: Energy flux to organic shell matrix, diss > 0, pomm > 0, pca > 0')
        call assert_comparable(pca5,  50.0, tolerance, 'Validate pca:  Energy flux to calcification of shell matrix, diss > 0, pomm > 0, pca > 0')

    end subroutine test_debgrz_calculate_shell_formation_fluxes

    subroutine test_debgrz_calculate_mortality()
        real(kind=real_wp) :: rmor_ref1, rmor_ref2                   !< Reference mortality rate grazers           [1/d]
        real(kind=real_wp) :: vtot                                   !< Structural biomass grazer pop.  [gC/m3 or gC/m2]
        real(kind=real_wp) :: ddmfk1, ddmfk2, ddmfk3                 !< Half concentration for density dependent 
                                                                     !< mortality factor                [gC/m3 or gC/m2]
        real(kind=real_wp) :: cmor                                   !< Length-dep coefficient mortality rate      [1/d]
        real(kind=real_wp) :: conv_j_gc                              !< Conversion factor from energy into mass   [gC/J]
        real(kind=real_wp) :: conv_cm3_gc                            !< Conversion factor from cm3 into gC      [gC/cm3]
        real(kind=real_wp) :: rhrv_ref1, rhrv_ref2                   !< Reference  harvesting rate grazers         [1/d]
        real(kind=real_wp) :: chrv                                   !< Length-dep coefficient harvesting rate     [1/d]
        real(kind=real_wp) :: tp                                     !< P:C ratio grazers                        [gP/gC]
        real(kind=real_wp) :: tn                                     !< N:C ratio grazers                        [gN/gC]
        real(kind=real_wp) :: length                                 !< Individual Length                           [cm]
        real(kind=real_wp) :: v                                      !< Individual volume                      [cm3/ind]
        real(kind=real_wp) :: e                                      !< Individual energy                        [J/ind]
        real(kind=real_wp) :: r                                      !< Individual gonads                        [J/ind]
        real(kind=real_wp) :: kt                                     !< Temperature_dependent_rate
        real(kind=real_wp) :: pv1, pv2                               !< Overhead costs per volume              [J/ind/d]
        real(kind=real_wp) :: rmor1, rmor2, rmor3, rmor4, rmor5      !< Mortality rate
        real(kind=real_wp) :: ddmf1, ddmf2, ddmf3, ddmf4, ddmf5      !< Density dependent mortality factor           [-]
        real(kind=real_wp) :: rhrv1, rhrv2, rhrv3, rhrv4, rhrv5      !< Overhead costs per volume              [J/ind/d]
        real(kind=real_wp) :: dmor1, dmor2, dmor3, dmor4, dmor5      !< Mortality difference for carbon        [gC/m3/d]
        real(kind=real_wp) :: dnmor1, dnmor2, dnmor3, dnmor4, dnmor5 !< Mortality difference for nitrogen      [gN/m3/d]
        real(kind=real_wp) :: dpmor1, dpmor2, dpmor3, dpmor4, dpmor5 !< Mortality difference for phosphorus    [gP/m3/d]

        ! Arrange
        rmor_ref1   = 0.55e-2
        rmor_ref2   = 0.55
        vtot        = 0.01
        ddmfk1      = 0.0
        ddmfk2      = 0.01
        ddmfk3      = 0.02
        cmor        = 0.5
        conv_j_gc   = 0.1
        conv_cm3_gc = 0.3
        rhrv_ref1   = 0.3e-2
        rhrv_ref2   = 0.3
        chrv        = 2
        tp          = 0.1
        tn          = 0.2
        length      = 3.0
        v           = 5
        e           = 7
        r           = 11
        kt          = 0.5
        pv1         = 10
        pv2         = -10

        ! Act
        call calculate_mortality(rmor_ref1, vtot, ddmfk1, cmor, conv_j_gc, conv_cm3_gc, rhrv_ref1, chrv, tn, tp, &
        length, v, e, r, rmor1, ddmf1, rhrv1, dmor1, dnmor1, dpmor1, kt, pv1)
        call calculate_mortality(rmor_ref1, vtot, ddmfk1, cmor, conv_j_gc, conv_cm3_gc, rhrv_ref1, chrv, tn, tp, &
        length, v, e, r, rmor2, ddmf2, rhrv2, dmor2, dnmor2, dpmor2, kt, pv2)
        call calculate_mortality(rmor_ref2, vtot, ddmfk1, cmor, conv_j_gc, conv_cm3_gc, rhrv_ref2, chrv, tn, tp, &
        length, v, e, r, rmor3, ddmf3, rhrv3, dmor3, dnmor3, dpmor3, kt, pv2)
        call calculate_mortality(rmor_ref2, vtot, ddmfk2, cmor, conv_j_gc, conv_cm3_gc, rhrv_ref2, chrv, tn, tp, &
        length, v, e, r, rmor4, ddmf4, rhrv4, dmor4, dnmor4, dpmor4, kt, pv2)
        call calculate_mortality(rmor_ref2, vtot, ddmfk3, cmor, conv_j_gc, conv_cm3_gc, rhrv_ref2, chrv, tn, tp, &
        length, v, e, r, rmor5, ddmf5, rhrv5, dmor5, dnmor5, dpmor5, kt, pv2)


        ! Assert
        call assert_comparable(rmor1,    0.4763139E-02, tolerance, 'Validate rmor: Mortality rate, pv > 0')
        call assert_comparable(ddmf1,              1.0, tolerance, 'Validate ddmf: Density dependent mortality factor, pv > 0')
        call assert_comparable(rhrv1,    0.2700000E-01, tolerance, 'Validate rhrv: Overhead costs per volume, pv > 0')
        call assert_comparable(dmor1,    0.1571836E-01, tolerance, 'Validate dmor: Mortality difference for carbon, pv > 0')
        call assert_comparable(dnmor1,   0.3143672E-02, tolerance, 'Validate dnmor: Mortality difference for nitrogen, pv > 0')
        call assert_comparable(dpmor1,   0.1571836E-02, tolerance, 'Validate dpmor: Mortality difference for phosphorus, pv > 0')

        call assert_comparable(rmor2,    0.4763139E-02, tolerance, 'Validate rmor: Mortality rate, pv < 0, rmor and rhrv not modified with min function')
        call assert_comparable(ddmf2,              1.0, tolerance, 'Validate ddmf: Density dependent mortality factor, pv < 0, rmor and rhrv not modified with min function')
        call assert_comparable(rhrv2,    0.2700000E-01, tolerance, 'Validate rhrv: Overhead costs per volume, pv < 0, rmor and rhrv not modified with min function')
        call assert_comparable(dmor2,    0.1571836E-01, tolerance, 'Validate dmor: Mortality difference for carbon, pv < 0, rmor and rhrv not modified with min function')
        call assert_comparable(dnmor2,   0.3143672E-02, tolerance, 'Validate dnmor: Mortality difference for nitrogen, pv < 0, rmor and rhrv not modified with min function')
        call assert_comparable(dpmor2,   0.1571836E-02, tolerance, 'Validate dpmor: Mortality difference for phosphorus, pv < 0, rmor and rhrv not modified with min function')

        call assert_comparable(rmor3,         0.333333, tolerance, 'Validate rmor: Mortality rate, pv < 0, rmor and rhrv modified with min function')
        call assert_comparable(ddmf3,              1.0, tolerance, 'Validate ddmf: Density dependent mortality factor, pv < 0, rmor and rhrv modified with min function')
        call assert_comparable(rhrv3,              0.0, tolerance, 'Validate rhrv: Overhead costs per volume, pv < 0, rmor and rhrv modified with min function')
        call assert_comparable(dmor3,              1.1, tolerance, 'Validate dmor: Mortality difference for carbon, pv < 0, rmor and rhrv modified with min function')
        call assert_comparable(dnmor3,            0.22, tolerance, 'Validate dnmor: Mortality difference for nitrogen, pv < 0, rmor and rhrv modified with min function')
        call assert_comparable(dpmor3,            0.11, tolerance, 'Validate dpmor: Mortality difference for phosphorus, pv < 0, rmor and rhrv modified with min function')

        call assert_comparable(rmor4,        0.2381570, tolerance, 'Validate rmor: Mortality rate, pv < 0, rmor and rhrv modified with min function')
        call assert_comparable(ddmf4,              0.5, tolerance, 'Validate ddmf: Density dependent mortality factor, pv < 0, rmor and rhrv modified with min function')
        call assert_comparable(rhrv4,    9.5176339E-02, tolerance, 'Validate rhrv: Overhead costs per volume, pv < 0, rmor and rhrv modified with min function')
        call assert_comparable(dmor4,        0.7859181, tolerance, 'Validate dmor: Mortality difference for carbon, pv < 0, rmor and rhrv modified with min function')
        call assert_comparable(dnmor4,       0.1571836, tolerance, 'Validate dnmor: Mortality difference for nitrogen, pv < 0, rmor and rhrv modified with min function')
        call assert_comparable(dpmor4,   7.8591816E-02, tolerance, 'Validate dpmor: Mortality difference for phosphorus, pv < 0, rmor and rhrv modified with min function')

        call assert_comparable(rmor5,        0.1587713, tolerance, 'Validate rmor: Mortality rate, pv < 0, rmor and rhrv modified with min function')
        call assert_comparable(ddmf5,        0.3333333, tolerance, 'Validate ddmf: Density dependent mortality factor, pv < 0, rmor and rhrv modified with min function')
        call assert_comparable(rhrv5,        0.1745620, tolerance, 'Validate rhrv: Overhead costs per volume, pv < 0, rmor and rhrv modified with min function')
        call assert_comparable(dmor5,        0.5239455, tolerance, 'Validate dmor: Mortality difference for carbon, pv < 0, rmor and rhrv modified with min function')
        call assert_comparable(dnmor5,       0.1047891, tolerance, 'Validate dnmor: Mortality difference for nitrogen, pv < 0, rmor and rhrv modified with min function')
        call assert_comparable(dpmor5,   5.2394547E-02, tolerance, 'Validate dpmor: Mortality difference for phosphorus, pv < 0, rmor and rhrv modified with min function')

    end subroutine test_debgrz_calculate_mortality
end program tests_debgrz_computations
