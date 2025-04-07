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

module m_debgrz_computations
    use m_waq_precision
    use m_logger_helper

    implicit none

    private
    public :: get_maximum_conversion_coeff, &
              calculate_rescale_factors, &
              rescale_non_food_vars, &
              rescale_food_arrays, &
              rescale_units, &
              temperature_dependent_rate, &
              calculate_uptake, &
              calculate_defaecation,  &
              calculate_energy_reserve_dynamics, &
              calculate_maintenance, &
              calculate_growth, &
              calculate_maturity_and_reproduction, &
              get_maturity_fractions, &
              get_gsi, &
              calculate_maturity_and_reproduction_energies, &
              calculate_spawning, &
              calculate_respiration,  &
              calculate_shell_formation_fluxes, &
              calculate_mortality

    contains

    !> Costs for growth set a maximum to the energy content of structural material and thus to the conversion
    !! coefficient conv_cm3_gC.
    !! This may affect the overhead costs for growth (kappa_G)
    real(kind=real_wp) function get_maximum_conversion_coeff(conv_cm3_gc, conv_j_gc, eg_l3)
        real(kind=real_wp), intent(in) :: conv_cm3_gc !< Conversion factor from cm3 into gC         [gC/cm3]
        real(kind=real_wp), intent(in) :: conv_j_gc   !< Conversion factor from energy into mass      [gC/J]
        real(kind=real_wp), intent(in) :: eg_l3       !< Volume-spec costs for growth of DEB species [J/cm3]

        integer(kind=int_wp) :: lunrep                   !< Logical unit number for logging message
        logical, save :: conv_adjustment_logged = .true. !< Indicates if the adjustment of conv_cm3_gC to conv_J_gC * Eg_L3 has already been logged
                                                         !< (to avoid multiple logs).

        if (.not. conv_adjustment_logged) then
            if (conv_cm3_gc > conv_j_gc * eg_l3) then
                call get_log_unit_number(lunrep)
                write(lunrep,*) 'WARNING: conv_cm3_gC larger than costs for growth.'
                write(lunrep,*) 'Therefore adjusted to conv_J_gC * Eg_L3'
                write(lunrep,*) 'This implies kappa_G=0 (no overhead costs for growth)'
                conv_adjustment_logged = .false.
            end if
        end if
        get_maximum_conversion_coeff = min(conv_cm3_gC, conv_J_gC * eg_L3)
    end function get_maximum_conversion_coeff

    !> Rescale the units of different parameters to per unit of surface instead of per volume
    subroutine rescale_units(iv, ov, av, dens, dens_m2, v_m2, e_m2, r_m2, food_count)
        use m_debgrz_input
        use m_debgrz_output
        use m_debgrz_auxiliary

        integer(kind=int_wp), intent(in) :: food_count !< Number of food types

        real(kind=real_wp), intent(out) :: dens    !< Density derived from Vtot(unit dep on BENTHS)
        real(kind=real_wp), intent(out) :: dens_m2 !< Density derived from Vtot per m2
        real(kind=real_wp), intent(out) :: v_m2    !< Population structural biomass             [gC/m2]
        real(kind=real_wp), intent(out) :: e_m2    !< Population energy biomass                 [gC/m2]
        real(kind=real_wp), intent(out) :: r_m2    !< Population gonadal (reproductive) biomass [gC/m2]

        type(debgrz_input)    , intent(inout) :: iv !< Input variables
        type(debgrz_output)   , intent(inout) :: ov !< Output variables
        type(debgrz_auxiliary), intent(inout) :: av !< Auxiliary variables

        ! local
        real(kind=real_wp)   :: area
        real(kind=real_wp)   :: vd
        real(kind=real_wp)   :: depth_factor

        call calculate_rescale_factors(iv%switchv1, iv%benths, iv%shape, iv%length_ini, iv%vtot, iv%conv_cm3_gc, &
                                         iv%dens_ini, iv%depth, vd, dens, depth_factor)

        call rescale_non_food_vars(iv%switchv1, depth_factor, dens, iv%conv_cm3_gc, iv%conv_j_gc, iv%shape, iv%em_l3, &
                                   vd, iv%vtot, iv%etot, iv%rtot, dens_m2, v_m2, e_m2, r_m2, &
                                   ov%v, ov%e, ov%r, ov%length, ov%e_scaled)

        call rescale_food_arrays(iv%get_area(), food_count, av%benfood, av%cfood)
    end subroutine rescale_units

    !> Calculates the factors to rescale units
    subroutine calculate_rescale_factors(switchv1, benths, shape, length_ini, vtot, conv_cm3_gc, &
                                         dens_ini, depth, vd, dens, depth_factor)
        integer(kind=int_wp), intent(in   ) :: switchv1     !< Use ISO-morphs (0) or V1-morphs (1)            [-]
        integer(kind=int_wp), intent(in   ) :: benths       !< Switch to use benthic or pelagic for DEB species
                                                            !< 0 = do not use benthic = use pelagic;
                                                            !< 1 = use benthic
        real(kind=real_wp), intent(in   )   :: shape        !< Shape coefficient                              [-]
        real(kind=real_wp), intent(in   )   :: length_ini   !< Initial Individual Length                 [gWW/m2]
        real(kind=real_wp), intent(in   )   :: vtot         !< Structural biomass grazer pop.    [gC/m3 or gC/m2]
        real(kind=real_wp), intent(in   )   :: conv_cm3_gc  !< Conversion factor from cm3 into gC        [gC/cm3]
        real(kind=real_wp), intent(in   )   :: dens_ini     !< Initial number of grazer individuals           [-]
        real(kind=real_wp), intent(in   )   :: depth        !< Depth of segment                               [m]
        real(kind=real_wp), intent(inout)   :: vd           !< Reference volume                             [cm3]
        real(kind=real_wp), intent(  out)   :: dens         !< Density derived from Vtot(unit dep on BENTHS)
        real(kind=real_wp), intent(  out)   :: depth_factor !< Factor to rescale depth

        ! convert benthic and pelagic grazer components to units /m2,
        if (switchv1==1) then
            vd = (shape*length_ini)**3                                 !Vd is reference volume (cm3)
            dens = max((vtot/conv_cm3_gc)/vd,tiny(dens))               !Density derived from Vtot(unit dep on BENTHS)
        else
            dens = max(dens_ini,tiny(dens))                            !Density is dynamic variable
        end if

        if (benths==0) then          ! pelagics (=active substance)
            depth_factor = depth     ! conversion from #/m3 to #/m2
        else                            ! benthics (=inactive substance)
            depth_factor = 1.0          ! no conversion or conversion from #/cell to #/m2
        end if

    end subroutine calculate_rescale_factors

    !> Rescales non food-related variables
    subroutine rescale_non_food_vars(switchv1, depth_factor, dens, conv_cm3_gc, conv_j_gc, shape, em_l3, &
                                     vd, vtot, etot, rtot, dens_m2, v_m2, e_m2, r_m2, v, e, r, length, e_scaled)
        integer(kind=int_wp), intent(in   ) :: switchv1     !<

        real(kind=real_wp), intent(in   )   :: depth_factor !<
        real(kind=real_wp), intent(in   )   :: dens         !< Density derived from Vtot(unit dep on BENTHS)
        real(kind=real_wp), intent(in   )   :: conv_cm3_gc  !< Conversion factor from cm3 into gC        [gC/cm3]
        real(kind=real_wp), intent(in   )   :: conv_j_gc    !< Conversion factor from energy into mass     [gC/J]
        real(kind=real_wp), intent(in   )   :: shape        !< Shape coefficient                              [-]
        real(kind=real_wp), intent(in   )   :: em_l3        !< Maximum storage density of DEB species     [J/cm3]
        real(kind=real_wp), intent(in   )   :: vd           !< Reference volume                             [cm3]
        real(kind=real_wp), intent(in   )   :: vtot         !< Structural biomass grazer pop.    [gC/m3 or gC/m2]
        real(kind=real_wp), intent(inout)   :: etot         !< Structural biomass grazer pop.    [gC/m3 or gC/m2]
        real(kind=real_wp), intent(inout)   :: rtot         !< Reproductional storage grazer pop.[gC/m3 or gC/m2]
        real(kind=real_wp), intent(  out)   :: dens_m2      !< Density derived from Vtot per m2
        real(kind=real_wp), intent(  out)   :: v_m2         !< Population structural biomass             [gC/m2]
        real(kind=real_wp), intent(  out)   :: e_m2         !< Population energy biomass                 [gC/m2]
        real(kind=real_wp), intent(  out)   :: r_m2         !< Population gonadal (reproductive) biomass [gC/m2]
        real(kind=real_wp), intent(  out)   :: v            !< Individual volume    [cm3/ind]
        real(kind=real_wp), intent(  out)   :: e            !< Individual energy      [J/ind]
        real(kind=real_wp), intent(  out)   :: r            !< Individual gonads      [J/ind]
        real(kind=real_wp), intent(  out)   :: length       !< Individual Length         [cm]
        real(kind=real_wp), intent(  out)   :: e_scaled     !< Scaled energy density      [-]

        etot=max(0.,etot)
        rtot=max(0.,rtot)
        v_m2 = vtot * depth_factor
        e_m2 = etot * depth_factor
        r_m2 = rtot * depth_factor
        dens_m2 = dens * depth_factor

        v = max( vtot / (dens * conv_cm3_gc), tiny(v) )
        e = max( etot / (dens * conv_j_gc),   tiny(e) )
        r = max( rtot / (dens * conv_j_gc),   tiny(r) )

        if (switchv1==1) then
            v=vd                     !to avoid numerical artifacts at small biomasses
        end if

        length = (v**(1/3.0))/shape      !Length is derived from individual V
        e_scaled = e /( em_l3 * v )   !E_scaled is derived from E and V
    end subroutine rescale_non_food_vars

    !> Rescales food arrays using the area
    subroutine rescale_food_arrays(area, food_count, benfood, cfood)
        real(kind=real_wp),   intent(in   ) :: area       !< Area of the cell
        integer(kind=int_wp), intent(in   ) :: food_count !< Number of food types
        integer(kind=int_wp), intent(in   ) :: benfood(:) !< Benthic foods (true/false)
        real(kind=real_wp),   intent(inout) :: cfood(:)   !< Carbon foods

        integer(kind=int_wp) :: ifood

        ! convert benthic FOOD components to units gC/m2, do not convert pelagic components: unit stays gC/m3
        do ifood = 1,food_count
            if (benfood(ifood)==1) then
                cfood(ifood)=max(cfood(ifood) / area , 0.)
            end if
        end do
    end subroutine rescale_food_arrays

    !> Calculates a temperature-dependent rate (kT) using Arrhenius equations and additional factors to
    !! describe the temperature sensitivity of biological processes
    function temperature_dependent_rate(temp, ta, tal, tah, th, tl) result(kt)

        real(kind = real_wp), intent(in) :: temp !< Ambient water temperature                       [oC]
        real(kind = real_wp), intent(in) :: ta   !< Arrhenius temperature                            [K]
        real(kind = real_wp), intent(in) :: tal  !< Arr temp for rate of decrease at lower boundary  [K]
        real(kind = real_wp), intent(in) :: tah  !< Arr temp for rate of decrease at upper boundary  [K]
        real(kind = real_wp), intent(in) :: th   !< Upper boundary of tolerance range                [K]
        real(kind = real_wp), intent(in) :: tl   !< Lower boundary of tolerance range                [K]

        real(kind = real_wp) :: kt               !< Temperature dependent rate

        kt = exp(ta / (20. + 273.) - ta / (temp + 273.)) * &
              (1. + exp(tal / (20. + 273.) - tal / tl) + exp(tah / th - tah / (20. + 273.)))  &
              / (1. + exp(tal / (temp + 273.) - tal / tl) + exp(tah / th - tah / (temp + 273.)))
    end function temperature_dependent_rate

    !> Calculates Uptake: filtration, ingestion and assimilation per individual,
    !! effective food concentrations (gC/m3), and their faecal (=indigestible) fractions (faecal_fraction)
    subroutine calculate_uptake(food_count, is_food_benthic, cfood, fffood, pref, suspension, xk, &
                                minfood, dfil, totaldepth, ccfood, ncfood, pcfood, sicfood, conv_j_gc, &
                                jxm_l2, kappa_i, tim, yk, c_filtr, n_filtr, p_filtr, si_filtr, kT, &
                                v, dens_m2, delt, depth, faecal_fraction, food_pelagic, food_benthic)
        integer(kind=int_wp), intent(in   ) :: food_count           !< Number of food types
        integer(kind=int_wp), intent(in   ) :: is_food_benthic(:)   !< Is food type benthic (1) or pelagic (0)

        real(kind=real_wp),   intent(inout) :: cfood(:)   !< Carbon foods
        real(kind=real_wp),   intent(in   ) :: fffood(:)  !< Faecal fraction of grazers
        real(kind=real_wp),   intent(in   ) :: ccfood(:)  !< Stoichiometry ratio of carbon to food unit
        real(kind=real_wp),   intent(in   ) :: ncfood(:)  !< Nitrogen foods
        real(kind=real_wp),   intent(in   ) :: pcfood(:)  !< Phosphorus foods
        real(kind=real_wp),   intent(in   ) :: sicfood(:) !< Silicon foods
        real(kind=real_wp),   intent(in   ) :: pref(:)    !< Preference of DEB grazers
        real(kind=real_wp),   intent(in   ) :: suspension !< DEB species preference for suspension over deposit feeding
        real(kind=real_wp),   intent(in   ) :: xk         !< Halfrate const food uptake Sup fdr               [gC/m3]
        real(kind=real_wp),   intent(in   ) :: minfood    !< Minimum amount of food for DEB species
        real(kind=real_wp),   intent(in   ) :: totaldepth !< Depth of entire water column to which cell belongs   [m]
        real(kind=real_wp),   intent(in   ) :: conv_j_gc  !< Conversion factor from energy into mass           [gC/J]
        real(kind=real_wp),   intent(in   ) :: jxm_l2     !< Max ingestion rate of DEB species              [J/cm2/d]
        real(kind=real_wp),   intent(in   ) :: kappa_i    !< Ingestion efficiency (pseudofaeces production)       [-]
        real(kind=real_wp),   intent(in   ) :: tim        !< Total inorganic matter                          [gDM/m3]
        real(kind=real_wp),   intent(in   ) :: yk         !< Halfrate const TIM                               [gC/m3]
        real(kind=real_wp),   intent(in   ) :: kT         !< Temperature_dependent_rate
        real(kind=real_wp),   intent(in   ) :: v          !< Individual volume                              [cm3/ind]
        real(kind=real_wp),   intent(in   ) :: dens_m2    !< Density derived from Vtot per m2
        real(kind=real_wp),   intent(in   ) :: delt       !< Timestep for processes                               [d]
        real(kind=real_wp),   intent(in   ) :: depth      !< Depth of segment/cell                                [m]

        real(kind=real_wp), intent(inout) :: faecal_fraction !< Global value for faecal fraction
        real(kind=real_wp), intent(inout) :: dfil(:)         !< Daily filtration rate for each food type [gC/ind/d]

        real(kind=real_wp), intent(  out)   :: c_filtr  !< Daily filtration rate for carbon     [gC/ind/d]
        real(kind=real_wp), intent(  out)   :: n_filtr  !< Daily filtration rate for nitrogen   [gN/ind/d]
        real(kind=real_wp), intent(  out)   :: p_filtr  !< Daily filtration rate for phosphorus [gP/ind/d]
        real(kind=real_wp), intent(  out)   :: si_filtr !< Daily filtration rate for silicon   [gSi/ind/d]
        real(kind=real_wp), intent(  out)   :: food_pelagic !< Food for pelagic organisms
        real(kind=real_wp), intent(  out)   :: food_benthic !< Food for benthic organisms

        ! local vars
        real(kind=real_wp) :: food_suspended          !< Suspended food
        real(kind=real_wp) :: food_bottom             !< Food at the bottom of the water column
        real(kind=real_wp) :: faecal_fraction_pelagic !< Faecal fraction corresponding to pelagic organisms
        real(kind=real_wp) :: faecal_fraction_benthic !< Faecal fraction corresponding to benthic organisms
        real(kind=real_wp) :: xk_suspended            !< Half saturation for suspended food
        real(kind=real_wp) :: xk_bottom               !< Half saturation for food in bottom
        real(kind=real_wp) :: dupte                   !< Daily energy ingestion rate or uptake [J/ind/d]

        integer(kind=int_wp) :: ifood !< Index for food type iteration

        food_pelagic = 0.
        food_benthic = 0.
        faecal_fraction_pelagic = 0.
        faecal_fraction_benthic = 0.
        do ifood = 1,food_count
            cfood(ifood) = pref(ifood) * cfood(ifood)
            if (is_food_benthic(ifood)==1) then
                food_benthic = food_benthic + cfood(ifood)
                faecal_fraction_benthic = faecal_fraction_benthic + fffood(ifood) * cfood(ifood)
            else
                food_pelagic = food_pelagic + cfood(ifood)
                faecal_fraction_pelagic = faecal_fraction_pelagic + fffood(ifood) * cfood(ifood)
            end if
        end do
        faecal_fraction_benthic = faecal_fraction_benthic/(food_benthic+tiny(food_benthic))
        faecal_fraction_pelagic = faecal_fraction_pelagic/(food_pelagic+tiny(food_pelagic))
        faecal_fraction = (1.-suspension) * faecal_fraction_benthic + suspension * faecal_fraction_pelagic

        ! Calculate scaled functional respons FoodPel (-)
        ! Half-saturation constant same for suspended and bottom food
        xk_suspended = xk
        xk_bottom = xk

        ! No assimilation and uptake when depth < 5 cm (to prevent uptake at dry-falling mudflats)
        if (totaldepth < 0.05) then
            food_suspended = 0.
            food_bottom    = 0.
        else
            food_suspended = ((food_pelagic - minfood) / ( (food_pelagic - minfood) +  xk_suspended * (1. + tim/yk)))  ! ZZ: Modify half-saturation relationship with a minimum food threshold.
            food_bottom    = ((food_benthic - minfood) / ( (food_benthic - minfood) +  xk_bottom *    (1. + tim/yk)))  ! ZZ: Modify half-saturation relationship with a minimum food threshold.
        end if
        ! to avoid negative values or values larger than 1, e.g. due to negative TIM
        food_suspended = max(food_suspended,0.)
        food_bottom    = max(food_bottom,   0.)
        food_suspended = min(food_suspended,1.)
        food_bottom    = min(food_bottom,   1.)

        ! Calculate the energy ingestion rates (J/ind/d) and filtration rates (gC/ind/d)
        ! Filtration rates are determined from ingestion rates by conversion into units of gC,
        ! by correction for pseudofaeces losses (1/kappaI),
        ! and for the faecal fraction of food (FF), which fraction is low in energy and thus does not increase
        ! the ingested energy, but does add to the ingested carbon.

        c_filtr   = 0.
        n_filtr  = 0.
        p_filtr  = 0.
        si_filtr = 0.

        do ifood= 1, food_count
            if (is_food_benthic(ifood)==1) then ! Deposit (benthic) feeding
                dupte = (1.-suspension)*(cfood(ifood)/(food_benthic+tiny(food_benthic))) &
                        * food_bottom * kT  * (v**(2./3.)) * jxm_l2
                dfil(ifood) = dupte * (conv_j_gc/(1.-faecal_fraction_benthic))*(1./kappa_i)
            else                                ! Suspension (pelagic) feeding
                dupte = suspension * ( cfood(ifood) / (food_pelagic+tiny(food_pelagic))) &
                        * food_suspended * kT  * (v**(2./3.)) * jxm_l2
                dfil(ifood) = dupte * (conv_j_gc/(1.-faecal_fraction_pelagic))*(1./kappa_i)
            end if

            dfil(ifood) = min(dfil(ifood), &
                            ( (cfood(ifood) - minfood)/(delt*dens_m2/depth)))      !ZZ: to impose a minimum food conc threshold (mainly a threshold for large time steps).
            dfil(ifood) = max(dfil(ifood),0.)                                      !to avoid negative food uptake

            c_filtr  = c_filtr  + dfil(ifood) * ccfood(ifood)
            n_filtr  = n_filtr  + dfil(ifood) * ncfood(ifood)
            p_filtr  = p_filtr  + dfil(ifood) * pcfood(ifood)
            si_filtr = si_filtr + dfil(ifood) * sicfood(ifood)
        end do
    end subroutine calculate_uptake

    !> Calculates defaecation per individual
    !! From the ingested material, a fraction is lost due to (lack of) assimilation efficiency (kappa_A)
    !! leading to faeces production and/or due to (lack of) ingestion efficiency (kappa_I), which, on its turn, leads to
    !! pseudofaeces production. Also, the faecal food fraction (faecal_fraction) is not assimilated; it is assumed to
    !! consist of carbon fibres only, and to be low in energy.
    !! Furthermore the assimilated material has to match the N/C and P/C ratio of the grazer
    subroutine calculate_defaecation(kappa_i, kappa_a, tn, tp, conv_j_gc, c_filtr, n_filtr, p_filtr, si_filtr, &
                                    faecal_fraction, c_defaec, n_defaec, p_defaec, si_defaec, pa)

        real(kind=real_wp), intent(in   ) :: kappa_i         !< Ingestion efficiency (pseudofaeces production)  [-]
        real(kind=real_wp), intent(in   ) :: kappa_a         !< Assimilation efficiency                         [-]
        real(kind=real_wp), intent(in   ) :: tn              !< N:C ratio grazers                           [gN/gC]
        real(kind=real_wp), intent(in   ) :: tp              !< P:C ratio grazers                           [gP/gC]
        real(kind=real_wp), intent(in   ) :: conv_j_gc       !< Conversion factor from energy into mass      [gC/J]
        real(kind=real_wp), intent(in   ) :: c_filtr         !< Daily filtration rate for carbon         [gC/ind/d]
        real(kind=real_wp), intent(in   ) :: n_filtr         !< Daily filtration rate for nitrogen       [gN/ind/d]
        real(kind=real_wp), intent(in   ) :: p_filtr         !< Daily filtration rate for phosphorus     [gP/ind/d]
        real(kind=real_wp), intent(in   ) :: si_filtr        !< Daily filtration rate for silicon       [gSi/ind/d]
        real(kind=real_wp), intent(in   ) :: faecal_fraction !< Faecal fraction
        real(kind=real_wp), intent(  out) :: c_defaec        !< Daily defaecation rate for carbon        [gC/ind/d]
        real(kind=real_wp), intent(  out) :: n_defaec        !< Daily defaecation rate for nitrogen      [gN/ind/d]
        real(kind=real_wp), intent(  out) :: p_defaec        !< Daily defaecation rate for phosphorus    [gP/ind/d]
        real(kind=real_wp), intent(  out) :: si_defaec       !< Si loss by defaecation in carbon equivalents [gC/d]
        real(kind=real_wp), intent(  out) :: pa              !< Energy reserves                           [J/ind/d]

        real(kind=real_wp) :: c_uptake   !< Carbon uptake                           [gC/ind/d]
        real(kind=real_wp) :: n_uptake   !< Nitrogen uptake in carbon equivalents   [gN/ind/d]
        real(kind=real_wp) :: p_uptake   !< Phosphorus uptake in carbon equivalents [gP/ind/d]
        real(kind=real_wp) :: lim_uptake !< Limiting uptake in carbon equivalents   [gC/ind/d]

        if (c_filtr > 0.) then
            c_uptake = c_filtr * (kappa_i * kappa_a) * (1.-faecal_fraction)
            n_uptake = n_filtr * (kappa_i * kappa_a) / tn
            p_uptake = p_filtr * (kappa_i * kappa_a) / tp
            lim_uptake = min(n_uptake, p_uptake, c_uptake)

            ! Pseudofaeces, efficiency losses, and excess nutrients are all released as Faeces
            ! All uptake of silicate is lost by defaecation
            c_defaec  = c_filtr - lim_uptake
            n_defaec  = (n_filtr/ tn - lim_uptake) * tn
            p_defaec  = (p_filtr/tp - lim_uptake) * tp
            si_defaec = si_filtr
        else ! no food uptake, so no stoichiometric losses
            c_defaec = 0.
            n_defaec = 0.
            p_defaec = 0.
            si_defaec = 0.
            lim_uptake = 0.
        end if
        ! The remaining material is assimilated into the energy reserves
        pa  =  lim_uptake / conv_j_gc
    end subroutine calculate_defaecation

    !>  Energy reserve dynamics per individual
    !!  Volume specific and theoretically maximum uptake rate
    !!  this is the maximum rate with which energy can be obtained from the energy reserves
    !!  and (being a theoretical maximum) it is not dependent on the actual algae uptake Pa
    subroutine calculate_energy_reserve_dynamics(jxm_l2, kappa_a, eg_l3, em_l3, pm_l3, kappa, delt, kT, v, e, pc)
        real(kind=real_wp), intent(in   ) :: jxm_l2  !< Max ingestion rate of DEB species           [J/cm2/d]
        real(kind=real_wp), intent(in   ) :: kappa_a !< Assimilation efficiency                           [-]
        real(kind=real_wp), intent(in   ) :: eg_l3   !< Volume-spec costs for growth of DEB species   [J/cm3]
        real(kind=real_wp), intent(in   ) :: em_l3   !< Maximum storage density of DEB species        [J/cm3]
        real(kind=real_wp), intent(in   ) :: pm_l3   !< Respiration rate constant of DEB species        [J/d]
        real(kind=real_wp), intent(in   ) :: kappa   !< Fraction of util.energy spent on maint&growth     [-]
        real(kind=real_wp), intent(in   ) :: delt    !< Timestep for processes                            [d]
        real(kind=real_wp), intent(in   ) :: kT      !< Temperature_dependent_rate
        real(kind=real_wp), intent(in   ) :: v       !< Individual volume                           [cm3/ind]
        real(kind=real_wp), intent(in   ) :: e       !< Individual energy                             [J/ind]
        real(kind=real_wp), intent(  out) :: pc      !< Energy reserve dynamics per individual      [J/ind/d]

        real(kind=real_wp) :: pam_l2
        real(kind=real_wp) :: e_l3

        pam_l2 = jxm_l2 * kappa_a * kT
        e_l3 = (e/(v+tiny(v)))
        pc = ((eg_l3 / em_l3) * pam_l2 * v**(2./3.) + pm_l3*v*kT ) * &
                (e_l3 /(kappa*e_l3 + eg_l3))
        pc = min(pc, e/delt)
        pc = max(pc, 0.)
    end subroutine calculate_energy_reserve_dynamics

    !> Calculate maintenance per individual
    !! Respiration is only due to basal respiration, not to activity or stress.
    !! Respiration of nutrients is related to the carbon respiration with ratios TN and TP
    subroutine calculate_maintenance(pm_l3, v, kt, pm)
        real(kind=real_wp), intent(in   ) :: pm_l3  !< Respiration rate constant of DEB species [J/d]
        real(kind=real_wp), intent(in   ) :: v      !< Individual volume                    [cm3/ind]
        real(kind=real_wp), intent(in   ) :: kt     !< Temperature-dependent rate
        real(kind=real_wp), intent(  out) :: pm     !< Maintenance per individual           [J/ind/d]

        pm = pm_l3 * v * kt
    end subroutine calculate_maintenance

    !> Growth per individual:
    !! when growing, energy will be put in the new tissue and some will be lost due to overhead costs
    !! if too little energy catabolized to pay maintenance, the organisms will shrink
    !! in that case, the overhead costs are assumed to be proportional to those for growth
    subroutine calculate_growth(kappa, pc, conv_cm3_gc, conv_j_gc, eg_l3, delt, v, pm, pv, pg, kappa_g)
        real(kind=real_wp), intent(in   ) :: kappa       !< Fraction of util.energy spent on maint&growth  [-]
        real(kind=real_wp), intent(in   ) :: pc          !< Energy reserve dynamics per individual   [J/ind/d]
        real(kind=real_wp), intent(in   ) :: conv_cm3_gc !< Conversion factor from cm3 into gC        [gC/cm3]
        real(kind=real_wp), intent(in   ) :: conv_j_gc   !< Conversion factor from energy into mass     [gC/J]
        real(kind=real_wp), intent(in   ) :: eg_l3       !< Volume-spec costs for growth of DEB species[J/cm3]
        real(kind=real_wp), intent(in   ) :: delt        !< Timestep for processes                         [d]
        real(kind=real_wp), intent(in   ) :: v           !< Individual volume                        [cm3/ind]
        real(kind=real_wp), intent(inout) :: pm          !< Maintenance per individual               [J/ind/d]
        real(kind=real_wp), intent(  out) :: pv          !< Overhead costs per volume                [J/ind/d]
        real(kind=real_wp), intent(  out) :: pg          !< Energy costs for growth                  [J/ind/d]
        real(kind=real_wp), intent(  out) :: kappa_g     !< Overhead costs for growth                      [-]

        pg = kappa*pc - pm
        kappa_g = conv_cm3_gc/(conv_j_gc*eg_l3)

        if (pg > 0.) then
            pv = kappa_g*pg
        else
            pv = (1.+ (1.-kappa_g))*pg
            pm = pm + abs((1.-kappa_g) * pg)

            pv = max(pv,(-v/delt)*(conv_cm3_gc/conv_j_gc))
            pg = max(pg, (((-v/delt)/(1.+ (1.-kappa_g)))*(conv_cm3_gc/conv_j_gc)))
            pm = min(pm,(kappa*pc + (v/delt)*(conv_cm3_gc/conv_j_gc)))
        end if
    end subroutine calculate_growth

    !> Maturity and reproduction per individual
    !! ISO-morphs only produce gonads if they are larger than Vp and if GSI > G_upper
    !! V1-morphs produce gonads with a fraction related to the ratio of V and Vp
    !! Some adjustments were made with respect to original equations to make sure all catabolized energy is
    !! being used
    subroutine calculate_maturity_and_reproduction(switchv1, vp, conv_j_gc, conv_cm3_gc, kappa, pm_l3, &
                                kappar, delt, gsi_upper, gsi_lower, temp, minsptemp, &
                                v, e, r, pc, kT, rspawn, tn, tp, dospawn, gsi, &
                                pjj, prj, pja, pra, pr, dspw, dnspw, dpspw)
        integer(kind=int_wp), intent(in) :: switchv1     !< Use ISO-morphs (0) or V1-morphs (1)

        real(kind=real_wp), intent(in   ) :: vp          !< Volume at start of reproductive stage        [cm3]
        real(kind=real_wp), intent(in   ) :: conv_j_gc   !< Conversion factor from energy into mass     [gC/J]
        real(kind=real_wp), intent(in   ) :: conv_cm3_gc !< Conversion factor from cm3 into gC        [gC/cm3]
        real(kind=real_wp), intent(in   ) :: kappa       !< Fraction of util.energy spent on maint&growth  [-]
        real(kind=real_wp), intent(in   ) :: pm_l3       !< Respiration rate constant of DEB species     [J/d]
        real(kind=real_wp), intent(in   ) :: kappar      !< Fraction of repro.energy spent on              [-]
        real(kind=real_wp), intent(in   ) :: delt        !< Timestep for processes                         [d]
        real(kind=real_wp), intent(in   ) :: gsi_upper   !< Minimum GSI for spawning                       [-]
        real(kind=real_wp), intent(in   ) :: gsi_lower   !< Minimum GSI while spawning                     [-]
        real(kind=real_wp), intent(in   ) :: temp        !< Ambient water temperature                     [oC]
        real(kind=real_wp), intent(in   ) :: minsptemp   !< Minimum temperature for spawning              [oC]
        real(kind=real_wp), intent(in   ) :: v           !< Individual volume                        [cm3/ind]
        real(kind=real_wp), intent(in   ) :: e           !< Individual energy                          [J/ind]
        real(kind=real_wp), intent(in   ) :: r           !< Individual gonads                          [J/ind]
        real(kind=real_wp), intent(in   ) :: pc          !< Energy reserve dynamics per individual   [J/ind/d]
        real(kind=real_wp), intent(in   ) :: kT          !< Temperature_dependent_rate
        real(kind=real_wp), intent(in   ) :: rspawn      !< Spawning rate                                  [-]
        real(kind=real_wp), intent(in   ) :: tn          !< N:C ratio grazers                          [gN/gC]
        real(kind=real_wp), intent(in   ) :: tp          !< P:C ratio grazers                          [gP/gC]
        real(kind=real_wp), intent(inout) :: dospawn     !< Indication of spawning                         [-]
        real(kind=real_wp), intent(  out) :: gsi         !< Gonadosomatic Index                            [-]
        real(kind=real_wp), intent(  out) :: pjj         !< Maturity maintenance juveniles           [J/ind/d]
        real(kind=real_wp), intent(  out) :: prj         !< Maturity development                     [J/ind/d]
        real(kind=real_wp), intent(  out) :: pja         !< Maturity maintenance adults              [J/ind/d]
        real(kind=real_wp), intent(  out) :: pra         !< Specific energy for reproduction         [J/ind/d]
        real(kind=real_wp), intent(  out) :: pr          !< Energy for reproduction                  [J/ind/d]
        real(kind=real_wp), intent(  out) :: dspw        !< Delta energy for carbon for spawning     [J/ind/d]
        real(kind=real_wp), intent(  out) :: dnspw       !< Delta energy for nitrogen for spawning   [J/ind/d]
        real(kind=real_wp), intent(  out) :: dpspw       !< Delta energy for phosphorus for spawning [J/ind/d]

        real(kind=real_wp) :: fadult                     !< Adult fraction of the population               [-]
        real(kind=real_wp) :: fjuv                       !< Juvenile fraction of the population            [-]

        call get_maturity_fractions(switchv1, v, vp, fjuv, fadult)
        gsi = get_gsi(switchv1, vp, v, e, r, fadult, conv_j_gc, conv_cm3_gc)

        call calculate_maturity_and_reproduction_energies(vp, kappa, pm_l3, kappar, delt, v, r, pc, kT, fadult, fjuv, &
                                                          pjj, prj, pja, pra, pr)
        call calculate_spawning(gsi, gsi_lower, gsi_upper, temp, minsptemp, r, rspawn, delt, tn, tp, &
                                pr, dospawn, dspw, dnspw, dpspw)
    end subroutine calculate_maturity_and_reproduction

    !> Calculates the adult and juvenile fractions of the population.
    subroutine get_maturity_fractions(switchv1, v, vp, fjuv, fadult)
        integer(kind=int_wp), intent(in   ) :: switchv1 !< Use ISO-morphs (0) or V1-morphs (1)         [-]

        real(kind=real_wp), intent(in   )   :: v        !< Individual volume                     [cm3/ind]
        real(kind=real_wp), intent(in   )   :: vp       !< Volume at start of reproductive stage [cm3/ind]
        real(kind=real_wp), intent(  out)   :: fjuv     !< Juvenile fraction of the population         [-]
        real(kind=real_wp), intent(  out)   :: fadult   !< Adult fraction of the population            [-]

        if (switchv1==1) then
            fjuv= vp/(vp+v)
        elseif (v < vp) then
            fjuv=1.
        else
            fjuv=0.
        end if
        fadult = 1. - fjuv
    end subroutine get_maturity_fractions

    !> Calculates the gonadosomatic index (GSI).
    function get_gsi(switchv1, vp, v, e, r, fadult, conv_j_gc, conv_cm3_gc) result(gsi)
        integer(kind=int_wp), intent(in   ) :: switchv1 !< Use ISO-morphs (0) or V1-morphs (1)            [-]

        real(kind=real_wp), intent(in   )   :: vp          !< Volume at start of reproductive stage [cm3/ind]
        real(kind=real_wp), intent(in   )   :: v           !< Individual volume                     [cm3/ind]
        real(kind=real_wp), intent(in   )   :: e           !< Individual energy                       [J/ind]
        real(kind=real_wp), intent(in   )   :: r           !< Individual gonads                       [J/ind]
        real(kind=real_wp), intent(in   )   :: fadult      !< Adult fraction of the population            [-]
        real(kind=real_wp), intent(in   )   :: conv_j_gc   !< Conversion factor from energy into mass  [gC/J]
        real(kind=real_wp), intent(in   )   :: conv_cm3_gc !< Conversion factor from cm3 into gC     [gC/cm3]
        real(kind=real_wp)                  :: gsi         !< Gonadosomatic Index                         [-]

        if (switchv1/=1 .and. v < vp) then
            gsi = 0.
            return
        end if
        gsi = (r*conv_j_gc)/(v*fadult*conv_cm3_gc + &
              e*fadult*conv_j_gc + r*conv_j_gc)
    end function get_gsi

    !> Calculates the energies associated to the different fractions and stages of development of the species.
    subroutine calculate_maturity_and_reproduction_energies(vp, kappa, pm_l3, kappar, delt, v, r, pc, kT, fadult, fjuv, &
                                                            pjj, prj, pja, pra, pr)
        real(kind=real_wp), intent(in   ) :: vp          !< Volume at start of reproductive stage        [cm3]
        real(kind=real_wp), intent(in   ) :: kappa       !< Fraction of util.energy spent on maint&growth  [-]
        real(kind=real_wp), intent(in   ) :: pm_l3       !< Respiration rate constant of DEB species     [J/d]
        real(kind=real_wp), intent(in   ) :: kappar      !< Fraction of repro.energy spent on              [-]
        real(kind=real_wp), intent(in   ) :: delt        !< Timestep for processes                         [d]
        real(kind=real_wp), intent(in   ) :: v           !< Individual volume                     [cm3/ind]
        real(kind=real_wp), intent(in   ) :: r           !< Individual gonads                          [J/ind]
        real(kind=real_wp), intent(in   ) :: pc          !< Energy reserve dynamics per individual   [J/ind/d]
        real(kind=real_wp), intent(in   ) :: kT          !< Temperature_dependent_rate
        real(kind=real_wp), intent(in   ) :: fadult      !< Adult fraction of the population               [-]
        real(kind=real_wp), intent(in   ) :: fjuv        !< Juvenile fraction of the population            [-]
        real(kind=real_wp), intent(  out) :: pjj         !< Maturity maintenance juveniles           [J/ind/d]
        real(kind=real_wp), intent(  out) :: prj         !< Maturity development                     [J/ind/d]
        real(kind=real_wp), intent(  out) :: pja         !< Maturity maintenance adults              [J/ind/d]
        real(kind=real_wp), intent(  out) :: pra         !< Specific energy for reproduction         [J/ind/d]
        real(kind=real_wp), intent(  out) :: pr          !< Energy for reproduction                  [J/ind/d]

        pjj = ((1.-kappa)/(kappa+tiny(kappa))) * pm_l3 * (v*fjuv) * kT
        pjj = min(pjj, ((1.-kappa) * pc * fjuv ))
        prj = max(0., ((1.-kappa) * pc * fjuv - pjj))                    !remainder goes to maturity development
        pja = ((1.-kappa)/(kappa+tiny(kappa)))* pm_l3 * (vp*fadult) * kT  !maturity maintenance adults (J/ind/d)
        pra = (1.-kappa) * pc * fadult - pja                             !remainder goes to reproduction flux

        ! if too little energy for juv mat maint and dev, these processes simply stop
        ! but if too little energy for adult mat maint, costs are paid by R with additional overhead costs
        ! proportional to kappaR:
        if (pra > 0.) then
            pr = kappar*pra
        else
            pr = (1.+(1.-kappar))*pra
            pja = pja + abs((1.-kappar) * pra)

            pr  = max(pr,  (-r/delt))
            pra = max(pra, (((-r/delt)/(1.+(1.-kappar)))))
            pja = min(pja, ((1.-kappa)*pc*fadult + (r/delt)))
        end if
    end subroutine calculate_maturity_and_reproduction_energies

    !> Calculates if spawing is taking place, and the deltas of energy associated to spawning for each nutrient type (C, N and P)
    subroutine calculate_spawning(gsi, gsi_lower, gsi_upper, temp, minsptemp, r, rspawn, delt, tn, tp, &
                                  pr, dospawn, dspw, dnspw, dpspw)
        real(kind=real_wp), intent(in   ) :: gsi         !< Gonadosomatic Index                            [-]
        real(kind=real_wp), intent(in   ) :: gsi_upper   !< Minimum GSI for spawning                       [-]
        real(kind=real_wp), intent(in   ) :: gsi_lower   !< Minimum GSI while spawning                     [-]
        real(kind=real_wp), intent(in   ) :: temp        !< Ambient water temperature                     [oC]
        real(kind=real_wp), intent(in   ) :: minsptemp   !< Minimum temperature for spawning              [oC]
        real(kind=real_wp), intent(in   ) :: r           !< Individual gonads                          [J/ind]
        real(kind=real_wp), intent(in   ) :: rspawn      !< Spawning rate                                  [-]
        real(kind=real_wp), intent(in   ) :: delt        !< Timestep for processes                         [d]
        real(kind=real_wp), intent(in   ) :: tn          !< N:C ratio grazers                          [gN/gC]
        real(kind=real_wp), intent(in   ) :: tp          !< P:C ratio grazers                          [gP/gC]
        real(kind=real_wp), intent(in   ) :: pr          !< Energy for reproduction                  [J/ind/d]
        real(kind=real_wp), intent(inout) :: dospawn     !< Indication of spawning                         [-]
        real(kind=real_wp), intent(  out) :: dspw        !< Delta energy for carbon for spawning     [J/ind/d]
        real(kind=real_wp), intent(  out) :: dnspw       !< Delta energy for nitrogen for spawning   [J/ind/d]
        real(kind=real_wp), intent(  out) :: dpspw       !< Delta energy for phosphorus for spawning [J/ind/d]

        ! if conditions are suitable, spawning will start
        if (gsi > gsi_upper) then
            dospawn = 1.
        end if

        ! spawning continues as long as conditions remain suitable
        if (dospawn > 0) then
            if (( gsi > gsi_lower .and. temp > minsptemp ) .and. r > 0 ) then
                dspw = (rspawn * r + max(pr, 0.))
                dspw = min(dspw, (r/delt + min(pr,0.)) )
            else
                dspw = 0.
            end if

            if ( gsi < gsi_lower ) then
                dospawn = 0.
            end if
        else
            dspw = 0.
        end if

        dnspw = dspw * tn
        dpspw = dspw * tp
    end subroutine calculate_spawning



    !> Shell formation fluxes per individual
    subroutine calculate_shell_formation_fluxes(pm, pja, pjj, prj, pv, fpgrosmo, fpdissmo, ycacosmo, ddis, pomm, pca)
        real(kind=real_wp), intent(in   ) :: pm       !< Maintenance per individual                      [J/ind/d]
        real(kind=real_wp), intent(in   ) :: pja      !< Maturity maintenance adults                     [J/ind/d]
        real(kind=real_wp), intent(in   ) :: pjj      !< Maturity maintenance juveniles                  [J/ind/d]
        real(kind=real_wp), intent(in   ) :: prj      !< Maturity development                            [J/ind/d]
        real(kind=real_wp), intent(in   ) :: pv       !< Energy flux to growth (i.e. structural biomass)[J/ind/d]
        real(kind=real_wp), intent(in   ) :: fpgrosmo !< Growth-based contribution to shell matrix      [-]
        real(kind=real_wp), intent(in   ) :: fpdissmo !< Dissipation-based contribution to shell matrix [-]
        real(kind=real_wp), intent(in   ) :: ycacosmo !< Yield coefficient CaCO3 deposition on matrix   [-]

! AM: for the second round
!        real(kind=real_wp), intent(in   ) :: satarg   !< aragonite saturation state                           [-]
!        real(kind=real_wp), intent(in   ) :: ksat     !< half sat const for reduced aragonite precipitation   [-]

        real(kind=real_wp), intent(  out) :: ddis     !< Dissipation flux (not the same as respiration!)[J/ind/d]
        real(kind=real_wp), intent(  out) :: pomm     !< Energy flux to organic shell matrix            [J/ind/d]
        real(kind=real_wp), intent(  out) :: pca      !< Energy flux to calcification of shell matrix   [J/ind/d]

        ddis = pm + pja + pjj + prj
        pomm = max( 0.0, pv * fpgrosmo + ddis * fpdissmo )
        if ( ycacosmo > 0.0 ) then
            pca  = max( 0.0, pomm / ycacosmo )
        else
            pca  = 0.0
        endif

        ! AM: for the second round
        ! s_s2 = satarg / (satarg + ksarag);                   ! reduced precipitation
        ! pca  =  max(0., (pomm / frsmosmi) * s_s2)
    end subroutine calculate_shell_formation_fluxes

 !> Respiration per individual
    !! Last two terms refer to overhead costs of growth and reproduction
    subroutine calculate_respiration(ddis, pomm, kappa_g, pg, pra, kappar, tn, tp, dres, dnres, dpres)
        real(kind=real_wp), intent(in   ) :: ddis    !< Dissipation flux                       [J/ind/d]
        real(kind=real_wp), intent(in   ) :: pomm    !< Energy flux to organic shell matrix    [J/ind/d]
        real(kind=real_wp), intent(in   ) :: kappa_g !< Overhead costs for growth                    [-]
        real(kind=real_wp), intent(in   ) :: pg      !< Energy costs for growth                [J/ind/d]
        real(kind=real_wp), intent(in   ) :: pra     !< Specific energy for reproduction       [J/ind/d]
        real(kind=real_wp), intent(in   ) :: kappar  !< Fraction of repro.energy spent on            [-]
        real(kind=real_wp), intent(in   ) :: tn      !< N:C ratio grazers                        [gN/gC]
        real(kind=real_wp), intent(in   ) :: tp      !< P:C ratio grazers                        [gP/gC]
        real(kind=real_wp), intent(  out) :: dres    !< Respiration per individual (carbon)
        real(kind=real_wp), intent(  out) :: dnres   !< Respiration per individual (nitrogen)
        real(kind=real_wp), intent(  out) :: dpres   !< Respiration per individual (phosphorus)

        real(kind=real_wp) :: overhead_costs_growth
        real(kind=real_wp) :: overhead_costs_reproduction

        overhead_costs_growth =       (1.-kappa_g) * max(pg, 0.)
        overhead_costs_reproduction = (1.-kappar)  * max(pra,0.)
        dres = ddis - pomm + overhead_costs_growth + overhead_costs_reproduction
        dnres = dres * tn
        dpres = dres * tp
    end subroutine calculate_respiration

    !> Natural mortality and harvesting (only former comes back into the system as detritus)
    !! These added fractions cannot be larger than one (minus the material used for maintenance, at Pv<0)
    subroutine calculate_mortality(rmor_ref, vtot, ddmfk, cmor, conv_j_gc, conv_cm3_gc, rhrv_ref, chrv, &
                        tn, tp, length, v, e, r, rmor, ddmf, rhrv, dmor, dnmor, dpmor, kt, pv)

        real(kind=real_wp), intent(in   ) :: rmor_ref    !< Reference mortality rate grazers           [1/d]
        real(kind=real_wp), intent(in   ) :: vtot        !< Structural biomass grazer pop.  [gC/m3 or gC/m2]
        real(kind=real_wp), intent(in   ) :: ddmfk       !< Halfrate concentration for density dependent 
                                                         !< mortality factor                [gC/m3 or gC/m2]
        real(kind=real_wp), intent(in   ) :: cmor        !< Length-dep coefficient mortality rate      [1/d]
        real(kind=real_wp), intent(in   ) :: conv_j_gc   !< Conversion factor from energy into mass   [gC/J]
        real(kind=real_wp), intent(in   ) :: conv_cm3_gc !< Conversion factor from cm3 into gC      [gC/cm3]
        real(kind=real_wp), intent(in   ) :: rhrv_ref    !< Reference  harvesting rate grazers         [1/d]
        real(kind=real_wp), intent(in   ) :: chrv        !< Length-dep coefficient harvesting rate     [1/d]
        real(kind=real_wp), intent(in   ) :: tp          !< P:C ratio grazers                        [gP/gC]
        real(kind=real_wp), intent(in   ) :: tn          !< N:C ratio grazers                        [gN/gC]
        real(kind=real_wp), intent(in   ) :: length      !< Individual Length                           [cm]
        real(kind=real_wp), intent(in   ) :: v           !< Individual volume                      [cm3/ind]
        real(kind=real_wp), intent(in   ) :: e           !< Individual energy                        [J/ind]
        real(kind=real_wp), intent(in   ) :: r           !< Individual gonads                        [J/ind]
        real(kind=real_wp), intent(in   ) :: kt          !< Temperature_dependent_rate
        real(kind=real_wp), intent(in   ) :: pv          !< Overhead costs per volume              [J/ind/d]
        real(kind=real_wp), intent(  out) :: rmor        !< Mortality rate
        real(kind=real_wp), intent(  out) :: ddmf        !< Density dependent mortality factor           [-]
        real(kind=real_wp), intent(  out) :: rhrv        !< Overhead costs per volume              [J/ind/d]
        real(kind=real_wp), intent(  out) :: dmor        !< Mortality difference for carbon        [gC/m3/d]
        real(kind=real_wp), intent(  out) :: dnmor       !< Mortality difference for nitrogen      [gN/m3/d]
        real(kind=real_wp), intent(  out) :: dpmor       !< Mortality difference for phosphorus    [gP/m3/d]

        ! local variables
        real(kind=real_wp) :: pvmin
        real(kind=real_wp) :: v_gc
        real(kind=real_wp) :: x

        pvmin = min(pv, 0.)
        v_gc = v*conv_cm3_gc
        x = (pvmin*conv_j_gc)/v_gc

        if(ddmfk > 0.) then
           ddmf = vtot/(vtot + ddmfk)
        else
           ddmf = 1.
        end if

        rmor  = rmor_ref * (length**cmor) * ddmf * kt
        rmor  = min(rmor,     (1. + (pvmin*conv_j_gc)/(v*conv_cm3_gc)))
        rhrv  = rhrv_ref * (length**chrv)
        rhrv  = min(rhrv,     (1. - rmor + (pvmin*conv_j_gc)/(v*conv_cm3_gc)))

        dmor  = rmor * (v*conv_cm3_gc + (e+r) * conv_j_gc)
        dnmor = dmor * tn
        dpmor = dmor * tp
    end subroutine calculate_mortality

end module m_debgrz_computations
