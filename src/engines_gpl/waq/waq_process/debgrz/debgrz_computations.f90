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
    use m_monsys

    implicit none

    private
    public :: set_maximum_conversion_coeff, &
              rescale_units, &
              temperature_dependent_rate, &
              calculate_uptake, &
              calculate_defaecation,  &
              calculate_energy_reserve_dynamics, &
              calculate_maintenance, &
              calculate_growth, &
              calculate_maturity_and_reproduction, &
              calculate_respiration,  &
              calculate_shell_formation_fluxes, &
              calculate_mortality

    contains


    subroutine set_maximum_conversion_coeff(conv_cm3_gc, conv_j_gc, eg_l3)
        !< Costs for growth set a maximum to the energy content of structural material and thus to the conversion
        !< coefficient conv_cm3_gC
        !< TThis may affect the overhead costs for growth (kappa_G)
        real(kind=real_wp), intent(inout) :: conv_cm3_gc !<
        real(kind=real_wp), intent(in   ) :: conv_j_gc   !<
        real(kind=real_wp), intent(in   ) :: eg_l3       !<

        integer(kind=int_wp) :: lunrep                   !< logical unit number for logging message
        logical, save :: conv_adjustment_logged = .true. !< Indicates if the adjustment of conv_cm3_gC to conv_J_gC * Eg_L3 has already been logged (to avoid multiple logs).

        if (.not. conv_adjustment_logged) then
            if (conv_cm3_gc > conv_j_gc * eg_l3) then
                call getmlu(lunrep)
                write(lunrep,*) 'WARNING: conv_cm3_gC larger than costs for growth.'
                write(lunrep,*) 'Therefore adjusted to conv_J_gC * Eg_L3'
                write(lunrep,*) 'This implies kappa_G=0 (no overhead costs for growth)'
                conv_adjustment_logged = .false.
            end if
        end if
        conv_cm3_gC = min(conv_cm3_gC, conv_J_gC * eg_L3)
    end subroutine set_maximum_conversion_coeff

    subroutine rescale_units(iv, ov, av, dens, dens_m2, v_m2, e_m2, r_m2, food_count)
        use m_debgrz_input
        use m_debgrz_output
        use m_debgrz_auxiliary

        integer(kind=int_wp), intent(in   ) :: food_count

        real(kind=real_wp),        intent(inout) :: dens
        real(kind=real_wp),        intent(inout) :: dens_m2
        real(kind=real_wp),        intent(  out) :: v_m2    !< population structural biomass (gC/m2)
        real(kind=real_wp),        intent(  out) :: e_m2    !< population energy biomass (gC/m2)
        real(kind=real_wp),        intent(  out) :: r_m2    !< population gonadal (reproductive) biomass (gC/m2)

        type(debgrz_input)    , intent(inout) :: iv      !< input variables
        type(debgrz_output)   , intent(inout) :: ov      !< output variables
        type(debgrz_auxiliary), intent(inout) :: av      !< auxiliary variables

        ! local
        integer(kind=int_wp) :: ifood
        real(kind=real_wp)   :: area
        real(kind=real_wp)   :: vd
        real(kind=real_wp)   :: depth_factor

        area = iv%get_area()
        iv%etot=max(0.,iv%etot)
        iv%rtot=max(0.,iv%rtot)

        ! convert benthic and pelagic grazer components to units /m2,
        if (iv%switchv1==1) then
            vd = (iv%shape*iv%length_ini)**3                                 !Vd is reference volume (cm3)
            dens = max(((iv%vtot/iv%conv_cm3_gc)/vd),tiny(dens))             !Density derived from Vtot(unit dep on BENTHS)
        else
            dens = max(iv%dens_ini,tiny(dens))                               !Density is dynamic variable
        end if

        if (iv%benths==0) then          ! pelagics (=active substance)
            depth_factor = iv%depth     ! conversion from #/m3 to #/m2
        else                            ! benthics (=inactive substance)
            depth_factor = 1.0          ! no conversion or conversion from #/cell to #/m2
        end if
        dens_m2 = dens * depth_factor
        v_m2 = iv%vtot * depth_factor
        e_m2 = iv%etot * depth_factor
        r_m2 = iv%rtot * depth_factor

        ov%v = max( v_m2 / (dens_m2 * iv%conv_cm3_gc), tiny(ov%v) )
        ov%e = max( e_m2 / (dens_m2 * iv%conv_j_gc),   tiny(ov%e) )
        ov%r = max( r_m2 / (dens_m2 * iv%conv_j_gc),   tiny(ov%r) )

        if (iv%switchv1==1) then
            ov%v=vd                     !to avoid numerical artifacts at small biomasses
        end if

        ov%length = (ov%v**(1/3.0))/iv%shape      !Length is derived from individual V
        ov%e_scaled = ov%e /( iv%em_l3 * ov%v )   !E_scaled is derived from E and V

            ! convert benthic FOOD components to units gC m-2, do not convert pelagic components: unit stays gC m-3
        do ifood = 1,food_count
            if (av%benfood(ifood)==1) then
                av%cfood(ifood)=max(av%cfood(ifood) / area , 0.)
            end if
        end do
    end subroutine rescale_units

    function temperature_dependent_rate(temp, ta, tal, tah, th, tl) result(kt)
        !<-- Calculates a temperature-dependent rate (kT) using Arrhenius equations and additional factors to
        ! describe the temperature sensitivity of biological processes

        real(kind = real_wp), intent(in) :: temp !< temperature
        real(kind = real_wp), intent(in) :: ta   !< arrhenius_temp_K
        real(kind = real_wp), intent(in) :: tal  !< temp_decrease_rate_at_lower_bound
        real(kind = real_wp), intent(in) :: tah  !< temp_decrease_rate_at_upper_bound
        real(kind = real_wp), intent(in) :: th   !< temp_upper_tolerance
        real(kind = real_wp), intent(in) :: tl   !< temp_lower_tolerance

        real(kind = real_wp) :: kt               !< temperature dependent rate

        kt = exp(ta / (20. + 273.) - ta / (temp + 273.)) * &
              (1. + exp(tal / (20. + 273.) - tal / tl) + exp(tah / th - tah / (20. + 273.)))  &
              / (1. + exp(tal / (temp + 273.) - tal / tl) + exp(tah / th - tah / (temp + 273.)))
    end function temperature_dependent_rate

    subroutine calculate_uptake(food_count, is_food_benthic, cfood, fffood, pref, suspension, xk, &
                                minfood, dfil, totaldepth, ccfood, ncfood, pcfood, sicfood, conv_j_gc, &
                                jxm_l2, kappa_i, tim, yk, c_filtr, n_filtr, p_filtr, si_filtr, kT, &
                                v, dens_m2, delt, depth, faecal_fraction, food_pelagic, food_benthic)
        !< Calculates Uptake: filtration, ingestion and assimilation per individual
        !< Effective food concentrations (gC/m3), and their faecal (=indigestible) fractions (faecal_fraction)
        integer(kind=int_wp), intent(in   ) :: food_count           !< Number of food types
        integer(kind=int_wp), intent(in   ) :: is_food_benthic(:)   !< Is food type benthic (1) or not (0)
        real(kind=real_wp),   intent(inout) :: cfood(:)             !<
        real(kind=real_wp),   intent(in   ) :: fffood(:)            !<
        real(kind=real_wp),   intent(in   ) :: ccfood(:)            !<
        real(kind=real_wp),   intent(in   ) :: ncfood(:)            !<
        real(kind=real_wp),   intent(in   ) :: pcfood(:)            !<
        real(kind=real_wp),   intent(in   ) :: sicfood(:)           !<
        real(kind=real_wp),   intent(in   ) :: pref(:)              !<
        real(kind=real_wp),   intent(in   ) :: suspension           !<
        real(kind=real_wp),   intent(in   ) :: xk                   !< Halfrate const food uptake Sup fdr [gC/m3]
        real(kind=real_wp),   intent(in   ) :: minfood              !<
        real(kind=real_wp),   intent(in   ) :: totaldepth           !<
        real(kind=real_wp),   intent(in   ) :: conv_j_gc            !<
        real(kind=real_wp),   intent(in   ) :: jxm_l2               !<
        real(kind=real_wp),   intent(in   ) :: kappa_i              !< Ingestion efficiency (pseudofaeces production) [-]
        real(kind=real_wp),   intent(in   ) :: tim                  !<
        real(kind=real_wp),   intent(in   ) :: yk                   !<
        real(kind=real_wp),   intent(in   ) :: kT                   !<
        real(kind=real_wp),   intent(in   ) :: v                    !< Individual volume (cm3/ind)
        real(kind=real_wp),   intent(in   ) :: dens_m2              !<
        real(kind=real_wp),   intent(in   ) :: delt                 !<
        real(kind=real_wp),   intent(in   ) :: depth                !<

        real(kind=real_wp),   intent(inout) :: dfil(:)              !< Daily filtration rate for each food type [gC/ind/d]

        real(kind=real_wp), intent(  out)   :: c_filtr              !< Daily filtration rate for carbon     [gC/ind/d]
        real(kind=real_wp), intent(  out)   :: n_filtr              !< Daily filtration rate for nitrogen   [gN/ind/d]
        real(kind=real_wp), intent(  out)   :: p_filtr              !< Daily filtration rate for phosphorus [gP/ind/d]
        real(kind=real_wp), intent(  out)   :: si_filtr             !< Daily filtration rate for silicon   [gSi/ind/d]

        real(kind=real_wp), intent(  out)   :: food_pelagic           !< Food for pelagic organisms
        real(kind=real_wp), intent(  out)   :: food_benthic          !< Food for benthic organisms

        ! local vars
        real(kind=real_wp) :: food_suspended                        !< Suspended food
        real(kind=real_wp) :: food_bottom                           !< Food at the bottom of the water column
        real(kind=real_wp) :: faecal_fraction_pelagic               !< Faecal fraction corresponding to pelagic organisms
        real(kind=real_wp) :: faecal_fraction_benthic               !< Faecal fraction corresponding to benthic organisms
        real(kind=real_wp) :: faecal_fraction                       !< Global value for faecal fraction
        real(kind=real_wp) :: xk_suspended                          !< Half saturation for suspended food
        real(kind=real_wp) :: xk_bottom                             !< Half saturation for food in bottom
        real(kind=real_wp) :: dupte                                 !< Daily energy ingestion rate or uptake [J/ind/d]

        integer(kind=int_wp) :: ifood                               !< Index for food type iteration

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
            if (is_food_benthic(ifood)==1) then ! Deposit feeding
                dupte = (1.-suspension)*(cfood(ifood)/(food_benthic+tiny(food_benthic))) &
                        * food_bottom * kT  * (v**(2./3.)) * jxm_l2
                dfil(ifood) = dupte * (conv_j_gc/(1.-faecal_fraction_benthic))*(1./kappa_i)
            else                                ! Suspension feeding
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

    subroutine calculate_defaecation(kappa_i, kappa_a, tn, tp, conv_j_gc, c_filtr, n_filtr, p_filtr, si_filtr, &
                                    faecal_fraction, c_defaec, n_defaec, p_defaec, si_defaec, pa)
        !< Calculates defaecation per individual
        !< From the ingested material, a fraction is lost due to (lack of) assimilation efficiency (kappa_A)
        !< leading to faeces production and/or due to (lack of) ingestion efficiency (kappa_I) leading to
        !< pseudofaeces production. Also, the faecal food fraction (faecal_fraction) is not assimilated; it is assumed to
        !< consist of carbon fibres only, and to be low in energy.
        !< Furthermore the assimilated material has to match the N/C and P/C ratio of the grazer

        real(kind=real_wp), intent(in   ) :: kappa_i            !< Ingestion efficiency (pseudofaeces production)  [-]
        real(kind=real_wp), intent(in   ) :: kappa_a            !< Assimilation efficiency                         [-]
        real(kind=real_wp), intent(in   ) :: tn                 !< N:C ratio grazers                           [gN/gC]
        real(kind=real_wp), intent(in   ) :: tp                 !< P:C ratio grazers                           [gP/gC]
        real(kind=real_wp), intent(in   ) :: conv_j_gc          !< conversion factor from J into gC             [gC/J]
        real(kind=real_wp), intent(in   ) :: c_filtr            !< Daily filtration rate for carbon         [gC/ind/d]
        real(kind=real_wp), intent(in   ) :: n_filtr            !< Daily filtration rate for nitrogen       [gN/ind/d]
        real(kind=real_wp), intent(in   ) :: p_filtr            !< Daily filtration rate for phosphorus     [gP/ind/d]
        real(kind=real_wp), intent(in   ) :: si_filtr           !< Daily filtration rate for silicon       [gSi/ind/d]
        real(kind=real_wp), intent(in   ) :: faecal_fraction    !< Faecal fraction
        real(kind=real_wp), intent(  out) :: c_defaec           !< Daily defaecation rate for carbon        [gC/ind/d]
        real(kind=real_wp), intent(  out) :: n_defaec           !< Daily defaecation rate for nitrogen      [gN/ind/d]
        real(kind=real_wp), intent(  out) :: p_defaec           !< Daily defaecation rate for phosphorus    [gP/ind/d]
        real(kind=real_wp), intent(  out) :: si_defaec          !< Si loss by defaecation in carbon equivalents [gC/d]
        real(kind=real_wp), intent(  out) :: pa                 !< Energy reserves                           [J/ind/d]

        real(kind=real_wp) :: c_uptake                          !< Carbon uptake in (gC/ind/d)
        real(kind=real_wp) :: n_uptake                          !< Nitrogen uptake in carbon equivalents    [gC/ind/d]
        real(kind=real_wp) :: p_uptake                          !< Phosphorus uptake in carbon equivalents  [gC/ind/d]
        real(kind=real_wp) :: lim_uptake                        !< Limiting uptake in carbon equivalents    [gC/ind/d]


        if (c_filtr > 0.) then
            c_uptake = c_filtr  *(kappa_i * kappa_a) * (1.-faecal_fraction)
            n_uptake = n_filtr *(kappa_i * kappa_a) / tn
            p_uptake = p_filtr *(kappa_i * kappa_a) / tp
            lim_uptake = min(n_uptake, p_uptake, c_uptake)

            ! Pseudofaeces, efficiency losses, and excess nutrients are all released as Faeces
            ! All uptake of silicate is lost by defecation
            c_defaec  =  c_filtr  - lim_uptake
            n_defaec = (n_filtr/ tn - lim_uptake) * tn
            p_defaec = (p_filtr/ tp - lim_uptake) * tp
            si_defaec  = si_filtr
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

    subroutine calculate_energy_reserve_dynamics(jxm_l2, kappa_a, eg_l3, em_l3, pm_l3, kappa, delt, kT, v, e, pc)
        !<  Energy reserve dynamics per individual
        !<  Volume specific and theoretically maximum uptake rate
        !<  this is the maximum rate with which energy can be obtained from the energy reserves
        !<  and (being a theoretical maximum) it is not dependent on the actual algae uptake Pa
        real(kind=real_wp), intent(in   ) :: jxm_l2  !<
        real(kind=real_wp), intent(in   ) :: kappa_a !< Assimilation efficiency    [-]
        real(kind=real_wp), intent(in   ) :: eg_l3   !<
        real(kind=real_wp), intent(in   ) :: em_l3   !<
        real(kind=real_wp), intent(in   ) :: pm_l3   !<
        real(kind=real_wp), intent(in   ) :: kappa   !<
        real(kind=real_wp), intent(in   ) :: delt    !<
        real(kind=real_wp), intent(in   ) :: kT      !<
        real(kind=real_wp), intent(in   ) :: v       !<
        real(kind=real_wp), intent(in   ) :: e       !<
        real(kind=real_wp), intent(  out) :: pc      !< (J/ind/d)

        real(kind=real_wp) :: pam_l2                 !<
        real(kind=real_wp) :: e_l3                   !<

        pam_l2 = jxm_l2 * kappa_a * kT
        e_l3 = (e/(v+tiny(v)))
        pc = ((eg_l3 / em_l3) * pam_l2 * v**(2./3.) + pm_l3*v*kT ) * &
                (e_l3 /(kappa*e_l3 + eg_l3))
        pc = min(pc, e/delt)
        pc = max(pc, 0.)
    end subroutine calculate_energy_reserve_dynamics

    subroutine calculate_maintenance(pm_l3, v, kt, pm)
        !< Calculate maintenance per individual
        !< Respiration is only due to basal respiration, not to activity or stress.
        !< Respiration of nutrients is related to the carbon respiration with ratios TN and TP
        real(kind=real_wp), intent(in   ) :: pm_l3  !<
        real(kind=real_wp), intent(in   ) :: v      !<
        real(kind=real_wp), intent(in   ) :: kt     !<
        real(kind=real_wp), intent(  out) :: pm     !< (J/ind/d)

        pm = pm_l3 * v * kt
    end subroutine calculate_maintenance

    subroutine calculate_growth(kappa, pc, pm, pv, pg, conv_cm3_gc, conv_j_gc, eg_l3, delt, v, kappa_g)
        !< Growth per individual:
        !< when growing, energy will be put in the new tissue and some will be lost due to overhead costs
        !< if too little energy catabolized to pay maintenance, the organisms will shrink
        !< in that case, the overhead costs are assumed to be proportional to those for growth
        real(kind=real_wp), intent(in   ) :: kappa          !<
        real(kind=real_wp), intent(in   ) :: pc             !<
        real(kind=real_wp), intent(inout) :: pm             !<
        real(kind=real_wp), intent(  out) :: pv             !<
        real(kind=real_wp), intent(  out) :: pg             !< (J/ind/d)
        real(kind=real_wp), intent(in   ) :: conv_cm3_gc    !<
        real(kind=real_wp), intent(in   ) :: conv_j_gc      !<
        real(kind=real_wp), intent(in   ) :: eg_l3          !<
        real(kind=real_wp), intent(in   ) :: delt           !<
        real(kind=real_wp), intent(in   ) :: v              !<
        real(kind=real_wp), intent(  out) :: kappa_g        !< overhead costs for growth

        pg = kappa*pc - pm
        kappa_g = conv_cm3_gc/(conv_j_gc*eg_l3)

        if (pg > 0.) then
            pv = kappa_g*pg                                                          !(J/ind/d)
        else
            pv = (1.+ (1.-kappa_g))*pg
            pm = pm + abs((1.-kappa_g) * pg)

            pv = max(pv,(-v/delt)*(conv_cm3_gc/conv_j_gc))
            pg = max(pg, (((-v/delt)/(1.+ (1.-kappa_g)))*(conv_cm3_gc/conv_j_gc)))
            pm = min(pm,(kappa*pc + (v/delt)*(conv_cm3_gc/conv_j_gc)))
        end if
    end subroutine calculate_growth

    subroutine calculate_maturity_and_reproduction(switchv1, vp, conv_j_gc, conv_cm3_gc, kappa, pm_l3, &
                                kappar, delt, gsi_upper, gsi_lower, dospawn, temp, minsptemp, &
                                v, e, r, gsi, pjj, pc, prj, pja, pra, pr, dspw, kT, rspawn, dnspw, dpspw, &
                                tn, tp)
        !< Maturity and reproduction per individual
        !< ISO-morphs only produce gonads if they are larger than Vp and if GSI > G_upper
        !< V1-morphs produce gonads with a fraction related to the ratio of V and Vp
        !< Some adjustments were made with respect to original equations to make sure all catabolized energy is
        !< being used
        integer(kind=int_wp), intent(in) :: switchv1 !<

        real(kind=real_wp), intent(in   ) :: vp !<
        real(kind=real_wp), intent(in   ) :: conv_j_gc !<
        real(kind=real_wp), intent(in   ) :: conv_cm3_gc !<
        real(kind=real_wp), intent(in   ) :: kappa !<
        real(kind=real_wp), intent(in   ) :: pm_l3 !<
        real(kind=real_wp), intent(in   ) :: kappar !<
        real(kind=real_wp), intent(in   ) :: delt !<
        real(kind=real_wp), intent(in   ) :: gsi_upper !<
        real(kind=real_wp), intent(in   ) :: gsi_lower !<
        real(kind=real_wp), intent(inout) :: dospawn !<
        real(kind=real_wp), intent(in   ) :: temp !<
        real(kind=real_wp), intent(in   ) :: minsptemp !<
        real(kind=real_wp), intent(in   ) :: v !<
        real(kind=real_wp), intent(in   ) :: e !<
        real(kind=real_wp), intent(in   ) :: r !<
        real(kind=real_wp), intent(  out) :: gsi !<
        real(kind=real_wp), intent(  out) :: pjj !<
        real(kind=real_wp), intent(in   ) :: pc !<
        real(kind=real_wp), intent(  out) :: prj !<
        real(kind=real_wp), intent(  out) :: pja !<
        real(kind=real_wp), intent(  out) :: pra !<
        real(kind=real_wp), intent(  out) :: pr !<
        real(kind=real_wp), intent(  out) :: dspw !<
        real(kind=real_wp), intent(in   ) :: kT !<
        real(kind=real_wp), intent(in   ) :: rspawn !<
        real(kind=real_wp), intent(  out) :: dnspw !<
        real(kind=real_wp), intent(  out) :: dpspw !<
        real(kind=real_wp), intent(in   ) :: tn !<
        real(kind=real_wp), intent(in   ) :: tp !<

        real(kind=real_wp) :: fadult
        real(kind=real_wp) :: fjuv


        if (switchv1==1) then
            fjuv= vp/(vp+v)                                                 !juvenile fraction of the population
            fadult= 1.-vp/(vp+v)                                            !adult fraction of the population
            gsi = (r*conv_j_gc)/(v*fadult*conv_cm3_gc + &
                    e*fadult*conv_j_gc + r*conv_j_gc)
        elseif (v < vp) then
            fjuv=1.
            fadult=0.
            gsi = 0.
        else
            fjuv=0.
            fadult=1.
            gsi = (r*conv_j_gc)/(v*fadult*conv_cm3_gc + &
                    e*fadult*conv_j_gc + r*conv_j_gc)
        end if

        pjj = ((1.-kappa)/(kappa+tiny(kappa))) * pm_l3 * (v*fjuv) * kT    !maturity maintenance juveniles (J/ind/d)
        pjj = min(pjj, ((1.-kappa) * pc * fjuv ))
        prj = max(0., ((1.-kappa) * pc * fjuv - pjj))                    !remainder goes to maturity development
        pja = ((1.-kappa)/(kappa+tiny(kappa)))* pm_l3 * (vp*fadult) * kT  !maturity maintenance adults (J/ind/d)
        pra = (1.-kappa) * pc * fadult - pja                             !remainder goes to reproduction flux

        ! if too little energy for juv mat maint and dev, these processes simply stop
        ! but if too little energy for adult mat maint, costs are paid by R with additional overhead costs
        ! proportional to kappaR:
        if (pra > 0.) then
            pr = kappar*pra                                                          !(J/ind/d)
        else
            pr = (1.+(1.-kappar))*pra
            pja = pja + abs((1.-kappar) * pra)

            pr = max(pr,(-r/delt))
            pra = max(pra, (((-r/delt)/(1.+(1.-kappar)))))
            pja = min(pja,((1.-kappa)*pc*fadult + (r/delt)))
        end if

        ! if conditions are suitable, spawning will start
        if (gsi > gsi_upper) then
            dospawn = 1.
        end if

        ! spawning continues as long as conditions remain suitable
        if (dospawn > 0) then
            if (( gsi > gsi_lower .and. temp > minsptemp ) .and. r > 0 ) then

                dspw = (rspawn * r + max(pr,0.))                                    !(J/ind/d)
                dspw = min(dspw,(r/delt+min(pr,0.)))
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
    end subroutine calculate_maturity_and_reproduction

    subroutine calculate_respiration(pm, pja, pjj, prj, dres, dnres, dpres, kappa_g, pg, pra, kappar, tn, tp)
        !< Respiration per individual
        !< last two terms refer to overhead costs of growth and reproduction
        real(kind=real_wp), intent(in   ) :: pm !<
        real(kind=real_wp), intent(in   ) :: pja !<
        real(kind=real_wp), intent(in   ) :: pjj !<
        real(kind=real_wp), intent(in   ) :: prj !<
        real(kind=real_wp), intent(  out) :: dres !<
        real(kind=real_wp), intent(  out) :: dnres !<
        real(kind=real_wp), intent(  out) :: dpres !<
        real(kind=real_wp), intent(in   ) :: kappa_g !<
        real(kind=real_wp), intent(in   ) :: pg !<
        real(kind=real_wp), intent(in   ) :: pra !<
        real(kind=real_wp), intent(in   ) :: kappar !<
        real(kind=real_wp), intent(in   ) :: tn !<
        real(kind=real_wp), intent(in   ) :: tp !<

        real(kind=real_wp) :: overhead_costs_growth !<
        real(kind=real_wp) :: overhead_costs_reproduction !<

        overhead_costs_growth =       (1.-kappa_g) * max(pg, 0.)
        overhead_costs_reproduction = (1.-kappar)  * max(pra,0.)
        dres =pm+pja+pjj+prj+(1.-kappa_g) * max(pg, 0.)+(1.-kappar)  * max(pra,0.)
        dnres = dres * tn
        dpres = dres * tp
    end subroutine calculate_respiration

    subroutine calculate_shell_formation_fluxes(pv, frgsmo, frsmosmi, pomm, pca)
        !< Shell formation fluxes per individual
        real(kind=real_wp), intent(in   ) :: pv  !<
        real(kind=real_wp), intent(in   ) :: frgsmo !< fraction of growth flux to shell matrix        (-)
        real(kind=real_wp), intent(in   ) :: frsmosmi !< fraction of shell matrix flux to calcification (-)
        real(kind=real_wp), intent(  out) :: pomm !< energy flux to organic shell matrix [J/ind/d])
        real(kind=real_wp), intent(  out) :: pca !< energy flux to calcification of shell matrix [J/ind/d])

        pomm =  max(0., (pv   * frgsmo))
        pca  =  max(0., (pomm * frsmosmi))
    end subroutine calculate_shell_formation_fluxes

    subroutine calculate_mortality(rmor_ref, cmor, conv_j_gc, conv_cm3_gc, rhrv_ref, chrv, tn, tp, &
                        length, v, e, r, rmor, rhrv, dmor, dnmor, dpmor, kt, pv)
        ! Natural mortality and harvesting (only former comes back into the system as detritus)
        ! These added fractions cannot be larger than one (minus the material used for maintenance, at Pv<0)

        real(kind=real_wp), intent(in   ) :: rmor_ref !<
        real(kind=real_wp), intent(in   ) :: cmor !<
        real(kind=real_wp), intent(in   ) :: conv_j_gc !<
        real(kind=real_wp), intent(in   ) :: conv_cm3_gc !<
        real(kind=real_wp), intent(in   ) :: rhrv_ref !<
        real(kind=real_wp), intent(in   ) :: chrv !<
        real(kind=real_wp), intent(in   ) :: tn !<
        real(kind=real_wp), intent(in   ) :: tp !<
        real(kind=real_wp), intent(in   ) :: length !<
        real(kind=real_wp), intent(in   ) :: v !<
        real(kind=real_wp), intent(in   ) :: e !<
        real(kind=real_wp), intent(in   ) :: r !<
        real(kind=real_wp), intent(in   ) :: kt !<
        real(kind=real_wp), intent(in   ) :: pv !<
        real(kind=real_wp), intent(  out) :: rmor !<
        real(kind=real_wp), intent(  out) :: rhrv !<
        real(kind=real_wp), intent(  out) :: dmor !<
        real(kind=real_wp), intent(  out) :: dnmor !<
        real(kind=real_wp), intent(  out) :: dpmor !<

        rmor  = rmor_ref * (length**cmor) * kt
        rmor  = min(rmor,(1.+(min(pv,0.)*conv_j_gc)/(v*conv_cm3_gc)))
        rhrv  = rhrv_ref * (length**chrv)
        rhrv  = min(rhrv,(1.-rmor+(min(pv,0.)*conv_j_gc)/(v*conv_cm3_gc)))

        dmor  = rmor *(v*conv_cm3_gc+(e+r)*conv_j_gc)                             !(gC/ind/d)
        dnmor = dmor * tn
        dpmor = dmor * tp
    end subroutine calculate_mortality

end module m_debgrz_computations
