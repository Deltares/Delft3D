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
module m_debgrz
    use m_monsys
    use m_srstop
    use m_waq_precision
    use m_debgrz_computations, only: set_maximum_conversion_coeff, rescale_units, temperature_dependent_rate, &
            calculate_uptake, calculate_defaecation, calculate_energy_reserve_dynamics, &
            calculate_maintenance, calculate_growth, calculate_maturity_and_reproduction, &
            calculate_respiration, calculate_shell_formation_fluxes, calculate_mortality
    use m_debgrz_input
    use m_debgrz_auxiliary
    use m_debgrz_output

    implicit none

    private
    public :: debgrz

    type :: process_variables
        type(debgrz_input)     :: inp
        type(debgrz_output)    :: outp
        type(debgrz_auxiliary) :: aux
        integer(kind=int_wp)      :: pointers_count !< Number of pointers, equal to the number of input and output parameters.
        integer(kind=int_wp)      :: input_count    !< Number of inut parameters
        integer(kind=int_wp)      :: food_count     !< Number of food types
    end type process_variables
    contains

    subroutine debgrz(pmsa , fl , ipoint , increm , noseg , noflux , &
                      iexpnt, iknmrk, ipodim, noq1, noq2, noq3, noq4)
        !< General routine for the dynamics of a grazer based on DEB theory.
        !< The modeled grazers can form either a cohort of equal organisms of
        !< increasing length (isomorphs) or a simplified population of individuals
        !< with an overall fixed size distribution (V1 morphs).
        !< Furthermore, the organisms can be either non-mobile or passively
        !< transported with the water.
        !< The organisms can consume various (pelagic and benthic) food types,
        !< including dynamo and bloom algae and various detritus fractions
        !< (DetX, POX and DetXS1). The consumer has a specific preference
        !< for each food type.

       ! subroutine arguments
        integer(kind=int_wp), dimension(*  ) :: ipoint
        integer(kind=int_wp), dimension(*  ) :: increm
        integer(kind=int_wp), dimension(*  ) :: iknmrk
        integer(kind=int_wp), dimension(4,*) :: iexpnt
        integer(kind=int_wp) :: noseg
        integer(kind=int_wp) :: noflux
        integer(kind=int_wp) :: ipodim
        integer(kind=int_wp) :: noq1
        integer(kind=int_wp) :: noq2
        integer(kind=int_wp) :: noq3
        integer(kind=int_wp) :: noq4

        real(kind=real_wp), dimension(*) :: pmsa
        real(kind=real_wp), dimension(*) :: fl
       ! subroutine arguments

       ! local variables
        logical, save :: first_run = .true.
        integer(kind=int_wp), dimension(:), allocatable :: ip !< index pointer in pmsa array updated for each segment
        integer(kind=int_wp) :: iflux
        integer(kind=int_wp) :: iseg
        integer(kind=int_wp) :: lunrep

        type(process_variables) :: process_vars
       ! local variables

        if (.not. process_may_be_run(pmsa, ipoint)) then
            call getmlu(lunrep)
            write(lunrep,*) 'ERROR: DEB in combination with Protist AND Dynamo/Bloom is not allowed.'
            call srstop(1)
        end if
        call initialize_variables(ip, ipoint, iflux, process_vars, (pmsa(ipoint(1))/=0))

        if (first_run) then
            call remove_floating_benthic_species(pmsa, ip, noseg, iknmrk, fl, iflux, noflux)
            first_run = .false.
        end if
        do iseg = 1, noseg
            if (must_calculate_cell(iknmrk(iseg), pmsa, ip)) then
                call process_vars%inp%initialize(ip, pmsa)

                call calculate_process_in_segment(process_vars, pmsa, ip, fl, iflux)

                call process_vars%outp%update_pmsa(pmsa, ip, process_vars%input_count)
            end if
            call update_loop_vars(iflux, noflux, process_vars, pmsa, ip, increm)
        end do
        return
    end subroutine debgrz

    logical function process_may_be_run(pmsa, ipoint) result(res)
        !< Make sure that the process may be run, stop simulation otherwise.
        !< Prevents that simulation uses both Protist AND Dynamo/Bloom in different "processes" (=instances of debgrz).
        !< This is not allowed due to biological reasons.
        real(kind=real_wp), intent(in)   :: pmsa(*)
        integer(kind=int_wp), intent(in) :: ipoint(*)

        logical, save :: first_run_validation = .true.
        logical, save :: previous_process_uses_protist
        logical       :: current_process_uses_protist

        current_process_uses_protist = (nint(pmsa(ipoint(1))) == 1 )
        if (first_run_validation) then
            previous_process_uses_protist = current_process_uses_protist
            first_run_validation = .false.
            res = .true.
            return
        end if
        res = (current_process_uses_protist .eqv. previous_process_uses_protist)
    end function process_may_be_run

    logical function must_calculate_cell(segment_attribute, pmsa, iparray) result(must_calculate)
        !< Boolean indicating whether the calculation for current cell (segment) should
        !< be carried out or not. If false, then the cell is skipped.

        use m_evaluate_waq_attribute, only : evaluate_waq_attribute

        real(kind=real_wp), intent(in)   :: pmsa(*)
        integer(kind=int_wp), intent(in) :: segment_attribute
        integer(kind=int_wp), intent(in) :: iparray(*)

        ! local
        integer(kind=int_wp) :: benthic_species_index !<  benthic_species_index
        real(kind=real_wp)   :: vtot
        integer(kind=int_wp) :: atrribute_active

        must_calculate = .false.
        call evaluate_waq_attribute(1,segment_attribute,atrribute_active)
        if (atrribute_active==1) then
            benthic_species_index = nint(pmsa(iparray(10)))
            ! pelagics can occur everywhere, but benthic grazers can only exist in bottom layer
            must_calculate = (.not. is_floating_benthic_species(segment_attribute, benthic_species_index))
            if (must_calculate) then
                vtot = pmsa( iparray(11))
                must_calculate = (vtot>tiny(vtot))
            end if
        end if
    end function must_calculate_cell

    logical function is_floating_benthic_species(cell_attributes, is_benthic) result(res)
        use m_evaluate_waq_attribute, only : evaluate_waq_attribute
        integer(kind=int_wp), intent(in) ::   cell_attributes     !< attibutes of the currernt cell
        integer(kind=int_wp), intent(in) ::   is_benthic          !< indicates if a species is benthic (1) or not (0 = pelagic)

        ! locals
        integer(kind=int_wp) :: attribute_location

        call evaluate_waq_attribute(2, cell_attributes, attribute_location)
        res = ( (attribute_location == 1 .OR. attribute_location == 2 ) .AND. is_benthic.eq.1 )
    end function is_floating_benthic_species

    subroutine set_counters(use_with_protist, input_count, food_count, pointers_count)
        !<-- Set number of parameters depending on usage with Protist or Dynamo/Bloom
        logical, intent(in)  :: use_with_protist !< indicates if the process uses protists (1) or not (0)
        integer(kind=int_wp), intent(out) :: input_count, food_count, pointers_count

        if (use_with_protist) then
            input_count = 158
            food_count = 10 + 2             ! 2 diatoms, 2 greens, 2 constitutive and 2 non-constitutive mixoplankton types, 2 zooplankton types and 2 detritus types
        else
            input_count = 358
            food_count = 40 + 2             ! 30 BLOOM algae + 2 DYNAMO algae + 8 dummy food sources + DetCS1 + Detritus = 40+2
        end if
        pointers_count = input_count + 22   ! number of input output variables in PMSA array = 56 + 3 * ntotnut + 7 * (nfood - 2) + 21 output vars
    end subroutine set_counters

    subroutine remove_floating_benthic_species(pmsa, ip, cell_count, iknmrk, fl, iflux, noflux)
        !< If benthic species are present in any non-bottom cell (floaters), then this subroutine removes them
        !< by setting the (out)flow exactly equal to the current amount.
        !< In the next time iteration, once the flux has been applied, they will be exactly equal to zero.
        use m_evaluate_waq_attribute, only : evaluate_waq_attribute
        integer(kind=int_wp), intent(in)    :: ip(:), cell_count, iknmrk(*), noflux
        integer(kind=int_wp), intent(inout) :: iflux
        real(kind=real_wp), intent(in)      :: pmsa(*)
        real(kind=real_wp), intent(inout)   :: fl(*)

        real(kind=real_wp)                  :: vtot, etot, rtot, depth
        integer(kind=int_wp)                :: benthic_species_index, i_cell

        do i_cell = 1, cell_count
            benthic_species_index = nint(pmsa(ip(10)))
            if (is_floating_benthic_species(iknmrk(i_cell), benthic_species_index)) then
                depth = pmsa(ip(5))
                vtot = pmsa(ip(11))
                etot = pmsa(ip(12))
                rtot = pmsa(ip(13))
                fl(18 + iflux) = vtot / real(depth)   ! mortality (Vtot [gC/m3/d])
                fl(21 + iflux) = etot / real(depth)   ! mortality (Etot [gC/m3/d])
                fl(23 + iflux) = rtot / real(depth)   ! mortality (Rtot [gC/m3/d])
            end if
            iflux = iflux + noflux
        end do
        iflux = 0
    end subroutine remove_floating_benthic_species

    subroutine initialize_variables(iparray, ipoint, iflux, p_vars, use_with_protist)
        !< Initializes arrays and other variables.

        integer(kind=int_wp), intent(inout) :: iflux
        integer(kind=int_wp), allocatable, intent(out) :: iparray(:)
        integer(kind=int_wp), intent(in) :: ipoint(*)
        logical :: use_with_protist !< process is used with protist

        type(process_variables), intent(inout) :: p_vars

        integer(kind=int_wp) :: pointers_count !< Number of pointers in iparray = input_count + output_count
        integer(kind=int_wp) :: input_count    !< Number of input parameters
        integer(kind=int_wp) :: food_count     !< Number of food types

        call set_counters(use_with_protist, input_count, food_count, pointers_count)

        allocate(iparray(1:pointers_count))
        call p_vars%inp%allocate_food_arrays(food_count)
        call p_vars%aux%allocate_food_arrays(food_count)

        iparray(:) = ipoint(1:pointers_count)
        iflux = 0

        p_vars%pointers_count = pointers_count
        p_vars%input_count    = input_count
        p_vars%food_count     = food_count
    end subroutine initialize_variables

    subroutine calculate_process_in_segment(process_vars, pmsa, ip, fl, iflux)
        type(process_variables), intent(inout) :: process_vars
        real(kind=real_wp), intent(inout)      :: pmsa(*)
        integer(kind=int_wp), intent(in)    :: ip(:)
        integer(kind=int_wp), intent(in) :: iflux !<
        real(kind=real_wp), dimension(*), intent(inout) :: fl

       ! internal variables
        type(debgrz_input) :: iv
        type(debgrz_auxiliary) :: av
        type(debgrz_output) :: ov

        integer(kind=int_wp), parameter :: nqfood  = 8
        integer(kind=int_wp), parameter :: ntotnut = 4 ! carbon, nitrogen, phosphorus and silica
        integer(kind=int_wp), save  :: food_count
        integer(kind=int_wp) :: lunrep

        real(kind=real_wp) :: dens          !< number of grazer individuals        (#/m3 or #/m2)
        real(kind=real_wp) :: rhrv
        real(kind=real_wp) :: rmor
        real(kind=real_wp) :: dens_m2
        real(kind=real_wp) :: v_m2          !< population structural biomass (gC/m2)
        real(kind=real_wp) :: e_m2          !< population energy biomass (gC/m2)
        real(kind=real_wp) :: r_m2          !< population gonadal (reproductive) biomass (gC/m2)
        real(kind=real_wp) :: kT
        real(kind=real_wp) :: filtr
        real(kind=real_wp) :: nfiltr
        real(kind=real_wp) :: pfiltr
        real(kind=real_wp) :: sifiltr
        real(kind=real_wp)  :: faecal_fraction
        real(kind=real_wp) :: ddef
        real(kind=real_wp) :: dndef
        real(kind=real_wp) :: dpdef
        real(kind=real_wp) :: dsidef
        real(kind=real_wp) :: pa
        real(kind=real_wp) :: pc
        real(kind=real_wp) :: pv
        real(kind=real_wp) :: pg
        real(kind=real_wp) :: pm
        real(kind=real_wp) :: pr
        real(kind=real_wp) :: kappa_g       !< overhead costs for growth
        real(kind=real_wp) :: pjj
        real(kind=real_wp) :: prj
        real(kind=real_wp) :: pja
        real(kind=real_wp) :: pra
        real(kind=real_wp) :: pca           !< energy flux to calcification of shell matrix [J/ind/d])
        real(kind=real_wp) :: dnspw
        real(kind=real_wp) :: dspw
        real(kind=real_wp) :: dpspw
        real(kind=real_wp) :: dres
        real(kind=real_wp) :: dnres
        real(kind=real_wp) :: dpres
        real(kind=real_wp) :: dmor
        real(kind=real_wp) :: dnmor
        real(kind=real_wp) :: dpmor
        real(kind=real_wp) :: pomm          !< energy flux to organic shell matrix [J/ind/d])
        real(kind=real_wp) :: food_pelagic  !< Food for pelagic organisms
        real(kind=real_wp) :: food_benthic  !< Food for benthic organisms
       ! internal variables

        iv = process_vars%inp
        ov = process_vars%outp
        av = process_vars%aux

        call assign_food_arrays(pmsa, ip, av%cfood, av%ccfood, av%chlcfood, av%ncfood, av%pcfood, &
                            av%sicfood, iv%pref, av%benfood, iv%fffood, iv%detbio, process_vars%food_count, &
                            iv%use_with_protist, nqfood, iv%dets1)

        call set_maximum_conversion_coeff(iv%conv_cm3_gc, iv%conv_j_gc, iv%eg_l3)
        call rescale_units(iv, ov, av, dens, dens_m2, v_m2, e_m2, r_m2, process_vars%food_count)
        kT = temperature_dependent_rate(iv%temp, iv%ta, iv%tal, iv%tah, iv%th, iv%tl)
        call calculate_uptake(&
                            process_vars%food_count, av%benfood, av%cfood, iv%fffood, iv%pref, &
                            iv%suspension, iv%xk, iv%minfood, av%dfil, iv%totaldepth, &
                            av%ccfood, av%ncfood, av%pcfood, av%sicfood, iv%conv_j_gc, &
                            iv%jxm_l2, iv%kappai, iv%tim, iv%yk, filtr, nfiltr, pfiltr, &
                            sifiltr, kT, ov%v, dens_m2, iv%delt, iv%depth, faecal_fraction, &
                            food_pelagic, food_benthic)

        call calculate_defaecation(&
                            iv%kappai, iv%kappaa, iv%tn, iv%tp, iv%conv_j_gc, filtr, &
                            nfiltr, pfiltr, sifiltr, faecal_fraction, &
                            ddef, dndef, dpdef, dsidef, pa)
        call calculate_energy_reserve_dynamics(&
                            iv%jxm_l2, iv%kappaa, iv%eg_l3, iv%em_l3, iv%pm_l3, &
                            iv%kappa, iv%delt, kT, ov%v, ov%e, pc)
        call calculate_maintenance(iv%pm_l3, ov%v, kt, pm)
        call calculate_growth(&
                            iv%kappa, pc, pm, pv, pg, iv%conv_cm3_gc, iv%conv_j_gc, &
                            iv%eg_l3, iv%delt, ov%v, kappa_g)
        call calculate_maturity_and_reproduction(&
                            iv%switchv1, iv%vp, iv%conv_j_gc, iv%conv_cm3_gc, &
                            iv%kappa, iv%pm_l3, iv%kappar, iv%delt, iv%gsi_upper, iv%gsi_lower,   &
                            iv%dospawn, iv%temp, iv%minsptemp, ov%v, ov%e, ov%r, ov%gsi, pjj, pc, &
                            prj, pja, pra, pr, dspw, kT, iv%rspawn, dnspw, dpspw, &
                            iv%tn, iv%tp)
        call calculate_respiration(&
                            pm, pja, pjj, prj, dres, dnres, dpres, kappa_g, &
                            pg, pra, iv%kappar, iv%tn, iv%tp)
        call calculate_shell_formation_fluxes(pv, iv%frgsmo, iv%frsmosmi, pomm, pca)
        call calculate_mortality(&
                            iv%rmor_ref, iv%cmor, iv%conv_j_gc, iv%conv_cm3_gc, iv%rhrv_ref, iv%chrv, &
                            iv%tn, iv%tp, ov%length, ov%v, ov%e, ov%r, rmor, rhrv, dmor, dnmor, dpmor, kT, pv)

        call assign_fluxes(&
                            fl, iflux, iv%depth, dens_m2, iv%frdetbot, &
                            dres, dspw, rmor, rhrv, pa, &
                            ov%v, ov%e, ov%r, &
                            iv%conv_cm3_gc, iv%conv_j_gc, iv%switchv1, &
                            iv%egsmo, iv%egsmi, iv%cso_cm3_gc, iv%csi_cm3_gc, &
                            iv%gem, iv%use_with_protist,&
                            av%dfil, av%ncfood, av%pcfood, av%sicfood, av%chlcfood, &
                            process_vars%food_count, dnspw, dpspw, ddef, dndef, &
                            dpdef, dsidef, dnres, dpres, dmor, dnmor, &
                            dpmor, pv, pomm, pc, pr, pca)

        call calculate_output_vars(&
                            ov, iv, fl, iflux, dens_m2, &
                            iv%depth, iv%benths, iv%tn, iv%tp, &
                            rmor, iv%conv_gww_gc, iv%conv_j_gc, iv%conv_gafdw_gc, &
                            rhrv, dspw, pa, dres, filtr, nfiltr, pfiltr, sifiltr, &
                            food_pelagic, food_benthic, v_m2, e_m2, r_m2)
        process_vars%inp  = iv
        process_vars%outp = ov
        process_vars%aux  = av
    end subroutine calculate_process_in_segment

    subroutine assign_fluxes(fl, iflux, depth, dens_m2, frdetbot, &
                             dres, dspw, rmor, rhrv, pa, &
                             v, e, r, &
                             conv_cm3_gc, conv_j_gc, switchv1, &
                             egsmo, egsmi, cso_cm3_gc, csi_cm3_gc, &
                             gem, use_with_protist, &
                             dfil, ncfood, pcfood, sicfood, chlcfood, &
                             food_count, dnspw, dpspw, ddef, dndef, &
                             dpdef, dsidef, dnres, dpres, dmor, dnmor, &
                             dpmor, pv, pomm, pc, pr, pca &
                             )

        real(kind=real_wp), dimension(*), intent(inout) :: fl
        integer(kind=int_wp), intent(in) :: iflux
        real(kind=real_wp), intent(in) :: depth
        real(kind=real_wp), intent(in) :: dens_m2
        real(kind=real_wp), intent(in) :: frdetbot

        real(kind=real_wp), intent(in) :: dres
        real(kind=real_wp), intent(in) :: dspw
        real(kind=real_wp), intent(in) :: rmor
        real(kind=real_wp), intent(in) :: rhrv

        real(kind=real_wp), intent(in) :: dnspw
        real(kind=real_wp), intent(in) :: dpspw
        real(kind=real_wp), intent(in) :: ddef
        real(kind=real_wp), intent(in) :: dndef
        real(kind=real_wp), intent(in) :: dpdef
        real(kind=real_wp), intent(in) :: dsidef
        real(kind=real_wp), intent(in) :: dnres
        real(kind=real_wp), intent(in) :: dpres
        real(kind=real_wp), intent(in) :: dmor
        real(kind=real_wp), intent(in) :: dnmor
        real(kind=real_wp), intent(in) :: dpmor
        real(kind=real_wp), intent(in) :: pv
        real(kind=real_wp), intent(in) :: pomm
        real(kind=real_wp), intent(in) :: pc
        real(kind=real_wp), intent(in) :: pr
        real(kind=real_wp), intent(in) :: pca

        real(kind=real_wp), intent(in) :: v
        real(kind=real_wp), intent(in) :: e
        real(kind=real_wp), intent(in) :: r

        real(kind=real_wp), intent(in) :: conv_cm3_gc
        real(kind=real_wp), intent(in) :: conv_j_gc
        integer(kind=int_wp), intent(in) :: switchv1

        real(kind=real_wp), intent(in) :: pa

        real(kind=real_wp), intent(in) :: egsmo
        real(kind=real_wp), intent(in) :: egsmi
        real(kind=real_wp), intent(in) :: cso_cm3_gc
        real(kind=real_wp), intent(in) :: csi_cm3_gc

        real(kind=real_wp), intent(in) :: gem
        real(kind=real_wp), dimension(:), intent(in) :: dfil

        real(kind=real_wp), dimension(:), intent(in) :: ncfood
        real(kind=real_wp), dimension(:), intent(in) :: pcfood
        real(kind=real_wp), dimension(:), intent(in) :: sicfood
        real(kind=real_wp), dimension(:), intent(in) :: chlcfood

        integer(kind=int_wp), intent(in) :: use_with_protist
        integer(kind=int_wp), intent(in) :: food_count

        ! local
        integer(kind=int_wp):: ifood
        integer(kind=int_wp):: offsset
        real(kind=real_wp) :: dens !< Density [/m3] = dens_m2 [/m2] / depth [m]

        !********************************************************************
        ! Fluxes in units of gX/ind/d converted to gX/m3/d for WAQ
        !
        dens = dens_m2 /depth
        fl ( 1 + iflux  ) = dmor*frdetbot*dens                      !mortality to detritus sediment [gC/m3/d]
        fl ( 2 + iflux  ) = dnmor*frdetbot*dens                     !mortality to detritus sediment [gN/m3/d]
        fl ( 3 + iflux  ) = dpmor*frdetbot*dens                     !mortality to detritus sediment [gP/m3/d]
        fl ( 4 + iflux  ) = dmor*(1.-frdetbot)*dens                 !mortality to detritus [gC/m3/d]
        fl ( 5 + iflux  ) = dnmor*(1.-frdetbot)*dens                !mortality to detritus [gN/m3/d]
        fl ( 6 + iflux  ) = dpmor*(1.-frdetbot)*dens                !mortality to detritus [gP/m3/d]
        fl ( 7 + iflux  ) = dres * conv_j_gc*dens                   !respiration [gC/m3/d]
        fl ( 8 + iflux  ) = dnres* conv_j_gc*dens                   !respiration [gN/m3/d]
        fl ( 9 + iflux  ) = dpres* conv_j_gc*dens                   !respiration [gP/m3/d]
        fl (10 + iflux  ) = ddef*dens                               !defecation [gC/m3/d]
        fl (11 + iflux  ) = dndef*dens                              !defecation [gN/m3/d]
        fl (12 + iflux  ) = dpdef*dens                              !defecation [gP/m3/d]
        fl (13 + iflux  ) = dsidef*dens                             !defecation [gSi/m3/d]
        fl (14 + iflux  ) = dspw * conv_j_gc*dens                   !spawning to detritus [gC/m3/d]
        fl (15 + iflux  ) = dnspw* conv_j_gc*dens                   !spawning to detritus [gN/m3/d]
        fl (16 + iflux  ) = dpspw* conv_j_gc*dens                   !spawning to detritus [gP/m3/d]
        fl (17 + iflux  ) = ((pv-pomm)*conv_j_gc) *dens             !growth    (Vtot [gC/m3/d])
        fl (18 + iflux  ) = (rmor+rhrv)*v*conv_cm3_gc *dens         !mortality (Vtot [gC/m3/d])
        fl (19 + iflux  ) = (pa * conv_j_gc )*dens                  !anabolic  (Etot [gC/m3/d])
        fl (20 + iflux  ) = (pc * conv_j_gc )*dens                  !catabolic (Etot [gC/m3/d])
        fl (21 + iflux  ) = (rmor+rhrv)* e * conv_j_gc *dens        !mortality (Etot [gC/m3/d])
        fl (22 + iflux  ) = (pr*conv_j_gc) *dens                    !growth    (Rtot [gC/m3/d])
        fl (23 + iflux  ) = (rmor+rhrv)* r * conv_j_gc *dens        !mortality (Rtot [gC/m3/d])
        if (switchv1==0) then                                                !if iso-morph
            ! iso-morphs only grow in V, not in density
            fl (24 + iflux  ) = 0.                                              !growth    (Dens [#/m3/d])
        else                                                                   !if V1-morph
            ! V1-morphs have a constant (individual) V, the population V only grows through increase in density
            fl (24 + iflux  ) =((pv*conv_j_gc/conv_cm3_gc)/v)*dens      !growth    (Dens [#/m3/d])
        end if
        fl (25 + iflux  ) = min(1.,(rmor + rhrv))*dens               !mortality (Dens [#/m3/d])
        ! shell fluxes (if taken into account) remain part of the structural volume
        ! hence, their (explicit) presence does thus not change any other fluxes
        fl (26 + iflux  ) =  ((pomm-pca)/egsmo)*cso_cm3_gc * dens   ! change in organic shell matrix [gC/m3/d])
        fl (27 + iflux  ) =  (pca/egsmi) * csi_cm3_gc * dens       ! calcification of shell matrix [gC/m3/d])


        fl (28 + iflux  ) = (0.+ gem)*dfil(1) * dens                 !uptake    [gC/m3/d]
        fl (29 + iflux  ) = (1.- gem)*dfil(1) * dens
        fl (30 + iflux  ) =           dfil(2) * dens
        fl (31 + iflux  ) = (0.+ gem)*dfil(1) * ncfood(1) * dens
        fl (32 + iflux  ) = (1.- gem)*dfil(1) * ncfood(1) * dens
        fl (33 + iflux  ) =           dfil(2) * ncfood(2) * dens
        fl (34 + iflux  ) = (0.+ gem)*dfil(1) * pcfood(1) * dens
        fl (35 + iflux  ) = (1.- gem)*dfil(1) * pcfood(1) * dens
        fl (36 + iflux  ) =           dfil(2) * pcfood(2) * dens
        fl (37 + iflux  ) = (0.+ gem)*dfil(1) * sicfood(1)* dens
        fl (38 + iflux  ) = (1.- gem)*dfil(1) * sicfood(1)* dens
        fl (39 + iflux  ) =           dfil(2) * sicfood(2)* dens

        if (use_with_protist == 1) then
            ! Five fluxes per food source - correct with the nutrient contents
            !
            do ifood=3,food_count
                offsset = 39 + 5 * (ifood - 3 ) + iflux
                fl(offsset + 1) = dfil(ifood)                   * dens
                fl(offsset + 2) = dfil(ifood) * chlcfood(ifood) * dens
                fl(offsset + 3) = dfil(ifood) * ncfood(ifood)   * dens
                fl(offsset + 4) = dfil(ifood) * pcfood(ifood)   * dens
                fl(offsset + 5) = dfil(ifood) * sicfood(ifood)  * dens
            end do
        else
           do ifood=3,food_count
                fl(37 + ifood + iflux) = dfil(ifood) * dens
           end do
        end if
    end subroutine assign_fluxes

    subroutine calculate_output_vars(ov, iv, fl, iflux, &
                                     dens_m2, depth, benths, &
                                     tn, tp, &
                                     rmor, conv_gww_gc, conv_j_gc, conv_gafdw_gc, &
                                     rhrv, dspw, pa, dres, filtr, nfiltr, pfiltr, sifiltr, &
                                     food_pelagic, food_benthic, v_m2, e_m2, r_m2)

       ! arguments
        type(debgrz_output), intent(inout) :: ov !< output variables to set
        type(debgrz_input), intent(in) :: iv
        real(kind=real_wp), dimension(*), intent(in) :: fl
        integer(kind=int_wp), intent(in) :: iflux

        real(kind=real_wp), intent(in) :: dens_m2
        real(kind=real_wp), intent(in) :: depth
        integer(kind=int_wp), intent(in) :: benths

        real(kind=real_wp), intent(in) :: tn
        real(kind=real_wp), intent(in) :: tp

        real(kind=real_wp), intent(in) :: rmor
        real(kind=real_wp), intent(in) :: conv_gww_gc
        real(kind=real_wp), intent(in) :: conv_j_gc
        real(kind=real_wp), intent(in) :: conv_gafdw_gc

        real(kind=real_wp), intent(in) :: rhrv
        real(kind=real_wp), intent(in) :: dspw
        real(kind=real_wp), intent(in) :: pa
        real(kind=real_wp), intent(in) :: dres
        real(kind=real_wp), intent(in) :: filtr
        real(kind=real_wp), intent(in) :: nfiltr
        real(kind=real_wp), intent(in) :: pfiltr
        real(kind=real_wp), intent(in) :: sifiltr
        real(kind=real_wp), intent(in) :: food_pelagic              !< Food for pelagic organisms
        real(kind=real_wp), intent(in) :: food_benthic             !< Food for benthic organisms
        real(kind=real_wp), intent(in)  :: v_m2
        real(kind=real_wp), intent(in)  :: e_m2
        real(kind=real_wp), intent(in)  :: r_m2
       ! arguments

       ! local variables
        real(kind=real_wp) :: c_in      !< Inwards  flow of carbon (C)
        real(kind=real_wp) :: c_out     !< Outwards flow of carbon (C)
        real(kind=real_wp) :: n_in      !< Inwards  flow of nitrogen (N)
        real(kind=real_wp) :: n_out     !< Outwards flow of nitrogen (N)
        real(kind=real_wp) :: p_in      !< Inwards  flow of phosphorus (P)
        real(kind=real_wp) :: p_out     !< Outwards flow of phosphorus (P)
        real(kind=real_wp) :: si_in     !< Inwards  flow of silicon (Si)
        real(kind=real_wp) :: si_out    !< Outwards flow of silicon (Si)
        real(kind=real_wp) :: area

        real(kind=real_wp) :: food     ! unused variable
        real(kind=real_wp) :: natmort  ! unused variable
        real(kind=real_wp) :: sibal    ! unused variable
       ! local variables

        ! Check on budgets: Nbal, Pbal and Sibal should be zero (unless material is harvested!!).
        ! shell fluxes (if taken into account) remain part of the structural volume
        ! hence, their (explicit) presence does thus not change the calculation of the total mass budgets

        c_in   = filtr * dens_m2/depth
        c_out  = fl(1+iflux)+ fl(4+iflux)+fl(7+iflux)+fl(10+iflux)+fl(14+iflux)
        n_in   = nfiltr *  dens_m2/depth
        n_out  = fl(2+iflux)+ fl(5+iflux)+fl(8+iflux)+fl(11+iflux)+fl(15+iflux)
        p_in   = pfiltr *  dens_m2/depth
        p_out  = fl(3+iflux)+ fl(6+iflux)+fl(9+iflux)+fl(12+iflux)+fl(16+iflux)
        si_in  = sifiltr *  dens_m2/depth
        si_out = fl(13+iflux)

        ov%c_balance = c_in - c_out &
               -( fl(17+iflux)- fl(18+iflux) ) &
               -( fl(19+iflux)- fl(20+iflux)- fl(21+iflux)) &
               -( fl(22+iflux)- fl(23+iflux)- fl(14+iflux) )

        ov%n_balance = n_in - n_out &
               -( fl(17+iflux)- fl(18+iflux) )*tn &
               -( fl(19+iflux)- fl(20+iflux)- fl(21+iflux))*tn &
               -( fl(22+iflux)- fl(23+iflux)- fl(14+iflux))*tn

        ov%p_balance = p_in - p_out &
               -( fl(17+iflux)- fl(18+iflux) )*tp &
               -( fl(19+iflux)- fl(20+iflux)- fl(21+iflux))*tp &
               -( fl(22+iflux)- fl(23+iflux)- fl(14+iflux))*tp

        sibal = si_in - si_out                ! unused
        food  = food_pelagic + food_benthic   ! unused
        area = iv%get_area()

        natmort=rmor*(v_m2+e_m2+r_m2)/conv_gww_gc*area           !(gWW d-1)  ! unused
        ov%harvest=rhrv*(v_m2+e_m2+r_m2)/conv_gww_gc*area        !(gWW d-1)
        ov%spawn =  dspw * conv_j_gc *  dens_m2 / depth          !(gC d-1)
        ov%totbiomass = (v_m2 + e_m2 + r_m2) * area              !(gC/cell)
        ov%biomass = (v_m2 + e_m2 + r_m2)                        !(gC/m2)
        ov%totafdw = ov%totbiomass / conv_gafdw_gc               !(gAFDW/cell)
        ov%afdw = ov%biomass / conv_gafdw_gc                     !(gAFDW/m2)
        ov%totww = ov%totbiomass / conv_gww_gc                   !(gWW/cell)
        ov%ww = (ov%biomass / conv_gww_gc) / area                !(gWW/m2)
        ov%ww_ind = ((ov%biomass / conv_gww_gc) / area) / dens_m2!(gWW/ind)
        ov%grossgr= pa * conv_j_gc * dens_m2                     !(gC/m2/d)
        ov%nettgr= ov%grossgr - dres* conv_j_gc * dens_m2        !(gC/m2/d)
        if (benths==0) then                                      !pelagics (=active substance)
            ov%dens_out = dens_m2/depth                          !(#/m3)
            ov%biomass = ov%biomass / depth                      !(gC/m3)
            ov%afdw = ov%afdw / depth                            !(gAFDW/m3)
            ov%ww = ov%ww/depth                                  !(gWW/m3)
        else                                                     !benthics (=inactive substance)
            ov%dens_out = dens_m2
        end if
    end subroutine calculate_output_vars

    subroutine assign_food_arrays(pmsa, ip, cfood, ccfood, chlcfood, ncfood, pcfood, &
        sicfood, pref, benfood, fffood, detbio, food_count, use_with_protist, nqfood, dets1)
        real(kind=real_wp)   :: pmsa(*) !<
        integer(kind=int_wp) :: ip(*), benfood(:)   !<
        real(kind=real_wp), dimension(:)   :: cfood, ccfood, chlcfood, ncfood, pcfood, &
                                sicfood, pref, fffood !< different food sources

        integer(kind=int_wp), intent(in) :: food_count
        integer(kind=int_wp), intent(in) :: use_with_protist
        integer(kind=int_wp), intent(in) :: nqfood
        real(kind=real_wp), dimension(4), intent(in) :: dets1

        ! local
        real(kind=real_wp), dimension(4) :: detbio
        integer(kind=int_wp) :: ifood
        integer(kind=int_wp) :: ipmsa_off
        real(kind=real_wp), parameter :: small = 1.0e-10   ! Small, but not excessively small, so that it is not likely to cause overflows.

        if (use_with_protist == 1) then
            ! The various quantities per food source are grouped together
            ! (DEBGRZ expects the various quantities grouped per type
            ! of quantity)
            ! Eight quantities per food source:
            ! 1. Carbon
            ! 2. Chlorophyll - passive, it should be updated
            ! 3. Nitrogen
            ! 4. Phosphorus
            ! 5. Silicon     - only plays a role for diatoms, but we take them along for all PROTIST food sources
            ! 6. Preference of DEB grazers
            ! 7. Use 0 [pelagic, in water column] or 1 [benthic, in bottom] algae
            ! 8. Faecal fraction of grazers
            do ifood=3,food_count ! food_count=12
                ipmsa_off      = 78 + (ifood - 3) * nqfood
                cfood(ifood)   = max(0.,pmsa( ip(ipmsa_off + 1)))
                ccfood(ifood)  = 1.
                chlcfood(ifood) =       pmsa( ip(ipmsa_off + 2))   / (small + cfood(ifood))  ! To avoid division by zero
                ncfood(ifood)  =        pmsa( ip(ipmsa_off + 3))   / (small + cfood(ifood))
                pcfood(ifood)  =        pmsa( ip(ipmsa_off + 4))   / (small + cfood(ifood))
                sicfood(ifood) =        pmsa( ip(ipmsa_off + 5))   / (small + cfood(ifood))
                pref(ifood)    =        pmsa( ip(ipmsa_off + 6))
                benfood(ifood) =  nint (pmsa( ip(ipmsa_off + 7)))
                fffood(ifood)  =        pmsa( ip(ipmsa_off + 8))
            end do
        else
            do ifood=3,food_count ! food_count=42
                cfood(ifood)  = max(0.,pmsa( ip(76 +                    ifood)))
                ccfood(ifood) = 1.
                ncfood(ifood) =        pmsa( ip(76 +   (food_count-2) + ifood))
                pcfood(ifood) =        pmsa( ip(76 + 2*(food_count-2) + ifood))
                sicfood(ifood)=        pmsa( ip(76 + 3*(food_count-2) + ifood))
                pref(ifood)   =        pmsa( ip(76 + 4*(food_count-2) + ifood))
                benfood(ifood)=  nint (pmsa( ip(76 + 5*(food_count-2) + ifood)))
                fffood(ifood) =        pmsa( ip(76 + 6*(food_count-2) + ifood))
            end do
        end if

        ! Add Detbio and DetS1 to the food array's
        ! DetBIO is pelagic detritus
        cfood   (1) = detbio(1)
        ccfood  (1) = 1.
        benfood (1) = nint(0.)

        if (detbio(1) > tiny(detbio(1))) then
            ncfood(1)   = detbio(2) / detbio(1)
            pcfood(1)   = detbio(3) / detbio(1)
            sicfood(1)  = detbio(4) / detbio(1)
        else
            ncfood(1)   = 0.
            pcfood(1)   = 0.
            sicfood(1)  = 0.
        end if

        ! dets1 is a benthic detritus
        cfood   (2) = dets1(1)
        ccfood  (2) = 1.
        BenFood (2) = nint(1.)

        if (dets1(1) > tiny(dets1(1))) then
            ncfood(2)   = dets1(2) / dets1(1)
            pcfood(2)   = dets1(3) / dets1(1)
            sicfood(2)  = dets1(4) / dets1(1)
        else
            ncfood(2)   = 0.
            pcfood(2)   = 0.
            sicfood(2)  = 0.
        end if
    end subroutine assign_food_arrays

    subroutine update_loop_vars(iflux, noflux, pv, pmsa, iparray, increm)
        !< Update all variables for the next cell (segment) iteration.

        integer(kind=int_wp), intent(in)    :: noflux
        integer(kind=int_wp), intent(inout) :: iflux, iparray(:)
        integer(kind=int_wp), intent(in)    :: increm(*)
        type(process_variables), intent(in) :: pv
        real(kind=real_wp), intent(inout)   :: pmsa(*)


        integer(kind=int_wp) :: params_count

        params_count = pv%pointers_count
        pmsa(iparray(34)) = pv%inp%dospawn
        iflux = iflux + noflux
        iparray(1:params_count) = iparray(1:params_count) + increm(1:params_count)
    end subroutine update_loop_vars

end module m_debgrz
