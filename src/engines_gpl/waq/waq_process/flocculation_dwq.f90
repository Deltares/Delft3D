!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!  
!  
!
!!--description-----------------------------------------------------------------
!
! Module for flocculation formulations
!
! Note:
! Version for D-Water Quality, derived from the original.
! Avoid clashes between modules with the same name and other incompatibilities.
!
module flocculation_dwq
    use m_waq_precision

    !use precision
    implicit none

    integer(kind = int_wp), parameter, private :: fp = kind(1.0)

    integer(kind = int_wp), parameter, private :: FLOC_MANNING_DYER = 1
    integer(kind = int_wp), parameter, private :: FLOC_CHASSAGNE_SAFAR = 2
    integer(kind = int_wp), parameter, private :: FLOC_VERNEY_ETAL = 3   ! Note: not implemented yet

    real(fp), parameter :: param_soulsby = 3.0  ! Coefficient of proportionality according to Soulsby (see Manning and Dyer)

contains

    subroutine macro_floc_settling_manning(spm, tke, ws_macro)

        !!--description-----------------------------------------------------------------
        !
        ! Calculate the settling velocity of macro flocs according the formulation
        ! by Manning and Dyer
        !
        !!--pseudo code and references--------------------------------------------------
        ! Manning, A.J. and Dyer, K.R.
        ! Mass settling flux of fine sediments in Northern European estuaries:
        ! Measurements and predictions
        ! Marine Geology, 245 (2007), 107-122
        ! DOI: 10.1016/j.margeo.2007.07.005
        !!--declarations----------------------------------------------------------------
        !
        ! Global variables
        !
        real(fp), intent(in) :: spm                  !< (Total) concentration of suspended particulate matter (not including organic matter; g/m3)
        real(fp), intent(in) :: tke                  !< Turbulent kinetic energy (as a measure for turbulent shear stress; N/m2)
        real(fp), intent(out) :: ws_macro             !< Settling velocity of macro flocs (m/s)

        !
        ! Local variables and parameters
        !
        real(fp), parameter :: param_soulsby = 3.0  ! Coefficient of proportionality according to Soulsby (see Manning and Dyer)
        real(fp) :: tshear               ! Turbulent shear stress

        tshear = param_soulsby * tke

        !
        ! Settling velocity of macro flocs
        !
        if (tshear < 0.65_fp) then
            ws_macro = 0.644_fp - 0.000471_fp * spm + 9.36_fp * tshear - 13.1_fp * tshear ** 2
        elseif (tshear < 1.45_fp) then
            ws_macro = 3.96_fp + 0.000346_fp * spm - 4.38_fp * tshear + 1.33_fp * tshear ** 2
        else
            ! Note: in the article the upper limit for tshear is 5 N/m2
            ws_macro = 1.18_fp + 0.000302_fp * spm - 0.491_fp * tshear + 0.057_fp * tshear ** 2
        endif

        !
        ! Settling flux for both macro flocs
        ! (Convert to m/s - the settling velocities as calculated above are in mm/s)
        !
        ws_macro = 0.001_fp * ws_macro

    end subroutine macro_floc_settling_manning


    subroutine micro_floc_settling_manning(tke, ws_micro)

        !!--description-----------------------------------------------------------------
        !
        ! Calculate the settling velocity of micro flocs according the formulation
        ! by Manning and Dyer
        !
        !!--pseudo code and references--------------------------------------------------
        ! Manning, A.J. and Dyer, K.R.
        ! Mass settling flux of fine sediments in Northern European estuaries:
        ! Measurements and predictions
        ! Marine Geology, 245 (2007), 107-122
        ! DOI: 10.1016/j.margeo.2007.07.005
        !!--declarations----------------------------------------------------------------
        !
        ! Global variables
        !
        real(fp), intent(in) :: tke                  !< Turbulent kinetic energy (as a measure for turbulent shear stress; N/m2)
        real(fp), intent(out) :: ws_micro             !< Settling velocity of micro flocs (m/s)

        !
        ! Local variables and parameters
        !
        real(fp) :: tshear               ! Turbulent shear stress

        tshear = param_soulsby * tke

        !
        ! Settling velocity of micro flocs
        !
        if (tshear < 0.52_fp) then
            ws_micro = 0.244_fp + 3.25_fp * tshear - 3.71_fp * tshear ** 2
        else
            ! Note: in the article the upper limit for tshear is 10 N/m2
            ws_micro = 0.65_fp * tshear ** (-0.541_fp)
        endif

        !
        ! Settling flux for both micro flocs
        ! (Convert to m/s - the settling velocities as calculated above are in mm/s)
        !
        ws_micro = 0.001_fp * ws_micro

    end subroutine micro_floc_settling_manning


    subroutine macro_micro_floc_manning(spm, floc_ratio)

        !!--description-----------------------------------------------------------------
        !
        ! Calculate the equilibrium ratio of macro and micro flocs using the formulation
        ! by Manning and Dyer
        !
        !!--pseudo code and references--------------------------------------------------
        ! Manning, A.J. and Dyer, K.R.
        ! Mass settling flux of fine sediments in Northern European estuaries:
        ! Measurements and predictions
        ! Marine Geology, 245 (2007), 107-122
        ! DOI: 10.1016/j.margeo.2007.07.005
        !!--declarations----------------------------------------------------------------
        !
        ! Global variables
        !
        real(fp), intent(in) :: spm                  !< (Total) concentration of suspended particulate matter (not including organic matter; g/m3)
        real(fp), intent(out) :: floc_ratio           !< Mass ratio of macro flocs versus micro flocs (-)

        !
        ! Local variables
        !
        !   NONE

        !
        ! Distribution of macro and micro flocs
        !
        floc_ratio = 0.815_fp + 3.18e-3 * spm - 1.4e-7_fp * spm ** 2

    end subroutine macro_micro_floc_manning


    subroutine floc_manning(spm, tke, settling_flux, floc_ratio, ws_macro, ws_micro)

        !!--description-----------------------------------------------------------------
        !
        ! Calculate the settling flux of suspended particulate matter using the formulation
        ! by Manning and Dyer
        !
        !!--pseudo code and references--------------------------------------------------
        ! Manning, A.J. and Dyer, K.R.
        ! Mass settling flux of fine sediments in Northern European estuaries:
        ! Measurements and predictions
        ! Marine Geology, 245 (2007), 107-122
        ! DOI: 10.1016/j.margeo.2007.07.005
        !!--declarations----------------------------------------------------------------
        !
        ! Global variables
        !
        real(fp), intent(in) :: spm                  !< (Total) concentration of suspended particulate matter (not including organic matter; g/m3)
        real(fp), intent(in) :: tke                  !< Turbulent kinetic energy (as a measure for turbulent shear stress; N/m2)
        real(fp), intent(out) :: settling_flux        !< Downward flux of SPM due to settling (g/m2/s)
        real(fp), intent(out) :: floc_ratio           !< Mass ratio of macro flocs versus micro flocs
        real(fp), intent(out) :: ws_macro             !< Settling velocity of macro flocs (m/s)
        real(fp), intent(out) :: ws_micro             !< Settling velocity of micro flocs (m/s)

        !
        ! Local variables
        !
        !   NONE

        !
        ! Settling velocity of macro flocs
        !
        call macro_floc_settling_manning(spm, tke, ws_macro)

        !
        ! Settling velocity of micro flocs
        !
        call micro_floc_settling_manning(tke, ws_micro)

        !
        ! Mass ratio for macro/micro flocs
        !
        call macro_micro_floc_manning(spm, floc_ratio)

        !
        ! Settling flux for both macro and micro flocs together
        !
        settling_flux = spm * (floc_ratio * ws_macro + ws_micro) / (floc_ratio + 1.0_fp)

    end subroutine floc_manning


    subroutine macro_floc_settling_chassagne(spm, tau, totaldepth, localdepth, grav, viscosity, rho_water, ws_macro)

        !!--description-----------------------------------------------------------------
        !
        ! Calculate the settling velocity of macro flocs using the formulation
        ! by Chassagne and Safar
        !
        !!--pseudo code and references--------------------------------------------------
        ! Chassagne, C. and Safar, Z.
        ! Modelling flocculation: Towards an integration in large-scale sediment transport models
        ! Marine Geology, 430 (2020), 106361
        ! DOI: 10.1016/j.margeo.2020.106361
        !!--declarations----------------------------------------------------------------
        !
        ! Global variables
        !
        real(fp), intent(in) :: spm                  !< (Total) concentration of suspended particulate matter (not including organic matter; g/m3)
        real(fp), intent(in) :: tau                  !< Bed shear stress (N/m2)
        real(fp), intent(in) :: totaldepth           !< Distance between bottom and surface (m)
        real(fp), intent(in) :: localdepth           !< Distance between current segment and surface (m)
        real(fp), intent(in) :: grav                 !< Gravitational acceleration (m/s2)
        real(fp), intent(in) :: viscosity            !< Viscosity of water (kg/sm)
        real(fp), intent(in) :: rho_water            !< Water density (kg/m3)
        real(fp), intent(out) :: ws_macro             !< Settling velocity of macro flocs (m/s)

        !
        ! Local variables
        !
        real(fp) :: ustar
        real(fp) :: zlocal, zcorr
        real(fp) :: factor1, factor2, factor3, factor4

        real(fp) :: d_macro
        real(fp) :: ustar_macro

        !
        ! Constants provided in the article
        !
        ustar_macro = 0.067_fp ! (m/s)
        d_macro = 1.0e-4_fp ! (m)

        !
        ! Calculate the gouverning parameters
        !
        ustar = sqrt(tau / rho_water)                  ! shear stress velocity
        zlocal = totaldepth - localdepth                  ! height above the bottom
        zcorr = zlocal / (1.0_fp - zlocal / totaldepth) ! "Z" in the article

        !
        ! Settling velocity of macro flocs (m/s)
        !
        factor1 = (ustar**3 * d_macro ** 4 / zcorr / viscosity ** 3) ** 0.166_fp
        factor2 = (spm / rho_water) ** 0.22044_fp
        factor3 = sqrt(zcorr * viscosity / ustar ** 3)
        factor4 = ustar_macro / ustar * sqrt(zcorr / zlocal)

        ws_macro = 0.095_fp * grav * factor1 * factor2 * factor3 * exp(- factor4 ** 0.463_fp)

    end subroutine macro_floc_settling_chassagne


    subroutine micro_floc_settling_chassagne(tau, totaldepth, localdepth, grav, viscosity, rho_water, ws_micro)

        !!--description-----------------------------------------------------------------
        !
        ! Calculate the settling velocity of micro flocs using the formulation
        ! by Chassagne and Safar
        !
        !!--pseudo code and references--------------------------------------------------
        ! Chassagne, C. and Safar, Z.
        ! Modelling flocculation: Towards an integration in large-scale sediment transport models
        ! Marine Geology, 430 (2020), 106361
        ! DOI: 10.1016/j.margeo.2020.106361
        !!--declarations----------------------------------------------------------------
        !
        ! Global variables
        !
        real(fp), intent(in) :: tau                  !< Bed shear stress (N/m2)
        real(fp), intent(in) :: totaldepth           !< Distance between bottom and surface (m)
        real(fp), intent(in) :: localdepth           !< Distance between current segment and surface (m)
        real(fp), intent(in) :: grav                 !< Gravitational acceleration (m/s2)
        real(fp), intent(in) :: viscosity            !< Viscosity of water (kg/sm)
        real(fp), intent(in) :: rho_water            !< Water density (kg/m3)
        real(fp), intent(out) :: ws_micro             !< Settling velocity of micro flocs (m/s)

        !
        ! Local variables
        !
        real(fp) :: ustar
        real(fp) :: zlocal, zcorr
        real(fp) :: factor1, factor2, factor3, factor4

        real(fp) :: d_micro
        real(fp) :: ustar_micro

        !
        ! Constants provided in the article
        !
        ustar_micro = 0.025_fp ! (m/s)
        d_micro = 1.0e-5_fp ! (m)

        !
        ! Calculate the gouverning parameters
        !
        ustar = sqrt(tau / rho_water)                  ! shear stress velocity
        zlocal = totaldepth - localdepth                  ! height above the bottom
        zcorr = zlocal / (1.0_fp - zlocal / totaldepth) ! "Z" in the article

        !
        ! Settling velocity of micro flocs (m/s)
        !
        factor1 = (ustar**3 * d_micro ** 4 / zcorr / viscosity ** 3) ** 0.166_fp
        factor3 = sqrt(zcorr * viscosity / ustar ** 3)
        factor4 = ustar_micro / ustar * sqrt(zcorr / zlocal)

        ws_micro = 0.5372_fp * grav * factor1 * factor3 * exp(- factor4 ** 0.66_fp)

    end subroutine micro_floc_settling_chassagne


    subroutine macro_micro_floc_chassagne(spm, floc_ratio)

        !!--description-----------------------------------------------------------------
        !
        ! Calculate the equilibrium ratio of macro and micro flocs using the formulation
        ! by Chassagne and Safar
        !
        !!--pseudo code and references--------------------------------------------------
        ! Chassagne, C. and Safar, Z.
        ! Modelling flocculation: Towards an integration in large-scale sediment transport models
        ! Marine Geology, 430 (2020), 106361
        ! DOI: 10.1016/j.margeo.2020.106361
        !!--declarations----------------------------------------------------------------
        !
        ! Global variables
        !
        real(fp), intent(in) :: spm                  !< (Total) concentration of suspended particulate matter (not including organic matter; g/m3)
        real(fp), intent(out) :: floc_ratio           !< Mass ratio of macro flocs versus micro flocs (-)

        !
        ! Local variables
        !
        !   NONE

        !
        ! Distribution of macro and micro flocs
        !
        floc_ratio = 0.1_fp
        if (spm <= 0.1_fp) then
            floc_ratio = 0.1_fp
        elseif (spm >= 1174.0_fp) then
            floc_ratio = 1.0_fp
        else
            floc_ratio = 0.1_fp + 0.221_fp * log10(spm)
        endif

    end subroutine macro_micro_floc_chassagne


    subroutine floc_chassagne(spm, tau, totaldepth, localdepth, grav, viscosity, rho_water, &
            settling_flux, floc_ratio, ws_macro, ws_micro)

        !!--description-----------------------------------------------------------------
        !
        ! Calculate the settling flux of suspended particulate matter using the formulation
        ! by Chassagne and Safar
        !
        ! Explicit assumption:
        ! The flocculation has reached an equilibrium, so that we only need to consider the
        ! class 2 of macro and micro flocs described in the article. Should this not be
        ! applicable, a different approach is required, but it is not really clear how
        ! to determine if the condition is violated or how to deal with a non-stationary
        ! situation.
        !
        !!--pseudo code and references--------------------------------------------------
        ! Chassagne, C. and Safar, Z.
        ! Modelling flocculation: Towards an integration in large-scale sediment transport models
        ! Marine Geology, 430 (2020), 106361
        ! DOI: 10.1016/j.margeo.2020.106361
        !!--declarations----------------------------------------------------------------
        !
        ! Global variables
        !
        real(fp), intent(in) :: spm                  !< (Total) concentration of suspended particulate matter (not including organic matter; g/m3)
        real(fp), intent(in) :: tau                  !< Bed shear stress (N/m2)
        real(fp), intent(in) :: totaldepth           !< Distance between bottom and surface (m)
        real(fp), intent(in) :: localdepth           !< Distance between current segment and surface (m)
        real(fp), intent(in) :: grav                 !< Gravitational acceleration (m/s2)
        real(fp), intent(in) :: viscosity            !< Viscosity of water (kg/sm)
        real(fp), intent(in) :: rho_water            !< Water density (kg/m3)
        real(fp), intent(out) :: settling_flux        !< Downward flux of SPM due to settling (g/m2/s)
        real(fp), intent(out) :: floc_ratio           !< Mass ratio of macro flocs versus micro flocs (-)
        real(fp), intent(out) :: ws_macro             !< Settling velocity of macro flocs (m/s)
        real(fp), intent(out) :: ws_micro             !< Settling velocity of micro flocs (m/s)

        !
        ! Local variables
        !
        !   NONE

        !
        ! Distribution of macro and micro flocs
        !
        call macro_micro_floc_chassagne(spm, floc_ratio)

        !
        ! Settling velocity of macro flocs (m/s)
        !
        call macro_floc_settling_chassagne(spm, tau, totaldepth, localdepth, grav, viscosity, rho_water, ws_macro)

        !
        ! Settling velocity of micro flocs (m/s)
        !
        call micro_floc_settling_chassagne(tau, totaldepth, localdepth, grav, viscosity, rho_water, ws_micro)

        !
        ! Settling flux for both macro and micro flocs together
        !
        settling_flux = spm * (floc_ratio * ws_macro + ws_micro) / (floc_ratio + 1.0_fp)

    end subroutine floc_chassagne


    subroutine flocculate_dwq(swfloform, cmacro, cmicro, tpm, tke, tau, total_depth, local_depth, viscosity, rho_water, &
            spmratioem, ws_macro, ws_micro)


        !!--description-----------------------------------------------------------------
        !
        ! Calculate the fall velocities and the equilibrium concentration for the two floc sizes.
        !
        !!--declarations----------------------------------------------------------------
        !
        ! Global variables
        !
        integer(kind = int_wp), intent(in) :: swfloform    !< Formulation for the flocculation process
        real(kind = real_wp), intent(in) :: cmacro       !< Macro floc concentration
        real(kind = real_wp), intent(in) :: cmicro       !< Micro floc concentration
        real(kind = real_wp), intent(in) :: tpm          !< Total sediment concentration (includes organic material)
        real(kind = real_wp), intent(in) :: tke          !< Turbulent kinectic energy
        real(kind = real_wp), intent(in) :: tau          !< Bottom shear stress
        real(kind = real_wp), intent(in) :: total_depth  !< Total depth (distance bottom to surface)
        real(kind = real_wp), intent(in) :: local_depth  !< Total depth (distance segment to surface)
        real(kind = real_wp), intent(in) :: viscosity    !< Kinematic viscosity
        real(kind = real_wp), intent(in) :: rho_water    !< Density of water
        real(kind = real_wp), intent(out) :: spmratioem   !< Ratio of concentration macro flocs to total
        real(kind = real_wp), intent(out) :: ws_macro     !< Fall velocity for macro flocs
        real(kind = real_wp), intent(out) :: ws_micro     !< Fall velocity for micro flocs

        !
        ! Local variables
        !
        real(kind = real_wp) :: settling_flux
        real, parameter :: grav = 9.81    ! Small variations only, so make it a fixed value

        select case (swfloform)
        case (FLOC_MANNING_DYER)
            call floc_manning(tpm, tke, settling_flux, spmratioem, ws_macro, ws_micro)

        case (FLOC_CHASSAGNE_SAFAR)
            call floc_chassagne(tpm, tau, total_depth, local_depth, grav, viscosity, rho_water, &
                    settling_flux, spmratioem, ws_macro, ws_micro)

        case (FLOC_VERNEY_ETAL)
            !call floc_verney

        end select

        ws_macro = ws_macro * 86400.0  ! From m/s to m/day
        ws_micro = ws_micro * 86400.0

    end subroutine flocculate_dwq

end module flocculation_dwq
