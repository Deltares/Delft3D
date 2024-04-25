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
module flocculation
    use precision
    implicit none
    private
    
    public macro_floc_settling_manning
    public micro_floc_settling_manning
    public macro_floc_frac_manning
    public floc_manning
    
    public macro_floc_settling_chassagne
    public micro_floc_settling_chassagne
    public macro_floc_frac_chassagne
    public floc_chassagne
    
    public flocculate
    public get_tshear_tdiss

    integer, parameter, public :: FLOC_NONE                 = 0 ! no flocculation
    integer, parameter, public :: FLOC_MANNING_DYER         = 1 ! flocculation based on Manning and Dyer (2007)
    integer, parameter, public :: FLOC_CHASSAGNE_SAFAR      = 2 ! flocculation based on Chassagne and Safar (2020)
    integer, parameter, public :: FLOC_VERNEY_ETAL          = 3 ! flocculation based on Verney et al (2010)
    
    real(fp), parameter        :: PARAM_SOULSBY = 3.0  ! Coefficient of proportionality according to Soulsby (see Manning and Dyer)

contains
    
subroutine macro_floc_settling_manning( spm, tshear, ws_macro )

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
    real(fp), intent(in)  :: spm                  !< (Total) concentration of suspended particulate matter (not including organic matter) [g/m3]
    real(fp), intent(in)  :: tshear               !< Turbulent shear stress [N/m2]
    real(fp), intent(out) :: ws_macro             !< Settling velocity of macro flocs [m/s]

!
! Local variables and parameters
!
!   NONE

    !
    ! Settling velocity of macro flocs
    !
    if ( tshear < 0.65_fp ) then
        ws_macro = 0.644_fp + 0.000471_fp * spm + 9.36_fp * tshear - 13.1_fp * tshear ** 2
    elseif ( tshear < 1.45_fp ) then
        ws_macro = 3.96_fp  + 0.000346_fp * spm - 4.38_fp * tshear + 1.33_fp * tshear ** 2
    else
        ! Note: in the article the upper limit for tshear is 5 N/m2
        ws_macro = 1.18_fp  + 0.000302_fp * spm - 0.491_fp * tshear + 0.057_fp * tshear ** 2
    endif

    !
    ! Settling velocity for both macro flocs
    ! (Convert to m/s - the settling velocities as calculated above are in mm/s)
    !
    ws_macro      = 0.001_fp * ws_macro

end subroutine macro_floc_settling_manning


subroutine micro_floc_settling_manning( tshear, ws_micro )

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
    real(fp), intent(in)  :: tshear               !< Turbulent shear stress [N/m2]
    real(fp), intent(out) :: ws_micro             !< Settling velocity of micro flocs [m/s]

!
! Local variables and parameters
!
!   NONE

    !
    ! Settling velocity of micro flocs
    !
    if ( tshear < 0.52_fp ) then
        ws_micro = 0.244_fp + 3.25_fp * tshear - 3.71_fp * tshear ** 2
    else
        ! Note: in the article the upper limit for tshear is 10 N/m2
        ws_micro = 0.65_fp * tshear ** (-0.541_fp)
    endif

    !
    ! Settling velocity for both micro flocs
    ! (Convert to m/s - the settling velocities as calculated above are in mm/s)
    !
    ws_micro      = 0.001_fp * ws_micro

end subroutine micro_floc_settling_manning


subroutine macro_floc_frac_manning( spm, macro_frac )

!!--description-----------------------------------------------------------------
!
! Calculate the equilibrium fraction of macro flocs using the formulation
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
    real(fp), intent(in)  :: spm                  !< (Total) concentration of suspended particulate matter (not including organic matter) [g/m3]
    real(fp), intent(out) :: macro_frac           !< Fraction of macro flocs mass of total spm mass [-]
    
!
! Local variables
!
    real(fp)              :: floc_ratio           !< Mass ratio of macro flocs versus micro flocs [-]
    real(fp)              :: spm1                 !< Concentration clipped to value below 10000 [mg/l]

    !
    ! Distribution of macro and micro flocs
    !
    spm1 = min(spm, 100000.0_fp)
    floc_ratio = 0.815_fp + 0.00318_fp * spm1 - 1.4e-7_fp * spm1 ** 2
    macro_frac = floc_ratio / (1.0_fp + floc_ratio)

end subroutine macro_floc_frac_manning


subroutine floc_manning( spm, tshear, ws_avg, macro_frac, ws_macro, ws_micro )

!!--description-----------------------------------------------------------------
!
! Calculate the settling velocities of suspended particulate matter using the formulation
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
    real(fp), intent(in)  :: spm                  !< (Total) concentration of suspended particulate matter (not including organic matter) [g/m3]
    real(fp), intent(in)  :: tshear               !< Turbulent shear stress (N/m2)
    real(fp), intent(out) :: ws_avg               !< Effective settling velocity of SPM [m/s]
    real(fp), intent(out) :: macro_frac           !< Fraction of macro flocs mass of total spm mass [-]
    real(fp), intent(out) :: ws_macro             !< Settling velocity of macro flocs [m/s]
    real(fp), intent(out) :: ws_micro             !< Settling velocity of micro flocs [m/s]

!
! Local variables
!
!   NONE

    !
    ! Settling velocity of macro flocs
    !
    call macro_floc_settling_manning( spm, tshear, ws_macro )

    !
    ! Settling velocity of micro flocs
    !
    call micro_floc_settling_manning( tshear, ws_micro )

    !
    ! Mass fraction of macro flocs
    !
    call macro_floc_frac_manning( spm, macro_frac )

    !
    ! Effective settling velocity for both macro and micro flocs together
    !
    ws_avg = ws_micro + macro_frac * (ws_macro - ws_micro)

end subroutine floc_manning


subroutine macro_floc_settling_chassagne( spm, tshear, tdiss, grav, viskin, rho_water, ws_macro )

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
    real(fp), intent(in)  :: spm                  !< (Total) concentration of suspended particulate matter (not including organic matter) [g/m3]
    real(fp), intent(in)  :: tshear               !< Turbulent shear stress [N/m2]
    real(fp), intent(in)  :: tdiss                !< Turbulent dissipation [m2/s3]
    real(fp), intent(in)  :: grav                 !< Gravitational acceleration [m/s2]
    real(fp), intent(in)  :: viskin               !< Kinematic viscosity of water [m2/s]
    real(fp), intent(in)  :: rho_water            !< Water density [kg/m3]
    real(fp), intent(out) :: ws_macro             !< Settling velocity of macro flocs [m/s]

!
! Local variables
!
    real(fp)              :: factor1, factor2, factor3, factor4
    
    real(fp), parameter   :: d_micro     = 1.0e-4_fp ! Characteristic diameter of micro flocs [m]
    real(fp), parameter   :: ustar_macro = 0.067_fp  ! Characteristic shear velocity of macro flocs [m/s]

    !
    ! Compute dimensionless terms
    !
    factor1  = (tdiss * d_micro ** 4 / viskin ** 3) ** 0.166_fp
    factor2  = (spm / rho_water) ** 0.22044_fp
    factor3  = sqrt(viskin / max(tdiss, 1.0e-12_fp))
    factor4  = sqrt(rho_water * ustar_macro ** 2 / max(tshear, 1.0e-12_fp))

    !
    ! Settling velocity of macro flocs [m/s]
    !
    ws_macro = 0.129_fp * grav * factor1 * factor2 * factor3 * exp( - factor4 ** 0.463_fp )

end subroutine macro_floc_settling_chassagne


subroutine micro_floc_settling_chassagne( tshear, tdiss, grav, viskin, rho_water, ws_micro )

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
    real(fp), intent(in)  :: tshear               !< Turbulent shear stress [N/m2]
    real(fp), intent(in)  :: tdiss                !< Turbulent dissipation [m2/s3]
    real(fp), intent(in)  :: grav                 !< Gravitational acceleration [m/s2]
    real(fp), intent(in)  :: viskin               !< Kinematic viscosity of water [m2/s]
    real(fp), intent(in)  :: rho_water            !< Water density [kg/m3]
    real(fp), intent(out) :: ws_micro             !< Settling velocity of micro flocs [m/s]

!
! Local variables
!
    real(fp)              :: factor1, factor2, factor3, factor4

    real(fp), parameter   :: d_1         = 1.0e-5_fp ! Characteristic diameter of elementary particles [m]
    real(fp), parameter   :: ustar_micro = 0.025_fp  ! Characteristic shear velocity of micro flocs [m/s]

    !
    ! Compute dimensionless terms
    !
    factor1  = (tdiss * d_1 ** 4 / viskin ** 3) ** 0.166_fp
    factor3  = sqrt(viskin / max(tdiss, 1.0e-12_fp))
    factor4  = sqrt(rho_water * ustar_micro ** 2 / max(tshear, 1.0e-12_fp))

    !
    ! Settling velocity of macro flocs [m/s]
    !    
    ws_micro = 0.594_fp * grav * factor1 * factor3 * exp( - factor4 ** 0.66_fp )

end subroutine micro_floc_settling_chassagne


subroutine macro_floc_frac_chassagne( spm, macro_frac )

!!--description-----------------------------------------------------------------
!
! Calculate the equilibrium fraction of macro flocs using the formulation
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
    real(fp), intent(in)  :: spm                  !< (Total) concentration of suspended particulate matter (not including organic matter) [g/m3]
    real(fp), intent(out) :: macro_frac           !< Fraction of macro flocs mass of total spm mass [-]

!
! Local variables
!
!   NONE

    !
    ! Distribution of macro and micro flocs
    !
    if ( spm <= 1.0_fp ) then
        macro_frac = 0.1_fp
    else
        macro_frac = min(0.1_fp + 0.221_fp * log10( spm ), 1.0_fp)
    endif

end subroutine macro_floc_frac_chassagne


subroutine floc_chassagne( spm, tshear, tdiss, grav, viskin, rho_water, ws_avg, macro_frac, ws_macro, ws_micro )

!!--description-----------------------------------------------------------------
!
! Calculate the settling velocities of suspended particulate matter using the formulation
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
    real(fp), intent(in)  :: spm                  !< (Total) concentration of suspended particulate matter (not including organic matter) [g/m3]
    real(fp), intent(in)  :: tshear               !< Turbulent shear stress [N/m2]
    real(fp), intent(in)  :: tdiss                !< Turbulent dissipation [m2/s3]
    real(fp), intent(in)  :: grav                 !< Gravitational acceleration [m/s2]
    real(fp), intent(in)  :: viskin               !< Kinematic viscosity of water [m2/s]
    real(fp), intent(in)  :: rho_water            !< Water density [kg/m3]
    real(fp), intent(out) :: ws_avg               !< Downward flux of SPM due to settling [g/m2/s]
    real(fp), intent(out) :: macro_frac           !< Fraction of macro flocs mass of total spm mass [-]
    real(fp), intent(out) :: ws_macro             !< Settling velocity of macro flocs [m/s]
    real(fp), intent(out) :: ws_micro             !< Settling velocity of micro flocs [m/s]

!
! Local variables 
!
!   NONE

    !
    ! Mass fraction of macro flocs
    !
    call macro_floc_frac_chassagne( spm, macro_frac )

    !
    ! Settling velocity of macro flocs (m/s)
    !
    call macro_floc_settling_chassagne( spm, tshear, tdiss, grav, viskin, rho_water, ws_macro )

    !
    ! Settling velocity of micro flocs (m/s)
    !
    call micro_floc_settling_chassagne( tshear, tdiss, grav, viskin, rho_water, ws_micro )

    !
    ! Settling velocity for both macro and micro flocs together
    !
    ws_avg = ws_micro + macro_frac * (ws_macro - ws_micro)

end subroutine floc_chassagne


subroutine flocculate(cfloc, flocdt, breakdt, flocmod)
!!--description-----------------------------------------------------------------
!
! Update the mass distribution of clay over the various floc sizes.
!
!!--declarations----------------------------------------------------------------
!
! Global variables
!
    real(fp), dimension(:,:), intent(inout)  :: cfloc   !< Concentration split per clay fraction and floc size [g/m3]
    real(fp),                 intent(in)     :: flocdt  !< Relaxation factor towards equilibrium with more macro flocs [-]
    real(fp),                 intent(in)     :: breakdt !< Relaxation factor towards equilibrium with less macro flocs [-]
    integer,                  intent(in)     :: flocmod !< Flocculation model being used [-]
    
!
! Local variables 
!
    integer  :: i              !< Clay population index
    integer  :: j              !< Floc size index
    integer  :: nflocpop       !< Number of clay populations
    integer  :: nflocsizes     !< Number of floc size classes
    real(fp) :: adt            !< Relaxation factor towards equilibtium [-]
    real(fp) :: eq_cfloc_micro !< Equilibrium concentration of micro flocs within specific clay population [kg/m3]
    real(fp) :: eq_cfloc_macro !< Equilibrium concentration of macro flocs within specific clay population [kg/m3]
    real(fp) :: macro_frac     !< Fraction of macro flocs mass of total spm mass [-]
    real(fp) :: tcclay         !< Total clay concentration [kg/m3]
    real(fp) :: tcpop          !< Total concentration of specific clay population [kg/m3]
    !
    nflocpop = size(cfloc,1)
    nflocsizes = size(cfloc,2)
    
    tcclay = 0.0_fp
    do j = 1, nflocsizes
       do i = 1, nflocpop
           tcclay = tcclay + cfloc(i,j)
       enddo
    enddo

    select case (flocmod)
    case (FLOC_MANNING_DYER, FLOC_CHASSAGNE_SAFAR)
       if (flocmod == FLOC_MANNING_DYER) then
          call macro_floc_frac_manning( tcclay, macro_frac )
       else
          call macro_floc_frac_chassagne( tcclay, macro_frac )
       endif
       
       do i = 1, nflocpop
          tcpop = cfloc(i,1) + cfloc(i,2)
          !
          eq_cfloc_macro = macro_frac * tcpop
          eq_cfloc_micro = tcpop - eq_cfloc_macro
          !
          if (eq_cfloc_macro > cfloc(i,2)) then ! towards more macro flocs, use flocculation time scale
             adt = flocdt
          else ! towards less macro flocs, use break-up time scale
             adt = breakdt
          endif
          cfloc(i,1) = cfloc(i,1) + adt * (eq_cfloc_micro - cfloc(i,1))
          cfloc(i,2) = cfloc(i,2) + adt * (eq_cfloc_macro - cfloc(i,2))
       enddo

    case (FLOC_VERNEY_ETAL)
       !call floc_verney

    end select
   
end subroutine flocculate


subroutine get_tshear_tdiss( tshear, tdiss, rho_water, tke, tlength, timtur, taub, waterdepth, localdepth, vonkar )

!!--description-----------------------------------------------------------------
!
! Calculate the turbulent shear and dissipation for different flow models
!
!!--declarations----------------------------------------------------------------
!
! Global variables
!
    real(fp), intent(out)           :: tshear     !< Turbulent shear stress [N/m2)
    real(fp), intent(inout)         :: tdiss      !< Turbulent dissipation epsilon [m2/s3]
    real(fp), intent(in)            :: rho_water  !< Water density [kg/m3]
    real(fp), optional, intent(in)  :: tke        !< Turbulent kinetic energy lk [N/m2]
    real(fp), optional, intent(in)  :: tlength    !< Turbulent length scale L [m]
    real(fp), optional, intent(in)  :: timtur     !< Turbulent time scale tau [s]
    real(fp), optional, intent(in)  :: taub       !< Bed shear stress [N/m2]
    real(fp), optional, intent(in)  :: waterdepth !< Total water depth [m]
    real(fp), optional, intent(in)  :: localdepth !< Depth below water surface [m]
    real(fp), optional, intent(in)  :: vonkar     !< Von Karman constant [-]

!
! Local variables 
!
    real(fp), parameter :: cd = 0.09_fp ** 0.75_fp   ! turbulence constant [-] cmu^(3/4)
    
    real(fp) :: ustar         ! shear velocity [m/s]
    real(fp) :: z             ! height above the bed [m]
    real(fp) :: xi            ! relative depth [-]

    if (present(tke)) then
       if (present(timtur)) then ! k-tau
          tshear = PARAM_SOULSBY * rho_water * tke
          tdiss = tke * timtur
       elseif (present(tlength)) then ! k-L
          tshear = PARAM_SOULSBY * rho_water * tke
          tdiss = cd * tke ** 1.5_fp / tlength
       else ! k-eps
          tshear = PARAM_SOULSBY * rho_water * tke
          ! tdiss already set
       endif
    elseif (present(waterdepth) .and. present(taub) .and. present(vonkar)) then
       if (present(localdepth)) then ! algebraic
          z = waterdepth - localdepth
          xi = localdepth/waterdepth
       else ! 2D
          z  = 0.5_fp * waterdepth
          xi = 0.5_fp
       endif
       ustar = sqrt(taub / rho_water)
       tshear = xi * taub
       tdiss = (xi * ustar ** 3) / (vonkar * z)
    endif
end subroutine get_tshear_tdiss

end module flocculation
