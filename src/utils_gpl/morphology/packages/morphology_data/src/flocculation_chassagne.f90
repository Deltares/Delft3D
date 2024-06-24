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

!> Module for flocculation formulations of Chassagne & Safar
!!
!! Reference:
!! Chassagne, C. and Safar, Z.
!! Modelling flocculation: Towards an integration in large-scale sediment transport models
!! Marine Geology, 430 (2020), 106361
!! DOI: 10.1016/j.margeo.2020.106361
module flocculation_chassagne
    use precision, only: fp
    implicit none
    private
    
    public macro_floc_settling_chassagne
    public micro_floc_settling_chassagne
    public macro_floc_frac_chassagne
    public floc_chassagne

contains
    
!> Calculate the settling velocity of macro flocs using the formulation
subroutine macro_floc_settling_chassagne( spm, tshear, tdiss, grav, viskin, rho_water, d_micro, ustar_macro, ws_macro )
!
! Global variables
!
    real(fp), intent(in)  :: spm                  !< (Total) concentration of suspended particulate matter (not including organic matter) [g/m3]
    real(fp), intent(in)  :: tshear               !< Turbulent shear stress [N/m2]
    real(fp), intent(in)  :: tdiss                !< Turbulent dissipation [m2/s3]
    real(fp), intent(in)  :: grav                 !< Gravitational acceleration [m/s2]
    real(fp), intent(in)  :: viskin               !< Kinematic viscosity of water [m2/s]
    real(fp), intent(in)  :: rho_water            !< Water density [kg/m3]
    real(fp), intent(in)  :: d_micro              !< Characteristic diameter of micro flocs [m]
    real(fp), intent(in)  :: ustar_macro          !< Characteristic shear velocity of macro flocs [m/s]
    real(fp), intent(out) :: ws_macro             !< Settling velocity of macro flocs [m/s]

!
! Local variables
!
    real(fp)              :: factor1, factor2, factor3, factor4
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


!> Calculate the settling velocity of micro flocs using the formulation
subroutine micro_floc_settling_chassagne( tshear, tdiss, grav, viskin, rho_water, ws_micro )
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
    factor1  = (tdiss * d_1 ** 4 / viskin ** 3) ** 0.39_fp
    factor3  = sqrt(viskin / max(tdiss, 1.0e-12_fp))
    factor4  = sqrt(rho_water * ustar_micro ** 2 / max(tshear, 1.0e-12_fp))

    !
    ! Settling velocity of macro flocs [m/s]
    !    
    ws_micro = 0.594_fp * grav * factor1 * factor3 * exp( - factor4 ** 0.66_fp )

end subroutine micro_floc_settling_chassagne


!> Calculate the equilibrium fraction of macro flocs using the formulation
subroutine macro_floc_frac_chassagne( spm, macro_frac )
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


!> Calculate the settling velocities of suspended particulate matter using the formulation
!!
!! Explicit assumption:
!! The flocculation has reached an equilibrium, so that we only need to consider the
!! class 2 of macro and micro flocs described in the article. Should this not be
!! applicable, a different approach is required, but it is not really clear how
!! to determine if the condition is violated or how to deal with a non-stationary
!! situation.
subroutine floc_chassagne( spm, tshear, tdiss, grav, viskin, rho_water, d_micro, ustar_macro, &
                           ws_avg, macro_frac, ws_macro, ws_micro )
!
! Global variables
!
    real(fp), intent(in)  :: spm                  !< (Total) concentration of suspended particulate matter (not including organic matter) [g/m3]
    real(fp), intent(in)  :: tshear               !< Turbulent shear stress [N/m2]
    real(fp), intent(in)  :: tdiss                !< Turbulent dissipation [m2/s3]
    real(fp), intent(in)  :: grav                 !< Gravitational acceleration [m/s2]
    real(fp), intent(in)  :: viskin               !< Kinematic viscosity of water [m2/s]
    real(fp), intent(in)  :: rho_water            !< Water density [kg/m3]
    real(fp), intent(in)  :: d_micro              !< Characteristic diameter of micro flocs [m]
    real(fp), intent(in)  :: ustar_macro          !< Characteristic shear velocity of macro flocs [m/s]
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
    call macro_floc_settling_chassagne( spm, tshear, tdiss, grav, viskin, rho_water, d_micro, ustar_macro, ws_macro )

    !
    ! Settling velocity of micro flocs (m/s)
    !
    call micro_floc_settling_chassagne( tshear, tdiss, grav, viskin, rho_water, ws_micro )

    !
    ! Settling velocity for both macro and micro flocs together
    !
    ws_avg = ws_micro + macro_frac * (ws_macro - ws_micro)

end subroutine floc_chassagne

end module flocculation_chassagne
