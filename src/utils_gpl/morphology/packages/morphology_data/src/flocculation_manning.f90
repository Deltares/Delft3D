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

!> Module for flocculation formulations of Manning & Dyer
!!
!! Reference:
!! Manning, A.J. and Dyer, K.R.
!! Mass settling flux of fine sediments in Northern European estuaries:
!! Measurements and predictions
!! Marine Geology, 245 (2007), 107-122
!! DOI: 10.1016/j.margeo.2007.07.005
module flocculation_manning
    use precision, only: fp
    implicit none
    private
    
    public macro_floc_settling_manning
    public micro_floc_settling_manning
    public macro_floc_frac_manning
    public floc_manning

contains
    
!> Calculate the settling velocity of macro flocs according the formulation
subroutine macro_floc_settling_manning( spm, tshear, ws_macro )

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


!> Calculate the settling velocity of micro flocs according the formulation
subroutine micro_floc_settling_manning( tshear, ws_micro )

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


!> Calculate the equilibrium fraction of macro flocs using the formulation
subroutine macro_floc_frac_manning( spm, macro_frac )

!
! Global variables
!
    real(fp), intent(in)  :: spm                  !< (Total) concentration of suspended particulate matter (not including organic matter) [g/m3]
    real(fp), intent(out) :: macro_frac           !< Fraction of macro flocs mass of total spm mass [-]

!
! parameters
!
    real(fp), parameter   :: SPM1_LIM = 100000.0_fp !< Upper limit for spm concentration [g/m3] or equivalently [mg/l]

!
! Local variables
!
    real(fp)              :: floc_ratio           !< Mass ratio of macro flocs versus micro flocs [-]
    real(fp)              :: spm1                 !< Concentration clipped to value below SPM1_LIM [mg/l]

    !
    ! Distribution of macro and micro flocs
    !
    spm1 = min(spm, SPM1_LIM)
    floc_ratio = 0.815_fp + 0.00318_fp * spm1 - 1.4e-7_fp * spm1 ** 2
    macro_frac = floc_ratio / (1.0_fp + floc_ratio)

end subroutine macro_floc_frac_manning


!> Calculate the settling velocities of suspended particulate matter using the formulation
subroutine floc_manning( spm, tshear, ws_avg, macro_frac, ws_macro, ws_micro )

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

end module flocculation_manning
