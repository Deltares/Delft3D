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
module flocculation_dwq
    use m_waq_precision
    use flocculation, only: get_tshear_tdiss, floc_manning, floc_chassagne, FLOC_MANNING_DYER, &
                            FLOC_CHASSAGNE_SAFAR, FLOC_VERNEY_ETAL
    use precision, only: fp
    implicit none

contains

!> Update the mass distribution of clay over the various floc sizes.
subroutine flocculate_dwq( swfloform, cmacro, cmicro, tpm, tau, total_depth, local_depth, viscosity, rho_water, &
                           d_micro, ustar_macro, spmratioem, ws_macro, ws_micro )
!
! Global variables
!
    integer(kind=int_wp), intent(in) ::  swfloform    !< Formulation for the flocculation process
    real(kind=real_wp), intent(in)   ::  cmacro       !< Macro floc concentration [g/m3]
    real(kind=real_wp), intent(in)   ::  cmicro       !< Micro floc concentration [g/m3]
    real(kind=real_wp), intent(in)   ::  tpm          !< Total sediment concentration (includes organic material) [g/m3]
    real(kind=real_wp), intent(in)   ::  tau          !< Bed shear stress [N/m2]
    real(kind=real_wp), intent(in)   ::  total_depth  !< Total depth (distance bottom to surface) [m]
    real(kind=real_wp), intent(in)   ::  local_depth  !< Total depth (distance segment to surface) [m]
    real(kind=real_wp), intent(in)   ::  viscosity    !< Kinematic viscosity [m2/s]
    real(kind=real_wp), intent(in)   ::  rho_water    !< Density of water- [kg/m3]
    real(kind=real_wp), intent(in)   ::  d_micro      !< characteristic diameter of micro flocs [m]
    real(kind=real_wp), intent(in)   ::  ustar_macro  !< characteristic shear velocity of macro flocs [m/s]
    real(kind=real_wp), intent(out)  ::  spmratioem   !< Ratio of concentration macro flocs to total [-]
    real(kind=real_wp), intent(out)  ::  ws_macro     !< Fall velocity for macro flocs [m/d]
    real(kind=real_wp), intent(out)  ::  ws_micro     !< Fall velocity for micro flocs [m/d]

!
! Local variables
!
    real(kind=fp), parameter :: GRAV_fp = 9.81_fp   !< Gravitational acceleration [m/s2]
    real(kind=fp), parameter :: VONKAR_fp = 0.41_fp !< Von Karman constant [-]
    
    real(kind=fp) :: tpm_fp         !< Total sediment concentration (includes organic material) [g/m3]
    real(kind=fp) :: tau_fp         !< Bed shear stress [N/m2]
    real(kind=fp) :: total_depth_fp !< Total depth (distance bottom to surface) [m]
    real(kind=fp) :: local_depth_fp !< Total depth (distance segment to surface) [m]
    real(kind=fp) :: viscosity_fp   !< Kinematic viscosity [m2/s]
    real(kind=fp) :: rho_water_fp   !< Density of water- [kg/m3]
    real(kind=fp) :: d_micro_fp     !< Characteristic diameter of micro flocs [m]
    real(kind=fp) :: ustar_macro_fp !< Characteristic shear velocity of macro flocs [m/s]
    real(kind=fp) :: spmratioem_fp  !< Ratio of concentration macro flocs to total [-]
    real(kind=fp) :: turb_diss_fp   !< Turbulent dissipation epsilon [m2/s3]
    real(kind=fp) :: tshear_fp      !< Turbulent shear stress [N/m2)
    real(kind=fp) :: ws_macro_fp    !< Fall velocity for macro flocs [m/s]
    real(kind=fp) :: ws_micro_fp    !< Fall velocity for micro flocs [m/s]
    real(kind=fp) :: ws_avg_fp      !< Effective settling velocity of SPM [m/s]

    tpm_fp = real(tpm,fp)
    tau_fp = real(tau,fp)
    total_depth_fp = real(total_depth,fp)
    local_depth_fp = real(local_depth,fp)
    viscosity_fp = real(viscosity,fp)
    rho_water_fp = real(rho_water,fp)
    d_micro_fp   = real(d_micro,fp)
    ustar_macro_fp = real(ustar_macro,fp)
    
    call get_tshear_tdiss( tshear_fp, turb_diss_fp, rho_water_fp, taub=tau_fp, &
                           waterdepth=total_depth_fp, localdepth=local_depth_fp, &
                           vonkar=VONKAR_fp )

    select case (swfloform)
    case (FLOC_MANNING_DYER)
       call floc_manning( tpm_fp, tshear_fp, ws_avg_fp, spmratioem_fp, ws_macro_fp, ws_micro_fp )

    case (FLOC_CHASSAGNE_SAFAR)
       call floc_chassagne( tpm_fp, tshear_fp, turb_diss_fp, GRAV_fp, viscosity_fp, rho_water_fp, d_micro_fp, ustar_macro_fp, &
                ws_avg_fp, spmratioem_fp, ws_macro_fp, ws_micro_fp )

    case (FLOC_VERNEY_ETAL)
       !call floc_verney

    end select

    spmratioem = real(spmratioem_fp, real_wp)
    ws_macro = real(ws_macro_fp * 86400.0_fp, real_wp) ! From m/s to m/day
    ws_micro = real(ws_micro_fp * 86400.0_fp, real_wp)

end subroutine flocculate_dwq

end module flocculation_dwq
