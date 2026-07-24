module m_tranb2
   implicit none

contains

   subroutine tranb2(utot, d50, d90, chezy, h, &
                   & npar, par, hidexp, sbot, ssus)
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2026.
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
!!--description-----------------------------------------------------------------
! computes sediment transport according to
! meyer-peter-muller (comor/rivcom)
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
      use precision
      use morphology_data_module, only: MISSING_VALUE
!
! Arguments
!
      integer, intent(in) :: npar
      real(fp), intent(in) :: chezy
      real(fp), intent(in) :: d50
      real(fp), intent(in) :: d90
      real(fp), intent(in) :: h
      real(fp), intent(in) :: hidexp !< hiding & exposure factor
      real(fp), dimension(npar), intent(inout) :: par
      real(fp), intent(in) :: utot
      !
      real(fp), intent(out) :: sbot
      real(fp), intent(out) :: ssus
!
! Local variables
!
      real(fp) :: acal ! user-specified calibration coefficient
      real(fp) :: ag ! gravity acceleration
      real(fp) :: mu_ripple ! ripple factor
      real(fp) :: chezy_grain ! grain related Chezy value
      real(fp) :: delta ! relative density of sediment particle
      real(fp) :: theta ! dimensionless shear stress
      real(fp) :: excess_theta ! excess dimensionless shear stress
      
      real(fp), parameter :: THETA_CRITICAL = 0.047_fp ! critical dimensionless shear stress
!
!! executable statements -------------------------------------------------------
!
      sbot = 0.0_fp
      ssus = 0.0_fp
      !
      ag = par(1)
      delta = par(4)
      acal = par(11)
      !
      !     bed load transport
      !
      chezy_grain = 18.0_fp * log10(max(12.0_fp * h / d90, 1.0_fp))
      mu_ripple = (chezy / chezy_grain)**1.5_fp
      mu_ripple = min(mu_ripple, 1.0_fp)
      theta = (utot / chezy)**2 / delta / d50
      excess_theta = max(mu_ripple * theta - hidexp * THETA_CRITICAL, 0.0_fp)
      !
      sbot = acal * 8.0_fp * sqrt(ag * delta * d50 * excess_theta) * d50 * excess_theta
      ssus = 0.0_fp
      !
      ! Meyer-Peter-Muller specific output
      par = MISSING_VALUE
      par(1) = chezy
      par(2) = theta
      par(3) = excess_theta

   end subroutine tranb2

end module m_tranb2
