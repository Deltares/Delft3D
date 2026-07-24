module m_tranb4
   implicit none

contains

   subroutine tranb4(utot, d50, chezy, npar, par, &
                   & hidexp, sbot, ssus)
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
! general formula
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
      use precision
      !
      implicit none
!
! Arguments
!
      integer, intent(in) :: npar !< length of par array
      real(fp), intent(in) :: chezy !< Chezy value
      real(fp), intent(in) :: d50 !< grain diameter
      real(fp), intent(in) :: hidexp !< hiding & exposure factor
      real(fp), dimension(npar), intent(in) :: par !< sediment transport formula parameters
      real(fp), intent(in) :: utot !< depth averaged velocity magnitude
      !
      real(fp), intent(out) :: sbot !< bedload transport rate
      real(fp), intent(out) :: ssus !< suspended transport rate
!
! Local variables
!
      real(fp) :: acal_bed ! calibration factor for bedload
      real(fp) :: acal_sus ! calibration factor for suspended load
      real(fp) :: ag ! gravity acceleration
      real(fp) :: b_bed ! exponent of Shields number for bedload
      real(fp) :: b_sus ! exponent of Shields number for suspended load
      real(fp) :: cc_bed ! exponent of excess Shields number for bedload
      real(fp) :: cc_sus ! exponent of excess Shields number for suspended load
      real(fp) :: delta ! relative density of sediment particle
      real(fp) :: theta_excess ! help variable for excess Shields number
      real(fp) :: rmu_bed ! ripple factor for bedload
      real(fp) :: rmu_sus ! ripple factor for suspended load
      real(fp) :: theta ! Shields number
      real(fp) :: theta_cr_bed ! critical Shields number for bedload
      real(fp) :: theta_cr_sus ! critical Shields number for suspended load
!
!! executable statements -------------------------------------------------------
!
      sbot = 0.0
      ssus = 0.0
      !
      ag = par(1)
      delta = par(4)
      acal_bed = par(11)
      b_bed = par(12)
      cc_bed = par(13)
      rmu_bed = par(14)
      theta_cr_bed = par(15)
      acal_sus = par(16)
      b_sus = par(17)
      cc_sus = par(18)
      rmu_sus = par(19)
      theta_cr_sus = par(20)
      !
      theta = (utot / chezy)**2 / (delta * d50)
      !
      theta_excess = rmu_bed * theta - hidexp * theta_cr_bed
      sbot = acal_bed * d50**1.5 * sqrt(ag * delta) * theta**b_bed * theta_excess**cc_bed
      !
      theta_excess = rmu_sus * theta - hidexp * theta_cr_sus
      ssus = acal_sus * d50**1.5 * sqrt(ag * delta) * theta**b_sus * theta_excess**cc_sus
      !
      ! general formula specific output
      par = missing_value
      par(1) = chezy
      par(2) = theta
   end subroutine tranb4

end module m_tranb4
