module m_tranb3
   implicit none

contains

   subroutine tranb3(utot, d35, chezy, water_depth, npar, &
                   & par, sbot, ssus)
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
! swanby (ackers white)
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
      real(fp), intent(in) :: d35
      real(fp), intent(in) :: water_depth
      real(fp), dimension(npar), intent(inout) :: par
      real(fp), intent(in) :: utot
      !
      real(fp), intent(out) :: sbot
      real(fp), intent(out) :: ssus
!
!
! Local variables
!
      real(fp) :: a
      real(fp) :: acal
      real(fp) :: ag ! gravity acceleration
      real(fp) :: ccc
      real(fp) :: chezy_grain ! grain related Chezy value
      real(fp) :: cf
      real(fp) :: delta ! relative density of sediment particle
      real(fp) :: dgr
      real(fp) :: f ! real help array
      real(fp) :: fwc
      real(fp) :: rk
      real(fp) :: rm
      real(fp) :: rn
      real(fp) :: u_star
      !
      real(fp), parameter :: MIN_DEPTH = 0.001_fp
!
!
!! executable statements -------------------------------------------------------
!
      sbot = 0.0
      ssus = 0.0
      !
      ag = par(1)
      delta = par(4)
      acal = par(11)
      !rk = par(12) ! obsolete
      !
      cf = ag / chezy / chezy
      u_star = sqrt(cf) * utot
      chezy_grain = 18.0_fp * log10(12.0_fp * max(water_depth, MIN_DEPTH) / d35)
      !
      dgr = 25300.0_fp * d35
      ccc = log(dgr)
      ccc = exp(2.86_fp * ccc - 0.4343_fp * ccc * ccc - 8.128_fp)
      !
      rn = 1.0_fp - 0.2432_fp * log(dgr)
      rm = 9.66_fp / dgr + 1.34_fp
      !
      f = utot**(1.0_fp-rn) * u_star**rn / chezy_grain**(1.0_fp-rn) / ag**(rn / 2.0_fp) / sqrt(delta * d35)
      a = 0.23_fp / sqrt(dgr) + 0.14_fp
      fwc = max((f - a) / a, 0.0_fp)
      !
      sbot = acal * utot * d35 * (utot / max(u_star, 1.0e-12_fp))**rn * ccc * fwc**rm
      ssus = 0.0_fp
      !
      ! Swanby (Ackers-White) specific output
      par = MISSING_VALUE
      par(1) = chezy
      par(2) = u_star
   end subroutine tranb3

end module m_tranb3
