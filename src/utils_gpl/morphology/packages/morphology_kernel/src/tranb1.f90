-module m_tranb1
   implicit none

contains

   subroutine tranb1(utot, d50, chezy, npar, &
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
! engelund hansen
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
      use precision
      implicit none
!
! Arguments
!
      integer, intent(in) :: npar !< length of transport parameter array
      real(fp), intent(in) :: chezy !< Chezy value
      real(fp), intent(in) :: d50 !< mean diameter
      real(fp), dimension(npar), intent(inout) :: par !< transport parameter array
      real(fp), intent(in) :: utot !< velocity magnitude
      !
      real(fp), intent(out) :: sbot !< bed load transport
      real(fp), intent(out) :: ssus !< suspended load transport
!
! Local variables
!
      real(fp) :: acal ! user-specified calibration coefficient
      real(fp) :: ag ! gravity acceleration
      real(fp) :: delta ! relative density of sediment particle
      real(fp) :: suspfac ! user-specified suspended sediment factor
      real(fp) :: total ! total transport (not yet split into bedload and suspended load)
      real(fp) :: theta ! Shields number
!
!! executable statements -------------------------------------------------------
!
      sbot = 0.0_fp
      ssus = 0.0_fp
      !
      ag = par(1)
      delta = par(4)
      acal = par(11)
      !rk = par(12) ! obsolete
      suspfac = par(13)
      !
      ! total transport
      !
      theta = (utot / chezy)**2 / (delta * d50)
      total = 0.05_fp * acal * (chezy**2 / ag) * d50**1.5_fp * sqrt(ag * delta) * theta**2.5_fp
      !
      ! split into bedload and suspended load
      !
      sbot = (1.0_fp - suspfac) * total
      ssus = suspfac * total
      !
      ! Engelund-Hansen specific output
      par = missing_value
      par(1) = chezy
      par(2) = theta

   end subroutine tranb1

end module m_tranb1
