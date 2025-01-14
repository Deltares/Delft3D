!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

module m_tauwavehk

   implicit none

   private

   public :: tauwavehk

contains

   subroutine tauwavehk(Hrms, Tsig, Depth, Uorbi, rlabd, ust)
      use precision, only: dp
      use m_getwavenr, only: getwavenr
      use m_flow, only: rhog
      use m_sferic
      use m_waves, only: gammax, jauorb
      use m_drawthis

      implicit none
      real(kind=dp) :: Hrms, Tsig, Depth, uorbi, hrm, ust
      real(kind=dp) :: hk, sh2hk, hksh2, asg, ew, sxx, syy, sxy, syx, shs, cp, cg, omeg
      real(kind=dp) :: rk, rkx, rky, cgcp, rk2cgcp, cgcp5, arms, rlabd
      real(kind=dp), external :: tanhsafe, sinhsafe, sinhsafei

      if (depth < 0.01d0 .or. Tsig < 0.1d0) then ! flume cases with wave nr 5
         Uorbi = 0d0; rlabd = 0d0; ust = 0d0
      else
         call getwavenr(depth, tsig, rk)
         hrm = min(Hrms, gammax * depth)
         arms = 0.5d0 * hrm
         omeg = twopi / tsig
         shs = sinhsafei(rk * depth)
         uorbi = omeg * arms * shs !omeg*(0.5*hsig)
         if (jauorb == 0) then ! for consistency with old d3d convention
            uorbi = uorbi * sqrt(pi) / 2d0
         end if
         ust = 0.5d0 * omeg * arms * arms / depth
         rlabd = twopi / rk
      end if

      return

      if (ndraw(28) > 40) then
         omeg = twopi / tsig ! omega
         cp = omeg / rk ! fase velocity
         hk = rk * depth ! kh
         sh2hk = sinhsafei(2d0 * hk) ! 1/sinh(2hk)
         hksh2 = hk * sh2hk ! kh/sinh(2kh)
         cgcp = 0.5d0 + hksh2 ! cg/cp
         cg = cp * cgcp ! group velocity
         asg = 0.5d0 * hrms ! rms wave amplitude
         ew = 0.5d0 * rhog * asg * asg ! wave energy
         !ustokes(z) =       rk*omeg*asg*asg*exp(2d0*rk*z)                         ! Vertical Stokes drift profile deep water
         !ustokes    =    0.5d0*omeg*asg*asg/depth                                 ! deep water vertical averaged
         !ustokes(z) = 0.5d0*rk*omeg*asg*asg*cosh(2d0*rk*(z+depth)/sinh2(rk*depth) ! Stokes drift profile  (5) Monismithetal2007.pdf

         Sxx = ew * (0.5d0 + 2d0 * hksh2) ! radiation stress in wave dir
         Syy = ew * hksh2 ! radiation stress perpendicular to wave dir

         rk2cgcp = rk * rk * cgcp ! or, Wikipedia
         cgcp5 = cgcp - 0.5d0
         Sxx = ew * (rkx * rkx / rk2cgcp + cgcp5)
         Syy = ew * (rky * rky / rk2cgcp + cgcp5)
         Syx = ew * (rkx * rky / rk2cgcp + 0d0)
         Sxy = Syx

         ! standard deviation or RMS of sine wave a*sin(om*t) : 0.5*sqrt(2)*a
         ! Hsig or HRMS is equal to 4 times RMS
         ! Hsig = 4*0.5*sqrt(2)*a = 2*sqrt(2)*a = 2.8*a

      end if

   end subroutine tauwavehk

end module m_tauwavehk
