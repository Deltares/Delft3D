!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_setcdwcoefficient
   use m_getwavenr, only: getwavenr

   implicit none

   private

   public :: setcdwcoefficient

contains

   subroutine setcdwcoefficient(uwi, cd10, L)
      use precision, only: dp
      use m_wind, only: wind_drag_type, cdb, wdb, CD_TYPE_CONST, CD_TYPE_SMITHBANKE_2PT, CD_TYPE_SMITHBANKE_3PT, &
          CD_TYPE_CHARNOCK1955, CD_TYPE_HWANG2005, CD_TYPE_WUEST2003, CD_TYPE_HERSBACH2011, &
          CD_TYPE_CHARNOCK_PLUS_VISCOUS, CD_TYPE_GARRATT1977
      use m_physcoef, only: vonkarw, viskinair
      use m_missing, only: dmiss
      use m_flow, only: ag, hs, jaCdwusp, Cdwusp
      use m_flowgeom, only: ln
      use m_sferic, only: pi, twopi
      use m_waves, only: twav

      implicit none
      integer, intent(in) :: L
      integer :: k1, maxnit = 100, nit, jalightwind = 0
      real(kind=dp) :: uwi, cd10, rk, hsurf = 10.0_dp, ust, z0w
      real(kind=dp) :: omw, cdL2, dkpz0, s, sold, eps = 1.0e-4_dp, awin
      real(kind=dp) :: p = -12.0_dp, pinv = -0.083333_dp, A, A10log, bvis, bfit, balf, r

      if (wind_drag_type == CD_TYPE_CONST) then

         cd10 = cdb(1)

         if (jaCdwusp == 1) then
            if (Cdwusp(L) /= dmiss) then
               cd10 = Cdwusp(L)
            end if
         end if

      else if (wind_drag_type == CD_TYPE_SMITHBANKE_2PT) then

         if (uwi <= wdb(1)) then
            cd10 = cdb(1)
         else if (uwi <= wdb(2)) then
            cd10 = cdb(1) + (uwi - wdb(1)) * (cdb(2) - cdb(1)) / (wdb(2) - wdb(1))
         else
            cd10 = cdb(2)
         end if

      else if (wind_drag_type == CD_TYPE_SMITHBANKE_3PT) then

         if (uwi <= wdb(1)) then
            cd10 = cdb(1)
         else if (uwi <= wdb(2)) then
            cd10 = cdb(1) + (uwi - wdb(1)) * (cdb(2) - cdb(1)) / (wdb(2) - wdb(1))
         else if (uwi <= wdb(3)) then
            cd10 = cdb(2) + (uwi - wdb(2)) * (cdb(3) - cdb(2)) / (wdb(3) - wdb(2))
         else
            cd10 = cdb(3)
         end if

      else if (wind_drag_type == CD_TYPE_CHARNOCK1955) then

         ! Charnock drag coefficient formulation, logarithmic wind velocity profile in the turbulent layer
         ! above the free surface:

         ! uwi      1        z            z=10 m, for U10
         ! ---- = ----- ln (---)
         ! u*      kappa    z0

         ! where u* is the friction velocity, kappa is the Von Karman constant
         ! z is the vertical height above the free surface and z0 is the roughness height:

         ! z0 = b * u*^2/g

         ! with b the dimensionless Charnock coefficient and g the gravity acceleration.
         ! Cd = u*^2/uwi^2, so we have an implicit relation between drag coefficient Cd and wind speed Ws.
         ! Newton-Raphson :

         nit = 0
         s = 19.6_dp
         sold = 0.0_dp

         do while (abs(sold - s) > (eps * s))

            nit = nit + 1
            sold = s
            s = sold * (log(hsurf * ag * sold * sold / (max(0.001_dp, cdb(1) * uwi * uwi))) - 2.0_dp) / (vonkarw * sold - 2.0_dp)

            if (nit >= maxnit) then
               cd10 = 1.0e-3_dp
               exit
            end if

         end do

         if (s > 0.0_dp) then
            cd10 = 1.0_dp / (s * s)
         end if

         !z0w  = cdb(2)*viskinair / ust + cdb(1)*ust2 / ag
         !ust2 = cd10*uwi*uwi
         !z0w  = cdb(1)*ust2/ag

      else if (wind_drag_type == CD_TYPE_HWANG2005) then ! Hwang 2005, wave frequency dependent

         ! (A.)=http://onlinelibrary.wiley.com/doi/10.1029/2005JC002912/full

         k1 = ln(1, L)
         if (uwi < 4.0_dp) then
            awin = max(0.1_dp, uwi)
            cd10 = 0.0044_dp / awin**1.15_dp
         else if (twav(k1) < 0.1_dp) then
            cd10 = 0.00063_dp + 0.000066_dp * uwi
         else
            omw = twopi / max(0.1_dp, twav(k1)) ! wave frequency
            cdL2 = 0.001289_dp * (omw * uwi / ag)**0.815_dp ! Cd at half wavelength above surface(11a)    (A7)

            dkpz0 = pi * exp(-vonkarw / sqrt(CdL2)) ! (5)                                         (A2)
            call getwavenr(hs(k1), twav(k1), rk)
            cd10 = (vonkarw / log(10.0_dp * rk / dkpz0))**2 ! (A4b)
         end if

      else if (wind_drag_type == CD_TYPE_WUEST2003) then ! Wuest 2003 & Smith en Banke, uit Rob's DPM

         if (uwi > 4.0_dp) then
            cd10 = 0.00063_dp + 0.000066_dp * uwi
         else
            awin = max(0.1_dp, uwi)
            cd10 = 0.0044_dp / awin**1.15_dp
         end if

      else if (wind_drag_type == CD_TYPE_HERSBACH2011) then ! Hans Hersbach, 2011, ECMWF fit (Charnock plus viscous term)
         ! Hersbach, H. (2011). Sea surface roughness and drag coefficient as functions of neutral wind speed. Journal of Physical Oceanography, 41(1), 247-251.
         ! https://journals.ametsoc.org/doi/full/10.1175/2010JPO4567.1
         A = (cdb(1) * (vonkarw * uwi)**2) / (ag * hsurf)
         A10log = log(A) ! (2) shows that log actually means: ln
         balf = 2.65_dp - 1.44_dp * A10log - 0.015_dp * A10log * A10log
         R = (hsurf * vonkarw * uwi) / (cdb(2) * viskinair)
         bvis = -1.47_dp + 0.93_dp * log(R)
         bfit = (bvis**p + balf**p)**pinv
         cd10 = (vonkarw / bfit)**2

      else if (wind_drag_type == CD_TYPE_CHARNOCK_PLUS_VISCOUS) then

         ust = uwi / 25.0_dp
         do nit = 1, 10 ! good for about 8 decimals of cd10
            z0w = cdb(1) * ust * ust / ag + cdb(2) * viskinair / ust
            ust = uwi * vonkarw / log(hsurf / z0w)
         end do
         cd10 = (ust / uwi)**2

      else if (wind_drag_type == CD_TYPE_GARRATT1977) then ! Garratt, J. R., 1977: Review of Drag Coefficients over Oceans and Continents. Mon. Wea. Rev., 105, 915-929.

         cd10 = min(1.0e-3_dp * (0.75_dp + 0.067_dp * uwi), 0.0035_dp)

      end if

      if (jalightwind == 1 .and. wind_drag_type /= CD_TYPE_CHARNOCK_PLUS_VISCOUS .and. wind_drag_type /= CD_TYPE_HERSBACH2011 .and. wind_drag_type /= CD_TYPE_WUEST2003 .and. wind_drag_type /= CD_TYPE_HWANG2005) then
         if (uwi < 4.0_dp) then ! for wind < 4 m/s use wuest anyway
            awin = max(0.1_dp, uwi)
            cd10 = max(cd10, 0.0044_dp / awin**1.15_dp)
         end if
      end if
   end subroutine setcdwcoefficient

end module m_setcdwcoefficient
