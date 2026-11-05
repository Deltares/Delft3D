!!  Copyright (C)  Stichting Deltares, 2012-2026.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module m_heatf2
   use m_extract_waq_attribute
contains
   subroutine heatf2(pmsa, fl, ipoint, increm, noseg, &
                     noflux, iexpnt, iknmrk, noq1, noq2, &
                     noq3, noq4)

!>       Total heat flux for surface water absolute temperature model

!
!     Description of the module :
!
! Name    T   L I/O   Description                                    Units
! ----    --- -  -    -------------------                            -----
! beta    R*4 1 L Bowens constant                                [mbar/°C]
! cloud   R*4 1 L Cloud coverage (fraction)                            [-]
! cpa     R*4 1 I Specific heat capacity of air                   [J/kg,K]
! cp      R*4 1 I Specific heat capacity of water                [J/kg,°C]
! CwindA  R*4 1 I Coefficient in wind function a                       [-]
! CwindB  R*4 1 I Coefficient in wind function b                       [-]
! CwindC  R*4 1 I Coefficient in wind function c                       [-]
! DeltaT  R*4 1 I Correction factor for TempWa near discharge          [-]
! Depth   R*4 1 I Water depth of segment                               [m]
! dTemp   R*4 1 O Flux on water temperature                         [°C/d]
! Emiss   R*4 1 L Emissivity (colour factor) for the atmosphere        [-]
! EWater  R*4 1 I Emissivity (colour factor) of surface water          [-]
! Fa      R*4 1 I Reflection factor for Qa                             [-]
! Fsw     R*4 1 I Reflection factor for Qsw                            [-]
! Fwind   R*4 1 L Wind function for evaporation                 [m/s,mbar]
! Fwind2  R*4 1 L Wind function for evaporation, incl corr.    [W/m2,mbar]
! Ha      R*4 1 I Height of wind speed needed for wind function        [m]
! Hm      R*4 1 I Height at which wind speed is measured               [m]
! HTVap   R*4 1 L Heat of evaporation                               [J/kg]
! HTVap   R*4 1 L Heat of evaporation at reference temperature      [J/kg]
! K       R*4 1 I Roughness coef for calculation vwind                 [-]
! Patm    R*4 1 I Air pressure                                 [mb of HPa]
! Pvap    R*4 1 L Actual vapour pressure in air                     [mbar]
! Psvap   R*4 1 L Saturated vapour pressure in air                  [mbar]
! PvapWa  R*4 1 L Saturated vapour pressure at water-air interface  [mbar]
! Qa      R*4 1 L Long wave atmospheric radiation reaching water    [W/m2]
! Qan     R*4 1 L Non refl. (net) atm. long wave rad. reaching water[W/m2]
! Qbr     R*4 1 L Long wave back radiation from water               [W/m2]
! Ql      R*4 1 L Latent heat flux by evaporation or condensation   [W/m2]
! Qrb     R*4 1 I Heat exchange between water and riverbed          [W/m2]
! Qsg     R*4 1 L Sensible heat of conduction                       [W/m2]
! Qsn     R*4 1 L Non refl. (net) short wave rad. reaching water    [W/m2]
! Qsw     R*4 1 L Short wave radiation reaching water               [W/m2]
! Qt      R*4 1 L Total heat flux                                   [W/m2]
! RatBow  R*4 1 L Bowens ratio                                         [-]
! RelHum  R*4 1 I Relative air humidity                                [%]
! Rho0    R*4 1 I Density of surface water at 4°C                  [kg/m3]
! RhoRef  R*4 1 L Density of surface water at reference temperature[kg/m3]
! SBC     R*4 1 I StefanBolzman Constant                        [W/m2/K-4]
! SunFac  R*4 1 I Percentage sunshine                                  [%]
! SWEmis  R*4 1 I Switch for calculation of emissivity                 [-]
! TempAt  R*4 1 I Air temperature                                     [°C]
! TempWa  R*4 1 I Surface water temperature                           [°C]
! Tref    R*4 1 I Reference water temperature for wind function       [°C]
! Vevap   R*4 1 L Evaporation rate                                   [m/s]
! VWinda  R*4 1 L Calculated wind speed                              [m/s]
! VWindm  R*4 1 I Measured wind speed                                [m/s]

      implicit none
      real pmsa(*), fl(*)
      integer ipoint(*), increm(*), noseg, noflux, &
         iexpnt(4, *), iknmrk(*), noq1, noq2, noq3, noq4, &
         iseg, iflux, ikmrk1, ikmrk2
      integer ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, ip9, ip10, &
         ip11, ip12, ip13, ip14, ip15, ip16, ip17, ip18, ip19, &
         ip20, ip21, ip22, ip23, ip24, ip25, ip26, ip27, ip28, &
         ip29, ip30, ip31, ip32, ip33, ip34, ip35, ip36, ip37, &
         ip38, ip39, ip40
      integer iswtemp, iswemis
      real Qsw, Fsw, Pvap, TempAt, SBC, Fa, EWater, &
         TempWa, RhoWat, CWindA, CWindB, CWindC, VWindm, K, &
         PvapWa, Cpa, Patm, Qsn, Cloud, Emiss, Qa, Qan, Qbr, C, &
         HTVap, Fwind, Ql, Beta, Qsg, Qt, Qrb, Tref, &
         dTemp, cp, DEPTH, RelHum, Psvap, Hm, Ha, Vevap, Fwind2, &
         VWinda, DeltaT, HtVRef, RhoRef, Rho0, ModTemp, mindeptht

      ip1 = ipoint(1)
      ip2 = ipoint(2)
      ip3 = ipoint(3)
      ip4 = ipoint(4)
      ip5 = ipoint(5)
      ip6 = ipoint(6)
      ip7 = ipoint(7)
      ip8 = ipoint(8)
      ip9 = ipoint(9)
      ip10 = ipoint(10)
      ip11 = ipoint(11)
      ip12 = ipoint(12)
      ip13 = ipoint(13)
      ip14 = ipoint(14)
      ip15 = ipoint(15)
      ip16 = ipoint(16)
      ip17 = ipoint(17)
      ip18 = ipoint(18)
      ip19 = ipoint(19)
      ip20 = ipoint(20)
      ip21 = ipoint(21)
      ip22 = ipoint(22)
      ip23 = ipoint(23)
      ip24 = ipoint(24)
      ip25 = ipoint(25)
      ip26 = ipoint(26)
      ip27 = ipoint(27)
      ip28 = ipoint(28)
      ip29 = ipoint(29)
      ip30 = ipoint(30)
      ip31 = ipoint(31)
      ip32 = ipoint(32)
      ip33 = ipoint(33)
      ip34 = ipoint(34)
      ip35 = ipoint(35)
      ip36 = ipoint(36)
      ip37 = ipoint(37)
      ip38 = ipoint(38)
      ip39 = ipoint(39)
      ip40 = ipoint(40)
!
      iflux = 0
      do iseg = 1, noseg

!     Heat exchange only for active water segments
!
         call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
         if (ikmrk1 == 1) then
!
!     Heat exchange only for top layer segments
!
            call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
            if (ikmrk2 == 0 .or. ikmrk2 == 1) then
!
               Qsw = pmsa(ip1)
               Fsw = pmsa(ip2)
               Cloud = pmsa(ip3) / 100.0
               ISWEmis = nint(pmsa(ip4))
               TempAt = pmsa(ip5)
               SBC = pmsa(ip6)
               Fa = pmsa(ip7)
               Ewater = pmsa(ip8)
               TempWa = pmsa(ip9)
               Rho0 = pmsa(ip10)
               CWinda = pmsa(ip11)
               CWindb = pmsa(ip12)
               CWindc = pmsa(ip13)
               VWindm = pmsa(ip14)
               hm = pmsa(ip15)
               ha = pmsa(ip16)
               k = pmsa(ip17)
               cpa = pmsa(ip18)
               Patm = pmsa(ip19)
               Qrb = pmsa(ip20)
               Cp = pmsa(ip21)
               DEPTH = pmsa(ip22)
               RelHum = pmsa(ip23) / 100.0
               DeltaT = pmsa(ip24)
               Tref = pmsa(ip25)
               ISWTEMP = nint(pmsa(ip26))
               mindeptht = pmsa(ip27)
               ! limiter
               TempWa = min(50., max(TempWa, -50.))

!
!     ------Short wave solar radiation-----------
!
               Qsn = Qsw * (1 - Fsw)
!
!     ------Long wave atmospheric radiation------
!
               Psvap = 6.131 + 0.467 * TempAt + 0.0089 * &
                       TempAt**2.0 + 0.000527 * TempAt**3.0
               Psvap = max(Psvap, 0.001)
!
               Pvap = RelHum * Psvap
!
!           ----1.  Option 1 = Brunt, 1932
!
               if (ISWEmis == 1) then
!
                  Emiss = (0.51 + 0.066 * sqrt(Pvap)) * &
                          (1.0 + 0.17 * cloud**2.0)
!
                  Qa = Emiss * SBC * (TempAt + 273.15)**4.0
!
!           ----2.  Option 2 = Edinger, 1965 and Koberg, 1962
!
               elseif (ISWEmis == 2) then
!
!           Pascal heeft formulering voor C (Brunt coefficient)
!           aangevraagd, wordt later ingevuld...
!
                  C = 1
!
                  Emiss = 1.1 * C + 0.030 * sqrt(Pvap)
!
                  Qa = Emiss * SBC * (TempAt + 273.15)**4.0
!
!           ----3.  Option 3 = Edinger, 1965
!
               elseif (ISWEmis == 3) then
!
                  Emiss = 0.74 * (1 + 0.17 * cloud) + &
                          0.0045 * (1 - 0.4 * cloud) * &
                          Pvap
!
                  Qa = Emiss * SBC * (TempAt + 273.15)**4.0
!
!           ----4.  Option 4 = Ludikhuize, 1994 as in WAQUA
!
               elseif (ISWEmis == 4) then
!
                  Qa = (218.0 + 6.3 * TempAt) * &
                       (1.0 + 0.17 * cloud**2.0)
!
               else
               end if
!
               Qan = Qa * (1 - Fa)
!
!     ------Long wave back radiation from water------
!
               Qbr = Ewater * SBC * ((TempWa + DeltaT + 273.15)**4.0)
!
!     ------Latent heat (evaporation or condensation)-----
!
               RhoWat = Rho0 * (1.0 - 7.17e-6 * &
                                (TempWa + DeltaT - 4.0)**2.0)
!
               RhoRef = Rho0 * (1.0 - 7.17e-6 * &
                                (Tref - 4.0)**2.0)
!
               HtVap = 2.5e+06 - 2300.0 * (TempWa + DeltaT)
!
               HtVRef = 2.5e+06 - 2300.0 * Tref
!
               PvapWa = 6.131 + 0.467 * (TempWa + DeltaT) + 0.0089 * &
                        (TempWa + DeltaT)**2.0 + 0.000527 * &
                        (TempWa + DeltaT)**3.0
!
               if (abs(Hm - Ha) < 0.0001) then
                  VWinda = VWindm
               else
                  if ((Hm > (10 * k)) .and. (Ha > (10 * k))) then
                     VWinda = VWindm * log(Hm / k) / log(Ha / k)
                  else
                     VWinda = VWindm
                  end if
               end if
!
               FWind = CwindA + CwindB * VWinda + CwindC * VWinda**2.0
!
               FWind2 = Fwind / (RhoRef * HtVRef)
!
               Vevap = FWind2 * (PvapWa - Pvap)
!
               Ql = RhoWat * HtVap * Vevap
!
!     ------Convective heat-----------------------------
!
               beta = (cpa * Patm) / (0.62 * HtVap)
!
               Qsg = beta * (TempWa + DeltaT - TempAt) * RhoWat * HtVap * Fwind2
!
!     ------Convective heat to river bed----------------
!           formulations can be included later...
!
!     ------Total heat flux-----------------------------
!
               Qt = Qsn + Qan - Qbr - Ql - Qsg + Qrb
!
!     ------Change of water temperature-----------------
!
               dTemp = (Qt * 86400.0) / (RhoWat * Cp * Depth)

!     ------For output to exces temperature moddeling
!
               if (ISWTEMP == 0) then
                  ModTemp = TempWa
               else
                  ModTemp = 0.0
               end if

               pmsa(ip28) = Qsn
               pmsa(ip29) = Psvap
               pmsa(ip30) = Emiss
               pmsa(ip31) = Qan
               pmsa(ip32) = Qbr
               pmsa(ip33) = Ql
               pmsa(ip34) = Qsg
               pmsa(ip35) = Qt
               pmsa(ip36) = PvapWa
               pmsa(ip37) = VWinda
               pmsa(ip38) = Vevap
               pmsa(ip39) = RhoWat
               pmsa(ip40) = ModTemp

               if (depth < mindeptht) then

                  ! depth less then mindeptht no fluxes

                  dTemp = 0.0

               end if
               if (TempWa < 0.0) then

                  ! no cooling below 0 degrees

                  dTemp = max(dTemp, 0.0)

               end if
!
            else
!
!           For non top layer segments no heat flux is calculated
!
               dTemp = 0.0
!
            end if

            ! move this inside endif for BTEST
            if (isnan(dTemp)) write (*, *) 'dtemp = nan in ', iseg
            fl(1 + iflux) = dTemp
!
         end if

         iflux = iflux + noflux
         ip1 = ip1 + increm(1)
         ip2 = ip2 + increm(2)
         ip3 = ip3 + increm(3)
         ip4 = ip4 + increm(4)
         ip5 = ip5 + increm(5)
         ip6 = ip6 + increm(6)
         ip7 = ip7 + increm(7)
         ip8 = ip8 + increm(8)
         ip9 = ip9 + increm(9)
         ip10 = ip10 + increm(10)
         ip11 = ip11 + increm(11)
         ip12 = ip12 + increm(12)
         ip13 = ip13 + increm(13)
         ip14 = ip14 + increm(14)
         ip15 = ip15 + increm(15)
         ip16 = ip16 + increm(16)
         ip17 = ip17 + increm(17)
         ip18 = ip18 + increm(18)
         ip19 = ip19 + increm(19)
         ip20 = ip20 + increm(20)
         ip21 = ip21 + increm(21)
         ip22 = ip22 + increm(22)
         ip23 = ip23 + increm(23)
         ip24 = ip24 + increm(24)
         ip25 = ip25 + increm(25)
         ip26 = ip26 + increm(26)
         ip27 = ip27 + increm(27)
         ip28 = ip28 + increm(28)
         ip29 = ip29 + increm(29)
         ip30 = ip30 + increm(30)
         ip31 = ip31 + increm(31)
         ip32 = ip32 + increm(32)
         ip33 = ip33 + increm(33)
         ip34 = ip34 + increm(34)
         ip35 = ip35 + increm(35)
         ip36 = ip36 + increm(36)
         ip37 = ip37 + increm(37)
         ip38 = ip38 + increm(38)
         ip39 = ip39 + increm(39)
         ip40 = ip40 + increm(40)

      end do
   end subroutine heatf2
end module m_heatf2
