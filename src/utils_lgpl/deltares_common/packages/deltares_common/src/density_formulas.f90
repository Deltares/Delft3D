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

module m_density_formulas
   use precision_basics, only: dp

   implicit none

   private

   integer, parameter, public :: DENSITY_OPTION_UNIFORM = 0 !< Uniform density
   integer, parameter, public :: DENSITY_OPTION_ECKART = 1 !< Carl Henry Eckart, 1958
   integer, parameter, public :: DENSITY_OPTION_UNESCO = 2 !< Unesco org
   integer, parameter, public :: DENSITY_OPTION_UNESCO83 = 3 !< Unesco83
   integer, parameter, public :: DENSITY_OPTION_BAROCLINIC = 5 !< Baroclinic instability
   integer, parameter, public :: DENSITY_OPTION_DELTARES_FLUME = 6 !< For Deltares flume experiment IJmuiden, Kees Kuipers saco code 1

   public :: calculate_density_unesco83, calculate_density_eckart, calculate_density_unesco, calculate_density_nacl, calculate_density_baroclinic
   public :: derivative_density_to_salinity_eckart, derivative_density_to_temperature_eckart
contains

   !> Calculate the density from the specific volume anomaly.
   !! Specific volume anomaly (steric anomaly) based on 1980 equation
   !! of state for seawater and 1978 practerical salinity scale.
   !! references:
   !! millero, et al (1980) deep-sea res.,27a,255-264
   !! millero and poisson 1981,deep-sea res.,28a pp 625-629.
   !!
   !! This subroutine can be found in: Algorithms for computation of fundamental properties of sea water
   !!                                  Unesco 1983
   !!
   !! units:
   !!       pressure        p04       Pascal, N/m2
   !!       temperature     t4        deg celsius (ipts-68)
   !!       salinity        s4        (ipss-78)
   !!       spec. vol. ana. svan     m**3/kg *1.0e-8
   !!       density ana.    sigma    kg/m**3
   !!
   !! check value: svan=981.3021 e-8 m**3/kg. for s = 40 (ipss-78),
   !! t = 40 deg c, p0= 10000 decibars.
   !! check value: sigma = 59.82037  kg/m**3. for s = 40 (ipss-78) ,
   !! t = 40 deg c, p0= 10000 decibars.
   pure function calculate_density_unesco83(salinity, temperature, pressure) result(density)
      implicit none
      real(kind=dp), intent(in) :: salinity
      real(kind=dp), intent(in) :: temperature
      real(kind=dp), intent(in) :: pressure
      real(kind=dp) :: density

      real(kind=dp) :: p4, sig, sr, rr1, rr2, rr3, v350p, dk
      real(kind=dp) :: a4, b4, c4, d4, e4, aa1, bb1, aw, bw, k0, kw, k35, sva
      real(kind=dp) :: gam, pk, dvan, dr35p

      real(kind=dp), parameter :: R3500 = 1028.1063_dp
      real(kind=dp), parameter :: RR4 = 4.8314e-4_dp
      real(kind=dp), parameter :: DR350 = 28.106331_dp

      ! RR4 is refered to as c in millero and poisson 1981
      ! convert pressure to bars and take square root salinity.

      ! p4=pressure/10.0_dp
      p4 = pressure * 1e-5_dp ! p4(bar), pressure(Pascal)

      sr = sqrt(abs(salinity))

      ! pure water density at atmospheric pressure
      !   bigg p.h.,(1967) br. j. applied physics 8 pp 521-537.
      !

      rr1 = ((((6.536332e-9_dp * temperature - 1.120083e-6_dp) * temperature + 1.001685e-4_dp) * temperature &
              - 9.095290e-3_dp) * temperature + 6.793952e-2_dp) * temperature - 28.263737_dp

      ! seawater density atm press.
      !  coefficients involving salinity
      !  rr2 = a   in notation of millero and poisson 1981

      rr2 = (((5.3875e-9_dp * temperature - 8.2467e-7_dp) * temperature + 7.6438e-5_dp) * temperature - 4.0899e-3_dp) * temperature &
            + 8.24493e-1_dp

      !  rr3 = b4  in notation of millero and poisson 1981

      rr3 = (-1.6546e-6_dp * temperature + 1.0227e-4_dp) * temperature - 5.72466e-3_dp

      !  international one-atmosphere equation of state of seawater

      sig = (RR4 * salinity + rr3 * sr + rr2) * salinity + rr1

      ! specific volume at atmospheric pressure

      v350p = 1.0_dp / R3500
      sva = -sig * v350p / (R3500 + sig)

      if (pressure == 0.0_dp) then
         density = sig + DR350 + 1e3_dp ! rho=sigma+1e3
         return
      end if

      !-------------------------------------------------------------|
      !    new high pressure equation of sate for seawater          |
      !                                                             |
      !        millero, el al., 1980 dsr 27a, pp 255-264            |
      !        constant notation follows article                    |
      !-------------------------------------------------------------|
      ! compute compression terms

      e4 = (9.1697e-10 * temperature + 2.0816e-8_dp) * temperature - 9.9348e-7_dp
      bw = (5.2787e-8_dp * temperature - 6.12293e-6_dp) * temperature + 3.47718e-5_dp
      b4 = bw + e4 * salinity

      d4 = 1.91075e-4_dp
      c4 = (-1.6078e-6_dp * temperature - 1.0981e-5_dp) * temperature + 2.2838e-3_dp
      aw = ((-5.77905e-7_dp * temperature + 1.16092e-4_dp) * temperature + 1.43713e-3_dp) * temperature &
           - 0.1194975_dp
      a4 = (d4 * sr + c4) * salinity + aw

      bb1 = (-5.3009e-4_dp * temperature + 1.6483e-2_dp) * temperature + 7.944e-2_dp
      aa1 = ((-6.1670e-5_dp * temperature + 1.09987e-2_dp) * temperature - 0.603459_dp) * temperature + 54.6746
      kw = (((-5.155288e-5_dp * temperature + 1.360477e-2_dp) * temperature - 2.327105_dp) * temperature &
            + 148.4206_dp) * temperature - 1930.06_dp
      k0 = (bb1 * sr + aa1) * salinity + kw

      ! evaluate pressure polynomial
      !-----------------------------------------------------|
      !   k equals the secant bulk modulus of seawater      |
      !   dk=k(s,t,p)-k(35,0,p)                             |
      !   k35=k(35,0,p)                                     |
      !-----------------------------------------------------|

      dk = (b4 * p4 + a4) * p4 + k0
      k35 = (5.03217e-5_dp * p4 + 3.359406_dp) * p4 + 21582.27_dp
      gam = p4 / k35
      pk = 1.0_dp - gam
      sva = sva * pk + (v350p + sva) * p4 * dk / (k35 * (k35 + dk))
      v350p = v350p * pk

      !----------------------------------------------------------|
      ! compute density anamoly with respect to 1000.0 kg/m**3   |
      !  1) dr350: density anamoly at 35 (ipss-78),              |
      !                               0 deg. c and 0 decibars    |
      !  2) dr35p: density anamoly at 35 (ipss-78),              |
      !                               0 deg. c, pres. variation  |
      !  3) dvan : density anamoly variations involving specific |
      !            volume anamoly                                |
      !                                                          |
      ! check values: sigma = 59.82037 kg/m**3                   |
      ! for s = 40 (ipss-78), t = 40 deg c, p0= 10000 decibars.  |
      !----------------------------------------------------------|

      dr35p = gam / v350p
      dvan = sva / (v350p * (v350p + sva))
      density = DR350 + dr35p - dvan + 1e3_dp ! rho=sigma+1e3
   end function calculate_density_unesco83

   pure subroutine density_eckart_helper(salinity, temperature, clam0, cp1, clam1)
      real(kind=dp), intent(in) :: salinity
      real(kind=dp), intent(in) :: temperature
      real(kind=dp), intent(out) :: clam0
      real(kind=dp), intent(out) :: cp1
      real(kind=dp), intent(out) :: clam1

      real(kind=dp) :: cp0
      real(kind=dp) :: clam

      cp0 = 5890.0_dp + 38.00_dp * temperature - 0.3750_dp * temperature**2
      clam = 1779.5_dp + 11.25_dp * temperature - 0.0745_dp * temperature**2
      clam0 = 3.8_dp + 0.01_dp * temperature
      cp1 = cp0 + 3.0_dp * salinity
      clam1 = clam - clam0 * salinity
   end subroutine density_eckart_helper

   !> Calculate the water density from temperature and salinity using the Eckart formula.
   pure function calculate_density_eckart(salinity, temperature) result(density)
      use precision, only: dp
      real(kind=dp), intent(in) :: salinity
      real(kind=dp), intent(in) :: temperature
      real(kind=dp) :: density

      real(kind=dp) :: cp1, clam1, clam0

      real(kind=dp), parameter :: alph0 = 0.698_dp

      call density_eckart_helper(salinity, temperature, clam0, cp1, clam1)
      density = 1000.0_dp * cp1 / (alph0 * cp1 + clam1)
   end function calculate_density_eckart

   !> Calculate the derivative of the water density to the salinity using the Eckart formula.
   pure function derivative_density_to_salinity_eckart(salinity, temperature) result(derivative)
      use precision, only: dp
      real(kind=dp), intent(in) :: salinity
      real(kind=dp), intent(in) :: temperature
      real(kind=dp) :: derivative

      real(kind=dp), parameter :: cp1ds = 3.0_dp
      real(kind=dp), parameter :: alph0 = 0.698_dp

      real(kind=dp) :: clam0, cp1, clam1, den

      call density_eckart_helper(salinity, temperature, clam0, cp1, clam1)
      den = (alph0 * cp1 + clam1)**2
      derivative = 1000.0_dp * (cp1ds * clam1 + cp1 * clam0) / den
   end function derivative_density_to_salinity_eckart

   !> Calculate the derivative of the water density to the temperature using the Eckart formula.
   pure function derivative_density_to_temperature_eckart(salinity, temperature) result(derivative)
      use precision, only: dp
      real(kind=dp), intent(in) :: salinity
      real(kind=dp), intent(in) :: temperature
      real(kind=dp) :: derivative

      real(kind=dp), parameter :: alph0 = 0.698_dp

      real(kind=dp) :: clam0, cp1, clam1, cp1dt, cladt, den

      call density_eckart_helper(salinity, temperature, clam0, cp1, clam1)
      den = (alph0 * cp1 + clam1)**2
      cp1dt = 38.00_dp - 0.750_dp * temperature
      cladt = 11.25_dp - 0.149_dp * temperature - 0.01_dp * salinity
      derivative = 1000.0_dp * (cp1dt * clam1 - cp1 * cladt) / den
   end function derivative_density_to_temperature_eckart

   !> Computes water density from temperature and salinity using equation of state (rhowat).
   !! Equation of state following UNESCO, (UNESCO, Algorithms for computation of fundamental
   !! properties of seawater, UNESCO technical papers in marine science, 1983)
   !! JvK and HK checked this on 12-05-2022, and we found that the correct reference is:
   !! Background papers and supporting data, on the international equation of state of seawater 1980, Unesco 1981
   !! (both years 1980 and 1981 are on cover), formula taken from page 20
   pure function calculate_density_unesco(salinity, temperature) result(density)
      use precision, only: dp
      implicit none
      real(kind=dp), intent(in) :: salinity
      real(kind=dp), intent(in) :: temperature
      real(kind=dp) :: density

      real(kind=dp) :: square_root_salinity, rhwa, asal, bsal
      real(kind=dp), dimension(0:5), parameter :: cf = &
                                                  [999.842594_dp, 6.793952e-2_dp, -9.095290e-3_dp, 1.001685e-4_dp, -1.120083e-6_dp, 6.536332e-9_dp]
      real(kind=dp), dimension(0:4), parameter :: ca = &
                                                  [8.24493e-1_dp, -4.0899e-3_dp, 7.6438e-5_dp, -8.2467e-7_dp, 5.3875e-9_dp]
      real(kind=dp), dimension(0:2), parameter :: cb = &
                                                  [-5.72466e-3_dp, 1.0227e-4_dp, -1.6546e-6_dp]
      real(kind=dp), parameter :: csal = 4.8314e-4_dp

      square_root_salinity = sqrt(max(0.0_dp, salinity))

      rhwa = cf(0) + cf(1) * temperature + cf(2) * temperature**2 + cf(3) * temperature**3 + cf(4) * temperature**4 + cf(5) * temperature**5
      asal = ca(0) + ca(1) * temperature + ca(2) * temperature**2 + ca(3) * temperature**3 + ca(4) * temperature**4
      bsal = cb(0) + cb(1) * temperature + cb(2) * temperature**2

      density = rhwa + (asal + bsal * square_root_salinity + csal * salinity) * salinity
   end function calculate_density_unesco

   !> Computes water density from temperature and salinity using equation of state (rhowat).
   !! Method used: Equation of state following Millero/Delft Hydraulics, valid for NaCl solutions
   pure function calculate_density_nacl(salinity, temperature) result(density)
      real(kind=dp), intent(in) :: salinity
      real(kind=dp), intent(in) :: temperature
      real(kind=dp) :: density

      density = 999.904_dp + 4.8292e-2_dp * temperature - 7.2312e-3_dp * temperature**2 + &
                2.9963e-5_dp * temperature**3 + 7.6427e-1_dp * salinity - &
                3.1490e-3_dp * salinity * temperature + 3.1273e-5_dp * salinity * temperature**2
   end function calculate_density_nacl

   !> Baroclinic instability
   pure function calculate_density_baroclinic(salinity) result(density)
      real(kind=dp), intent(in) :: salinity
      real(kind=dp) :: density
      density = 1025.0_dp + 0.78_dp * (salinity - 33.73_dp)
   end function calculate_density_baroclinic
end module m_density_formulas
