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

module m_density_formulas
   use precision_basics, only: dp

   implicit none

   private

   public :: densfm, add_sediment_effect_to_density
   real(kind=dp), parameter, public :: RHO_MIN = 990.0_dp !< lower limit of density [kg/m3]
   real(kind=dp), parameter, public :: RHO_MAX = 1250.0_dp !< upper limit of density [kg/m3]

   interface densfm
      module procedure calculate_density_from_salinity_and_temperature
      module procedure calculate_density_from_salinity_temperature_and_pressure
   end interface

   public :: rho_eckart
contains

   function calculate_density_from_salinity_and_temperature(salinity, temperature) result(density)
      use precision, only: dp
      use m_physcoef, only: rhomean
      use m_flow, only: idensform

      real(kind=dp), intent(in) :: salinity
      real(kind=dp), intent(in) :: temperature
      real(kind=dp) :: density

      select case (idensform)
      case (0) ! Uniform density
         density = rhomean
      case (1) ! Carl Henry Eckart, 1958
         density = rho_eckart(salinity, temperature)
      case (2) ! Unesco org
         density = rho_unesco(salinity, temperature)
      case (3) ! Unesco83 at surface , call with 0.0_dp for early exit
         density = rho_unesco83(salinity, temperature, 0.0_dp)
      case (5) ! baroclinic instability
         density = 1025.0_dp + 0.78_dp * (salinity - 33.73_dp)
      case (6) ! For Deltares flume experiment IJmuiden , Kees Kuipers saco code 1
         density = 999.904_dp + 4.8292e-2_dp * temperature - 7.2312e-3_dp * temperature**2 + &
                   2.9963e-5_dp * temperature**3 + 7.6427e-1_dp * salinity - &
                   3.1490e-3_dp * salinity * temperature + 3.1273e-5_dp * salinity * temperature**2
      end select
   end function calculate_density_from_salinity_and_temperature

   function calculate_density_from_salinity_temperature_and_pressure(salinity, temperature, pressure) result(density)
      use precision, only: dp
      use m_flow, only: idensform

      real(kind=dp), intent(in) :: salinity
      real(kind=dp), intent(in) :: temperature
      real(kind=dp), intent(in) :: pressure
      real(kind=dp) :: density

      if (abs(idensform) == 13) then ! Unesco83 pressure dependent
         density = rho_unesco83(salinity, temperature, pressure)
      end if
   end function calculate_density_from_salinity_temperature_and_pressure

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
   function rho_unesco83(salinity, temperature, pressure) result(density)
      use precision, only: dp
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
   end function rho_unesco83

   real(kind=dp) function rho_Eckart(sal, temp)
      use precision, only: dp
      real(kind=dp) :: sal, temp
      real(kind=dp) :: cp1, clam1, temp2
      real(kind=dp) :: cp0, clam0, clam

      temp2 = temp * temp
      cp0 = 5890.0d0 + 38.00d0 * temp - 0.3750d0 * temp2
      clam = 1779.5d0 + 11.25d0 * temp - 0.0745d0 * temp2
      clam0 = 3.8d0 + 0.01d0 * temp
      cp1 = cp0 + 3.0d0 * saL
      clam1 = clam - clam0 * saL
      rho_Eckart = 1000.0d0 * cp1 / (0.698d0 * cp1 + clam1)
   end function rho_Eckart

   !> Computes water density from temperature and salinity using equation of state (rhowat).
   !! Equation of state following UNESCO, (UNESCO, Algorithms for computation of fundamental
   !! properties of seawater, UNESCO technical papers in marine science, 1983)
   !! JvK and HK checked this on 12-05-2022, and we found that the correct reference is:
   !! Background papers and supporting data, on the international equation of state of seawater 1980, Unesco 1981
   !! (both years 1980 and 1981 are on cover), formula taken from page 20
   function rho_unesco(salinity, temperature) result(res)
      use precision, only: dp
      implicit none
      real(kind=dp), intent(in) :: salinity
      real(kind=dp), intent(in) :: temperature
      real(kind=dp) :: res

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

      res = rhwa + (asal + bsal * square_root_salinity + csal * salinity) * salinity
   end function rho_unesco

   !> Adds the effect of sediment on the density of a cell
   subroutine add_sediment_effect_to_density(rho, cell)
      use precision, only: dp
      use m_sediment, only: jased, jaseddenscoupling, jasubstancedensitycoupling, mxgr, rhosed, sed, stmpar, stm_included
      use m_transport, only: constituents, ised1, itra1, itran
      use m_turbulence, only: rhowat
      use sediment_basics_module, only: has_advdiff
      use messagehandling, only: LEVEL_ERROR, mess
      use unstruc_model, only: check_positive_value

      implicit none

      real(kind=dp), intent(inout) :: rho !< density in a cell [kg/m3]
      integer, intent(in) :: cell !< cell index
      real(kind=dp), parameter :: SEDIMENT_DENSITY = 2600.0_dp !< default/typical sediment density [kg/m3]
      real(kind=dp) :: rhom !< density in a cell [kg/m3] before adding sediment effects
      integer :: i, lsed !< loop indices

      if (jased > 0 .and. stm_included) then
         rhom = rho ! UNST-5170 for mor, only use salt+temp, not sediment effect
         rhom = min(rhom, RHO_MAX) ! check overshoots at thin water layers
         rhom = max(rhom, RHO_MIN) !
         rhowat(cell) = rhom
         if (stmpar%morpar%densin) then ! sediment density effects
            i = ised1
            rhom = rho
            do lsed = 1, stmpar%lsedtot
               if (has_advdiff(stmpar%sedpar%tratyp(lsed))) then ! has suspended component
                  rho = rho + constituents(i, cell) * (stmpar%sedpar%rhosol(lsed) - rhom) / stmpar%sedpar%rhosol(lsed)
                  i = i + 1
               end if
            end do
         end if
      else if (jasubstancedensitycoupling > 0) then ! for now, only works for DELWAQ sediment fractions (concentrations in g/m3 and density of SEDIMENT_DENSITY)
         if (itra1 == 0) then
            call mess(LEVEL_ERROR, 'SubstanceDensityCoupling was set to 1, but there are no substances.')
         end if
         rhom = rho
         do i = itra1, itran
            rho = rho + (1d-3) * constituents(i, cell) * (SEDIMENT_DENSITY - rhom) / SEDIMENT_DENSITY
         end do
      else if (jaseddenscoupling > 0) then ! jased < 4
         rhom = rho
         do i = 1, mxgr
            call check_positive_value('rhosed', rhosed(i))
            rho = rho + sed(i, cell) * (rhosed(i) - rhom) / rhosed(i)
         end do

      end if
   end subroutine add_sediment_effect_to_density

end module m_density_formulas

!   subroutine checkunesco83()
!      use precision, only: dp
!      real(kind=dp) :: sal, tem, pres, dum0, dum1, dum2, rho_u
!
!      write (*, *) 'rhounesco83 at 0 m and 10 km depth '
!
!      sal = 30.0_dp; tem = 30.0_dp; pres = 0.0_dp * 1d5
!      dum0 = rho_unesco83(sal, tem, pres)
!
!      sal = 30.0_dp; tem = 30.0_dp; pres = 1.0_dp * 1d5
!      dum1 = rho_unesco83(sal, tem, pres)
!
!      sal = 8.0_dp; tem = 10.0_dp; pres = 10.0_dp * 1d5
!      dum2 = rho_unesco83(sal, tem, pres)
!
!      sal = 0.0_dp; tem = 0.0_dp; pres = 0.0_dp * 1d5; rho_u = rho_unesco83(sal, tem, pres)
!      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u
!      sal = 0.0_dp; tem = 0.0_dp; pres = 1000.0_dp * 1d5; rho_u = rho_unesco83(sal, tem, pres)
!      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u
!
!      sal = 40.0_dp; tem = 0.0_dp; pres = 000.0_dp * 1d5; rho_u = rho_unesco83(sal, tem, pres)
!      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u
!      sal = 40.0_dp; tem = 0.0_dp; pres = 1000.0_dp * 1d5; rho_u = rho_unesco83(sal, tem, pres)
!      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u
!
!      sal = 00.0_dp; tem = 40.0_dp; pres = 000.0_dp * 1d5; rho_u = rho_unesco83(sal, tem, pres)
!      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u
!      sal = 00.0_dp; tem = 40.0_dp; pres = 1000.0_dp * 1d5; rho_u = rho_unesco83(sal, tem, pres)
!      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u
!
!      sal = 40.0_dp; tem = 40.0_dp; pres = 000.0_dp * 1d5; rho_u = rho_unesco83(sal, tem, pres)
!      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u
!      sal = 40.0_dp; tem = 40.0_dp; pres = 1000.0_dp * 1d5; rho_u = rho_unesco83(sal, tem, pres)
!      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u
!
!   end subroutine checkunesco83