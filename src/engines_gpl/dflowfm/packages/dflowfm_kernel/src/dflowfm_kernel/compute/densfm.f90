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

module m_densfm

   implicit none

   private

   public :: densfm

contains

   real(kind=dp) function densfm(sal, temp, p0)
      use precision, only: dp
      use m_physcoef
      use m_flow
      use m_rho_eckart, only: rho_eckart
      use m_rho_unesco, only: rho_unesco

      real(kind=dp) :: sal, temp, p0

      if (idensform == 0) then ! Uniform density
         densfm = rhomean
         return
      else if (abs(idensform) == 1) then ! Carl Henry Eckart, 1958
         densfm = rho_Eckart(sal, temp)
      else if (abs(idensform) == 2) then ! Unesco org
         densfm = rho_Unesco(sal, temp)
      else if (abs(idensform) == 3) then ! Unesco83 at surface , call with 0d0 for early exit
         densfm = rho_unesco83(sal, temp, 0d0)
      else if (abs(idensform) == 13) then ! Unesco83 pressure dependent
         densfm = rho_unesco83(sal, temp, p0)
      else if (abs(idensform) == 4) then ! Teos10 at surface
         ! densfm = rho_Teos10(sal,temp,1d5)
      else if (abs(idensform) == 14) then ! Teos10 pressure dependent
         ! densfm = rho_Teos10(sal,temp,p0)
      else if (abs(idensform) == 5) then ! baroclinic instability
         densfm = 1025d0 + 0.78d0 * (sal - 33.73d0)
      else if (abs(idensform) == 6) then ! For Deltares flume experiment IJmuiden , Kees Kuipers saco code 1
         densfm = 999.904d0 + 4.8292d-2 * temp - 7.2312d-3 * temp**2 + &
                  2.9963d-5 * temp**3 + 7.6427d-1 * sal - &
                  3.1490d-3 * sal * temp + 3.1273d-5 * sal * temp**2
      end if
   end function densfm

   subroutine checkunesco83()
      use precision, only: dp
      real(kind=dp) :: sal, tem, pres, dum0, dum1, dum2, rho_u

      write (*, *) 'rhounesco83 at 0 m and 10 km depth '

      sal = 30d0; tem = 30d0; pres = 0d0 * 1d5
      dum0 = rho_unesco83(sal, tem, pres)

      sal = 30d0; tem = 30d0; pres = 1d0 * 1d5
      dum1 = rho_unesco83(sal, tem, pres)

      sal = 8d0; tem = 10d0; pres = 10d0 * 1d5
      dum2 = rho_unesco83(sal, tem, pres)

      sal = 0d0; tem = 0d0; pres = 0d0 * 1d5; rho_u = rho_unesco83(sal, tem, pres)
      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u
      sal = 0d0; tem = 0d0; pres = 1000d0 * 1d5; rho_u = rho_unesco83(sal, tem, pres)
      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u

      sal = 40d0; tem = 0d0; pres = 000d0 * 1d5; rho_u = rho_unesco83(sal, tem, pres)
      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u
      sal = 40d0; tem = 0d0; pres = 1000d0 * 1d5; rho_u = rho_unesco83(sal, tem, pres)
      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u

      sal = 00d0; tem = 40d0; pres = 000d0 * 1d5; rho_u = rho_unesco83(sal, tem, pres)
      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u
      sal = 00d0; tem = 40d0; pres = 1000d0 * 1d5; rho_u = rho_unesco83(sal, tem, pres)
      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u

      sal = 40d0; tem = 40d0; pres = 000d0 * 1d5; rho_u = rho_unesco83(sal, tem, pres)
      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u
      sal = 40d0; tem = 40d0; pres = 1000d0 * 1d5; rho_u = rho_unesco83(sal, tem, pres)
      write (*, '(4(A,F20.6))') 'sal= ', sal, ' tem= ', tem, ' pres= ', pres, ' rho= ', rho_u

   end subroutine checkunesco83

   real(kind=dp) function rho_unesco83(sal, temp, pres)
      use precision, only: dp
      real(kind=dp) :: sal, temp, pres, sigma, sva

      sva = svan(sal, temp, pres, sigma)

      rho_unesco83 = sigma + 1d3

   end function rho_unesco83

   function svan(S4, T4, P04, SIGMA)
      !==============================================================================!
      ! specific volume anomaly (steric anomaly) based on 1980 equation              |
      ! of state for seawater and 1978 practerical salinity scale.                   |
      ! references:                                                                  |
      ! millero, et al (1980) deep-sea res.,27a,255-264                              |
      ! millero and poisson 1981,deep-sea res.,28a pp 625-629.                       |
      !
      ! This subroutine can be found in: Algorithms for computation of fundamental properties of sea water
      !                                  Unesco 1983
      !                                                                              |
      ! units:                                                                       |
      !       ! pressure        p04       decibars
      !       pressure        p04       Pascal, N/m2
      !       temperature     t4        deg celsius (ipts-68)                        |
      !       salinity        s4        (ipss-78)                                    |
      !       spec. vol. ana. svan     m**3/kg *1.0e-8                               |
      !       density ana.    sigma    kg/m**3                                       |
      !                                                                              |
      ! check value: svan=981.3021 e-8 m**3/kg. for s = 40 (ipss-78),                |
      ! t = 40 deg c, p0= 10000 decibars.                                            |
      ! check value: sigma = 59.82037  kg/m**3. for s = 40 (ipss-78) ,               |
      ! t = 40 deg c, p0= 10000 decibars.                                            |
      !==============================================================================!

      use precision

      implicit none
      real(hP) :: SVAN
      real(hP), intent(IN) :: S4, T4, P04
      real(hP), intent(OUT) :: SIGMA
      real(hP) P4, SIG, SR, RR1, RR2, RR3, V350P, DK
      real(hP) A4, B4, C4, D4, E4, AA1, BB1, AW, BW, K0, KW, K35, SVA
      real(hP) GAM, PK, DVAN, DR35P

      real(hP), parameter :: R3500 = 1028.1063_sp
      real(hP), parameter :: RR4 = 4.8314e-4_sp
      real(hP), parameter :: DR350 = 28.106331_sp

      !   rr4 is refered to as  c  in millero and poisson 1981
      ! convert pressure to bars and take square root salinity.

      ! p4=p04/10.0_sp
      p4 = p04 * 1d-5 ! p4(bar), p04(Pascal

      sr = sqrt(abs(s4))

      ! pure water density at atmospheric pressure
      !   bigg p.h.,(1967) br. j. applied physics 8 pp 521-537.
      !

      rr1 = ((((6.536332e-9_sp * t4 - 1.120083e-6_sp) * t4 + 1.001685e-4_sp) * t4 &
              - 9.095290e-3_sp) * t4 + 6.793952e-2_sp) * t4 - 28.263737_sp

      ! seawater density atm press.
      !  coefficients involving salinity
      !  rr2 = a   in notation of millero and poisson 1981

      rr2 = (((5.3875e-9_sp * t4 - 8.2467e-7_sp) * t4 + 7.6438e-5_sp) * t4 - 4.0899e-3_sp) * t4 &
            + 8.24493e-1_sp

      !  rr3 = b4  in notation of millero and poisson 1981

      rr3 = (-1.6546e-6_sp * t4 + 1.0227e-4_sp) * t4 - 5.72466e-3_sp

      !  international one-atmosphere equation of state of seawater

      sig = (rr4 * s4 + rr3 * sr + rr2) * s4 + rr1

      ! specific volume at atmospheric pressure

      v350p = 1.0_sp / r3500
      sva = -sig * v350p / (r3500 + sig)
      sigma = sig + dr350

      !  scale specific vol. anamoly to normally reported units

      svan = sva * 1.0e+8_sp
      if (p4 == 0.0_sp) return

      !-------------------------------------------------------------|
      !    new high pressure equation of sate for seawater          |
      !                                                             |
      !        millero, el al., 1980 dsr 27a, pp 255-264            |
      !        constant notation follows article                    |
      !-------------------------------------------------------------|
      ! compute compression terms

      e4 = (9.1697e-10 * t4 + 2.0816e-8_sp) * t4 - 9.9348e-7_sp
      bw = (5.2787e-8_sp * t4 - 6.12293e-6_sp) * t4 + 3.47718e-5_sp
      b4 = bw + e4 * s4

      d4 = 1.91075e-4
      c4 = (-1.6078e-6_sp * t4 - 1.0981e-5_sp) * t4 + 2.2838e-3_sp
      aw = ((-5.77905e-7_sp * t4 + 1.16092e-4_sp) * t4 + 1.43713e-3_sp) * t4 &
           - 0.1194975_sp
      a4 = (d4 * sr + c4) * s4 + aw

      bb1 = (-5.3009e-4_sp * t4 + 1.6483e-2_sp) * t4 + 7.944e-2_sp
      aa1 = ((-6.1670e-5_sp * t4 + 1.09987e-2_sp) * t4 - 0.603459_sp) * t4 + 54.6746
      kw = (((-5.155288e-5_sp * t4 + 1.360477e-2_sp) * t4 - 2.327105_sp) * t4 &
            + 148.4206_sp) * t4 - 1930.06_sp
      k0 = (bb1 * sr + aa1) * s4 + kw

      ! evaluate pressure polynomial
      !-----------------------------------------------------|
      !   k equals the secant bulk modulus of seawater      |
      !   dk=k(s,t,p)-k(35,0,p)                             |
      !   k35=k(35,0,p)                                     |
      !-----------------------------------------------------|

      dk = (b4 * p4 + a4) * p4 + k0
      k35 = (5.03217e-5_sp * p4 + 3.359406_sp) * p4 + 21582.27_sp
      gam = p4 / k35
      pk = 1.0_sp - gam
      sva = sva * pk + (v350p + sva) * p4 * dk / (k35 * (k35 + dk))

      !  scale specific vol. anamoly to normally reported units

      svan = sva * 1.0e+8_sp
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
      sigma = dr350 + dr35p - dvan ! rho=sigma+1d3

      return
   end function svan

end module m_densfm
