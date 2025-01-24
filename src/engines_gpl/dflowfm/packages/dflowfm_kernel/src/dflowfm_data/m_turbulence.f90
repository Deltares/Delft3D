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

module m_turbulence
   use precision, only: dp

   implicit none

   ! Coefficients of k-e model:
   real(kind=dp) :: cmukep
   real(kind=dp) :: sqcmukep
   real(kind=dp) :: sqcmukepi
   real(kind=dp) :: cewall
   real(kind=dp) :: cde
   real(kind=dp) :: c1e
   real(kind=dp) :: c2e
   real(kind=dp) :: sigdif
   real(kind=dp) :: sigtke, sigtkei
   real(kind=dp) :: sigeps, sigepsi
   real(kind=dp) :: sigrho

   real(kind=dp) :: c1t
   real(kind=dp) :: c2t
   real(kind=dp) :: c3tsta
   real(kind=dp) :: c3tuns

   real(kind=dp) :: coefn2
   real(kind=dp) :: skmy
   real(kind=dp) :: a1ph
   real(kind=dp) :: a2
   real(kind=dp) :: b1
   real(kind=dp) :: b2
   real(kind=dp) :: c1
   real(kind=dp) :: e1
   real(kind=dp) :: e2
   !real(kind=dp) :: e2     = e1-1+vonkar**2*b1**0.6667/skmy
   real(kind=dp) :: ghmin
   real(kind=dp) :: ghmax

   !quasi-equilibrium coefficients:
   !real(kind=dp) :: csb1   = a2*(1-6*a1ph/b1)
   !real(kind=dp) :: csb2   = 3*a2*(6*a1ph+b2)
   !real(kind=dp) :: csu1   = a1*(1-3*c1-6*a1ph/b1)
   !real(kind=dp) :: csu2   = 3*a1ph*a2*((b2-3*a2)*(1-6*a1ph/b1)-3*c1*(6*a1ph+b2))
   !real(kind=dp) :: csu3   = 3*a2*(6*a1ph+b2)
   !real(kind=dp) :: csu4   = 9*a1*a2

   integer, parameter :: kmxx = 2000 ! max dim of nr of vertical layers
   integer, parameter :: mg = 4 ! max dim of nr of sediment fractions

   real(kind=dp) :: dijdij(0:kmxx) ! dudz(k)**2+dvdz(k)**2 vertical shear squared
   real(kind=dp) :: buoflu(kmxx)
   real(kind=dp) :: bruva(kmxx)
   real(kind=dp) :: tkepro(0:kmxx)

   real(kind=dp) :: ak(0:kmxx) ! local arrays, (0:
   real(kind=dp) :: bk(0:kmxx)
   real(kind=dp) :: ck(0:kmxx)
   real(kind=dp) :: dk(0:kmxx)
   real(kind=dp) :: ek(0:kmxx)
   real(kind=dp) :: dz(0:kmxx)
   real(kind=dp) :: dke(0:kmxx)

   real(kind=dp) :: ucxref(kmxx) ! and for reference/plotting:
   real(kind=dp) :: ucm(kmxx) ! and for reference/plotting:
   real(kind=dp) :: dijdijref(0:kmxx)
   real(kind=dp) :: tkin1ref(0:kmxx)
   real(kind=dp) :: teps1ref(0:kmxx)
   real(kind=dp) :: vicwref(0:kmxx)
   real(kind=dp) :: hcref(kmxx) ! mid-layer heigths
   real(kind=dp) :: hwref(0:kmxx) ! layer interface height, 0=bed

   real(kind=dp) :: epstke = 1d-32 ! D3D: - 7, dpm: -32
   real(kind=dp) :: epseps = 1d-32 ! D3D: - 7, dpm: -32
   real(kind=dp) :: epsd = 1d-32 ! D3D: - 7, dpm: -32

   real(kind=dp), allocatable :: turkin0(:) ! k old (m2/s2)  , at layer interface at u     these will become global, rename to : turkinwu0
   real(kind=dp), allocatable, target :: turkin1(:) !< [m2/s2] turbulent kinectic energy at layer interface u {"location": "edge", "shape": ["lnkx"]}

   real(kind=dp), allocatable :: tureps0(:) ! eps old (1/s)  , at layer interface at u
   real(kind=dp), allocatable :: tureps1(:) ! eps new        , at layer interface at u

   real(kind=dp), allocatable :: vicwwu(:) ! vertical eddy viscosity (m2/s) at layer interface at u point
   real(kind=dp), allocatable, target :: vicwws(:) !< [m2/s] vertical eddy viscosity at layer interface at s point {"location": "face", "shape": ["ndkx"]}

   !real            , allocatable    :: tkepro   (:)      ! vertical production t
   !real            , allocatable    :: tkedis   (:)      ! vertical dissipation
   real(kind=dp), allocatable, target :: rho(:) ! density at cell centres (kg/m3)
   real(kind=dp), allocatable :: rho0(:) ! density at cell centres (kg/m3), previous step
   real(kind=dp), allocatable :: rhosww(:) ! deviatoric density at vertical interfaces, w points (kg/m3)
   real(kind=dp), allocatable :: rhowat(:) ! density at cell centres (kg/m3), only salt and temp
   real(kind=dp), allocatable :: dpbdx0(:) ! previous step baroclinic pressure gradient, at u points
   real(kind=dp), allocatable :: rvdn(:) ! help integral of (rho-rhomean)*deltaz at pressure points (kg/m2)
   real(kind=dp), allocatable :: grn(:) ! help integral of vertical baroclinic pressure integral at pressure points  (kg/m)

   real(kind=dp), allocatable :: rhou(:) ! density at flow links   (kg/m3)

   real(kind=dp) :: Schmidt_number_salinity = 0.7_dp !< Turbulent Schmidt number for salinity
   real(kind=dp) :: Prandtl_number_temperature = 0.7_dp !< Turbulent Prandtl number for temperature
   real(kind=dp) :: Schmidt_number_tracer = 1.0_dp !< Turbulent Schmidt number for tracers
   real(kind=dp), allocatable :: sigsed(:) ! prandtl schmidt per sediment fraction
   real(kind=dp), allocatable :: sigdifi(:) ! inverse prandtl schmidt nrs
   real(kind=dp), allocatable :: wsf(:) ! fall velocities of all numconst constituents

   real(kind=dp), allocatable :: turkinepsws(:, :) ! k and eps,1,2     at layer interface at c , horizontal transport of k and eps
   real(kind=dp), allocatable :: tqcu(:) ! sum of q*turkinws at layer interface at cupw , horizontal transport of k and eps
   real(kind=dp), allocatable :: eqcu(:) ! sum of q*turepsws at layer interface at cupw , horizontal transport of k and eps
   real(kind=dp), allocatable :: sqcu(:) ! sum of q          at layer interface at cupw , horizontal transport of k and eps

   real(kind=dp), allocatable :: tttu(:), ttqc(:), tttc(:) ! test12

   integer, allocatable :: ln0(:, :) ! links in transport trimmed to minimum of ktop,ktop0 for z-layers

contains
!> Sets ALL (scalar) variables in this module to their default values.
   subroutine default_turbulence()
      use m_physcoef

! Coefficients of k-e model:
      sigdif = 1.0_dp
      sigtke = 1.0_dp; sigtkei = 1.0_dp / sigtke
      sigeps = 1.3_dp; sigepsi = 1.0_dp / sigeps
      sigrho = 0.7_dp ! bouyancy

      cmukep = 0.09_dp
      sqcmukep = sqrt(cmukep)
      sqcmukepi = 1.0_dp / sqcmukep

      cde = cmukep**0.75_dp
      cewall = cmukep**0.75_dp / vonkar ! 0.4769_dp  !        0.09**0.75/0.41  ! /vonkar
      c2e = 1.92_dp
      c1e = 1.44_dp
      c1e = c2e - vonkar**2 / (sigeps * sqcmukep)

      c1t = (1.0_dp - c1e) * cmukep
      c2t = 1.0_dp - c2e
      c3tsta = 1.0_dp * cmukep
      c3tuns = (1.0_dp - c1e) * cmukep

      coefn2 = -ag / (sigrho * rhomean)

      skmy = 1.96_dp
      a1ph = 0.92_dp
      a2 = 0.74_dp
      b1 = 16.6_dp
      b2 = 10.1_dp
      c1 = 0.08_dp
      e1 = 1.80_dp
      e2 = 1.33_dp
!e2     = e1-1+vonkar**2*b1**0.6667/skmy = 1.358, not 1.33?
      ghmin = -0.280_dp
      ghmax = 0.0233_dp
   end subroutine default_turbulence

end module m_turbulence
