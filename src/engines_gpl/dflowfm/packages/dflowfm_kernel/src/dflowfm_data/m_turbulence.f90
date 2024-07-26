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
   implicit none

   ! Coefficients of k-e model:
   double precision :: cmukep
   double precision :: sqcmukep
   double precision :: sqcmukepi
   double precision :: cewall
   double precision :: cde
   double precision :: c1e
   double precision :: c2e
   double precision :: sigdif
   double precision :: sigtke, sigtkei
   double precision :: sigeps, sigepsi
   double precision :: sigrho
   double precision :: sigsal
   double precision :: sigtem
   double precision :: sigtracer

   !                                    c1e    = c2e-vonkar**2/(sigeps*sqrt(cmukep))

   double precision :: c1t
   double precision :: c2t
   double precision :: c3tsta
   double precision :: c3tuns

   double precision :: coefn2
   double precision :: skmy
   double precision :: a1ph
   double precision :: a2
   double precision :: b1
   double precision :: b2
   double precision :: c1
   double precision :: e1
   double precision :: e2
   !double precision :: e2     = e1-1+vonkar**2*b1**0.6667/skmy
   double precision :: ghmin
   double precision :: ghmax

   !quasi-equilibrium coefficients:
   !double precision :: csb1   = a2*(1-6*a1ph/b1)
   !double precision :: csb2   = 3*a2*(6*a1ph+b2)
   !double precision :: csu1   = a1*(1-3*c1-6*a1ph/b1)
   !double precision :: csu2   = 3*a1ph*a2*((b2-3*a2)*(1-6*a1ph/b1)-3*c1*(6*a1ph+b2))
   !double precision :: csu3   = 3*a2*(6*a1ph+b2)
   !double precision :: csu4   = 9*a1*a2

   integer, parameter :: kmxx = 2000 ! max dim of nr of vertical layers
   integer, parameter :: mg = 4 ! max dim of nr of sediment fractions

   double precision :: dijdij(0:kmxx) ! dudz(k)**2+dvdz(k)**2 vertical shear squared
   double precision :: buoflu(kmxx)
   double precision :: bruva(kmxx)
   double precision :: tkepro(0:kmxx)

   double precision :: ak(0:kmxx) ! local arrays, (0:
   double precision :: bk(0:kmxx)
   double precision :: ck(0:kmxx)
   double precision :: dk(0:kmxx)
   double precision :: ek(0:kmxx)
   double precision :: dz(0:kmxx)
   double precision :: dke(0:kmxx)

   double precision :: ucxref(kmxx) ! and for reference/plotting:
   double precision :: ucm(kmxx) ! and for reference/plotting:
   double precision :: dijdijref(0:kmxx)
   double precision :: tkin1ref(0:kmxx)
   double precision :: teps1ref(0:kmxx)
   double precision :: vicwref(0:kmxx)
   double precision :: hcref(kmxx) ! mid-layer heigths
   double precision :: hwref(0:kmxx) ! layer interface height, 0=bed

   double precision :: epstke = 1d-32 ! D3D: - 7, dpm: -32
   double precision :: epseps = 1d-32 ! D3D: - 7, dpm: -32
   double precision :: epsd = 1d-32 ! D3D: - 7, dpm: -32

   double precision, allocatable :: turkin0(:) ! k old (m2/s2)  , at layer interface at u     these will become global, rename to : turkinwu0
   double precision, allocatable, target :: turkin1(:) !< [m2/s2] turbulent kinectic energy at layer interface u {"location": "edge", "shape": ["lnkx"]}

   double precision, allocatable :: tureps0(:) ! eps old (1/s)  , at layer interface at u
   double precision, allocatable :: tureps1(:) ! eps new        , at layer interface at u

   double precision, allocatable :: vicwwu(:) ! vertical eddy viscosity (m2/s) at layer interface at u point
   double precision, allocatable, target :: vicwws(:) !< [m2/s] vertical eddy viscosity at layer interface at s point {"location": "face", "shape": ["ndkx"]}

   !real            , allocatable    :: tkepro   (:)      ! vertical production t
   !real            , allocatable    :: tkedis   (:)      ! vertical dissipation
   double precision, allocatable, target :: rho(:) ! density at cell centres (kg/m3)
   double precision, allocatable :: rho0(:) ! density at cell centres (kg/m3), previous step
   double precision, allocatable :: rhosww(:) ! deviatoric density at vertical interfaces, w points (kg/m3)
   double precision, allocatable :: rhowat(:) ! density at cell centres (kg/m3), only salt and temp
   double precision, allocatable :: dpbdx0(:) ! previous step baroclinic pressure gradient, at u points
   double precision, allocatable :: rvdn(:) ! help integral of (rho-rhomean)*deltaz at pressure points (kg/m2)
   double precision, allocatable :: grn(:) ! help integral of vertical baroclinic pressure integral at pressure points  (kg/m)

   double precision, allocatable :: rhou(:) ! density at flow links   (kg/m3)

   double precision, allocatable :: sigsed(:) ! prandtl schmidt per sediment fraction
   double precision, allocatable :: sigdifi(:) ! inverse prandtl schmidt nrs
   double precision, allocatable :: wsf(:) ! fall velocities of all numconst constituents

   double precision, allocatable :: turkinepsws(:, :) ! k and eps,1,2     at layer interface at c , horizontal transport of k and eps
   double precision, allocatable :: tqcu(:) ! sum of q*turkinws at layer interface at cupw , horizontal transport of k and eps
   double precision, allocatable :: eqcu(:) ! sum of q*turepsws at layer interface at cupw , horizontal transport of k and eps
   double precision, allocatable :: sqcu(:) ! sum of q          at layer interface at cupw , horizontal transport of k and eps

   double precision, allocatable :: tttu(:), ttqc(:), tttc(:) ! test12

   integer, allocatable :: ln0(:, :) ! links in transport trimmed to minimum of ktop,ktop0 for z-layers

contains
!> Sets ALL (scalar) variables in this module to their default values.
   subroutine default_turbulence()
      use m_physcoef

! Coefficients of k-e model:
      sigdif = 1d0
      sigtke = 1.0d0; sigtkei = 1d0 / sigtke
      sigeps = 1.3d0; sigepsi = 1d0 / sigeps
      sigrho = 0.7d0 ! bouyancy
      sigsal = Schmidt_number_salinity
      sigtem = Prandtl_number_temperature
      sigtracer = Schmidt_number_tracer

      cmukep = 0.09d0
      sqcmukep = sqrt(cmukep)
      sqcmukepi = 1.d0 / sqcmukep

      cde = cmukep**0.75d0
      cewall = cmukep**0.75d0 / vonkar ! 0.4769d0  !        0.09**0.75/0.41  ! /vonkar
      c2e = 1.92d0
      c1e = 1.44d0
      c1e = c2e - vonkar**2 / (sigeps * sqcmukep)

      c1t = (1d0 - c1e) * cmukep
      c2t = 1d0 - c2e
      c3tsta = 1d0 * cmukep
      c3tuns = (1d0 - c1e) * cmukep

      coefn2 = -ag / (sigrho * rhomean)

      skmy = 1.96d0
      a1ph = 0.92d0
      a2 = 0.74d0
      b1 = 16.6d0
      b2 = 10.1d0
      c1 = 0.08d0
      e1 = 1.80d0
      e2 = 1.33d0
!e2     = e1-1+vonkar**2*b1**0.6667/skmy = 1.358, not 1.33?
      ghmin = -0.280d0
      ghmax = 0.0233d0
   end subroutine default_turbulence

end module m_turbulence
