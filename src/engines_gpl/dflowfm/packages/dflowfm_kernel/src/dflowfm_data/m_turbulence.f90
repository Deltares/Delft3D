!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

   ! Coefficients of turbulence model
   real(kind=dp) :: cmukep
   real(kind=dp) :: cewall
   real(kind=dp) :: cde
   real(kind=dp) :: c1e
   real(kind=dp) :: c3e_stable
   real(kind=dp) :: c3e_unstable
   real(kind=dp) :: c2e
   real(kind=dp) :: sigdif
   real(kind=dp) :: sigtke, sigtkei
   real(kind=dp) :: sigeps, sigepsi
   real(kind=dp) :: sigrho !< bouyancy

   real(kind=dp) :: c1t
   real(kind=dp) :: c2t
   real(kind=dp) :: c3t_stable
   real(kind=dp) :: c3t_unstable

   real(kind=dp) :: brunt_vaisala_coefficient

   integer, parameter :: kmxx = 2000 !< max dim of nr of vertical layers
   integer, parameter :: mg = 4 !< max dim of nr of sediment fractions

   integer, parameter :: TURB_LAX_ALL = 1
   integer, parameter :: TURB_LAX_CONNECTED = 2

   real(kind=dp) :: dijdij(0:kmxx) !< dudz(k)**2+dvdz(k)**2 vertical shear squared
   real(kind=dp) :: buoflu(kmxx)
   real(kind=dp) :: bruva(kmxx)
   real(kind=dp) :: tkepro(0:kmxx) ! vertical production t

   real(kind=dp) :: ak(0:kmxx) ! local arrays, (0:
   real(kind=dp) :: bk(0:kmxx)
   real(kind=dp) :: ck(0:kmxx)
   real(kind=dp) :: dk(0:kmxx)
   real(kind=dp) :: ek(0:kmxx)
   real(kind=dp) :: dz(0:kmxx)
   real(kind=dp) :: dke(0:kmxx)

   real(kind=dp) :: ucxref(kmxx) !< for reference/plotting:
   real(kind=dp) :: ucm(kmxx) !< for reference/plotting:
   real(kind=dp) :: dijdijref(0:kmxx)
   real(kind=dp) :: tkin1ref(0:kmxx)
   real(kind=dp) :: teps1ref(0:kmxx)
   real(kind=dp) :: vicwref(0:kmxx)
   real(kind=dp) :: hcref(kmxx) !< mid-layer heigths
   real(kind=dp) :: hwref(0:kmxx) !< layer interface height, 0=bed

   real(kind=dp), parameter :: MINIMUM_VALUE_K_EPS_TAU = 1e-32_dp
   real(kind=dp) :: tke_min
   real(kind=dp) :: eps_min

   real(kind=dp), allocatable, dimension(:) :: turkin0 ! k old (m2/s2)  , at layer interface at u     these will become global, rename to : turkinwu0
   real(kind=dp), allocatable, dimension(:), target :: turkin1 !< [m2/s2] turbulent kinectic energy at layer interface u {"location": "edge", "shape": ["lnkx"]}

   real(kind=dp), allocatable, dimension(:) :: tureps0 ! eps old (1/s)  , at layer interface at u
   real(kind=dp), allocatable, dimension(:) :: tureps1 ! eps new        , at layer interface at u

   real(kind=dp), allocatable, dimension(:) :: vicwwu ! vertical eddy viscosity (m2/s) at layer interface at u point
   real(kind=dp), allocatable, dimension(:), target :: vicwws !< [m2/s] vertical eddy viscosity at layer interface at s point {"location": "face", "shape": ["ndkx"]}
   real(kind=dp), allocatable, dimension(:), target :: difwws !< [m2/s] vertical eddy diffusivity of salinity at layer interface at s point {"location": "face", "shape": ["ndkx"]}
   real(kind=dp), allocatable, dimension(:) :: rich !< Richardson number at velocity-point
   real(kind=dp), allocatable, dimension(:) :: richs !< Richardson number at pressure-point

   real(kind=dp), allocatable, dimension(:), target :: in_situ_density ! Pressure dependent water density at cell centres (kg/m3)
   real(kind=dp), allocatable, dimension(:), target :: potential_density ! Potential water density at cell centres (kg/m3)
   real(kind=dp), dimension(:), pointer :: rho ! Water density at cell centres (kg/m3)
   real(kind=dp), allocatable, dimension(:) :: drhodz !< Vertical density gradient
   real(kind=dp), allocatable, dimension(:) :: rhosww ! deviatoric density at vertical interfaces, w points (kg/m3)
   real(kind=dp), allocatable, dimension(:) :: rhowat ! density at cell centres (kg/m3), only salt and temp
   real(kind=dp), allocatable, dimension(:) :: baroclinic_force_prev ! previous step baroclinic force, at u points
   real(kind=dp), allocatable, dimension(:) :: baroclinic_pressures ! baroclinic pressures (/ag), at pressure points
   real(kind=dp), allocatable, dimension(:) :: integrated_baroclinic_pressures ! depth-integrated baroclinic pressures (/ag), at pressure points

   real(kind=dp), allocatable, dimension(:) :: rhou !< density at flow links (kg/m3)

   real(kind=dp) :: Schmidt_number_salinity = 0.7_dp !< Turbulent Schmidt number for salinity
   real(kind=dp) :: Prandtl_number_temperature = 0.7_dp !< Turbulent Prandtl number for temperature
   real(kind=dp) :: Schmidt_number_tracer = 1.0_dp !< Turbulent Schmidt number for tracers

   real(kind=dp), allocatable, dimension(:) :: sigsed !< prandtl schmidt per sediment fraction
   real(kind=dp), allocatable, dimension(:) :: sigdifi !< inverse prandtl schmidt nrs
   real(kind=dp), allocatable, dimension(:) :: wsf !< fall velocities of all numconst constituents

   real(kind=dp), allocatable, dimension(:) :: turkinws !< k   at layer interface at c , horizontal transport of k and eps
   real(kind=dp), allocatable, dimension(:) :: turepsws !< eps at layer interface at c , horizontal transport of k and eps
   real(kind=dp), allocatable, dimension(:) :: tqcu !< sum of q*turkinws at layer interface at cupw , horizontal transport of k and eps
   real(kind=dp), allocatable, dimension(:) :: eqcu !< sum of q*turepsws at layer interface at cupw , horizontal transport of k and eps
   real(kind=dp), allocatable, dimension(:) :: sqcu !< sum of q          at layer interface at cupw , horizontal transport of k and eps

   integer, allocatable :: ln0(:, :) !< links in transport trimmed to minimum of ktop,ktop0 for z-layers

   real(kind=dp), parameter :: BACKGROUND_DIFFUSION_ON = 1.0_dp
   real(kind=dp), parameter :: BACKGROUND_DIFFUSION_OFF = 0.0_dp

contains

   !> Sets (underived) variables in this module to their default values.
   subroutine default_turbulence()
      use m_physcoef, only: vonkar

      sigdif = 1.0_dp
      sigtke = 1.0_dp
      sigeps = 1.3_dp
      sigrho = 0.7_dp

      cmukep = 0.09_dp
      c2e = 1.92_dp
      c1e = c2e - vonkar**2 / (sigeps * sqrt(cmukep)) ! Can be overriden by user and is therefore not a derived coefficient

      c3e_stable = 0.0_dp
      c3e_unstable = c1e ! Can be overriden by user and is therefore not a derived coefficient

      tke_min = MINIMUM_VALUE_K_EPS_TAU
      eps_min = MINIMUM_VALUE_K_EPS_TAU
   end subroutine default_turbulence

   !> Calculates derived coefficients for turbulence
   subroutine calculate_derived_coefficients_turbulence()
      use m_physcoef, only: vonkar, rhomean, ag

      sigtkei = 1.0_dp / sigtke
      sigepsi = 1.0_dp / sigeps

      cewall = cmukep**0.75_dp / vonkar
      cde = cmukep**0.75_dp

      c1t = (1.0_dp - c1e) * cmukep
      c2t = 1.0_dp - c2e
      c3t_stable = 1.0_dp * cmukep
      c3t_unstable = (1.0_dp - c1e) * cmukep

      brunt_vaisala_coefficient = -ag / (sigrho * rhomean)

   end subroutine calculate_derived_coefficients_turbulence

end module m_turbulence
