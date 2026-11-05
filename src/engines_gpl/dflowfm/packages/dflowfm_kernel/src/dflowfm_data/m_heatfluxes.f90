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

module m_heatfluxes
   use precision, only: dp, fp

   implicit none(type, external)

   real(kind=dp) :: albedo !< [-] Reflection coefficient of water at average incidence angle of 60 deg,
   ! (albedo is .025 at angle 0 deg, 0.13 at angle 70 deg)
   real(kind=dp) :: emmisivity_factor !< [-] Emissivity factor
   real(kind=dp), parameter :: SPECIFIC_HEAT_AIR = 1004.0_dp !< [J/(kg*K)] Specific heat air
   real(kind=dp), parameter :: SPECIFIC_HEAT_WATER = 3986.0_dp !< [J/(kg*K)] Specific heat capacity of water
   real(kind=dp) :: rcpi !< [(m^3*K)/J] 1/(rho*cpi)

   real(kind=dp) :: qsunav !< [W/m2] Solar influx
   real(kind=dp) :: qevaav !< [W/m2] Evaporative heat loss
   real(kind=dp) :: qconav !< [W/m2] Convective heat loss
   real(kind=dp) :: qlongav !< [W/m2] Long wave back radiation
   real(kind=dp) :: qfreeav !< [W/m2] Free conv + evap heat loss
   real(kind=dp) :: qfrconav !< [W/m2] Free convection heat loss
   real(kind=dp) :: qfrevaav !< [W/m2] Free evaporation heat loss

   real(kind=dp) :: sarea !< Only for excess temp model temperature_model=TEMPERATURE_MODEL_EXCESS, lake area
   real(kind=dp) :: fwind !< Only for excess temp model temperature_model=TEMPERATURE_MODEL_EXCESS, wind factor

   integer :: jarichardsononoutput !< write Richardson nr to his
   integer :: rho_water_in_wind_stress !< Use rhomean or local (surface) density of model in windstress: 0,1
   integer, parameter :: RHO_MEAN = 0 !< Use rhomean in windstress

   real(kind=dp), dimension(:), allocatable, target :: qsunmap !< [W/m2] solar radiation reaching water surface {"location": "face", "shape": ["ndx"]}
   real(kind=dp), dimension(:), allocatable :: qevamap
   real(kind=dp), dimension(:), allocatable :: qconmap
   real(kind=dp), dimension(:), allocatable :: qlongmap
   real(kind=dp), dimension(:), allocatable :: qfrevamap
   real(kind=dp), dimension(:), allocatable :: qfrconmap
   real(kind=dp), dimension(:), allocatable :: qtotmap

   ! Secchi depth variables
   logical :: secchi_depth_is_spatially_varying !< Flag to indicate if spatially varying Secchi depth is available
   logical :: secchi_depth_is_time_varying !< Flag to indicate if time-varying Secchi depth is available
   real(kind=dp), dimension(:), allocatable, target :: spatial_secchi_depth !< [m] Space-varying Secchi depth {"location": "face", "shape": ["ndx"]}

   real(kind=dp), parameter :: PRANDTL_NUMBER_SQUARED = 0.49_dp !< [1] Prandtl number (0.7) squared for air
   real(kind=dp), parameter :: KINEMATIC_VISCOSITY_AIR = 16.0e-06_dp !< [m^2/s] Kinematic viscosity of air at reference temperature
   real(kind=dp), parameter :: GAS_CONSTANT_DRY_AIR = 287.05e-02_dp !< [J/(kg*K)] Specific gas constant for dry air
   real(kind=dp), parameter :: GAS_CONSTANT_WATER_VAPOR = 461.495e-02_dp !< [J/(kg*K)] Specific gas constant for water vapor
   real(kind=dp), parameter :: MIN_ICE_SNOW_THICKNESS = 0.001_fp !< [m] Threshold thickness for ice/snow to overrule the underlying layer

contains

   !> Sets heat flux model constants to default values.
   subroutine default_heatfluxes()
      albedo = 0.06_dp
      emmisivity_factor = 0.985_dp
      jarichardsononoutput = 0
      rho_water_in_wind_stress = RHO_MEAN

   end subroutine default_heatfluxes

   !> Calculate derived coefficients for heatfluxes.
   subroutine calculate_derived_coefficients_heatfluxes()
      use m_physcoef, only: rhomean

      rcpi = 1.0_dp / (rhomean * SPECIFIC_HEAT_WATER)

   end subroutine calculate_derived_coefficients_heatfluxes

end module m_heatfluxes
