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

module m_heatfluxes
   use precision, only: dp

   implicit none

   real(kind=dp) :: albedo !< reflection coefficient of water () at average incidence angle of 60 deg,
   ! (albedo is .025 at angle 0 deg, 0.13 at angle 70 deg)
   real(kind=dp) :: em !< Emissivity ()
   real(kind=dp) :: cpa !< Specific heat air   [J/kg/K]
   real(kind=dp) :: cpw !< Specific heat water [J/kg/K]
   real(kind=dp) :: rcpi !< 1/(rho*cpi) m3K/J
   real(kind=dp) :: emstf !< Em*Stf [W/m^2/K^4]

   real(kind=dp) :: qsunav !< Solar influx              (W/m2)
   real(kind=dp) :: qevaav !< Evaporative heat loss     (W/m2)
   real(kind=dp) :: qconav !< Convective heat loss      (W/m2)
   real(kind=dp) :: qlongav !< Long wave back radiation  (W/m2)
   real(kind=dp) :: qfreeav !< Free conv + evap heat loss (W/m2)
   real(kind=dp) :: qfrconav !< Free convection heat loss (W/m2)
   real(kind=dp) :: qfrevaav !< Free evaporation heat loss (W/m2)

   real(kind=dp) :: sarea !< Only for excess temp model temperature_model=TEMPERATURE_MODEL_EXCESS, lake area
   real(kind=dp) :: fwind !< Only for excess temp model temperature_model=TEMPERATURE_MODEL_EXCESS, wind factor

   integer :: jamapheatflux !< write heatfluxes to map
   integer :: jarichardsononoutput !< write Richardson nr to his
   integer :: jasecchisp !< Spatial Secchi 0,1
   integer :: rho_water_in_wind_stress !< Use rhomean or local (surface) density of model in windstress: 0,1
   integer, parameter :: RHO_MEAN = 0 !< Use rhomean in windstress

   real(kind=dp), dimension(:), allocatable, target :: qsunmap !< [W/m2] solar radiation reaching water surface {"location": "face", "shape": ["ndx"]}
   real(kind=dp), dimension(:), allocatable :: qevamap
   real(kind=dp), dimension(:), allocatable :: qconmap
   real(kind=dp), dimension(:), allocatable :: qlongmap
   real(kind=dp), dimension(:), allocatable :: qfrevamap
   real(kind=dp), dimension(:), allocatable :: qfrconmap
   real(kind=dp), dimension(:), allocatable :: qtotmap

   real(kind=dp), dimension(:), allocatable, target :: secchisp !< [m] Space-varying secchi depth {"location": "face", "shape": ["ndx"]}

contains

   !< sets heat flux model constants to default values
   subroutine default_heatfluxes()
      albedo = 0.06_dp
      em = 0.985_dp
      cpa = 1004.0_dp
      cpw = 3986.0_dp
      jamapheatflux = 0
      jarichardsononoutput = 0
      rho_water_in_wind_stress = RHO_MEAN

   end subroutine default_heatfluxes

   !> calculate derived coefficients for heatfluxes
   subroutine calculate_derived_coefficients_heatfluxes()
      use m_physcoef, only: rhomean
      use physicalconsts, only: stf

      rcpi = 1.0_dp / (rhomean * cpw)
      emstf = em * stf

   end subroutine calculate_derived_coefficients_heatfluxes

end module m_heatfluxes
