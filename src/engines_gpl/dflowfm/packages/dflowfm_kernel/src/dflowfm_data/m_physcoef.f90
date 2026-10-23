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

!> @file modules.f90
!! Modules with global data.
!! call default_*() routines upon program startup and when loading a new MDU.
!! call only reset_*() routines when reinitialising an active flow model.
module m_physcoef
   use precision, only: dp
   use m_density_parameters, only: idensform, apply_thermobaricity, thermobaricity_in_pressure_gradient, max_iterations_pressure_density, jabarocponbnd
   use m_array_or_scalar, only: t_array_or_scalar
   
   implicit none(type, external)

   real(kind=dp) :: ag !< [m/s2] Gravitational acceleration.
   real(kind=dp) :: square_root_ag !< [m^(1/2)/s] Square root of gravitational acceleration.
   integer :: use_helmert = 0 !< [-] Use Helmerts equation for agp only (0: no, 1: yes).

   real(kind=dp), parameter :: VON_KARMAN = 0.41_dp !< [-] von Karman constant. {former:vonkar}
   real(kind=dp), parameter :: VON_KARMAN_WIND = 0.40_dp !< [-] von Karman constant used in wind formulations. {former:vonkarw}

   real(kind=dp) :: frcuni !< [-] Uniform friction coeff 2D.
   real(kind=dp) :: frcuni1D !< [-] Uniform friction coeff 1D.
   real(kind=dp) :: frcuni1D2D !< [-] Uniform friction coeff 1D2D.

   real(kind=dp), parameter :: UNIFORM_FRICTION_COEFFICIENT_STREET_INLET = 0.035_dp !< [-] Uniform friction coefficient for street inlet. {former:frcunistreetinlet}
   real(kind=dp), parameter :: UNIFORM_FRICTION_COEFFICIENT_ROOF_GUTTER_PIPE = 0.035_dp !< [-] Uniform friction coefficient for roof gutter pipe. {former:frcuniroofgutterpipe}
   real(kind=dp), parameter :: UNIFORM_FRICTION_COEFFICIENT_ROOF = 0.030_dp !< [-] Uniform friction coefficient for roof. {former:frcuniroof}

   real(kind=dp) :: frcuni1Dgrounlay !< [-] Uniform friction coeff groundlayer.
   real(kind=dp) :: frcmax !< [-] Max friction coeff in frcu.
   integer :: dynroughveg !< [-] 0=off, 1=on, effect erosion/sedimentation on Manning roughness representative of dune vegetation in storm models, only for 2D friction.
   real(kind=dp) :: frcu_no_vegetation !< [s/m^(1/3)] Base (Manning) friction (without vegetation), default 0.023.
   real(kind=dp) :: droot !< [m] Root depth for dynamic roughness vegetation, range 0-100m, default 0.5m.
   real(kind=dp) :: dstem !< [m] Stem height for dynamic roughness vegetation, range 0-100m, default 0.5m.

   integer :: ifrctypuni !< [-] 0=chezy, 1=manning, 2=white colebrook D3D, 3=white colebrook Waqua (now only 2D).
   real(kind=dp) :: frcunilin !< [-] Uniform friction coefficient.
   real(kind=dp) :: umodlin !< [m/s] Linear friction umod, friction type 4,5,6.

   real(kind=dp) :: wall_ks !< [m] Vertical wall Nikuradse roughness.
   real(kind=dp) :: wall_z0 !< [m] z0 for vertical walls, ~= Ks/30.
                            !! z0 for bottom follows from friction type R_WHITE_COLEBROOK and z0=frcuni
   real(kind=dp) :: z0 !< [m] z0.

   real(kind=dp) :: uniform_horizontal_eddy_viscosity !< [m2/s] Uniform horizontal eddy viscosity for momentum. {former:vicouv}
   real(kind=dp) :: uniform_horizontal_eddy_diffusivity !< [m2/s] Uniform horizontal eddy diffusivity for constituents. {former:dicouv}

   real(kind=dp) :: Elder !< [-] Add Elder viscosity.
   real(kind=dp) :: Smagorinsky !< [-] Add Smagorinsky Cs coefficient, vic = vic + (Cs*dx)**2 * S.

   type(t_array_or_scalar), target :: dicoww !< [m2/s] Background vertical eddy diffusivity.
   type(t_array_or_scalar), target :: vicoww !< [m2/s] Background vertical eddy viscosity.

   real(kind=dp) :: rhomean !< [kg/m3] Mean ambient density.
   real(kind=dp) :: rhog !< [N/m3] rhomean * gravitational acceleration.
   real(kind=dp), parameter :: C9OF1 = 9.0_dp !< [-] VON_KARMAN/log(C9OF1 + dzb / z0)

   ! Molecular diffusivity coefficients:
   real(kind=dp), parameter :: VISKIN = 1e-6_dp !< [m2/s] Kinematic  viscosity water in keps model.
   real(kind=dp) :: vismol !< [m2/s] Molecular viscosity.
   real(kind=dp), parameter :: DIFMOLSAL = VISKIN / 700.0_dp !< [m2/s] Molecular diffusivity of salinity.
   real(kind=dp), parameter :: DIFMOLTEM = VISKIN / 6.7_dp !< [m2/s] Diffusivity of temperature.
   real(kind=dp), parameter :: DIFMOLTRACER = 0.0_dp !< [m2/s] Diffusivity of tracers.

   real(kind=dp) :: tracer_concentration_min !< [kg/m3] Lower limit for tracer concentration.
   logical :: tracer_concentration_min_enabled !< [-] Is lower limit for tracer concentration enabled?
   real(kind=dp) :: tracer_concentration_max !< [kg/m3] Upper limit for tracer concentration.
   logical :: tracer_concentration_max_enabled !< [-] Is upper limit for tracer concentration enabled?

   real(kind=dp) :: vicwminb !< [m2/s] Minimum eddy viscosity in production terms shear and buoyancy
   real(kind=dp) :: xlozmidov !< [m] Ozmidov length scale.

   real(kind=dp), parameter :: VISKINAIR = 1.5e-5_dp !< [m2/s] Kinematic air viscosity.
   real(kind=dp) :: backgroundwatertemperature !< [degC] Background water temp.
   real(kind=dp) :: backgroundsalinity !< [ppt] Background salinity, in eq of state, if salinity not computed.
   real(kind=dp), parameter :: BACKGROUND_AIR_PRESSURE = 101325.0_dp !< [Pa] Background air pressure.
   real(kind=dp), parameter :: BACKGROUND_AIR_TEMPERATURE = 20.0_dp !< [degC] Background air temperature.
   real(kind=dp), parameter :: BACKGROUND_CLOUDINESS = 50.0_dp !< [%] Cloudiness for non-specified points.
   real(kind=dp), parameter :: BACKGROUND_HUMIDITY = 50.0_dp !< [%] Relative humidity for non-specified points.

   ! Secchi depth variables
   real(kind=dp), dimension(2) :: secchi_depth !< [m] Constant Secchi depth; 1 = visible light and UV radiation, 2 = infrared radiation.
   real(kind=dp), dimension(2) :: secchi_radiation_fraction !< [-] Radiation fraction in (1) visible light and UV radiation, (2) infrared radiation used in Secchi computation.
   real(kind=dp), dimension(2) :: diffuse_attenuation_coefficient !< [m] Diffuse attenuation coefficient for radiation.
   real(kind=dp), parameter :: POOLE_ATKINS_PARAMETER = 1.7_dp !< [-] Parameter for conversion of Secchi depth to diffuse attenuation coefficient.

   real(kind=dp) :: Stanton !< [-] Coeff for convective heat flux, if negative, take wind Cd.
   real(kind=dp) :: Dalton !< [-] Coeff for evaporative heat flux, if negative, take wind Cd.
   real(kind=dp) :: temperature_max = -999.0_dp !< [degC] Upper temperature limit.
   real(kind=dp) :: temperature_min = 0.0_dp !< [degC] Lower temperature limit.
   logical :: use_salinity_freezing_point = .false. !< [-] A flag to use a salinity dependent freezing point.
   real(kind=dp) :: salinity_max = -999.0_dp !< [ppt] Upper salinity limit.
   real(kind=dp) :: salinity_min = 0.0_dp !< [ppt] Lower salinity limit.
   real(kind=dp) :: epshstem = 0.001_dp !< [m] Only compute heatflx + evap if depth > trsh.
   real(kind=dp) :: surftempsmofac = 0.0_dp !< [-] Surface temperature smoothing factor (0 - 10^5).
   real(kind=dp) :: Soiltempthick = 0.0_dp !< [m] If soil buffer desired make thick > 0, e.g. 0.2 m.

   integer :: Jadelvappos !< [-] Only positive forced evaporation fluxes.
   real(kind=dp) :: free_convection_coefficient !< [-] Free convection turbulence coefficient.

   real(kind=dp), parameter :: THETA_TRANSPORT = 0.55_dp !< [-] Theta used for vertical transport of constituents. {former:tetav}
   real(kind=dp), parameter :: THETA_KEPS_DIFFUSION = 0.55_dp !< [-] Theta used for k-epsilon vertical diffusion. {former:tetavkeps}

   real(kind=dp) :: locsaltlev !< [ppt] Salinity level for case of lock exchange.
   real(kind=dp) :: locsaltmin !< [ppt] Minimum salinity for case of lock exchange.
   real(kind=dp) :: locsaltmax !< [ppt] Maximum salinity for case of lock exchange.

   integer :: NFEntrainmentMomentum = 0 !< [-] 1: switched on: Momentum transfer in NearField related entrainment.

contains

!> Sets all variables in this module to their default values.
   subroutine default_physcoef()

      ag = 9.81_dp
      frcuni = 0.023_dp
      frcuni1D = frcuni
      frcuni1D2D = frcuni
      frcuni1Dgrounlay = 0.05_dp
      frcmax = 0.0_dp
      ifrctypuni = 1
      frcunilin = 0.0_dp
      umodlin = 1.0_dp
      wall_ks = 0.0_dp
      uniform_horizontal_eddy_viscosity = 0.1_dp
      uniform_horizontal_eddy_diffusivity = 0.1_dp
      Elder = 0.0_dp
      Smagorinsky = 0.2_dp
      dicoww%scalar = 1e-6_dp
      vicoww%scalar = 1e-6_dp
      rhomean = 1000.0_dp
      backgroundwatertemperature = 20.0_dp
      backgroundsalinity = 30.0_dp
      secchi_depth(1) = 1.0_dp
      secchi_depth(2) = 0.0_dp
      secchi_radiation_fraction(1) = 1.0_dp
      secchi_radiation_fraction(2) = 0.0_dp
      diffuse_attenuation_coefficient(1) = secchi_depth(1) / POOLE_ATKINS_PARAMETER
      diffuse_attenuation_coefficient(2) = secchi_depth(2) / POOLE_ATKINS_PARAMETER
      vicwminb = 0.0_dp
      xlozmidov = 0.0_dp
      idensform = 2
      apply_thermobaricity = .false.
      thermobaricity_in_pressure_gradient = .false.
      Stanton = 0.0013_dp
      Dalton = 0.0013_dp
      Jadelvappos = 0
      free_convection_coefficient = 0.14_dp
      locsaltlev = 1.0_dp
      locsaltmin = 5.0_dp
      locsaltmax = 10.0_dp
      NFEntrainmentMomentum = 0
      dynroughveg = 0
      droot = 0.5_dp
      dstem = 0.5_dp
      frcu_no_vegetation = 2.3e-2_dp
      tracer_concentration_min = -huge(1.0_dp)
      tracer_concentration_min_enabled = .false.
      tracer_concentration_max = huge(1.0_dp)
      tracer_concentration_max_enabled = .false.
   end subroutine default_physcoef

   !> Calculates derived coefficients.
   subroutine calculate_derived_physcoef()
      square_root_ag = sqrt(ag)
      rhog = ag * rhomean
      vismol = 4.0_dp / (20.0_dp + backgroundwatertemperature) * 1e-5_dp ! Van Rijn, 1993, from iniphys.f90
   end subroutine calculate_derived_physcoef

end module m_physcoef
