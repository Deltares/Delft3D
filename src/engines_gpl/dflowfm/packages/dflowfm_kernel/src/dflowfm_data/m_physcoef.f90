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

!> @file modules.f90
!! Modules with global data.
!! call default_*() routines upon program startup and when loading a new MDU.
!! call only reset_*() routines when reinitialising an active flow model.

module m_physcoef
   use precision, only: dp

   implicit none

   real(kind=dp) :: ag !< gravitational acceleration (m/s2)
   real(kind=dp) :: sag !< sqrt(ag)
   integer :: jahelmert = 0 !< 1=use Helmerts equation for agp only
   real(kind=dp) :: vonkar !< von Karman constant ()
   real(kind=dp) :: vonkarw !< von Karman constant used in wind formulations, on Firmijns request ()
   real(kind=dp) :: frcuni !< uniform friction coeff 2D
   real(kind=dp) :: frcuni1D !< uniform friction coeff 1D
   real(kind=dp) :: frcuni1D2D !< uniform friction coeff 1D2D
   real(kind=dp) :: frcunistreetinlet = 0.035
   real(kind=dp) :: frcuniroofgutterpipe = 0.035
   real(kind=dp) :: frcuniroof = 0.030
   real(kind=dp) :: frcuni1Dgrounlay !< uniform friction coeff groundlayer
   real(kind=dp) :: frcmax !< max friction coeff in frcu

   integer :: ifrctypuni !< 0=chezy, 1=manning, 2=white colebrook D3D, 3=white colebrook Waqua (now only 2D)
   real(kind=dp) :: frcunilin !< uniform friction coeff
   real(kind=dp) :: umodlin !< linear friction umod, friction type 4,5,6

   real(kind=dp) :: wall_ks !< vertical wall Nikuradse roughness (m)
   real(kind=dp) :: wall_z0 !< z0 for vertical walls, ~= Ks/30    (m)
                            !! z0 for bottom follows from friction type R_WHITE_COLEBROOK and z0=frcuni
   real(kind=dp) :: z0 !< z0

   real(kind=dp) :: vicouv !< constant horizontal eddy viscosity   (m2/s) mom
   real(kind=dp) :: dicouv !< constant horizontal eddy diffusivity (m2/s) sal, sed

   real(kind=dp) :: Elder !< add Elder viscosity
   real(kind=dp) :: Smagorinsky !< add Smagorinsky Cs coefficient, vic = vic + (Cs*dx)**2 * S
   real(kind=dp) :: viuchk !< if < 0.5 then eddy viscosity cell peclet check viu<viuchk*dx*dx/dt

   real(kind=dp) :: vicoww !< user specified constant vertical   eddy viscosity  (m2/s)
   real(kind=dp) :: dicoww !< user specified constant vertical   eddy diffusivity(m2/s)

   real(kind=dp) :: rhomean !< mean ambient density (kg/m3)
   real(kind=dp) :: rhog !< rhomean*g
   real(kind=dp) :: c9of1 !< vonkar/log(c9of1 + dzb / z0)

   !< Molecular diffusivity coefficients (m2/s):
   real(kind=dp) :: viskin !< kinematic  viscosity water in keps model
   real(kind=dp) :: vismol !< molecular viscosity (m2/s)
   real(kind=dp) :: difmolsal !< molecular diffusivity of salinity
   real(kind=dp) :: difmoltem !<           diffusivity of temperature
   real(kind=dp) :: difmolsed !<           diffusivity of sediment
   real(kind=dp) :: difmoltracer !<        diffusivity of tracers

   real(kind=dp) :: vicwminb !< minimum eddy viscosity in production terms shear and buoyancy
   real(kind=dp) :: xlozmidov !< Ozmidov length scale (m)

   real(kind=dp) :: viskinair !< kinematic air viscosity
   real(kind=dp) :: backgroundwatertemperature !< background water temp (C)
   real(kind=dp) :: backgroundsalinity !< background salinity (ppt), in eq of state, if salinity not computed
   real(kind=dp), parameter :: BACKGROUND_AIR_PRESSURE = 101325.0_dp !< background air pressure (Pa)
   real(kind=dp), parameter :: BACKGROUND_AIR_TEMPERATURE = 20.0_dp !< background air temperature (degrees Celsius)
   real(kind=dp), parameter :: BACKGROUND_CLOUDINESS = 50.0_dp !< (%) cloudiness for non-specified points
   real(kind=dp), parameter :: BACKGROUND_HUMIDITY = 50.0_dp !< (%) relative humidity for non-specified points
   real(kind=dp) :: secchidepth !< (m) secchidepth
   real(kind=dp) :: secchidepth2 !< (m) secchidepth2
   real(kind=dp) :: secchidepth2fraction !< (m) fraction of total absorbed by profile 2
   real(kind=dp) :: zab(2), sfr(2) !< help variables

   integer :: idensform !< 0 = Uniform density, 1 = Eckart, 2 = UNESCO, 3 = UNESCO83
   logical :: apply_thermobaricity !< Check if density is pressure dependent
   integer :: Maxitpresdens = 1 !< max nr of density-pressure iterations
   integer :: Jarhointerfaces = 0 !< rho computed at vertical interfaces, yes=1, 0=cell center
   integer :: Jabaroczlaybed = 0 !< use fix for zlaybed yes/no
   integer :: Jabarocponbnd = 0 !< baroclini pressure on open boundaries yes/no

   integer :: limiterhordif !< 0=No, 1=Horizontal gradient densitylimiter, 2=Finite volume

   real(kind=dp) :: Stanton !< coeff for convective  heat flux, if negative , take wind Cd
   real(kind=dp) :: Dalton !< coeff for evaporative heat flux, if negative , take wind Cd
   real(kind=dp) :: Tempmax = -999.0_dp !< limit
   real(kind=dp) :: Tempmin = 0.0_dp !< limit
   integer :: Jaallowcoolingbelowzero = 0 !< Allow cooling below 0 degrees C (0=default since 2017)
   real(kind=dp) :: Salimax = -999.0_dp !< limit
   real(kind=dp) :: Salimin = 0.0_dp !< limit
   real(kind=dp) :: epshstem = 0.001_dp !< only compute heatflx + evap if depth > trsh
   real(kind=dp) :: surftempsmofac = 0.0_dp !< surface temperature smoothing factor (0 - 10^5)
   real(kind=dp) :: Soiltempthick = 0.0_dp !< if soil buffer desired make thick > 0, e.g. 0.2 m

   integer :: Jadelvappos !< only positive forced evaporation fluxes

   real(kind=dp) :: tetav !< vertical teta transport
   real(kind=dp) :: tetavkeps !< vertical teta k-eps
   real(kind=dp) :: tetavmom !< vertical teta momentum

   real(kind=dp) :: locsaltlev !< salinity level for case of lock exchange
   real(kind=dp) :: locsaltmin !< minimum salinity for case of lock exchange
   real(kind=dp) :: locsaltmax !< maximum salinity for case of lock exchange

   integer :: NFEntrainmentMomentum = 0 !< 1: switched on: Momentum transfer in NearField related entrainment
contains
!> Sets all variables in this module to their default values.
   subroutine default_physcoef()
      ag = 9.81_dp
      vonkar = 0.41_dp
      vonkarw = 0.40_dp
      frcuni = 0.023_dp
      frcuni1D = 0.023_dp
      frcuni1D2D = 0.023_dp
      frcuni1Dgrounlay = 0.05_dp
      frcmax = 0.0_dp
      ifrctypuni = 1
      frcunilin = 0.0_dp
      umodlin = 1.0_dp
      wall_ks = 0.0_dp
      vicouv = 0.1_dp
      dicouv = 0.1_dp
      Elder = 0.0_dp
      Smagorinsky = 0.2_dp
      viuchk = 0.24_dp
      vicoww = 1e-6_dp
      dicoww = 1e-6_dp
      rhomean = 1000.0_dp
      c9of1 = 9.0_dp
      backgroundwatertemperature = 20.0_dp
      backgroundsalinity = 30.0_dp
      secchidepth = 1.0_dp
      secchidepth2 = 0.0_dp
      secchidepth2fraction = 0.0_dp
      viskin = 1e-6_dp
      viskinair = 1.5e-5_dp
      difmolsed = 0.0_dp
      difmoltracer = 0.0_dp
      vicwminb = 0.0_dp
      xlozmidov = 0.0_dp
      idensform = 2
      apply_thermobaricity = .false.
      limiterhordif = 2
      Stanton = 0.0013_dp
      Dalton = 0.0013_dp
      Jadelvappos = 0
      tetav = 0.55_dp
      tetavkeps = 0.55_dp
      tetavmom = 0.55_dp
      locsaltlev = 1.0_dp 
      locsaltmin = 5.0_dp
      locsaltmax = 10.0_dp
      NFEntrainmentMomentum = 0

   end subroutine default_physcoef
   
   !> Calculates derived coefficients.
   subroutine calculate_derived_physcoef()
      sag = sqrt(ag)
      rhog = ag * rhomean
      vismol = 4.0_dp / (20.0_dp + backgroundwatertemperature) * 1e-5_dp ! Van Rijn, 1993, from iniphys.f90
      difmolsal = viskin / 700.0_dp
      difmoltem = viskin / 6.7_dp
   end subroutine calculate_derived_physcoef

end module m_physcoef
