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
! NOTE: this document is automatically parsed
! CONVENTION:
! please use the following variable notation so the parser will pickup variables for dynamic exchange
! {=optional}
! typename, {allocatable, }target :: name{(:)} !< {(altname)} [units] description {JSON}
! NOTE: only one variable definition per line, the variable should not continue on the next line.
!
! The JSON part can contain additional key-value pairs in JSON format, e.g.:
! !< [m] waterlevel at previous timestep {"state":true,"slice":"1:nodtot","standard_name":"sea_surface_height"}
!
! For state variables values the following JSON key-value pairs are required:
! "standard_name" is the netcdf standard name for the variable, e.g. "sea_surface_height"
!NOTE: the modules
! m_dimens, m_polygon moved to gridgeom
! m_save_ugrid_state saves the variable names for saving UGrid format
module m_physcoef
   use precision, only: dp

   implicit none

   real(kind=dp) :: ag !< 10.0   ! 9.81    ! (m/s2)
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
   real(kind=dp) :: frcunilin !<60.    ! 6      ! 66     ! uniform friction coeff
   real(kind=dp) :: umodlin !< linear friction umod, friction type 4,5,6

   real(kind=dp) :: wall_ks !< vertical wall nIKURADSE ROUGHNESSs (m)
   real(kind=dp) :: wall_z0 !< z0 for vertical walls, ~= Ks/30    (m)
                            !! z0 for bottom follows from friction type R_WHITE_COLEBROOK and z0=frcuni
   real(kind=dp) :: z0 !< z0

   real(kind=dp) :: vicouv !< constant horizontal eddy viscosity   (m2/s) mom
   real(kind=dp) :: dicouv !< constant horizontal eddy diffusivity (m2/s) sal, sed

   real(kind=dp) :: Elder !< add Elder viscosity
   real(kind=dp) :: Smagorinsky !< add Smagorinsky Cs coefficient, vic = vic + (Cs*dx)**2 * S
   real(kind=dp) :: viuchk !< if < 0.5 then eddy viscosity cell peclet check viu<viuchk*dx*dx/dt

   real(kind=dp) :: vicoww !< 1D-6   !                 ! user specified constant vertical   eddy viscosity  (m2/s)
   real(kind=dp) :: dicoww !< 1D-6   !                 ! user specified constant vertical   eddy diffusivity(m2/s)

   real(kind=dp) :: rhomean !< mean ambient rho ! (kg/m3)
   real(kind=dp) :: rhog !< rhomean*g
   real(kind=dp) :: c9of1 !< vonkar/log(c9of1 + dzb / z0)

   !< Molecular diffusivity coefficients (m2/s):
   real(kind=dp) :: viskin !< kinematic  viscosity
   real(kind=dp) :: vismol !< molecular viscosity (m2/s)
   real(kind=dp) :: difmolsal !< molecular diffusivity of salinity
   real(kind=dp) :: difmoltem !<           diffusivity of temperature
   real(kind=dp) :: difmolsed !<           diffusivity of sediment
   real(kind=dp) :: difmoltracer !<         diffusivity of tracers

   real(kind=dp) :: vicwminb ! minimum eddy viscosity in production terms shear and buoyancy
   real(kind=dp) :: xlozmidov ! Ozmidov length scale (m)

   real(kind=dp) :: viskinair !< kinematic  viscosity
   real(kind=dp) :: backgroundwatertemperature !< background water temp (C)
   real(kind=dp) :: backgroundsalinity !< background salinity (ppt)
   real(kind=dp), parameter :: BACKGROUND_AIR_PRESSURE = 101325.0_dp !< background air pressure (Pa)
   real(kind=dp), parameter :: BACKGROUND_AIR_TEMPERATURE = 20.0_dp !< background air temperature (degrees Celsius)
   real(kind=dp), parameter :: BACKGROUND_CLOUDINESS = 50.0_dp !< (%) cloudiness for non-specified points
   real(kind=dp), parameter :: BACKGROUND_HUMIDITY = 50.0_dp !< (%) relative humidity for non-specified points
   real(kind=dp) :: secchidepth !< (m) secchidepth
   real(kind=dp) :: secchidepth2 !< (m) secchidepth2
   real(kind=dp) :: secchidepth2fraction !< (m) fraction of total absorbed by profile 2
   real(kind=dp) :: zab(2), sfr(2) !< help variables

   real(kind=dp) :: cp0 !< eckart density parameters
   real(kind=dp) :: clam !< eckart density parameters
   real(kind=dp) :: clam0 !< eckart density parameters
   real(kind=dp) :: alph0 !< eckart density parameters
   integer :: idensform !< 0 = no, 1 = eckart
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

   real(kind=dp) :: locsaltlev, locsaltmin, locsaltmax

   integer :: NFEntrainmentMomentum = 0 !< 1: switched on: Momentum transfer in NearField related entrainment
contains
!> Sets ALL (scalar) variables in this module to their default values.
   subroutine default_physcoef()
      ag = 9.81_dp ! 10.0      ! (m/s2)
      sag = sqrt(ag)
      vonkar = 0.41_dp ! von Karman constant ()
      vonkarw = 0.40_dp ! von Karman constant for wind ()
      frcuni = 0.023_dp ! 60.    ! 6      ! 66     ! uniform friction coeff
      frcuni1D = 0.023_dp ! 60.    ! 6      ! 66     ! uniform friction coeff
      frcuni1D2D = 0.023_dp ! 60.    ! 6      ! 66     ! uniform friction coeff
      frcuni1Dgrounlay = 0.05_dp ! 60.    ! 6      ! 66     ! uniform friction coeff
      frcmax = 0.0_dp
      ifrctypuni = 1 ! 0=chezy, 1=manning, 2=white colebrook (D3D), 3=white colebrook (WAQUA)
      frcunilin = 0.0_dp !
      umodlin = 1.0_dp ! linear friction umod, friction type 4,5,6
      wall_ks = 0.0_dp ! vertical wall nIKURADSE ROUGHNESSs (m)
      vicouv = 0.1_dp ! constant horizontal eddy viscosity (m2/s) mom
      dicouv = 0.1_dp ! constant horizontal eddy diffusivity (m2/s) sal, sed

      Elder = 0.0_dp ! add Elder viscosity
      Smagorinsky = 0.2_dp ! add Smagorinsky Cs coefficient, vic = vic + (Cs*dx)**2 * S
      viuchk = 0.24_dp ! if < 0.5 then eddy viscosity cell check viu<viuchk*dx*dx/dt

      vicoww = 1e-6_dp ! background vertical eddy viscosity (m2/s)
      dicoww = 1e-6_dp ! background vertical eddy diffusivity (m2/s)

      rhomean = 1000.0_dp ! mean ambient rho ! (kg/m3)
      rhog = ag * rhomean
      c9of1 = 9.0_dp ! vonkar/log(c9of1 + dzb / z0)

      backgroundwatertemperature = 20.0_dp ! background water temp (degC)
      backgroundsalinity = 30.0_dp ! background salinity (ppt), in eq of state, if salinity not computed
      secchidepth = 1.0_dp !< (m) secchidepth
      secchidepth2 = 0.0_dp !< (m) secchidepth2
      secchidepth2fraction = 0.0_dp !< (m) fraction of total absorbed by profile 2

      ! Molecular diffusivity coefficients:
      viskin = 1e-6_dp ! kinematic  viscosity water in keps model
      vismol = 4.0_dp / (20.0_dp + backgroundwatertemperature) * 1e-5_dp ! Van Rijn, 1993, from iniphys.f90
      viskinair = 1.5e-5_dp ! kinematic  viscosity air
      difmolsal = viskin / 700.0_dp ! molecular diffusivity of salinity
      difmoltem = viskin / 6.7_dp !           diffusivity of temperature
      difmolsed = 0.0_dp
      difmoltracer = 0.0_dp

      vicwminb = 0.0_dp ! was 0.0_dp, minimum viscosity in production terms shear and buoyancy
      xlozmidov = 0.0_dp ! Ozmidov length scale

      alph0 = 0.698_dp ! =Eckart density parameters

      idensform = 2 !< 0 = no, 1 = Eckart, 2 = UNESCO
      limiterhordif = 2 !< 0=No, 1=Horizontal gradient densitylimiter, 2=Finite volume

      Stanton = 0.0013 !< coeff for convective  heat flux, if negative , take wind Cd
      Dalton = 0.0013 !< coeff for evaporative heat flux, if negative , take wind Cd

      Jadelvappos = 0 !< only positive forced evaporation fluxes

      tetav = 0.55_dp !< vertical teta transport
      tetavkeps = 0.55_dp !< vertical teta k-eps
      tetavmom = 0.55_dp !< vertical teta momentum

      locsaltlev = 1.0_dp !< salinity level for case of lock exchange
      locsaltmin = 5.0_dp !< minimum salinity for case of lock exchange
      locsaltmax = 10.0_dp !< maximum salinity for case of lock exchange

      NFEntrainmentMomentum = 0

   end subroutine default_physcoef

!> Check if density is pressure dependent
   pure function density_is_pressure_dependent() result(res)
      logical :: res !< Return value

      res = (idensform > 10)
   end function density_is_pressure_dependent

end module m_physcoef
