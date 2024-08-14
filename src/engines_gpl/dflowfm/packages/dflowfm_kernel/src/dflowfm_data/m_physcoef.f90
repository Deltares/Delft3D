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

!----- AGPL --------------------------------------------------------------------
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
   implicit none
   double precision :: ag !< 10.0   ! 9.81    ! (m/s2)
   double precision :: sag !< sqrt(ag)
   integer :: jahelmert = 0 !< 1=use Helmerts equation for agp only
   double precision :: vonkar !< von Karman constant ()
   double precision :: vonkarw !< von Karman constant used in wind formulations, on Firmijns request ()
   double precision :: frcuni !< uniform friction coeff 2D
   double precision :: frcuni1D !< uniform friction coeff 1D
   double precision :: frcuni1D2D !< uniform friction coeff 1D2D
   double precision :: frcunistreetinlet = 0.035
   double precision :: frcuniroofgutterpipe = 0.035
   double precision :: frcuniroof = 0.030
   double precision :: frcuni1Dgrounlay !< uniform friction coeff groundlayer
   double precision :: frcmax !< max friction coeff in frcu

   integer :: ifrctypuni !< 0=chezy, 1=manning, 2=white colebrook D3D, 3=white colebrook Waqua (now only 2D)
   double precision :: frcunilin !<60.    ! 6      ! 66     ! uniform friction coeff
   double precision :: umodlin !< linear friction umod , ifrctyp 4,5,6

   double precision :: wall_ks !< vertical wall nIKURADSE ROUGHNESSs (m)
   double precision :: wall_z0 !< z0 for vertical walls, ~= Ks/30    (m)
                                              !! z0 for bottom follows from ifrctyp==3 and z0=frcuni
   double precision :: z0 !< z0
   double precision :: ee !< natural e
   double precision :: ee9 !< natural e/c9of1

   double precision :: vicouv !< constant horizontal eddy viscosity   (m2/s) mom
   double precision :: dicouv !< constant horizontal eddy diffusivity (m2/s) sal, sed

   double precision :: Schmidt_number_salinity = 0.7d0 !< Turbulent Schmidt number for salinity
   double precision :: Prandtl_number_temperature = 0.7d0 !< Turbulent Prandtl number for temperature
   double precision :: Schmidt_number_tracer = 1.0d0 !< Turbulent Schmidt number for tracers

   double precision :: Elder !< add Elder viscosity
   double precision :: Smagorinsky !< add Smagorinsky Cs coefficient, vic = vic + (Cs*dx)**2 * S
   double precision :: viuchk !< if < 0.5 then eddy viscosity cell peclet check viu<viuchk*dx*dx/dt

   double precision :: vicoww !< 1D-6   !                 ! user specified constant vertical   eddy viscosity  (m2/s)
   double precision :: dicoww !< 1D-6   !                 ! user specified constant vertical   eddy diffusivity(m2/s)

   double precision :: rhomean !< mean ambient rho ! (kg/m3)
   double precision :: rhog !< rhomean*g
   double precision :: c9of1 !< vonkar/log(c9of1 + dzb / z0)

   !< Molecular diffusivity coefficients (m2/s):
   double precision :: viskin !< kinematic  viscosity
   double precision :: vismol !< molecular viscosity (m2/s)
   double precision :: difmolsal !< molecular diffusivity of salinity
   double precision :: difmoltem !<           diffusivity of temperature
   double precision :: difmolsed !<           diffusivity of sediment
   double precision :: difmoltracer !<         diffusivity of tracers

   double precision :: vicwminb ! minimum eddy viscosity in production terms shear and buoyancy
   double precision :: xlozmidov ! Ozmidov length scale (m)

   double precision :: viskinair !< kinematic  viscosity
   double precision :: backgroundairtemperature !< background air   temp (C)
   double precision :: backgroundwatertemperature !< background water temp (C)
   double precision :: backgroundsalinity !< background salinity (ppt)
   double precision :: backgroundcloudiness !< (%) cloudiness        for non-specified points
   double precision :: backgroundhumidity !< (%) relative humidity for non-specified points
   double precision :: secchidepth !< (m) secchidepth
   double precision :: secchidepth2 !< (m) secchidepth2
   double precision :: secchidepth2fraction !< (m) fraction of total absorbed by profile 2
   double precision :: zab(2), sfr(2) !< help variables

   double precision :: cp0 !< eckart density parameters
   double precision :: clam !< eckart density parameters
   double precision :: clam0 !< eckart density parameters
   double precision :: alph0 !< eckart density parameters
   integer :: idensform !< 0 = no, 1 = eckart
   integer :: Maxitpresdens = 1 !< max nr of density-pressure iterations
   integer :: Jarhointerfaces = 0 !< rho computed at vertical interfaces, yes=1, 0=cell center
   integer :: Jabaroczlaybed = 0 !< use fix for zlaybed yes/no
   integer :: Jabarocponbnd = 0 !< baroclini pressure on open boundaries yes/no

   integer :: limiterhordif !< 0=No, 1=Horizontal gradient densitylimiter, 2=Finite volume

   double precision :: Stanton !< coeff for convective  heat flux, if negative , take wind Cd
   double precision :: Dalton !< coeff for evaporative heat flux, if negative , take wind Cd
   double precision :: Tempmax = -999d0 !< limit
   double precision :: Tempmin = 0d0 !< limit
   integer :: Jaallowcoolingbelowzero = 0 !< Allow cooling below 0 degrees C (0=default since 2017)
   double precision :: Salimax = -999d0 !< limit
   double precision :: Salimin = 0d0 !< limit
   double precision :: epshstem = 0.001d0 !< only compute heatflx + evap if depth > trsh
   double precision :: surftempsmofac = 0.0d0 !< surface temperature smoothing factor 0-1d05
   double precision :: Soiltempthick = 0.0d0 !< if soil buffer desired make thick > 0, e.g. 0.2 m

   integer :: Jadelvappos !< only positive forced evaporation fluxes

   double precision :: tetav !< vertical teta transport
   double precision :: tetavkeps !< vertical teta k-eps
   double precision :: tetavmom !< vertical teta momentum

   double precision :: locsaltlev, locsaltmin, locsaltmax

   integer :: NFEntrainmentMomentum = 0 !< 1: switched on: Momentum transfer in NearField related entrainment
contains
!> Sets ALL (scalar) variables in this module to their default values.
   subroutine default_physcoef()
      ag = 9.81d0 ! 10.0      ! (m/s2)
      sag = sqrt(ag)
      vonkar = 0.41d0 ! von Karman constant ()
      vonkarw = 0.40d0 ! von Karman constant for wind ()
      ee = exp(1d0) ! natural e ()
      ee9 = 9d0 * ee !
      frcuni = 0.023d0 ! 60.    ! 6      ! 66     ! uniform friction coeff
      frcuni1D = 0.023d0 ! 60.    ! 6      ! 66     ! uniform friction coeff
      frcuni1D2D = 0.023d0 ! 60.    ! 6      ! 66     ! uniform friction coeff
      frcuni1Dgrounlay = 0.05d0 ! 60.    ! 6      ! 66     ! uniform friction coeff
      frcmax = 0d0
      ifrctypuni = 1 ! 0=chezy, 1=manning, 2=white colebrook (D3D), 3=white colebrook (WAQUA)
      frcunilin = 0d0 !
      umodlin = 1.d0 ! linear friction umod , ifrctyp 4,5,6
      wall_ks = 0.0d0 ! vertical wall nIKURADSE ROUGHNESSs (m)
      vicouv = 0.1d0 ! constant horizontal eddy viscosity (m2/s) mom
      dicouv = 0.1d0 ! constant horizontal eddy diffusivity (m2/s) sal, sed

      Elder = 0d0 ! add Elder viscosity
      Smagorinsky = 0.2d0 ! add Smagorinsky Cs coefficient, vic = vic + (Cs*dx)**2 * S
      viuchk = 0.24 ! if < 0.5 then eddy viscosity cell check viu<viuchk*dx*dx/dt

      vicoww = 1d-6 ! 5d-5   !                 ! background vertical eddy viscosity (m2/s)
      dicoww = 1d-6 ! 5d-5   !                 ! background vertical eddy diffusivity (m2/s)

      rhomean = 1000d0 ! mean ambient rho ! (kg/m3)
      rhog = ag * rhomean
      c9of1 = 9d0 ! vonkar/log(c9of1 + dzb / z0)

      backgroundairtemperature = 20d0 ! background air   temp (degC)
      backgroundwatertemperature = 20d0 ! background water temp (degC)
      backgroundsalinity = 30d0 ! background salinity (ppt), in eq of state, if salinity not computed
      backgroundcloudiness = 50d0 ! (%) cloudiness        for non-specified points
      backgroundhumidity = 50d0 !<(%) relative humidity for non-specified points
      secchidepth = 1d0 !< (m) secchidepth
      secchidepth2 = 0d0 !< (m) secchidepth2
      secchidepth2fraction = 0d0 !< (m) fraction of total absorbed by profile 2

      ! Molecular diffusivity coefficients:
      viskin = 1.d-6 ! kinematic  viscosity water in keps model
      vismol = 4.d0 / (20.d0 + backgroundwatertemperature) * 1d-5 ! Van Rijn, 1993, from iniphys.f90
      viskinair = 1.5d-5 ! kinematic  viscosity air
      difmolsal = viskin / 700d0 ! molecular diffusivity of salinity
      difmoltem = viskin / 6.7d0 !           diffusivity of temperature
      difmolsed = 0d0
      difmoltracer = 0d0

      vicwminb = 0d-7 ! was 0d0, minimum viscosity in production terms shear and buoyancy
      xlozmidov = 0d0 ! Ozmidov length scale

      alph0 = 0.698d0 ! =Eckart density parameters

      idensform = 2 !< 0 = no, 1 = Eckart, 2 = UNESCO
      limiterhordif = 2 !< 0=No, 1=Horizontal gradient densitylimiter, 2=Finite volume

      Stanton = 0.0013 !< coeff for convective  heat flux, if negative , take wind Cd
      Dalton = 0.0013 !< coeff for evaporative heat flux, if negative , take wind Cd

      Jadelvappos = 0 !< only positive forced evaporation fluxes

      tetav = 0.55d0 !< vertical teta transport
      tetavkeps = 0.55d0 !< vertical teta k-eps
      tetavmom = 0.55d0 !< vertical teta momentum

      locsaltlev = 1d0 !< salinity level for case of lock exchange
      locsaltmin = 5d0 !< minimum salinity for case of lock exchange
      locsaltmax = 10d0 !< maximum salinity for case of lock exchange

      NFEntrainmentMomentum = 0

   end subroutine default_physcoef

!> Check if density is pressure dependent
   pure function density_is_pressure_dependent() result(res)
      logical :: res !< Return value

      res = (idensform > 10)
   end function density_is_pressure_dependent

end module m_physcoef
