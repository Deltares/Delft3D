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

module m_wind
   use precision, only: dp
   implicit none

   real(kind=dp), allocatable, target :: wx(:) !< [m/s] wind x velocity   (m/s) at u point {"location": "edge", "shape": ["lnx"]}
   real(kind=dp), allocatable, target :: wy(:) !< [m/s] wind y velocity   (m/s) at u point {"location": "edge", "shape": ["lnx"]}
   real(kind=dp), allocatable, target :: ec_pwxwy_x(:) !< Temporary array, for comparing EC-module to Meteo1.
   real(kind=dp), allocatable, target :: ec_pwxwy_y(:) !< Temporary array, for comparing EC-module to Meteo1.
   real(kind=dp), allocatable, target :: ec_pwxwy_c(:) !< Temporary array, for comparing EC-module to Meteo1.
   real(kind=dp), allocatable, target :: ec_charnock(:) !< Temporary array, for comparing EC-module to Meteo1.
   real(kind=dp), allocatable, target :: wcharnock(:) !< space var charnock (-) at u point {"location": "edge", "shape": ["lnx"]}

   real(kind=dp), allocatable, target :: patm(:) !< atmospheric pressure user specified in (N/m2), internally reworked to (m2/s2)
                                                      !! so that it can be merged with tidep later and difpatm/dx = m/s2, saves 1 array , using mode = 'add'
   real(kind=dp), allocatable, target :: rain(:) !< [mm/day] rain at xz,yz {"location": "face", "shape": ["ndx"]}
   real(kind=dp), allocatable, target :: evap(:) !< [m/s] evaporation at xz,yz {"location": "face", "shape": ["ndx"]}
   integer :: id_first_wind, id_last_wind !< counters to avoid looping over all ec_etims when only interessed in wind

   real(kind=dp), allocatable, target :: qext(:) !< [m3/s] External discharge per cell {"location": "face", "shape": ["ndkx"]}
   real(kind=dp), allocatable, target :: qextreal(:) !< [m3/s] Realized external discharge per cell {"location": "face", "shape": ["ndkx"]}
   real(kind=dp), allocatable, target :: vextcum(:) !< [m3] Cumulative realized volume through qext {"location": "face", "shape": ["ndkx"]}

   real(kind=dp), allocatable, target :: tair(:) !< air temperature       (degC)
   real(kind=dp), allocatable, target :: rhum(:) !< air relative humidity (%)
   real(kind=dp), allocatable, target :: clou(:) !< air cloudiness        (%)
   real(kind=dp), allocatable, target :: airdensity(:) !< air density           (kg/m3)
   real(kind=dp), allocatable, target :: qrad(:) !< solar radiation       (W/m2)
   real(kind=dp), dimension(:), allocatable :: solar_radiation !< solar radiation (W/m2) incl. albedo correction
   real(kind=dp), allocatable, target :: longwave(:) !< long wave radiation   (W/m2)
   real(kind=dp), allocatable :: heatsrc(:) !< resulting 2D or 3D heat source per cell (Km3/s)
   real(kind=dp), allocatable :: heatsrc0(:) !< resulting 2D or 3D heat source per cell, only set at timeuser (Km3/s)
   real(kind=dp), allocatable :: tbed(:) !< bed temperature       (degC)

   real(kind=dp), allocatable :: cdwcof(:) !< wind stress cd coefficient () , only if jatemp ==5

   integer :: jawind !< use wind yes or no
   integer :: japatm !< use patm yes or no
   integer :: jaspacevarcharn !< use space and time varying Charnock coefficients yes or no
   integer :: jawindstressgiven !< wind given as stress, no conversion needed
   integer :: jastresstowind !< if jawindstressgiven==1, convert stress to wind yes/no 1/0
   integer :: ja_computed_airdensity !< compute airdensity yes/no 1/0
   integer :: jarain !< use rain yes or no
   integer :: jaevap !< use evap yes or no
   integer :: jatair !< use air temperature   yes or no
   integer :: jarhum !< use relative humidity yes or no
   integer :: jaclou !< use cloudiness        yes or no
   integer :: ja_airdensity !< use variabele air density yes or no
   logical :: solrad_available = .false. !< solar radiation provided by user
   logical :: longwave_available = .false. !< longwave radiation provided by user
   integer :: jaheat_eachstep = 0 !< if 1, do it each step, else in externalforcings (default)
   integer :: jaQext !< use Qin externally provided yes or no
   integer :: jaqin !< use qin , sum of all in fluxes
   integer :: update_wind_stress_each_time_step = 0 !< if 1, update wind (and air pressure) in each computational time step, else in externalforcings (default)
   real(kind=dp) :: windxav, windyav !< average wind for plotting

   real(kind=dp) :: windsp
   real(kind=dp) :: winddir !< deg from north sailor
   real(kind=dp), target :: rainuni !< [mm/hr] uniform rain intensity. {"rank": 0}
   real(kind=dp) :: wsx
   real(kind=dp) :: wsy
   real(kind=dp) :: rhoair !< (kg/m3)
   real(kind=dp) :: PavBnd !< average ambient pressure (N/m2) for correction on open boundaries
   real(kind=dp) :: PavIni !< average ambient pressure (N/m2) for initial waterlevel correction
   real(kind=dp) :: patmfac !< 100 if Mbar, 1 if Pascal

   real(kind=dp) :: cdb(3) !< breakpoints cd function cd coefficient
   real(kind=dp) :: wdb(3) !< breakpoints cd function windspeed
   integer :: ICdtyp !< 1=Const; 2=Smith&Banke (2 pts); 3=S&B (3 pts); 4=Charnock 1955; 5=Hwang 2005; 6=Wuest 2005; 7=Hersbach 2010 (2 pts), 8: 4+viscous), 9=Garratt 1977.
   real(kind=dp) :: relativewind !< factor for top layer speed in relative wind, 0=no, 1 =full top layer speed
   integer :: jawindhuorzwsbased !< 1 = finite volume , 0 = hu
   integer :: jawindpartialdry !< Reduce windstress on water if link partially dry, only for bedlevtyp=3, 0 = no, 1 = yes
contains

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_wind() instead.
   subroutine default_wind()
      windsp = 0
      winddir = 90.0_dp !< deg from north sailor
      rainuni = 0.0_dp
      rhoair = 1.2_dp
      Pavini = 0.0_dp
      PavBnd = 0.0_dp !< default: no pressure correction on open boundaries.
      !< choose ambient pressure on boundaries equal to overall standard ambient pressure
      patmfac = 1.0_dp !< 100 if Mbar, 1 if Pascal

      cdb(1) = 0.00063_dp !< first  wind breakpoint
      wdb(1) = 0
      cdb(2) = 0.00723_dp !< second wind breakpoint
      wdb(2) = 100
      cdb(3) = 0.003_dp !< third  wind breakpoint
      wdb(3) = 30
      icdtyp = 2
      relativewind = 0.0_dp !< factor for top layer speed in wind relative wind, 0=no, 1 =full top layer speed
      jawindhuorzwsbased = 0 !< default: HU-based both in 2D and 3D (and not zws-based)
      jawindpartialdry = 1 !< default: partially dry cells switched off

      windxav = 0.0_dp
      windyav = 0.0_dp

      ! Rain+qin+wind not reset every re-init, only upon new MDU load, because rain can be
      ! enabled by user in MDU (for BMI use, even without rain in external forcings file)
      jarain = 0 !< use rain yes or no
      jaevap = 0 !< use evap yes or no
      jaqin = 0 !< use qin , sum of all in fluxes
      jaQext = 0 !< use Qin externally provided yes or no
      jawind = 0 !< use wind yes or no
      jastresstowind = 0 !< if jawindstressgiven==1, convert stress to wind yes/no 1/0
      ja_computed_airdensity = 0
      ! Remaining of variables is handled in reset_wind()
      call reset_wind()
   end subroutine default_wind

   !> Resets only wind variables intended for a restart of flow simulation.
   !! Upon loading of new model/MDU, call default_wind() instead.
   subroutine reset_wind()
      japatm = 0 !< use patm yes or no
      jaspacevarcharn = 0 !< use space varying Charnock coefficients
      jawindstressgiven = 0 !< wind stress given in meteo file
      jatair = 0
      ja_airdensity = 0
   end subroutine reset_wind
end module m_wind
