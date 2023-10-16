!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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
implicit none

double precision, allocatable, target :: wx(:), wx_t0(:), wx_t1(:)  !< [m/s] wind x velocity   (m/s) at u point {"location": "edge", "shape": ["lnx"]}
double precision, allocatable, target :: wy(:), wy_t0(:), wy_t1(:)  !< [m/s] wind y velocity   (m/s) at u point {"location": "edge", "shape": ["lnx"]}
double precision, allocatable, target :: ec_pwxwy_x(:)  !< Temporary array, for comparing EC-module to Meteo1.
double precision, allocatable, target :: ec_pwxwy_y(:)  !< Temporary array, for comparing EC-module to Meteo1.
double precision, allocatable, target :: ec_pwxwy_c(:)  !< Temporary array, for comparing EC-module to Meteo1.
double precision, allocatable, target :: wcharnock(:)   !< space var charnock (-) at u point {"location": "edge", "shape": ["lnx"]}

double precision, allocatable, target :: patm(:), patm_t0(:), patm_t1(:)     !< atmospheric pressure user specified in (N/m2), internally reworked to (m2/s2)
                                                      !! so that it can be merged with tidep later and difpatm/dx = m/s2, saves 1 array , using mode = 'add'
double precision, allocatable, target :: rain(:), rain_t0(:), rain_t1(:)     !< [mm/day] rain at xz,yz {"location": "face", "shape": ["ndx"]}
double precision, allocatable, target :: evap(:)     !< [m/s] evaporation at xz,yz {"location": "face", "shape": ["ndx"]}
integer :: id_first_wind, id_last_wind  !< counters to avoid looping over all ec_etims when only interessed in wind

!!
!! Laterals
!!
integer, parameter :: ILATTP_ALL = 0 !< Type code for laterals that apply to both 2D and 1D nodes.
integer, parameter :: ILATTP_1D  = 1 !< Type code for laterals that only apply to 1D nodes.
integer, parameter :: ILATTP_2D  = 2 !< Type code for laterals that only apply to 2D nodes.

integer                      , target :: numlatsg          !< [-] nr of lateral discharge providers  {"rank": 0}
double precision, allocatable, target :: qplat(:), qplat_t0(:), qplat_t1(:)   !< [m3/s] Lateral discharge of provider {"shape": ["numlatsg"]}
double precision, allocatable, target :: qqlat(:)          !< [m3/s] Lateral discharge at xz,yz {"location": "face", "shape": ["ndx"]}
double precision, allocatable, target :: balat(:)          !< [m2] total area of all cells in provider numlatsg {"shape": ["numlatsg"]}
character(len=128), allocatable       :: lat_ids(:)        !< id of laterals {"shape": ["numlatsg"]}
double precision, allocatable, target :: qplatCum(:)       !< [m3/s] Cumulative lateral discharge of provider {"shape": ["numlatsg"]}
double precision, allocatable, target :: qplatCumPre(:)    !< [m3/s] Cumulative lateral discharge of provider at previous history output time{"shape": ["numlatsg"]}
double precision, allocatable, target :: qplatAve(:)       !< [m3/s] Average lateral discharge of provider during the past history output interal {"shape": ["numlatsg"]}
double precision, allocatable, target :: qLatReal(:)       !< [m3/s] Realized lateral discharge {"shape": ["numlatsg"]}
double precision, allocatable, target :: qLatRealCum(:)    !< [m3/s] Cumulative realized lateral discharge {"shape": ["numlatsg"]}
double precision, allocatable, target :: qLatRealCumPre(:) !< [m3/s] Cumulative realized lateral discharge at previous history output time{"shape": ["numlatsg"]}
double precision, allocatable, target :: qLatRealAve(:)    !< [m3/s] Average realized lateral discharge during the past history output interal{"shape": ["numlatsg"]}

!! Lateral lookup tables: n1/n2latsg(ilat) = n1/n2, nnlat(n1:n2) = { flow node nrs affected by lateral ilat }
integer                               :: nlatnd      !< lateral nodes dimension, counter of nnlat(:)
integer,          allocatable, target :: n1latsg(:)  !< [-] first  nlatnd point in lateral signal numlatsg {"shape": ["numlatsg"]}
integer,          allocatable, target :: n2latsg(:)  !< [-] second nlatnd point in lateral signal numlatsg {"shape": ["numlatsg"]}
integer,          allocatable, target :: nnlat(:)    !< [-] for each lateral node, flow node number == pointer to qplat/balat {"shape": ["nlatnd"]}
integer,          allocatable, target :: kclat(:)    !< [-] for each cell: 0 when not accepting lateral discharge (e.g. pipe) {"location": "face", "shape": ["ndx"]}

!! Lateral geometry variables
integer                               :: nNodesLat           !< [-] Total number of geom nodes for all laterals.
integer,          allocatable, target :: nodeCountLat(:)     !< [-] Count of nodes per lateral.
double precision, allocatable, target :: geomXLat(:)         !< [m] x coordinates of laterals.
double precision, allocatable, target :: geomYLat(:)         !< [m] y coordinates of laterals.

double precision, allocatable, target :: qext(:)     !< [m3/s] External discharge per cell {"location": "face", "shape": ["ndkx"]}
double precision, allocatable, target :: qextreal(:) !< [m3/s] Realized external discharge per cell {"location": "face", "shape": ["ndkx"]}
double precision, allocatable, target :: vextcum(:)  !< [m3] Cumulative realized volume through qext {"location": "face", "shape": ["ndkx"]}

double precision, allocatable, target :: tair(:)     !< air temperature       (degC)
double precision, allocatable, target :: rhum(:)     !< air relative humidity (%)
double precision, allocatable, target :: clou(:)     !< air cloudiness        (%)
double precision, allocatable, target :: qrad(:)     !< solar radiation       (W/m2)
double precision, allocatable, target :: longwave(:) !< long wave radiation   (W/m2)
double precision, allocatable         :: heatsrc (:) !< resulting 2D or 3D heat source per cell (Km3/s)
double precision, allocatable         :: heatsrc0(:) !< resulting 2D or 3D heat source per cell, only set at timeuser (Km3/s)
double precision, allocatable         :: tbed(:)     !< bed temperature       (degC)


double precision, allocatable         :: cdwcof(:)   !< wind stress cd coefficient () , only if jatemp ==5

integer         , allocatable         :: kcw (:)     !< mask array

integer                           :: jawind              !< use wind yes or no
integer                           :: japatm              !< use patm yes or no
integer                           :: jaspacevarcharn     !< use space and time varying Charnock coefficients yes or no
integer                           :: jawindstressgiven   !< wind given as stress, no conversion needed
integer                           :: jastresstowind      !< if jawindstressgiven==1, convert stress to wind yes/no 1/0 
integer                           :: jarain              !< use rain yes or no
integer                           :: jaevap              !< use evap yes or no
integer                           :: jatair              !< use air temperature   yes or no
integer                           :: jarhum              !< use relative humidity yes or no
integer                           :: jaclou              !< use cloudiness        yes or no
integer                           :: jasol = 0           !< use 1 = use solrad, 2 = use cloudiness
integer                           :: jalongwave = 0      !< >0 longwaveradiation from file; otherwise internal formulation
integer                           :: jaheat_eachstep = 0 !< if 1, do it each step, else in externalforcings (default)
integer                           :: jaQext              !< use Qin externally provided yes or no
integer                           :: jaqin               !< use qin , sum of all in fluxes

double precision                  :: windxav, windyav  !< average wind for plotting

double precision                  :: windsp
double precision                  :: winddir         !< deg from north sailor
double precision, target          :: rainuni         !< [mm/hr] uniform rain intensity. {"rank": 0}
double precision                  :: wsx
double precision                  :: wsy
double precision                  :: rhoair          !< (kg/m3)
double precision                  :: PavBnd          !< average ambient pressure (N/m2) for correction on open boundaries
double precision                  :: PavIni          !< average ambient pressure (N/m2) for initial waterlevel correction
double precision                  :: paver           !< Average ambient pressure (N/m2)
double precision                  :: patmfac         !< 100 if Mbar, 1 if Pascal

double precision                  :: cdb(3)          !< breakpoints cd function cd coefficient
double precision                  :: wdb(3)          !< breakpoints cd function windspeed
integer                           :: ICdtyp          !< 1=Const; 2=Smith&Banke (2 pts); 3=S&B (3 pts); 4=Charnock 1955, 5=Hwang 2005, 6=Wuest 2005
double precision                  :: relativewind    !< factor for top layer speed in relative wind, 0=no, 1 =full top layer speed 
integer                           :: jawindhuorzwsbased   !< 1 = finite volume , 0 = hu
integer                           :: jawindpartialdry     !< Reduce windstress on water if link partially dry, only for bedlevtyp=3, 0 = no, 1 = yes
contains

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_wind() instead.
subroutine default_wind()
use m_physcoef, only : rhomean
    windsp  = 0
    winddir = 90d0        !< deg from north sailor
    rainuni = 0d0
    rhoair  = 1.2d0
    paver   = 101325.0
    Pavini  = 0d0
    PavBnd  = 0d0         !< default: no pressure correction on open boundaries.
                          !< choose ambient pressure on boundaries equal to overall standard ambient pressure
    patmfac = 1d0         !< 100 if Mbar, 1 if Pascal

    cdb(1)  = 0.00063d0   !< first  wind breakpoint
    wdb(1)  = 0
    cdb(2)  = 0.00723d0   !< second wind breakpoint
    wdb(2)  = 100
    cdb(3)  = 0.003d0     !< third  wind breakpoint
    wdb(3)  = 30
    icdtyp  = 2
    relativewind = 0d0    !< factor for top layer speed in wind relative wind, 0=no, 1 =full top layer speed   
    jawindhuorzwsbased   = 0    !< default: HU-based both in 2D and 3D (and not zws-based)
    jawindpartialdry     = 1    !< default: partially dry cells switched off

    windxav = 0d0
    windyav = 0d0

    ! Rain+qin+wind not reset every re-init, only upon new MDU load, because rain can be
    ! enabled by user in MDU (for BMI use, even without rain in external forcings file)
    jarain  = 0         !< use rain yes or no
    jaevap  = 0         !< use evap yes or no
    jaqin   = 0         !< use qin , sum of all in fluxes
    jaQext  = 0         !< use Qin externally provided yes or no
    jawind  = 0         !< use wind yes or no
    jastresstowind    = 0  !< if jawindstressgiven==1, convert stress to wind yes/no 1/0 
 
    ! Remaining of variables is handled in reset_wind()
    call reset_wind()
   end subroutine default_wind

   !> Resets only wind variables intended for a restart of flow simulation.
   !! Upon loading of new model/MDU, call default_wind() instead.
   subroutine reset_wind()
   japatm   = 0           !< use patm yes or no
   numlatsg = 0           !< [] nr of lateral discharge providers
   nlatnd   = 0           !< lateral nodes dimension, counter of nnlat(:)
   jaspacevarcharn   = 0  !< use space varying Charnock coefficients
   jawindstressgiven = 0  !< wind stress given in meteo file
   end subroutine reset_wind
end module m_wind
