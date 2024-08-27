module morphology_data_module
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!--module description----------------------------------------------------------
!
! This module defines the data structures for sediment transport and
! morphology.
!
!!--module declarations---------------------------------------------------------
use precision
use sediment_basics_module, only: SEDTYP_SILT, SEDTYP_SAND
use flocculation, only: FLOC_NONE
use handles, only:handletype
use properties, only:tree_data
use m_tables, only:t_table

implicit none

private

!
! public data types
!
public morpar_type
public moroutputtype
public mornumericstype
public bedbndtype
public cmpbndtype
public sedpar_type
public parfile_type
public trapar_type
public sedtra_type
public fluffy_type
public t_noderelation
public t_nodefraction
public t_nodereldata

!
! public routines
!
public nullmorpar
public clrmorpar
public nullsedpar
public clrsedpar
public nulltrapar
public clrtrapar
public nullsedtra
public allocsedtra
public clrsedtra
public allocfluffy
public initmoroutput
public get_transport_parameters
public get_one_transport_parameter

! define a missing value consistent with netCDF _fillvalue
real(fp), parameter, public :: missing_value = 9.9692099683868690e+36_fp

integer, parameter, public :: CHARLEN = 40

integer, parameter, public :: PARSOURCE_UNKNOWN = 0
integer, parameter, public :: PARSOURCE_FIELD   = 1
integer, parameter, public :: PARSOURCE_TIME    = 2

integer, parameter, public :: RP_TIME  =  1     ! time since reference date [s]
integer, parameter, public :: RP_EFUMN =  2     ! U component of effective depth averaged velocity [m/s]
integer, parameter, public :: RP_EFVMN =  3     ! V component of effective depth averaged velocity [m/s]
integer, parameter, public :: RP_EFVLM =  4     ! effective depth averaged flow velocity magnitude [m/s]
integer, parameter, public :: RP_UCHAR =  5     ! U component of characteristic flow velocity [m/s]
integer, parameter, public :: RP_VCHAR =  6     ! V component of characteristic flow velocity [m/s]
integer, parameter, public :: RP_VELCH =  7     ! characteristic flow velocity magnitude [m/s]
integer, parameter, public :: RP_ZVLCH =  8     ! elevation above bed at which characteristic velocity is given [m]
integer, parameter, public :: RP_DEPTH =  9     ! water depth [m]
integer, parameter, public :: RP_CHEZY = 10     ! Chezy roughness [m0.5/s]
integer, parameter, public :: RP_HRMS  = 11     ! wave height [m]
integer, parameter, public :: RP_TPEAK = 12     ! peak wave period [s]
integer, parameter, public :: RP_TETA  = 13     ! wave angle [deg pos counter-clockwise relative to U direction]
integer, parameter, public :: RP_RLAMB = 14     ! wave length [m]
integer, parameter, public :: RP_UORB  = 15     ! orbital velocity [m/s]
integer, parameter, public :: RP_D50   = 16     ! D50 of sediment fraction [m]
integer, parameter, public :: RP_DSS   = 17     ! effective suspended sediment diameter of sediment fraction [m]
integer, parameter, public :: RP_DSTAR = 18     ! Dstar of sediment fraction [m]
integer, parameter, public :: RP_D10MX = 19     ! D10 of particle size mix of the part of the bed exposed to transport [m]
integer, parameter, public :: RP_D90MX = 20     ! D90 of particle size mix of the part of the bed exposed to transport [m]
integer, parameter, public :: RP_MUDFR = 21     ! mud fraction of particle size mix of the part of the bed exposed to transport [-]
integer, parameter, public :: RP_HIDEX = 22     ! hiding-exposure factor correcting the shear stress [-]
integer, parameter, public :: RP_SETVL = 23     ! settling velocity [m/s]
integer, parameter, public :: RP_RHOSL = 24     ! solid density of sediment [kg/m3]
integer, parameter, public :: RP_RHOWT = 25     ! density of water [kg/m3]
integer, parameter, public :: RP_SALIN = 26     ! salinity [ppt]
integer, parameter, public :: RP_TEMP  = 27     ! temperature [deg C]
integer, parameter, public :: RP_GRAV  = 28     ! gravitational acceleration [m2/s]
integer, parameter, public :: RP_VICML = 29     ! molecular viscosity [m2/s]
integer, parameter, public :: RP_TAUB  = 30     ! bed shear stress [N/m2]
integer, parameter, public :: RP_UBED  = 31     ! U component of near-bed velocity [m/s]
integer, parameter, public :: RP_VBED  = 32     ! V component of near-bed velocity [m/s]
integer, parameter, public :: RP_VELBD = 33     ! near-bed velocity magnitude [m/s]
integer, parameter, public :: RP_ZVLBD = 34     ! elevation above bed at which near-bed velocity is given [m]
integer, parameter, public :: RP_VNKAR = 35     ! von Karman constant [-]
integer, parameter, public :: RP_Z0CUR = 36     ! current related roughness height [m]
integer, parameter, public :: RP_Z0ROU = 37     ! wave enhanced roughness height [m]
integer, parameter, public :: RP_KTUR  = 38     ! flow induced turbulence [m2/s2]
integer, parameter, public :: RP_DG    = 39     ! geometric mean sediment diameter of the part of the bed exposed to transport [m]
integer, parameter, public :: RP_SNDFR = 40     ! sand fraction of particle size mix of the part of the bed exposed to transport [-]
integer, parameter, public :: RP_DGSD  = 41     ! geometric standard deviation of particle size mix of the part of the bed exposed to transport [m]
integer, parameter, public :: RP_UMEAN = 42     ! U component of velocity [m/s]
integer, parameter, public :: RP_VMEAN = 43     ! V component of velocity [m/s]
integer, parameter, public :: RP_VELMN = 44     ! velocity magnitude [m/s]
integer, parameter, public :: RP_USTAR = 45     ! effective shear velocity [m/s]
integer, parameter, public :: RP_KWTUR = 46     ! wave breaking induced turbulence
integer, parameter, public :: RP_UAU   = 47     ! U component of velocity asymmetry due to short waves [m/s]
integer, parameter, public :: RP_VAU   = 48     ! V component of velocity asymmetry due to short waves [m/s]
integer, parameter, public :: RP_BLCHG = 49     ! bed level change rate (needed for dilatancy calculation in van Thiel formulation) [m/s]
integer, parameter, public :: RP_D15MX = 50     ! D15 of particle size mix of the part of the bed exposed to transport [m]
integer, parameter, public :: RP_POROS = 51     ! porosity of particle size mix of the part of the bed exposed to transport [-]
integer, parameter, public :: RP_DZDX  = 52     ! U component of bed slope [-]
integer, parameter, public :: RP_DZDY  = 53     ! V component of bed slope [-]
integer, parameter, public :: RP_DM    = 54     ! median sediment diameter of particle size mix of the part of the bed exposed to transport [m]
integer, parameter, public :: RP_ZB    = 55     ! bed level (positive up) [m]
integer, parameter, public :: RP_DBG   = 56     ! debug array value from eqtran [-]
integer, parameter, public :: MAX_RP   = 56     ! maximum number of real parameters
!
integer, parameter, public :: IP_NM    =  1     ! local (i.e. within partition) cell index
integer, parameter, public :: IP_N     =  2     ! local (i.e. within partition) fastest dimension index -- only for structured mesh models
integer, parameter, public :: IP_M     =  3     ! local (i.e. within partition) slowest dimension index -- only for structured mesh models
integer, parameter, public :: IP_ISED  =  4     ! sediment fraction number
integer, parameter, public :: IP_NST   =  5     ! time step number -- only for fixed time step models
integer, parameter, public :: MAX_IP   =  5     ! maximum number of integer parameters
!
integer, parameter, public :: SP_RUNID =  1     ! ID of simulation
integer, parameter, public :: SP_USRFL =  2     ! name of user specified input file
integer, parameter, public :: MAX_SP   =  2     ! maximum number of strings

integer, parameter, public :: WS_FORM_FUNCTION_SALTEMCON    = 1
integer, parameter, public :: WS_FORM_FUNCTION_DSS          = 2
integer, parameter, public :: WS_FORM_FUNCTION_DSS_2004     = -2
integer, parameter, public :: WS_FORM_MANNING_DYER_MACRO    = 3
integer, parameter, public :: WS_FORM_MANNING_DYER_MICRO    = 4
integer, parameter, public :: WS_FORM_MANNING_DYER          = 5
integer, parameter, public :: WS_FORM_CHASSAGNE_SAFAR_MACRO = 6
integer, parameter, public :: WS_FORM_CHASSAGNE_SAFAR_MICRO = 7
integer, parameter, public :: WS_FORM_CHASSAGNE_SAFAR       = 8
integer, parameter, public :: WS_FORM_USER_ROUTINE          = 15

integer, parameter, public :: WS_RP_TIME  =  1 ! Time
integer, parameter, public :: WS_RP_ULOC  =  2 ! Horizontal velocity component 1 [m/s]
integer, parameter, public :: WS_RP_VLOC  =  3 ! Horizontal velocity component 2 [m/s]
integer, parameter, public :: WS_RP_WLOC  =  4 ! Vertical velocity [m/s]
integer, parameter, public :: WS_RP_SALIN =  5 ! Salinity [ppt]
integer, parameter, public :: WS_RP_TEMP  =  6 ! Water temperature [degC]
integer, parameter, public :: WS_RP_RHOWT =  7 ! Water density [kg/m3]
integer, parameter, public :: WS_RP_CFRCB =  8 ! Concentration of fraction [kg/m3]
integer, parameter, public :: WS_RP_CTOT  =  9 ! Total sediment concentration [kg/m3]
integer, parameter, public :: WS_RP_KTUR  = 10 ! Turbulent kinetic energy [m2/s2]
integer, parameter, public :: WS_RP_EPTUR = 11 ! Turbulent dissipation [m2/s3]
integer, parameter, public :: WS_RP_D50   = 12 ! Median grainsize [m]
integer, parameter, public :: WS_RP_DSS   = 13 ! Median grainsize in suspension [m]
integer, parameter, public :: WS_RP_RHOSL = 14 ! Mineral density [kg/m3]
integer, parameter, public :: WS_RP_CSOIL = 15 ! Hindered settling reference density [kg/m3]
integer, parameter, public :: WS_RP_GRAV  = 16 ! Gravitational acceleration [m/s2]
integer, parameter, public :: WS_RP_VICML = 17 ! Molecular viscosity
integer, parameter, public :: WS_RP_WDEPT = 18 ! Water depth [m]
integer, parameter, public :: WS_RP_UMEAN = 19 ! Depth-averaged flow velocity component 1 [m/s]
integer, parameter, public :: WS_RP_VMEAN = 20 ! Depth-averaged flow velocity component 2 [m/s]
integer, parameter, public :: WS_RP_CHEZY = 21 ! Chezy roughness
integer, parameter, public :: WS_RP_SHTUR = 22 ! Turbulent shear stress [N/m2]
integer, parameter, public :: WS_RP_CCLAY = 23 ! Clay concenrtration [kg/m3]
integer, parameter, public :: WS_MAX_RP   = 23
!
integer, parameter, public :: WS_IP_NM    =  1
integer, parameter, public :: WS_IP_N     =  2
integer, parameter, public :: WS_IP_M     =  3
integer, parameter, public :: WS_IP_K     =  4
integer, parameter, public :: WS_IP_ISED  =  5
integer, parameter, public :: WS_MAX_IP   =  5
!
integer, parameter, public :: WS_SP_RUNID =  1
integer, parameter, public :: WS_SP_USRFL =  2
integer, parameter, public :: WS_MAX_SP   =  2

integer, parameter, public :: CODE_DEFAULT = 0
integer, parameter, public :: CODE_DELFT3D = 1

integer,parameter, public  :: FLUX_LIMITER_NONE   = 0
integer,parameter, public  :: FLUX_LIMITER_MINMOD = 1
integer,parameter, public  :: FLUX_LIMITER_MC     = 2

integer,parameter, public  :: MOR_STAT_MIN = 1
integer,parameter, public  :: MOR_STAT_MAX = 2
integer,parameter, public  :: MOR_STAT_MEAN= 4
integer,parameter, public  :: MOR_STAT_STD = 8
integer,parameter, public  :: MOR_STAT_CUM = 16

integer,parameter,public   :: MOR_STAT_TIME= 1
integer,parameter,public   :: MOR_STAT_BODS= 2
!
! Soulsby & Clarke skin friction options
!
integer,parameter,public   :: SC_MUDTHC  = 1
integer,parameter,public   :: SC_MUDFRAC = 2
!
! collection of morphology output options
!
type moroutputtype
    integer :: transptype      ! 0 = mass
                               ! 1 = volume including pores
                               ! 2 = volume excluding pores
    !
    character(len=30), dimension(4) :: statqnt = (/"H1  ","UV  ","SBUV","SSUV"/)
    character(len=30), dimension(4) :: statnam = (/"water depth              ", &
                                                   "velocity                 ", &
                                                   "total bedload transport  ",  &
                                                   "total suspended transport"/)
    character(len=30), dimension(4) :: statunt = (/"m  ","m/s","   ","   "/)
    !integer, dimension(5,4)         :: statflg  ! 1 = waterdepth, 2 = velocity, 3 = bedload, 4 = suspload
    integer, dimension(6,4)         :: statflg  ! 1 = waterdepth, 2 = velocity, 3 = bedload, 4 = suspload
    integer                         :: nstatqnt ! number of quantities for morphology statistics output
    integer                         :: weightflg ! weighting by time or dbodsd
    real(fp), dimension(3)          :: avgintv  ! interval, start, stop for writing statistics (FM only)
    !
    logical :: aks
    logical :: cumavg
    logical :: morstats
    logical :: dg
    logical :: dgsd
    logical :: dm
    logical :: dmsedcum
    logical :: dpbedlyr
    logical :: dzduuvv
    logical :: fixfac
    logical :: hidexp
    logical :: frac
    logical :: lyrfrac
    logical :: msed
    logical :: mudfrac
    logical :: percentiles
    logical :: poros
    logical :: rca
    logical :: rsedeq
    logical :: sandfrac
    logical :: sbuuvv
    logical :: sbcuv
    logical :: sscuv
    logical :: sbcuuvv
    logical :: sbwuv
    logical :: sbwuuvv
    logical :: sedpar
    logical :: seddif
    logical :: ssuuvv
    logical :: sswuv
    logical :: sswuuvv
    logical :: suvcor
    logical :: sourcesink
    logical :: taub
    logical :: taurat
    logical :: umod
    logical :: ustar
    logical :: uuuvvv
    logical :: ws
    logical :: zumod
    logical :: rawtransports    ! output flag for transports before upwinding/bed slope effects
    logical :: blave
    logical :: bamor
    logical :: wumor
end type moroutputtype

!
! sediment transport and morphology numerical settings
!
type mornumericstype
    logical :: pure1d                   ! temporary switch for 1D treatment in FM
    logical :: upwindbedload            ! switch for upwind bedload in UPWBED
    logical :: laterallyaveragedbedload ! bedload transport laterally averaged in UPWBED
    logical :: maximumwaterdepth        ! limit minimum water depth at zeta point for morphodynamics 
    double precision :: maximumwaterdepthfrac   ! if `maximumwaterdepth=.true.`, the minimum water depth
                                        ! at zeta point is `maximumwaterdepthfrac` times the 
                                        ! maximum water depth at active velocity points.
    integer :: fluxlim                  ! flux limiter choice
end type mornumericstype

!
! morphology boundary conditions at one open boundary
!
type bedbndtype
    integer                         :: icond    ! bed boundary condition
                                                !   0: "free boundary" (default if updinf = true)
                                                !   1: fixed bed level (default if updinf = false)
                                                !   2: prescribed bed level
                                                !   3: prescribed bed level change rate
                                                !   4: prescribed bed load transport rate
    integer , dimension(4)          :: ibcmt    ! boundary conditions morphology table
                                                !  (1) table index in bcm file
                                                !  (2) first index of boundary parameter
                                                !  (3) number of entries
                                                !  (4) last used record in table
    integer                         :: npnt     ! number points of boundary
    integer , dimension(:), pointer :: idir
    integer , dimension(:), pointer :: nm       ! outside boundary cell
    integer , dimension(:), pointer :: nxmx     ! inside boundary cell
    integer , dimension(:), pointer :: lm       ! "flow link"
    real(fp), dimension(:), pointer :: alfa_mag
    real(fp), dimension(:), pointer :: alfa_dist
end type bedbndtype

!
! bed composition boundary conditions at one open boundary
!
type cmpbndtype
    integer                :: icond    ! bed composition condition
                                       !   0: "free composition"
                                       !   1: fixed composition (default)
                                       !   2: prescribed composition
    integer , dimension(4) :: ibcmt    ! boundary conditions morphology table
                                       !  (1) table index in bcm file
                                       !  (2) first index of boundary parameter
                                       !  (3) number of entries
                                       !  (4) last used record in table
end type cmpbndtype

type fluffy_type
    !
    ! doubles (hp)
    !
    !
    ! single / doubles (fp)
    !
    !
    ! singles (sp)
    !
    !
    ! integers
    !
    integer :: iflufflyr  !  switch for fluff layer concept
                          !  0: no fluff layer
                          !  1: all mud to fluff layer, burial to bed layers
                          !  2: part mud to fluff layer, other part to bed layers (no burial) 
    !
    ! pointers
    !
    real(fp)      , dimension(:)    , pointer :: mfluni        ! constant fluff mass
    real(fp)      , dimension(:,:)  , pointer :: mfluff        ! composition of fluff layer: mass of mud fractions [kg /m2]
    real(fp)      , dimension(:,:)  , pointer :: bfluff0       ! burial parameter fluff layer (only when FluffLayer=1) [kg/m2/s]
    real(fp)      , dimension(:,:)  , pointer :: bfluff1       ! burial parameter fluff layer (only when FluffLayer=1) [1/s]
    real(fp)      , dimension(:,:)  , pointer :: depfac        ! Deposition factor to fluff layer (only when FluffLayer=2) [-]
    real(fp)      , dimension(:,:)  , pointer :: sinkf         ! Settling to fluff layer []
    real(fp)      , dimension(:,:)  , pointer :: sourf         ! Source from fluff layer [] 
    character(256), dimension(:)    , pointer :: mflfil        ! fluff mass file
    ! 
    ! logicals
    !
    !
    ! characters
    !
    character(256) :: bfluff0_fil      !  name of file for burial parameter 1
    character(256) :: bfluff1_fil      !  name of file for burial parameter 2
    character(256) :: depfac_fil       !  name of file for deposition factor
    !
end type fluffy_type

type morpar_type
    !
    ! doubles (hp)
    !
    real(hp):: hydrt      !  hydraulic time (only used to compute the average morphological factor)
    real(hp):: hydrt0     !  initial hydraulic time (only used to compute the average morphological factor)
    real(hp):: morft      !  morphological time
    real(hp):: morft0     !  initial morphological time
    !
    ! single / doubles (fp)
    !
    real(fp):: morfac     !  morphological timescale factor
    real(fp):: thresh     !  threshold value for slowing erosion near a fixed layer (m)
    real(fp):: aksfac     !  factor for setting aks height
    real(fp):: rwave      !  factor for calculating wave-related roughness from ripple dimensions
    real(fp):: alfabs     !  factor for longitudinal bed load transport
    real(fp):: alfabn     !  factor for transverse bed load transport
    real(fp):: camax      !  Maximum volumetric reference concentration
    real(fp):: dzmax      !  factor for limiting source and sink term in EROSED (percentage of water depth)
    real(fp):: sus        !  calibration factor for suspended load transport
    real(fp):: suscorfac  !  calibration factor for near-bed suspended load transport correction
    real(fp):: bed        !  calibration factor for bed load transport
    real(fp):: pangle     !  phase lead angle acc. to Nielsen (1992) for TR2004 expression
    real(fp):: fpco       !  coefficient for phase llag effects
    real(fp):: factcr     !  calibration factor on Shields' critical shear stress   
    real(fp):: ti_sedtrans!  time where calculation for sediment transport computation start (tunit relative to ITDATE,00:00:00)
    real(fp):: tmor       !  time where calculation for morphological changes start (tunit relative to ITDATE,00:00:00)
    real(fp):: tcmp       !  time where calculation for bed composition changes start (tunit relative to ITDATE,00:00:00)
    real(fp):: thetsduni  !  uniform value for dry cell erosion factor
    real(fp):: susw       !  calibration factor for wave-related suspended sand transport (included in bed-load)
    real(fp):: sedthr     !  minimum depth for sediment calculations
    real(fp):: hmaxth     !  maximum depth for setting theta for erosion of dry bank
    real(fp):: bedw       !  calibration factor for wave-related bed-load sand transport (included in bed-load)
    real(fp):: factsd     !  calibration factor for 2D suspended load relaxation time
    real(fp):: rdw
    real(fp):: rdc
    real(fp):: espir      !  factor for weighing the effect of the spiral flow intensity in 2D simulations
    real(fp):: ashld      !  bed slope direction effect Shields' parameter
    real(fp):: bshld      !  bed slope direction effect Shields' parameter (power of theta_i)
    real(fp):: cshld      !  bed slope direction effect Shields' parameter (power of d_i/depth)
    real(fp):: dshld      !  bed slope direction effect Shields' parameter (power of d_i/d_m)
    real(fp):: coulfri    !  Coulomb friction coef. in formula of Parker and Andrews
    real(fp):: flfdrat    !  ratio of lift and drag forces (Fl/Fd) in formula of Parker and Andrews
    real(fp):: alfpa      !  coulfri/(1+coulfri*flfdrat)
    real(fp):: thcrpa     !  bed slope theta_cr in formula of Parker and Andrews
    real(fp):: asklhe     !  exponent alpha in hiding & exposure according Soehngen, Kellermann, Loy
    real(fp):: mwwjhe     !  exponent m factor in hiding & exposure according Wu, Wang, Jia
    real(fp):: ttlalpha   !  transport layer thickness: proportionality factor
    real(fp):: ttlmin     !  transport layer thickness: minimum thickness
    real(fp):: wetslope   !  maximum wet bed slope (used for avalanching)
    real(fp):: dryslope   !  maximum dry bed slope (used for avalanching)
    real(fp):: avaltime   !  time scale in seconds (used for avalanching)
    real(fp):: hswitch    !  depth to switch dryslope and wetslope
    real(fp):: dzmaxdune  !  Maximum bed level change per hydrodynamic time step
    real(fp):: bermslope       !  Swash zone slope for (semi-) reflective beaches
    real(fp):: bermslopefac    !  Bed slope transport factor for bermslope model
    real(fp):: bermslopegamma  !  Wave height - water depth ratio to turn on bermslope swash transport
    real(fp):: bermslopedepth  !  Depth to turm on berm slope swash transport
    !
    !  (sp)
    !
    !
    ! integers
    !
    integer :: mergehandle !  stream handle for communication with mormerge
    integer :: i10         !  index of D10 in the xx array
    integer :: i15         !  index of D15 in the xx array
    integer :: i50         !  index of D50 in the xx array
    integer :: i90         !  index of D90 in the xx array
    integer :: ihidexp     !  switch for hiding exposure effect
                           !  1: none
                           !  2: Egiazaroff (1965)
                           !  3: Ashida & Michiue (1971), modified Egiazaroff
                           !  4: Soehngen, Kellermann, Loy (1992)
                           !  5: Wu, Wang, Jia (2000)
    integer :: itmor       !  time step where calculation for bed level updating starts
    integer :: itcmp       !  time step where calculation for bed composition updating starts
    integer :: iti_sedtrans!  Sediment transport computation start time step
    integer :: iopkcw
    integer :: iopsus
    integer :: islope      !  switch for bed slope effect, according
                           !  1: none
                           !  2: Bagnold, Ikeda/Van Rijn
                           !  3: Van Bendegom, Koch&Flokstra
    integer :: morfacpar   ! parameter index of morfac in table structure
                           ! only used when varyingmorfac=true
    integer :: morfacrec   ! record index of morfac in table structure
                           ! only used when varyingmorfac=true
    integer :: morfactable ! table index of morfac in table structure
                           ! only used when varyingmorfac=true
    integer :: nxx         !  number of percentiles in xx
    integer :: subiw       ! wave period subdivision in TR2004    
    integer :: ttlform     !  switch for thickness of transport layer
                           !  1: fixed (user-spec.) thickness
                           !  2: 
                           !  3: 
    integer :: telform     !  switch for thickness of exchange layer
                           !  1: fixed (user-spec.) thickness
    !
    ! pointers
    !
    type (fluffy_type)                  , pointer :: flufflyr   ! data for optional fluff layer
    type (handletype)                             :: bcmfile    ! tables containing morphological boundary conditions
    type (handletype)                             :: morfacfile ! table  containing morphological factor
    type (moroutputtype)                , pointer :: moroutput  ! structure containing morphology output options
    type (mornumericstype)              , pointer :: mornum     ! structure containing numerical settings
    type (bedbndtype)     , dimension(:), pointer :: morbnd     ! transport / bed level boundary parameters
    type (cmpbndtype)     , dimension(:), pointer :: cmpbnd     ! bed composition boundary parameters
    real(hp)              , dimension(:), pointer :: mergebuf   ! buffer array for communcation with mormerge
    real(fp)              , dimension(:), pointer :: xx         ! percentile xx (dxx stored in erosed.ig*)
    real(fp)              , dimension(:), pointer :: thetsd     ! global dry bank erosion factor
    ! 
    ! logicals
    !
    logical :: bedupd              !  flag for doing bed level updates
    logical :: cmpupd              !  flag for doing composition (underlayer) updates
    logical :: eqmbcsand           !  flag for setting equilibrium sediment concentration profiles at the open boundaries for sand sediment
    logical :: eqmbcmud            !  flag for setting equilibrium sediment concentration profiles at the open boundaries for mud sediment
    logical :: densin              !  Flag=TRUE when sediment is included in fluid density calculations flag for including sediment in fluid density calculations
    logical :: rouse               !  flag for setting equilibrium concentrations to standard Rouse profiles
    logical :: epspar
    logical :: eulerisoglm         !  Flag for using eulerian velocities for suspended transports    
    logical :: glmisoeuler         !  Flag for using GLM velocities for bedload transports and reference concentrations   
    logical :: updinf              !  Flag for updating bottom at inflow openboundaries
    logical :: neglectentrainment  !  flag for neglecting entrainment and suspension in the mass balance (mass balance based on suspended load fluxes)
    logical :: oldmudfrac          !  true: use the old method for the mud source term calculation (without Frac multiplication)
    logical :: varyingmorfac       !  true: morfac specified in a time serie file
    logical :: multi               !  Flag for merging bottoms of different parallel runs
    logical :: duneavalan          !  Flag for avalanching using wetslope and dryslope
    logical :: l_suscor            !  Flag for applying correction to doublecounting of sus/bed transport in 3d
    logical :: bermslopetransport  !  Flag to turn on bermslope swash transport model
    logical :: bermslopebed        !  Flag to turn on bermslope swash transport model for bedload
    logical :: bermslopesus        !  Flag to turn on bermslope swash transport model for suspended load
    
    !
    ! characters
    !
    character(256) :: bcmfilnam    !  name of input  file for morphological boundary conditions
    character(256) :: flcomp       !  name of file containing initial bed composition
    character(256) :: mmsyncfilnam !  name of output file for synchronisation of mormerge run
    character(256) :: telfil       !  name of file containing exchange layer thickness
    character(256) :: ttlfil       !  name of file containing transport layer thickness
    character(256) :: flsthetsd    !  name of file containing dry cell erosion factor
    !
end type morpar_type

type t_noderelation
   character(len=CHARLEN)                         :: Node       = ' '
   integer                                        :: NodeIdx    = 0    !< Cell centre index 
   character(len=CHARLEN)                         :: BranchIn   = ' '
   integer                                        :: BranchInLn = 0    !< Link index 
   character(len=CHARLEN)                         :: BranchOut1 = ' '
   integer                                        :: BranchOut1Ln = 0   !< Link index 
   character(len=CHARLEN)                         :: BranchOut2 = ' '
   integer                                        :: BranchOut2Ln = 0   !< Link index 
   character(len=CHARLEN)                         :: tableName  = ' '
   character(len=CHARLEN)                         :: Method     = ' '
   real(fp)                                       :: expQ       = -1.0_fp
   real(fp)                                       :: expW       = -1.0_fp
   type(t_table), pointer                         :: Table
end type t_noderelation

type t_nodefraction
   character(20)                                   :: Name
   character(256)                                  :: tableFile      = ' '  ! Name of Table File for Node Relations
   integer                                         :: nNodeRelations = 0
   type(t_noderelation), pointer, dimension(:)     :: noderelations
end type t_nodefraction

type t_nodereldata
    integer                                     :: nFractions       = 0
    logical                                     :: NRD_Overall
    logical                                     :: NRD_Default      = .false.
    character(256), dimension(:), pointer       :: flnrd            !  Files with Node Relation Data (NRD-Files)
    type(t_nodefraction), pointer, dimension(:) :: nodefractions
end type t_nodereldata

type sedpar_type
    !
    ! doubles
    !
    real(fp) :: csoil     !  concentration at bed used in hindered settling formulation
    real(fp) :: mdcuni    !  mud content / mud fraction uniform value (non-zero only
                          !  if mud is not included simulation)
    real(fp) :: kssilt    !  ks value for silt for Soulsby 2004 formulation (used below sc_cmf1)
    real(fp) :: kssand    !  ks value for sand (used above sc_cmf2)
    real(fp) :: sc_cmf1   !  lower critical mud factor for determining bed roughness length for Soulsby & Clarke (2005)
    real(fp) :: sc_cmf2   !  upper critical mud factor for determining bed roughness length for Soulsby & Clarke (2005)
    real(fp) :: sc_flcf   !  fraction of ParFluff0/ParFluff1 when the fluff layer fully covers the bed for Soulsby & Clarke (2005)
    real(fp) :: tbreakup  !  relaxation time scale for break-up of flocs [s]
    real(fp) :: tfloc     !  relaxation time scale for flocculation [s]
    real(fp) :: d_micro   !  characteristic diameter of micro flocs [m]
    real(fp) :: ustar_macro   ! characteristic shear velocity of macro flocs [m/s]
    real(fp) :: version   !  interpreter version
    !
    ! reals
    !
    !
    ! integers
    !
    integer  :: flocmod        !  flocculation model applied to clay fractions
    integer  :: nflocpop       !  number of floc populations (groups of clay fractions that exchange mass)
    integer  :: nflocsizes     !  number of floc sizes distinguished in the flocculation model
    integer  :: nmudfrac       !  number of simulated mud fractions
    integer  :: sc_mudfac      !  formulation used for determining bed roughness length for Soulsby & Clarke (2005): SC_MUDFRAC, or SC_MUDTHC
    integer  :: max_mud_sedtyp !  largest sediment type associated with mud
    integer  :: min_dxx_sedtyp !  smallest sediment type included in computation of characteristic sediment diameters
    !
    ! pointers
    !
    type(tree_data)     , dimension(:), pointer :: sedblock => null()  !  Pointer to array of data block per fraction in .sed file (version 2)
    type(t_nodefraction), dimension(:), pointer :: nodefractions       !  Pointer to array of nodal point relations
    !
    logical       , dimension(:)    , pointer :: cmpupdfrac !  Flag for doing composition (underlayer) updates per fraction
    !
    real(fp)      , dimension(:)    , pointer :: tpsnumber  !  Turbulent Prandtl-Schmidt number
    real(fp)      , dimension(:)    , pointer :: rhosol     !  Soil density
    !
    real(fp)      , dimension(:,:,:), pointer :: logseddia             !  Characteristic sediment diameter table using log scale [%,log(m)]
    real(fp)      , dimension(:)    , pointer :: logsedsig             !  Standard deviation on log scale (log of geometric std.) [-]
    real(fp)      , dimension(:)    , pointer :: sedd10                !  10% Diameter sediment fraction [m]
    real(fp)      , dimension(:)    , pointer :: sedd50                !  50% Diameter sediment fraction [m]
    real(fp)      , dimension(:)    , pointer :: sedd50fld  => null()  !  Spatially varying 50% sediment diameter [m]
    real(fp)      , dimension(:)    , pointer :: seddm                 !  Arithmetic mean sediment diameter [m]
    real(fp)      , dimension(:)    , pointer :: sedd90                !  90% Diameter sediment fraction [m]
    !
    real(fp)      , dimension(:)    , pointer :: cdryb      !  Dry bed concentration for determining
                                                            !  sediment depths
    real(fp)      , dimension(:)    , pointer :: dstar      !  Dimensionless grain size 
    real(fp)      , dimension(:)    , pointer :: taucr      !  Critical shear stress 
    real(fp)      , dimension(:)    , pointer :: tetacr     !  Dimensionless critical shear stress (Shields parameter)
    !
    real(fp)      , dimension(:,:)  , pointer :: dss        !  Characteristic suspended sediment diameter
    real(fp)      , dimension(:)    , pointer :: facdss     !  Ratio between suspended sediment diameter and D50
    real(fp)      , dimension(:)    , pointer :: sdbuni     !  Uniform value of initial sediment mass at bed
    real(fp)      , dimension(:)    , pointer :: sedtrcfac  !  Calibration factor for tracer sediments
    real(fp)      , dimension(:)    , pointer :: thcmud     !  Critical stress erosion uniform values for mud
    real(fp)      , dimension(:)    , pointer :: tcguni     !  Calibration factor on critical shear stress in Van Rijn (2004) uniform values
    real(fp)      , dimension(:)    , pointer :: mudcnt     !  mud content / mud fraction field (non-zero only if mud
                                                            !  is not included simulation)
    real(fp)      , dimension(:)    , pointer :: pmcrit     !  Critical mud fraction for non-cohesive behaviour
    integer       , dimension(:)    , pointer :: nseddia    !  Number of characteristic sediment diameters
    integer       , dimension(:)    , pointer :: sedtyp     !  Sediment type: SEDTYP_CLAY, SEDTYP_SILT, SEDTYP_SAND, SEDTYP_GRAVEL
    integer       , dimension(:)    , pointer :: tratyp     !  Transport method type: TRA_BEDLOAD, TRA_ADVDIFF, TRA_COMBINE
    integer       , dimension(:)    , pointer :: flocsize   !  Floc size within floc population
    integer       , dimension(:,:)  , pointer :: floclist   !  Table of groups of clay fractions that belong together (flocculation)
    character(10) , dimension(:)    , pointer :: inisedunit !  'm' or 'kg/m2' : Initial sediment at bed specified as thickness ([m]) or density ([kg/m2])
    character(20) , dimension(:)    , pointer :: namsed     !  Names of all sediment fractions
    character(20) , dimension(:)    , pointer :: namclay    !  Label of clay floc population to which the sediment fraction belongs
    character(20) , dimension(:)    , pointer :: namflocpop !  Clay floc population labels
    character(256), dimension(:)    , pointer :: flsdbd     !  File name containing initial sediment mass at bed
    character(256), dimension(:)    , pointer :: flstcg     !  File name calibration factor on critical shear stress in Van Rijn (2004) uniform values
    character(256), dimension(:)    , pointer :: flnrd      !  File names of Node Relation Data (NRD-Files) for bifurcation points in 1D morphology
    ! 
    ! logicals
    !
    logical :: anymud     ! Flag to indicate whether a mud fraction is included in the simulation.
    logical :: bsskin     ! Flag to indicate whether a bed stress should be computed according to Soulsby 2004
    !
    ! characters
    !
    character(256) :: flsdia     ! spatial sediment diameter file (in case of one sediment fraction only)
    character(256) :: flsmdc     ! mud content / mud fraction file (only if mud is not included in simulation)
    character(256) :: flspmc     ! critical mud fraction for non-cohesive behaviour file
end type sedpar_type

type parfile_type
    integer                                   :: source     !< data source for the parameter: time-series or spatial field
    !
    integer                                   :: ipar_ts    !< parameter number within the time series object
    integer                                   :: irec_ts    !< most recently used time series record
    integer                                   :: itable_ts  !< table nunber within the time series object
    integer                                   :: npar_ts    !< number of parameter values within time series object (should be 1)
    integer                                   :: refjulday  !< Julian reference date
    real(fp)                                  :: timhr      !< time of previous value
    real(fp)                                  :: par        !< previous parameter value
    !
    real(fp), allocatable                     :: parfld(:)  !< spatial field
    type(handletype)                          :: ts         !< time series object
end type parfile_type

type trapar_type
    !
    ! doubles
    !
    ! reals
    !
    ! integers
    !
    integer                                    :: max_integers_settle !  Maximum number of integers which can be delivered to shared library
    integer                                    :: max_reals_settle    !  Maximum number of reals which can be delivered to shared library
    integer                                    :: max_strings_settle  !  Maximum number of character strings which can be delivered to shared library
    !
    integer                                    :: max_integers        !  Maximum number of integers which can be delivered to shared library
    integer                                    :: max_reals           !  Maximum number of reals which can be delivered to shared library
    integer                                    :: max_strings         !  Maximum number of character strings which can be delivered to shared library
    integer                                    :: npar                !  Maximum number of sediment transport formula parameters
    integer                                    :: nparfile            !  Number of sediment transport formula parameters specified by file
    integer                                    :: nouttot             !  Total number of output parameters (sum of noutpar)
    !
    ! pointers
    !
    integer          , dimension(:)  , pointer :: noutpar             !< (lsedtot) Number of output parameters per sediment fraction i1
    integer          , dimension(:,:), pointer :: ioutpar             !< (max, lsedtot) Index of output parameter i2 of sediment fraction i1 into XX array
    real(fp)         , dimension(:,:), pointer :: outpar              !< (noutpar,nmmax) Sediment transport parameters spatially varying
    character(256)   , dimension(:,:), pointer :: outpar_name         !< (max, lsedtot) Name of sediment transport parameter i2 of sediment fraction i1
    character(256)   , dimension(:,:), pointer :: outpar_longname     !< (max, lsedtot) Long name of sediment transport parameter i2 of sediment fraction i1
    !
    character(256)   , dimension(:)  , pointer :: dll_function_settle !< Name of subroutine in DLL that calculates the Settling velocity
    character(256)   , dimension(:)  , pointer :: dll_name_settle     !< Name of DLL that contains the Settling velocity subroutine
    integer(pntrsize), dimension(:)  , pointer :: dll_handle_settle   !< Handle of DLL that contains the Settling velocity subroutine
    integer          , dimension(:)  , pointer :: dll_integers_settle !< Input integer array to shared library
    real(hp)         , dimension(:)  , pointer :: dll_reals_settle    !< Input real array to shared library
    character(256)   , dimension(:)  , pointer :: dll_strings_settle  !< Input character string array to shared library
    character(256)   , dimension(:)  , pointer :: dll_usrfil_settle   !< Name of input file to be passed to subroutine in DLL
    integer          , dimension(:)  , pointer :: iform_settle        !< Number of sediment settling velocity formula
    real(fp)         , dimension(:,:), pointer :: par_settle          !< Settling velocity formula parameters
    !
    character(256)   , dimension(:)  , pointer :: dll_function        !< Name of subroutine in DLL that calculates the Sediment transport formula
    character(256)   , dimension(:)  , pointer :: dll_name            !< Name of DLL that calculates the Sediment transport formula
    integer(pntrsize), dimension(:)  , pointer :: dll_handle          !< DLL containing Sediment transport formula
    integer          , dimension(:)  , pointer :: dll_integers        !< Input integer array to shared library
    real(hp)         , dimension(:)  , pointer :: dll_reals           !< Input real array to shared library
    character(256)   , dimension(:)  , pointer :: dll_strings         !< Input character string array to shared library
    character(256)   , dimension(:)  , pointer :: dll_usrfil          !< Name of input file to be passed to subroutine in DLL
    character(256)   , dimension(:)  , pointer :: flstrn              !< Sediment transport formula file names
    integer          , dimension(:)  , pointer :: iform               !< Sediment transport formula number
    character(256)   , dimension(:)  , pointer :: name                !< Sediment transport formula names
    real(fp)         , dimension(:,:), pointer :: par                 !< Sediment transport formula parameter values
    character(25)    , dimension(:,:), pointer :: parname             !< Sediment transport formula parameter names
    character(256)   , dimension(:,:), pointer :: parfilename         !< Sediment transport formula file names
    integer          , dimension(:,:), pointer :: iparfile            !< Index of parameter in parfile array (0 if parameter is a constant)
    type(parfile_type), dimension(:)  , pointer :: parfile(:)         !< Sediment transport parameter read from file - time- or space-varying data
    ! 
    ! logicals
    !
    !
    ! characters
end type trapar_type

type sedtra_type
    integer          , dimension(:)      , pointer :: kfsed    !(nc1:nc2)
    integer          , dimension(:,:)    , pointer :: kmxsed   !(nc1:nc2,lsed)
    !
    real(fp)         , dimension(:)      , pointer :: bc_mor_array !(lsedtot*2)
    !
    real(fp)         , dimension(:)      , pointer :: dcwwlc   !(0:num_layers_grid)
    real(fp)         , dimension(:)      , pointer :: epsclc   !(0:num_layers_grid)
    real(fp)         , dimension(:)      , pointer :: epswlc   !(0:num_layers_grid)
    real(fp)         , dimension(:)      , pointer :: rsdqlc   !(1:num_layers_grid)
    real(fp)         , dimension(:)      , pointer :: sddflc   !(0:num_layers_grid)
    real(fp)         , dimension(:)      , pointer :: wslc     !(0:num_layers_grid)
    !
    real(fp)         , dimension(:)      , pointer :: e_dzdn   !(nu1:nu2)         dzduu in structured Delft3D-FLOW
    real(fp)         , dimension(:)      , pointer :: e_dzdt   !(nu1:nu2)         dzdvv in structured Delft3D-FLOW
    !
    real(fp)         , dimension(:,:)    , pointer :: e_sbcn   !(nu1:nu2,lsedtot) sbcuu in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: e_sbct   !(nu1:nu2,lsedtot) sbcvv in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: e_sbwn   !(nu1:nu2,lsedtot) sbwuu in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: e_sbwt   !(nu1:nu2,lsedtot) sbwvv in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: e_sswn   !(nu1:nu2,lsedtot) sswuu in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: e_sswt   !(nu1:nu2,lsedtot) sswvv in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: e_scrn   !(nu1:nu2,lsedtot) sucor in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: e_scrt   !(nu1:nu2,lsedtot) svcor in structured Delft3D-FLOW
    !
    real(fp)         , dimension(:,:)    , pointer :: e_sbn    !(nu1:nu2,lsed)    equivalent sbuu allocated via esm/fsm in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: e_sbt    !(nu1:nu2,lsed)    equivalent sbvv allocated via esm/fsm in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: e_sbnc   !(nu1:nu2,lsedtot) sbuuc in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: e_sbtc   !(nu1:nu2,lsedtot) sbvvc in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: e_ssn    !(nu1:nu2,lsed)    ssuu  in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: e_sst    !(nu1:nu2,lsed)    ssvv  in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: e_ssnc   !(nu1:nu2,lsed)    ssuuc in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: e_sstc   !(nu1:nu2,lsed)    ssvvc in structured Delft3D-FLOW
    !
    real(fp)         , dimension(:,:)    , pointer :: frac     !< (nc1:nc2,lsedtot) effective fraction of sediment in bed available for transport
    real(fp)         , dimension(:)      , pointer :: mudfrac  !< (nc1:nc2)         effective mud fraction in the part of the bed exposed to transport
    real(fp)         , dimension(:)      , pointer :: sandfrac !< (nc1:nc2)         effective sand fraction in the part of the bed exposed to transport (mud excluded)
    real(fp)         , dimension(:)      , pointer :: dm       !< (nc1:nc2)         arithmetic mean sediment diameter of the part of the bed exposed to transport (mud excluded)
    real(fp)         , dimension(:)      , pointer :: dg       !< (nc1:nc2)         geometric mean sediment diameter of the part of the bed exposed to transport (mud excluded)
    real(fp)         , dimension(:)      , pointer :: dgsd     !< (nc1:nc2)         geometric standard deviation of particle size mix of the part of the bed exposed to transport (mud excluded)
    real(fp)         , dimension(:,:)    , pointer :: dxx      !< (nc1:nc2,nxx)     sediment diameter corresponding to percentile xx (mud excluded)
    real(fp)         , dimension(:,:)    , pointer :: hidexp   !< (nc1:nc2,lsedtot) hiding-exposure factor correcting the shear stress (sand-gravel mixtures)
    !
    real(fp)         , dimension(:)      , pointer :: uuu      !(nc1:nc2)
    real(fp)         , dimension(:)      , pointer :: vvv      !(nc1:nc2)
    real(fp)         , dimension(:)      , pointer :: umod     !(nc1:nc2)
    real(fp)         , dimension(:)      , pointer :: zumod    !(nc1:nc2)
    real(fp)         , dimension(:)      , pointer :: ust2     !(nc1:nc2)
    !
    real(fp)         , dimension(:,:)    , pointer :: aks      !(nc1:nc2,lsed)
    real(fp)         , dimension(:,:)    , pointer :: rca      !(nc1:nc2,lsed)
    real(fp)         , dimension(:,:)    , pointer :: rsedeq   !(nc1:nc2,lsed)
    real(fp)         , dimension(:,:)    , pointer :: sinkse   !(nc1:nc2,lsed)
    real(fp)         , dimension(:,:)    , pointer :: sourse   !(nc1:nc2,lsed)
    real(fp)         , dimension(:,:)    , pointer :: sour_im  !(nc1:nc2,lsed)
    !
    real(fp)         , dimension(:,:)    , pointer :: dbodsd   !(lsedtot,nc1:nc2)
    !
    real(fp)         , dimension(:,:)    , pointer :: sbcx     !(nc1:nc2,lsedtot) sbcu in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: sbcy     !(nc1:nc2,lsedtot) sbcv in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: sbwx     !(nc1:nc2,lsedtot) sbwu in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: sbwy     !(nc1:nc2,lsedtot) sbwv in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: sswx     !(nc1:nc2,lsedtot) sswu in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: sswy     !(nc1:nc2,lsedtot) sswv in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: sxtot    !(nc1:nc2,lsedtot) sutot in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: sytot    !(nc1:nc2,lsedtot) svtot in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: sscx     !(nc1:nc2,lsedtot) svtot in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: sscy     !(nc1:nc2,lsedtot) svtot in structured Delft3D-FLOW
    real(fp)         , dimension(:,:)    , pointer :: sbxcum   !(nc1:nc2,lsedtot) Cumulative transports in FM in zeta
    real(fp)         , dimension(:,:)    , pointer :: sbycum   !(nc1:nc2,lsedtot) Cumulative transports in FM in zeta
    real(fp)         , dimension(:,:)    , pointer :: ssxcum   !(nc1:nc2,lsedtot) Cumulative transports in FM in zeta
    real(fp)         , dimension(:,:)    , pointer :: ssycum   !(nc1:nc2,lsedtot) Cumulative transports in FM in zeta    
    !
    real(fp)         , dimension(:,:)    , pointer :: srcmax   !(nc1:nc2,lsedtot)
    real(fp)         , dimension(:,:)    , pointer :: fixfac   !(nc1:nc2,lsedtot)
    real(fp)         , dimension(:)      , pointer :: taub     !(nc1:nc2)
    real(fp)         , dimension(:,:)    , pointer :: taurat   !(nc1:nc2,lsedtot)
    !
    real(fp)         , dimension(:,:)    , pointer :: statqnt  !(nc1:nc2,nstatistics)
end type sedtra_type

    contains

!> Nullify/initialize a sedtra_type data structure.
subroutine nullsedtra(sedtra)
!!--declarations----------------------------------------------------------------
    !
    ! Function/routine arguments
    !
    type (sedtra_type)                                       :: sedtra
    !
    ! Local variables
    !
    ! None
!
!! executable statements -------------------------------------------------------
!
    nullify(sedtra%kfsed)
    nullify(sedtra%kmxsed)
    !
    nullify(sedtra%bc_mor_array)
    !
    nullify(sedtra%dcwwlc)
    nullify(sedtra%epsclc)
    nullify(sedtra%epswlc)
    nullify(sedtra%rsdqlc)
    nullify(sedtra%sddflc)
    nullify(sedtra%wslc)
    !
    nullify(sedtra%e_dzdn)
    nullify(sedtra%e_dzdt)
    !
    nullify(sedtra%e_sbcn)
    nullify(sedtra%e_sbct)
    nullify(sedtra%e_sbwn)
    nullify(sedtra%e_sbwt)
    nullify(sedtra%e_sswn)
    nullify(sedtra%e_sswt)
    nullify(sedtra%e_scrn)
    nullify(sedtra%e_scrt)
    !
    nullify(sedtra%e_sbn)
    nullify(sedtra%e_sbt)
    nullify(sedtra%e_sbnc)
    nullify(sedtra%e_sbtc)
    nullify(sedtra%e_ssn)
    nullify(sedtra%e_sst)
    nullify(sedtra%e_ssnc)
    nullify(sedtra%e_sstc)
    !
    nullify(sedtra%frac)
    nullify(sedtra%mudfrac)
    nullify(sedtra%sandfrac)
    nullify(sedtra%dm)
    nullify(sedtra%dg)
    nullify(sedtra%dgsd)
    nullify(sedtra%dxx)
    nullify(sedtra%hidexp)
    !
    nullify(sedtra%uuu)
    nullify(sedtra%vvv)
    nullify(sedtra%umod)
    nullify(sedtra%zumod)
    nullify(sedtra%ust2)
    !
    nullify(sedtra%aks)
    nullify(sedtra%rca)
    nullify(sedtra%rsedeq)
    nullify(sedtra%sinkse)
    nullify(sedtra%sourse)
    nullify(sedtra%sour_im)
    !
    nullify(sedtra%dbodsd)
    !
    nullify(sedtra%sbcx)
    nullify(sedtra%sbcy)
    nullify(sedtra%sbwx)
    nullify(sedtra%sbwy)
    nullify(sedtra%sswx)
    nullify(sedtra%sswy)
    nullify(sedtra%sxtot)
    nullify(sedtra%sytot)
    nullify(sedtra%sscx)
    nullify(sedtra%sscy)
    nullify(sedtra%sbxcum)
    nullify(sedtra%sbycum)
    nullify(sedtra%ssxcum)
    nullify(sedtra%ssycum)    
    !
    nullify(sedtra%srcmax)
    nullify(sedtra%fixfac)
    nullify(sedtra%taub)
    nullify(sedtra%taurat)
    !
    nullify(sedtra%statqnt)
end subroutine nullsedtra


!> Allocate the arrays of sedtra_type data structure.
subroutine allocsedtra(sedtra, moroutput, num_layers_grid, lsed, lsedtot, nc1, nc2, nu1, nu2, nxx, nstatqnt, iopt)
!!--declarations----------------------------------------------------------------
    !
    ! Function/routine arguments
    !
    type (sedtra_type)                                       :: sedtra
    type (moroutputtype)                                     :: moroutput
    integer                                    , intent(in)  :: num_layers_grid
    integer                                    , intent(in)  :: lsed
    integer                                    , intent(in)  :: lsedtot
    integer                                    , intent(in)  :: nc1
    integer                                    , intent(in)  :: nc2
    integer                                    , intent(in)  :: nu1
    integer                                    , intent(in)  :: nu2
    integer                                    , intent(in)  :: nxx
    integer                                    , intent(in)  :: nstatqnt
    integer                         , optional , intent(in)  :: iopt
    !
    ! Local variables
    !
    integer                                                  :: iq
    integer                                                  :: istat
    integer                                                  :: ioptloc
!
!! executable statements -------------------------------------------------------
!
    ioptloc = CODE_DEFAULT
    if (present(iopt)) ioptloc=iopt
    !
                  allocate(sedtra%kfsed   (nc1:nc2)     , STAT = istat)
    if (istat==0) allocate(sedtra%kmxsed  (nc1:nc2,lsed), STAT = istat)
    !
    if (istat==0) allocate(sedtra%bc_mor_array (lsedtot*2), STAT = istat)
    !
    if (istat==0) allocate(sedtra%dcwwlc  (0:num_layers_grid), STAT = istat)
    if (istat==0) allocate(sedtra%epsclc  (0:num_layers_grid), STAT = istat)
    if (istat==0) allocate(sedtra%epswlc  (0:num_layers_grid), STAT = istat)
    if (istat==0) allocate(sedtra%rsdqlc  (1:num_layers_grid), STAT = istat)
    if (istat==0) allocate(sedtra%sddflc  (0:num_layers_grid), STAT = istat)
    if (istat==0) allocate(sedtra%wslc    (0:num_layers_grid), STAT = istat)
    !
    if (istat==0) allocate(sedtra%e_dzdn  (nu1:nu2), STAT = istat)
    if (istat==0) allocate(sedtra%e_dzdt  (nu1:nu2), STAT = istat)
    !
    if (istat==0) allocate(sedtra%e_sbcn  (nu1:nu2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%e_sbct  (nu1:nu2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%e_sbwn  (nu1:nu2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%e_sbwt  (nu1:nu2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%e_sswn  (nu1:nu2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%e_sswt  (nu1:nu2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%e_scrn  (nu1:nu2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%e_scrt  (nu1:nu2,lsedtot), STAT = istat)
    !
    if (ioptloc==CODE_DEFAULT) then
       if (istat==0) allocate(sedtra%e_sbn   (nu1:nu2,lsedtot), STAT = istat)
       if (istat==0) allocate(sedtra%e_sbt   (nu1:nu2,lsedtot), STAT = istat)
    else
       if (istat==0) allocate(sedtra%e_sbn   (1,1), STAT = istat) ! not used in structured Delft3D-FLOW
       if (istat==0) allocate(sedtra%e_sbt   (1,1), STAT = istat) ! not used in structured Delft3D-FLOW
    endif
    if (istat==0) allocate(sedtra%e_sbnc  (nu1:nu2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%e_sbtc  (nu1:nu2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%e_ssn   (nu1:nu2,lsed), STAT = istat)
    if (istat==0) allocate(sedtra%e_sst   (nu1:nu2,lsed), STAT = istat)
    if (istat==0) allocate(sedtra%e_ssnc  (nu1:nu2,lsed), STAT = istat)
    if (istat==0) allocate(sedtra%e_sstc  (nu1:nu2,lsed), STAT = istat)
    !
    if (istat==0) allocate(sedtra%frac    (nc1:nc2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%mudfrac (nc1:nc2), STAT = istat)
    if (istat==0) allocate(sedtra%sandfrac(nc1:nc2), STAT = istat)
    if (istat==0) allocate(sedtra%dm      (nc1:nc2), STAT = istat)
    if (istat==0) allocate(sedtra%dg      (nc1:nc2), STAT = istat)
    if (istat==0) allocate(sedtra%dgsd    (nc1:nc2), STAT = istat)
    if (istat==0) allocate(sedtra%dxx     (nc1:nc2,nxx), STAT = istat)
    if (istat==0) allocate(sedtra%hidexp  (nc1:nc2,lsedtot), STAT = istat)
    !
    if (istat==0) allocate(sedtra%uuu     (nc1:nc2), STAT = istat)
    if (istat==0) allocate(sedtra%vvv     (nc1:nc2), STAT = istat)
    if (istat==0) allocate(sedtra%umod    (nc1:nc2), STAT = istat)
    if (istat==0) allocate(sedtra%zumod   (nc1:nc2), STAT = istat)
    if (istat==0) allocate(sedtra%ust2    (nc1:nc2), STAT = istat)
    !
    if (istat==0) allocate(sedtra%aks     (nc1:nc2,lsed), STAT = istat)
    if (istat==0) allocate(sedtra%rca     (nc1:nc2,lsed), STAT = istat)
    if (istat==0) allocate(sedtra%rsedeq  (nc1:nc2,lsed), STAT = istat)
    if (istat==0) allocate(sedtra%sinkse  (nc1:nc2,lsed), STAT = istat)
    if (istat==0) allocate(sedtra%sourse  (nc1:nc2,lsed), STAT = istat)
    if (istat==0) allocate(sedtra%sour_im (nc1:nc2,lsed), STAT = istat)
    !
    if (istat==0) allocate(sedtra%dbodsd  (lsedtot,nc1:nc2), STAT = istat)
    !
    if (istat==0) allocate(sedtra%sbcx    (nc1:nc2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%sbcy    (nc1:nc2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%sbwx    (nc1:nc2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%sbwy    (nc1:nc2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%sswx    (nc1:nc2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%sswy    (nc1:nc2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%sxtot   (nc1:nc2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%sytot   (nc1:nc2,lsedtot), STAT = istat)
    if (ioptloc==CODE_DEFAULT) then
       if (istat==0) allocate(sedtra%sscx   (nc1:nc2,lsedtot), STAT = istat)  ! to have ss output in FM in zeta points
       if (istat==0) allocate(sedtra%sscy   (nc1:nc2,lsedtot), STAT = istat)  ! dimensioned on sedtot on purpose, see reconstructsedtransports()
       if (istat==0) allocate(sedtra%sbxcum (nc1:nc2,lsedtot), STAT = istat)  ! Cumulative transports in FM in zeta points
       if (istat==0) allocate(sedtra%sbycum (nc1:nc2,lsedtot), STAT = istat)  
       if (istat==0) allocate(sedtra%ssxcum (nc1:nc2,lsedtot), STAT = istat)  
       if (istat==0) allocate(sedtra%ssycum (nc1:nc2,lsedtot), STAT = istat)         
    else
       if (istat==0) allocate(sedtra%sscx   (1,1), STAT = istat)           ! not used in structured Delft3D-FLOW
       if (istat==0) allocate(sedtra%sscy   (1,1), STAT = istat)           ! not used in structured Delft3D-FLOW
       if (istat==0) allocate(sedtra%sbxcum (1,1), STAT = istat)  ! Cumulative transports in FM, compare to e_sstc
       if (istat==0) allocate(sedtra%sbycum (1,1), STAT = istat)  
       if (istat==0) allocate(sedtra%ssxcum (1,1), STAT = istat)  
       if (istat==0) allocate(sedtra%ssycum (1,1), STAT = istat)       
    endif
    !
    if (istat==0) allocate(sedtra%srcmax  (nc1:nc2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%fixfac  (nc1:nc2,lsedtot), STAT = istat)
    if (istat==0) allocate(sedtra%taub    (nc1:nc2), STAT = istat)
    if (istat==0) allocate(sedtra%taurat  (nc1:nc2,lsedtot), STAT = istat)
    !
    if (istat==0) allocate(sedtra%statqnt (nc1:nc2,nstatqnt), STAT = istat)
    !
    sedtra%kfsed    = 0
    sedtra%kmxsed   = 0
    !
    sedtra%bc_mor_array = 0.0_fp
    !
    sedtra%dcwwlc   = 0.0_fp
    sedtra%epsclc   = 0.0_fp
    sedtra%epswlc   = 0.0_fp
    sedtra%rsdqlc   = 0.0_fp
    sedtra%sddflc   = 0.0_fp
    sedtra%wslc     = 0.0_fp
    !
    sedtra%e_dzdn   = 0.0_fp
    sedtra%e_dzdt   = 0.0_fp
    !
    sedtra%e_sbcn   = 0.0_fp
    sedtra%e_sbct   = 0.0_fp
    sedtra%e_sbwn   = 0.0_fp
    sedtra%e_sbwt   = 0.0_fp
    sedtra%e_sswn   = 0.0_fp
    sedtra%e_sswt   = 0.0_fp
    sedtra%e_scrn   = 0.0_fp
    sedtra%e_scrt   = 0.0_fp
    !
    sedtra%e_sbn    = 0.0_fp
    sedtra%e_sbt    = 0.0_fp
    sedtra%e_sbnc   = 0.0_fp
    sedtra%e_sbtc   = 0.0_fp
    sedtra%e_ssn    = 0.0_fp
    sedtra%e_sst    = 0.0_fp
    sedtra%e_ssnc   = 0.0_fp
    sedtra%e_sstc   = 0.0_fp
    !
    sedtra%frac     = 0.0_fp
    sedtra%mudfrac  = 0.0_fp
    sedtra%sandfrac = 0.0_fp
    sedtra%dm       = 0.0_fp
    sedtra%dg       = 0.0_fp
    sedtra%dgsd     = 0.0_fp
    sedtra%dxx      = 0.0_fp
    sedtra%hidexp   = 1.0_fp
    !
    sedtra%ust2     = 0.0_fp
    sedtra%uuu      = 0.0_fp
    sedtra%vvv      = 0.0_fp
    sedtra%umod     = 0.0_fp
    sedtra%zumod    = 0.0_fp
    !
    sedtra%aks      = 0.0_fp
    sedtra%rca      = 0.0_fp
    sedtra%rsedeq   = 0.0_fp
    sedtra%sinkse   = 0.0_fp
    sedtra%sourse   = 0.0_fp
    sedtra%sour_im  = 0.0_fp
    !
    sedtra%dbodsd   = 0.0_fp
    !
    sedtra%sbcx     = 0.0_fp
    sedtra%sbcy     = 0.0_fp
    sedtra%sbwx     = 0.0_fp
    sedtra%sbwy     = 0.0_fp
    sedtra%sswx     = 0.0_fp
    sedtra%sswy     = 0.0_fp
    sedtra%sxtot    = 0.0_fp
    sedtra%sytot    = 0.0_fp
    sedtra%sscx     = 0.0_fp
    sedtra%sscy     = 0.0_fp
    sedtra%sbxcum   = 0.0_fp
    sedtra%sbycum   = 0.0_fp
    sedtra%ssxcum   = 0.0_fp
    sedtra%ssycum   = 0.0_fp    
    !
    sedtra%srcmax   = 0.0_fp
    sedtra%fixfac   = 1.0_fp
    sedtra%taub     = 0.0_fp
    sedtra%taurat   = 0.0_fp
    !
    sedtra%statqnt  = 0.0_fp
    do iq = 1,4
        ! min
        if (moroutput%statflg(2,iq)>0) then
            sedtra%statqnt(:,moroutput%statflg(2,iq)) = 1e10
        endif
        ! max
        if (moroutput%statflg(3,iq)>0) then
            sedtra%statqnt(:,moroutput%statflg(3,iq)) = -1e10
        endif
    enddo
end subroutine allocsedtra


!> Clear the arrays of sedtra_type data structure.
subroutine clrsedtra(istat, sedtra)
!!--declarations----------------------------------------------------------------
    !
    ! Function/routine arguments
    !
    type (sedtra_type)                         , intent(inout) :: sedtra
    integer                                    , intent(out)   :: istat
    !
    ! Local variables
    !
    ! NONE
!
!! executable statements -------------------------------------------------------
!
    if (associated(sedtra%kfsed   ))   deallocate(sedtra%kfsed   , STAT = istat)
    if (associated(sedtra%kmxsed  ))   deallocate(sedtra%kmxsed  , STAT = istat)
    !
    if (associated(sedtra%bc_mor_array))   deallocate(sedtra%bc_mor_array, STAT = istat)
    !
    if (associated(sedtra%dcwwlc  ))   deallocate(sedtra%dcwwlc  , STAT = istat)
    if (associated(sedtra%epsclc  ))   deallocate(sedtra%epsclc  , STAT = istat)
    if (associated(sedtra%epswlc  ))   deallocate(sedtra%epswlc  , STAT = istat)
    if (associated(sedtra%rsdqlc  ))   deallocate(sedtra%rsdqlc  , STAT = istat)
    if (associated(sedtra%sddflc  ))   deallocate(sedtra%sddflc  , STAT = istat)
    if (associated(sedtra%wslc    ))   deallocate(sedtra%wslc    , STAT = istat)
    !
    if (associated(sedtra%e_dzdn  ))   deallocate(sedtra%e_dzdn  , STAT = istat)
    if (associated(sedtra%e_dzdt  ))   deallocate(sedtra%e_dzdt  , STAT = istat)
    !
    if (associated(sedtra%e_sbcn  ))   deallocate(sedtra%e_sbcn  , STAT = istat)
    if (associated(sedtra%e_sbct  ))   deallocate(sedtra%e_sbct  , STAT = istat)
    if (associated(sedtra%e_sbwn  ))   deallocate(sedtra%e_sbwn  , STAT = istat)
    if (associated(sedtra%e_sbwt  ))   deallocate(sedtra%e_sbwt  , STAT = istat)
    if (associated(sedtra%e_sswn  ))   deallocate(sedtra%e_sswn  , STAT = istat)
    if (associated(sedtra%e_sswt  ))   deallocate(sedtra%e_sswt  , STAT = istat)
    if (associated(sedtra%e_scrn  ))   deallocate(sedtra%e_scrn  , STAT = istat)
    if (associated(sedtra%e_scrt  ))   deallocate(sedtra%e_scrt  , STAT = istat)
    !
    if (associated(sedtra%e_sbn   ))   deallocate(sedtra%e_sbn   , STAT = istat)
    if (associated(sedtra%e_sbt   ))   deallocate(sedtra%e_sbt   , STAT = istat)
    if (associated(sedtra%e_sbnc  ))   deallocate(sedtra%e_sbnc  , STAT = istat)
    if (associated(sedtra%e_sbtc  ))   deallocate(sedtra%e_sbtc  , STAT = istat)
    if (associated(sedtra%e_ssn   ))   deallocate(sedtra%e_ssn   , STAT = istat)
    if (associated(sedtra%e_sst   ))   deallocate(sedtra%e_sst   , STAT = istat)
    if (associated(sedtra%e_ssnc  ))   deallocate(sedtra%e_ssnc  , STAT = istat)
    if (associated(sedtra%e_sstc  ))   deallocate(sedtra%e_sstc  , STAT = istat)
    !
    if (associated(sedtra%frac    ))   deallocate(sedtra%frac    , STAT = istat)
    if (associated(sedtra%mudfrac ))   deallocate(sedtra%mudfrac , STAT = istat)
    if (associated(sedtra%sandfrac))   deallocate(sedtra%sandfrac, STAT = istat)
    if (associated(sedtra%dm      ))   deallocate(sedtra%dm      , STAT = istat)
    if (associated(sedtra%dg      ))   deallocate(sedtra%dg      , STAT = istat)
    if (associated(sedtra%dgsd    ))   deallocate(sedtra%dgsd    , STAT = istat)
    if (associated(sedtra%dxx     ))   deallocate(sedtra%dxx     , STAT = istat)
    if (associated(sedtra%hidexp  ))   deallocate(sedtra%hidexp  , STAT = istat)
    !
    if (associated(sedtra%uuu     ))   deallocate(sedtra%uuu     , STAT = istat)
    if (associated(sedtra%vvv     ))   deallocate(sedtra%vvv     , STAT = istat)
    if (associated(sedtra%umod    ))   deallocate(sedtra%umod    , STAT = istat)
    if (associated(sedtra%zumod   ))   deallocate(sedtra%zumod   , STAT = istat)
    if (associated(sedtra%ust2    ))   deallocate(sedtra%ust2    , STAT = istat)
    !
    if (associated(sedtra%aks     ))   deallocate(sedtra%aks     , STAT = istat)
    if (associated(sedtra%rca     ))   deallocate(sedtra%rca     , STAT = istat)
    if (associated(sedtra%rsedeq  ))   deallocate(sedtra%rsedeq  , STAT = istat)
    if (associated(sedtra%sinkse  ))   deallocate(sedtra%sinkse  , STAT = istat)
    if (associated(sedtra%sourse  ))   deallocate(sedtra%sourse  , STAT = istat)
    if (associated(sedtra%sour_im ))   deallocate(sedtra%sour_im , STAT = istat)
    !
    if (associated(sedtra%dbodsd  ))   deallocate(sedtra%dbodsd  , STAT = istat)
    !
    if (associated(sedtra%sbcx    ))   deallocate(sedtra%sbcx    , STAT = istat)
    if (associated(sedtra%sbcy    ))   deallocate(sedtra%sbcy    , STAT = istat)
    if (associated(sedtra%sbwx    ))   deallocate(sedtra%sbwx    , STAT = istat)
    if (associated(sedtra%sbwy    ))   deallocate(sedtra%sbwy    , STAT = istat)
    if (associated(sedtra%sswx    ))   deallocate(sedtra%sswx    , STAT = istat)
    if (associated(sedtra%sswy    ))   deallocate(sedtra%sswy    , STAT = istat)
    if (associated(sedtra%sxtot   ))   deallocate(sedtra%sxtot   , STAT = istat)
    if (associated(sedtra%sytot   ))   deallocate(sedtra%sytot   , STAT = istat)
    if (associated(sedtra%sscx    ))   deallocate(sedtra%sscx    , STAT = istat)
    if (associated(sedtra%sscy    ))   deallocate(sedtra%sscy    , STAT = istat)
    if (associated(sedtra%sbxcum  ))   deallocate(sedtra%sbxcum  , STAT = istat)
    if (associated(sedtra%sbycum  ))   deallocate(sedtra%sbycum  , STAT = istat)
    if (associated(sedtra%ssxcum  ))   deallocate(sedtra%ssxcum  , STAT = istat)
    if (associated(sedtra%ssycum  ))   deallocate(sedtra%ssycum  , STAT = istat)    
    !
    if (associated(sedtra%srcmax  ))   deallocate(sedtra%srcmax  , STAT = istat)
    if (associated(sedtra%fixfac  ))   deallocate(sedtra%fixfac  , STAT = istat)
    if (associated(sedtra%taub    ))   deallocate(sedtra%taub    , STAT = istat)
    if (associated(sedtra%taurat  ))   deallocate(sedtra%taurat  , STAT = istat)
    !
    if (associated(sedtra%statqnt ))   deallocate(sedtra%statqnt , STAT = istat)
end subroutine clrsedtra


!> Nullify/initialize a sedpar_type data structure.
subroutine nullsedpar(sedpar)
!!--declarations----------------------------------------------------------------
    !
    ! Function/routine arguments
    !
    type (sedpar_type)                                     :: sedpar
    !
    ! Local variables
    !
    ! None
!
!! executable statements -------------------------------------------------------
!
    sedpar%csoil    = 1.0e4_fp
    sedpar%mdcuni   = 0.0_fp
    sedpar%kssilt   = 0.0_fp
    sedpar%kssand   = 0.0_fp
    sedpar%sc_cmf1  = 0.01_fp
    sedpar%sc_cmf2  = 0.01_fp
    sedpar%sc_flcf  = 0.5_fp
    sedpar%kssand   = 0.0_fp
    sedpar%version  = 2.0_fp
    sedpar%tbreakup = 1e-10_fp
    sedpar%tfloc    = 1e-10_fp
    sedpar%d_micro  = 1e-4_fp
    sedpar%ustar_macro = 0.067_fp
    !
    sedpar%flocmod        = FLOC_NONE
    sedpar%nflocpop       = 1
    sedpar%nflocsizes     = 1
    sedpar%nmudfrac       = 0
    sedpar%sc_mudfac      = SC_MUDTHC
    sedpar%max_mud_sedtyp = SEDTYP_SILT
    sedpar%min_dxx_sedtyp = SEDTYP_SAND
    !
    sedpar%anymud    = .false.
    sedpar%bsskin    = .false.
    !
    sedpar%flsdia   = ' '
    sedpar%flsmdc   = ' '
    sedpar%flspmc   = ' '
    !
    nullify(sedpar%sedblock)
    !
    nullify(sedpar%cmpupdfrac)
    nullify(sedpar%tpsnumber)
    nullify(sedpar%rhosol)
    !
    nullify(sedpar%logseddia)
    nullify(sedpar%logsedsig)
    nullify(sedpar%sedd10)
    nullify(sedpar%sedd50)
    nullify(sedpar%sedd50fld)
    nullify(sedpar%sedd90)
    !
    nullify(sedpar%cdryb)
    nullify(sedpar%dstar)
    nullify(sedpar%taucr)
    nullify(sedpar%tetacr)
    !
    nullify(sedpar%dss)
    nullify(sedpar%facdss)
    nullify(sedpar%sdbuni)
    nullify(sedpar%tcguni)
    nullify(sedpar%mudcnt)
    nullify(sedpar%pmcrit)
    nullify(sedpar%sedtrcfac)
    !
    nullify(sedpar%nseddia)
    nullify(sedpar%sedtyp)
    nullify(sedpar%tratyp)
    !
    nullify(sedpar%namclay)
    nullify(sedpar%namflocpop)
    nullify(sedpar%flocsize)
    nullify(sedpar%floclist)
    !
    nullify(sedpar%inisedunit)
    nullify(sedpar%namsed)
    nullify(sedpar%flsdbd)
    nullify(sedpar%flstcg)
end subroutine nullsedpar


!> Clean up a sedpar_type data structure.
subroutine clrsedpar(istat     ,sedpar  )
!!--declarations----------------------------------------------------------------
    !
    ! Function/routine arguments
    !
    type (sedpar_type)                       , pointer     :: sedpar
    integer                                  , intent(out) :: istat
!
!! executable statements -------------------------------------------------------
!
    if (associated(sedpar%sedblock))   deallocate(sedpar%sedblock,   STAT = istat) ! the actual data tree should be deleted as part of the whole sed_ptr tree.
    !
    if (associated(sedpar%cmpupdfrac)) deallocate(sedpar%cmpupdfrac, STAT = istat)
    if (associated(sedpar%tpsnumber))  deallocate(sedpar%tpsnumber,  STAT = istat)
    if (associated(sedpar%rhosol))     deallocate(sedpar%rhosol,     STAT = istat)
    !
    if (associated(sedpar%logseddia))  deallocate(sedpar%logseddia,  STAT = istat)
    if (associated(sedpar%logsedsig))  deallocate(sedpar%logsedsig,  STAT = istat)
    if (associated(sedpar%sedd10))     deallocate(sedpar%sedd10,     STAT = istat)
    if (associated(sedpar%sedd50))     deallocate(sedpar%sedd50,     STAT = istat)
    if (associated(sedpar%sedd50fld))  deallocate(sedpar%sedd50fld,  STAT = istat)
    if (associated(sedpar%sedd90))     deallocate(sedpar%sedd90,     STAT = istat)
    !
    if (associated(sedpar%cdryb))      deallocate(sedpar%cdryb,      STAT = istat)
    if (associated(sedpar%dstar))      deallocate(sedpar%dstar,      STAT = istat)
    if (associated(sedpar%taucr))      deallocate(sedpar%taucr,      STAT = istat)
    if (associated(sedpar%tetacr))     deallocate(sedpar%tetacr,     STAT = istat)
    !
    if (associated(sedpar%dss))        deallocate(sedpar%dss,        STAT = istat)
    if (associated(sedpar%facdss))     deallocate(sedpar%facdss,     STAT = istat)
    if (associated(sedpar%sdbuni))     deallocate(sedpar%sdbuni,     STAT = istat)
    if (associated(sedpar%tcguni))     deallocate(sedpar%tcguni,     STAT = istat)
    if (associated(sedpar%mudcnt))     deallocate(sedpar%mudcnt,     STAT = istat)
    if (associated(sedpar%pmcrit))     deallocate(sedpar%pmcrit,     STAT = istat)
    !
    if (associated(sedpar%nseddia))    deallocate(sedpar%nseddia,    STAT = istat)
    if (associated(sedpar%sedtyp))     deallocate(sedpar%sedtyp,     STAT = istat)
    if (associated(sedpar%tratyp))     deallocate(sedpar%tratyp,     STAT = istat)
    !
    if (associated(sedpar%namclay))    deallocate(sedpar%namclay,    STAT = istat)
    if (associated(sedpar%namflocpop)) deallocate(sedpar%namflocpop, STAT = istat)
    if (associated(sedpar%flocsize))   deallocate(sedpar%flocsize,   STAT = istat)
    if (associated(sedpar%floclist))   deallocate(sedpar%floclist,   STAT = istat)
    !
    if (associated(sedpar%inisedunit)) deallocate(sedpar%inisedunit, STAT = istat)
    if (associated(sedpar%namsed))     deallocate(sedpar%namsed,     STAT = istat)
    if (associated(sedpar%flsdbd))     deallocate(sedpar%flsdbd,     STAT = istat)
    if (associated(sedpar%flstcg))     deallocate(sedpar%flstcg,     STAT = istat)
end subroutine clrsedpar


!> Nullify/initialize a morpar_type data structure.
subroutine nullmorpar(morpar)
!!--declarations----------------------------------------------------------------
    !
    ! Function/routine arguments
    !
    type (morpar_type)                   , target  :: morpar
    !
    ! Local variables
    !
    integer                              , pointer :: ihidexp
    integer                              , pointer :: iti_sedtrans
    integer                              , pointer :: itmor
    integer                              , pointer :: itcmp
    integer                              , pointer :: iopkcw
    integer                              , pointer :: iopsus
    integer                              , pointer :: islope
    integer                              , pointer :: morfacpar
    integer                              , pointer :: morfacrec
    integer                              , pointer :: morfactable
    integer                              , pointer :: nxx
    integer                              , pointer :: subiw
    integer                              , pointer :: ttlform
    integer                              , pointer :: telform
    real(hp)                             , pointer :: hydrt
    real(hp)                             , pointer :: hydrt0
    real(hp)                             , pointer :: morft
    real(hp)                             , pointer :: morft0
    real(fp)                             , pointer :: morfac
    real(fp)                             , pointer :: thresh
    real(fp)                             , pointer :: aksfac
    real(fp)                             , pointer :: rwave
    real(fp)                             , pointer :: alfabs
    real(fp)                             , pointer :: alfabn
    real(fp)                             , pointer :: camax
    real(fp)                             , pointer :: dzmax
    real(fp)                             , pointer :: sus
    real(fp)                             , pointer :: suscorfac
    real(fp)                             , pointer :: bed
    real(fp)                             , pointer :: ti_sedtrans
    real(fp)                             , pointer :: tmor
    real(fp)                             , pointer :: tcmp
    real(fp)              , dimension(:) , pointer :: thetsd
    real(fp)                             , pointer :: thetsduni
    real(fp)                             , pointer :: susw
    real(fp)                             , pointer :: sedthr
    real(fp)                             , pointer :: hmaxth
    real(fp)                             , pointer :: bedw
    real(fp)                             , pointer :: factsd
    real(fp)                             , pointer :: rdc
    real(fp)                             , pointer :: rdw
    real(fp)                             , pointer :: espir
    real(fp)                             , pointer :: ashld
    real(fp)                             , pointer :: bshld
    real(fp)                             , pointer :: cshld
    real(fp)                             , pointer :: dshld
    real(fp)                             , pointer :: coulfri
    real(fp)                             , pointer :: flfdrat
    real(fp)                             , pointer :: alfpa
    real(fp)                             , pointer :: thcrpa
    real(fp)                             , pointer :: asklhe
    real(fp)                             , pointer :: mwwjhe
    real(fp)                             , pointer :: pangle
    real(fp)                             , pointer :: fpco
    real(fp)                             , pointer :: factcr
    real(fp)                             , pointer :: ttlalpha
    real(fp)                             , pointer :: ttlmin
    real(fp)                             , pointer :: wetslope
    real(fp)                             , pointer :: dryslope
    real(fp)                             , pointer :: avaltime
    logical                              , pointer :: duneavalan
    real(fp)                             , pointer :: hswitch
    real(fp)                             , pointer :: dzmaxdune
    logical                              , pointer :: bermslopetransport
    logical                              , pointer :: bermslopebed
    logical                              , pointer :: bermslopesus
    real(fp)                             , pointer :: bermslope
    real(fp)                             , pointer :: bermslopefac
    real(fp)                             , pointer :: bermslopegamma
    real(fp)                             , pointer :: bermslopedepth
    real(fp)              , dimension(:) , pointer :: xx
    !
    real(hp)              , dimension(:) , pointer :: mergebuf
    logical                              , pointer :: bedupd
    logical                              , pointer :: cmpupd
    logical                              , pointer :: eqmbcsand
    logical                              , pointer :: eqmbcmud
    logical                              , pointer :: densin
    logical                              , pointer :: rouse
    logical                              , pointer :: epspar
    logical                              , pointer :: updinf
    logical                              , pointer :: neglectentrainment
    logical                              , pointer :: oldmudfrac
    logical                              , pointer :: varyingmorfac
    logical                              , pointer :: multi
    logical                              , pointer :: eulerisoglm
    logical                              , pointer :: glmisoeuler
    logical                              , pointer :: l_suscor
    character(256)                       , pointer :: bcmfilnam
    character(256)                       , pointer :: flcomp
    character(256)                       , pointer :: mmsyncfilnam
    character(256)                       , pointer :: ttlfil
    character(256)                       , pointer :: telfil
    character(256)                       , pointer :: flsthetsd
    type (bedbndtype)     , dimension(:) , pointer :: morbnd
    type (cmpbndtype)     , dimension(:) , pointer :: cmpbnd
    !
    real(fp) :: rmissval
    integer  :: imissval
    integer  :: istat
!
!! executable statements -------------------------------------------------------
!
    hydrt               => morpar%hydrt
    hydrt0              => morpar%hydrt0
    morft               => morpar%morft
    morft0              => morpar%morft0
    morfac              => morpar%morfac
    thresh              => morpar%thresh
    aksfac              => morpar%aksfac
    rwave               => morpar%rwave
    alfabs              => morpar%alfabs
    alfabn              => morpar%alfabn
    camax               => morpar%camax
    dzmax               => morpar%dzmax
    sus                 => morpar%sus
    suscorfac           => morpar%suscorfac
    bed                 => morpar%bed
    ti_sedtrans         => morpar%ti_sedtrans
    tmor                => morpar%tmor
    tcmp                => morpar%tcmp
    thetsd              => morpar%thetsd
    thetsduni           => morpar%thetsduni
    susw                => morpar%susw
    sedthr              => morpar%sedthr
    hmaxth              => morpar%hmaxth
    bedw                => morpar%bedw
    factsd              => morpar%factsd
    rdc                 => morpar%rdc
    rdw                 => morpar%rdw
    espir               => morpar%espir
    ashld               => morpar%ashld
    bshld               => morpar%bshld
    cshld               => morpar%cshld
    dshld               => morpar%dshld
    coulfri             => morpar%coulfri
    flfdrat             => morpar%flfdrat
    alfpa               => morpar%alfpa
    thcrpa              => morpar%thcrpa
    asklhe              => morpar%asklhe
    mwwjhe              => morpar%mwwjhe
    ttlalpha            => morpar%ttlalpha
    ttlmin              => morpar%ttlmin
    wetslope            => morpar%wetslope
    dryslope            => morpar%dryslope
    avaltime            => morpar%avaltime
    duneavalan          => morpar%duneavalan
    hswitch             => morpar%hswitch
    dzmaxdune           => morpar%dzmaxdune
    bermslopetransport  => morpar%bermslopetransport
    bermslopebed        => morpar%bermslopebed
    bermslopesus        => morpar%bermslopesus
    bermslope           => morpar%bermslope
    bermslopefac        => morpar%bermslopefac
    bermslopegamma      => morpar%bermslopegamma
    bermslopedepth      => morpar%bermslopedepth
    !
    ihidexp             => morpar%ihidexp
    iti_sedtrans        => morpar%iti_sedtrans
    itmor               => morpar%itmor
    itcmp               => morpar%itcmp
    iopkcw              => morpar%iopkcw
    iopsus              => morpar%iopsus
    islope              => morpar%islope
    morfacpar           => morpar%morfacpar
    morfacrec           => morpar%morfacrec
    morfactable         => morpar%morfactable
    nxx                 => morpar%nxx
    morbnd              => morpar%morbnd
    cmpbnd              => morpar%cmpbnd
    mergebuf            => morpar%mergebuf
    xx                  => morpar%xx
    ttlform             => morpar%ttlform
    telform             => morpar%telform
    !
    bedupd              => morpar%bedupd
    cmpupd              => morpar%cmpupd
    eqmbcsand           => morpar%eqmbcsand
    eqmbcmud            => morpar%eqmbcmud
    densin              => morpar%densin
    rouse               => morpar%rouse
    epspar              => morpar%epspar
    updinf              => morpar%updinf
    neglectentrainment  => morpar%neglectentrainment
    oldmudfrac          => morpar%oldmudfrac
    varyingmorfac       => morpar%varyingmorfac
    multi               => morpar%multi
    !
    bcmfilnam           => morpar%bcmfilnam
    flcomp              => morpar%flcomp
    mmsyncfilnam        => morpar%mmsyncfilnam
    ttlfil              => morpar%ttlfil
    telfil              => morpar%telfil
    flsthetsd           => morpar%flsthetsd
    !
    istat = 0
    allocate (morpar%moroutput  , STAT = istat)
    allocate (morpar%mornum     , STAT = istat)
    allocate (morpar%flufflyr   , STAT = istat)
    !
    pangle              => morpar%pangle
    fpco                => morpar%fpco
    factcr              => morpar%factcr
    subiw               => morpar%subiw
    eulerisoglm         => morpar%eulerisoglm
    glmisoeuler         => morpar%glmisoeuler
    l_suscor            => morpar%l_suscor
    !
    call initmoroutput(morpar%moroutput)
    !
    morpar%mornum%pure1d                   = .false.
    morpar%mornum%upwindbedload            = .true.
    morpar%mornum%laterallyaveragedbedload = .false.
    morpar%mornum%maximumwaterdepth        = .false.
    morpar%mornum%maximumwaterdepthfrac    = 1.0d0 !by default, if `maximumwaterdepth=.true.`, `hs_mor=max(hs,hu)`, which is the old functionality. 
    !
    rmissval           = -999.0_fp
    imissval           = -999
    !
    hydrt              = 0.0_hp
    hydrt0             = 0.0_hp
    morft              = 0.0_hp
    morft0             = 0.0_hp
    !
    bcmfilnam          = ' '
    flcomp             = ' '
    mmsyncfilnam       = ' '
    ttlfil             = ' '
    telfil             = ' '
    flsthetsd          = ' '
    !
    morfac             = 1.0_fp
    thresh             = 0.1_fp
    aksfac             = 1.0_fp
    rwave              = 2.0_fp
    alfabs             = 1.0_fp
    alfabn             = 1.5_fp
    camax              = 0.65_fp
    dzmax              = 0.05_fp
    sus                = 1.0_fp
    bed                = 1.0_fp
    ti_sedtrans        = 0.0_fp
    tmor               = 0.0_fp
    tcmp               = 0.0_fp
    thetsduni          = 0.0_fp
    susw               = 1.0_fp
    sedthr             = 0.5_fp
    hmaxth             = 1.0_fp
    bedw               = 1.0_fp
    factcr             = 1.0_fp
    factsd             = 1.0_fp
    rdw                = 0.02_fp
    rdc                = 0.01_fp
    espir              = 0.0_fp
    ashld              = 0.85_fp
    bshld              = 0.5_fp
    cshld              = 0.0_fp
    dshld              = 0.0_fp
    pangle             = 0.0_fp
    fpco               = 1.0_fp
    subiw              = 51
    coulfri            = rmissval
    flfdrat            = rmissval
    alfpa              = rmissval
    thcrpa             = rmissval
    asklhe             = rmissval
    mwwjhe             = rmissval
    ttlalpha           = 0.1_fp
    ttlmin             = 0.0_fp
    wetslope           = 10.0_fp
    dryslope           = 1.0_fp
    avaltime           = 86400.0_fp
    duneavalan         = .false.
    hswitch            = 0.1_fp
    dzmaxdune          = 100.0_fp           ! with Marlies, 20180417
    bermslopetransport = .false.
    bermslopebed       = .true.
    bermslopesus       = .true.
    bermslope          = 1d-1
    bermslopefac       = 1d0
    bermslopegamma     = 1d0
    bermslopedepth     = 1d0
    !
    ihidexp            = 1
    iti_sedtrans       = 0
    itmor              = 0
    itcmp              = 0
    iopkcw             = 1
    iopsus             = 0
    islope             = 2
    morfacpar          = imissval
    morfacrec          = imissval
    morfactable        = imissval
    nxx                = 0
    ttlform            = 1
    telform            = 1
    !
    bedupd             = .false.
    cmpupd             = .false.
    eqmbcsand          = .true.
    eqmbcmud           = .false.
    eulerisoglm        = .false.    
    glmisoeuler        = .false.    
    l_suscor           = .true.    
    suscorfac          = 1.0_fp
    densin             = .true.
    rouse              = .false.
    epspar             = .false.
    updinf             = .false.
    neglectentrainment = .false.
    oldmudfrac         = .false.
    varyingmorfac      = .false.
    multi              = .false.
    !
    nullify(morpar%morbnd)
    nullify(morpar%cmpbnd)
    nullify(morpar%xx)
    nullify(morpar%mergebuf)
    nullify(morpar%thetsd)
    !
    call initfluffy(morpar%flufflyr)
end subroutine nullmorpar


!> Give the morphological output flags their default value
subroutine initmoroutput(moroutput, def)
    type(moroutputtype), intent(inout) :: moroutput !< data structure containing all morphology output flags
    logical  , optional, intent(in)    :: def       !< optional default value for morphology output flags
    !
    logical :: yes ! value assigned to output flags that are .true. by default
    logical :: no  ! value assigned to output flags that are .false. by default
    !
    if (present(def)) then
        ! if default value is  specified, use it for all flags
        yes = def
        no  = def
    else
        ! if default value is not specified, use .true. and .false.
        yes = .true.
        no  = .false.
    endif
    !
    moroutput%transptype  = 2
    !
    moroutput%statflg(:,:) = 0
    moroutput%nstatqnt     = 0
    moroutput%weightflg    = 1
    moroutput%avgintv      = -999d0
    moroutput%morstats     = .false.
    !
    moroutput%aks           = no
    moroutput%cumavg        = no
    moroutput%dg            = no
    moroutput%dgsd          = no
    moroutput%dm            = no
    moroutput%dmsedcum      = no
    moroutput%dpbedlyr      = yes
    moroutput%dzduuvv       = no
    moroutput%fixfac        = no
    moroutput%hidexp        = no
    moroutput%frac          = no
    moroutput%lyrfrac       = yes
    moroutput%msed          = yes
    moroutput%mudfrac       = no
    moroutput%percentiles   = no
    moroutput%poros         = yes
    moroutput%rca           = yes
    moroutput%rsedeq        = yes
    moroutput%sandfrac      = no
    moroutput%sedpar        = no
    moroutput%seddif        = no
    moroutput%sbuuvv        = yes
    moroutput%sbcuv         = no
    moroutput%sscuv         = no
    moroutput%sbcuuvv       = no
    moroutput%ssuuvv        = yes
    moroutput%sbwuv         = no
    moroutput%sbwuuvv       = no
    moroutput%sswuv         = no
    moroutput%sswuuvv       = no
    moroutput%suvcor        = no
    moroutput%sourcesink    = no
    moroutput%taub          = no
    moroutput%taurat        = no
    moroutput%umod          = no
    moroutput%ustar         = no
    moroutput%uuuvvv        = no
    moroutput%ws            = yes
    moroutput%zumod         = no
    moroutput%rawtransports = no
    moroutput%blave         = no
    moroutput%bamor         = no
    moroutput%wumor         = no
end subroutine initmoroutput


!> Initialize a fluff layer data structure
subroutine initfluffy(flufflyr)
!!--declarations----------------------------------------------------------------
    !
    ! Function/routine arguments
    !
    type (fluffy_type)                   , target      :: flufflyr
    !
    ! Local variables
    !
!
!! executable statements -------------------------------------------------------
!
    flufflyr%iflufflyr = 0
    !
    nullify(flufflyr%mfluni)
    nullify(flufflyr%mfluff)
    nullify(flufflyr%bfluff0)
    nullify(flufflyr%bfluff1)
    nullify(flufflyr%depfac)
    nullify(flufflyr%sinkf)
    nullify(flufflyr%sourf)
    nullify(flufflyr%mflfil)
    !
    flufflyr%bfluff0_fil = ' '
    flufflyr%bfluff1_fil = ' '
    flufflyr%depfac_fil  = ' '
end subroutine initfluffy


!> Allocate a fluff layer data structure.
function allocfluffy(flufflyr, lsed, nmlb, nmub) result(istat)
!!--declarations----------------------------------------------------------------
    !
    ! Function/routine arguments
    !
    type (fluffy_type)                   , pointer     :: flufflyr
    integer                                            :: istat
    integer                              , intent(in)  :: lsed
    integer                              , intent(in)  :: nmlb
    integer                              , intent(in)  :: nmub
    !
    ! Local variables
    !
!
!! executable statements -------------------------------------------------------
!
                  allocate(flufflyr%mfluni(lsed), STAT = istat)
    if (istat==0) allocate(flufflyr%mfluff(lsed,nmlb:nmub), STAT = istat)
    if (istat==0) allocate(flufflyr%sinkf(lsed,nmlb:nmub), STAT = istat)
    if (istat==0) allocate(flufflyr%sourf(lsed,nmlb:nmub), STAT = istat)
    if (istat==0) allocate(flufflyr%mflfil(lsed), STAT = istat)
    !
    select case (flufflyr%iflufflyr)
    case (1)
       if (istat==0) allocate(flufflyr%bfluff0(lsed,nmlb:nmub), STAT = istat)
       if (istat==0) allocate(flufflyr%bfluff1(lsed,nmlb:nmub), STAT = istat)
    case (2)
       if (istat==0) allocate(flufflyr%depfac(lsed,nmlb:nmub), STAT = istat)
    endselect
end function allocfluffy


!> Clean up a fluff layer data structure.
subroutine clrfluffy(istat, flufflyr)
!!--declarations----------------------------------------------------------------
    !
    ! Function/routine arguments
    !
    type (fluffy_type)                   , pointer     :: flufflyr
    integer                              , intent(out) :: istat
    !
    ! Local variables
    !
!
!! executable statements -------------------------------------------------------
!
    flufflyr%iflufflyr = 0
    !
    if (associated(flufflyr%mfluni))      deallocate(flufflyr%mfluni,      STAT = istat)
    if (associated(flufflyr%mfluff))      deallocate(flufflyr%mfluff,      STAT = istat)
    if (associated(flufflyr%bfluff0))     deallocate(flufflyr%bfluff0,     STAT = istat)
    if (associated(flufflyr%bfluff1))     deallocate(flufflyr%bfluff1,     STAT = istat)
    if (associated(flufflyr%depfac))      deallocate(flufflyr%depfac,      STAT = istat)
    if (associated(flufflyr%sinkf))       deallocate(flufflyr%sinkf,       STAT = istat)
    if (associated(flufflyr%sourf))       deallocate(flufflyr%sourf,       STAT = istat)
    if (associated(flufflyr%mflfil))      deallocate(flufflyr%mflfil,      STAT = istat)
end subroutine clrfluffy


!> Clean up a morpar_type data structure.
subroutine clrmorpar(istat, morpar)
!!--declarations----------------------------------------------------------------
    use table_handles
    !
    ! Function/routine arguments
    !
    type (morpar_type)                   , pointer     :: morpar
    integer                              , intent(out) :: istat
    !
    ! Local variables
    !
    integer                                        :: i
    type (bedbndtype)     , dimension(:) , pointer :: morbnd
!
!! executable statements -------------------------------------------------------
!
    morbnd              => morpar%morbnd
    !
    if (associated(morpar%morbnd)) then
       do i = 1, size(morpar%morbnd)
          if (associated(morbnd(i)%idir))      deallocate(morbnd(i)%idir,      STAT = istat)
          if (associated(morbnd(i)%nm))        deallocate(morbnd(i)%nm,        STAT = istat)
          if (associated(morbnd(i)%nxmx))      deallocate(morbnd(i)%nxmx,      STAT = istat)
          if (associated(morbnd(i)%lm))        deallocate(morbnd(i)%lm,        STAT = istat)
          if (associated(morbnd(i)%alfa_dist)) deallocate(morbnd(i)%alfa_dist, STAT = istat)
          if (associated(morbnd(i)%alfa_mag))  deallocate(morbnd(i)%alfa_mag,  STAT = istat)
       enddo
       deallocate(morpar%morbnd, STAT = istat)
    endif
    if (associated(morpar%cmpbnd))    deallocate(morpar%cmpbnd,    STAT = istat)
    if (associated(morpar%xx))        deallocate(morpar%xx,        STAT = istat)
    if (associated(morpar%mergebuf))  deallocate(morpar%mergebuf,  STAT = istat)
    if (associated(morpar%moroutput)) deallocate(morpar%moroutput, STAT = istat)
    if (associated(morpar%mornum))    deallocate(morpar%mornum,    STAT = istat)
    call cleartable(morpar%bcmfile)
    call cleartable(morpar%morfacfile)
    if (associated(morpar%flufflyr)) then
        call clrfluffy(istat, morpar%flufflyr)
        deallocate(morpar%flufflyr, STAT = istat)
    endif
    if (associated(morpar%thetsd))    deallocate(morpar%thetsd,    STAT = istat)
    !
end subroutine clrmorpar


!> Nullify/initialize a trapar_type data structure.
subroutine nulltrapar(trapar  )
!!--declarations----------------------------------------------------------------
    !
    ! Function/routine arguments
    !
    type (trapar_type)                   , target      :: trapar
    !
    ! Local variables
    !
    ! NONE
!
!! executable statements -------------------------------------------------------
!
    !
    ! Note: 30 is hardcoded in sediment transport formulae
    !
    trapar%npar     = 30
    trapar%nparfile = 0
    !
    nullify(trapar%dll_function_settle)
    nullify(trapar%dll_name_settle)
    nullify(trapar%dll_handle_settle)
    nullify(trapar%dll_integers_settle)
    nullify(trapar%dll_reals_settle)
    nullify(trapar%dll_strings_settle)
    nullify(trapar%dll_usrfil_settle)
    nullify(trapar%iform_settle)
    nullify(trapar%par_settle)
    !
    nullify(trapar%dll_function)
    nullify(trapar%dll_name)
    nullify(trapar%dll_handle)
    nullify(trapar%dll_integers)
    nullify(trapar%dll_reals)
    nullify(trapar%dll_strings)
    nullify(trapar%dll_usrfil)
    nullify(trapar%flstrn)
    nullify(trapar%iform)
    nullify(trapar%name)
    nullify(trapar%par)
    nullify(trapar%parname)
    nullify(trapar%parfilename)
    nullify(trapar%iparfile)
end subroutine nulltrapar


!> Clean up a trapar_type data structure.
subroutine clrtrapar(istat     ,trapar  )
!!--declarations----------------------------------------------------------------
    !
    ! Function/routine arguments
    !
    type (trapar_type)                   , pointer     :: trapar
    integer                              , intent(out) :: istat
    !
    ! Local variables
    !
    integer                     :: i
    integer(pntrsize), external :: close_shared_library
    integer(pntrsize)           :: error
!
!! executable statements -------------------------------------------------------
!
    if (associated(trapar%dll_handle)) then
       do i = 1,size(trapar%dll_handle)
          if (trapar%dll_handle_settle(i) /= 0) then
             error = close_shared_library(trapar%dll_handle_settle(i))
          endif
          if (trapar%dll_handle(i) /= 0) then
             error = close_shared_library(trapar%dll_handle(i))
          endif
       enddo
    endif
    !
    if (associated(trapar%dll_function_settle)) deallocate(trapar%dll_function_settle, STAT = istat)
    if (associated(trapar%dll_name_settle    )) deallocate(trapar%dll_name_settle    , STAT = istat)
    if (associated(trapar%dll_handle_settle  )) deallocate(trapar%dll_handle_settle  , STAT = istat)
    if (associated(trapar%dll_integers_settle)) deallocate(trapar%dll_integers_settle, STAT = istat)
    if (associated(trapar%dll_reals_settle   )) deallocate(trapar%dll_reals_settle   , STAT = istat)
    if (associated(trapar%dll_strings_settle )) deallocate(trapar%dll_strings_settle , STAT = istat)
    if (associated(trapar%dll_usrfil_settle  )) deallocate(trapar%dll_usrfil_settle  , STAT = istat)
    if (associated(trapar%iform_settle       )) deallocate(trapar%iform_settle       , STAT = istat)
    if (associated(trapar%par_settle         )) deallocate(trapar%par_settle         , STAT = istat)
    !
    if (associated(trapar%dll_function)) deallocate(trapar%dll_function, STAT = istat)
    if (associated(trapar%dll_name    )) deallocate(trapar%dll_name    , STAT = istat)
    if (associated(trapar%dll_handle  )) deallocate(trapar%dll_handle  , STAT = istat)
    if (associated(trapar%dll_integers)) deallocate(trapar%dll_integers, STAT = istat)
    if (associated(trapar%dll_reals   )) deallocate(trapar%dll_reals   , STAT = istat)
    if (associated(trapar%dll_strings )) deallocate(trapar%dll_strings , STAT = istat)
    if (associated(trapar%dll_usrfil  )) deallocate(trapar%dll_usrfil  , STAT = istat)
    if (associated(trapar%flstrn      )) deallocate(trapar%flstrn      , STAT = istat)
    if (associated(trapar%iform       )) deallocate(trapar%iform       , STAT = istat)
    if (associated(trapar%name        )) deallocate(trapar%name        , STAT = istat)
    if (associated(trapar%par         )) deallocate(trapar%par         , STAT = istat)
    if (associated(trapar%parname     )) deallocate(trapar%parname     , STAT = istat)
    if (associated(trapar%parfilename )) deallocate(trapar%parfilename , STAT = istat)
    if (associated(trapar%iparfile    )) deallocate(trapar%iparfile    , STAT = istat)
end subroutine clrtrapar

!> return values for all transport formula parameters
subroutine get_transport_parameters(trapar, l, nm, timhr, localpar)
    use table_handles, only: gettabledata
    !
    type(trapar_type)     , intent(in)  :: trapar       !< transport settings
    integer               , intent(in)  :: l            !< sediment fraction
    integer               , intent(in)  :: nm           !< cell index
    real(fp)              , intent(in)  :: timhr        !< time since reference date [h]
    real(fp)              , intent(out) :: localpar(:)  !< collection of sediment parameters
    
    integer                    :: i            !< parameter index

    do i = 1, trapar%npar
       call get_one_transport_parameter(localpar(i:i), trapar, l, i, timhr, nm)
    end do
end subroutine get_transport_parameters


!> return a value for one transport formula parameter
subroutine get_one_transport_parameter(val, trapar, l, i, timhr, nm)
    use table_handles, only: gettabledata
    !
    real(fp)              , intent(inout) :: val(:)     !< the parameter value at all nm
    type(trapar_type)     , intent(in)    :: trapar     !< transport settings
    integer               , intent(in)    :: l          !< sediment fraction
    integer               , intent(in)    :: i          !< parameter index
    real(fp)    , optional, intent(in)    :: timhr      !< time since reference date [h]
    integer     , optional, intent(in)    :: nm         !< spatial index for which value is requested
    
    integer                     :: j           !< sediment parameter source file index
    real(fp)                    :: parvec(1)   !< array to receive the value
    character(256)              :: message     !< error message
    type(parfile_type), pointer :: parfile     !< temporary to one trapar%parfile field
    
    j = trapar%iparfile(i,l)
    if (j == 0) then
        val(:) = trapar%par(i,l)
        
    else
        select case (trapar%parfile(j)%source)
        case (PARSOURCE_FIELD)
            if (present(nm)) then
                val(:) = trapar%parfile(j)%parfld(nm)
            else
                val(:) = trapar%parfile(j)%parfld(:)
            endif
            
        case (PARSOURCE_TIME)
            parfile => trapar%parfile(j)
            if (present(timhr)) then
                if (timhr > parfile%timhr) then
                    message = ' '
                    call gettabledata(parfile%ts ,parfile%itable_ts, parfile%ipar_ts, parfile%npar_ts, parfile%irec_ts, parvec, timhr, parfile%refjulday, message)
                    if (message /= ' ') return ! TODO
                    parfile%par = parvec(1)
                    parfile%timhr = timhr
                end if
            end if
            val(:) = parfile%par
            
        end select
    end if
end subroutine get_one_transport_parameter

end module morphology_data_module
