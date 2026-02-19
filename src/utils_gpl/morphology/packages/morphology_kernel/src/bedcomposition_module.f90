module bedcomposition_module
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
! This module keeps track of the bed composition at one or more locations. The
! locations are indicated by indices nmlb to nmub. The bed composition consists
! of nfrac sediment fractions; their mass and volume fractions sum to 1. The
! bed may be schematized using one or more layers.
!
!!--module declarations---------------------------------------------------------
use precision
use sediment_basics_module, only: SEDTYP_SILT
private

!
! public data types
!
public bedcomp_data
public bedcomp_settings
public erosion_settings

!
! public routines
!
public  bedcomposition_module_info
public  copybedcomp
public  updmorlyr
public  gettoplyr
public  detthcmud
public  getalluvthick
public  getporosity
public  getfrac
public  getbedprop
public  getmfrac
public  setmfrac
public  getvfrac
public  setvfrac
public  getsedthick
public  initmorlyr
public  setbedfracprop
public  allocmorlyr
public  clrmorlyr
public  bedcomp_use_bodsed
public  initpreload
public  lyrdiffusion
public  set_default_fractions
!
public bedcomp_getpointer_integer
public bedcomp_getpointer_logical
public bedcomp_getpointer_realfp
public bedcomp_getpointer_realprec
!
! interfaces
!
interface bedcomp_getpointer_logical
   module procedure bedcomp_getpointer_logical_scalar
end interface
interface bedcomp_getpointer_integer
   module procedure bedcomp_getpointer_integer_scalar
end interface
interface bedcomp_getpointer_realfp
   module procedure bedcomp_getpointer_fp_scalar
   module procedure bedcomp_getpointer_fp_1darray
   module procedure bedcomp_getpointer_fp_2darray
   module procedure bedcomp_getpointer_fp_3darray
end interface
interface bedcomp_getpointer_realprec
   module procedure bedcomp_getpointer_prec_2darray
end interface
interface getsedthick
   module procedure getsedthick_1point
   module procedure getsedthick_allpoints
end interface
!
! morphology layers numerical settings
!
type morlyrnumericstype
    logical  :: track_mass_shortage       ! track the mass shortage
    real(fp) :: mass_shortage_thresh      ! minimum erosion thickness for a shortage warning
    integer  :: max_num_shortage_warnings ! maximum number of shortage warnings remaining
end type morlyrnumericstype

! iconsolidate
integer, parameter, public :: CONSOL_NONE       = 0 !  0: no consolidation
integer, parameter, public :: CONSOL_GIBSON     = 1 !  1: full Gibson model
integer, parameter, public :: CONSOL_DECON      = 2 !  2: Dynamic Equilibrium CONsolidation (DECON)
integer, parameter, public :: CONSOL_TERZAGHI   = 3 !  3: simple loading model (Terzaghi)
integer, parameter, public :: CONSOL_TERZ_PEAT  = 4 !  4: simple loading including peat (Terzaghi)
integer, parameter, public :: CONSOL_NOCOMP     = 5 !  5: No Compaction

! idiffusion
integer, parameter, public :: BDIFF_NONE        = 0 !  0: no diffusion
integer, parameter, public :: BDIFF_ACTIVE      = 1 !  1: diffusion

! ierosion
integer, parameter, public :: EROS_CONST        = 0 !  0: cohesive sediment erodibility doesn't depend on bed composition
integer, parameter, public :: EROS_WHITEHOUSE   = 1 !  1: Whitehouse (2001)
integer, parameter, public :: EROS_LE_HIR       = 2 !  2: Le Hir (2011)
integer, parameter, public :: EROS_ALONSO       = 3 !  3: Alonso et al (2021)
integer, parameter, public :: EROS_WINTERWERP   = 4 !  4: Winterwerp (2013)
integer, parameter, public :: EROS_MUSA         = 5 !  5: MUSA project - Weerdenburg

! ifracdef
integer, parameter, public :: FRAC_MASS         = 1 !  1: mass fractions (sum of all fractions equals 1)
integer, parameter, public :: FRAC_VOLUME       = 2 !  2: (solid) volume fractions (sum of all fractions equals 1)

! iporosity
integer, parameter, public :: POROS_IN_DENSITY  = 0 !  0: porosity included in densities, set porosity to 0
integer, parameter, public :: POROS_FRINGS      = 1 !  1: Frings (May 2009)
integer, parameter, public :: POROS_WELTJE      = 2 !  2: Weltje based on data by Beard & Weyl (AAPG Bull., 1973)
integer, parameter, public :: POROS_SVFRAC0     = 3 !  3: svfrac0
integer, parameter, public :: POROS_SVFRAC0SM   = 4 !  4: weight average of svfrac0m and svfrac0s
integer, parameter, public :: POROS_CDRYB       = 5 !  5: similar to 0, but with porosity shared

! iunderlyr
integer, parameter, public :: BED_MIXED         = 1 !  1: standard fully mixed concept
integer, parameter, public :: BED_LAYERED       = 2 !  2: layered bed concept

! updbaselyr
integer, parameter, public :: BASELYR_UPDATED   = 1 !  1: base layer is an independent layer (both composition and thickness computed like any other layer)
integer, parameter, public :: BASELYR_CONST_FRC = 2 !  2: base layer composition is kept fixed (thickness is computed - total mass conserved)
integer, parameter, public :: BASELYR_COPY_FRC  = 3 !  3: base layer composition is set equal to the composition of layer above it (thickness computed - total mass conserved)
integer, parameter, public :: BASELYR_CONST     = 4 !  4: base layer composition and thickness constant (no change whatsoever)
integer, parameter, public :: BASELYR_CONST_THK = 5 !  5: base layer composition is updated, but thickness is kept constant

! updtoplyr
integer, parameter, public :: TOPLYR_POR_RESET  = 1 !  1: top layer porosity is recomputed based on new mixture
integer, parameter, public :: TOPLYR_POR_UPDATE = 2 !  2: top layer porosity is updated based on newly added sediment

type erosion_settings
    real(fp) :: A                                    !< activity of soil, which is used to calculate PI index    
    real(fp) :: alpha                                !< a constant in determining critical bed shear stress for erosion
    real(fp) :: alpha_me                             !< tuning parameter in simple Me equation 
    real(fp) :: alpha_mix                            !< tuning parameter for cohesionless mixture
    real(fp) :: alpha_lehir                          !< tuning parameter in Le Hir tcrero equation 
    real(fp) :: alpha_winterwerp                     !< tuning parameter in Winterwerp Me equation 
    real(fp) :: alpha1                               !< non-linearity coefficient for the interpolation between rho_min1 and rho_star [-]
    real(fp) :: alpha2                               !< non-linearity coefficient for the interpolation between rho_star and rho_min2 [-]
    real(fp) :: beta                                 !< a constant in determining critical bed shear stress for erosion
    real(fp) :: beta_mix                             !< tuning parameter for cohesionless mixture
    real(fp) :: C0                                   !< interceptin x axis of mud fraction vs critical bed shear stress for erosion plot
    real(fp) :: d50sed                               !< d50 grain size of sediment supply [m]
    real(fp) :: rho_max                              !< layer density at and above which critical shear stress taucr_min2 should be applied [kg/m3]
    real(fp) :: rho_min                              !< layer density at and below which critical shear stress taucr_min1 should be applied [kg/m3]
    real(fp) :: rho_star                             !< layer density at which critical shear stress maximum taucr_max should be applied [kg/m3]
    real(fp) :: taucr_max                            !< maximum critical shear stress [N/m2]
    real(fp) :: taucr_min1                           !< critical shear stress at low density [N/m2]
    real(fp) :: taucr_min2                           !< critical shear stress at high density [N/m2]
end type erosion_settings

type bedcomp_settings
    !
    ! doubles
    !
    real(fp) :: thlalyr                              !< thickness of Lagrangian underlayer layers
    real(fp) :: theulyr                              !< thickness of Eulerian underlayer layers
    !
    ! reals
    real(fp) :: ag                                   !< gravity
    real(fp) :: dtdecon                              !< time interval to call consolidation for DECON [s]
    real(fp) :: dzprofile                            !< m, resolution of equilibrium concentration profile for Dynamic Equilibrium CONsolidation (DECON)
    real(fp) :: nf                                   !< fractal dimension nf
    real(fp) :: kbioturb                             !< bioturbation induced diffusion coefficient [m2/s]
    real(fp) :: kk                                   !< permeability coefficient [m/s]
    real(fp) :: ksigma                               !< effective stress coefficient [Pa]
    real(fp) :: ksigma0                              !< effective stress coefficient (usually set as 0) [Pa]
    real(fp) :: ky                                   !< strength coefficient [Pa]
    real(fp) :: confac                               !< ratio between consolidation and morphological time scales [-]
    real(fp) :: svfrac0                              !< user-input initial solids volume fraction for newly deposited sediments (general)
    real(fp) :: svfrac0m                             !< user-input initial solids volume fraction for newly deposited mud fractions
    real(fp) :: svfrac0s                             !< user-input initial solids volume fraction for newly deposited sand fractions
    real(fp) :: svgel                                !< solids volume fraction at gelling point
    real(fp) :: svmax                                !< if svfrac > svmax, consolidation stops
    real(fp) :: thconlyr                             !< initial total thickness of consolidatng layers, delta_c in Winterwerp's note.
                                                     ! thconlyr is a time-varying variable
    real(fp) :: thtrconcr                            !< the critical thickness of transport layer above which consolidation module is called [m]
                                                     ! just choose a very small value to avoid numerical issues, e.g., 10E-6.
    real(fp) :: thtrempty                            !< the critical thickness of transport layer below which it's considered empty [m]
    real(fp) :: ymodpeat                             !< Young modulus of peat (Terzaghi., 1942)
    real(fp) :: ccpeat                               !< consolidation rate of peat (Terzaghi., 1942)
    real(fp) :: peatloi                              !< loss on ignition (LI) or organic content in peat
    real(fp) :: peatthick                            !< Initial thickness of peat layer
    real(fp) :: parb                                 !< coefficient in peat equation (Van Asselen, 2009)
    real(fp) :: parc                                 !< coefficient in peat equation (Van Asselen, 2009)
    real(fp) :: pard                                 !< coefficient in peat equation (Van Asselen, 2009)
    real(fp) :: minporm                              !< critical porosity for mud
    real(fp) :: minpors                              !< critical porosity for sand
    real(fp) :: crmud                                !< consolidation rate of mud
    real(fp) :: crsand                               !< consolidation rate of sand
    real(fp) :: crmsec                               !< secondary mud consolidation
    real(fp) :: porini                               !< initial porosity 
    !critical bed shear stress
    real(fp) :: rhow_const                           !< a constant that defines water density
    real(fp) :: ptr                                  !< percentage of thickness reduction
    !
    ! integers
    !
    integer :: imixtr                                !< flag define whether we consider to mix the transport layer with layers below
                                                     !       for replenish step, default = 1, mix.
    integer :: nconlyr                               !< number of consolidating layers used, user defined
    integer :: iconsolidate                          !< switch for consolidation model
                                                     !  0: no consolidation
                                                     !  1: full Gibson model
                                                     !  2: Dynamic Equilibrium CONsolidation (DECON)
                                                     !  3: simple loading model (Terzaghi)
                                                     !  4: simple loading including peat (Terzaghi)
                                                     !  5: No Compaction
    integer :: idiffusion                            !< switch for diffusion between layers
                                                     !  0: no diffusion
                                                     !  1: diffusion
    integer :: ierosion                              !< switch for cohesive sediment erodibility
                                                     !  0: cohesive sediment erodibility doesn't depend on bed composition
                                                     !  1: Whitehouse (2001)
                                                     !  2: Le Hir (2011)
                                                     !  3: Winterwerp (2013)
    integer :: ifractions                            !< switch for fractions returned by getfrac
                                                     !  1: mass fractions (sum of all fractions equals 1)
                                                     !  2: solid volume fractions (sum of all fractions equals 1)
    integer :: iporosity                             !< switch for porosity (simulate porosity if iporosity > 0)
                                                     !  0: porosity included in densities, set porosity to 0
                                                     !  1: Frings (May 2009)
                                                     !  2: Weltje based on data by Beard & Weyl (AAPG Bull., 1973)
                                                     !  3: svfrac0
                                                     !  4: weight average
    integer :: iunderlyr                             !< switch for underlayer concept
                                                     !  1: standard fully mixed concept
                                                     !  2: graded sediment concept
    integer :: keuler                                !< index of first Eulerian (i.e. non-moving) layer
                                                     !  2   : standard Eulerian, only top layer moves with bed level
                                                     !  nlyr: fully Lagrangian (all layers move with bed level)
    integer :: nfrac                                 !< number of sediment fractions
    integer :: neulyr                                !< number of Eulerian underlayers
    integer :: nlalyr                                !< number of Lagrangian underlayers
    integer :: nlyr                                  !< number of layers (transport + exchange + under layers)
    integer :: ndiff                                 !< number of diffusion coefficients in vertical direction
    integer :: nmlb                                  !< start index of segments
    integer :: nmub                                  !< nm end index
    integer :: updtoplyr                             !< switch for top layer porosity updating
                                                     !  1: top layer porosity is recomputed based on new mixture
                                                     !  2: top layer porosity is updated based on newly added sediment
    integer :: updbaselyr                            !< switch for computing composition of base layer
                                                     !  1: base layer is an independent layer (both composition and thickness computed like any other layer)
                                                     !  2: base layer composition is kept fixed (thickness is computed - total mass conserved)
                                                     !  3: base layer composition is set equal to the composition of layer above it (thickness computed - total mass conserved)
                                                     !  4: base layer composition and thickness constant (no change whatsoever)
                                                     !  5: base layer composition is updated, but thickness is kept constant
    integer  :: peatfrac                             !< peat flag (no peat growth, peat thickness is homogeneous)
    integer  :: max_mud_sedtyp                       !< highest sediment type number that is considered a mud fraction
    !
    ! pointers
    !
    type (morlyrnumericstype) , pointer :: morlyrnum ! structure containing numerical settings
    type (erosion_settings)   , pointer :: erosion   ! structure containing for crtical shear stress for erosion and erosion parameter
    !
    integer  , dimension(:)   , pointer :: sedtyp    ! sediment type: 0=total/1=noncoh/2=coh
    real(fp) , dimension(:,:) , pointer :: kdiff     ! diffusion coefficients for mixing between layers, units : m2/s
    real(fp) , dimension(:)   , pointer :: phi       ! D50 diameter expressed on phi scale
    real(fp) , dimension(:)   , pointer :: rhofrac   ! density of fraction (specific density or including pores)
    real(fp) , dimension(:)   , pointer :: sigphi    ! standard deviation expressed on phi scale
    real(fp) , dimension(:)   , pointer :: thexlyr   ! thickness of exchange layer
    real(fp) , dimension(:)   , pointer :: thtrlyr   ! thickness of transport layer
    real(fp) , dimension(:)   , pointer :: zdiff     ! depth below bed level for which diffusion coefficients are defined, units : m
    real(fp) , dimension(:)   , pointer :: plyrthk
    real(fp) , dimension(:)   , pointer :: ymod
    real(fp) , dimension(:)   , pointer :: cc
    ! 
    ! logicals
    !
    logical :: exchlyr    !  flag for use of exchange layer (underlayer bookkeeping system)
    !
    ! characters
    !
    character(999)                       :: plyrstr
end type bedcomp_settings
!
type bedcomp_state
    real(prec) , dimension(:,:)  , pointer :: bodsed       !< Array with total sediment [kg/m2]
    real(fp)   , dimension(:)    , pointer :: dpsed        !< Total depth sediment layer [m]
    real(fp)   , dimension(:)    , pointer :: dzc          !< subsidence
    real(fp)   , dimension(:,:,:), pointer :: msed         !< composition of morphological layers: mass of sediment fractions [kg /m2]
    real(fp)   , dimension(:,:,:), pointer :: conclyr      !< composition of morphological layers: concentration of sediment fractions, conclyr=msed/thlyr
    real(fp)   , dimension(:,:)  , pointer :: preload      !< historical largest load [kg/m3]
    real(fp)   , dimension(:,:)  , pointer :: td           !< (morphological) time of latest load increment, i.e. that initiates primary compaction [minutes]
    real(fp)   , dimension(:)    , pointer :: rhow         !< Water density [kg/m3] (currently 2D, but should be 3D in the future)
    real(fp)   , dimension(:,:)  , pointer :: sedshort     !< sediment shortage in transport layer [kg /m2]
    real(fp)   , dimension(:,:)  , pointer :: svfrac       !< 1 - porosity coefficient [-]
    real(fp)   , dimension(:,:)  , pointer :: thlyr        !< thickness of morphological layers [m]
    real(fp)   , dimension(:,:)  , pointer :: cmudlyr      !< mud dry bed density each layer
    real(fp)   , dimension(:,:)  , pointer :: csandlyr     !< sand dry bed density each layer
    real(fp)   , dimension(:)    , pointer :: thmudgibson  !<
    real(fp)   , dimension(:)    , pointer :: thsandgibson !<
    real(fp)   , dimension(:,:)  , pointer :: thlyrtprev   !< overburden thickness of previous time step
    !peat
    real(fp)   , dimension(:,:)  , pointer :: strain       !<
    real(hp)                               :: tdecon       !< latest morphological time (morft) of consolidation [days since reference date]
end type bedcomp_state
!
type bedcomp_work
    real(fp), dimension(:,:) , pointer :: msed2
    real(fp), dimension(:)   , pointer :: svfrac2
    real(fp), dimension(:)   , pointer :: thlyr2
    real(fp), dimension(:)   , pointer :: preload2
    real(fp), dimension(:)   , pointer :: td2
    
    ! working arrays for high-concentration consolidation
    real(fp), dimension(:)   , pointer :: dthsedlyr      !< thickness of average pure sediment between two neighbouring layers [m]

    real(fp), dimension(:)   , pointer :: sigmaeff       !< effective stress [Pa]
    real(fp), dimension(:)   , pointer :: thsedlyr       !< thickness of pure sediment in each layer [m]
    real(fp), dimension(:)   , pointer :: svfracsand     !< new solids fraction after consolidation 
    real(fp), dimension(:)   , pointer :: svfracmud      !< new solids fraction after consolidation     

    real(fp), dimension(:)   , pointer :: vs0p5          !< particle settling velocity at layer interface [m/s]
    real(fp), dimension(:)   , pointer :: k0p5           !< permeability at layer interface [m/s]
    real(fp), dimension(:)   , pointer :: svfrac0p5      !< solids fraction at layer interface
    real(fp), dimension(:)   , pointer :: svfracsand0p5  !< new solids fraction after consolidation  
    real(fp), dimension(:)   , pointer :: svfracmud0p5   !< new solids fraction after consolidation
    
    real(fp), dimension(:)   , pointer :: svfracnew      !< new solid volume fraction after consolidation
    real(fp), dimension(:)   , pointer :: thlyrnew       !< new thickness after consolidation
    
    ! working arrays for low-concentration consolidation
    real(fp), dimension(:)   , pointer :: mmudlyr        !< mud mass each layer
    real(fp), dimension(:)   , pointer :: msandlyr       !< sand mass each layer
end type bedcomp_work
!
type bedcomp_data
   type (bedcomp_settings), pointer :: settings
   type (bedcomp_state)   , pointer :: state
   type (bedcomp_work)    , pointer :: work
end type bedcomp_data

contains

!> module version information ... this isn't going to work in Git ...
subroutine bedcomposition_module_info(messages)
    use message_module
    !
    type(message_stack) :: messages
    !
    call addmessage(messages,'$Id: bedcomposition_module.f90 140649 2022-01-20 14:39:56Z jagers $')
    call addmessage(messages,'$URL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Technical%20University%20of%20Delft/20190419_consolidation_compaction_v2/src/utils_gpl/morphology/packages/morphology_kernel/src/bedcomposition_module.f90 $')
end subroutine bedcomposition_module_info


subroutine set_default_fractions(this)
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                                                                           :: this     !< bed composition object
    !
    ! Local variables
    !
!
!! executable statements -------------------------------------------------------
!
    if (this%settings%iunderlyr == BED_MIXED) then
        this%settings%ifractions = FRAC_MASS
    else
        this%settings%ifractions = FRAC_VOLUME
    endif
end subroutine set_default_fractions

!> Update underlayer bookkeeping system for given erosion/sedimentation flux
function updmorlyr(this, dbodsd, dz, messages, morft, dtmor) result (istat)
    use precision
    use message_module, only: message_stack, message_len, addmessage
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                                                                           :: this     !< bed composition object
    real(fp), dimension(this%settings%nfrac, this%settings%nmlb:this%settings%nmub), intent(in)  :: dbodsd   !< change in sediment composition, units : kg/m2
    real(fp), dimension(this%settings%nmlb:this%settings%nmub)                     , intent(out) :: dz       !< change in bed level, units : m
    type(message_stack)                                                                          :: messages !< message stack
    real(hp)                                                                       , intent(in)  :: morft    !< morphological time [days since reference date]
    real(fp)                                                                       , intent(in)  :: dtmor    !< morphological time step [s]
    integer                                                                                      :: istat    !< function status
    !
    ! Local variables
    !
    logical                                 :: track_shortage
    integer                                 :: l
    integer                                 :: nm
    integer                                 :: k
    integer                                 :: kk
    integer                                 :: ktemp
    !
    real(fp)                                :: mmudlyr1
    real(fp)                                :: msandlyr1
    real(fp)                                :: seddep0
    real(fp)                                :: seddep1
    real(fp)                                :: thdiff
    real(fp)                                :: fac
    real(fp)                                :: temp
    real(fp)                                :: thick
    real(fp)                                :: thickd   !< thickness of newly deposited sediment
    real(fp)                                :: thicke   !< thickness of eroded sediment
    real(fp)                                :: preload0 !< preload of newly deposited sediment
    real(fp)                                :: svfracd
    real(fp)                                :: thtrlyrnew
    real(fp)                                :: phi_mud       !< mud volume fraction
    real(fp), dimension(this%settings%nfrac):: dmi
    real(fp)                                :: sdbodsed !< the sum of dbodsd
    real(fp)                                :: td0
    !
    character(message_len)                  :: message
    !
    type (morlyrnumericstype)     , pointer :: morlyrnum
    integer                       , pointer :: iconsolidate
    integer                       , pointer :: nconlyr
    real(prec) , dimension(:,:)   , pointer :: bodsed
    real(fp)   , dimension(:,:)   , pointer :: svfrac
    real(fp)   , dimension(:)     , pointer :: dpsed
    real(fp)   , dimension(:,:,:) , pointer :: msed
    real(fp)   , dimension(:)     , pointer :: rhofrac
    real(fp)   , dimension(:,:)   , pointer :: sedshort
    real(fp)   , dimension(:,:)   , pointer :: thlyr
    real(fp)                      , pointer :: thtrempty
    real(fp)   , dimension(:)     , pointer :: thtrlyr
    real(fp)   , dimension(:,:)   , pointer :: cmudlyr
    real(fp)   , dimension(:,:)   , pointer :: csandlyr
    real(fp)   , dimension(:,:)   , pointer :: preload
    real(fp)   , dimension(:,:)   , pointer :: td
    real(fp)                                :: poros
    
    real(fp),dimension(this%settings%nfrac) :: permud       ! mud fraction mass percentage
    real(fp),dimension(this%settings%nfrac) :: persand      ! sand fraction mass percentage
    real(fp),dimension(this%settings%nfrac) :: mmud         ! mud fraction mass 
    real(fp),dimension(this%settings%nfrac) :: msand        ! sand fraction mass
    real(fp),dimension(this%settings%nfrac) :: mfrac
    real(fp)                                :: totmassd     !< total mass of deposited sediment
    real(fp)                                :: totsv        !< total sediment volume in top layer
    real(fp)                                :: totsvd       !< total sediment volume of deposited sediment
    real(fp)                                :: totsve       !< total sediment volume of eroded sediment
    logical                                 :: call_consolidate !< flag indicating whether consolidate should be called
!
!! executable statements -------------------------------------------------------
!
    morlyrnum    => this%settings%morlyrnum
    iconsolidate => this%settings%iconsolidate
    nconlyr      => this%settings%nconlyr
    rhofrac      => this%settings%rhofrac
    thtrempty    => this%settings%thtrempty
    thtrlyr      => this%settings%thtrlyr
    !
    svfrac      => this%state%svfrac
    bodsed      => this%state%bodsed
    dpsed       => this%state%dpsed
    msed        => this%state%msed
    sedshort    => this%state%sedshort
    thlyr       => this%state%thlyr
    cmudlyr     => this%state%cmudlyr
    csandlyr    => this%state%csandlyr
    preload     => this%state%preload
    td          => this%state%td
    !
    istat = allocwork(this)
    if (istat /= 0) return
    track_shortage = this%settings%morlyrnum%track_mass_shortage
    select case (this%settings%iunderlyr)
    case (BED_LAYERED)
        if (this%settings%iconsolidate == CONSOL_NONE) then
            call_consolidate = .false.
        elseif (this%settings%iconsolidate == CONSOL_DECON) then
            ! check whether it's time to consolidate the bed again
            if (morft < this%state%tdecon) then
                call_consolidate = .false.
            else
                ! consolidate now and set the next consolidation time
                this%state%tdecon = morft + real(this%settings%dtdecon,hp)/86400.0_hp
                call_consolidate = .true.
            endif
        else
            call_consolidate = .true.
        endif
        
        do nm = this%settings%nmlb,this%settings%nmub
            call getsedthick_1point(this, nm, seddep0)
            !
            totmassd = 0.0_fp ! total deposited mass
            totsvd   = 0.0_fp ! total deposited volume
            totsve   = 0.0_fp ! total eroded volume
            totsv    = 0.0_fp ! total sediment volume
            do l = 1, this%settings%nfrac
                if (dbodsd(l,nm) > 0.0_fp) then
                    ! fraction being deposited
                    totmassd = totmassd + dbodsd(l,nm)
                    totsvd   = totsvd   + dbodsd(l,nm) / rhofrac(l)
                else
                    ! fraction being eroded
                    totsve   = totsve   - dbodsd(l,nm) / rhofrac(l)
                endif
                !
                temp  = msed(l, 1, nm) + dbodsd(l, nm)
                if (temp < 0.0_fp) then
                   if (temp < -morlyrnum%mass_shortage_thresh .and. morlyrnum%max_num_shortage_warnings>0) then
                      morlyrnum%max_num_shortage_warnings = morlyrnum%max_num_shortage_warnings - 1
                        write(message,'(a,i5,a,i3,a,e20.4,a,e20.4)') &
                           & 'Sediment erosion shortage at NM ', nm, ' Fraction: ', l, &
                           & ' Mass available   : ' ,msed(l, 1, nm), &
                           & ' Mass to be eroded: ', dbodsd(l, nm)
                        call addmessage(messages,message)
                      if (morlyrnum%max_num_shortage_warnings == 0) then
                            message = 'Sediment erosion shortage messages suppressed'
                            call addmessage(messages,message)
                        endif
                    endif
                    if (track_shortage) then
                        sedshort(l, nm) = sedshort(l, nm) + temp
                    endif
                    temp = 0.0_fp
                elseif ( sedshort(l, nm) < 0.0_fp ) then
                    temp = temp + sedshort(l, nm)
                    if ( temp < 0.0_fp ) then
                        sedshort(l, nm) = temp
                        temp = 0.0_fp
                    else
                        sedshort(l, nm) = 0.0_fp
                    endif
                endif
                msed(l, 1, nm) = temp
                totsv = totsv + temp / rhofrac(l)
            enddo
            !
            ! get new requested transport layer thickness.
            !
            thtrlyrnew = thtrlyr(nm)
            !
            ! compute actual current thickness of top layer
            !
            if (this%settings%updtoplyr == TOPLYR_POR_RESET) then
                ! thickness of top layer based on the porosity
                ! formula for the complete mixture of sediment
                ! irrespective age (i.e. freshly deposited or
                ! remnant of previous top layer composition).
                if (iconsolidate == CONSOL_NONE) then
                    call updateporosity(this, nm, 1)
                endif
                thick = totsv/svfrac(1, nm)
            else
                ! thickness of transport layer based on a combination
                ! of freshly deposited sediment (using porosity formula)
                ! and the remainder of the original top layer.

                ! reduce thickness by eroded volume
                thicke = totsve / svfrac(1,nm)
                thick = thlyr(1,nm) - thicke

                if (totmassd > 0.0_fp) then
                    ! some deposition occurred (maybe also some erosion)
                    ! determine porosity and thickness of added mixture
                    do l = 1, this%settings%nfrac
                        if (dbodsd(l,nm) > 0.0_fp) then
                            mfrac(l) = dbodsd(l,nm) / totmassd
                        else
                            mfrac(l) = 0.0_fp
                        endif
                    enddo
                    call getporosity(this, mfrac, poros)
                    svfracd = 1.0_fp - poros
                    thickd = totsvd / svfracd
                    
                    ! new sediment comes without preload history
                    preload0 = 0.0_fp
                    td0 = real(morft,fp)
                                    
                    ! some deposition (maybe also some erosion)
                    preload(1,nm) = (thick * preload(1,nm) + thickd * preload0) / (thick + thickd)
                    td(1,nm)      = (thick * td(1,nm) + thickd * td0) / (thick + thickd)
                    svfrac(1,nm)  = (thick * svfrac(1,nm) + thickd * svfracd) / (thick + thickd)
                    !
                    ! new transport layer thickness takes into account deposition
                    !
                    thick = thick + thickd
                else
                    !
                    ! only erosion … preload and svfrac don’t need updating
                    !
                endif
            endif
            !
            if (iconsolidate == CONSOL_DECON) then
                !
                ! In case of Dynamic Equilibrium CONsolidation (DECON), we erode the top layer until
                ! it runs out of sediment and then push the administration of the top NCONLYR layers
                ! up. In case of sedimentation, the sediment is added to the top layer and
                ! redistributed over the top NCONLYR layers once every DTDECON.
                !
                if (thick < thtrempty) then
                    !
                    ! The top layer is eroded almost completely.
                    ! Check if there is still some sediment left in the consolidation layers
                    !
                    do k = 2,nconlyr
                         thick = thick + thlyr(k,nm)
                         if (thick > thtrempty) then
                             ktemp = k
                             exit
                         endif
                    enddo
                    !
                    ! If there is sufficient sediment left in the top KTEMP consolidation layers.
                    !
                    if (thick > thtrempty) then
                        !
                        ! Accumulate the contents of the top KTEMP layers such that the new top layer
                        ! is sufficiently thick.
                        !
                        cmudlyr(1,nm)  = cmudlyr(1,nm)*thlyr(1,nm)
                        csandlyr(1,nm) = csandlyr(1,nm)*thlyr(1,nm)
                        svfrac(1,nm)   = svfrac(1,nm)*thlyr(1,nm)
                        do k = 2,ktemp
                            do l = 1, this%settings%nfrac
                                msed(l,1,nm) = msed(l,1,nm) + msed(l,k,nm)
                            enddo
                            cmudlyr(1,nm)  = cmudlyr(1,nm)  + cmudlyr(k,nm)*thlyr(k,nm)
                            csandlyr(1,nm) = csandlyr(1,nm) + csandlyr(k,nm)*thlyr(k,nm)
                            svfrac(1,nm)   = svfrac(1,nm)   + svfrac(k,nm)*thlyr(k,nm)
                        enddo
                        cmudlyr(1,nm)  = cmudlyr(1,nm)/thick
                        csandlyr(1,nm) = csandlyr(1,nm)/thick
                        svfrac(1,nm)   = svfrac(1,nm)/thick
                        thlyr(1,nm) = thick
                        
                        ! Shift the content of the other consolidation layers up.
                        do k = ktemp+1,nconlyr
                            kk = k-ktemp+1
                            do l = 1, this%settings%nfrac
                                msed(l,kk,nm) = msed(l,k, nm)
                            enddo
                            cmudlyr(kk,nm) = cmudlyr(k+1,nm) 
                            csandlyr(kk,nm) = csandlyr(k+1,nm) 
                            svfrac(kk,nm) = svfrac(k+1,nm)
                            thlyr(kk,nm) = thlyr(k+1,nm)
                        enddo
                        
                        ! Erase the administration of the bottom-most consolidation layers.
                        do k = nconlyr-ktemp+2, nconlyr
                            msed(:,k,nm) = 0.0_fp
                            cmudlyr(k,nm) = 0.0_fp
                            csandlyr(k,nm) = 0.0_fp
                            svfrac(k,nm) = 0.0_fp
                            thlyr(k,nm) = 0.0_fp
                        enddo
                    endif
                endif
                
                ! Don't replenish the consolidation layers while there is still sediment in the consolidation layers
                if (thick > thtrempty) then
                   thtrlyrnew = thick  
                endif
            endif
            thdiff = thick-thtrlyrnew
            !
            ! get sediment from or put sediment into underlayers
            ! to get transport layer of requested thickness
            !
            if ( thdiff > 0.0_fp ) then
               !   
               ! sedimentation to underlayers
               ! determine surplus of mass per fraction
               ! 
               fac = thdiff/thick
               sdbodsed = 0.0_fp
               do l = 1, this%settings%nfrac
                   dmi(l) = msed(l, 1, nm)*fac
                   msed(l, 1, nm) = msed(l, 1, nm) - dmi(l)
                   sdbodsed = sdbodsed + dbodsd(l, nm)
               enddo
               !
               ! store surplus of mass in underlayers
               !
               call lyrsedimentation(this , nm, thdiff, dmi, svfrac(1, nm), sdbodsed, td(1, nm))
               !
            elseif ( thdiff < 0.0_fp ) then
               !
               ! erosion of underlayers
               ! total erosion thickness: thdiff
               ! associated mass returned in: dmi
               !
               thdiff = -thdiff
               !  
               call lyrerosion(this , nm, thdiff, dmi) ! TODO: get porosity, preload and td
               !
               ! add to top layer
               ! 
               do l = 1, this%settings%nfrac 
                   msed(l, 1, nm)   = msed(l, 1, nm) + dmi(l)
               enddo
               !
               do l = 1, this%settings%nfrac
                   if (sedshort(l, nm) < 0.0_fp .and. msed(l, 1, nm) > 0.0_fp) then
                       sedshort(l, nm) = sedshort(l, nm) + msed(l, 1, nm)
                       if (sedshort(l, nm) > 0.0_fp) then
                           msed(l, 1, nm)  = sedshort(l, nm)
                           sedshort(l, nm) = 0.0_fp
                       else
                           msed(l, 1, nm) = 0.0_fp
                       endif
                   endif
               enddo
               !
               if (iconsolidate == CONSOL_NONE) then
                   call updateporosity(this, nm, 1)
               endif
               thick = 0.0_fp
               do l = 1, this%settings%nfrac
                   thick = thick + msed(l, 1, nm)/rhofrac(l)
               enddo
               thick = thick/svfrac(1, nm)
               !
               mmudlyr1  = 0.0_fp
               msandlyr1 = 0.0_fp
               do l = 1, this%settings%nfrac
                   if (this%settings%sedtyp(l) <= this%settings%max_mud_sedtyp) then
                       mmudlyr1   = mmudlyr1 + msed(l,1,nm)
                   else
                       msandlyr1   = msandlyr1 + msed(l,1,nm)
                   endif
               enddo
               cmudlyr(1,nm)  = mmudlyr1/thick
               csandlyr(1,nm) = msandlyr1/thick
               !
               ! if there is not enough sediment in the bed then the actual
               ! thickness thick of the top layer may not reach the desired
               ! thickness thtrlyrnew, so we should here use thick as the
               ! thickness instead of thtrlyrnew
               !
               thtrlyrnew = thick
            endif
            thlyr(1, nm) = thtrlyrnew
            !
            if (call_consolidate) then
                call consolidate(this, nm, morft, dtmor)
            endif
            call getsedthick_1point(this, nm, seddep1)
            dz(nm) = seddep1-seddep0
        enddo
        
    case default ! BED_MIXED
        do nm = this%settings%nmlb,this%settings%nmub
            seddep0   = dpsed(nm)
            dpsed(nm) = 0.0_fp
            dz(nm)    = 0.0_fp
            do l = 1, this%settings%nfrac
                bodsed(l, nm) = bodsed(l, nm) + real(dbodsd(l, nm),prec)
                if (bodsed(l, nm) < 0.0_prec) then
                if (bodsed(l, nm) < real(-morlyrnum%mass_shortage_thresh,prec) .and. morlyrnum%max_num_shortage_warnings>0) then
                   morlyrnum%max_num_shortage_warnings = morlyrnum%max_num_shortage_warnings - 1
                        write(message,'(a,i0,a,i0,a,e20.4,a,e20.4)') &
                           & 'Sediment erosion shortage at NM ', nm, ' Fraction: ', l, &
                           & ' Mass available   : ' ,bodsed(l, nm), &
                           & ' Mass to be eroded: ', dbodsd(l, nm)
                        call addmessage(messages,message)
                   if (morlyrnum%max_num_shortage_warnings == 0) then
                            message = 'Sediment erosion shortage messages suppressed'
                            call addmessage(messages,message)
                        endif
                    endif
                if (track_shortage) then
                   sedshort(l, nm) = sedshort(l, nm) + bodsed(l, nm)
                endif
                    bodsed(l, nm) = 0.0_prec
             elseif (sedshort(l, nm) < 0.0_fp .and. bodsed(l, nm) > 0.0_prec) then
                bodsed(l, nm) = bodsed(l, nm) + real(sedshort(l, nm),prec)
                if (bodsed(l, nm) > 0.0_prec) then
                    sedshort(l, nm) = 0.0_fp
                else
                    sedshort(l, nm) = real(bodsed(l, nm), fp)
                    bodsed(l, nm) = 0.0_prec
                endif
             endif
                dpsed(nm) = dpsed(nm) + real(bodsed(l, nm),fp)/rhofrac(l)
            enddo    
            dz(nm) = dpsed(nm) - seddep0
        enddo
    endselect
    istat = deallocwork(this)
end function updmorlyr


!> Retrieve the sediment in the top layer (thickness dz_eros in m) from the
!! underlayer bookkeeping system and update the administration.
function gettoplyr(this, dz_eros, dbodsd, messages  ) result (istat)
    use precision
    use message_module
    !
    implicit none
    !
    ! Function/routine arguments
    !
    type(bedcomp_data)                                                                           :: this     !< bed composition object
    real(fp), dimension(this%settings%nfrac, this%settings%nmlb:this%settings%nmub), intent(out) :: dbodsd  !  change in sediment composition, units : kg/m2
    real(fp), dimension(this%settings%nmlb:this%settings%nmub)                     , intent(in)  :: dz_eros !  change in sediment thickness, units : m
    type(message_stack)                                                                          :: messages
    integer                                                                                      :: istat
    !
    ! Local variables
    !
    integer                                 :: k
    integer                                 :: l
    integer                                 :: nm
    real(fp)                                :: dz
    real(fp)                                :: fac
    real(fp)                                :: thtrlyrnew
    real(fp)                                :: thick
    real(fp), dimension(this%settings%nfrac):: dmi
    real(fp)                                :: sdbodsed !the sum of dbodsd	
    real(fp)                                :: dz_togo
    !
    character(message_len)                  :: message
    real(prec) , dimension(:,:)   , pointer :: bodsed
    real(fp)   , dimension(:,:)   , pointer :: svfrac
    real(fp)   , dimension(:)     , pointer :: dpsed
    real(fp)   , dimension(:,:,:) , pointer :: msed
    real(fp)   , dimension(:,:)   , pointer :: sedshort 
    real(fp)   , dimension(:)     , pointer :: rhofrac
    real(fp)   , dimension(:,:)   , pointer :: thlyr
    real(fp)   , dimension(:)     , pointer :: thtrlyr
    real(fp)   , dimension(:,:)   , pointer :: td
    !
    !! executable statements -------------------------------------------------------
    !
    thtrlyr     => this%settings%thtrlyr
    rhofrac     => this%settings%rhofrac
    svfrac      => this%state%svfrac
    bodsed      => this%state%bodsed
    dpsed       => this%state%dpsed
    msed        => this%state%msed
    sedshort    => this%state%sedshort
    thlyr       => this%state%thlyr
    td          => this%state%td
    !
    istat = allocwork(this)
    if (istat /= 0) return
    select case (this%settings%iunderlyr)
    case (BED_LAYERED)
       do nm = this%settings%nmlb,this%settings%nmub
          if ( dz_eros(nm) < 0.0_fp ) then
             !
             ! erosion should not be negative
             !
             message = 'Negative dz_eros encountered'
             call adderror(messages,message)
             istat = -1
             return
          elseif (comparereal(dz_eros(nm),0.0_fp) == 0) then
             !
             ! skip it
             !
             do l = 1, this%settings%nfrac
                dbodsd (l, nm)    = 0.0_fp
             enddo
          else
             if (dz_eros(nm) <= thlyr(1, nm)) then
                !
                ! erosion less than transport layer thickness
                !
                thick = thlyr(1, nm) - dz_eros(nm)
                fac = dz_eros(nm)/thlyr(1, nm)
                do l = 1, this%settings%nfrac
                   dbodsd (l, nm) = msed(l, 1, nm)*fac
                   msed(l, 1, nm) = msed(l, 1, nm) - dbodsd(l, nm)
                enddo
             else
                !
                ! erosion more than transport layer thickness
                !
                do l = 1, this%settings%nfrac
                   dbodsd (l, nm) = msed(l, 1, nm)
                   msed(l, 1, nm) = 0.0_fp
                enddo
                dz_togo = dz_eros(nm)-thlyr(1, nm)
                thick  = 0.0_fp
                !
                ! get remaining dz_togo from underlayers
                ! get dmi from underlayers 
                !
                call lyrerosion(this , nm, dz_togo, dmi)
                !
                do l = 1, this%settings%nfrac
                   dbodsd(l, nm) = dbodsd(l, nm) + dmi(l)
                enddo
             endif
             !
             ! get new transport layer thickness.
             !
             thtrlyrnew = thtrlyr(nm)
             !
             ! compute new thickness of top layer
             ! get sediment from or put sediment into underlayers
             ! to get transport layer of requested thickness
             !
             dz = thick-thtrlyrnew
             !
             if ( dz > 0.0_fp ) then
                !   
                ! sedimentation to underlayers
                ! 
                fac = dz/thick
                sdbodsed = 0.0_fp
                do l = 1, this%settings%nfrac
                    dmi(l) = msed(l, 1, nm)*fac
                    msed(l, 1, nm) = msed(l, 1, nm) - dmi(l)
                    sdbodsed = sdbodsed + dbodsd(l, nm)
                enddo   
                !
                ! store surplus of mass in underlayers
                !
                call lyrsedimentation(this , nm, dz, dmi, svfrac(1, nm), sdbodsed, td(1, nm))
                !
             elseif ( dz < 0.0_fp ) then
                !
                ! erosion of underlayers
                !
                dz = -dz
                call lyrerosion(this , nm, dz, dmi) ! TODO: might also get porosity, preload, td
                !
                ! add to top layer
                !  
                do l = 1, this%settings%nfrac 
                    msed(l, 1, nm)   = msed(l, 1, nm) + dmi(l) 
                enddo
                !
                do l = 1, this%settings%nfrac
                    if (sedshort(l, nm) < 0.0_fp .and. msed(l, 1, nm) > 0.0_fp) then
                        sedshort(l, nm) = sedshort(l, nm) + msed(l, 1, nm)
                        if (sedshort(l, nm) > 0.0_fp) then
                            msed(l, 1, nm)  = sedshort(l, nm)
                            sedshort(l, nm) = 0.0_fp
                        else
                            msed(l, 1, nm) = 0.0_fp
                        endif
                    endif
                enddo
                !
                call updateporosity(this, nm, 1)
                thick = 0.0_fp
                do l = 1, this%settings%nfrac
                    thick = thick + msed(l, 1, nm)/rhofrac(l)
                enddo
                thick = thick/svfrac(1, nm)
                !
                ! if there is not enough sediment in the bed then the actual
                ! thickness thick of the top layer may not reach the desired
                ! thickness thtrlyrnew, so we should here use thick as the
                ! thickness instead of thtrlyrnew
                !
                thtrlyrnew = thick
             endif
             thlyr(1, nm) = thtrlyrnew
          endif
       enddo

    case default ! BED_MIXED
       do nm = this%settings%nmlb,this%settings%nmub
          if (dz_eros(nm)<0.0_fp) then
             !
             ! erosion should not be negative
             !
             message = 'Negative dz_eros encountered'
             call adderror(messages,message)
             istat = -1
             return
          elseif (comparereal(dz_eros(nm),0.0_fp) == 0 .or. dpsed(nm)<=0.0_fp) then
             !
             ! skip it
             !
             do l = 1, this%settings%nfrac
                dbodsd (l, nm)    = 0.0_fp
             enddo
          elseif (dz_eros(nm) < dpsed(nm)) then
             !
             ! some sediment remaining
             !
             fac = dz_eros(nm)/dpsed(nm)
             dpsed(nm) = 0.0_fp
             do l = 1, this%settings%nfrac
                dbodsd(l, nm) = real(bodsed(l, nm),fp)*fac
                bodsed(l, nm) = bodsed(l, nm) - real(dbodsd(l, nm),prec)
                dpsed (nm)    = dpsed(nm) + real(bodsed(l, nm),fp)/rhofrac(l)
             enddo
          else
             !
             ! no sediment remaining
             !
             dpsed(nm) = 0.0_fp
             do l = 1, this%settings%nfrac
                dbodsd(l, nm) = real(bodsed(l, nm),fp)
                bodsed(l, nm) = 0.0_hp
             enddo
          endif
       enddo
    endselect
    istat = deallocwork(this)
end function gettoplyr


!> lyrerosion implements the erosion of sediment from the layers below the
!! transport and exchange layers
subroutine lyrerosion(this, nm, dzini, dmi) ! TODO: may collect porosity, preload and td information as well
    use precision
    !
    implicit none
    !
    ! Function/routine arguments
    !
    type(bedcomp_data)                                    :: this    !< bed composition object
    integer                                  , intent(in) :: nm
    real(fp)                                 , intent(in) :: dzini   !  thickness of eroded layer, units : m
    real(fp), dimension(this%settings%nfrac) , intent(out):: dmi     !  density of sediment fractions, units : kg/m3
    !
    ! Local variables
    !
    logical                                            :: remove
    integer                                            :: k
    integer                                            :: kero1  ! top-most layer that has been (partially) eroded
    integer                                            :: l
    real(fp)                                           :: dz
    real(fp)                                           :: dm
    real(fp)                                           :: fac
    real(fp)                                           :: thick
    real(fp)                                           :: thbaselyr
    real(fp), dimension(this%settings%nfrac)           :: mbaselyr  
    real(fp)                                 , pointer :: thlalyr
    integer                                  , pointer :: keuler 
    integer                                  , pointer :: nlyr
    integer                                  , pointer :: updbaselyr
    real(fp), dimension(:,:)                 , pointer :: svfrac
    real(fp), dimension(:,:,:)               , pointer :: msed
    real(fp), dimension(:,:)                 , pointer :: thlyr
    integer                                  , pointer :: peatfrac
    real(fp)                                           :: mpeat
    real(fp), dimension(:,:)                 , pointer :: preload
    real(fp), dimension(:,:)                 , pointer :: td
!
!! executable statements -------------------------------------------------------
!
    keuler      => this%settings%keuler
    nlyr        => this%settings%nlyr
    thlalyr     => this%settings%thlalyr
    updbaselyr  => this%settings%updbaselyr
    peatfrac    => this%settings%peatfrac
    svfrac      => this%state%svfrac
    msed        => this%state%msed
    thlyr       => this%state%thlyr
    preload     => this%state%preload
    td          => this%state%td
    !
    k   = 2
    if (this%settings%exchlyr) k = 3
    !
    thbaselyr = thlyr(nlyr, nm)
    mbaselyr  = msed(:, nlyr, nm)
    dmi = 0.0_fp
    dz  = dzini
    !
    ! initially remove sediment irrespective of layer type
    ! then fill the Lagrangian layers again up to their
    ! original thickness
    ! kero1 represents the Lagrangian layer that was eroded and needs
    ! to be replenished
    ! remove indicates that sediment should be eroded (stored in dmi)
    ! rather than shifted to another Lagrangian layer
    !
    kero1 = k-1
    remove = .true.
    do while (dz>0.0_fp .and. k<=nlyr)
        mpeat = 0.0_fp
        if (peatfrac>0) then
           mpeat = msed(peatfrac, k, nm)
        endif
        if (mpeat>0.0_fp) then
            ! don't erode into a peat layer with non-zero thickness
            ! and not any layer below it
            exit
        elseif ( thlyr(k, nm) < dz ) then
            !
            ! more sediment is needed than there is available in layer
            ! k, so all sediment should be removed from this layer
            !          
            do l = 1, this%settings%nfrac
                if (remove) then
                   dmi(l) = dmi(l) + msed(l, k, nm)
                else
                   msed(l, kero1, nm) = msed(l, kero1, nm) + msed(l, k, nm)
                endif
                msed(l, k, nm) = 0.0_fp
            enddo
            dz          = dz - thlyr(k, nm)
            if (.not.remove) then
               svfrac(kero1, nm) = svfrac(kero1, nm)*thlyr(kero1, nm) + svfrac(k, nm)*thlyr(k, nm)
               preload(kero1,nm) = preload(kero1,nm)*thlyr(kero1, nm) + preload(k,nm)*thlyr(k, nm)
               td(kero1,nm)      = td(kero1,nm)*thlyr(kero1, nm) + td(k,nm)*thlyr(k, nm)
               thlyr(kero1, nm)  = thlyr(kero1, nm) + thlyr(k, nm)
               svfrac(kero1, nm) = svfrac(kero1, nm)/thlyr(kero1, nm)
               preload(kero1,nm) = preload(kero1,nm)/thlyr(kero1,nm)
               td(kero1,nm)      = td(kero1,nm)/thlyr(kero1,nm)
            endif
            thlyr(k, nm) = 0.0_fp
            k           = k+1
        else ! thlyr(k)>=dz
            !
            ! layer k contains more sediment than is needed, so only part
            ! of the sediment has to be removed from the layer
            !
            fac = dz/thlyr(k, nm)
            do l = 1, this%settings%nfrac
                dm = msed(l, k, nm)*fac
                if (remove) then
                   dmi(l)             = dmi(l) + dm 
                else
                   msed(l, kero1, nm) = msed(l, kero1, nm) + dm
                endif
                msed(l, k, nm) = msed(l, k, nm) - dm
            enddo
            thlyr(k, nm)       = thlyr(k, nm)   - dz
            if (.not.remove) then
               svfrac(kero1, nm) = svfrac(kero1, nm)*thlyr(kero1, nm) + svfrac(k, nm)*dz
               preload(kero1,nm) = preload(kero1,nm)*thlyr(kero1, nm) + preload(k,nm)*dz
               td(kero1,nm)      = td(kero1,nm)*thlyr(kero1, nm) + td(k,nm)*dz
               thlyr(kero1, nm)  = thlyr(kero1, nm) + dz
               svfrac(kero1, nm) = svfrac(kero1, nm)/thlyr(kero1, nm)
               preload(kero1,nm) = preload(kero1,nm)/thlyr(kero1,nm)
               td(kero1,nm)      = td(kero1,nm)/thlyr(kero1,nm)
            endif
            !
            ! erosion complete (dz=0) now continue to replenish the
            ! (partially) eroded Lagrangian layers as long as
            ! sediment is available in lower layers. Note that the
            ! Eulerian layers don't get replenished.
            !
            kero1 = kero1+1
            remove = .false.
            !
            ! do we have to fill again some of the Lagrangian layers?
            !
            if (kero1<keuler) then
                dz = max(thlalyr - thlyr(kero1, nm),0.0_fp)
                k = max(k,kero1+1)
            else
                dz = 0.0_fp
            endif
        endif
    enddo
    !
    ! update composition of base layer
    !
    select case (updbaselyr)
    case(BASELYR_UPDATED) ! compute separate composition for base layer
       !
       ! no change necessary
       !
    
    case(BASELYR_CONST_FRC) ! composition of base layer constant
       !
       ! compute new masses based on old composition and new thickness
       ! Problem of current implementation:
       ! if the base layer runs out of sediment once (thlyr(nlyr,nm) -> 0),
       ! it looses the information on the composition and cannot recover.
       !
       if (thbaselyr>0.0_fp) then
          fac = thlyr(nlyr, nm)/thbaselyr
       else
          fac = 0.0_fp
       endif
       do l = 1, this%settings%nfrac
          msed(l, nlyr, nm) = mbaselyr(l)*fac
       enddo
    
    case(BASELYR_COPY_FRC) ! same as the (first non-empty) layer above it
       !
       ! find lowest non-empty layer
       !
       do k = nlyr-1,1,-1
          if ( thlyr(k, nm) > 0.0_fp ) exit
       enddo
       fac = thlyr(nlyr, nm)/thlyr(k, nm)
       do l = 1, this%settings%nfrac
          msed(l, nlyr, nm) = msed(l, k, nm)*fac
       enddo
    
    case(BASELYR_CONST) ! composition and thickness of base layer constant
       !
       ! reset thickness and masses
       !
       thlyr(nlyr, nm)  = thbaselyr
       msed(:, nlyr, nm) = mbaselyr
    
    case(BASELYR_CONST_THK) ! composition updated, but thickness unchanged
       !
       ! reset thickness and correct mass
       !
       if (thlyr(nlyr, nm)>0.0_fp) then
          fac = thbaselyr/thlyr(nlyr, nm)
          do l = 1, this%settings%nfrac
             msed(l, nlyr, nm) = msed(l, nlyr, nm)*fac
          enddo
       else
          msed(:, nlyr, nm) = mbaselyr
       endif
       thlyr(nlyr, nm)  = thbaselyr
    
    case default
       !
       ! ERROR
       !
    endselect
end subroutine lyrerosion


!> lyrsedimentation implements the deposition of sediment in the layers
!! below the transport and exchange layers
subroutine lyrsedimentation(this, nm, dzini, dmi, svfracdep, preloaddep, tddep)
    use precision
    !
    implicit none
    !
!
! Function/routine arguments
!
    type(bedcomp_data)                                    :: this     !< bed composition object
    integer                                  , intent(in) :: nm
    real(fp)                                 , intent(in) :: dzini
    real(fp)                                 , intent(in) :: svfracdep
    real(fp)                                 , intent(in) :: preloaddep
    real(fp)                                 , intent(in) :: tddep
    real(fp), dimension(this%settings%nfrac)              :: dmi
!
! Local variables
!
    integer                                     :: k
    integer                                     :: k2
    integer                                     :: kmin
    integer                                     :: kne
    integer                                     :: l
    real(fp)                                    :: depfrac
    real(fp)                                    :: dm
    real(fp)                                    :: dz
    real(fp)                                    :: dz2
    real(fp)                                    :: dzc
    real(fp)                                    :: dzk
    real(fp)                                    :: dzlmax
    real(fp)                                    :: fac
    real(fp)                                    :: newthlyr
    real(fp)                                    :: thick
    real(fp)                  , pointer         :: thlalyr
    integer                   , pointer         :: keuler
    integer                   , pointer         :: nlyr
    integer                   , pointer         :: updbaselyr
    real(fp), dimension(:,:)  , pointer         :: svfrac
    real(fp), dimension(:,:,:), pointer         :: msed
    real(fp), dimension(:,:)  , pointer         :: thlyr
    real(fp), dimension(this%settings%nfrac)    :: dmi2
    real(fp)                                    :: load
    real(fp), dimension(:,:)  , pointer         :: preload
    real(fp), dimension(:,:)  , pointer         :: td
    real(fp)                                    :: temp
    type(bedcomp_work)        , pointer         :: work
!
!! executable statements -------------------------------------------------------
!
    thlalyr     => this%settings%thlalyr
    keuler      => this%settings%keuler
    nlyr        => this%settings%nlyr
    updbaselyr  => this%settings%updbaselyr
    svfrac      => this%state%svfrac
    msed        => this%state%msed
    thlyr       => this%state%thlyr
    preload     => this%state%preload
    td          => this%state%td
    work        => this%work
    !
    kmin = 2
    if (this%settings%exchlyr) kmin = 3
    dz = dzini
    !
    ! copy Lagrangian layer data to temporary array
    !
    load = 0.0_fp
    do k = kmin,keuler-1
       do l = 1, this%settings%nfrac
          work%msed2(l, k) = msed(l, k, nm)
          msed(l, k, nm)   = 0.0_fp
       enddo
       work%svfrac2(k)  = svfrac(k, nm)
       work%thlyr2(k)   = thlyr(k, nm)
       work%preload2(k) = preload(k, nm)
       work%td2(k)      = td(k, nm)
       thlyr(k, nm)     = 0.0_fp
    enddo
    !
    ! fill the Lagrangian layers from top to bottom
    !
    do k = kmin,keuler-1
       !
       ! while there is newly deposited sediment use that to fill the layers
       !
       if (dz>thlalyr) then
          !
          ! sufficient newly deposited sediment to fill whole layer
          !
          fac = thlalyr/dz
          do l = 1, this%settings%nfrac
             dm = dmi(l) * fac
             msed(l, k, nm) = dm
             dmi(l) = dmi(l) - dm
          enddo
          svfrac(k, nm)  = svfracdep
          preload(k, nm) = preloaddep
          td(k,nm)       = tddep
          thlyr(k, nm)   = thlalyr
          dz             = dz - thlalyr
       elseif (dz>0.0_fp) then
          !
          ! last bit of newly deposited sediment to partially fill the layer
          !
          do l = 1, this%settings%nfrac
             msed(l, k, nm) = dmi(l)
             dmi(l) = 0.0_fp
          enddo
          svfrac(k, nm)  = svfracdep
          preload(k, nm) = preloaddep
          td(k, nm)      = tddep
          thlyr(k, nm)   = dz
          dz             = 0.0_fp
       endif
       !
       ! as long as there is still space in this layer, fill it with sediment
       ! from the old Lagrangian layers
       !
       k2  = kmin
       dzc = thlalyr - thlyr(k, nm)
       do while (k2<keuler .and. dzc > 0.0_fp)
          !
          ! did this Lagrangian layer contain sediment?
          !
          if (work%thlyr2(k2)>0.0_fp) then
             if (dzc<work%thlyr2(k2)) then
                !
                ! sufficient sediment left in old layer k2 to fill the
                ! remainder of new layer k
                !
                fac = dzc/work%thlyr2(k2)
                do l = 1, this%settings%nfrac
                   dm = work%msed2(l, k2) * fac
                   msed(l, k, nm)    = msed(l, k, nm)    + dm
                   work%msed2(l, k2) = work%msed2(l, k2) - dm
                enddo
                svfrac(k, nm)   = svfrac(k, nm)*thlyr(k, nm) + work%svfrac2(k2)*dzc
                preload(k, nm)  = preload(k, nm)*thlyr(k, nm) + work%preload2(k2)*dzc
                td(k, nm)       = td(k, nm)*thlyr(k, nm) + work%td2(k2)*dzc
                thlyr(k, nm)    = thlalyr
                svfrac(k, nm)   = svfrac(k, nm)/thlyr(k, nm)
                preload(k, nm)  = preload(k, nm) / thlyr(k, nm)
                td(k, nm)       = td(k, nm) / thlyr(k, nm)
                work%thlyr2(k2) = work%thlyr2(k2) - dzc
             else
                !
                ! all the sediment of old layer k2 fits into new layer k
                !
                do l = 1, this%settings%nfrac
                   msed(l, k, nm)    = msed(l, k, nm) + work%msed2(l, k2)
                   work%msed2(l, k2) = 0.0_fp
                enddo
                svfrac(k, nm)   = svfrac(k, nm)*thlyr(k, nm) + work%svfrac2(k2)*work%thlyr2(k2)
                preload(k, nm)  = preload(k, nm)*thlyr(k, nm) + work%preload2(k2)*work%thlyr2(k2)
                td(k, nm)       = td(k, nm)*thlyr(k, nm) + work%td2(k2)*work%thlyr2(k2)
                thlyr(k, nm)    = thlyr(k, nm) + work%thlyr2(k2)
                svfrac(k, nm)   = svfrac(k, nm)/thlyr(k, nm)
                preload(k, nm)  = preload(k, nm) / thlyr(k, nm)
                td(k, nm)       = td(k, nm) / thlyr(k, nm)
                work%thlyr2(k2) = 0.0_fp
             endif
             dzc = thlalyr - thlyr(k, nm)
          endif
          k2 = k2+1
       enddo
    enddo
    !
    ! if there is any sediment left in the old Lagrangian layers then move it
    ! to the Eulerian layers below
    !
    do k2 = keuler-1,kmin,-1
       if (work%thlyr2(k2)>0.0_fp) then
          do l = 1, this%settings%nfrac
             dmi2(l) = work%msed2(l, k2)
          enddo
          call lyrsedimentation_eulerian(this, nm, work%thlyr2(k2), dmi2, work%svfrac2(k2), work%preload2(k2), work%td2(k2))
       endif
    enddo
    !
    ! and finally if there is any sediment left in the original deposit that
    ! came from the active layer(s) then deposit that sediment
    !
    if (dz>0.0_fp) then
        do l = 1, this%settings%nfrac
             load = load + dmi(l)                   !Aulia
        enddo
        call lyrsedimentation_eulerian(this, nm, dz, dmi, svfracdep, preloaddep, tddep) !Aulia: newly deposited sediment has no preload, so if there is an excess mass, will be used to constitute preload of preload of eulerian
        load = 0.0_fp                               !Aulia: for each k and nt, load is different.
    endif    
end subroutine lyrsedimentation


!> lyrsedimentation_eulerian implements the deposition of sediment in the 
!! Eulerian layers below the transport, exchange and other Lagrangian
!! layers
subroutine lyrsedimentation_eulerian(this, nm, dzini, dmi, svfracdep, preloaddep, tddep)
    use precision
    !
    implicit none
    !
!
! Function/routine arguments
!
    type(bedcomp_data)                                    :: this     !< bed composition object
    integer                                  , intent(in) :: nm
    real(fp)                                 , intent(in) :: dzini
    real(fp)                                 , intent(in) :: svfracdep
    real(fp)                                 , intent(in) :: preloaddep
    real(fp)                                 , intent(in) :: tddep
    real(fp), dimension(this%settings%nfrac)              :: dmi
!
! Local variables
!
    integer                                     :: k
    integer                                     :: kne
    integer                                     :: l

    real(fp)                                    :: depfrac
    real(fp)                                    :: dm
    real(fp)                                    :: dz
    real(fp)                                    :: fac
    real(fp)                                    :: newthlyr
    real(fp)                                    :: thick
    real(fp)                  , pointer         :: theulyr
    integer                   , pointer         :: keuler
    integer                   , pointer         :: nlyr
    integer                   , pointer         :: updbaselyr
    real(fp), dimension(:,:)  , pointer         :: svfrac
    real(fp), dimension(:,:,:), pointer         :: msed
    real(fp), dimension(:,:)  , pointer         :: thlyr
    real(fp), dimension(:,:)  , pointer         :: preload
    real(fp), dimension(:,:)  , pointer         :: td
!
!! executable statements -------------------------------------------------------
!
    theulyr     => this%settings%theulyr
    keuler      => this%settings%keuler
    nlyr        => this%settings%nlyr
    updbaselyr  => this%settings%updbaselyr
    svfrac      => this%state%svfrac
    msed        => this%state%msed
    thlyr       => this%state%thlyr
    preload     => this%state%preload
    td          => this%state%td
    !
    dz = dzini
    !
    ! find first (partially) filled underlayer
    !
    k = keuler
    do while (comparereal(thlyr(k, nm),0.0_fp)==0 .and. k<nlyr)
       k = k+1
    enddo
    !
    ! don't fill the last underlayer unless it's empty
    !
    if (k == nlyr .and. thlyr(k, nm)>0.0_fp) k = k-1
    !
    ! start filling upwards
    !
    do while ( k>=keuler .and. dz > 0.0_fp )
       if ( thlyr(k, nm) < theulyr ) then
          !
          ! sediment can be added to this layer
          !
          if ( dz > theulyr-thlyr(k, nm) .and. thlyr(k, nm)>0.0_fp ) then
             !
             ! not all sediment can be added to this layer
             !
             fac     = (theulyr-thlyr(k, nm))/dz
             dz      = dz*(1.0_fp-fac)
             do l = 1, this%settings%nfrac
                dm = dmi(l)*fac
                msed(l, k, nm) = msed(l, k, nm) + dm
                dmi(l)         = dmi(l)         - dm
             enddo
             svfrac(k, nm)  = svfrac(k, nm)*thlyr(k, nm) + svfracdep*(theulyr-thlyr(k, nm))
             preload(k, nm) = preload(k, nm)*thlyr(k, nm) + preloaddep*(theulyr-thlyr(k, nm))
             td(k, nm)      = td(k, nm)*thlyr(k, nm) + tddep*(theulyr-thlyr(k, nm))
             thlyr(k, nm)   = theulyr
             svfrac(k, nm)  = svfrac(k, nm) / thlyr(k, nm)
             preload(k, nm) = preload(k, nm) / thlyr (k, nm)
             td(k, nm)      = td(k, nm) / thlyr (k, nm)
          else
             !
             ! everything can be added to this layer
             !
             do l = 1, this%settings%nfrac            
                msed(l, k, nm) = msed(l, k, nm) + dmi(l)
                dmi(l) = 0.0_fp
             enddo
             svfrac(k, nm)  = svfrac(k, nm)*thlyr(k, nm) + svfracdep*dz
             preload(k, nm) = preload(k, nm)*thlyr(k, nm) + preloaddep*dz                       !Aulia
             td(k, nm)      = td(k, nm)*thlyr(k, nm) + tddep*dz
             thlyr(k, nm)   = thlyr(k, nm) + dz
             svfrac(k, nm)  = svfrac(k, nm) / thlyr(k, nm)
             preload(k, nm) = preload(k, nm) / thlyr (k, nm)
             td(k, nm)      = td(k, nm) / thlyr (k, nm)
             dz             = 0.0_fp
          endif
       endif
       k = k-1
    enddo
    !
    ! the first Eulerian underlayer is completely filled or
    ! all sediment deposited
    !
    if ( dz > 0.0_fp ) then
       !
       ! still more sediment to be deposited
       !
       if (keuler == nlyr) then
          !
          ! no Eulerian underlayers, so put everything in
          ! the last (i.e. base) layer
          !
          select case (updbaselyr)
          case(BASELYR_UPDATED) ! compute separate composition for the base layer
             do l = 1, this%settings%nfrac           
                msed(l, nlyr, nm) = msed(l, nlyr, nm) + dmi(l)
             enddo
             svfrac(nlyr, nm)  = svfrac(nlyr, nm)*thlyr(nlyr, nm) + svfracdep*dz
             preload(nlyr, nm) = preload(nlyr, nm)*thlyr(nlyr, nm) + preloaddep*dz              !Aulia
             td(nlyr, nm)      = td(nlyr, nm)*thlyr(nlyr, nm) + tddep*dz
             thlyr(nlyr, nm)   = thlyr(nlyr, nm) + dz
             svfrac(nlyr, nm)  = svfrac(nlyr, nm)/thlyr(nlyr, nm)
             preload(nlyr, nm) = preload(nlyr, nm)/thlyr(nlyr, nm)
             td(nlyr, nm)      = td(nlyr, nm)/thlyr(nlyr, nm)
             dz                = 0.0_fp
          
          case(BASELYR_CONST_FRC) ! composition of base layer constant
             !
             ! composition of dz is lost, update thickness
             !
             fac = (thlyr(nlyr, nm)+dz)/thlyr(nlyr, nm)
             do l = 1, this%settings%nfrac
                msed(l, nlyr, nm) = msed(l, nlyr, nm)*fac
             enddo
             thlyr(nlyr, nm) = thlyr(nlyr, nm) + dz
          
          case(BASELYR_COPY_FRC) ! same as the (first non-empty) layer above it
             !
             ! composition of dz is lost, update thickness
             ! and set composition to that of layer nlyr-1
             !
             do kne = nlyr-1,1,-1
                if ( thlyr(kne, nm) > 0.0_fp ) exit
             enddo
             thlyr(nlyr, nm) = thlyr(nlyr, nm) + dz
             fac = thlyr(nlyr, nm)/thlyr(kne, nm)
             do l = 1, this%settings%nfrac
                msed(l, nlyr, nm) = msed(l, kne, nm)*fac
             enddo
          
          case default
             !
             ! ERROR
             !
          endselect
       else
          !
          ! thin underlayers filled shift administration and deposit
          ! remaining sediment
          !
          do while ( dz > 0.0_fp )
             !
             ! add last thin underlayer to the last (i.e. base) layer
             !
             newthlyr = thlyr(nlyr, nm)+thlyr(nlyr-1, nm)
             select case (updbaselyr)
             case(BASELYR_UPDATED) ! compute separate composition for the base layer
                if ( newthlyr > 0.0_fp ) then
                   do l = 1, this%settings%nfrac
                      msed(l, nlyr, nm) = msed(l, nlyr, nm) + msed(l, nlyr-1, nm) 
                   enddo
                   svfrac(nlyr, nm)  = svfrac(nlyr, nm)*thlyr(nlyr, nm) + svfrac(nlyr-1, nm)*thlyr(nlyr-1, nm)
                   preload(nlyr, nm) = preload(nlyr, nm)*thlyr(nlyr, nm) + preload(nlyr-1, nm)*thlyr(nlyr-1, nm) !Aulia
                   td(nlyr, nm)      = td(nlyr, nm)*thlyr(nlyr, nm) + td(nlyr-1, nm)*thlyr(nlyr-1, nm)
                   svfrac(nlyr, nm)  = svfrac(nlyr, nm)/newthlyr
                   preload(nlyr, nm) = preload(nlyr, nm)/newthlyr
                   td(nlyr, nm)      = td(nlyr, nm)/newthlyr
                endif
             
             case(BASELYR_CONST_FRC) ! composition of base layer constant
                !
                ! composition of layer nlyr-1 is lost; just the
                ! thickness of the base layer is updated
                !
                fac = newthlyr/thlyr(nlyr, nm)
                do l = 1, this%settings%nfrac
                    msed(l, nlyr, nm) = msed(l, nlyr, nm)*fac
                enddo
             
             case(BASELYR_COPY_FRC) ! same as the (first non-empty) layer above it
                !
                ! find lowest non-empty layer
                !
                do kne = nlyr-2,1,-1
                   if ( thlyr(kne, nm) > 0.0_fp ) exit
                enddo
                fac = newthlyr/thlyr(kne, nm)
                do l = 1, this%settings%nfrac
                   msed(l, nlyr, nm) = msed(l, kne, nm)*fac
                enddo
             
             case(BASELYR_CONST) ! composition and thickness of base layer constant
                !
                ! composition and sediment of layer nlyr-1 is lost
                ! make sure that the newthlyr of the base layer is equal
                ! to the old thickness
                !
                newthlyr = thlyr(nlyr, nm)
             
             case default
                !
                ! ERROR
                !
             end select
             thlyr(nlyr, nm) = newthlyr
             !
             ! shift layers down by one
             !
             do k = nlyr-1,keuler+1,-1
                do l = 1, this%settings%nfrac
                   msed(l, k, nm) = msed(l, k-1, nm)
                enddo
                thlyr(k, nm)   = thlyr(k-1, nm)
                svfrac(k, nm)  = svfrac(k-1, nm)
                preload(k, nm) = preload(k-1, nm)
                td(k, nm)      = td(k-1, nm)
             enddo
             !
             ! put all the sediment in one layer
             ! Aulia: After space for deposition is created, excess sediment is deposited at layer keuler (top eulerian)
             !
             k = keuler
             do l = 1, this%settings%nfrac 
                 msed(l, k, nm) = dmi(l)
             enddo
             thlyr(k, nm)   = dz
             svfrac(k, nm)  = svfracdep
             preload(k, nm) = preloaddep
             td(k, nm)      = tddep
             dz             = 0.0_fp
          enddo
       endif
    endif
end subroutine lyrsedimentation_eulerian


!> lyrdiffusion implements the mixing between the layers through diffusion
subroutine lyrdiffusion(this, dt)
    use precision
    !
    implicit none
    !
    ! Function/routine arguments
    !
    type(bedcomp_data)                                    :: this     !< bed composition object
    real(fp)                                 , intent(in) :: dt 
    !
    ! Local variables
    !
    integer                                            :: k
    integer                                            :: l
    integer                                            :: nd
    integer                                            :: nlyrloc
    integer                                            :: nm
    real(fp)                                           :: flx
    real(fp)                                           :: kd
    real(fp)                                           :: pth
    real(fp)                                           :: zd
    integer                                  , pointer :: ndiff 
    integer                                  , pointer :: nlyr
    real(fp), dimension(:,:)                 , pointer :: a
    real(fp), dimension(:)                   , pointer :: rhofrac
    real(fp), dimension(:)                   , pointer :: zdiff
    real(fp), dimension(:,:)                 , pointer :: kdiff
    real(fp), dimension(:,:)                 , pointer :: svfrac
    real(fp), dimension(:,:)                 , pointer :: thlyr
    real(fp), dimension(:,:,:)               , pointer :: msed
    real(fp), dimension(:)                   , pointer :: svfrac2
    real(fp), dimension(:,:)                 , pointer :: msed2
!
!! executable statements -------------------------------------------------------
!
    if (this%settings%iunderlyr == BED_MIXED) return
    if (this%settings%idiffusion == BDIFF_NONE) return
    
    kdiff       => this%settings%kdiff
    ndiff       => this%settings%ndiff
    nlyr        => this%settings%nlyr
    rhofrac     => this%settings%rhofrac
    zdiff       => this%settings%zdiff
    svfrac      => this%state%svfrac
    msed        => this%state%msed
    thlyr       => this%state%thlyr
    msed2       => this%work%msed2
    svfrac2     => this%work%svfrac2
    allocate(a(-1:1,nlyr))
    do nm = this%settings%nmlb,this%settings%nmub
        nlyrloc = 0
        a       = 0.0_fp
        !
        zd = 0.0_fp         ! location of interface between the layers
        nd = 1              ! index of used diffusion coefficient
        ! 
        do k = 1, nlyr
            if (comparereal(thlyr(k,nm),0.0_fp) == 0) cycle
            nlyrloc = nlyrloc+1
            msed2(:,nlyrloc) = msed(:,k,nm)
            svfrac2(nlyrloc) = svfrac(k,nm)*thlyr(k,nm)
            !
            if (nlyrloc==1) then
                flx = 0.0_fp
            else
                !
                ! Compute diffusion coefficient at interface between layers
                ! through linear interpolation
                ! Extrapolation is performed by assuming constant values
                !
                do while (nd < ndiff .and. zdiff(nd) < zd )
                    nd = nd + 1
                enddo
                !
                if (nd == 1) then
                    kd = kdiff(nd, nm)
                elseif (nd <= ndiff .and. zd < zdiff(ndiff)) then
                    kd = kdiff(nd-1, nm) + (zd - zdiff(nd-1))/(zdiff(nd)-zdiff(nd-1)) * (kdiff(nd, nm) - kdiff(nd-1, nm))
                else
                    kd = kdiff(nd, nm)
                endif
                !
                flx = 0.5_fp * dt * kd/(pth+thlyr(k,nm))
            endif
            !
            if (nlyrloc>1) then
                a( 0,nlyrloc-1) = a( 0,nlyrloc-1) + flx/pth ! diff flux from k-1 to k
                a(+1,nlyrloc-1) = -flx/thlyr(k,nm)          ! diff flux from k to k-1
                a(-1,nlyrloc)   = -flx/pth                  ! diff flux from k-1 to k
                a( 0,nlyrloc)   = 1.0_fp + flx/thlyr(k,nm)  ! diff flux from k to k-1
            else
                !a(-1,1) = 0.0_fp
                a( 0,1) = 0.0_fp
            endif
            !
            zd                = zd+thlyr(k,nm)
            pth               = thlyr(k,nm) 
            thlyr(nlyrloc,nm) = thlyr(k,nm)
        enddo
        !
        ! double sweep - sweep down
        !
        do k = 1, nlyrloc
            if (k>1) then
                svfrac2(k) = svfrac2(k) - a(-1,k)*svfrac2(k-1)
                msed2(:,k) = msed2(:,k) - a(-1,k)*msed2(:,k-1)
                a( 0,k)    = a( 0,k)    - a(-1,k)*a(+1,k)
                !a(-1,k)    = 0.0_fp
            endif
            !
            svfrac2(k) = svfrac2(k)/a( 0,k)
            msed2(:,k) = msed2(:,k)/a( 0,k)
            a(+1,k)    = a(+1,k)   /a( 0,k)
            !a( 0,k)    = 1.0_fp
        enddo
        !
        ! double sweep - sweep up
        !
        do k = nlyrloc-1, 1, -1
            svfrac2(k) = svfrac2(k) - a(+1,k)*svfrac2(k+1)
            msed2(:,k) = msed2(:,k) - a(+1,k)*msed2(:,k+1)
            !a(+1,k)    = 0.0_fp
        enddo
        !
        do k = 1, nlyrloc
            msed(:,k,nm) = msed2(:,k)
            svfrac(k,nm) = svfrac2(k)/thlyr(k,nm)
        enddo
        do k = nlyrloc+1,nlyr
            svfrac(k,nm) = 1.0_fp
            msed(:,k,nm) = 0.0_fp
            thlyr(k,nm)  = 0.0_fp
        enddo
    enddo
    deallocate(a)
end subroutine lyrdiffusion


!> Determines the total thickness of mud fraction in the bed
!! DEPRECATED FUNCTIONALITY; use mudfrac (sum of frac over mud fractions) instead
subroutine detthcmud(this, thcmud)
    use precision
    !
    implicit none
!
! Function/routine arguments
!
    type(bedcomp_data)                                        , intent(in)  :: this     !< bed composition object
    real(fp), dimension(this%settings%nmlb:this%settings%nmub), intent(out) :: thcmud  !  Total thickness of the mud layers
!
! Local variables
!
    integer                             :: l
    integer                             :: nm
    real(prec), dimension(:,:), pointer :: bodsed
    real(fp)  , dimension(:)  , pointer :: rhofrac
!
!! executable statements -------------------------------------------------------
!
    bodsed      => this%state%bodsed
    rhofrac     => this%settings%rhofrac
    !
    do nm = this%settings%nmlb,this%settings%nmub
       thcmud (nm) = 0.0
        do l = 1, this%settings%nfrac
           if (this%settings%sedtyp(l) <= this%settings%max_mud_sedtyp) then
              thcmud(nm) = thcmud(nm) + real(bodsed(l, nm),fp)/rhofrac(l)
           endif
        enddo
    enddo
end subroutine detthcmud


!> Determine sediment thickness optionally per sediment fraction
!! DEPRECATED FUNCTIONALITY; use getsedthick instead.
subroutine getalluvthick(this, seddep, nmfrom, nmto, nval)
    use precision 
    !
    implicit none
    !
!
! Function/routine arguments
!
    type(bedcomp_data)                                                , intent(in)  :: this     !< bed composition object
    integer                                                           , intent(in)  :: nmfrom
    integer                                                           , intent(in)  :: nmto
    integer                                                           , intent(in)  :: nval
    real(fp), dimension(nmfrom:nmto, nval)                            , intent(out) :: seddep
!
! Local variables
!
    integer                              :: k
    integer                              :: l
    integer                              :: nm
    real(fp)                             :: thkl
    integer, pointer                     :: nlyr
    real(prec), dimension(:,:) , pointer :: bodsed
    real(fp)  , dimension(:,:) , pointer :: thlyr
    real(fp)                   , pointer :: thresh
    real(fp)  , dimension(:)   , pointer :: rhofrac
!
!! executable statements -------------------------------------------------------
!
    nlyr                => this%settings%nlyr
    rhofrac             => this%settings%rhofrac
    bodsed              => this%state%bodsed
    thlyr               => this%state%thlyr
    !
    select case (this%settings%iunderlyr)
    case (BED_LAYERED)
       do nm = nmfrom,nmto
          thkl = 0.0_fp
          do k = 1, nlyr
             thkl = thkl + thlyr(k, nm)
          enddo
          do l = 1, nval
             seddep(nm, l) = thkl
          enddo
       enddo
       
    case default ! BED_MIXED
       do nm = nmfrom,nmto
          if (nval==1) then
             seddep(nm, nval) = 0.0
             do l = 1, this%settings%nfrac
                seddep(nm, nval) = seddep(nm, nval) + real(bodsed(l, nm),fp)/rhofrac(l)
             enddo
          else
             do l = 1, this%settings%nfrac
                seddep(nm, l) = real(bodsed(l, nm),fp)/rhofrac(l)
             enddo
          endif
       enddo
    endselect
end subroutine getalluvthick


!> Determines the (mass or volume) fractions.
!! The meaning of volume fraction depends on the porosity model.
!! If the porosity is included in the sediment density, the
!! volume fractions are based on the total volume including
!! fraction specific pore volume.
!! For all other porosity models, the volume fractions are based
!! on the solid volume. This second definition of the volume
!! fraction is equal to the mass fraction if the specific densities
!! of all sediment fractions are the same.
subroutine getfrac(this, frac, anymud, mudcnt, mudfrac, nmfrom, nmto, ifracreq)
    use precision 
    !
    implicit none
    !
    ! Function/routine arguments
    !
    type(bedcomp_data)                                                              :: this     !< bed composition object
    integer                                                           , intent(in)  :: nmfrom   !< first index requested
    integer                                                           , intent(in)  :: nmto     !< last index requested
    logical                                                           , intent(in)  :: anymud   !< flag indicating whether any cohesive sediment class is included in the simulation
    integer                                                 ,optional , intent(in)  :: ifracreq !< switch to request mass or volume fractions (overrules the default)
    real(fp), dimension(nmfrom:nmto)                                  , intent(in)  :: mudcnt   !< local (non-simulated) cohesive sediment class
    real(fp), dimension(nmfrom:nmto, this%settings%nfrac)             , intent(out) :: frac     !< mass or volume fraction per sediment class [-]
    real(fp), dimension(nmfrom:nmto)                                  , intent(out) :: mudfrac  !< total cohesive sediment fraction [-]
    !
    ! Local variables
    !
    integer  :: ifracreq_     !< type of fraction to be returned (mass or volume)
    integer  :: l             !< fraction index
    integer  :: nm            !< spatial index
    real(fp) :: nonmud        !< fraction of non-cohesive sediment [-]
    !
    !! executable statements -------------------------------------------------------
    !
    ! Determine whether to return mass or volume fractions
    !
    if (present(ifracreq)) then
        ifracreq_ = ifracreq
    else
        ifracreq_ = this%settings%ifractions
    endif
    
    !
    ! Call the appropriate routine
    !
    select case (ifracreq_)
    case (FRAC_MASS)
       call getmfrac(this ,frac, nmfrom, nmto)
    case default ! FRAC_VOLUME
       call getvfrac(this ,frac, nmfrom, nmto)
    endselect
    !
    ! Calculate mud fraction
    !
    if (anymud) then
       !
       ! Sum the simulated mud fractions.
       !
       mudfrac = 0.0
       do l = 1, this%settings%nfrac
          if (this%settings%sedtyp(l) <= this%settings%max_mud_sedtyp) then
             do nm = nmfrom, nmto
                mudfrac(nm) = mudfrac(nm) + frac(nm,l)
             enddo
          endif
       enddo
    else
       !
       ! Take into account possible non-simulated
       ! mud fraction.
       !
       do nm = nmfrom, nmto
          mudfrac(nm) = mudcnt(nm)
          nonmud      = 1.0_fp - mudcnt(nm)
          do l = 1, this%settings%nfrac
             frac(nm,l) = frac(nm,l) * nonmud
          enddo
       enddo
    endif
end subroutine getfrac


!> Determines general bed properties such as porosity and critical
!! shear stress for erosion.
subroutine getbedprop(this, nmfrom, nmto, poros, tcrero, eropar)
    use precision 
    use sediment_basics_module
    !
    implicit none
    !
    ! Function/routine arguments
    !
    type(bedcomp_data)                                                              :: this     !< bed composition object
    integer                                                           , intent(in)  :: nmfrom !< first index requested
    integer                                                           , intent(in)  :: nmto   !< last index requested
    real(fp), dimension(nmfrom:nmto)                        , optional, intent(out) :: poros  !< bed porosity
    real(fp), dimension(nmfrom:nmto)                        , optional, intent(out) :: tcrero !< critical shear stress for erosion
    real(fp), dimension(nmfrom:nmto)                        , optional, intent(out) :: eropar !< erosion parameter
    !
    real(fp) , dimension(:,:)  , pointer :: csandlyr      !< sand concentration at each layer
    real(fp) , dimension(:,:)  , pointer :: cmudlyr       !< mud concentration at each layer
    real(fp) , dimension(:)    , pointer :: rhofrac       !<
    real(fp) , dimension(:)    , pointer :: rhow
    
    real(fp)                   , pointer :: ag
    integer                    , pointer :: ierosion
    real(fp)                   , pointer :: ksigma        ! effective stress coefficient [Pa]
    real(fp)                   , pointer :: ky
    real(fp)                   , pointer :: nf            ! fractal dimension nf [-]
    real(fp)                   , pointer :: kk            ! permeability coefficient [m/s]
    !
    real(fp)                   , pointer :: rhow_const
    !
    real(fp) , dimension(:,:,:), pointer :: msed
    real(fp) , dimension(:,:)  , pointer :: svfrac
    real(fp) , dimension(:,:)  , pointer :: thlyr
    !
    !
    ! Local variables
    !
    integer                                 :: l             !< fraction index - loop variable 
    integer                                 :: nm            !< space index - loop variable
    real(fp)                                :: cv            !< consolidation coefficient
    real(fp)                                :: cvfac         !< multiplication factor in computation of the consolidation coefficient
    real(fp)                                :: cu            !< consolidation coefficient related to ky
    real(fp), dimension(this%settings%nfrac):: mfrac         !<
    real(fp)                                :: phi_mud       !< mud volume fraction (phi_mud + phi_sand = 1 - poros)
    real(fp)                                :: phi_sand      !< sand volume fraction (phi_mud + phi_sand = 1 - poros)
    real(fp)                                :: phi_term      !< term representing the influence of phi on tcrero and its inverse influence on eropar
    real(fp)                                :: pi_index      !< plasticity index
    real(fp)                                :: poros_ref     !< reference porosity [-]
    real(fp)                                :: rho           !< layer density = total mass divided by layer thickness [kg/m3]
    real(fp)                                :: rhos          !< sediment specific density [kg/m3]
    real(fp)                                :: thick         !< layer thickness [m]
    real(fp)                                :: totmass       !< total mass of sediment in transport layer per unit area [kg/m2]
    real(fp)                                :: xi            !< weight factor 0-1 [-]
    !
    real(fp)                      , pointer :: A                !< activity of soil, which is used to calculate PI index    
    real(fp)                      , pointer :: alpha            !< a constant in determining critical bed shear stress for erosion
    real(fp)                      , pointer :: alpha_me         !< tuning parameter in simple Me equation 
    real(fp)                      , pointer :: alpha_mix        !< tuning parameter for cohesionless mixture
    real(fp)                      , pointer :: alpha_lehir      !< tuning parameter in Le Hir tcrero equation 
    real(fp)                      , pointer :: alpha_winterwerp !< tuning parameter in Winterwerp Me equation 
    real(fp)                      , pointer :: alpha1           !< non-linearity coefficient for the interpolation between rho_min1 and rho_star [-]
    real(fp)                      , pointer :: alpha2           !< non-linearity coefficient for the interpolation between rho_star and rho_min2 [-]
    real(fp)                      , pointer :: beta             !< a constant in determining critical bed shear stress for erosion
    real(fp)                      , pointer :: beta_mix         !< tuning parameter for cohesionless mixture
    real(fp)                      , pointer :: C0               !< interceptin x axis of mud fraction vs critical bed shear stress for erosion plot
    real(fp)                      , pointer :: d50sed           !< d50 grain size of sediment supply [m]
    real(fp)                      , pointer :: rho_max          !< layer density at and above which critical shear stress taucr_min2 should be applied [kg/m3]
    real(fp)                      , pointer :: rho_min          !< layer density at and below which critical shear stress taucr_min1 should be applied [kg/m3]
    real(fp)                      , pointer :: rho_star         !< layer density at which critical shear stress maximum taucr_max should be applied [kg/m3]
    real(fp)                      , pointer :: taucr_max        !< maximum critical shear stress
    real(fp)                      , pointer :: taucr_min1       !< critical shear stress at low density
    real(fp)                      , pointer :: taucr_min2       !< critical shear stress at high density

    !
    !! executable statements -------------------------------------------------------
    !
    csandlyr       => this%state%csandlyr
    cmudlyr        => this%state%cmudlyr
    rhow           => this%state%rhow
    ag             => this%settings%ag
    ierosion       => this%settings%ierosion
    ksigma         => this%settings%ksigma
    ky             => this%settings%ky
    nf             => this%settings%nf
    kk             => this%settings%kk
    rhofrac        => this%settings%rhofrac
    rhow_const     => this%settings%rhow_const
    !
    A                => this%settings%erosion%A
    alpha            => this%settings%erosion%alpha
    alpha_me         => this%settings%erosion%alpha_me
    alpha_mix        => this%settings%erosion%alpha_mix
    alpha_lehir      => this%settings%erosion%alpha_lehir
    alpha_winterwerp => this%settings%erosion%alpha_winterwerp
    alpha1           => this%settings%erosion%alpha1
    alpha2           => this%settings%erosion%alpha2
    beta             => this%settings%erosion%beta
    beta_mix         => this%settings%erosion%beta_mix
    C0               => this%settings%erosion%C0
    d50sed           => this%settings%erosion%d50sed
    rho_max          => this%settings%erosion%rho_max
    rho_min          => this%settings%erosion%rho_min
    rho_star         => this%settings%erosion%rho_star
    taucr_max        => this%settings%erosion%taucr_max
    taucr_min1       => this%settings%erosion%taucr_min1
    taucr_min2       => this%settings%erosion%taucr_min2
    !
    msed           => this%state%msed
    svfrac         => this%state%svfrac
    thlyr          => this%state%thlyr
    
    ! tcrero and eropar initialized to 1. Only change if varying.

    select case(this%settings%iunderlyr)
    case (BED_MIXED)
        !
    case (BED_LAYERED)
        !
        ! Porosity is obtained based on transport layer only
        !
        do nm = nmfrom, nmto
            poros(nm) = 1.0_fp - svfrac(1, nm)
        enddo
        !
        if (ierosion == EROS_CONST) then
            !
            ! erodibility of cohesive sediment doesn't depend on bed composition
            !
        else
            rhos = rhofrac(1)
            cvfac = 2.0_fp/(3.0_fp - nf) * ksigma * kk / ag
            !
            if (ierosion == EROS_LE_HIR) then
                poros_ref = 1 - this%settings%svfrac0
            endif
            !
            do nm = nmfrom, nmto
                !
                ! Determine volume fractions -- note that they sum to 1 - porosity !!
                !
                phi_mud  = 0.0_fp
                phi_sand = 0.0_fp
                totmass = 0.0_fp
                thick = max(1e-10_fp, thlyr(1,nm))
                do l = 1, this%settings%nfrac
                    totmass = totmass + msed(l,1,nm)
                    if (this%settings%sedtyp(l) <= this%settings%max_mud_sedtyp) then
                        phi_mud   = phi_mud + (msed(l,1,nm)/rhofrac(l)/thick)
                    else
                        phi_sand  = phi_sand + (msed(l,1,nm)/rhofrac(l)/thick)
                    endif
                enddo
                rho = totmass / thick
                !
                select case(ierosion)
                case (EROS_WHITEHOUSE)
                    !
                    ! Whitehouse (2001)
                    !
                    ! TODO: verify formula: rhosol versus phi_sand
                    ! prevous code in erosilt: tcrero = e1 * (phi_mud*rhosol) ** e2
                    ! e1 = 0.0012_fp, e2 = 1.2_fp
                    tcrero(nm) = 0.0012_fp * (phi_mud * phi_sand)**1.2_fp

                case (EROS_LE_HIR)
                    !
                    ! Critical Bed Shear Stress for Erosion & Maximum Erosion Rate of cohesive sediment (tcrero and Me compilation by van Rijn, 2020 and Me from process-based by Winterwerp et al., 2013)
                    !
                    phi_term = (phi_mud)**(poros(nm)/poros_ref)
                    if (phi_mud <= C0) then     !transition from cohesionless to cohesive. In cohesionless sediment, the impact of clay is negligible: tcrero is constant
                        phi_mud = C0
                        !
                        tcrero(nm) = alpha_lehir * phi_term
                        eropar(nm) = alpha_me / phi_term
                    else
                        tcrero(nm) = alpha_lehir * phi_term
                        eropar(nm) = alpha_me / phi_term
                    endif

                case (EROS_ALONSO)
                    !
                    ! NEW Critical Bed Shear Stress for Erosion & Maximum Erosion Rate of cohesive sediment (tcrero and Me compilation by van Rijn, 2020 and Me from process-based by Winterwerp et al., 2013)
                    !
                    ! Reference porosity based on transport layer
                    !
                    if (totmass>0.0_fp) then
                        do l = 1, this%settings%nfrac
                            mfrac(l) = msed(l, 1, nm)/totmass
                        enddo
                        !
                        call getporosity(this, mfrac, poros_ref)
                    else
                        poros_ref = 1.0_fp
                    endif
                    
                    if (phi_mud <= C0) then     !transition from cohesionless to cohesive. In cohesionless sediment, the impact of clay is negligible: tcrero is constant
                        phi_mud = C0
                        !
                        tcrero(nm) = 0.0_fp
                        eropar(nm) = 0.0_fp
                    else
                        phi_term = (phi_mud/C0) * (poros_ref/poros(nm))
                        tcrero(nm) = alpha_lehir * phi_term
                        eropar(nm) = alpha_me / phi_term
                    endif

                case (EROS_WINTERWERP)
                    !
                    ! Critical bed shear stress and maximum erosion rate of cohesive sediment (Winterwerp et al., 2013)
                    !
                    pi_index = A * max(0.0_fp, (phi_mud - C0)) * 100.0_fp
                    cv = cvfac / rhow(nm) 
                    if (phi_mud <= C0) then     !transition from cohesionless to cohesive. In cohesionless sediment, the impact of clay is negligible: tcrero is constant
                        phi_mud = C0
                        !
                        tcrero(nm) = alpha_mix * pi_index**beta_mix 
                        !
                        phi_sand = 1.0_fp - phi_mud - poros(nm)
                        cu = ky * (phi_mud/(1.0_fp - phi_sand))**(2.0_fp/(3.0_fp - nf))
                        eropar(nm) = cv * (phi_mud**2) * rhos / alpha_winterwerp / d50sed / cu
                        !
                    else
                        !
                        tcrero(nm) = alpha * pi_index**beta
                        !
                        cu = ky * (phi_mud/(1.0_fp - phi_sand))**(2.0_fp/(3.0_fp - nf))
                        eropar(nm) = cv * (phi_mud**2) * rhos / alpha_winterwerp / d50sed / cu
                        !
                    endif

                case (EROS_MUSA)
                    !
                    ! rho refers here to dry bed densities of the sediment layer
                    !
                    if (rho < rho_min) then
                        tcrero(nm) = taucr_min1
                    elseif (rho < rho_star) then
                        xi = ((rho - rho_min)/(rho_star - rho_min))**alpha1
                        tcrero(nm) = taucr_min1 * (1-xi) + taucr_max * xi
                    elseif (rho < rho_max) then
                        xi = ((rho_max - rho)/(rho_max - rho_star))**alpha2
                        tcrero(nm) = taucr_max * xi + taucr_min2 * (1-xi)
                    else
                        tcrero(nm) = taucr_min2
                    endif

                endselect
            enddo
        endif
    endselect
end subroutine getbedprop


!> Determines the mass fractions for the top layer
subroutine getmfrac(this, frac, nmfrom, nmto)
    use precision 
    implicit none
    !
    ! Function/routine arguments
    !
    integer                                                           , intent(in)  :: nmfrom
    integer                                                           , intent(in)  :: nmto
    type(bedcomp_data)                                                , intent(in)  :: this     !< bed composition object
    real(fp), dimension(nmfrom:nmto, this%settings%nfrac)             , intent(out) :: frac
    !
    ! Local variables
    !
    integer                             :: l
    integer                             :: nm
    real(fp)                            :: sedtot
    real(prec), dimension(:,:), pointer :: bodsed
    real(fp), dimension(:,:,:), pointer :: msed
    !
    !! executable statements -------------------------------------------------------
    !
    bodsed      => this%state%bodsed
    msed        => this%state%msed
    !
    ! Calculate total bottom sediments and fractions
    !
    select case (this%settings%iunderlyr)
    case (BED_LAYERED)
       do nm = nmfrom, nmto
          sedtot = 0.0_fp
          do l = 1, this%settings%nfrac
             sedtot = sedtot + msed(l, 1, nm)
          enddo
          if (comparereal(sedtot,0.0_fp) == 0) then
             frac(nm, :) = 1.0_fp/this%settings%nfrac
          else
            do l = 1, this%settings%nfrac
                frac(nm, l) = msed(l, 1, nm)/sedtot
            enddo
          endif
       enddo
       
    case default ! BED_MIXED
       do nm = nmfrom, nmto
          sedtot = 0.0_fp
          do l = 1, this%settings%nfrac
             sedtot = sedtot + real(bodsed(l, nm),fp)
          enddo
          if (comparereal(sedtot,0.0_fp) == 0) then
             frac(nm, :) = 1.0_fp/this%settings%nfrac
          else
             do l = 1, this%settings%nfrac
                frac(nm, l) = real(bodsed(l, nm),fp)/sedtot
             enddo
          endif
       enddo
    endselect
end subroutine getmfrac


!> Update the bed composition of the top layer given the mass fraction data
subroutine setmfrac(this, frac, nmfrom, nmto)
    use precision 
    implicit none
    !
    ! Function/routine arguments
    !
    integer                                                           , intent(in)  :: nmfrom
    integer                                                           , intent(in)  :: nmto
    type(bedcomp_data)                                                              :: this     !< bed composition object
    real(fp), dimension(nmfrom:nmto, this%settings%nfrac)             , intent(in)  :: frac
    !
    ! Local variables
    !
    integer                             :: l
    integer                             :: nm
    real(fp)                            :: sedtot
    real(prec), dimension(:,:), pointer :: bodsed
    real(fp), dimension(:,:,:), pointer :: msed
    !
    !! executable statements -------------------------------------------------------
    !
    bodsed      => this%state%bodsed
    msed        => this%state%msed
    !
    ! Update the sediment mass per fraction, but keep the total mass identical
    !
    select case (this%settings%iunderlyr)
    case (BED_LAYERED)
       do nm = nmfrom, nmto
          sedtot = 0.0_fp
          do l = 1, this%settings%nfrac
             sedtot = sedtot + msed(l, 1, nm)
          enddo
          do l = 1, this%settings%nfrac
             msed(l, 1, nm) = frac(nm, l)*sedtot
          enddo
       enddo
       
    case default ! BED_MIXED
       do nm = nmfrom, nmto
          sedtot = 0.0_fp
          do l = 1, this%settings%nfrac
             sedtot = sedtot + real(bodsed(l, nm),fp)
          enddo
          do l = 1, this%settings%nfrac
             bodsed(l, nm) = real(frac(nm, l)*sedtot,prec)
          enddo
       enddo
    endselect
end subroutine setmfrac


!> Determines the volume fractions for the top layer
subroutine getvfrac(this, frac, nmfrom, nmto)
    use precision
    implicit none
    !
    ! Function/routine arguments
    !
    integer                                                           , intent(in)  :: nmfrom
    integer                                                           , intent(in)  :: nmto
    type(bedcomp_data)                                                , intent(in)  :: this     !< bed composition object
    real(fp), dimension(nmfrom:nmto, this%settings%nfrac)             , intent(out) :: frac
    !
    ! Local variables
    !
    integer                               :: l
    integer                               :: nm
    real(fp)                              :: thick
    real(prec), dimension(:,:)  , pointer :: bodsed
    real(fp)  , dimension(:)    , pointer :: dpsed
    real(fp)  , dimension(:,:)  , pointer :: svfrac
    real(fp)  , dimension(:,:,:), pointer :: msed
    real(fp)  , dimension(:,:)  , pointer :: thlyr
    real(fp)  , dimension(:)    , pointer :: rhofrac
!
!! executable statements -------------------------------------------------------
!
    rhofrac     => this%settings%rhofrac
    svfrac      => this%state%svfrac
    bodsed      => this%state%bodsed
    dpsed       => this%state%dpsed
    msed        => this%state%msed
    thlyr       => this%state%thlyr
    !
    ! Calculate total bottom sediments and fractions
    !
    select case (this%settings%iunderlyr)
    case (BED_LAYERED)
       do nm = nmfrom, nmto
          if (comparereal(thlyr(1, nm), 0.0_fp) == 0) then
             frac(nm, :) = 1.0_fp/this%settings%nfrac
          else
             thick = svfrac(1, nm) * thlyr(1, nm)
             do l = 1, this%settings%nfrac
                frac(nm, l) = msed(l, 1, nm)/(rhofrac(l)*thick)
             enddo
          endif
       enddo
       
    case default ! BED_MIXED
       do nm = nmfrom, nmto
          if (comparereal(dpsed(nm),0.0_fp) == 0) then
             frac(nm, :) = 1.0_fp/this%settings%nfrac
          else
             do l = 1, this%settings%nfrac
                frac(nm, l) = real(bodsed(l, nm),fp)/(rhofrac(l)*dpsed(nm))
             enddo
          endif
       enddo
    endselect
end subroutine getvfrac


!> Update the bed composition of the top layer given the volume fraction data
subroutine setvfrac(this, frac, nmfrom, nmto)
    use precision 
    implicit none
    !
    ! Function/routine arguments
    !
    integer                                                           , intent(in)  :: nmfrom
    integer                                                           , intent(in)  :: nmto
    type(bedcomp_data)                                                              :: this     !< bed composition object    
    real(fp), dimension(nmfrom:nmto, this%settings%nfrac)             , intent(in)  :: frac
    !
    ! Local variables
    !
    integer                                    :: l
    integer                                    :: nm
    real(fp)                                   :: sum
    real(fp)                                   :: sedtot
    real(fp)  , dimension(:,:)  , pointer      :: svfrac
    real(prec), dimension(:,:)  , pointer      :: bodsed
    real(fp)  , dimension(:,:,:), pointer      :: msed
    real(fp)  , dimension(:,:)  , pointer      :: thlyr
    real(fp)  , dimension(:)    , pointer      :: rhofrac
    !
    !! executable statements -------------------------------------------------------
    !
    rhofrac     => this%settings%rhofrac
    svfrac      => this%state%svfrac
    bodsed      => this%state%bodsed
    msed        => this%state%msed
    thlyr       => this%state%thlyr
    !
    ! Calculate total bottom sediments and fractions
    !
    select case (this%settings%iunderlyr)
    case (BED_LAYERED)
       do nm = nmfrom, nmto
          sedtot = 0.0_fp
          do l = 1, this%settings%nfrac
             sedtot = sedtot + msed(l, 1, nm)
          enddo
          sum = 0.0_fp
          do l = 1, this%settings%nfrac
             sum = sum + frac(nm, l)*rhofrac(l)
          enddo
          do l = 1, this%settings%nfrac
             msed(l, 1, nm) = sedtot*(frac(nm, l)*rhofrac(l)/sum)
          enddo
       enddo
       
    case default ! BED_MIXED
       do nm = nmfrom, nmto
          sedtot = 0.0_fp
          do l = 1, this%settings%nfrac
             sedtot = sedtot + real(bodsed(l, nm),fp)
          enddo
          sum = 0.0_fp
          do l = 1, this%settings%nfrac
             sum = sum + frac(nm, l)*rhofrac(l)
          enddo
          do l = 1, this%settings%nfrac
             bodsed(l, nm) = real(sedtot*(frac(nm, l)*rhofrac(l)/sum),prec)
          enddo
       enddo
    endselect
end subroutine setvfrac


!> Determines total thickness of sediment deposit at all points
subroutine getsedthick_allpoints(this, seddep)
    use precision 
    implicit none
    !
    ! Function/routine arguments
    !
    type(bedcomp_data)                                          , intent(in)  :: this     !< bed composition object 
    real(fp), dimension(this%settings%nmlb:this%settings%nmub)  , intent(out) :: seddep
    !
    ! Local variables
    !
    integer                           :: k
    integer                           :: nm
    real(fp), dimension(:), pointer   :: dpsed
    real(fp), dimension(:,:), pointer :: thlyr
    !
    !! executable statements -------------------------------------------------------
    !
    dpsed       => this%state%dpsed
    thlyr       => this%state%thlyr
    !
    ! Calculate total bottom sediments and fractions
    !
    select case (this%settings%iunderlyr)
    case (BED_LAYERED)
        seddep = 0.0_fp
        do nm = this%settings%nmlb, this%settings%nmub
            do k = 1, this%settings%nlyr
                seddep(nm) = seddep(nm) + thlyr(k, nm)
            enddo
        enddo
        
    case default ! BED_MIXED
       seddep = dpsed
    endselect
end subroutine getsedthick_allpoints


!> Determines total thickness of sediment deposit at one point
subroutine getsedthick_1point(this, nm, seddep)
    use precision 
    implicit none
    !
    ! Function/routine arguments
    !
    type(bedcomp_data)                      , intent(in)  :: this     !< bed composition object    
    integer                                 , intent(in)  :: nm
    real(fp)                                , intent(out) :: seddep
    !
    ! Local variables
    !
    integer                           :: k
    real(fp), dimension(:)  , pointer :: dpsed
    real(fp), dimension(:,:), pointer :: thlyr
    !
    !! executable statements -------------------------------------------------------
    !
    dpsed       => this%state%dpsed
    thlyr       => this%state%thlyr
    !
    ! Calculate total bottom sediments and fractions
    !
    select case (this%settings%iunderlyr)
    case (BED_LAYERED)
       seddep = 0.0_fp
       do k = 1, this%settings%nlyr
          seddep = seddep + thlyr(k, nm)
       enddo
       
    case default ! BED_MIXED
       seddep = dpsed(nm)
    endselect
end subroutine getsedthick_1point


!> initialize the morlyr data
function initmorlyr(this) result (istat)
    use precision
    implicit none
    !
    real(fp), parameter :: rmissval = -999.0_fp
    !
    ! Function/routine arguments
    !
    type (bedcomp_data), intent(inout) :: this     !< bed composition object    
    integer                            :: istat
    !
    ! Local variables
    !
    type (bedcomp_settings), pointer   :: settings
    type (bedcomp_state   ), pointer   :: state
    type (bedcomp_work    ), pointer   :: work
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    if (istat == 0) allocate (settings, stat = istat)
    if (istat == 0) allocate (state   , stat = istat)
    if (istat == 0) allocate (work    , stat = istat)
    if (istat /= 0) then
       !error
       return
    endif
    !
    allocate (settings%morlyrnum , stat = istat)
    if (istat == 0) then
       settings%morlyrnum%track_mass_shortage = .true.
       settings%morlyrnum%mass_shortage_thresh = 0.0_fp
       settings%morlyrnum%max_num_shortage_warnings = 100
    endif
    !
    settings%keuler     = 2
    settings%ndiff      = 0
    settings%nfrac      = 0
    settings%nlyr       = 0
    settings%nmlb       = 0
    settings%nmub       = 0
    settings%idiffusion = BDIFF_NONE
    settings%iunderlyr  = BED_MIXED
    settings%ifractions = FRAC_VOLUME
    settings%iporosity  = POROS_IN_DENSITY
    settings%exchlyr    = .false.
    settings%max_mud_sedtyp = SEDTYP_SILT
    settings%neulyr     = 0
    settings%nlalyr     = 0
    settings%theulyr    = rmissval
    settings%thlalyr    = rmissval
    settings%updtoplyr  = TOPLYR_POR_RESET         ! by default, the top layer porosity is reset
    settings%updbaselyr = BASELYR_UPDATED          ! 
    
    !!  --> default values, based on Merckelbach et al. (2000, 2004a, b)
    settings%iconsolidate = CONSOL_NONE            ! by default, consolidation is switched off
    settings%ierosion     = EROS_CONST             ! by default, critical bed shear stress for erosion is determined using empirical relation between mud fraction and bed strength.
    settings%ag           = 9.81_fp                ! gravitational acceleration [m/s2] (default value on Earth; to be overruled by calling component)
    settings%dtdecon      = 1209600.0_fp           ! seconds, default 2 week to update consolidation once
    settings%svgel        = 0.158_fp               ! volume fraction of pure sediment at gelling point
    settings%svmax        = 0.6_fp                 ! if svfrac > svmax, consolidation stops
    settings%nf           = 2.69!2.605_fp               ! fractal dimension [-]
    settings%ky           = 1.0E3_fp               ! [Pa]
    settings%ksigma       = 1.99E7_fp!7.1E7_fp               ! effective stress coefficient [Pa]
    settings%ksigma0      = 0.0_fp                 ! effective stress coefficient (usually set as 0) [Pa]
    settings%kk           = 1.59E-13_fp!7.6E-13_fp             ! permeability coefficient [m/s]
    settings%kbioturb     = 0.0_fp                 ! bioturbation coefficient [m2/s]
    !settings%svfrac0      = 500.0/2650.0           ! example from Townsend&MeVay1990
    settings%svfrac0      = 1600.0/2650.0          ! Example from Townsend&MeVay1990, svfrac is around 0.18, which is reasonable for unconsolidated sediment
    settings%svfrac0m     = 0.2_fp                 ! depositional svfrac for mud
    settings%svfrac0s     = 0.6_fp                 ! depositional svfrac for sand
    settings%minporm      = 0.05_fp                ! compacted porosity for mud
    settings%minpors      = 0.25_fp                ! compacted porosity for sand
    settings%confac       = 1.0_fp                 ! default consider consolidation occurs at morphological time scale
    settings%thtrconcr    = 1.0E-6_fp              ! default very small value to avoid numerical problems
    settings%thtrempty    = 0.0001_fp
    settings%imixtr       = 1                      ! 
    !settings%minpor       = 0.25_fp               ! overburden porosity of sand fraction at depth ~1.5 km
    settings%crmud        = 0.001_fp               ! consolidation rate of clay [m]
    settings%crsand       = 0.01_fp                ! consolidation rate of sand [m]
    settings%crmsec       = 3.0E-04_fp             ! secondary consolidation of mud
    settings%porini       = 0.75_fp                ! 
    !critical bed shear stress
    settings%rhow_const   = 1000.0_fp              ! water density [kg/m3]
    settings%ky           = 1.0E3_fp               ! vertical permeability [Pa]
    
    ! erosion settings
    allocate (settings%erosion , stat = istat)
    if (istat == 0) then
       settings%erosion%A                = 2.67_fp            !< activity of soil, which is used to calculate PI index    
       settings%erosion%alpha            = 0.7_fp             !< a constant in determining critical bed shear stress for erosion
       settings%erosion%alpha_me         = 1.0_fp             !< tuning parameter in simple Me equation 
       settings%erosion%alpha_mix        = 0.2205_fp          !< tuning parameter for cohesionless mixture
       settings%erosion%alpha_lehir      = 1.0_fp             !< tuning parameter in Le Hir tcrero equation 
       settings%erosion%alpha_winterwerp = 10.0_fp            !< tuning parameter in Winterwerp Me equation 
       settings%erosion%alpha1           = 1.0_fp             !< non-linearity coefficient for the interpolation between rho_min1 and rho_star [-]
       settings%erosion%alpha2           = 2.0_fp             !< non-linearity coefficient for the interpolation between rho_star and rho_min2 [-]
       settings%erosion%beta             = 0.2_fp             !< a constant in determining critical bed shear stress for erosion
       settings%erosion%beta_mix         = 0.9125_fp          !< tuning parameter for cohesionless mixture
       settings%erosion%C0               = 0.07_fp            !< interceptin x axis of mud fraction vs critical bed shear stress for erosion plot
       settings%erosion%d50sed           = 3.0E-5_fp          !< d50 grain size of sediment supply [m]
       settings%erosion%rho_max          = 1600.0_fp          !< layer density at and above which critical shear stress taucr_min2 should be applied [kg/m3]
       settings%erosion%rho_min          = 200.0_fp           !< layer density at and below which critical shear stress taucr_min1 should be applied [kg/m3]
       settings%erosion%rho_star         = 1200.0_fp          !< layer density at which critical shear stress maximum taucr_max should be applied [kg/m3]
       settings%erosion%taucr_max        = 1.5_fp             !< maximum critical shear stress [N/m2]
       settings%erosion%taucr_min1       = 0.2_fp             !< critical shear stress at low density [N/m2]
       settings%erosion%taucr_min2       = 0.2_fp             !< critical shear stress at high density [N/m2]

    endif
    
    !! input parameters for Dynamic Equilibrium CONsolidation (DECON)
    settings%nconlyr      = 6                      ! 
    settings%dzprofile    = 0.0001                 ! resolution [m]
    settings%plyrstr      = '0.05 0.05 0.10 0.15 0.20 0.45'
    settings%ptr          = 0.0_fp                 ! percentage of thickness reduction
    !! Peat 
    settings%ccpeat       = 0.0_fp                 ! 
    settings%ymodpeat     = 0.0_fp                 ! 
    settings%peatfrac     = 0                      ! 
    settings%peatloi      = 0.0_fp                 ! 
    settings%parb         = 0.009_fp               ! 
    settings%parc         = 0.08_fp                ! 
    settings%pard         = 0.05_fp                ! 
    settings%peatthick    = 4.0_fp                 ! 

    !
    nullify(settings%kdiff)
    nullify(settings%phi)
    nullify(settings%rhofrac)
    nullify(settings%sedtyp)
    nullify(settings%sigphi)
    nullify(settings%thexlyr)
    nullify(settings%thtrlyr)
    nullify(settings%zdiff)
    nullify(settings%plyrthk)
    !
    nullify(state%bodsed)
    nullify(state%dpsed)
    nullify(state%dzc)
    nullify(state%msed)
    nullify(state%conclyr)
    nullify(state%preload)
    nullify(state%td)
    nullify(state%rhow)
    nullify(state%sedshort)
    nullify(state%svfrac)
    nullify(state%thlyr)
    nullify(state%cmudlyr)
    nullify(state%csandlyr)
    nullify(state%thmudgibson)
    nullify(state%thsandgibson)
    nullify(state%thlyrtprev)
    ! Peat
    nullify(state%strain)
    ! trigger the first bed consolidation by setting DECON time to a large negative value
    state%tdecon = -huge(1.0_hp)
    !
    nullify(work%msed2)
    nullify(work%thlyr2)
    nullify(work%svfrac2)
    nullify(work%preload2)
    nullify(work%td2)

    ! work arrays for full Gibson model
    nullify(work%dthsedlyr)
    nullify(work%thlyrnew)

    nullify(work%sigmaeff)
    nullify(work%thsedlyr)
    nullify(work%svfracsand)
    nullify(work%svfracmud)

    nullify(work%vs0p5)
    nullify(work%k0p5)
    nullify(work%svfrac0p5)
    nullify(work%svfracsand0p5)    
    nullify(work%svfracmud0p5 )
    
    ! work arrays for Dynamic Equilibrium CONsolidation (DECON)
    nullify(work%mmudlyr)
    nullify(work%msandlyr)
    !
    this%settings => settings
    this%state    => state
    this%work     => work
end function initmorlyr


!> allocate the morlyr data arrays
function allocmorlyr(this) result (istat)
    use precision
    implicit none
    !
    ! Function/routine arguments
    !
    type (bedcomp_data)              :: this     !< bed composition object    
    integer                          :: istat
    !
    ! Local variables
    !
    type (bedcomp_settings), pointer :: settings
    type (bedcomp_state), pointer    :: state
    integer                          :: nmlb
    integer                          :: nmub
    integer                          :: nfrac
    !
    !! executable statements -------------------------------------------------------
    !
    settings => this%settings
    state => this%state
    !
    ! Number of layers: 1       transport layer
    !                   1       exchange layer (optional)
    !                   nlalyr  lagrangian underlayers
    !                   neulyr  eulerian underlayers
    !                   1       persistent base layer
    !
    if (settings%iunderlyr == BED_MIXED) then
       settings%nlyr   = 1
       settings%keuler = 1
    elseif (settings%iunderlyr == BED_LAYERED) then
       settings%nlyr   = 2 + settings%nlalyr + settings%neulyr
       settings%keuler = 2 + settings%nlalyr
       if (settings%exchlyr) then
          settings%nlyr   = settings%nlyr + 1
          settings%keuler = settings%keuler + 1
       endif
    endif
    !
    nmlb  = settings%nmlb
    nmub  = settings%nmub
    nfrac = settings%nfrac
    !
    istat = 0
    if (istat == 0) allocate (state%bodsed(nfrac,nmlb:nmub), stat = istat)
    if (istat == 0) state%bodsed = 0.0_fp
    if (istat == 0) allocate (state%dpsed(nmlb:nmub), stat = istat)
    if (istat == 0) state%dpsed = 0.0_fp
    if (istat == 0) allocate (state%rhow(nmlb:nmub), stat = istat)
    if (istat == 0) state%rhow = 1000.0_fp
    !
    if (istat == 0) allocate (settings%sedtyp(nfrac), stat = istat)
    if (istat == 0) settings%sedtyp = 0
    if (istat == 0) allocate (settings%rhofrac(nfrac), stat = istat)
    if (istat == 0) settings%rhofrac = 0.0_fp
    if (istat == 0) allocate (settings%phi(nfrac), stat = istat)
    if (istat == 0) settings%phi = 0.0_fp
    if (istat == 0) allocate (settings%sigphi(nfrac), stat = istat)
    if (istat == 0) settings%sigphi = 0.0_fp
    if (istat == 0) allocate (settings%ymod(nfrac), stat = istat)
    if (istat == 0) settings%ymod = 0.0_fp
    if (istat == 0) allocate (settings%cc(nfrac), stat = istat)
    if (istat == 0) settings%cc = 0.0_fp
    !
    if (settings%iunderlyr == BED_LAYERED) then
       if (istat == 0) allocate (settings%kdiff(settings%ndiff,nmlb:nmub), stat = istat)
       if (istat == 0) settings%kdiff = 0.0_fp
       if (istat == 0) allocate (settings%zdiff(settings%ndiff), stat = istat)
       if (istat == 0) settings%zdiff = 0.0_fp
       if (istat == 0) allocate (settings%thtrlyr(nmlb:nmub), stat = istat)
       if (istat == 0) settings%thtrlyr = 0.0_fp
       if (istat == 0) allocate (state%dzc(nmlb:nmub), stat = istat)
       if (istat == 0) state%dzc = 0.0_fp
       if (settings%exchlyr) then
          if (istat == 0) allocate (settings%thexlyr(nmlb:nmub), stat = istat)
          if (istat == 0) settings%thexlyr = 0.0_fp
       endif
       if (istat == 0) allocate (state%msed(nfrac,settings%nlyr,nmlb:nmub), stat = istat)
       if (istat == 0) state%msed = 0.0_fp
       if (istat == 0) allocate (state%conclyr(nfrac,settings%nlyr,nmlb:nmub), stat = istat)
       if (istat == 0) state%conclyr = 0.0_fp       
       if (istat == 0) allocate (state%thlyr(settings%nlyr,nmlb:nmub), stat = istat)
       if (istat == 0) state%thlyr = 0.0_fp
       if (istat == 0) allocate (state%svfrac(settings%nlyr,nmlb:nmub), stat = istat)
       if (istat == 0) state%svfrac = 1.0_fp
       if (istat == 0) allocate (state%preload(settings%nlyr,nmlb:nmub), stat = istat)
       if (istat == 0) state%preload = 0.0_fp
       if (istat == 0) allocate (state%td(settings%nlyr,nmlb:nmub), stat = istat)
       if (istat == 0) state%td = 0.0_fp
       if (istat == 0) allocate (state%cmudlyr(settings%nlyr,nmlb:nmub), stat = istat)
       if (istat == 0) state%cmudlyr = 0.0_fp
       if (istat == 0) allocate (state%csandlyr(settings%nlyr,nmlb:nmub), stat = istat)
       if (istat == 0) state%csandlyr = 0.0_fp
       if (istat == 0) allocate (state%thlyrtprev(settings%nlyr,nmlb:nmub), stat = istat)
       if (istat == 0) state%thlyrtprev = 0.0_fp
       
       if (istat == 0) allocate (state%thmudgibson(nmlb:nmub), stat = istat)
       if (istat == 0) state%thmudgibson = 0.0_fp
       if (istat == 0) allocate (state%thsandgibson(nmlb:nmub), stat = istat)
       if (istat == 0) state%thsandgibson = 0.0_fp
     
       if (istat == 0) allocate (state%strain(settings%nlyr,nmlb:nmub), stat = istat)
       if (istat == 0) state%strain = 0.0_fp
    endif
    if (istat == 0) allocate (state%sedshort(nfrac,nmlb:nmub), stat = istat)
    if (istat == 0) state%sedshort = 0.0_fp
    !
    ! WARNING: Do not allocate this%work here
    ! For some reason it needs to be allocated/deallocated in updmorlyr/gettoplyr
    !
end function allocmorlyr


!> initialize the morlyr work arrays
function allocwork(this) result (istat)
    use precision
    implicit none
    !
    ! Function/routine arguments
    !
    type (bedcomp_data), intent(in)  :: this     !< bed composition object    
    integer                          :: istat
    !
    ! Local variables
    !
    integer, pointer :: nfrac
    integer, pointer :: nlyr
    !
    real(fp) :: dmiss = -999.0_fp
    !
    !! executable statements -------------------------------------------------------
    !
    nfrac => this%settings%nfrac
    nlyr  => this%settings%nlyr
    !
    istat = 0
    !
    ! Deallocate if it already exists
    if (associated(this%work%msed2))    deallocate (this%work%msed2  , stat = istat)
    if (associated(this%work%thlyr2))   deallocate (this%work%thlyr2 , stat = istat)
    if (associated(this%work%svfrac2))  deallocate (this%work%svfrac2, stat = istat)
    if (associated(this%work%preload2)) deallocate (this%work%preload2, stat = istat)
    if (associated(this%work%td2))      deallocate (this%work%td2, stat = istat)
    !
    if (istat == 0) allocate (this%work%msed2(nfrac, nlyr), stat = istat)
    if (istat == 0) allocate (this%work%thlyr2(nlyr)      , stat = istat)
    if (istat == 0) allocate (this%work%svfrac2(nlyr)     , stat = istat)
    if (istat == 0) allocate (this%work%preload2(nlyr)    , stat = istat)
    if (istat == 0) allocate (this%work%td2(nlyr)         , stat = istat)
    !
    if (istat == 0) this%work%msed2 = dmiss
    if (istat == 0) this%work%thlyr2 = dmiss
    if (istat == 0) this%work%svfrac2 = dmiss
    if (istat == 0) this%work%preload2 = dmiss
    if (istat == 0) this%work%td2 = dmiss
    ! work arrys for full Gibson model
    if (istat == 0) allocate (this%work%dthsedlyr(nlyr-1), stat = istat)
    
    if (istat == 0) allocate (this%work%sigmaeff(nlyr), stat = istat)
    if (istat == 0) allocate (this%work%thsedlyr(nlyr) , stat = istat)
    if (istat == 0) allocate (this%work%svfracsand(nlyr), stat = istat)
    if (istat == 0) allocate (this%work%svfracmud(nlyr), stat = istat)
    
    if (istat == 0) allocate (this%work%vs0p5(nlyr+1), stat = istat)
    if (istat == 0) allocate (this%work%k0p5(nlyr+1) , stat = istat)
    if (istat == 0) allocate (this%work%svfrac0p5(nlyr+1), stat = istat)
    if (istat == 0) allocate (this%work%svfracsand0p5(nlyr+1), stat = istat)
    if (istat == 0) allocate (this%work%svfracmud0p5(nlyr+1), stat = istat)
    
    ! work arrays for Dynamic Equilibrium CONsolidation (DECON)
    if (istat == 0) allocate (this%work%mmudlyr(nlyr), stat = istat)   
    if (istat == 0) allocate (this%work%msandlyr(nlyr), stat = istat)
end function allocwork


!> deallocate the work arrays
function deallocwork(this) result (istat)
    use precision
    implicit none
    !
    ! Function/routine arguments
    !
    type (bedcomp_data), intent(in)  :: this     !< bed composition object    
    integer                          :: istat
    !
    ! Local variables
    !
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    if (istat == 0) deallocate (this%work%msed2        , stat = istat)
    if (istat == 0) deallocate (this%work%thlyr2       , stat = istat)
    if (istat == 0) deallocate (this%work%svfrac2      , stat = istat)
    if (istat == 0) deallocate (this%work%preload2     , stat = istat)
    if (istat == 0) deallocate (this%work%td2          , stat = istat)
    
    if (istat == 0) deallocate (this%work%dthsedlyr    , stat = istat)

    if (istat == 0) deallocate (this%work%sigmaeff     , stat = istat)
    if (istat == 0) deallocate (this%work%thsedlyr     , stat = istat)
    if (istat == 0) deallocate (this%work%svfracsand   , stat = istat)
    if (istat == 0) deallocate (this%work%svfracmud    , stat = istat)
    
    if (istat == 0) deallocate (this%work%vs0p5        , stat = istat)
    if (istat == 0) deallocate (this%work%k0p5         , stat = istat)
    if (istat == 0) deallocate (this%work%svfrac0p5    , stat = istat)
    if (istat == 0) deallocate (this%work%svfracsand0p5, stat = istat)
    if (istat == 0) deallocate (this%work%svfracmud0p5 , stat = istat)
    
    if (istat == 0) deallocate (this%work%mmudlyr      , stat = istat)
    if (istat == 0) deallocate (this%work%msandlyr     , stat = istat)
end function deallocwork


!> deallocate the morlyr arrays
function clrmorlyr(this) result (istat)
    use precision
    implicit none
    !
    ! Function/routine arguments
    !
    type (bedcomp_data)             :: this     !< bed composition object    
    integer                         :: istat
    !
    ! Local variables
    !
    type (bedcomp_settings), pointer :: settings
    type (bedcomp_state)   , pointer :: state
    !
    !! executable statements -------------------------------------------------------
    !
    if (associated(this%settings)) then
       settings => this%settings
       if (associated(settings%kdiff))     deallocate(settings%kdiff    , STAT = istat)
       if (associated(settings%morlyrnum)) deallocate(settings%morlyrnum, STAT = istat)
       if (associated(settings%thexlyr))   deallocate(settings%thexlyr  , STAT = istat)
       if (associated(settings%thtrlyr))   deallocate(settings%thtrlyr  , STAT = istat)
       if (associated(settings%zdiff))     deallocate(settings%zdiff    , STAT = istat)
       !
       if (associated(settings%sedtyp))    deallocate(settings%sedtyp   , STAT = istat)
       if (associated(settings%phi))       deallocate(settings%phi      , STAT = istat)
       if (associated(settings%rhofrac))   deallocate(settings%rhofrac  , STAT = istat)
       if (associated(settings%sigphi))    deallocate(settings%sigphi   , STAT = istat)
       if (associated(settings%plyrthk))   deallocate(settings%plyrthk  , STAT = istat)
       !
       deallocate(this%settings, STAT = istat)
       nullify(this%settings)
    endif
    !
    if (associated(this%state)) then
       state => this%state
       if (associated(state%svfrac))       deallocate(state%svfrac      , STAT = istat)
       if (associated(state%bodsed))       deallocate(state%bodsed      , STAT = istat)
       if (associated(state%dpsed))        deallocate(state%dpsed       , STAT = istat)
       if (associated(state%rhow))         deallocate(state%rhow        , STAT = istat)
       if (associated(state%msed))         deallocate(state%msed        , STAT = istat)
       if (associated(state%thlyr))        deallocate(state%thlyr       , STAT = istat)
       if (associated(state%sedshort))     deallocate(state%sedshort    , STAT = istat)
       if (associated(state%conclyr))      deallocate(state%conclyr     , STAT = istat)
       if (associated(state%cmudlyr))      deallocate(state%cmudlyr     , STAT = istat)
       if (associated(state%csandlyr))     deallocate(state%csandlyr    , STAT = istat)
       if (associated(state%thmudgibson))  deallocate(state%thmudgibson , STAT = istat)
       if (associated(state%thsandgibson)) deallocate(state%thsandgibson, STAT = istat)
       if (associated(state%strain))       deallocate(state%strain      , STAT = istat)
       if (associated(state%thlyrtprev))   deallocate(state%thlyrtprev, STAT = istat)
       !
       deallocate(this%state, STAT = istat)
       nullify(this%state)
    endif
    if (associated(this%work)) then
       istat = deallocwork(this)
       deallocate(this%work, STAT = istat)
    endif
end function clrmorlyr


!> Set sediment fraction properties
subroutine setbedfracprop(this, sedtyp, sedd50, logsedsig, rhofrac)
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                  :: this     !< bed composition object
    integer , dimension(:), intent(in)  :: sedtyp
    real(fp), dimension(:), intent(in)  :: sedd50
    real(fp), dimension(:), intent(in)  :: logsedsig
    real(fp), dimension(:), intent(in)  :: rhofrac
    !
    ! Local variables
    !
    integer :: l
    !
    !! executable statements -------------------------------------------------------
    !
    do l = 1, this%settings%nfrac
       this%settings%sedtyp(l) = sedtyp(l)
       if (sedd50(l)<=0.0001_fp) then
          this%settings%phi(l) = 13.9_fp ! -log(65um)/log(2)
          this%settings%ymod(l) = 5000000.0_fp
          this%settings%cc(l) = this%settings%crmud
       else
          this%settings%phi(l)     = -log(sedd50(l))/log(2.0_fp)
          this%settings%ymod(l) = 25000000.0_fp
          this%settings%cc(l) = this%settings%crsand
       endif
       if (logsedsig(l)<=0.0_fp) then
          this%settings%sigphi(l)  = log(1.34) ! use default for "well sorted" sediment (see rdsed.f90)
       else
          this%settings%sigphi(l)  = logsedsig(l)/log(2.0_fp)
       endif
       this%settings%rhofrac(l) = rhofrac(l) ! either rhosol or cdryb
    enddo
end subroutine setbedfracprop


!> Get the pointer to a scalar logical
function bedcomp_getpointer_logical_scalar(this, variable, val) result (istat)
    use string_module
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)    , intent(in)  :: this     !< bed composition object    
    character(*)          , intent(in)  :: variable
    logical, pointer                    :: val
    integer                             :: istat
    !
    ! Local variables
    !
    character(len(variable))    :: localname
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    localname = variable
    call str_lower(localname)
    select case (localname)
    case ('exchange_layer','exchlyr')
       val => this%settings%exchlyr
    case ('track_mass_shortage')
       val => this%settings%morlyrnum%track_mass_shortage
    case default
       val => NULL()
    end select
    if (.not.associated(val)) istat = -1
end function bedcomp_getpointer_logical_scalar


!> Get the pointer to a scalar integer
function bedcomp_getpointer_integer_scalar(this, variable, val) result (istat)
    use string_module
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)    , intent(in)  :: this     !< bed composition object    
    character(*)          , intent(in)  :: variable
    integer, pointer                    :: val
    integer                             :: istat
    !
    ! Local variables
    !
    character(len(variable))    :: localname
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    localname = variable
    call str_lower(localname)
    select case (localname)
    case ('diffusion_model_type','idiffusion')
       val => this%settings%idiffusion
    case ('bed_layering_type','iunderlyr')
       val => this%settings%iunderlyr
    case ('definition_of_fraction','ifractions')
       val => this%settings%ifractions
    case ('porosity_model_type','iporosity')
       val => this%settings%iporosity
    case ('consolidation_model_type','iconsolidate')
       val => this%settings%iconsolidate
    case ('keuler')
       val => this%settings%keuler
    case ('number_of_diffusion_values','ndiff')
       val => this%settings%ndiff
    case ('number_of_layers','nlyr')
       val => this%settings%nlyr
    case ('number_of_consolidating layers','nconlyr')
       val => this%settings%nconlyr       
    case ('max_num_shortage_warnings')
       val => this%settings%morlyrnum%max_num_shortage_warnings
    case ('number_of_eulerian_layers','neulyr')
       val => this%settings%neulyr
    case ('number_of_lagrangian_layers','nlalyr')
       val => this%settings%nlalyr
    case ('number_of_fractions','nfrac')
       val => this%settings%nfrac
    case ('first_column_number','nmlb')
       val => this%settings%nmlb
    case ('last_column_number','nmub')
       val => this%settings%nmub
    case ('top_layer_updating_type','updtoplyr')
       val => this%settings%updtoplyr
    case ('base_layer_updating_type','updbaselyr')
       val => this%settings%updbaselyr
    case ('erosion_type','ierosion','iero')
        val => this%settings%ierosion
    case default
       val => NULL()
    end select
    if (.not.associated(val)) istat = -1
end function bedcomp_getpointer_integer_scalar


!> Get the pointer to a scalar real(fp)
function bedcomp_getpointer_fp_scalar(this, variable, val) result (istat)
    use precision
    use string_module
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)    , intent(in)  :: this     !< bed composition object    
    character(*)          , intent(in)  :: variable
    real(fp), pointer                   :: val
    integer                             :: istat
    !
    ! Local variables
    !
    character(len(variable))    :: localname
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    localname = variable
    call str_lower(localname)
    select case (localname)
    case ('gravity','ag')
       val => this%settings%ag
    case ('thickness_of_eulerian_layers','theulyr')
       val => this%settings%theulyr
    case ('thickness_of_lagrangian_layers','thlalyr')
       val => this%settings%thlalyr
    case ('mass_shortage_thresh')
       val => this%settings%morlyrnum%mass_shortage_thresh
    case default
       val => NULL()
    end select
    if (.not.associated(val)) istat = -1
end function bedcomp_getpointer_fp_scalar


!> Get the pointer to a 1D real(fp) array
function bedcomp_getpointer_fp_1darray(this, variable, val) result (istat)
    use precision
    use string_module
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)             , intent(in)  :: this     !< bed composition object    
    character(*)                   , intent(in)  :: variable
    real(fp), dimension(:), pointer              :: val
    integer                                      :: istat
    !
    ! Local variables
    !
    character(len(variable))    :: localname
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    localname = variable
    call str_lower(localname)
    select case (localname)
    case ('total_sediment_thickness','dpsed')
       val => this%state%dpsed
    case ('water_density','rhow')
       val => this%state%rhow
    case ('sediment_density')
       val => this%settings%rhofrac
    case ('thickness_of_exchange_layer','thexlyr')
       val => this%settings%thexlyr
    case ('thickness_of_transport_layer','thtrlyr')
       val => this%settings%thtrlyr
    case ('dzc')
       val => this%state%dzc
    case ('diffusion_levels','zdiff')
       val => this%settings%zdiff
    case ('percentage_layerthk','plyrthk')
       val => this%settings%plyrthk
    case default
       val => NULL()
    end select
    if (.not.associated(val)) istat = -1
end function bedcomp_getpointer_fp_1darray


!> Get the pointer to a 2D real(fp) array
function bedcomp_getpointer_fp_2darray(this, variable, val) result (istat)
    use precision
    use string_module
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)               , intent(in)  :: this     !< bed composition object
    character(*)                     , intent(in)  :: variable
    real(fp), dimension(:,:), pointer              :: val
    integer                                        :: istat
    !
    ! Local variables
    !
    character(len(variable))    :: localname
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    localname = variable
    call str_lower(localname)
    select case (localname)
    case ('diffusion_coefficients','kdiff')
       val => this%settings%kdiff
    case ('solid_volume_fraction','svfrac')
       val => this%state%svfrac
    case ('time of load increment','td')
        val => this%state%td
    case ('historical largest load','preload')
       val => this%state%preload
    case ('layer_thickness','thlyr')
       val => this%state%thlyr
    case ('layer_mud_concentration','cmudlyr')
       val => this%state%cmudlyr
    case ('layer_sand_concentration','csandlyr')
       val => this%state%csandlyr
    case ('overburden_thickness_t-1','thlyrtprev')
       val => this%state%thlyrtprev
    case default
       val => NULL()
    end select
    if (.not.associated(val)) istat = -1
end function bedcomp_getpointer_fp_2darray


!> Get the pointer to a 3D real(fp) array
function bedcomp_getpointer_fp_3darray(this, variable, val) result (istat)
    use precision
    use string_module
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                 , intent(in)  :: this     !< bed composition object    
    character(*)                       , intent(in)  :: variable
    real(fp), dimension(:,:,:), pointer              :: val
    integer                                          :: istat
    !
    ! Local variables
    !
    character(len(variable))    :: localname
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    localname = variable
    call str_lower(localname)
    select case (localname)
    case ('layer_mass','msed')
       val => this%state%msed
    case ('layer_concentration','conclyr')
       val => this%state%conclyr
    case default
       val => NULL()
    end select
    if (.not.associated(val)) istat = -1
end function bedcomp_getpointer_fp_3darray


!> Get the pointer to a 2D real(prec) array
function bedcomp_getpointer_prec_2darray(this, variable, val) result (istat)
    use precision
    use string_module
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                 , intent(in)  :: this     !< bed composition object    
    character(*)                       , intent(in)  :: variable
    real(prec), dimension(:,:), pointer              :: val
    integer                                          :: istat
    !
    ! Local variables
    !
    character(len(variable))    :: localname
    !
    !! executable statements -------------------------------------------------------
    !
    istat = 0
    localname = variable
    call str_lower(localname)
    select case (localname)
    case ('total_sediment_mass','bodsed')
       val => this%state%bodsed
    case default
       val => NULL()
    end select
    if (.not.associated(val)) istat = -1
end function bedcomp_getpointer_prec_2darray


!> Use the values of BODSED to compute other quantities
subroutine bedcomp_use_bodsed(this)
    use precision
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                         :: this     !< bed composition object
    !
    ! Local variables
    !
    integer                                    :: ised
    integer                                    :: k
    integer                                    :: kstart
    integer                                    :: nm
    real(fp)                                   :: fac
    real(fp)  , dimension(this%settings%nfrac) :: mfrac
    real(fp)                                   :: poros
    real(fp)                                   :: sedthick
    real(fp)                                   :: sedthicklim
    real(fp)                                   :: svf
    real(fp)                                   :: thsed
    real(fp)                                   :: totsed
    real(prec), dimension(:,:)       , pointer :: bodsed
    real(fp)  , dimension(:)         , pointer :: dpsed
    real(fp)  , dimension(:,:,:)     , pointer :: msed
    real(fp)  , dimension(:,:)       , pointer :: svfrac
    real(fp)  , dimension(:,:)       , pointer :: thlyr
    real(fp)  , dimension(:,:)       , pointer :: cmudlyr
    real(fp)  , dimension(:)         , pointer :: thtrlyr
    real(fp)  , dimension(:)         , pointer :: thexlyr
    real(fp)  , dimension(:)         , pointer :: rhofrac
    !
    !! executable statements -------------------------------------------------------
    thtrlyr    => this%settings%thtrlyr
    thexlyr    => this%settings%thexlyr
    rhofrac    => this%settings%rhofrac
    bodsed     => this%state%bodsed
    dpsed      => this%state%dpsed
    msed       => this%state%msed
    svfrac     => this%state%svfrac
    thlyr      => this%state%thlyr
    cmudlyr    => this%state%cmudlyr
    !
    ! Fill initial values of DPSED
    !
    do nm = this%settings%nmlb, this%settings%nmub
       !
       ! Compute thickness correctly in case of
       ! multiple fractions with different rhofrac
       !
       dpsed(nm) = 0.0_fp
       do ised = 1, this%settings%nfrac
          dpsed(nm) = dpsed(nm) + real(bodsed(ised, nm),fp)/rhofrac(ised)
       enddo
    enddo
    
    select case(this%settings%iunderlyr)
    case(BED_LAYERED)
       !
       ! No file specified for initial bed composition: extract data from
       ! the BODSED data read above.
       !
       msed = 0.0_fp
       thlyr = 0.0_fp
       cmudlyr = 0.0_fp
       do nm = this%settings%nmlb, this%settings%nmub
          !if (kcs(nm)<1 .or. kcs(nm)>2) cycle  !TODO: find a solution for this line
          !
          ! nm = (m-1)*nmax + n
          !
          totsed = 0.0_fp
          do ised = 1, this%settings%nfrac
             totsed = totsed + real(bodsed(ised, nm),fp)
          enddo
          totsed         = max(totsed,1.0e-20_fp) ! avoid division by zero
          do ised = 1, this%settings%nfrac
             mfrac(ised) = (real(bodsed(ised, nm),fp)/totsed)
          enddo
          !
          call getporosity(this, mfrac, poros)
          svf = (1.0_fp - poros) !* this%settings%ptr
          !
          thsed = 0.0_fp
          do ised = 1, this%settings%nfrac
             thsed = thsed + real(bodsed(ised, nm),fp)/rhofrac(ised)
          enddo
          thsed         = thsed/svf
          sedthick      = thsed
          thsed         = max(thsed,1.0e-20_fp) ! avoid division by zero
          !
          ! active/transport layer
          !
          thlyr(1, nm)  = min(thtrlyr(nm),sedthick)
          fac = thlyr(1, nm)/thsed
          do ised = 1, this%settings%nfrac
             msed(ised, 1, nm) = real(bodsed(ised, nm),fp)*fac
          enddo
          svfrac(1, nm) = svf 
          cmudlyr(1,nm) = svfrac(1, nm)*rhofrac(1)  ! zhou
          sedthick      = sedthick - thlyr(1, nm)
          !
          ! exchange layer
          !
          kstart        = 1
          if (this%settings%exchlyr) then
             kstart       = 2
             thlyr(2, nm) = min(thexlyr(nm),sedthick)
             sedthick     = sedthick - thlyr(2, nm)
             fac = thlyr(2, nm)/thsed
             do ised = 1, this%settings%nfrac
                msed(ised, 2, nm) = real(bodsed(ised, nm),fp)*fac
             enddo
             svfrac(2, nm) = svf
          endif
          !
          ! Lagrangian layers
          !
          do k = kstart+1, this%settings%keuler-1
             thlyr(k, nm) = min(this%settings%thlalyr,sedthick)
             sedthick     = sedthick - thlyr(k, nm)
             fac = thlyr(k, nm)/thsed
             do ised = 1, this%settings%nfrac
                msed(ised, k, nm) = real(bodsed(ised, nm),fp)*fac
             enddo
             svfrac(k, nm) = svf
             cmudlyr(k,nm) = svfrac(k, nm)*rhofrac(1)  ! zhou
          enddo
          !
          ! Eulerian layers
          !
          do k = this%settings%keuler,this%settings%nlyr-1
             thlyr(k, nm) = min(this%settings%theulyr,sedthick)
             sedthick     = sedthick - thlyr(k, nm)
             fac = thlyr(k, nm)/thsed
             do ised = 1, this%settings%nfrac
                msed(ised, k, nm) = real(bodsed(ised, nm),fp)*fac
             enddo
             svfrac(k, nm) = svf
             cmudlyr(k,nm) = svfrac(k, nm)*rhofrac(1)  ! zhou
          enddo
          !
          ! base layer
          !
          thlyr(this%settings%nlyr, nm) = sedthick
          fac = thlyr(this%settings%nlyr, nm)/thsed
          do ised = 1, this%settings%nfrac
             msed(ised, this%settings%nlyr, nm) = real(bodsed(ised, nm),fp)*fac
          enddo
          svfrac(this%settings%nlyr, nm) = svf 
          cmudlyr(this%settings%nlyr,nm) = svfrac(this%settings%nlyr, nm)*rhofrac(1)  ! zhou
       enddo
       
    case default ! BED_MIXED
       !
       ! nothing to do, using bodsed as uniformly mixed sediment
       !
    endselect
end subroutine bedcomp_use_bodsed


!> Copy the bed composition from nmfrom to nmto
subroutine copybedcomp(this, nmfrom, nmto)
    use precision
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                   :: this     !< bed composition object
    integer                , intent(in)  :: nmfrom
    integer                , intent(in)  :: nmto
    !
    ! Local variables
    !
    integer                              :: k
    integer                              :: l
    real(prec), dimension(:,:) , pointer :: bodsed
    real(fp) , dimension(:)    , pointer :: dpsed
    real(fp) , dimension(:,:,:), pointer :: msed
    real(fp) , dimension(:,:)  , pointer :: svfrac
    real(fp) , dimension(:,:)  , pointer :: thlyr
    !
    !! executable statements -------------------------------------------------------
    bodsed     => this%state%bodsed
    dpsed      => this%state%dpsed
    msed       => this%state%msed
    svfrac     => this%state%svfrac
    thlyr      => this%state%thlyr
    !
    select case(this%settings%iunderlyr)
    case(BED_LAYERED)
       do k = 1, this%settings%nlyr
          do l = 1, this%settings%nfrac
             msed(l, k, nmto) = msed(l, k, nmfrom)
          enddo
          thlyr(k, nmto)  = thlyr(k, nmfrom)
          svfrac(k, nmto) = svfrac(k, nmfrom)
       enddo
       
    case default ! BED_MIXED
       do l = 1, this%settings%nfrac
          bodsed(l, nmto) = bodsed(l, nmfrom)
       enddo
       dpsed(nmto) = dpsed(nmfrom)
    end select
end subroutine copybedcomp


!> Update the porosity for layer k in column nm
subroutine updateporosity(this, nm, k)
    use precision
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                   :: this     !< bed composition object
    integer                , intent(in)  :: nm
    integer                , intent(in)  :: k
    !
    ! Local variables
    !
    integer                                   :: l
    real(fp)                                  :: poros
    real(fp)                                  :: totmass
    real(fp) , dimension(:,:,:), pointer      :: msed
    real(fp) , dimension(:,:)  , pointer      :: svfrac
    real(fp) , dimension(:,:)  , pointer      :: thlyr
    real(fp) , dimension(this%settings%nfrac) :: mfrac
    !
    !! executable statements -------------------------------------------------------
    msed       => this%state%msed
    svfrac     => this%state%svfrac
    thlyr      => this%state%thlyr
    !
    select case(this%settings%iunderlyr)
    case(BED_LAYERED)
        totmass = 0.0_fp
        do l = 1, this%settings%nfrac
            totmass    = totmass + msed(l, k, nm)
        enddo
        if (totmass>0.0_fp) then
            do l = 1, this%settings%nfrac
                mfrac(l)   = msed(l, k, nm)/totmass
            enddo
            !
            call getporosity(this, mfrac, poros)
        else
            poros = 0.0_fp
        endif
        svfrac(k, nm) = 1.0_fp - poros
        
    case default ! BED_MIXED
       ! option not available for this bed composition model
    end select
end subroutine updateporosity


!> Compute the porosity
subroutine getporosity(this, mfrac, poros)
    use precision
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                       , intent(in)  :: this     !< bed composition object
    real(fp) , dimension(this%settings%nfrac), intent(in)  :: mfrac !< mass fraction
    real(fp)                                 , intent(out) :: poros !< porosity
    !
    ! Local variables
    !
    integer                              :: l
    real(fp)                             :: a
    real(fp)                             :: b
    real(fp)                             :: phim
    real(fp)                             :: sigmix
    real(fp)                             :: x
    real(fp) , dimension(:)    , pointer :: phi
    real(fp) , dimension(:)    , pointer :: sigphi
    !
    !! executable statements -------------------------------------------------------
    phi        => this%settings%phi
    sigphi     => this%settings%sigphi
    !
    phim = 0.0_fp
    do l = 1, this%settings%nfrac
       phim   = phim + phi(l)*mfrac(l)
    enddo
    sigmix = 0.0_fp
    do l = 1, this%settings%nfrac
       sigmix = sigmix + mfrac(l)*((phi(l)-phim)**2 + sigphi(l)**2)
    enddo
    sigmix = sqrt(sigmix)
    !
    select case (this%settings%iporosity)
    case (POROS_FRINGS)
       !
       ! R. Frings (May 2009)
       !
       a = -0.06_fp
       b = 0.36_fp
       poros = max(0.0_fp,a*sigmix + b)
       
    case (POROS_WELTJE)
       !
       ! G.J. Weltje based on data by Beard & Weyl (AAPG Bull., 1973), change the name of author
       !
       x             = 3.7632_fp * sigmix**(-0.7552_fp)
       poros         = 0.45_fp*x/(1+x)
       
    case (POROS_SVFRAC0)
       !temporarily used, should be changed later, using user-specified initial values
       poros = 1.0_fp - this%settings%svfrac0
       
    case (POROS_SVFRAC0SM)
       poros = 0.0_fp
       do l = 1, this%settings%nfrac
           if (this%settings%sedtyp(l) <= this%settings%max_mud_sedtyp) then
               poros = poros + (1.0_fp - this%settings%svfrac0m) * mfrac(l)
           else
               poros = poros + (1.0_fp - this%settings%svfrac0s) * mfrac(l)
           endif
       enddo

    case default
       poros         = 0.0_fp
    end select
end subroutine getporosity


!> Consolidate the bed of column nm
subroutine consolidate(this, nm, morft, dtmor)
    use precision
    
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                                                              :: this     !< bed composition object
    integer                                                           , intent(in)  :: nm
    real(hp)                                                          , intent(in)  :: morft ! morphological time [days since reference date]
    real(fp)                                                          , intent(in)  :: dtmor ! morphological time step [s]
    
    !
    ! Local variables
    !

    !! executable statements -------------------------------------------------------
    
    select case (this%settings%iconsolidate)
    ! this routine is not called for (CONSOL_NONE) ! no consolidation
        
    case (CONSOL_GIBSON) ! full Gibson model
        ! Skip the computation if the transport layer becomes too thin
        if (this%state%thlyr(1,nm) <= this%settings%thtrconcr) return
        
        call consolidate_gibson(this, nm, dtmor)
    
    case (CONSOL_DECON) ! Dynamic Equilibrium CONsolidation (DECON)
        ! The actual consolidation step is only executed once every x time steps
        if (morft < this%state%tdecon + real(this%settings%dtdecon,hp)/86400.0_hp) return

        call consolidate_decon(this, nm, dtmor)

    case (CONSOL_TERZAGHI) ! simple loading model (primary compaction)
        call consolidate_terzaghi(this, nm, morft, dtmor)
        
    case (CONSOL_TERZ_PEAT) ! simple loading model (primary compaction) and peat compaction
        call consolidate_terzaghi_peat(this, nm, morft, dtmor)

    case (CONSOL_NOCOMP)
        call consolidate_no_compaction()
        
    case default
       ! consolidation option not yet implemented
    end select
end subroutine consolidate


!> This routine implements consolidation following the Gibson equation.
subroutine consolidate_gibson(this, nm, dtmor)
    use precision
    use sediment_basics_module
    use morphology_data_module
    
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                                                              :: this     !< bed composition object
    integer                                                           , intent(in)  :: nm
    real(fp)                                                          , intent(in)  :: dtmor ! morphological time step [s]
    
    integer                              :: istat
    
    !
    ! Local variables
    !
    integer                                   :: j         ! loop index used to deal with 0 layer thickness!, z.z
    integer                                   :: k
    integer                                   :: i         ! loop index used for replenish step, property change for transport layer, z.z
    integer                                   :: l
    real(fp)                                  :: svfractemp  ! temp real store and read in the volume fraction, z.z
    real(fp)                                  :: nfd       ! sediment fractal exponent number, = 2.0/(3.0-nf), z.z 
    real(fp)                                  :: load      ! not used in Gibson's formulation, z.z
    real(fp)                                  :: thnew     ! not used in Gibson's formulation, z.z
    real(fp) , dimension(this%settings%nfrac) :: dzl
    real(fp) , dimension(:), pointer          :: dzc
    real(fp) , dimension(:,:,:), pointer      :: msed 
    real(fp) , dimension(:,:,:), pointer      :: conclyr      
    real(fp) , dimension(:,:)  , pointer      :: preload   ! not used in Gibson's formulation, z.z
    real(fp) , dimension(:,:)  , pointer      :: svfrac
    real(fp) , dimension(:,:)  , pointer      :: strain
    real(fp) , dimension(:,:)  , pointer      :: thlyr     ! including pore water
    real(fp) , dimension(:)    , pointer      :: rhow
    real(fp) , dimension(:)    , pointer      :: ymod
    real(fp) , dimension(:)    , pointer      :: cc
    real(fp)                                  :: frac
    real(fp) , dimension(:)    , pointer      :: rhofrac
    real(fp)                                  :: thtrlyr
    
    real(fp)                                  :: thconlyr    ! consolidate layer thickness
    real(fp)                                  :: thconlyreqm ! equilibrium consolidate layer thickness
    
    ! used for replenish step average volume fraction between transport layer and layers below
    real(fp)                                  :: thtemp
    real(fp)                                  :: temp1
    real(fp)                                  :: temp2

    ! Dynamic Equilibrium CONsolidation (DECON)
    real(fp)                                  :: thmudgibson_new    ! total gibson height for mud
    real(fp)                                  :: thsandgibson_new   ! total gibson height for sand
    integer ,dimension(this%settings%nconlyr) :: kzlyr        ! number of vertical grid used to discretize equilibrium concentration profile in each layer
    real(fp),dimension(this%settings%nfrac)   :: permud       ! mud fraction mass percentage
    real(fp),dimension(this%settings%nfrac)   :: persand      ! sand fraction mass percentage
    real(fp),dimension(this%settings%nfrac)   :: mmud         ! mud fraction mass 
    real(fp),dimension(this%settings%nfrac)   :: msand        ! sand fraction mass
    real(fp)                                  :: mmudtot      ! total mud mass
    real(fp)                                  :: msandtot     ! total sand mass
    !real(fp),dimension(this%settings%nconlyr) :: czmudlyr
    integer                                   :: kztotal
    integer                                   :: lowerindex
    integer                                   :: upperindex
    real(fp), pointer                         :: dzprofile
    real(fp) , dimension(:) , allocatable     :: zcprofile
    real(fp) , dimension(:) , allocatable     :: czmud       ! equilibrium mud concentration profile
    real(fp)                                  :: z_up
    real(fp)                                  :: z_low
    
    !! ---> more variables used
    integer, pointer  :: nlyr
    real(fp), pointer :: ag
    real(fp)          :: dtcon !< consolidation time step [s]
    
    integer, pointer  :: nconlyr                           ! number of consolidating layers
    real(fp), pointer :: ksigma                            ! effective stress coefficient, Pa
    real(fp), pointer :: kk                                ! permeability., m/s
    real(fp), pointer :: kbioturb                          ! bioturbation coefficient, m2/s
        
    real(fp), parameter :: sigmawbnd=0.0_fp                ! effective stress at upward boundary, Pa
    real(fp) :: rhos                                       ! sediment specific density, kg/m3
    !!--->  add more working arrays
    real(fp), dimension(:)   , pointer :: svfrac2          ! new solids fraction after consolidation, temp 
    real(fp), dimension(:)   , pointer :: thlyr2           ! new layer thickness after consolidation

    real(fp), dimension(:)   , pointer :: dthsedlyr        ! thickness of average pure sediment between two neighbouring layers, m

    real(fp), dimension(:)   , pointer :: sigmaeff         ! effective stress, Pa
    real(fp), dimension(:)   , pointer :: thsedlyr         ! thickness of pure sediment in each layer, m
    real(fp), dimension(:)   , pointer :: svfracsand       ! total sand solids fraction at each layer
    real(fp), dimension(:)   , pointer :: svfracmud        ! total mud solids fraction at each layer    

    real(fp), dimension(:)   , pointer :: vs0p5            ! particle settling velocity at layer interface, m/s  
    real(fp), dimension(:)   , pointer :: k0p5             ! permeability at layer interface, m/s
    real(fp), dimension(:)   , pointer :: svfrac0p5        ! solids fraction at layer interface
    real(fp), dimension(:)   , pointer :: svfracsand0p5    ! sand solids fraction at layer interface
    real(fp), dimension(:)   , pointer :: svfracmud0p5     ! mud solids fraction at layer interface 
    
    !--> low-concentration consoldiation
    real(fp), dimension(:,:)   , pointer :: csandlyr       ! sand concentration at each layer
    real(fp), dimension(:,:)   , pointer :: cmudlyr        ! mud concentration at each layer
    real(fp), dimension(:)     , pointer :: msandlyr       ! sand mass at each layer
    real(fp), dimension(:)     , pointer :: mmudlyr        ! mud mass at each layer
    real(fp), dimension(:)     , pointer :: plyrthk
    real(fp), dimension(:)     , pointer :: thmudgibson    ! total gibson height for mud
    real(fp), dimension(:)     , pointer :: thsandgibson   ! total gibson height for sand  
    real(fp) , dimension(:)     , pointer     :: thlyrnew
    real(fp) , dimension(:,:)   , pointer     :: thlyrtprev
    
    !Peat 
    real(fp), pointer  :: ymodpeat
    real(fp), pointer  :: ccpeat
    integer , pointer  :: peatfrac
    real(fp), pointer  :: peatloi
    real(fp), pointer  :: parb
    real(fp), pointer  :: parc
    real(fp), pointer  :: pard
    real(fp), pointer  :: peatthick
    real(fp)           :: para
    real(fp)           :: mpeat

    !critical porosity
    real(fp)           :: critpor
    real(fp)           :: thicks
    real(fp)           :: thickm

    !! executable statements -------------------------------------------------------
    msed           => this%state%msed
    preload        => this%state%preload
    svfrac         => this%state%svfrac
    thlyr          => this%state%thlyr
    rhow           => this%state%rhow
    dzc            => this%state%dzc
    rhofrac        => this%settings%rhofrac
    ymod           => this%settings%ymod
    cc             => this%settings%cc
    !peat
    ymodpeat       => this%settings%ymodpeat
    ccpeat         => this%settings%ccpeat
    peatloi        => this%settings%peatloi
    parb           => this%settings%parb
    parc           => this%settings%parc
    pard           => this%settings%pard
    peatfrac       => this%settings%peatfrac
    peatthick      => this%settings%peatthick
    strain         => this%state%strain
    
    conclyr        => this%state%conclyr
    csandlyr       => this%state%csandlyr
    thsandgibson   => this%state%thsandgibson
    thmudgibson    => this%state%thmudgibson
    cmudlyr        => this%state%cmudlyr
    thlyrtprev      => this%state%thlyrtprev
    thlyrnew        => this%work%thlyrnew

    thlyr2         => this%work%thlyr2
    svfrac2        => this%work%svfrac2

    dthsedlyr      => this%work%dthsedlyr

    sigmaeff       => this%work%sigmaeff   
    thsedlyr       => this%work%thsedlyr  
    svfracsand     => this%work%svfracsand
    svfracmud      => this%work%svfracmud 

    vs0p5          => this%work%vs0p5   
    k0p5           => this%work%k0p5   
    svfrac0p5      => this%work%svfrac0p5 
    svfracsand0p5  => this%work%svfracsand0p5 
    svfracmud0p5   => this%work%svfracmud0p5

    msandlyr       => this%work%msandlyr
    mmudlyr        => this%work%mmudlyr

    nlyr           => this%settings%nlyr 
    ag             => this%settings%ag
    nconlyr        => this%settings%nconlyr  
    ksigma         => this%settings%ksigma
    kk             => this%settings%kk
    kbioturb       => this%settings%kbioturb
    plyrthk        => this%settings%plyrthk
    dzprofile      => this%settings%dzprofile
    
    ! Bert, Zhou, assume the sediment density is using constant for all fractions
    rhos     = this%settings%rhofrac(1)            ! sediment density
    nfd      = 2.0_fp/(3.0_fp-this%settings%nf)
    thtrlyr  = this%settings%thtrlyr(nm)           ! get the transport layer thickness
    thtemp   = 0.0_fp                              ! temporary thickness used for transition calculation
    temp1    = 0.0_fp                              ! temporary variable
    temp2    = 0.0_fp                              ! temporary variable
    dtcon    = this%settings%confac * dtmor
    
    ! Compute svfracsand and svfracmud per layer
    do k = 1, nlyr
        if (thlyr(k,nm) > 0.0_fp) then
            svfracsand(k) = 0.0_fp
            svfracmud(k) = 0.0_fp
            do l = 1, this%settings%nfrac
                svfractemp = this%state%msed(l,k,nm)/this%settings%rhofrac(l)/thlyr(k,nm)
                if (this%settings%sedtyp(l) <= this%settings%max_mud_sedtyp) then
                    svfracmud(k) = svfracmud(k) + svfractemp
                else
                    svfracsand(k) = svfracsand(k) + svfractemp
                endif
            enddo
        else
            svfracsand(k) = 0.0_fp
            svfracmud(k) = 0.0_fp
        endif
    enddo
    
    ! calculate the values of the dependent arrays, may be removed later.
    do k = 1, nlyr
        thsedlyr(k) = thlyr(k,nm)*svfrac(k,nm)
    enddo
    
    do k = 1, nlyr-1
        if( thlyr(k,nm) > 0) then
            ! find the next thlyr which is not zero and make the average with this one.
            do j = k+1, nlyr
                if (thlyr(j,nm) > 0.0_fp) then
                    dthsedlyr(k) = (thsedlyr(k)+thsedlyr(j))/2.0_fp
                    exit    ! jump out the inner do loop
                endif
                ! if all the layers below the transport layer have 0.0 thickness, otherwise,
                ! the following equation won't be used, since once the above if statement is 
                ! satisfied, it will exit the do loop.
                dthsedlyr(k) = (thsedlyr(k)+thsedlyr(nlyr))/2.0_fp
            enddo
        else
            dthsedlyr(k) = 0.0_fp
        endif
    enddo  
    
    ! Calculate the solids volume fractions at the layer interfaces.
    ! Set svfrac at water-bed interface equal to the value of transport layer.
    svfrac0p5(1) = svfrac(1,nm)
    svfracsand0p5(1) = svfracsand(1)
    svfracmud0p5(1) = svfracmud(1)
    ! Set svfrac above each non-empty layers equal to the average of that layer and the first
    ! non-empty layer above it. The transport layer should never be empty if one of the lower
    ! layers is non-empty.
    do k = 2, nlyr
        if (thlyr(k,nm) > 0.0_fp) then
            do j= k-1,1,-1
                if (thlyr(j,nm) > 0.0_fp) then
                    ! Compute an average weighted by layer thickness.
                    svfrac0p5(k) = (svfrac(k,nm)*thsedlyr(k)+svfrac(j,nm)*thsedlyr(j))/(thsedlyr(k)+thsedlyr(j))
                    svfracsand0p5(k) = (svfracsand(k)*thsedlyr(k)+svfracsand(j)*thsedlyr(j))/(thsedlyr(k)+thsedlyr(j))
                    svfracmud0p5(k) = (svfracmud(k)*thsedlyr(k)+svfracmud(j)*thsedlyr(j))/(thsedlyr(k)+thsedlyr(j))
                    exit    
                endif
            enddo
        else
            svfrac0p5(k) = 0.0_fp
            svfracsand0p5(k) = 0.0_fp
            svfracmud0p5(k) = 0.0_fp
        endif
    enddo
    ! Set svfrac at rock-bed interface equal to the value of base layer.
    svfrac0p5(nlyr+1)= svfrac(nlyr,nm)
    svfracsand0p5(nlyr+1) = svfracsand(nlyr)
    svfracmud0p5(nlyr+1) = svfracmud(nlyr)
    
    ! Compute hydraulic permeability at the layer interfaces.
    do k = 1, nlyr
        if(svfrac0p5(k) > 0.0_fp) then
            k0p5(k) = kk*(svfracmud0p5(k)/(1.0_fp-svfracsand0p5(k)))**(-nfd)
        else
            k0p5(k) = 0.0_fp
        endif
    enddo
    ! Set permeability to zero at rock-bed interface.
    k0p5(nlyr+1) = 0.0_fp
    
    ! Compute initial effective stress in each non-empty bed layer.
    do k = 1, nlyr
       if (thlyr(k,nm) > 0.0_fp) then
           if (svfrac(k,nm) < this%settings%svgel) then
               sigmaeff(k) = 0.0_fp
           else
               sigmaeff(k) = ksigma*(svfracmud(k)/(1.0_fp-svfracsand(k)))**(nfd)-this%settings%ksigma0
           endif
       else  
           sigmaeff(k) = 0.0_fp
       endif
    enddo
    
    ! Compute the particle settling velocity at the layer interfaces.
    if (thlyr(1,nm) > 0.0_fp) then
        ! The velocity is calculated for the water-bed interface by assuming effective stress at water-bed interface equates sigmawbnd=0
        vs0p5(1) = k0p5(1)*svfrac0p5(1)*((rhos-rhow(nm))/rhow(nm)-(1.0_fp/rhow(nm)/ag + kbioturb/nfd/ksigma/kk)*((sigmaeff(1)-sigmawbnd)/thsedlyr(1)))  
        if (vs0p5(1) < 0.0_fp) then
            vs0p5(1) = 0.0_fp
        endif
    endif
    do k = 2,nlyr
        if (thlyr(k,nm) > 0.0_fp) then
            ! Identify the first non-empty layer above it.
            do j= k-1,1,-1
                if (thlyr(j,nm) > 0.0_fp) then
                    vs0p5(k) = k0p5(k)*svfrac0p5(k)*((rhos-rhow(nm))/rhow(nm)-(1.0_fp/rhow(nm)/ag + kbioturb/nfd/ksigma/kk)*((sigmaeff(k)-sigmaeff(j))/dthsedlyr(j)))
                    exit
                endif
            enddo
            if (vs0p5(k) < 0.0_fp) vs0p5(k) = 0.0_fp
        else
            vs0p5(k) = 0.0_fp
        endif
    enddo
    vs0p5(nlyr+1) = 0.0_fp
    
    ! Compute consolidation
    do k = nlyr, 1, -1
        if (thlyr(k,nm) > 0.0_fp .and. svfracsand(k)/svfrac(k,nm) < 0.5_fp) then
            ! Identify the first non-empty layer above it.
            do j = k,1,-1
                if (svfrac0p5(j)> 0.0_fp .and. thsedlyr(k) > 1.0e-6_fp) then
                    svfrac2(k) = svfrac(k,nm) - dtcon*svfrac(k,nm)*svfrac(k,nm)*(vs0p5(k+1)-vs0p5(j))/thsedlyr(k)
                    exit
                endif
                ! if all the layers below the transport layer have 0.0 thickness
                svfrac2(k) = svfrac(k,nm) - dtcon*svfrac(k,nm)*svfrac(k,nm)*(vs0p5(nlyr+1)-vs0p5(k))/thsedlyr(k)
            enddo
            
            ! Make sure that the solid volume fraction doesn't decrease during consolidation, or increase beyond the maximum.
            if (svfrac2(k) < svfrac(k,nm)) svfrac2(k) = svfrac(k,nm)
            if (svfrac2(k) > this%settings%svmax) svfrac2(k) = this%settings%svmax
            
            ! Layers below should have larger svfrac, avoid numerical issues such as sedimentation-induced thin layers
            if (k<nlyr) then
                if (svfrac2(k) > svfrac2(k+1) .and. svfrac2(k+1) /= 0.0_fp) then
                    svfrac2(k) = svfrac(k,nm)
                endif
            endif
            
            ! Update the layer thickness, but the layer thickness shouldn't increase during consolidation.
            thlyr2(k) = thsedlyr(k)/svfrac2(k)
            if (thlyr2(k) > thlyr(k,nm)) thlyr2(k) = thlyr(k,nm)
            
            ! Update the state arrays based on new values.
            thlyr(k,nm) = thlyr2(k)
            svfrac(k,nm) = svfrac2(k)
            cmudlyr(k,nm) = svfrac(k,nm)*rhos
            csandlyr(k,nm)= 0.0_fp ! why?
        endif
        
        ! The property of the transport layer (1st layer) should be averaged with the layers below when replenish step is done.
        ! This is only considered when thlyr(1,nm) < thtrlyr so that replenish step is required.
        if (this%settings%imixtr==1 .and. thlyr(1,nm) < thtrlyr) then
            ! find the layer when enough thick soil can be used to replenish, avoid some 0 layer thickness
            do j = 2, nlyr
                thtemp = thtemp + thlyr(j,nm)
                if (thtemp > thtrlyr-thlyr(1,nm) ) then
                    do i = 1, j-1
                        temp1 = temp1 + svfrac(i,nm)*thlyr(i,nm)
                        temp2 = temp2 + thlyr(i,nm)
                    enddo
                    svfrac(1,nm) = (temp1 + svfrac(j,nm)*(thtrlyr-temp2))/thtrlyr
                    exit
                endif
            enddo
            temp1 = 0.0_fp
            temp2 = 0.0_fp
        endif
    enddo
end subroutine consolidate_gibson


!> This routine implements the Dynamic Equilibrium Consolidation
subroutine consolidate_decon(this, nm, dtmor)
    use precision
    use sediment_basics_module
    use morphology_data_module
    
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                                                              :: this     !< bed composition object
    integer                                                           , intent(in)  :: nm
    real(fp)                                                          , intent(in)  :: dtmor ! morphological time step [s]
    
    integer                              :: istat
    
    !
    ! Local variables
    !
    integer                                   :: j         ! loop index used to deal with 0 layer thickness!, z.z
    integer                                   :: k
    integer                                   :: i         ! loop index used for replenish step, property change for transport layer, z.z
    integer                                   :: l
    real(fp)                                  :: svfractemp  ! temp real store and read in the volume fraction, z.z
    real(fp)                                  :: nfd       ! sediment fractal exponent number, = 2.0/(3.0-nf), z.z 
    real(fp)                                  :: load      ! not used in Gibson's formulation, z.z
    real(fp)                                  :: thnew     ! not used in Gibson's formulation, z.z
    real(fp) , dimension(this%settings%nfrac) :: dzl
    real(fp) , dimension(:), pointer          :: dzc
    real(fp) , dimension(:,:,:), pointer      :: msed 
    real(fp) , dimension(:,:,:), pointer      :: conclyr      
    real(fp) , dimension(:,:)  , pointer      :: preload   ! not used in Gibson's formulation, z.z
    real(fp) , dimension(:,:)  , pointer      :: svfrac
    real(fp) , dimension(:,:)  , pointer      :: strain
    real(fp) , dimension(:,:)  , pointer      :: thlyr     ! including pore water
    real(fp) , dimension(:)    , pointer      :: rhow
    real(fp) , dimension(:)    , pointer      :: ymod
    real(fp) , dimension(:)    , pointer      :: cc
    real(fp)                                  :: frac
    real(fp) , dimension(:)    , pointer      :: rhofrac
    real(fp)                                  :: thtrlyr
    
    real(fp)                                  :: thconlyr    ! consolidate layer thickness
    real(fp)                                  :: thconlyreqm ! equilibrium consolidate layer thickness
    
    ! used for replenish step average volume fraction between transport layer and layers below
    real(fp)                                  :: thtemp
    real(fp)                                  :: temp1
    real(fp)                                  :: temp2

    ! Dynamic Equilibrium CONsolidation (DECON)
    real(fp)                                  :: thmudgibson_new    ! total gibson height for mud
    real(fp)                                  :: thsandgibson_new   ! total gibson height for sand
    integer ,dimension(this%settings%nconlyr) :: kzlyr        ! number of vertical grid used to discretize equilibrium concentration profile in each layer
    real(fp),dimension(this%settings%nfrac)   :: permud       ! mud fraction mass percentage
    real(fp),dimension(this%settings%nfrac)   :: persand      ! sand fraction mass percentage
    real(fp),dimension(this%settings%nfrac)   :: mmud         ! mud fraction mass 
    real(fp),dimension(this%settings%nfrac)   :: msand        ! sand fraction mass
    real(fp)                                  :: mmudtot      ! total mud mass
    real(fp)                                  :: msandtot     ! total sand mass
    !real(fp),dimension(this%settings%nconlyr) :: czmudlyr
    integer                                   :: kztotal
    integer                                   :: lowerindex
    integer                                   :: upperindex
    real(fp), pointer                         :: dzprofile
    real(fp) , dimension(:) , allocatable     :: zcprofile
    real(fp) , dimension(:) , allocatable     :: czmud       ! equilibrium mud concentration profile
    real(fp)                                  :: z_up
    real(fp)                                  :: z_low
    
    !! ---> more variables used
    integer, pointer  :: nlyr
    real(fp), pointer :: ag
    
    integer, pointer  :: nconlyr                           ! number of consolidating layers
    real(fp), pointer :: ksigma                            ! effective stress coefficient, Pa
    real(fp), pointer :: kk                                ! permeability., m/s
    real(fp), pointer :: kbioturb                          ! bioturbation coefficient, m2/s
        
    real(fp), parameter :: sigmawbnd=0.0_fp                ! effective stress at upward boundary, Pa
    real(fp) :: rhos                                       ! sediment specific density, kg/m3
    !!--->  add more working arrays
    real(fp), dimension(:)   , pointer :: svfrac2          ! new solids fraction after consolidation, temp 
    real(fp), dimension(:)   , pointer :: thlyr2           ! new layer thickness after consolidation

    real(fp), dimension(:)   , pointer :: dthsedlyr        ! thickness of average pure sediment between two neighbouring layers, m

    real(fp), dimension(:)   , pointer :: sigmaeff         ! effective stress, Pa
    real(fp), dimension(:)   , pointer :: thsedlyr         ! thickness of pure sediment in each layer, m
    real(fp), dimension(:)   , pointer :: svfracsand       ! total sand solids fraction at each layer
    real(fp), dimension(:)   , pointer :: svfracmud        ! total mud solids fraction at each layer    

    real(fp), dimension(:)   , pointer :: vs0p5            ! particle settling velocity at layer interface, m/s  
    real(fp), dimension(:)   , pointer :: k0p5             ! permeability at layer interface, m/s
    real(fp), dimension(:)   , pointer :: svfrac0p5        ! solids fraction at layer interface
    real(fp), dimension(:)   , pointer :: svfracsand0p5    ! sand solids fraction at layer interface
    real(fp), dimension(:)   , pointer :: svfracmud0p5     ! mud solids fraction at layer interface 
    
    !--> low-concentration consoldiation
    real(fp), dimension(:,:)   , pointer :: csandlyr       ! sand concentration at each layer
    real(fp), dimension(:,:)   , pointer :: cmudlyr        ! mud concentration at each layer
    real(fp), dimension(:)     , pointer :: msandlyr       ! sand mass at each layer
    real(fp), dimension(:)     , pointer :: mmudlyr        ! mud mass at each layer
    real(fp), dimension(:)     , pointer :: plyrthk
    real(fp), dimension(:)     , pointer :: thmudgibson    ! total gibson height for mud
    real(fp), dimension(:)     , pointer :: thsandgibson   ! total gibson height for sand  
    real(fp) , dimension(:)     , pointer     :: thlyrnew
    real(fp) , dimension(:,:)   , pointer     :: thlyrtprev
    
    !Peat 
    real(fp), pointer  :: ymodpeat
    real(fp), pointer  :: ccpeat
    integer , pointer  :: peatfrac
    real(fp), pointer  :: peatloi
    real(fp), pointer  :: parb
    real(fp), pointer  :: parc
    real(fp), pointer  :: pard
    real(fp), pointer  :: peatthick
    real(fp)           :: para
    real(fp)           :: mpeat

    !critical porosity
    real(fp)           :: critpor
    real(fp)           :: thicks
    real(fp)           :: thickm

    !! executable statements -------------------------------------------------------
    msed           => this%state%msed
    preload        => this%state%preload
    svfrac         => this%state%svfrac
    thlyr          => this%state%thlyr
    rhow           => this%state%rhow
    dzc            => this%state%dzc
    rhofrac        => this%settings%rhofrac
    ymod           => this%settings%ymod
    cc             => this%settings%cc
    !peat
    ymodpeat       => this%settings%ymodpeat
    ccpeat         => this%settings%ccpeat
    peatloi        => this%settings%peatloi
    parb           => this%settings%parb
    parc           => this%settings%parc
    pard           => this%settings%pard
    peatfrac       => this%settings%peatfrac
    peatthick      => this%settings%peatthick
    strain         => this%state%strain
    
    conclyr        => this%state%conclyr
    csandlyr       => this%state%csandlyr
    thsandgibson   => this%state%thsandgibson
    thmudgibson    => this%state%thmudgibson
    cmudlyr        => this%state%cmudlyr
    thlyrtprev      => this%state%thlyrtprev
    thlyrnew        => this%work%thlyrnew

    thlyr2         => this%work%thlyr2
    svfrac2        => this%work%svfrac2

    dthsedlyr      => this%work%dthsedlyr

    sigmaeff       => this%work%sigmaeff   
    thsedlyr       => this%work%thsedlyr  
    svfracsand     => this%work%svfracsand
    svfracmud      => this%work%svfracmud 

    vs0p5          => this%work%vs0p5   
    k0p5           => this%work%k0p5   
    svfrac0p5      => this%work%svfrac0p5 
    svfracsand0p5  => this%work%svfracsand0p5 
    svfracmud0p5   => this%work%svfracmud0p5

    msandlyr       => this%work%msandlyr
    mmudlyr        => this%work%mmudlyr

    nlyr           => this%settings%nlyr 
    ag             => this%settings%ag
    nconlyr        => this%settings%nconlyr  
    ksigma         => this%settings%ksigma
    kk             => this%settings%kk
    kbioturb       => this%settings%kbioturb
    plyrthk        => this%settings%plyrthk
    dzprofile      => this%settings%dzprofile
    
    ! Bert, Zhou, assume the sediment density is using constant for all fractions
    rhos     = this%settings%rhofrac(1)            ! sediment density
    nfd      = 2.0_fp/(3.0_fp-this%settings%nf)
    thtrlyr  = this%settings%thtrlyr(nm)           ! get the transport layer thickness
    thtemp   = 0.0_fp                              ! temporary thickness used for transition calculation
    temp1    = 0.0_fp                              ! temporary variable
    temp2    = 0.0_fp                              ! temporary variable
    
    thmudgibson_new    = 0.0_fp
    thsandgibson_new   = 0.0_fp
    thconlyr           = 0.0_fp
    mmudtot            = 0.0_fp
    msandtot           = 0.0_fp
    
    ! loop over all fractions to compute svfracsand and svfracmud
    do k = 1,nconlyr
         ! check if the layer thickness is larger than zero
         if (thlyr(k,nm)>0.0_fp) then
             svfracsand(k) = 0.0_fp
             svfracmud(k)  = 0.0_fp
             mmudlyr(k)    = 0.0_fp
             msandlyr(k)   = 0.0_fp
             do l = 1, this%settings%nfrac
                 conclyr(l,k,nm) = msed(l,k,nm)/thlyr(k,nm)
                 svfractemp = msed(l,k,nm)/this%settings%rhofrac(l)/thlyr(k,nm)
                 if (this%settings%sedtyp(l) <= this%settings%max_mud_sedtyp) then
                     svfracmud(k) = svfracmud(k) + svfractemp
                     mmudlyr(k)   = mmudlyr(k) + msed(l,k,nm)
                 else
                     svfracsand(k) = svfracsand(k) + svfractemp
                     msandlyr(k)   = msandlyr(k) + msed(l,k,nm)
                 endif
             enddo
             csandlyr(k,nm) =  msandlyr(k)/thlyr(k,nm)
             cmudlyr(k,nm) = mmudlyr(k)/thlyr(k,nm)
         else
             conclyr(:,k,nm)  = 0.0_fp
             mmudlyr(k)       = 0.0_fp
             msandlyr(k)      = 0.0_fp
             svfracsand(k)    = 0.0_fp
             svfracmud(k)     = 0.0_fp 
             csandlyr(k,nm)   = 0.0_fp 
             cmudlyr(k,nm)    = 0.0_fp 
         endif
         thmudgibson_new   = thmudgibson_new + cmudlyr(k,nm)/(rhos-csandlyr(k,nm))*thlyr(k,nm)
         thsandgibson_new  = thsandgibson_new + csandlyr(k,nm)/rhos*thlyr(k,nm)
         thconlyr      = thconlyr + thlyr(k,nm)
         mmudtot  = mmudtot + mmudlyr(k)
         msandtot = msandtot + msandlyr(k)
    enddo
    
    ! if the Gibson's height, i.e. total mass, has increased
    if (thmudgibson_new + thsandgibson_new - thmudgibson(nm) - thsandgibson(nm) > 0.0_fp) then
    
        ! compute permud(l) and persand(l)
        do l = 1, this%settings%nfrac
            mmud(l) = 0.0_fp
            msand(l)= 0.0_fp
            do k = 1, nconlyr
                if (this%settings%sedtyp(l) <= this%settings%max_mud_sedtyp) then
                    mmud(l) = mmud(l) + msed(l,k,nm)
                else
                    msand(l) = msand(l) + msed(l,k,nm)
                endif
            enddo
            permud(l) = mmud(l)/mmudtot
            persand(l)= msand(l)/msandtot
        enddo

        ! calculate equilibrium consolidating layer thickness, Delta_C
        thconlyreqm = thsandgibson_new + nfd/(nfd - 1.0)*ksigma/ag/(rhos-rhow(nm))*(ag*(rhos-rhow(nm))*thmudgibson_new/ksigma)**((nfd-1.0)/nfd)
        do k = 1, nconlyr
            thlyr(k,nm) = thconlyreqm * plyrthk(k)                  !layer thickness computation
            z_up=sum(plyrthk(k:size(plyrthk)))*(thconlyreqm-thsandgibson_new)          !elevation of the upper border of the layer
            z_low=sum(plyrthk((k+1):size(plyrthk)))*(thconlyreqm-thsandgibson_new)     !elevation of the lower border of the layer
            !averaged integral of the concentration profile from z low to z up
            cmudlyr(k,nm)=rhos/thlyr(k,nm)*((((nfd-1.0_fp)/nfd)*ag*(rhos-rhow(nm))/ksigma)**(1.0_fp/(nfd-1.0_fp)))*(-(nfd-1.0_fp)/nfd)*((thconlyreqm-z_up-thsandgibson_new)**(nfd/(nfd-1.0_fp))-(thconlyreqm-z_low-thsandgibson_new)**(nfd/(nfd-1.0_fp)))
            svfracmud(k) = cmudlyr(k,nm)/rhos
            svfrac(k,nm) = svfracmud(k) + svfracsand(k)
       enddo
       ! redistribute mass and concentration in each layer
       do k = 1, nconlyr
           if (thlyr(k,nm) > 0.0_fp) then
               do l = 1, this%settings%nfrac
                   if (this%settings%sedtyp(l) <= this%settings%max_mud_sedtyp) then
                       msed(l,k,nm) = thlyr(k,nm)*cmudlyr(k,nm)*permud(l)
                       conclyr(l,k,nm) = msed(l,k,nm)/thlyr(k,nm)
                   else
                       msed(l,k,nm) = thlyr(k,nm)*csandlyr(k,nm)*persand(l)
                       conclyr(l,k,nm) = msed(l,k,nm)/thlyr(k,nm)
                   endif
               enddo
            else
               do l = 1, this%settings%nfrac
                   msed(l,k,nm) = 0.0_fp
                   conclyr(l,k,nm) = 0.0_fp
               enddo
            endif
       enddo
       thmudgibson(nm)  = thmudgibson_new
       thsandgibson(nm) = thsandgibson_new
    endif
end subroutine consolidate_decon


!> This routine implements compaction following Terzaghi (1943), load model
subroutine consolidate_terzaghi(this, nm, morft, dtmor)
    use precision
    use sediment_basics_module
    use morphology_data_module
    
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                                                              :: this  !< bed composition object
    integer                                                           , intent(in)  :: nm
    real(hp)                                                          , intent(in)  :: morft !< morphological time [days since reference date]
    real(fp)                                                          , intent(in)  :: dtmor !< morphological time step [s]
    
    !
    ! Local variables
    !
    real(fp) , dimension(:,:,:), pointer      :: msed     !<  
    real(fp) , dimension(:,:)  , pointer      :: preload  !< previous overburden weight [kg/m2]
    real(fp) , dimension(:,:)  , pointer      :: td       !> time of latest load increment (days)
    real(fp) , dimension(:,:)  , pointer      :: svfrac   !< 
    real(fp) , dimension(:,:)  , pointer      :: thlyr    !< layer thickness, including pore water
    real(fp) , dimension(:)    , pointer      :: ymod     !< 
    real(fp) , dimension(:)    , pointer      :: cc       !< 
    real(fp) , dimension(:)    , pointer      :: rhofrac  !< 
    real(fp)                   , pointer      :: ag       !< gravitational accelaration [m/s2]

    integer                                   :: k        !< layer index
    integer                                   :: l        !< sediment index
    real(fp)                                  :: load     !< overburden weight [kg/m2]
    real(fp)                                  :: thnew
    real(fp)                                  :: frac

    real(fp)                                  :: critpor  !< critical porosity
    real(fp)                                  :: thicks   !> thickness of sand only in an underlayer
    real(fp)                                  :: thickm   !> thickness of mud only in an underlayer
    real(fp)                                  :: cceff    !> consolidation rate effective

    !! executable statements -------------------------------------------------------
    msed           => this%state%msed
    preload        => this%state%preload
    td             => this%state%td
    svfrac         => this%state%svfrac
    thlyr          => this%state%thlyr
    rhofrac        => this%settings%rhofrac
    ymod           => this%settings%ymod
    cc             => this%settings%cc
    ag             => this%settings%ag
    
    load = 0.0_fp
    do k = 1, (this%settings%nlyr-1)
        ! compute overburden weight including half of the current layer as self-weight
        if (k == 1) then
            do l = 1, this%settings%nfrac
                load = load + 0.5*msed(l, k, nm)
            enddo
        else
            do l = 1, this%settings%nfrac
                load = load + 0.5*(msed(l, k-1, nm) + msed(l, k, nm))
            enddo
        endif
        !
        if (comparereal(thlyr(k, nm),0.0_fp) == 0) then
            !
            ! layers with zero thickness don't consolidate
            !
        elseif (load > preload(k, nm)) then ! primary consolidation
            !
            ! update time of deposition
            !
            td(k,nm) = real(morft,fp)
            !
            ! compute critical porosity
            !
            critpor = 0.0_fp
            thicks = 0.0_fp
            thickm = 0.0_fp
            do l = 1, this%settings%nfrac
                if (this%settings%sedtyp(l) <= this%settings%max_mud_sedtyp) then
                    thickm = thickm + msed(l,k,nm)/rhofrac(l)/svfrac(k,nm)
                else
                    thicks = thicks + msed(l,k,nm)/rhofrac(l)/svfrac(k,nm)
                endif
            enddo
            critpor = (thickm*this%settings%minporm + thicks*this%settings%minpors) / thlyr(k,nm)
            !
            ! compute primary compaction
            !
            if (svfrac(k,nm) > (1.0_fp - critpor)) then
                thnew = thlyr(k,nm)                             ! no primary compaction
            elseif (k == this%settings%nlyr) then               ! base layer does not experience primary compaction
                thnew = thlyr(k,nm)                             ! no primary compaction
            else
                cceff = 0.0_fp
                do l = 1, this%settings%nfrac
                    frac = msed(l,k,nm)/rhofrac(l)/thlyr(k,nm)/svfrac(k,nm)
                    cceff = cceff + frac * cc(l) * 1.0_fp/ymod(l)  
                enddo
                thnew = thlyr(k,nm) - cceff * thlyr(k,nm) * (load - preload(k, nm)) * ag
            endif
            !
            svfrac(k,nm) = svfrac(k, nm) * thlyr(k, nm) / thnew 
            if (svfrac(k,nm) > (1.0_fp - critpor)) then
                thnew = thlyr(k,nm) * svfrac(k,nm) / (1 - critpor)
            else
                thnew = thnew
            endif
            svfrac(k,nm) = svfrac(k, nm) * thlyr(k, nm) / thnew
            thlyr(k,nm) = thnew
            preload(k,nm) = preload(k,nm)
            !
        elseif (load <= preload(k,nm)) then ! secondary consolidation
            !
            ! compute critical porosity
            !
            critpor = 0.0_fp
            thicks = 0.0_fp
            thickm = 0.0_fp
            do l = 1, this%settings%nfrac
                if (this%settings%sedtyp(l) <= this%settings%max_mud_sedtyp) then
                    thickm = thickm + msed(l,k,nm)/rhofrac(l)/svfrac(k,nm)
                else
                    thicks = thicks + msed(l,k,nm)/rhofrac(l)/svfrac(k,nm)
                endif
            enddo
            critpor = (thickm*this%settings%minporm + thicks*this%settings%minpors) / thlyr(k,nm)
            !
            ! compute secondary consolidation (new)
            !
            if (svfrac(k,nm) > (1 - critpor)) then
                thnew = thlyr(k,nm)                 ! no secondary compaction
            elseif (k == this%settings%nlyr) then
                thnew = thlyr(k,nm)                 ! no secondary compaction
            else
                cceff = 0.0_fp
                do l = 1, this%settings%nfrac
                    if (this%settings%sedtyp(l) <= this%settings%max_mud_sedtyp) then 
                        frac = msed(l,k,nm)/rhofrac(l)/thlyr(k,nm)/svfrac(k,nm)
                        cceff =  cceff + frac * cc(l)
                    endif
                enddo
                thnew = thlyr(k,nm) - cceff * thlyr(k,nm) * this%settings%crmsec * (log(max(1.0_fp, real(morft,fp)) - td(k,nm)) - (log(max(1.0_fp, real(morft,fp)) - td(k,nm) - real(dtmor,hp)/86400.0_hp)))
            endif
            svfrac(k,nm) = svfrac(k, nm) * thlyr(k, nm) / thnew 
            if (svfrac(k,nm) > (1.0_fp - critpor)) then
                thnew = thlyr(k,nm) * svfrac(k,nm) / (1 - critpor)
            else
                thnew = thnew
            endif
            svfrac(k,nm) = svfrac(k, nm) * thlyr(k, nm) / thnew
            thlyr(k,nm) = thnew
            preload(k,nm) = preload(k,nm)
        endif
    enddo
end subroutine consolidate_terzaghi


!> This routine implements compaction following Terzaghi, load model
subroutine consolidate_terzaghi_peat(this, nm, morft, dtmor)
    use precision
    use sediment_basics_module
    use morphology_data_module
    
    implicit none
    !
    ! Call variables
    !
    type(bedcomp_data)                                                              :: this     !< bed composition object
    integer                                                           , intent(in)  :: nm
    real(hp)                                                          , intent(in)  :: morft ! morphological time [days since reference date]
    real(fp)                                                          , intent(in)  :: dtmor ! morphological time step [s]
    
    integer                              :: istat
    
    !
    ! Local variables
    !
    integer                                   :: j         ! loop index used to deal with 0 layer thickness!, z.z
    integer                                   :: k
    integer                                   :: i         ! loop index used for replenish step, property change for transport layer, z.z
    integer                                   :: l
    real(fp)                                  :: svfractemp  ! temp real store and read in the volume fraction, z.z
    real(fp)                                  :: nfd       ! sediment fractal exponent number, = 2.0/(3.0-nf), z.z 
    real(fp)                                  :: load      ! not used in Gibson's formulation, z.z
    real(fp)                                  :: thnew     ! not used in Gibson's formulation, z.z
    real(fp) , dimension(this%settings%nfrac) :: dzl
    real(fp) , dimension(:), pointer          :: dzc
    real(fp) , dimension(:,:,:), pointer      :: msed 
    real(fp) , dimension(:,:,:), pointer      :: conclyr      
    real(fp) , dimension(:,:)  , pointer      :: preload   ! not used in Gibson's formulation, z.z
    real(fp) , dimension(:,:)  , pointer      :: svfrac
    real(fp) , dimension(:,:)  , pointer      :: strain
    real(fp) , dimension(:,:)  , pointer      :: thlyr     ! including pore water
    real(fp) , dimension(:)    , pointer      :: rhow
    real(fp) , dimension(:)    , pointer      :: ymod
    real(fp) , dimension(:)    , pointer      :: cc
    real(fp)                                  :: frac
    real(fp) , dimension(:)    , pointer      :: rhofrac
    real(fp)                                  :: thtrlyr
    
    real(fp)                                  :: thconlyr    ! consolidate layer thickness
    real(fp)                                  :: thconlyreqm ! equilibrium consolidate layer thickness
    
    ! used for replenish step average volume fraction between transport layer and layers below
    real(fp)                                  :: thtemp
    real(fp)                                  :: temp1
    real(fp)                                  :: temp2

    ! Dynamic Equilibrium CONsolidation (DECON)
    real(fp)                                  :: thmudgibson_new    ! total gibson height for mud
    real(fp)                                  :: thsandgibson_new   ! total gibson height for sand
    integer ,dimension(this%settings%nconlyr) :: kzlyr        ! number of vertical grid used to discretize equilibrium concentration profile in each layer
    real(fp),dimension(this%settings%nfrac)   :: permud       ! mud fraction mass percentage
    real(fp),dimension(this%settings%nfrac)   :: persand      ! sand fraction mass percentage
    real(fp),dimension(this%settings%nfrac)   :: mmud         ! mud fraction mass 
    real(fp),dimension(this%settings%nfrac)   :: msand        ! sand fraction mass
    real(fp)                                  :: mmudtot      ! total mud mass
    real(fp)                                  :: msandtot     ! total sand mass
    !real(fp),dimension(this%settings%nconlyr) :: czmudlyr
    integer                                   :: kztotal
    integer                                   :: lowerindex
    integer                                   :: upperindex
    real(fp), pointer                         :: dzprofile
    real(fp) , dimension(:) , allocatable     :: zcprofile
    real(fp) , dimension(:) , allocatable     :: czmud       ! equilibrium mud concentration profile
    real(fp)                                  :: z_up
    real(fp)                                  :: z_low
    
    !! ---> more variables used
    integer, pointer  :: nlyr
    real(fp), pointer :: ag
    
    integer, pointer  :: nconlyr                           ! number of consolidating layers
    real(fp), pointer :: ksigma                            ! effective stress coefficient, Pa
    real(fp), pointer :: kk                                ! permeability., m/s
    real(fp), pointer :: kbioturb                          ! bioturbation coefficient, m2/s
        
    real(fp), parameter :: sigmawbnd=0.0_fp                ! effective stress at upward boundary, Pa
    real(fp) :: rhos                                       ! sediment specific density, kg/m3
    !!--->  add more working arrays
    real(fp), dimension(:)   , pointer :: svfrac2          ! new solids fraction after consolidation, temp 
    real(fp), dimension(:)   , pointer :: thlyr2           ! new layer thickness after consolidation

    real(fp), dimension(:)   , pointer :: dthsedlyr        ! thickness of average pure sediment between two neighbouring layers, m

    real(fp), dimension(:)   , pointer :: sigmaeff         ! effective stress, Pa
    real(fp), dimension(:)   , pointer :: thsedlyr         ! thickness of pure sediment in each layer, m
    real(fp), dimension(:)   , pointer :: svfracsand       ! total sand solids fraction at each layer
    real(fp), dimension(:)   , pointer :: svfracmud        ! total mud solids fraction at each layer    

    real(fp), dimension(:)   , pointer :: vs0p5            ! particle settling velocity at layer interface, m/s  
    real(fp), dimension(:)   , pointer :: k0p5             ! permeability at layer interface, m/s
    real(fp), dimension(:)   , pointer :: svfrac0p5        ! solids fraction at layer interface
    real(fp), dimension(:)   , pointer :: svfracsand0p5    ! sand solids fraction at layer interface
    real(fp), dimension(:)   , pointer :: svfracmud0p5     ! mud solids fraction at layer interface 
    
    !--> low-concentration consoldiation
    real(fp), dimension(:,:)   , pointer :: csandlyr       ! sand concentration at each layer
    real(fp), dimension(:,:)   , pointer :: cmudlyr        ! mud concentration at each layer
    real(fp), dimension(:)     , pointer :: msandlyr       ! sand mass at each layer
    real(fp), dimension(:)     , pointer :: mmudlyr        ! mud mass at each layer
    real(fp), dimension(:)     , pointer :: plyrthk
    real(fp), dimension(:)     , pointer :: thmudgibson    ! total gibson height for mud
    real(fp), dimension(:)     , pointer :: thsandgibson   ! total gibson height for sand  
    real(fp) , dimension(:)     , pointer     :: thlyrnew
    real(fp) , dimension(:,:)   , pointer     :: thlyrtprev
    
    !Peat 
    real(fp), pointer  :: ymodpeat
    real(fp), pointer  :: ccpeat
    integer , pointer  :: peatfrac
    real(fp), pointer  :: peatloi
    real(fp), pointer  :: parb
    real(fp), pointer  :: parc
    real(fp), pointer  :: pard
    real(fp), pointer  :: peatthick
    real(fp)           :: para
    real(fp)           :: mpeat

    !critical porosity
    real(fp)           :: critpor
    real(fp)           :: thicks
    real(fp)           :: thickm

    !! executable statements -------------------------------------------------------
    msed           => this%state%msed
    preload        => this%state%preload
    svfrac         => this%state%svfrac
    thlyr          => this%state%thlyr
    rhow           => this%state%rhow
    dzc            => this%state%dzc
    rhofrac        => this%settings%rhofrac
    ymod           => this%settings%ymod
    cc             => this%settings%cc
    !peat
    ymodpeat       => this%settings%ymodpeat
    ccpeat         => this%settings%ccpeat
    peatloi        => this%settings%peatloi
    parb           => this%settings%parb
    parc           => this%settings%parc
    pard           => this%settings%pard
    peatfrac       => this%settings%peatfrac
    peatthick      => this%settings%peatthick
    strain         => this%state%strain
    
    conclyr        => this%state%conclyr
    csandlyr       => this%state%csandlyr
    thsandgibson   => this%state%thsandgibson
    thmudgibson    => this%state%thmudgibson
    cmudlyr        => this%state%cmudlyr
    thlyrtprev      => this%state%thlyrtprev
    thlyrnew        => this%work%thlyrnew

    thlyr2         => this%work%thlyr2
    svfrac2        => this%work%svfrac2

    dthsedlyr      => this%work%dthsedlyr

    sigmaeff       => this%work%sigmaeff   
    thsedlyr       => this%work%thsedlyr  
    svfracsand     => this%work%svfracsand
    svfracmud      => this%work%svfracmud 

    vs0p5          => this%work%vs0p5   
    k0p5           => this%work%k0p5   
    svfrac0p5      => this%work%svfrac0p5 
    svfracsand0p5  => this%work%svfracsand0p5 
    svfracmud0p5   => this%work%svfracmud0p5

    msandlyr       => this%work%msandlyr
    mmudlyr        => this%work%mmudlyr

    nlyr           => this%settings%nlyr 
    ag             => this%settings%ag
    nconlyr        => this%settings%nconlyr  
    ksigma         => this%settings%ksigma
    kk             => this%settings%kk
    kbioturb       => this%settings%kbioturb
    plyrthk        => this%settings%plyrthk
    dzprofile      => this%settings%dzprofile
    
    ! Bert, Zhou, assume the sediment density is using constant for all fractions
    rhos     = this%settings%rhofrac(1)            ! sediment density
    nfd      = 2.0_fp/(3.0_fp-this%settings%nf)
    thtrlyr  = this%settings%thtrlyr(nm)           ! get the transport layer thickness
    thtemp   = 0.0_fp                              ! temporary thickness used for transition calculation
    temp1    = 0.0_fp                              ! temporary variable
    temp2    = 0.0_fp                              ! temporary variable
    
    load = 0.0_fp
    parb = 0.009_fp
    parc = 0.08_fp
    pard = 0.05_fp
    mpeat = 0.0_fp
    para = parc * peatloi + pard
    do k = 2, this%settings%nlyr
        do l = 1, this%settings%nfrac
            load = load + msed(l, k-1, nm)
        enddo
        if (comparereal(thlyr(k, nm),0.0_fp)==0) then
            !
            ! layers with zero thickness don't consolidate
            !
        else
            mpeat = 0.0_fp
            if (peatfrac>0) mpeat = msed(peatfrac, k, nm)
            !
            if (mpeat > 0.0_fp) then 
                if (load > preload(k, nm)) then
                    !
                    !compute consolidation
                    !
                    if (load * ag < 1000) then    !*msed > 1000 is only at base layer?
                        thnew = thlyr(k, nm)
                        preload(k, nm) = load
                    else
                        strain(k, nm) = para * log(load*ag/1000) + parb * log(real(morft,fp)*86400.0_fp)
                        thnew = peatthick * exp(-1 * strain(k, nm))
                        preload(k, nm) = load
                    endif
                else
                    if (load == 0) then
                        thnew = thlyr(k, nm)
                        preload(k, nm) = preload(k, nm)
                    else 
                        strain(k, nm) = para * log(preload(k, nm)*ag/1000) + parb * log(real(morft,fp)*86400.0_fp)
                        thnew =  peatthick * exp(-1 * strain(k, nm))
                        preload(k, nm) = preload(k, nm)
                    endif
                endif
                svfrac(k, nm) = svfrac(k, nm) * thlyr(k, nm) / thnew
                thlyr(k, nm) = thnew
            else
                if (load > preload(k, nm)) then
                    if (preload(k,nm)==0) then
                        dzc(nm) = 0.0_fp
                    else
                        frac = msed(l,k,nm)/rhofrac(l)/thlyr(k,nm)/svfrac(k,nm);
                        dzl(l) = cc(l) * 1.0_fp/ymod(l) * ag  * (load-preload(k,nm)) * frac 
                        dzc(nm) = dzc(nm) + dzl(l)
                    endif
                elseif (load < preload(k, nm) .and. load > 0.0_fp) then
                    if (preload(k,nm)==0) then
                        dzc(nm) = 0.0_fp
                    else
                        frac = msed(l,k,nm)/rhofrac(l)/thlyr(k,nm)/svfrac(k,nm);
                        dzl(l) = cc(l) * 1.0_fp/ymod(l) * ag  * load * frac ! function of load-preload(k,nm) | the cc, and ymod paramaters were define at subroutine setbedfracprop
                        dzc(nm) = dzc(nm) + dzl(l)
                    endif
                endif
                thnew = thlyr(k, nm) - dzc(nm)
                svfrac(k, nm) = svfrac(k, nm) * thlyr(k, nm) / thnew
                thlyr(k, nm) = thnew
                preload(k, nm) = load
            endif
        !else
            ! Non peat compaction
        endif
    enddo
end subroutine consolidate_terzaghi_peat


subroutine consolidate_no_compaction()
    ! load = 0.0_fp
    ! do k = 2, (this%settings%nlyr-1)
    !     do l = 1, this%settings%nfrac
    !         load = load + msed(l, k-1, nm) 
    !     enddo
    !     if (comparereal(thlyr(k, nm),0.0_fp) == 0) then
    !         !
    !         ! layers with zero thickness don't consolidate
    !         !
    !     elseif (load > preload(k, nm)) then ! Primary consolidation
    !         !
    !         ! compute consolidation
    !         !
    !         dzc(nm) = 0.0_fp
    !         thnew = thlyr(k, nm) - dzc(nm)
    !         !
    !         svfrac(k, nm) = svfrac(k, nm) * thlyr(k, nm) / thnew  
    !         thlyr(k, nm) = thnew
    !         preload(k,nm) = load
    !     elseif (load <= preload(k, nm)) then !Secondary consolidation
    !         !
    !         ! compute consolidation
    !         !
    !         dzc(nm) = 0.0_fp
    !         thnew = thlyr(k, nm) - dzc(nm)
    !         !
    !         svfrac(k, nm) = svfrac(k, nm) * thlyr(k, nm) / thnew  
    !         thlyr(k, nm) = thnew
    !         preload(k,nm) = preload(k, nm)
    !     endif
    ! enddo
end subroutine consolidate_no_compaction


!> Initialize the preload array assuming that all sediment is fully consolidated.
subroutine initpreload(this)
    use precision
    implicit none
    !
    ! Function/routine arguments
    !
    type (bedcomp_data), intent(inout) :: this     !< bed composition object    
    !
    ! Local variables
    !
    integer                                   :: k
    integer                                   :: l
    integer                                   :: nm
    real(fp)                                  :: load
    real(fp) , dimension(:,:,:), pointer      :: msed
    real(fp) , dimension(:,:)  , pointer      :: td
    real(fp) , dimension(:,:)  , pointer      :: preload
    !
    !! executable statements -------------------------------------------------------
    !
    msed       => this%state%msed
    preload    => this%state%preload
    !
    select case (this%settings%iunderlyr)
    case (BED_LAYERED)
       do nm = this%settings%nmlb, this%settings%nmub
          td(1, nm)      = 0.0_fp
          preload(1, nm) = 0.0_fp
          load = 0.0_fp
          do k = 2, this%settings%nlyr
              do l = 1, this%settings%nfrac
                  load = load + msed(l, k-1, nm)
              enddo
              td(k, nm) = 0.0_fp
              preload(k, nm) = load
          enddo
       enddo
       
    case default ! BED_MIXED
       ! option not available for this bed composition model
    end select
end subroutine initpreload

end module bedcomposition_module
