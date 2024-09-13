subroutine cucnp(dischy    ,icreep    ,dpdksi    ,s0        ,u0        , &
               & v1        ,w1        ,hu        ,hv        ,dps       ,dpu       , &
               & umean     ,guu       ,gvv       ,gvu       ,gsqs      , &
               & gvd       ,gud       ,gvz       ,gsqiu     ,qxk       , &
               & qyk       ,disch     ,umdis     ,mnksrc    ,dismmt    ,j         , &
               & nmmaxj    ,nmmax     ,kmax      ,icx       ,icy       , &
               & nsrc      ,lsecfl    ,lstsci    ,betac     ,aak       , &
               & bbk       ,cck       ,ddk       ,bbka      ,bbkc      , &
               & thick     ,sig       ,rho       ,sumrho    ,vicuv     , &
               & vnu2d     ,vicww     ,wsu       ,fxw       ,wsbodyu   , &
               & rxx       ,rxy       ,kcs       ,kcu       ,kfu       ,kfv       , &
               & kfs       ,kspu      ,kadu      ,kadv      ,dfu       ,deltau    , &
               & tp        ,rlabda    ,cfurou    ,cfvrou    ,rttfu     , &
               & r0        ,diapl     ,rnpl      ,taubpu    ,taubsu    , &
               & windsu    ,patm      ,fcorio    ,ubrlsu    ,uwtypu    , &
               & hkru      ,pship     ,tgfsep    ,dteu      ,ua        , &
               & ub        ,ustokes   ,mom_output,u1        ,s1        , &
               & nst       ,GHOSTu1   ,GHOSTv1   , &
               & kWDu      ,kWDv      ,xcor      ,ycor      , &
               & aguu      ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: cucnp.f90 6388 2016-08-01 01:05:36Z canestrelli.x $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute/cucnp.f90 $
!!--description-----------------------------------------------------------------
!
! The coefficient for the momentum equations are computed and the stored in the
! arrays AAK, BBK, CCK, and DDK (velocity points). For the depth averaged
! equations the coefficients are stored in AA, BB, CC, DD (velocity points) and
! A, B, C, D (water level points). A double sweep is used to eliminate the
! coupling in the vertical.
!
! Reference : On the approximation of horizontal gradients in sigma co-ordinates
! for bathymetry with steep bottom slopes (G.S. Stelling and J.van Kester -
! International Journal for Methods in Fluids, Vol. 18 1994)
!
! - Horizontal Advection in U-direction : depending on the flag MOMSOL. Options:
!    explicit, central scheme (WAQUA)
!    explicit, central scheme (Cyclic; Ref.: Stelling & Leendertse "Approximation
!                              of Convective Processes by Cyclic AOI methods",
!                              Proceeding 2nd ASCE Conference on Est. and Coastal
!                              Modelling, Tampa, 1991)
!    explicit, conservative scheme (Flooding Scheme-FLS; Ref.: Stelling&Duijnmeijer
!                             "A Staggered conservative scheme for every Froude
!                              number in rapidly varied shallow water flows",
!                              Num. Method in Fluids)
!
! - Horizontal Diffusion: explicit, along Z-planes (3D), implicit (2DH)
! - Vertical Advection:   implicit, central scheme
! - Vertical Diffusion:   implicit
! - roughness (partial slip) of rigid walls
! - blockage flow for structures
! - 2D turbulence model at depth points
! - Special approximation pressure term, based on limiter to avoid artificial flow.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use flow2d3d_timers
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                          , pointer :: gammax
    real(fp)                          , pointer :: hdt
    real(fp)                          , pointer :: dryflc
    integer                           , pointer :: ibaroc
    logical                           , pointer :: cstbnd
    logical                           , pointer :: old_corio
    character(8)                      , pointer :: dpsopt
    character(6)                      , pointer :: momsol
    logical                           , pointer :: slplim
    real(fp)                          , pointer :: rhow
    real(fp)                          , pointer :: rhofrac
    real(fp)                          , pointer :: ag
    real(fp)                          , pointer :: vicmol
    integer                           , pointer :: iro
    integer                           , pointer :: irov
    logical                           , pointer :: wind
    logical                           , pointer :: wave
    logical                           , pointer :: roller
    logical                           , pointer :: xbeach
    logical                           , pointer :: veg3d
    real(fp), dimension(:,:)          , pointer :: mom_m_velchange     ! momentum du/dt term
    real(fp), dimension(:,:)          , pointer :: mom_m_densforce     ! density force term in u dir
    real(fp), dimension(:,:)          , pointer :: mom_m_flowresist    ! vegetation and porous plates in u dir
    real(fp), dimension(:,:)          , pointer :: mom_m_corioforce    ! coriolis term in u dir
    real(fp), dimension(:,:)          , pointer :: mom_m_visco         ! viscosity term in u dir
    real(fp), dimension(:)            , pointer :: mom_m_pressure      ! pressure term in u dir
    real(fp), dimension(:)            , pointer :: mom_m_tidegforce    ! tide generating forces in u dir
    real(fp), dimension(:)            , pointer :: mom_m_windforce     ! wind shear in u dir
    real(fp), dimension(:)            , pointer :: mom_m_bedforce      ! bed shear in u dir
    real(fp), dimension(:,:)          , pointer :: mom_m_waveforce     ! wave forces in u dir
    integer                           , pointer :: cutcell
    logical                           , pointer :: EXCLouterVEL
    logical                           , pointer :: originalMOMENTUM
    real(fp), dimension(:)            , pointer :: sourceU
    integer                           , pointer :: removeW1qzk
    real(fp), dimension(:)            , pointer :: frict_sud
    logical                           , pointer :: vvvSECord
!
! Global variables
!
    integer                                           , intent(in)  :: nst
    integer                                           , intent(in)  :: icreep  !  Description and declaration in tricom.igs
    integer                                                         :: icx     !  Increment in the X-dir., if 
                                                                               !  icx= nmax: computation proceeds in the X-dir
                                                                               !  icx= 1   : computation proceeds in the Y-dir.
    integer                                                         :: icy     !  Increment in the Y-dir. (see ICX)
    integer                                                         :: j       !  Begin pointer for arrays which have been transformed into 1D arrays.
                                                                               !  Due to the shift in the 2nd (M-)index, J = -2*NMAX + 1
    integer                                                         :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: lsecfl  !  Description and declaration in dimens.igs
    integer                                                         :: lstsci  !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: nmmax   !  Description and declaration in dimens.igs
    integer                                                         :: nmmaxj  !  Description and declaration in dimens.igs
    integer                                           , intent(in)  :: nsrc    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(7, nsrc)                       , intent(in)  :: mnksrc  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: GHOSTu1  
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: GHOSTv1 
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kcs     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kcu     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfs     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfu     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfv     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)               :: kspu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in)  :: kadu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in)  :: kadv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub,4)       , intent(in)  :: kWDu
    integer, dimension(gdp%d%nmlb:gdp%d%nmub,4)       , intent(in)  :: kWDv
    real(fp)                                                        :: betac   !  Description and declaration in tricom.igs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: deltau  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dfu     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dpu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: aguu    
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dteu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: fcorio  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: fxw     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: xcor    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: ycor      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gsqiu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gsqs    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gud     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: guu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvd     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvz     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: hkru    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: hu      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: hv      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: patm    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: pship   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: rlabda  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: s0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: s1      !  Description and declaration in esm_alloc_real.f90
                                                                               !  Only used in case mom_output = .true.
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: taubpu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: taubsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: tgfsep  !  Water elev. induced by tide gen.force
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: tp      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: umean   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: uwtypu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: vnu2d   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: windsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: wsu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: wsbodyu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: vicww   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: w1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 3)                   :: cfurou  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 3)                   :: cfvrou  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: aak     !  Internal work array, coupling of layer velocity in (N,M,K) with water level point left (down)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bbk     !  Internal work array, coefficient layer velocity in (N,M,K) implicit part
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bbka    !  Internal work array
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bbkc    !  Internal work array
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: cck     !  Internal work array, coupling layer velocity in (N,M,K) with water level point right (upper)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ddk     !  Internal work array, diagonal space at (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: diapl   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: dpdksi  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: qxk     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: qyk     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: rho     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rnpl    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: rttfu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rxx     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rxy     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: sumrho  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: u1      !  Description and declaration in esm_alloc_real.f90
                                                                               !  Only used in case mom_output = .true.

    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ua
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ub
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ubrlsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ustokes !  Description and declaration in trisol.igs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: v1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)              :: vicuv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)        :: r0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                       :: sig     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                       :: thick   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                         , intent(in)  :: disch   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                         , intent(in)  :: umdis   !  Description and declaration in esm_alloc_real.f90
    logical                                           , intent(in)  :: mom_output
    character(1), dimension(nsrc)                     , intent(in)  :: dismmt  !  Description and declaration in esm_alloc_char.f90
    character(8)                                      , intent(in)  :: dischy  !  Description and declaration in tricom.igs
!
! Local variables
!
    integer                    :: ddb
    integer                    :: iada
    integer                    :: iadc
    integer                    :: icxy    ! MAX value of ICX and ICY
    integer                    :: isrc
    integer                    :: k
    integer                    :: kdo
    integer                    :: kfw
    integer                    :: kk
    integer                    :: kup
    integer                    :: kspu0k
    integer                    :: maskval
    integer                    :: maskvalf
    integer                    :: nbaroc  ! No barocline pressure on open boundary points if IBAROC = 0
    integer                    :: ndm
    integer                    :: ndmd
    integer                    :: ndmu
    integer                    :: nm
    integer                    :: nmd
    integer                    :: nmdis
    integer                    :: nmu
    integer                    :: num
    integer                    :: numu
    real(fp)                   :: adza
    real(fp)                   :: adzb
    real(fp)                   :: adzc
    real(fp)                   :: ap
    real(fp)                   :: ap1
    real(fp)                   :: ap2
    real(fp)                   :: apd
    real(fp)                   :: apu
    real(fp)                   :: bdmwrp
    real(fp)                   :: bdmwrs
    real(fp)                   :: bi
    real(fp)                   :: corioforce
    real(fp)                   :: cnurh
    real(fp)                   :: ddza
    real(fp)                   :: ddzb
    real(fp)                   :: ddzc
    real(fp)                   :: densforce
    real(fp)                   :: dia
    real(fp)                   :: drythreshold
    real(fp), dimension(1,1,1) :: dummy
    real(fp)                   :: dpsmax
    real(fp)                   :: facmax
    real(fp)                   :: ff
    real(fp)                   :: flowresist
    real(fp)                   :: fxwl     ! local, modified fxw
    real(fp)                   :: gksi
    real(fp)                   :: h0i
    real(fp)                   :: hl
    real(fp)                   :: hr
    real(fp)                   :: hugsqs  ! HU(NM/NMD) * GSQS(NM) Depending on UMDIS the HU of point NM or NMD will be used
    real(fp)                   :: pressure
    real(fp)                   :: qwind
    real(fp),external          :: redvic
    real(fp)                   :: rn
    real(fp)                   :: rhou
    integer                    :: svvv
    real(fp)                   :: tidegforce
    real(fp)                   :: tsg1
    real(fp)                   :: tsg2
    real(fp)                   :: umod
    real(fp)                   :: uuu
    real(fp)                   :: uweir
    real(fp)                   :: viz1
    real(fp)                   :: viz2
    real(fp)                   :: vvv
    real(fp)                   :: vvvc   ! Tangential velocity component used in Coriolis term
    real(fp)                   :: wlpress
    real(fp)                   :: wsbodyul ! local, modified wsbodyu
    real(fp)                   :: wsul     ! local, modified wsu
    real(fp)                   :: wsumax
    real(fp)                   :: www
    real(fp),allocatable       :: advecx(:,:)
    real(fp),allocatable       :: advecy(:,:)
!
!! executable statements -------------------------------------------------------
!
    cutcell          => gdp%gdimbound%cutcell
    EXCLouterVEL     => gdp%gdimbound%EXCLouterVEL
    originalMOMENTUM => gdp%gdimbound%originalMOMENTUM
    sourceU          => gdp%gdimbound%sourceU
    removeW1qzk      => gdp%gdimbound%removeW1qzk
    frict_sud        => gdp%gdimbound%frict_sud
    vvvSECord        => gdp%gdimbound%vvvSECord
    gammax           => gdp%gdnumeco%gammax
    hdt              => gdp%gdnumeco%hdt
    dryflc           => gdp%gdnumeco%dryflc
    ibaroc           => gdp%gdnumeco%ibaroc
    cstbnd           => gdp%gdnumeco%cstbnd
    old_corio        => gdp%gdnumeco%old_corio
    dpsopt           => gdp%gdnumeco%dpsopt
    momsol           => gdp%gdnumeco%momsol
    slplim           => gdp%gdnumeco%slplim
    rhow             => gdp%gdphysco%rhow
    rhofrac          => gdp%gdphysco%rhofrac
    ag               => gdp%gdphysco%ag
    vicmol           => gdp%gdphysco%vicmol
    iro              => gdp%gdphysco%iro
    irov             => gdp%gdphysco%irov
    wind             => gdp%gdprocs%wind
    wave             => gdp%gdprocs%wave
    roller           => gdp%gdprocs%roller
    xbeach           => gdp%gdprocs%xbeach
    veg3d            => gdp%gdprocs%veg3d
    !
    ! INITIALISATION
    !
    call timer_start(timer_cucnp_ini, gdp)
    !
    if (mom_output) then
       if (icx==1) then ! solve V/N component
          mom_m_velchange  => gdp%gdflwpar%mom_n_velchange
          mom_m_densforce  => gdp%gdflwpar%mom_n_densforce
          mom_m_flowresist => gdp%gdflwpar%mom_n_flowresist
          mom_m_corioforce => gdp%gdflwpar%mom_n_corioforce
          mom_m_visco      => gdp%gdflwpar%mom_n_visco
          mom_m_pressure   => gdp%gdflwpar%mom_n_pressure
          mom_m_tidegforce => gdp%gdflwpar%mom_n_tidegforce
          mom_m_windforce  => gdp%gdflwpar%mom_n_windforce
          mom_m_bedforce   => gdp%gdflwpar%mom_n_bedforce
          mom_m_waveforce  => gdp%gdflwpar%mom_n_waveforce
       else ! solve U/M component
          mom_m_velchange  => gdp%gdflwpar%mom_m_velchange
          mom_m_densforce  => gdp%gdflwpar%mom_m_densforce
          mom_m_flowresist => gdp%gdflwpar%mom_m_flowresist
          mom_m_corioforce => gdp%gdflwpar%mom_m_corioforce
          mom_m_visco      => gdp%gdflwpar%mom_m_visco
          mom_m_pressure   => gdp%gdflwpar%mom_m_pressure
          mom_m_tidegforce => gdp%gdflwpar%mom_m_tidegforce
          mom_m_windforce  => gdp%gdflwpar%mom_m_windforce
          mom_m_bedforce   => gdp%gdflwpar%mom_m_bedforce
          mom_m_waveforce  => gdp%gdflwpar%mom_m_waveforce
       endif
    endif
    !
    ddb = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    ! factor in maximum wave force 1/4 alpha rho g gammax**2 h**2 / tp /(sqrt(g h)
    !
    facmax       = 0.25*sqrt(ag)*rhow*gammax**2
    drythreshold = 0.1_fp * dryflc
    !
    if (icx==1) then
       ff = -1.0
    else
       ff = 1.0
    endif
    !
    ! Initialise arrays aak, bbk and cck for all (nm,k)
    !
    aak = 0.0
    cck = 0.0
    !
    do k = 1, kmax
        do nm = 1, nmmax
            kspu0k = kspu(nm, 0)*kspu(nm, k)
            !
            ! For a closed gate layer or dry velocity points
            !
            if (kfu(nm) == 0 .or. kspu0k==4 .or. kspu0k==10) then
                if (mom_output) then
                    mom_m_velchange(nm, k) = mom_m_velchange(nm, k) &
                                           & + u1(nm, k)/hdt
                else
                    bbk(nm, k) = 1.0/hdt
                    ddk(nm, k) = 0.0_fp
                endif
            else
                !
                !  For 2D weir use UUU value derived from flux QXK (value at old time step)
                !
                if (abs(kspu(nm, 0))==9) then
                    uuu = qxk(nm, k)/(guu(nm)*hu(nm))
                else
                    uuu = u0(nm, k) 
                endif
                if (mom_output) then
                    mom_m_velchange(nm, k) = mom_m_velchange(nm, k) &
                                           & + (u1(nm, k) - uuu)/hdt
                    mom_m_densforce(nm, k) = mom_m_densforce(nm, k) &
                                           & - icreep*dpdksi(nm, k)
                else
                    bbk(nm, k) = 1.0/hdt
                    ddk(nm, k) = uuu/hdt - icreep*dpdksi(nm, k)
                endif
            endif
        enddo
    enddo
    call timer_stop(timer_cucnp_ini, gdp)
    !
    call timer_start(timer_cucnp_momsol, gdp)
    allocate(advecx(gdp%d%nmlb:gdp%d%nmub,kmax),advecy(gdp%d%nmlb:gdp%d%nmub,kmax))
    if (momsol == 'cyclic' .or. momsol == 'waqua ') then
       if (originalMOMENTUM) then
          call mom_cw_orig &
               &(icx       ,icy       ,nmmax     ,kmax      ,kcu       ,kcs       , &
               & kfu       ,kfv       ,kspu      ,kadu      ,kadv      ,            &
               & dps       ,s0        ,u0        ,v1        ,qxk       ,qyk       , &
               & hu        ,guu       ,gvv       ,gvd       ,gvu       ,gsqiu     , &
               & umean     ,bbk       ,ddk       ,dummy     ,dummy     ,dummy     , &
               & dummy     ,dummy     ,dummy     ,dummy     ,dummy     ,gdp) ! TODO: add mom_output
       else
       call mom_cw &
            &(icx       ,icy       ,nmmax     ,kmax      ,kcu       ,kcs       , &
            & kfu       ,kfv       ,kspu      ,kadu      ,kadv      ,            &
            & dps       ,s0        ,u0        ,v1        ,qxk       ,qyk       , &
            & hu        ,guu       ,gvv       ,gvd       ,gvu       ,gsqiu     , &
            & umean     ,bbk       ,ddk       ,dummy     ,dummy     ,dummy     , &
            & dummy     ,dummy     ,dummy     ,dummy     ,dummy     ,mom_output, &
               & u1     ,kWDu      ,kWDv      ,GHOSTu1   , &
               & GHOSTv1   ,aguu      ,advecx    ,advecy    ,nst       ,irov      , &
               & xcor      ,ycor      ,gdp)
       endif
    elseif (momsol == 'flood ') then
        call mom_fls &
             &(icx       ,icy       ,nmmax     ,kmax      ,kcu       ,kcs       , &
             & kfu       ,kfv       ,kspu      ,kadu      ,kadv      ,            &
             & dps       ,s0        ,u0        ,v1        ,qxk       ,qyk       , &
             & hu        ,guu       ,gvv       ,gvd       ,gvu       ,gsqiu     , &
             & umean     ,bbk       ,ddk       ,dummy     ,dummy     ,dummy     , &
             & dummy     ,dummy     ,dummy     ,ua        ,ub        ,thick     , &
             & mom_output,gdp)
    endif
    !extrapolate advective terms in small cut (should be done only for free slip)
    deallocate(advecx,advecy)
    call timer_stop(timer_cucnp_momsol, gdp)
    call timer_start(timer_cucnp_rhs, gdp)
    do k = 1, kmax
       nmd  = -icx
       ndm  = -icy
       ndmd = -icx - icy
       nmu  =  icx
       num  =  icy
       numu =  icx + icy
       ndmu =  icx - icy
       do nm = 1, nmmax
          nmd  = nmd  + 1
          ndm  = ndm  + 1
          ndmd = ndmd + 1
          nmu  = nmu  + 1
          num  = num  + 1
          numu = numu + 1
          ndmu = ndmu + 1
          kspu0k = kspu(nm, 0)*kspu(nm, k)
          !
          ! For an active point and not a gate or plate
          !
          if (kfu(nm)==1 .and. kspu0k /=4 .and. kspu0k /=10) then
             gksi = gvu(nm)
             svvv = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
             hl  = real(dps(nm),fp) + s0(nm)
             hr  = real(dps(nmu),fp) + s0(nmu)         
             if (       (cstbnd .and. (kcs(nm)==2 .or. kcs(nmu)==2) ) &
                    & .or. (kcs(nm)==3 .or. kcs(nmu)==3                ) .or.EXCLouterVEL ) then
                   vvv = (v1(ndm, k)*kfv(ndm)+ v1(ndmu, k)*kfv(ndmu) + v1(nm, k)  &
                       & *kfv(nm) + v1(nmu, k)*kfv(nmu))/svvv
             else
                vvv = 0.25_fp*(v1(ndm, k) + v1(ndmu, k) + v1(nm, k) + v1(nmu, k))
             endif
             if (old_corio) then
                vvvc = vvv
             else
                !
                ! Improved implementation Coriolis term for deep areas following
                ! Kleptsova, Pietrzak and Stelling, 2009.
                !
                vvvc = (  (  v1(nm,  k)*hv(nm  )*kfv(nm  )      &
                &          + v1(ndm, k)*hv(ndm )*kfv(ndm ))/max(drythreshold,hl)  & 
                &       + (  v1(nmu, k)*hv(nmu )*kfv(nmu )      &
                &          + v1(ndmu,k)*hv(ndmu)*kfv(ndmu))/max(drythreshold,hr)) &
                &      / svvv
             endif
             uuu  = u0(nm, k) 
             umod = sqrt(uuu*uuu + vvv*vvv)
             !
             rhou = .5*(rho(nm, k) + rho(nmu, k))
             !
             ! FLAG BAROCLINE PRESSURE ON OPEN BOUNDARY (DEFAULT YES IBAROC = 1)
             !
             nbaroc = 1
             if (kcs(nm)*kcs(nmu)==2) nbaroc = ibaroc
             !
             ! SUBSTITUTION IN COEFFICIENTS,  including in DDK
             !    CORIOLIS, GRAVITY PRESSURE TERM and TIDE GENERATING FORCES
             !     remember this is not a closed gate layer
             !     KSPU(NM,0)*KSPU(NM,K)<>4, so initialisation
             !     test like in loop 100 of UZD is not necessary
             !
             wlpress    =  ag*rhofrac/gksi
             flowresist = 0.5*rttfu(nm, k)*umod
             corioforce = ff*fcorio(nm)*vvvc
             densforce  = - ag*(1. - icreep)/(gksi*rhow)*nbaroc*(sig(k)*rhou*(hr - hl) + (sumrho(nmu, k)*hr - sumrho(nm, k)*hl))
             !
             ! limit pressure term in case of drying/flooding on steep slopes
             ! note correction explicit whereas actual pressure term is implicit here (see wlpres)
             !
             if (slplim) then
                dpsmax = max(-dps(nm),-dps(nmu))
                if (s0(nm) < dpsmax) then
                   pressure = - ag*rhofrac*(s0(nm) - dpsmax)/gksi
                elseif (s0(nmu) < dpsmax) then
                   pressure = - ag*rhofrac*(dpsmax - s0(nmu))/gksi
                else
                   pressure = 0.0_fp
                endif
             else
                pressure = 0.0_fp
             endif
             pressure   = pressure                                              &
                        & - (patm(nmu) - patm(nm))/(gksi*rhow)               &
                        & - (pship(nmu) - pship(nm))/(gksi*rhow)             
             tidegforce = ag*(tgfsep(nmu) - tgfsep(nm))/gksi
             !
             if (mom_output) then
                mom_m_flowresist(nm, k) = mom_m_flowresist(nm, k) &
                                        & - flowresist*u1(nm, k)
                mom_m_densforce(nm, k)  = mom_m_densforce(nm, k) + densforce
                mom_m_corioforce(nm, k) = mom_m_corioforce(nm, k) + corioforce
                if (k==1) then
                   mom_m_pressure(nm)   = mom_m_pressure(nm) &
                                        & + pressure - wlpress*(s1(nmu) - s1(nm))
                   mom_m_tidegforce(nm) = mom_m_tidegforce(nm) + tidegforce 
                endif
             else
                aak(nm, k) = - wlpress
                cck(nm, k) = + wlpress
                bbk(nm, k) = bbk(nm, k) + flowresist
                ddk(nm, k) = ddk(nm, k) + corioforce + densforce + pressure + tidegforce
             endif
          endif
       enddo
    enddo
    call timer_stop(timer_cucnp_rhs, gdp)
    !
    ! energy loss: e.g. local weirs, rigid sheets
    !
    call timer_start(timer_cucnp_eloss, gdp)
    if (kmax == 1) then
       call usrbrl2d(icx       ,icy       ,nmmax     ,kmax      ,kfu       , &
                   & kspu      ,guu       ,gvu       ,qxk       ,bbk       , &
                   & ddk       ,ubrlsu    ,dps       ,hkru      ,s0        , &
                   & hu        ,umean     ,thick     ,dteu      ,taubpu    , &
                   & gdp       )
    endif
    call usrbrl(icx       ,icy       ,nmmax     ,kmax      ,kfu       , &
              & kspu      ,gvu       ,u0        ,v1        ,bbk       , &
              & ubrlsu    ,diapl     ,rnpl      ,gdp       )
    call timer_stop(timer_cucnp_eloss, gdp)
    !
    ndm = -icy
    kdo = max(1, kmax - 1)
    kup = min(kmax, 2)
    !
    call timer_start(timer_cucnp_stress, gdp)
    do nm = 1, nmmax
       ndm = ndm + 1
       if (kfu(nm)==1) then
          h0i = 1./hu(nm)
          !
          ! Special measures for inundation
          !
          ! Estimate velocity on the basis of local equilibrium by solving u(nm)
          ! with an explicit procedure. So that high velocities can be avoided
          ! during flooding (kfu=1 and u=0.) in regions with steep topography.
          ! Velocity is estimated assuming critical flow over a short-crested
          ! weir. 
          ! Gates are excluded
          !
          if (dpsopt == 'DP  ' .or. slplim) then
             if (kfu(nm) == 1 .and. abs(u0(nm,kmax)) <= 1.0e-15 .and. kspu(nm, 0) /= 4 .and. kspu(nm, 0) /= 10) then
                !
                ! cfurou(nm,1) contains u/u*
                !
                uweir      = sqrt(2.0 / 3.0 * ag * hu(nm))
                taubpu(nm) = uweir / (cfurou(nm,1)**2)
             endif
          endif
          !
          ! WIND FRICTION AND BOTTOM STRESS DUE TO FLOW AND WAVES
          !

          qwind  = h0i*windsu(nm)/thick(1)
          if (mom_output) then
             mom_m_windforce(nm)     = mom_m_windforce(nm) - qwind/rhow
          else
             ddk(nm, 1)    = ddk(nm, 1) - qwind/rhow
          endif
          !
          bdmwrp = h0i*taubpu(nm)/thick(kmax)
          bdmwrs = h0i*taubsu(nm)/thick(kmax)
          if (mom_output) then
             mom_m_bedforce(nm)      = mom_m_bedforce(nm) &
                                     & + bdmwrs - bdmwrp*u1(nm, kmax)
          else
             bbk(nm, kmax) = bbk(nm, kmax) + bdmwrp
             ddk(nm, kmax) = ddk(nm, kmax) + bdmwrs
          endif
          !
          ! WAVE STRESS AT SURFACE
          ! Physical limit to WSU to prevent unrealistically large stress in shallow water
          !
          if (wave) then
             wsumax = facmax*hu(nm)**(1.5)/max(0.1_fp, tp(nm))
             wsul   = sign(min(abs(wsu(nm)), wsumax), wsu(nm))
             !
             if (mom_output) then
                mom_m_waveforce(nm, 1)  = mom_m_waveforce(nm, 1) &
                                        & + wsul*h0i/(rhow*thick(1))
             else
                ddk(nm, 1) = ddk(nm, 1) + wsul*h0i/(rhow*thick(1))
             endif
             !
             ! WAVE INDUCED BODY FORCE
             !
             if (roller .or. xbeach) then
                fxwl = sign(min(abs(fxw(nm)), wsumax), fxw(nm))
                do k = 1, kmax
                   if (mom_output) then
                      mom_m_waveforce(nm, k)  = mom_m_waveforce(nm, k) &
                                              & + fxwl*h0i/rhow
                   else
                      ddk(nm, k) = ddk(nm, k) + fxwl*h0i/rhow
                   endif
                enddo
             else
                wsbodyul = sign(min(abs(wsbodyu(nm)), wsumax), wsbodyu(nm))
                do k = 1, kmax
                   if (mom_output) then
                      mom_m_waveforce(nm, k)  = mom_m_waveforce(nm, k) &
                                              & + wsbodyul*h0i/rhow
                   else
                      ddk(nm, k) = ddk(nm, k) + wsbodyul*h0i/rhow
                   endif
                enddo
             endif
          endif
       endif
    enddo
    call timer_stop(timer_cucnp_stress, gdp)
    !
    ! In case of 3D waves:
    ! Added shear stress in wave boundary layer due to streaming
    !
    call timer_start(timer_cucnp_shrwav, gdp)
    if (wave .and. kmax>1) then
       call shrwav(nmmax     ,kmax      ,icx       ,dfu       ,deltau    , &
                 & tp        ,rlabda    ,hu        ,kfu       , &
                 & ddk       ,thick     ,gdp       )
    endif
    call timer_stop(timer_cucnp_shrwav, gdp)
    !
    ! DISCHARGE ADDITION OF MOMENTUM
    !
    call timer_start(timer_cucnp_dismmt, gdp)
    do isrc = 1, nsrc
       nm = (mnksrc(5, isrc) + ddb) + ((mnksrc(4, isrc) - 1)*icxy + ddb)
       nmd = nm - icx
       if (dismmt(isrc)=='Y' .and. disch(isrc)>0.0) then
          if (umdis(isrc)>=0.0) then
             nmdis = nm
             hugsqs = hu(nm)*gsqs(nm)
          else
             nmdis = nmd
             hugsqs = hu(nmd)*gsqs(nm)
          endif
          kk = mnksrc(6, isrc)
          if (kfu(nmdis)==1) then
             if (kk==0) then
                do k = 1, kmax
                   bbk(nmdis, k) = bbk(nmdis, k) + disch(isrc)/hugsqs
                   ddk(nmdis, k) = ddk(nmdis, k) + umdis(isrc)*disch(isrc)      &
                                 & /hugsqs
                enddo
             else
                bbk(nmdis, kk) = bbk(nmdis, kk) + disch(isrc)/(thick(kk)*hugsqs)
                ddk(nmdis, kk) = ddk(nmdis, kk) + umdis(isrc)*disch(isrc)       &
                               & /(thick(kk)*hugsqs)
             endif
          endif
       endif
    enddo
    call timer_stop(timer_cucnp_dismmt, gdp)
    !
    ! VERTICAL ADVECTION AND VISCOSITY, IMPLICIT
    !
    call timer_start(timer_cucnp_advdiffv, gdp)
    if (kmax>1) then
       do k = 1, kmax
          kfw = 0
          kdo = k - 1
          kup = k + 1
          if (k==1) then
             kfw = -1
             kdo = 1
          endif
          if (k==kmax) then
             kfw = 1
             kup = kmax
          endif
          tsg1 = thick(kdo) + thick(k)
          tsg2 = thick(k) + thick(kup)
          ndm = -icy
          nmu = icx
          do nm = 1, nmmax
             ndm = ndm + 1
             nmu = nmu + 1
             if (kfu(nm)==1) then
                maskval = min(kcs(nm), 2)*min(kcs(nmu), 2)
                !
                ! Free slip between open and closed layers of a gate
                !
                iada = 1
                iadc = 1
                if (kspu(nm, 0)==4 .or. kspu(nm, 0)==10) then
                   iada = max(1 - (kspu(nm, kdo) + kspu(nm, k)), 0)
                   iadc = max(1 - (kspu(nm, k) + kspu(nm, kup)), 0)
                endif
                h0i = 1.0/hu(nm)
                if (.not. veg3d) then
                   ap1 = 1.0
                   ap2 = 1.0
                else
                   !
                   ! (Rigid) 3D Vegetation Model
                   !
                   dia  = 0.5 * ( diapl(nm,kdo) + diapl(nmu,kdo) )
                   rn   = 0.5 * (  rnpl(nm,kdo) +  rnpl(nmu,kdo) )
                   apd  = 1.0 - dia*dia*rn*pi*0.25
                   dia  = 0.5 * ( diapl(nm,k  ) + diapl(nmu,k  ) )
                   rn   = 0.5 * (  rnpl(nm,k  ) +  rnpl(nmu,k  ) )
                   ap   = 1.0 - dia*dia*rn*pi*0.25
                   dia  = 0.5 * ( diapl(nm,kup) + diapl(nmu,kup) )
                   rn   = 0.5 * (  rnpl(nm,kup) +  rnpl(nmu,kup) )
                   apu  = 1.0 - dia*dia*rn*pi*0.25
                   ap1  = (ap+apd)/2.0
                   ap2  = (ap+apu)/2.0
                endif
                !
                ! vertical viscosity
                !
                cnurh = h0i * h0i
                !
                ! viz1 calculation 
                ! restriction is moved from TURCLO to here
                !
                viz1  = 0.25 * (2 + kfw*(1 - kfw)) * ap1             &
                      & * (2.0*vicmol + redvic(vicww(nm , kdo), gdp) &
                      &               + redvic(vicww(nmu, kdo), gdp))
                !
                ! viz1 calculation 
                ! restriction is moved from TURCLO to here
                !
                viz2 = 0.25 * (2 - kfw*(1 + kfw)) * ap2            &
                     & * (2.0*vicmol + redvic(vicww(nm , k), gdp)  &
                     &               + redvic(vicww(nmu, k), gdp))
                ddza = 2.0 * cnurh * viz1 / (tsg1*thick(k))
                ddzc = 2.0 * cnurh * viz2 / (tsg2*thick(k))
                !
                ddza = iada * ddza
                ddzc = iadc * ddzc
                ddzb = -ddza - ddzc
                !
                ! substitution in coefficients
                !
                bbka(nm, k) = -ddza
                bbk (nm, k) = bbk(nm, k) - ddzb
                bbkc(nm, k) = -ddzc
                !
                ! Effect of waves, due to Stokes drift
                !
                if (mom_output) then
                   mom_m_waveforce(nm, k)  = mom_m_waveforce(nm, k) &
                                        & - ( ddza*(ustokes(nm,kdo)-ustokes(nm,k  )) -   &
                                           &  ddzc*(ustokes(nm,k  )-ustokes(nm,kup))   )
                else
                   ddk(nm, k) = ddk(nm,k) - ( ddza*(ustokes(nm,kdo)-ustokes(nm,k  )) -   &
                                           &  ddzc*(ustokes(nm,k  )-ustokes(nm,kup))   )
                endif
                !
                ! advection in vertical direction; w*du/dz
                !
                !maskvalf = min(kfs(nm), 1)*min(kfs(nmu), 1) !for cut cells !not needed! if velocity is active  kfs=1 at both sides
                www = .25*abs(maskval)*(w1(nm, k - 1) + w1(nm, k) + w1(nmu, k - 1)   &
                    & + w1(nmu, k)) !*maskvalf
             !   if (removeW1qzk==2)  www = 0._fp
                if (www<0.0) then
                   adza = 2.0*www*h0i*tsg2/(tsg1*(tsg1 + tsg2))*(1 - abs(kfw))  &
                        & + kfw*(1 + kfw)*www*h0i/tsg1
                   adzc = -2.0*www*h0i*tsg1/(tsg2*(tsg1 + tsg2))*(1 - abs(kfw))
                else
                   adza = 2.0*www*h0i*tsg2/(tsg1*(tsg1 + tsg2))*(1 - abs(kfw))
                   adzc = -2.0*www*h0i*tsg1/(tsg2*(tsg1 + tsg2))*(1 - abs(kfw)) &
                        & + abs(kfw)*( - 1 + kfw)*www*h0i/tsg2
                endif
                !
                adza = iada*adza
                adzc = iadc*adzc
                adzb = -adza - adzc
                !
                ! substitution in coefficients
                !
                bbka(nm, k) = bbka(nm, k) + adza
                bbk (nm, k) = bbk (nm, k) + adzb
                bbkc(nm, k) = bbkc(nm, k) + adzc
             else
                !
                ! no implicit vertical part
                !
                bbka(nm, k) = 0.0
                bbkc(nm, k) = 0.0
             endif
          enddo
       enddo
    !
    endif
    call timer_stop(timer_cucnp_advdiffv, gdp)
    !
    ! HORIZONTAL VISCOSTY
    !
    call timer_start(timer_cucnp_vih, gdp)
    if (irov>0) then
       !
       ! Stresses due to rigid walls
       !     implemented fully explicit
       !
       call vihrov(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                 & icy       ,kcs       ,kfu       ,kfv       ,kfs       , &
                 & u0        ,v1        ,vicuv     ,vnu2d     ,guu       , &
                 & gvv       ,gvu       ,ddk       ,rxx       ,rxy       , &
                 & gdp       ,kWDu      ,kWDv)
    endif
    call timer_stop(timer_cucnp_vih, gdp)
    if (kmax>1) then
       !
       !***SCALE ROWS OF MATRIX/RIGHT HAND SIDE VECTOR
       !
       call timer_start(timer_cucnp_rowsc, gdp)
       do k = 1, kmax
          do nm = 1, nmmax
             if (kfu(nm)==1) then
                bi          = 1.0_fp / bbk(nm, k)
                aak (nm, k) = aak (nm, k) * bi
                bbk (nm, k) = 1.0_fp
                bbka(nm, k) = bbka(nm, k) * bi
                bbkc(nm, k) = bbkc(nm, k) * bi
                cck (nm, k) = cck (nm, k) * bi
                ddk (nm, k) = ddk (nm, k) * bi
             endif
          enddo
       enddo
       call timer_stop(timer_cucnp_rowsc, gdp)
       !
       ! SOLUTION PROCEDURE SYSTEM OF EQUATIONS IN VERTICAL DIRECTION
       !
       !
       ! Division by the pivot for k=1 is not needed anymore
       ! because of row scaling
       !
       call timer_start(timer_cucnp_lhs, gdp)
       do k = 2, kmax
          do nm = 1, nmmax
             if (kfu(nm)==1) then
                bi = 1./(bbk(nm, k) - bbka(nm, k)*bbkc(nm, k - 1))
                bbk(nm, k) = 1.0
                bbkc(nm, k) = bbkc(nm, k)*bi
                aak(nm, k) = (aak(nm, k) - bbka(nm, k)*aak(nm, k - 1))*bi
                cck(nm, k) = (cck(nm, k) - bbka(nm, k)*cck(nm, k - 1))*bi
                ddk(nm, k) = (ddk(nm, k) - bbka(nm, k)*ddk(nm, k - 1))*bi
             endif
          enddo
       enddo
       !
       ! back sweep
       !
       do k = kmax - 1, 1, -1
          do nm = 1, nmmax
             if (kfu(nm)==1) then
                aak(nm, k) = aak(nm, k) - bbkc(nm, k)*aak(nm, k + 1)
                cck(nm, k) = cck(nm, k) - bbkc(nm, k)*cck(nm, k + 1)
                ddk(nm, k) = ddk(nm, k) - bbkc(nm, k)*ddk(nm, k + 1)
             endif
          enddo
       enddo
       call timer_stop(timer_cucnp_lhs, gdp)
       !
    elseif (lsecfl/=0) then
       !
       ! STRESSES DUE TO SECONDARY FLOW (SPIRAL MOTION INTENSITY)
       !     IMPLEMENTED FULLY EXPLICIT, ACCORDING 3D CASE
       !
       call timer_start(timer_cucnp_vihsec, gdp)
       call vihsec(u0        ,v1        ,guu       ,gvu       ,gvv       , &
                 & r0        ,icx       ,icy       ,j         ,nmmaxj    , &
                 & nmmax     ,kmax      ,lsecfl    ,lstsci    ,betac     , &
                 & kfu       ,kfv       ,ddk       ,cfurou    ,cfvrou    , &
                 & rxx       ,rxy       ,gdp       )
       call timer_stop(timer_cucnp_vihsec, gdp)
    else
    endif
end subroutine cucnp
