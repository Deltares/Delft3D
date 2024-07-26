subroutine z_cucnp(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                 & icy       ,nsrc      ,kcs       ,kcs45     ,kcscut    , &
                 & kfu       ,kfuz0     ,kfumn0    ,kfumx0    ,kfv       , &
                 & kfvz0     ,kfvmn0    ,kfvmx0    ,dzv0      ,dzs0      , &
                 & kfs       ,kfsz0     ,kfsmn0    ,kfsmx0    ,kcu       , &
                 & u0        ,v1        ,w0        ,hu        ,hv        ,dzu0      , &
                 & guu       ,gvv       ,gvu       ,guv       ,gsqs      , &
                 & gud       ,gvd       ,guz       ,gvz       ,gsqiu     , &
                 & disch     ,umdis     ,kspu      ,mnksrc    ,dismmt    , &
                 & aak       ,bbk       ,cck       ,ddk       ,bbka      , &
                 & bbkc      ,vicuv     ,vnu2d     ,vicww     ,tgfsep    , &
                 & drhodx    ,wsu       ,wsbodyu   ,taubpu    ,taubsu    ,rxx       , &
                 & rxy       ,windsu    ,patm      ,fcorio    ,p0        , &
                 & tp        ,rlabda    ,dfu       ,deltau    ,fxw       , &
                 & ubrlsu    ,pship     ,diapl     ,rnpl      ,cfurou    , &
                 & qxk       ,qyk       ,umean     ,dps       ,s0        , &
                 & ustokes   ,gdp       )
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
!!--description-----------------------------------------------------------------
!
!    Function: The coefficient for the momentum equations are
!              computed and the stored in the arrays AAK, BBK,
!              CCK, and DDK (velocity points). For the depth-
!              averaged equations the coefficients are stored in
!              AA, BB, CC, DD (velocity points) and A, B, C, D
!              (water level points). A double sweep is used
!              to eliminate the coupling in the vertical.
! Method used: Reference : On the approximation of horizontal
!              gradients in sigma co-ordinates for bathymetry
!              with steep bottom slopes (G.S. Stelling and J.
!              van Kester - International Journal for Methods
!              in Fluids, Vol. 18 1994)
!              - Horizontal Advection in U-direction :
!                explicit, central scheme.
!              - Horizontal Advection in V-direction :
!                explicit, central scheme
!              - Horizontal Diffusion : explicit, along
!                Z-planes (3D), implicit (2DH)
!              - Vertical Advection : implicit, central scheme
!              - Vertical Diffusion : implicit
!              - roughness (partial slip) of rigid walls
!              - blockage flow by rigid sheets
!              Special approximation pressure term, based
!              on limiter to avoid artificial flow.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
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
    real(fp)                     , pointer :: drycrt
    real(fp)                     , pointer :: dryflc
    real(fp)                     , pointer :: gammax
    character(8)                 , pointer :: dpsopt
    character(6)                 , pointer :: momsol
    logical                      , pointer :: old_corio
    logical                      , pointer :: slplim
    real(fp)                     , pointer :: hdt
    real(fp)                     , pointer :: rhow
    real(fp)                     , pointer :: ag
    real(fp)                     , pointer :: vicmol
    integer                      , pointer :: iro
    integer                      , pointer :: irov
    logical                      , pointer :: wave
    logical                      , pointer :: roller
    logical                      , pointer :: xbeach
    real(fp)                     , pointer :: dzmin
    integer                      , pointer :: no_dis
	logical                      , pointer :: nf_src_mom
    real(fp), dimension(:,:,:)   , pointer :: nf_src_momu
    real(fp), dimension(:,:,:)   , pointer :: nf_src_momv
    real(fp)                     , pointer :: momrelax
!
! Global variables
!
    integer                                                         :: icx     !!  Increment in the X-dir., if ICX= NMAX
                                                                               !!  then computation proceeds in the X-
                                                                               !!  dir. If icx=1 then computation pro-
                                                                               !!  ceeds in the Y-dir.
    integer                                                         :: icy     !!  Increment in the Y-dir. (see ICX)
    integer                                                         :: j       !!  Begin pointer for arrays which have
                                                                               !!  been transformed into 1D arrays.
                                                                               !!  Due to the shift in the 2nd (M-)
                                                                               !!  index, J = -2*NMAX + 1
    integer                                                         :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: nmmax   !  Description and declaration in dimens.igs
    integer                                                         :: nmmaxj  !  Description and declaration in dimens.igs
    integer                                           , intent(in)  :: nsrc    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(7, nsrc)                      , intent(in)  :: mnksrc  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kcs     !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kcu     !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfs     !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kfsmx0  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kfsmn0  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfu     !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfumx0  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfumn0  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfvmx0  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfvmn0  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfv     !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: kspu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kcs45
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: kcscut  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kfsz0   !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kfuz0   !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kfvz0   !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 3)                   :: cfurou  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: deltau  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dfu     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: fcorio  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: fxw     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gsqiu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gsqs    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gud     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: guu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: guv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: guz     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvd     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gvv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvz     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: hu      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: hv      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: patm    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: pship   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: rlabda  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: s0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: taubpu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: taubsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: tgfsep  !!  Water elev. induced by tide gen.force
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: tp      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: umean   !  Description and declaration in esm_alloc_real.f90    
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: vnu2d   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: windsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: wsu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: wsbodyu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: vicww   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: w0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)            :: vicuv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: aak     !!  Internal work array, coupling of la-
                                                                               !!  yer velocity in (N,M,K) with water
                                                                               !!  level point left (down)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bbk     !!  Internal work array, coefficient la-
                                                                               !!  yer velocity in (N,M,K) implicit part
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bbka    !!  Internal work array
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bbkc    !!  Internal work array
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: cck     !!  Internal work array, coupling layer
                                                                               !!  velocity in (N,M,K) with water level
                                                                               !!  point right (upper)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ddk     !!  Internal work array, diagonal space
                                                                               !!  at (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: diapl   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: dzs0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: dzu0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: dzv0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: p0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rnpl    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rxx     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rxy     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: drhodx  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: qxk     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: qyk     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: ubrlsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ustokes !  Description and declaration in trisol.igs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: v1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                         , intent(in)  :: disch   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                         , intent(in)  :: umdis   !  Description and declaration in esm_alloc_real.f90
    character(1), dimension(nsrc)                     , intent(in)  :: dismmt  !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer            :: ddb
    integer            :: iada
    integer            :: iadc
    integer            :: icxy    ! MAX value of ICX and ICY 
    integer            :: idifc
    integer            :: idifd
    integer            :: idifu
    integer            :: idis
    integer            :: isrc
    integer            :: k
    integer            :: kdo
    integer            :: kenm
    integer            :: kfad
    integer            :: kk
    integer            :: kkmax
    integer            :: kmaxtl
    integer            :: kmin
    integer            :: kup
    integer            :: maskval
    integer            :: maxk
    integer            :: ndm
    integer            :: ndmd
    integer            :: ndmu
    integer            :: nm
    integer            :: nmd
    integer            :: nmdis
    integer            :: nmu
    integer            :: num
    integer            :: numd
    integer            :: numu
    real(fp)           :: advecx
    real(fp)           :: advecy
    real(fp)           :: adza
    real(fp)           :: adzb
    real(fp)           :: adzc
    real(fp)           :: area
    real(fp)           :: bdmwrp
    real(fp)           :: bdmwrs
    real(fp)           :: bi
    real(fp)           :: cbot
    real(fp)           :: cuu
    real(fp)           :: cvv
    real(fp)           :: dbk
    real(fp)           :: ddx
    real(fp)           :: ddy
    real(fp)           :: ddza
    real(fp)           :: ddzb
    real(fp)           :: ddzc
    real(fp)           :: dgeta
    real(fp)           :: dgvnm
    real(fp)           :: dpsmax
    real(fp)           :: drythreshold
    real(fp)           :: dux
    real(fp)           :: duy
    real(fp)           :: dz
    real(fp)           :: dzdo
    real(fp)           :: dzup
    real(fp)           :: ff
    real(fp)           :: fac
    real(fp)           :: facmax
    real(fp)           :: fxwl     ! local, modified fxw
    real(fp)           :: geta
    real(fp)           :: getad
    real(fp)           :: getau
    real(fp)           :: gksi
    real(fp)           :: gksid
    real(fp)           :: gksiu
    real(fp)           :: gsqi
    real(fp)           :: hl
    real(fp)           :: hr
    real(fp)           :: h0fac
    real(fp)           :: drytrsh
    real(fp)           :: hugsqs  ! HU(NM/NMD) * GSQS(NM) Depending on UMDIS the HU of point NM or NMD will be used 
    real(fp)           :: qwind
    real(fp), external :: redvic
    real(fp)           :: svvv
    real(fp)           :: thvert     ! theta coefficient for vertical advection terms
    real(fp)           :: timest
    real(fp)           :: trelaxi
    real(fp)           :: uuu
    real(fp)           :: uweir
    real(fp)           :: vih
    real(fp)           :: viznm
    real(fp)           :: viznmd
    real(fp)           :: vvvc   ! Tangential velocity component used in Coriolis term
    real(fp)           :: wsbodyul ! local, modified wsbodyu
    real(fp)           :: wsul     ! local, modified wsu
    real(fp)           :: wsumax
    real(fp)           :: www
    real(fp)           :: wavg0
!
!! executable statements -------------------------------------------------------
!
    drycrt         => gdp%gdnumeco%drycrt
    dryflc         => gdp%gdnumeco%dryflc
    dpsopt         => gdp%gdnumeco%dpsopt
    momsol         => gdp%gdnumeco%momsol
    old_corio      => gdp%gdnumeco%old_corio
    slplim         => gdp%gdnumeco%slplim
    gammax         => gdp%gdnumeco%gammax
    rhow           => gdp%gdphysco%rhow
    ag             => gdp%gdphysco%ag
    vicmol         => gdp%gdphysco%vicmol
    iro            => gdp%gdphysco%iro
    irov           => gdp%gdphysco%irov
    wave           => gdp%gdprocs%wave
    roller         => gdp%gdprocs%roller
    xbeach         => gdp%gdprocs%xbeach
    hdt            => gdp%gdnumeco%hdt
    dzmin          => gdp%gdzmodel%dzmin
    no_dis         => gdp%gdnfl%no_dis
    nf_src_mom     => gdp%gdnfl%nf_src_mom
    nf_src_momu    => gdp%gdnfl%nf_src_momu
    nf_src_momv    => gdp%gdnfl%nf_src_momv
    momrelax       => gdp%gdnfl%momrelax
    !
    call timer_start(timer_cucnp_ini, gdp)
    !
    ddb    = gdp%d%ddbound
    icxy   = max(icx, icy)
    !
    ! factor in maximum wave force 1/4 alpha rho g gammax**2 h**2 / tp /(sqrt(g h)
    facmax       = 0.25_fp*sqrt(ag)*rhow*gammax**2
    drythreshold = 0.1_fp * dryflc
    !
    if (icx==1) then
       ff = -1.0_fp
    else
       ff = 1.0_fp
    endif
    kmaxtl = 0
    drytrsh  = drycrt
    !
    ! Flag for vertical advection set to 1.0 by default = Central implicit discretisation of 
    ! advection in vertical (0.0 means 1st order upwind explicit)
    ! 
    thvert = 1.0_fp
    !
    ! Initialise arrays aak, bbk, cck and ddk for all (nm, k)
    !
    aak  = 0.0_fp
    bbk  = 1.0_fp
    cck  = 0.0_fp
    ddk  = 0.0_fp
    bbka = 0.0_fp
    bbkc = 0.0_fp
    !
    call timer_stop(timer_cucnp_ini, gdp)
    !
    call timer_start(timer_cucnp_momsol, gdp)
    if (momsol == 'mdue  ') then
       !
       ! Advection determined explicitly using multi-directional upwind method
       ! Using the whole time step
       !
       timest = 2.0_fp*hdt
       !
       ! Horizontal advection: multi-directional upwind explicit (mdue) method
       !
       call z_hormom_mdue(nmmax     ,kmax      ,icx       , &
                        & icy       ,kcs       ,kcs45     ,kcscut    , &
                        & kfu       ,kfuz0     ,kfumn0    ,kfumx0    , &
                        & kfvz0     ,kfsmn0    ,kfsmx0    , &
                        & u0        ,v1        ,hu        , &
                        & guu       ,gvv       ,gvu       ,guv       ,gsqs      , &
                        & gud       ,gvd       ,guz       ,gvz       ,gsqiu     , &
                        & ddk       ,gdp       )
    elseif (momsol == 'iupw  ' .or. momsol == 'mdui  ') then
       !
       ! Advection determined implicitly in z_uzd
       !
       timest = hdt
       !
       ! Horizontal advection: multi-directional upwind explicit (mdue) method
       !
       call z_hormom_mdue(nmmax     ,kmax      ,icx       , &
                        & icy       ,kcs       ,kcs45     ,kcscut    , &
                        & kfu       ,kfuz0     ,kfumn0    ,kfumx0    , &
                        & kfvz0     ,kfsmn0    ,kfsmx0    , &
                        & u0        ,v1        ,hu        , &
                        & guu       ,gvv       ,gvu       ,guv       ,gsqs      , &
                        & gud       ,gvd       ,guz       ,gvz       ,gsqiu     , &
                        & ddk       ,gdp       )
    elseif (momsol == 'finvol') then
       !
       ! Finite volume approach for horizontal advection
       !
       ! Temporary switch for vertical advection of horizontal momentum
       ! THVERT = 0 means explicit 1st order upwind vertical advection
       !
       thvert = 0.0_fp
       timest = hdt
       ! 
       call z_hormom_finvol(nmmax     ,kmax      ,icx       ,icy       ,kcs       , &
                          & kfu       ,kcu       ,kfv       ,kfuz0     ,kfumn0    , &
                          & kfumx0    ,kfvz0     ,kfs       ,kfsz0     ,kfsmn0    , &
                          & kfsmx0    ,u0        ,v1        ,w0        ,dzs0      , &
                          & dzu0      ,dzv0      ,s0        ,guu       ,gvv       , &
                          & gvu       ,guv       ,gsqs      ,gud       ,gvd       , &
                          & guz       ,gvz       ,ddk       ,p0        ,gdp       )
       !
    elseif (momsol == 'flood ') then
       !
       timest = hdt
       !
       ! Flooding solver for horizontal advection (both here and in z_uzd)
       !
       call z_hormom_fls(nmmax     ,kmax      ,icx       , &
                       & icy       ,kcs       ,kcs45     ,kcscut    , &
                       & kfu       ,kfuz0     ,kfumn0    ,kfumx0    ,kfv       , &
                       & kfvz0     ,kfsz0     ,kfsmn0    ,kfsmx0    ,kspu      , &
                       & u0        ,v1        ,hu        ,kfvmn0    ,kfvmx0    , &
                       & guu       ,gvv       ,gvu       ,guv       ,gsqs      , &
                       & gud       ,gvd       ,guz       ,gvz       ,gsqiu     , &
                       & qxk       ,qyk       ,dzu0      ,dzv0      ,dzs0      , &
                       & ddk       ,umean     ,dps       ,s0        ,bbka      , &
                       & bbkc      ,gdp       )
       !
       ! Reset the arrays bbka and bbkc which have been used as work arrays in z_hormom_fls
       !
       bbka = 0.0_fp
       bbkc = 0.0_fp
    else
       ! No correct advection option specified: error?
    endif
    call timer_stop(timer_cucnp_momsol, gdp)
    !
    ! Fill the array elements and the right hand side
    !
    call timer_start(timer_cucnp_rhs, gdp)
    do nm = 1, nmmax
       if (kfu(nm)==1) then
          ndm        = nm - icy
          nmu        = nm + icx
          ndmu       = nm + icx - icy
          gksi       = gvu(nm)
          do k = kfumn0(nm), kfumx0(nm)
             if (kfuz0(nm, k)==1) then
                !
                ! BAROCLINIC PRESSURE, CORIOLIS, ATMOSPHERIC PRES. and TIDE GEN. FORCE
                ! Surface gradient is fully accounted for here and not in z_uzd:
                ! -> aak = -2.0 * ag/gki
                ! -> cck =  2.0 * ag/gki
                !
                if (old_corio) then
                   vvvc = 0.25_fp*(v1(ndm, k) + v1(ndmu, k) + v1(nm, k) + v1(nmu, k))
                else
                   !
                   ! Improved implementation Coriolis term for deep areas following
                   ! Kleptsova, Pietrzak and Stelling, 2009.
                   !
                   svvv = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
                   hl   = real(dps(nm ),fp)+s0(nm)
                   hr   = real(dps(nmu),fp)+s0(nmu)
                   vvvc = (  (  v1(nm,  k)*hv(nm  )*kfv(nm  )      &
                   &          + v1(ndm, k)*hv(ndm )*kfv(ndm ))/max(drythreshold,hl)  & 
                   &       + (  v1(nmu, k)*hv(nmu )*kfv(nmu )      &
                   &          + v1(ndmu,k)*hv(ndmu)*kfv(ndmu))/max(drythreshold,hr)) &
                   &      / svvv
                endif
                aak(nm, k) = -ag/gksi
                bbk(nm, k) = 1.0_fp/timest
                cck(nm, k) = ag/gksi
                ddk(nm, k) = ddk(nm, k)                                       & 
                         & + u0(nm, k)/timest                                 &
                         & + ff*fcorio(nm)*vvvc                               &
                         & - ag/rhow*drhodx(nm, k)*kfsz0(nm, k)*kfsz0(nmu, k) &
                         & - (patm(nmu) - patm(nm))/(gksi*rhow)               &
                         & - (pship(nmu) - pship(nm))/(gksi*rhow)             &
                         & + ag*(tgfsep(nmu) - tgfsep(nm))/gksi               &
                         & - (p0(nmu, k) - p0(nm, k))/(gksi*rhow)
             endif
          enddo
       endif
    enddo
    call timer_stop(timer_cucnp_rhs, gdp)
    !
    ! 2D Weir not included
    ! energy loss for CDW (remainder structure untested yet)
    !
    call timer_start(timer_cucnp_eloss, gdp)
    call usrbrl(icx       ,icy       ,nmmax     ,kmax      ,kfu       , &
              & kspu      ,gvu       ,u0        ,v1        ,bbk       , &
              & ubrlsu    ,diapl     ,rnpl      ,gdp       )
    call timer_stop(timer_cucnp_eloss, gdp)
    !
    call timer_start(timer_cucnp_stress, gdp)
    do nm = 1, nmmax
       nmu  = nm + icx
       kmin = kfumn0(nm)
       if (kfu(nm)==1) then
          kkmax = max(kmin, kfumx0(nm))
          !
          ! WIND AND BOTTOM FRICTION
          !
          qwind  = 0.0_fp
          bdmwrp = 0.0_fp
          bdmwrs = 0.0_fp
          !
          ! BOTTOM STRESS DUE TO FLOW AND WAVES
          !
          ! Special measures for smooth inundation
          !
          ! Estimate velocity on the basis of local equilibrium by solving u(nm)
          ! with an explicit procedure. So that high velocities can be avoided
          ! during flooding (kfu=1 and u=0.) in regions with steep topography.
          ! Velocity is estimated assuming critical flow over a short-crested
          ! weir.
          ! Gates are excluded
          !
          if (dpsopt == 'DP' .or. slplim) then
             if (kfu(nm)==1 .and. abs(u0(nm,kmin)) <= 1.0e-15 .and. kspu(nm, 0) /= 4 .and. kspu(nm, 0) /= 10) then
                !
                ! cfurou(nm,1) contains u/u*
                !
                uweir      = sqrt(2.0_fp / 3.0_fp * ag * max(hu(nm), drytrsh))
                taubpu(nm) = uweir / (cfurou(nm,1)**2)
             endif
          endif
          !
          ! Slope correction for steep slopes
          !
          if (slplim) then
             nmu    = nm + icx
             dpsmax = max(-dps(nm),-dps(nmu))
             if (s0(nm) < dpsmax) then
                do k = kfumn0(nm), kfumx0(nm)
                   ddk(nm,k) = ddk(nm,k) - ag*(s0(nm)-dpsmax)/gvu(nm)
                enddo
             elseif (s0(nmu) < dpsmax) then
                do k = kfumn0(nm), kfumx0(nm)
                   ddk(nm,k) = ddk(nm,k) + ag*(s0(nmu)-dpsmax)/gvu(nm)
                enddo
             endif
          endif
          !
          ! End of special measures for smooth inundation
          !
          ! Bottom and wind shear stress
          !
          ! Apply factor h0fac to reduce wind force from cells with small depth
          ! Note that the direction of windsu is opposite to what is expected
          ! This is due to the change in sign in windtogridc.f90
          !
          h0fac = 1.0_fp
          if (windsu(nm) < 0.0_fp .and.  s0(nm)+real(dps(nm),fp) < 2.0_fp*dryflc) then
             h0fac = (s0(nm)+real(dps(nm),fp)) / (2.0_fp*dryflc)
          elseif (windsu(nm) > 0.0_fp .and.  s0(nmu)+real(dps(nmu),fp) < 2.0_fp*dryflc) then
             h0fac = (s0(nmu)+real(dps(nmu),fp)) / (2.0_fp*dryflc)
          endif          
          qwind          = h0fac*windsu(nm) / max(dzu0(nm, kkmax),drytrsh)
          cbot           = taubpu(nm)
          bdmwrp         = cbot/max(dzu0(nm, kmin),drytrsh)
          bdmwrs         = taubsu(nm)/max(dzu0(nm, kmin),drytrsh)
          bbk(nm, kmin)  = bbk(nm, kmin) + bdmwrp
          ddk(nm, kkmax) = ddk(nm, kkmax) - qwind/rhow
          ddk(nm, kmin)  = ddk(nm, kmin) + bdmwrs
          !
          ! WAVE FORCE AT SURFACE
          !
          if (wave) then
             wsumax = facmax*hu(nm)**(1.5_fp)/max(0.1_fp, 0.5_fp*(tp(nm)+tp(nmu)))
             wsul   = sign(min(abs(wsu(nm)), wsumax), wsu(nm))
             !
             ddk(nm, kkmax) = ddk(nm, kkmax) + wsul/(rhow*dzu0(nm, kkmax))          !
             !
             ! WAVE INDUCED BODY FORCE
             !
             if (roller .or. xbeach) then
                fxwl = sign(min(abs(fxw(nm)), wsumax), fxw(nm))
                do k = kfumn0(nm), kfumx0(nm)
                   ddk(nm, k) = ddk(nm, k) + fxwl/(rhow*hu(nm))
                enddo
             else
                wsbodyul = sign(min(abs(wsbodyu(nm)), wsumax), wsbodyu(nm))
                do k = kfumn0(nm), kfumx0(nm)
                   ddk(nm, k) = ddk(nm, k) + wsbodyul/(rhow*hu(nm))
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
       call z_shrwav(nmmax     ,kmax      ,icx       ,dfu       ,deltau    , &
                   & tp        ,rlabda    ,hu        ,kfu       ,ddk       , &
                   & kfumn0    ,kfumx0    ,dzu0      ,gdp       )
    endif
    call timer_stop(timer_cucnp_shrwav, gdp)
    !
    ! DISCHARGE ADDITION OF MOMENTUM
    !
    call timer_start(timer_cucnp_dismmt, gdp)
    do isrc = 1, nsrc
       nm = (mnksrc(2, isrc) + ddb) + ((mnksrc(1, isrc) - 1) + ddb)*icxy
       nmd = nm - icx
       if (dismmt(isrc) == 'Y' .and. disch(isrc) > 0.0_fp) then
          if (umdis(isrc) >= 0.0_fp) then
             nmdis  = nm
             hugsqs = hu(nm)*gsqs(nm)
          else
             nmdis  = nmd
             hugsqs = hu(nmd)*gsqs(nm)
          endif
          kk = mnksrc(3, isrc)
          if (kfu(nmdis) == 1) then
             if (kk == 0) then
                do k = 1, kmax
                   bbk(nmdis, k) = bbk(nmdis, k) + disch(isrc)/hugsqs
                   ddk(nmdis, k) = ddk(nmdis, k) + umdis(isrc)*disch(isrc)      &
                                 & /hugsqs
                enddo
             else
                bbk(nmdis, kk) = bbk(nmdis, kk) + disch(isrc)                   &
                               & /(dzu0(nmdis, kk)*gsqs(nm))
                ddk(nmdis, kk) = ddk(nmdis, kk) + umdis(isrc)*disch(isrc)       &
                               & /(dzu0(nmdis, kk)*gsqs(nm))
             !
             endif
          endif
       endif
    enddo
    if (nf_src_mom) then
       !
       ! DISCHARGE ADDITION OF MOMENTUM FROM NEARFIELD
       !
       ! Instead of weighting the nearfield momentum with the volume added via the nearfield source,
       ! it is choosen to use a trelax, based on the time step.
       ! This should force the cell velocity to get the a value "growing" fast to the nearfield momentum value.
       !
       ! Only change ddk/bbk when the momentum source in the present direction is non-negative
       ! Inverse of the relaxation time:
       !
       trelaxi = 1.0 / (momrelax*2.0*hdt)
       do nm = 1, nmmax
          if (kfu(nm) == 1) then
             do k = kfumn0(nm), kfumx0(nm)
                if (kfuz0(nm, k) == 1) then
                  do idis = 1, no_dis
                     if (icx == 1 ) then
                        if (comparereal(nf_src_momv(nm,k,idis), 0.0_fp) /= 0) then
                           ddk(nm,k) = ddk(nm,k) + nf_src_momv(nm,k,idis)*trelaxi
                           bbk(nm,k) = bbk(nm,k) + trelaxi
                        endif
                     else 
                        if (comparereal(nf_src_momu(nm,k,idis), 0.0_fp) /= 0) then
                           ddk(nm,k) = ddk(nm,k) + nf_src_momu(nm,k,idis)*trelaxi
                           bbk(nm,k) = bbk(nm,k) + trelaxi
                        endif
                     endif
                  enddo
                endif
             enddo
          endif
       enddo
    endif
    call timer_stop(timer_cucnp_dismmt, gdp)
    !
    ! VERTICAL ADVECTION AND VISCOSITY
    !
    call timer_start(timer_cucnp_advdiffv, gdp)
    do nm = 1, nmmax
       if (kfu(nm)==1 .and. kfumx0(nm)>kfumn0(nm)) then
          kmaxtl = 1
          nmu    = nm + icx
          do k = kfumn0(nm), kfumx0(nm)
             if (kfuz0(nm, k) == 1) then
                kfad = 0
                kdo  = k - 1
                kup  = k + 1
                if (k==kfumn0(nm)) then
                   kfad = 1
                   kdo  = k
                endif
                if (k==kfumx0(nm)) then
                   kfad = -1
                   kup  = k
                endif
                !
                ! Free slip between open and closed layers of a gate
                !
                iada = 1
                iadc = 1
                if (kspu(nm, 0) == 4 .or. kspu(nm, 0) == 10) then
                   iada = max(1 - (kspu(nm, kdo) + kspu(nm, k)), 0)
                   iadc = max(1 - (kspu(nm, k) + kspu(nm, kup)), 0)
                endif
                !
                dzup = dzu0(nm, k) + dzu0(nm, kup)
                dzdo = dzu0(nm, k) + dzu0(nm, kdo)
                !
                !   ADVECTION IN VERTICAL DIRECTION; W*DU/DZ
                !
                ! Is this correct at the surface when there are no neighbours?
                !
                if (thvert == 1.0_fp) then
                   !
                   ! Central implicit
                   !
                   maskval = min(abs(kcs(nm)), 2)*min(abs(kcs(nmu)), 2)
                   www     = 0.25_fp*maskval*(w0(nm, k - 1) + w0(nm, k) + w0(nmu, k - 1) + w0(nmu, k))
                   if (www < 0.0_fp) then
                      adza = -2.0_fp*www/(dzup + dzdo)*(1 - abs(kfad))
                      adzc =  2.0_fp*www/(dzup + dzdo)*(1 - abs(kfad)) + kfad*(1 + kfad)*www/dzup
                   else
                      adza = -2.0_fp*www/(dzup + dzdo)*(1 - abs(kfad)) + abs(kfad)*(-1 + kfad)*www/dzdo
                      adzc =  2.0_fp*www/(dzup + dzdo)*(1 - abs(kfad))
                   endif
                   adza = iada*adza
                   adzc = iadc*adzc
                   adzb = -adza - adzc
                else
                   !
                   ! First order upwind explicit
                   !
                   adza = 0.0_fp
                   adzc = 0.0_fp
                   adzb = 0.0_fp
                   !
                   ! downwards face
                   !
                   if (k > kfumn0(nm)) then
                       area  = guu(nm) * gvu(nm)
                       wavg0 = 0.5_fp * (w0(nm,k-1)+w0(nmu,k-1))
                       if (wavg0 > 0.0_fp) then
                           ddk(nm,k) = ddk(nm,k) + area*wavg0*u0(nm,k-1)
                       else
                           ddk(nm,k) = ddk(nm,k) + area*wavg0*u0(nm,k)
                       endif
                   endif
                   !
                   ! upwards face
                   !
                   if (k < kfumx0(nm)) then
                       area  = guu(nm) * gvu(nm)
                       wavg0 = 0.5_fp * (w0(nm,k)+w0(nmu,k))
                       if (wavg0 > 0.0_fp) then
                           ddk(nm,k) = ddk(nm,k) - area*wavg0*u0(nm,k)
                       else
                           ddk(nm,k) = ddk(nm,k) - area*wavg0*u0(nm,k+1)
                       endif
                   endif
                endif
                !
                ! Subtitution in coefficients
                !
                bbka(nm, k) = adza
                bbk (nm, k) = bbk(nm, k) + adzb
                bbkc(nm, k) = adzc
                !
                ! Vertical viscosity
                !
                !
                ! viznmd calculation 
                ! restriction is moved from Z_TURCLO to here
                !
                viznmd = 0.25_fp * (2 - kfad*(1 + kfad))                 &
                       & * (2.0_fp*vicmol + redvic(vicww(nm , kdo), gdp) &
                       &                  + redvic(vicww(nmu, kdo), gdp))
                !
                ! viznm calculation 
                ! restriction is moved from Z_TURCLO to here
                !
                viznm  = 0.25_fp * (2 + kfad*(1 - kfad))               &
                       & * (2.0_fp*vicmol + redvic(vicww(nm , k), gdp) &
                       &                  + redvic(vicww(nmu, k), gdp))
                dz    = dzu0(nm, k)
                !
                ddza = iada * 2.0_fp * viznmd / (dzdo*dz)
                ddzc = iadc * 2.0_fp * viznm  / (dzup*dz)
                ddzb = -ddza - ddzc
                !
                ! subtitution in coefficients
                !
                bbka(nm, k) = bbka(nm, k) - ddza
                bbk (nm, k) = bbk (nm, k) - ddzb
                bbkc(nm, k) = bbkc(nm, k) - ddzc
                !
                ! Effect of waves, due to Stokes drift 
                !
                ddk(nm, k) = ddk(nm,k) - ( ddzc*(ustokes(nm,kup)-ustokes(nm,k  )) -  &
                                        &  ddza*(ustokes(nm,k  )-ustokes(nm,kdo))  )               
             endif
          enddo
       endif
    enddo
    call timer_stop(timer_cucnp_advdiffv, gdp)
    !
    ! HORIZONTAL VISCOSITY
    !
    call timer_start(timer_cucnp_vih, gdp)
    if (irov>0) then
       !
       ! Stresses due to rigid walls
       !     implemented fully explicit
       !
       call z_vihrov(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                   & icy       ,kcs45     ,kcs       ,kfu       ,kfv       , &
                   & kfs       ,u0        ,v1        ,vicuv     ,vnu2d     , &
                   & gud       ,guu       ,gvd       ,gvu       ,gvz       , &
                   & ddk       ,rxx       ,rxy       ,kfuz0     ,kfvz0     , &
                   & kfsz0     ,kfumn0    ,kfumx0    ,gdp       )
    !
    ! for Crank Nicolson method: is computed in routine uzd (implicitly)
    ! for fractional step method: is computed here (explicitly)
    !
    else
       if (momsol == 'mdue  ') then 
          do nm = 1, nmmax
             nmd  = nm - icx
             ndm  = nm - icy
             ndmd = nm - icx - icy
             nmu  = nm + icx
             num  = nm + icy
             numu = nm + icx + icy
             ndmu = nm + icx - icy
             numd = nm - icx + icy
             if (kfu(nm)==1 .and. (kcu(nm)==1)) then
                do k = kfumn0(nm), kfumx0(nm)
                   if (kfuz0(nm, k)==1) then
                      gksid = gvz(nm)
                      gksiu = gvz(nmu)
                      gksi  = gvu(nm)
                      getad = gud(ndm)
                      getau = gud(nm)
                      geta  = guu(nm)
                      idifd = kfvz0(ndm, k)*kfvz0(ndmu, k)*kfuz0(ndm, k)
                      idifu = kfvz0(nm, k)*kfvz0(nmu, k)*kfuz0(num, k)
                      idifc = abs(2 - abs(kcs(nm)))*abs(2 - abs(kcs(nmu)))
                      !
                      ! EDDY VISCOSITY FOR KMAX = 1, USING LAPLACE OPERATOR
                      ! (2*VIH*(D2U/DX2 + D2U/DY2)
                      !
                      vih        = 0.5_fp*(vicuv(nm, k) + vicuv(nmu, k) + vnu2d(nm) + vnu2d(ndm))
                      dbk        = -2.0_fp*vih/(gksid*gksiu)*idifc - vih/(getau*geta)         &
                                 & *idifu - vih/(getad*geta)*idifd
                      dux        = vih/(gksiu*gksi)*idifc
                      ddx        = vih/(gksid*gksi)*idifc
                      duy        = vih/(getau*geta)*idifu
                      ddy        = vih/(getad*geta)*idifd
                      ddk(nm, k) = ddk(nm, k) + dbk*u0(nm, k) + ddx*u0(nmd, k)     &
                                 & + dux*u0(nmu, k) + ddy*u0(ndm, k)               &
                                 & + duy*u0(num, k)
                   endif
                enddo
             endif
          enddo
       else
          !
          ! Implicit upwind, so horizontal viscosity is included in z_uzd
          !
       endif
    endif
    call timer_stop(timer_cucnp_vih, gdp)
    !
    ! SOLUTION PROCEDURE SYSTEM OF EQUATIONS
    !
    call timer_start(timer_cucnp_lhs, gdp)
    if (kmaxtl > 0) then
       do nm = 1, nmmax
          if (kfumn0(nm) /= 0 .and. kfumx0(nm) > 1) then
             maxk           = kfumx0(nm)
             bi             = 1.0_fp/bbk(nm, maxk)
             bbk (nm, maxk) = 1.0_fp
             bbka(nm, maxk) = bbka(nm, maxk)*bi
             aak (nm, maxk) = aak(nm, maxk)*bi
             cck (nm, maxk) = cck(nm, maxk)*bi
             ddk (nm, maxk) = ddk(nm, maxk)*bi
          endif
       enddo
       do nm = 1, nmmax
          do k = kfumx0(nm) - 1, kfumn0(nm), -1
             bi          = 1.0_fp/(bbk(nm, k) - bbkc(nm, k)*bbka(nm, k + 1))
             bbk (nm, k) = 1.0_fp
             bbka(nm, k) = bbka(nm, k)*bi
             aak (nm, k) = (aak(nm, k) - bbkc(nm, k)*aak(nm, k + 1))*bi
             cck (nm, k) = (cck(nm, k) - bbkc(nm, k)*cck(nm, k + 1))*bi
             ddk (nm, k) = (ddk(nm, k) - bbkc(nm, k)*ddk(nm, k + 1))*bi
          enddo
       enddo
       !
       ! back sweep
       !
       do nm = 1, nmmax
          do k = kfumn0(nm) + 1, kfumx0(nm)
             aak(nm, k) = aak(nm, k) - bbka(nm, k)*aak(nm, k - 1)
             cck(nm, k) = cck(nm, k) - bbka(nm, k)*cck(nm, k - 1)
             ddk(nm, k) = ddk(nm, k) - bbka(nm, k)*ddk(nm, k - 1)
          enddo
       enddo
    endif
    call timer_stop(timer_cucnp_lhs, gdp)
end subroutine z_cucnp
