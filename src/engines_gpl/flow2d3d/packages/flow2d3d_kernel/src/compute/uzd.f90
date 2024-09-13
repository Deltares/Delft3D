recursive subroutine uzd(icreep    ,dpdksi    ,s0        ,u0        , &
             & u1        ,v         ,u0INTv    ,v0INTu    ,w1        ,umean     , &
             & hu        ,hv        ,guu       ,gvv       ,gvu       ,gsqs      , &
             & gvd       ,gud       ,gvz       ,gsqiu     ,qxk       ,qyk       , &
             & disch     ,umdis     ,dismmt    ,mnksrc    ,kcu       , &
             & kcs       ,kfu       ,kfv       ,kfs       , &
             & kspu      ,kadu      ,kadv      ,norow     ,icx       ,icy       , &
             & irocol    ,j         ,nmmaxj    ,nmmax     ,kmax      , &
             & nmax      ,mmax      ,nmaxus    ,&
             & nsrc      ,lsecfl    ,lstsci    ,betac     ,nst       , &
             & aak       ,bbk       ,cck       ,ddk       ,bddx      , &
             & bdx       ,bux       ,buux      ,bddy      ,bdy       , &
             & buy       ,buuy      ,uvdwk     ,vvdwk     ,ua        , &
             & ub        ,taubpu    ,taubsu    ,rho       ,sumrho    , &
             & thick     ,sig       ,dps       ,wsu       ,fxw       ,wsbodyu   , &
             & vicuv     ,vnu2d     ,vicww     ,rxx       ,rxy       , &
             & dfu       ,deltau    ,tp        ,rlabda    , &
             & diapl     ,rnpl      ,ghostU1   ,ghostV1   , &
             & cfurou    ,cfvrou    ,rttfu     ,r0        ,windsu    , &
             & patm      ,fcorio    ,ubrlsu    ,hkru      , &
             & pship     ,tgfsep    ,dteu      ,ustokes   ,mom_output, &
             & xG_L      ,yG_L      ,kWDu      ,kWDv      ,aguu      , &
             & xcor      ,ycor      ,gdp       )
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
!  $Id: uzd.f90 6388 2016-08-01 01:05:36Z canestrelli.x $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute/uzd.f90 $
!!--description-----------------------------------------------------------------
!
! This subroutine evaluates/solves at each half time
! step the momentum equation with implicit advection
! approximation, and implicit diffusion in the ver-
! tical direction.
! Reference : A.O.I. - scheme (G.S. Stelling and
! J.J. Leendertse, Approximation of Convective
! Processes by Cyclic AOI Methods, Proceedings,
! ASCE Conference, Tampa, 1991).
! - Horizontal Advection in U-direction: depending on the flag MOMSOL. Options: 
!    implicit, central scheme (WAQUA)
!    implicit, central scheme (Cyclic; Ref.: Stelling & Leendertse "Approximation 
!                              of Convective Processes by Cyclic AOI methods", 
!                              Proceeding 2nd ASCE Conference on Est. and Coastal
!                              Modelling, Tampa, 1991)
!    explicit, conservative scheme (Flooding Scheme-FLS; Ref.: Stelling&Duijnmeijer
!                             "A Staggered conservative scheme for every Froude
!                              number in rapidly varied shallow water flows", 
!                              Num. Method in Fluids)
!
! - Horizontal Advection in V-direction: implicit, higher order upwind
! - Horizontal Diffusion:                explicit, along Z-planes
! - Vertical Advection :                 implicit, central scheme
! - Vertical Diffusion :                 implicit
! - special approximation pressure term, based on limiter to avoid artificial flow.
! - roughness (partial slip) of rigid walls
! - blockage flow for structures
! - 2D turbulence model at depth points
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use globaldata
    use dfparall
    use flow2d3d_timers
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'flow_steps_f.inc'
    real(fp)                          , pointer :: eps
    integer                           , pointer :: lundia
    real(fp)                          , pointer :: dryflc
    real(fp)                          , pointer :: gammax
    real(fp)                          , pointer :: hdt
    integer                           , pointer :: ibaroc
    logical                           , pointer :: cstbnd
    character(6)                      , pointer :: momsol
    logical                           , pointer :: old_corio
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
    integer                           , pointer :: mfg
    integer                           , pointer :: nfg
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
    integer                           , pointer :: lunscr
    integer                           , pointer :: nmlb
    integer                           , pointer :: nmub
    integer                           , pointer :: mlb
    integer                           , pointer :: mub
    integer                           , pointer :: nlb
    integer                           , pointer :: nub
    integer                           , pointer :: cutcell
    integer                           , pointer :: GHOSTIMPL
    integer                           , pointer :: GhostMethod
    logical                           , pointer :: printUZDITERghost
    logical                           , pointer :: EXCLouterVEL
    logical                           , pointer :: periodSURFACE
    logical                           , pointer :: TRANSVperIMPL
    logical                           , pointer :: WAQUAfullyCENTRED
    logical                           , pointer :: CYCLICtransCENTR
    integer                           , pointer :: PERIODalongM
    logical                           , pointer :: originalMOMENTUM
    logical                           , pointer :: HORIZviscZERO
    logical                           , pointer :: corrSURFslopeUZD
    integer, dimension(:,:)           , pointer :: por012
    integer, dimension(:,:)           , pointer :: KFS_CC
    logical                           , pointer :: deferredS1uzd
    real(fp), dimension(:,:)          , pointer :: xG
    real(fp), dimension(:,:)          , pointer :: yG
    real(fp), dimension(:,:)          , pointer :: PSIx
    real(fp), dimension(:,:)          , pointer :: PSIy
    real(fp), dimension(:,:)          , pointer :: ETAx
    real(fp), dimension(:,:)          , pointer :: ETAy
    real(fp), dimension(:)            , pointer :: sourceU
    logical                           , pointer :: constSOLforCHECKmomTERM
    real(fp), dimension(:,:)          , pointer :: Ny
    real(fp), dimension(:,:)          , pointer :: Nx
    real(fp), dimension(:,:,:)        , pointer :: INTx_GRS
    real(fp), dimension(:,:,:)        , pointer :: INTy_GRS
    integer, dimension(:,:)           , pointer :: Ndry_GRS
    integer, dimension(:,:,:)         , pointer :: EDGEtypeBANK
    real(fp), dimension(:,:,:,:,:)    , pointer :: EDGExyBANK
    real(fp), dimension(:,:)          , pointer :: agsqs
    integer                           , pointer :: removeW1qzk
    real(fp), dimension(:,:)          , pointer :: xCORV1
    real(fp), dimension(:,:)          , pointer :: yCORV1
    real(fp), dimension(:,:)          , pointer :: xG_U1
    real(fp), dimension(:,:)          , pointer :: yG_U1
    real(fp), dimension(:,:)          , pointer :: xCORU1
    real(fp), dimension(:,:)          , pointer :: yCORU1
    real(fp), dimension(:,:)          , pointer :: xG_V1
    real(fp), dimension(:,:)          , pointer :: yG_V1
    real(fp)                          , pointer :: thrPRINTerrGRAD
    integer                           , pointer :: TYPEofFORCING
    logical                           , pointer :: testGHOSTaccur
    logical                           , pointer :: SingleLOOPuzd
    integer                           , pointer :: TYPEgradDEFERRuzd
!
! Global variables
!
    integer                                             , intent(in)  :: icreep  !  Description and declaration in tricom.igs
    integer                                             , intent(in)  :: icx     !  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                             , intent(in)  :: icy     !  Increment in the Y-dir. (see ICX)
    integer                                             , intent(in)  :: nmax
    integer                                             , intent(in)  :: mmax
    integer                                                           :: j       !  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                                           :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                           :: lsecfl  !  Description and declaration in dimens.igs
    integer                                                           :: lstsci  !  Description and declaration in esm_alloc_int.f90
    integer                                                           :: nmmax   !  Description and declaration in dimens.igs
    integer                                             , intent(in)  :: nmaxus   !  Description and declaration in dimens.igs
    integer                                                           :: nmmaxj  !  Description and declaration in dimens.igs
    integer                                             , intent(in)  :: norow   !  Description and declaration in esm_alloc_int.f90
    integer                                             , intent(in)  :: nsrc    !  Description and declaration in esm_alloc_int.f90
    integer                                             , intent(in)  :: nst     !  Time step number
    integer   , dimension(7, norow)                     , intent(in)  :: irocol  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(7, nsrc)                      , intent(in)  :: mnksrc  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: GHOSTu1
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: GHOSTv1
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kcs     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kcu     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfs     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfu     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfv     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: kspu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: kadu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: kadv    !  Description and declaration in esm_alloc_int.f90
    real(fp)                                                          :: betac   !  Description and declaration in tricom.igs
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: aguu      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: deltau  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dfu     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dteu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: fcorio  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: fxw     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gsqiu   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gsqs    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gud     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: guu     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvd     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvu     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvv     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvz     !  Description and declaration in esm_alloc_real.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub,4)       , intent(in) :: kWDu
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub,4)       , intent(in) :: kWDv
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: hkru    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: hu      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: hv      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: patm    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: pship   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: rlabda  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: s0      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: taubpu  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: taubsu  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: tgfsep  !  Water elev. induced by tide gen.force
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: tp      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: xG_L   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: yG_L   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: umean   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: vnu2d   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: windsu  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: wsu     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: wsbodyu !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: xcor    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: ycor
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: vicww   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: w1      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 3)                   :: cfurou  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 3)                   :: cfvrou  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: aak     !  Internal work array, lower diagonal tridiagonal matrix, implicit coupling of layer velocity in (N,M,K) with layer velocity in (N,M,K-1)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bbk     !  Internal work array, coefficient la=yer velocity in (N,M,K) implicit part
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bddx    !  Internal work array, implicit coupling of layer velocity in (N,M,K) with layer velocity in (N,M-2,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bddy    !  Internal work array, implicit coupling of layer velocity in (N,M,K) with layer velocity in (N-2,M,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bdx     !  Internal work array, implicit coupling of layer velocity in (N,M,K) with layer velocity in (N,M-1,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bdy     !  Internal work array, implicit coupling of layer velocity in (N,M,K) with layer velocity in (N-1,M,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: buux    !  Internal work array, implicit coupling of layer velocity in (N,M,K) with layer velocity in (N,M+2,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: buuy    !  Internal work array, implicit coupling of layer velocity in (N,M,K) with layer velocity in (N+2,M,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bux     !  Internal work array, implicit coupling of layer velocity in (N,M,K) with layer velocity in (N,M+1,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: buy     !  Internal work array, implicit coupling of layer velocity in (N,M,K) with layer velocity in (N+1,M,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: cck     !  Internal work array, upper diagonal tridiagonal matrix, implicit coupling of layer velocity in (N,M,K) with layer velocity in (N,M,K+1)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ddk     !  Internal work array, diagonal space at (N,M,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: diapl   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: dpdksi  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: qxk     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: qyk     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: rho     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rnpl    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rttfu   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rxx     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rxy     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: sumrho  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u0      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u1      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ua
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ub
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ubrlsu  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,kmax)                 :: ustokes !  Description and declaration in trisol.igs
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: uvdwk   !  Internal work array for Jac.iteration
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: v       !  V-velocities
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: v0INTu
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u0INTv
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)              :: vicuv   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: vvdwk   !  Internal work array for Jac.iteration
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)        :: r0      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax)                                       :: sig     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax)                                       :: thick   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nsrc)                         , intent(in)  :: disch   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nsrc)                         , intent(in)  :: umdis   !  Description and declaration in esm_alloc_real.f90
    logical                                             , intent(in)  :: mom_output ! true: generate momentum terms for output, false=solve equation
    character(1), dimension(nsrc)                       , intent(in)  :: dismmt  !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer            :: iterFR
    integer            :: exitloop
    integer            :: ddb
    integer            :: iada
    integer            :: iadc
    integer            :: ibf
    integer            :: ibl
    integer            :: ic
    integer            :: icxy   ! MAX value of ICX and ICY 
    integer            :: idifc
    integer            :: idifd
    integer            :: idifu  ! Work space, Identification if numeri- cal diffusive flux is added 
    integer            :: isrc
    integer            :: istat
    integer            :: iter
    integer            :: itr
    integer            :: k
    integer            :: kdo
    integer            :: kfw
    integer            :: kk
    integer            :: kup
    integer            :: kspu0k
    integer            :: maskval
    integer            :: maskvalf
    integer            :: mf
    integer            :: ml
    integer            :: n
    integer            :: nbaroc  ! No barocline pressure on open boundary points if IBAROC = 0 
    integer            :: nddm
    integer            :: nddmu
    integer            :: ndm
    integer            :: ndmd
    integer            :: ndmu
    integer            :: nhystp
    integer            :: nm
    integer            :: nmd
    integer            :: nmdd
    integer            :: nmdis
    integer            :: nmf
    integer            :: nml
    integer            :: nmsta
    integer            :: nmu
    integer            :: nmuu
    integer            :: num
    integer            :: numu
    integer            :: nuum
    integer            :: i
    integer            :: mGP
    integer            :: NGP
    integer            :: nmaxOK
    integer            :: nmMAXerr
    integer            :: nMAXerr
    integer            :: mMAXerr
    integer            :: m
    real(fp)           :: gradNUMERbutta
    real(fp)           :: adza
    real(fp)           :: adzb
    real(fp)           :: adzc
    real(fp)           :: ap
    real(fp)           :: ap1
    real(fp)           :: ap2
    real(fp)           :: apd
    real(fp)           :: apu
    real(fp)           :: bdmwrp
    real(fp)           :: bdmwrs
    real(fp)           :: bi
    real(fp)           :: cnurh
    real(fp)           :: corioforce
    real(fp)           :: dpsmax
    real(fp)           :: ddza
    real(fp)           :: ddzb
    real(fp)           :: ddzc
    real(fp)           :: densforce
    real(fp)           :: dia
    real(fp)           :: drythreshold
    real(fp)           :: facmax
    real(fp)           :: ff
    real(fp)           :: flowresist
    real(fp)           :: fxwl     ! local, modified fxw
    real(fp)           :: geta
    real(fp)           :: getad
    real(fp)           :: getau
    real(fp)           :: gksi
    real(fp)           :: gksid
    real(fp)           :: gksiu
    real(fp)           :: h0i
    real(fp)           :: hl
    real(fp)           :: hr
    real(fp)           :: hugsqs  ! HU(NM/NMD) * GSQS(NM) Depending on UMDIS the HU of point NM or NMD will be used 
    real(fp)           :: pressure
    real(fp)           :: qwind
    real(fp), external :: redvic
    real(fp)           :: rn
    real(fp)           :: rhou
    real(fp)           :: smax
    integer            :: svvv
    real(fp)           :: termc
    real(fp)           :: termdx
    real(fp)           :: termux
    real(fp)           :: termdy
    real(fp)           :: termuy
    real(fp)           :: tidegforce
    real(fp)           :: tsg1
    real(fp)           :: tsg2
    real(fp)           :: umod
    real(fp)           :: uuu
    real(fp)           :: vih
    real(fp)           :: viz1
    real(fp)           :: viz2
    real(fp)           :: vvv
    real(fp)           :: vvvc     ! Tangential velocity component used in Coriolis term
    real(fp)           :: wsbodyul ! local, modified wsbodyu
    real(fp)           :: wsul     ! local, modified wsu
    real(fp)           :: wsumax
    real(fp)           :: www
    real(fp)           :: zz
    real(fp)           :: dksi
    real(fp)           :: maxERR
    character(20)      :: errtxt
    integer            :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
!! executable statements -------------------------------------------------------
!
    cutcell                 => gdp%gdimbound%cutcell
    GHOSTIMPL               => gdp%gdimbound%GHOSTIMPL
    GhostMethod             => gdp%gdimbound%GhostMethod
    printUZDITERghost       => gdp%gdimbound%printUZDITERghost
    EXCLouterVEL            => gdp%gdimbound%EXCLouterVEL
    periodSURFACE           => gdp%gdimbound%periodSURFACE
    TRANSVperIMPL           => gdp%gdimbound%TRANSVperIMPL
    WAQUAfullyCENTRED       => gdp%gdimbound%WAQUAfullyCENTRED
    CYCLICtransCENTR        => gdp%gdimbound%CYCLICtransCENTR
    PERIODalongM            => gdp%gdimbound%PERIODalongM
    originalMOMENTUM        => gdp%gdimbound%originalMOMENTUM
    HORIZviscZERO           => gdp%gdimbound%HORIZviscZERO
    corrSURFslopeUZD        => gdp%gdimbound%corrSURFslopeUZD
    por012                  => gdp%gdimbound%por012
    KFS_CC                  => gdp%gdimbound%KFS_CC
    deferredS1uzd           => gdp%gdimbound%deferredS1uzd
    xG                      => gdp%gdimbound%xG
    yG                      => gdp%gdimbound%yG
    PSIx                    => gdp%gdimbound%PSIx
    PSIy                    => gdp%gdimbound%PSIy
    ETAx                    => gdp%gdimbound%ETAx
    ETAy                    => gdp%gdimbound%ETAy
    sourceU                 => gdp%gdimbound%sourceU
    constSOLforCHECKmomTERM => gdp%gdimbound%constSOLforCHECKmomTERM
    Ny                      => gdp%gdimbound%Ny
    Nx                      => gdp%gdimbound%Nx
    INTx_GRS                => gdp%gdimbound%INTx_GRS
    INTy_GRS                => gdp%gdimbound%INTy_GRS
    Ndry_GRS                => gdp%gdimbound%Ndry_GRS
    EDGEtypeBANK            => gdp%gdimbound%EDGEtypeBANK
    EDGExyBANK              => gdp%gdimbound%EDGExyBANK
    agsqs                   => gdp%gdimbound%agsqs
    removeW1qzk             => gdp%gdimbound%removeW1qzk
    xCORV1                  => gdp%gdimbound%xCORV1
    yCORV1                  => gdp%gdimbound%yCORV1
    xG_U1                   => gdp%gdimbound%xG_U1
    yG_U1                   => gdp%gdimbound%yG_U1
    xCORU1                  => gdp%gdimbound%xCORU1
    yCORU1                  => gdp%gdimbound%yCORU1
    xG_V1                   => gdp%gdimbound%xG_V1
    yG_V1                   => gdp%gdimbound%yG_V1
    thrPRINTerrGRAD         => gdp%gdimbound%thrPRINTerrGRAD
    TYPEofFORCING           => gdp%gdimbound%TYPEofFORCING
    testGHOSTaccur          => gdp%gdimbound%testGHOSTaccur
    SingleLOOPuzd           => gdp%gdimbound%SingleLOOPuzd
    TYPEgradDEFERRuzd       => gdp%gdimbound%TYPEgradDEFERRuzd
    eps                     => gdp%gdconst%eps
    lundia                  => gdp%gdinout%lundia
    dryflc                  => gdp%gdnumeco%dryflc
    gammax                  => gdp%gdnumeco%gammax
    hdt                     => gdp%gdnumeco%hdt
    ibaroc                  => gdp%gdnumeco%ibaroc
    cstbnd                  => gdp%gdnumeco%cstbnd
    momsol                  => gdp%gdnumeco%momsol
    old_corio               => gdp%gdnumeco%old_corio
    slplim                  => gdp%gdnumeco%slplim
    rhow                    => gdp%gdphysco%rhow
    rhofrac                 => gdp%gdphysco%rhofrac
    ag                      => gdp%gdphysco%ag
    vicmol                  => gdp%gdphysco%vicmol
    iro                     => gdp%gdphysco%iro
    irov                    => gdp%gdphysco%irov
    wind                    => gdp%gdprocs%wind
    wave                    => gdp%gdprocs%wave
    roller                  => gdp%gdprocs%roller
    xbeach                  => gdp%gdprocs%xbeach
    veg3d                   => gdp%gdprocs%veg3d
    mfg                     => gdp%gdparall%mfg
    nfg                     => gdp%gdparall%nfg
    nmlb                    => gdp%d%nmlb  
    nmub                    => gdp%d%nmub
    mlb                     => gdp%d%mlb  
    nlb                     => gdp%d%nlb  
    mub                     => gdp%d%mub  
    nub                     => gdp%d%nub  
    lunscr                  => gdp%gdinout%lunscr
    !
    !  INITIALIZE
    !
    call timer_start(timer_uzd_ini, gdp)
    !
    if (mom_output) then
       if (icx==1) then
          !
          ! solve V/N component
          !
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
       else
          !
          ! solve U/M component
          !
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
    ddb    = gdp%d%ddbound
    icxy   = max(icx, icy)
    nm_pos = 1
    !
    ! factor in maximum wave force 1/4 alpha rho g gammax**2 h**2 / tp /(sqrt(g h)
    ! = facmax * h**1.5/tp
    !
    facmax       = 0.25*sqrt(ag)*rhow*gammax**2
    drythreshold = 0.1_fp * dryflc
    !
    if (icx == 1) then
       ff = -1.0
    else
       ff = 1.0
    endif
    !
    ! Initialise all arrays aak - cck for all (nm,k)
    !
    aak  = 0.0
    buux = 0.0
    bux  = 0.0
    bdx  = 0.0
    bddx = 0.0
    buuy = 0.0
    buy  = 0.0
    bdy  = 0.0
    bddy = 0.0
    cck  = 0.0
    !
    do k = 1, kmax
       do nm = 1, nmmax
          kspu0k      = kspu(nm, 0)*kspu(nm, k)
          if (mom_output) then
             if (kspu0k==4 .or. kspu0k==10) then
                mom_m_velchange(nm, k) = mom_m_velchange(nm, k) &
                                       & + u1(nm, k)/hdt
             else
                mom_m_velchange(nm, k) = mom_m_velchange(nm, k) &
                                       & + (u1(nm, k) - u0(nm, k))/hdt
                mom_m_densforce(nm, k) = mom_m_densforce(nm, k) &
                                       & - icreep*dpdksi(nm, k)
             endif
          else
             bbk (nm, k) = 1.0/hdt
             !
             ! For a closed gate layer
             !
             if (kspu0k==4 .or. kspu0k==10) then
                ddk(nm, k) = 0.0
             else
                ddk(nm, k) = u0(nm, k)/hdt - icreep*dpdksi(nm, k)
             endif
          endif
       enddo
    enddo
    call timer_stop(timer_uzd_ini, gdp)
    !
    ! COMPUTATION OF INTERIOR POINTS
    !
    call timer_start(timer_uzd_momsol, gdp)
    !
    ! Note: kfv is turned on and then off for periodic condition 
    ! in order to have the correct value of vvv at boundaries for coriolis and transversal advection
    !
    if (periodSURFACE) then
       if ((icx==1.and.PERIODalongM==1).or.(icx/=1.and..not.PERIODalongM==1)) then
          !
          ! turn on kfu and kfv (second argument has to be the location of the tangential velocity)
          !
          CALL OFFonPERvel(kfv,kfu,icx,nlb,nub,mlb,mub,1, gdp)
       else
          !
          ! turn on kfu and kfv (second argument has to be the location of the tangential velocity)
          !
          CALL OFFonPERvel(kfu,kfv,icx,nlb,nub,mlb,mub,1, gdp)
       endif
    endif
    !
    ! It has to be done before mom_cyclic since its used in there
    !
    if (momsol == 'cyclic') then
       IF (CYCLICtransCENTR) THEN
          call mom_cyclic_transCENTR &
                &(icx       ,icy       ,nmmax     ,kmax      ,kcu       ,kcs       , &
                & kfu       ,kfv       ,kspu      ,kadu      ,kadv      ,            &
                & dps       ,s0        ,u0        ,v         ,qxk       ,qyk       , &
                & hu        ,guu       ,gvv       ,gvd       ,gvu       ,gsqiu     , &
                & umean     ,bbk       ,ddk       ,bddx      ,bddy      ,bdx       , &
                & bdy       ,bux       ,buy       ,buux      ,buuy      ,gdp) 
       ELSE
          if (originalMOMENTUM) then
             call mom_cyclic_orig &
                &(icx       ,icy       ,nmmax     ,kmax      ,kcu       ,kcs       , &
                & kfu       ,kfv       ,kspu      ,kadu      ,kadv      ,            &
                & dps       ,s0        ,u0        ,v         ,qxk       ,qyk       , &
                & hu        ,guu       ,gvv       ,gvd       ,gvu       ,gsqiu     , &
                & umean     ,bbk       ,ddk       ,bddx      ,bddy      ,bdx       , &
                & bdy       ,bux       ,buy       ,buux      ,buuy      ,gdp) 
          else
             call mom_cyclic &
                   &(icx       ,icy       ,nmmax     ,kmax      ,kcu       ,kcs       , &
                   & kfu       ,kfv       ,kspu      ,kadu      ,kadv      ,            &
                   & dps       ,s0        ,u0        ,v         ,qxk       ,qyk       , &
                   & hu        ,guu       ,gvv       ,gvd       ,gvu       ,gsqiu     , &
                   & umean     ,bbk       ,ddk       ,bddx      ,bddy      ,bdx       , &
                   & bdy       ,bux       ,buy       ,buux      ,buuy      ,mom_output, &
                   & u1        ,kWDu      ,kWDv      ,ghostU1   ,ghostV1   ,aguu      , &
                   & xcor      ,ycor      ,nst       ,gdp) 
          endif
       ENDIF
    elseif (momsol == 'waqua ') then
      IF (WAQUAfullyCENTRED) THEN
          call mom_waqua_fullyCENTRED &
                &(icx       ,icy       ,nmmax     ,kmax      ,kcu       ,kcs       , &
                & kfu       ,kfv       ,kspu      ,kadu      ,kadv      ,            &
                & dps       ,s0        ,u0        ,v         ,qxk       ,qyk       , &
                & hu        ,guu       ,gvv       ,gvd       ,gvu       ,gsqiu     , &
                & umean     ,bbk       ,ddk       ,bddx      ,bddy      ,bdx       , &
                & bdy       ,bux       ,buy       ,buux      ,buuy      ,gdp) 
       ELSE
          call mom_waqua &
                &(icx       ,icy       ,nmmax     ,kmax      ,kcu       ,kcs       , &
                & kfu       ,kfv       ,kspu      ,kadu      ,kadv      ,            &
                & dps       ,s0        ,u0        ,v         ,qxk       ,qyk       , &
                & hu        ,guu       ,gvv       ,gvd       ,gvu       ,gsqiu     , &
                & umean     ,bbk       ,ddk       ,bddx      ,bddy      ,bdx       , &
                & bdy       ,bux       ,buy       ,buux      ,buuy      ,mom_output, &
                & u1        ,ghostU1   ,ghostV1   ,aguu      ,gdp) 

       ENDIF
    elseif (momsol == 'flood ') then
        call mom_fls &
             &(icx       ,icy       ,nmmax     ,kmax      ,kcu       ,kcs       , &
             & kfu       ,kfv       ,kspu      ,kadu      ,kadv      ,            &
             & dps       ,s0        ,u0        ,v         ,qxk       ,qyk       , &
             & hu        ,guu       ,gvv       ,gvd       ,gvu       ,gsqiu     , &
             & umean     ,bbk       ,ddk       ,bddx      ,bddy      ,bdx       , &
             & bdy       ,bux       ,buy       ,ua        ,ub        ,thick     , &
             & mom_output,gdp)
    endif
    call timer_stop(timer_uzd_momsol, gdp)
    !
    call timer_start(timer_uzd_rhs, gdp)
    maxERR = -999.
    do k = 1, kmax
       nmd   = -icx
       nmdd  = -icx - icx
       ndm   = -icy
       nddm  = -icy - icy
       nddmu = -icy - icy + icx
       ndmd  = -icy - icx
       nmu   =  icx
       num   =  icy
       nuum  =  icy + icy
       numu  =  icx + icy
       nmuu  =  icx + icx
       ndmu  = -icy + icx
       do nm = 1, nmmax
          nmd    = nmd   + 1
          nmdd   = nmdd  + 1
          ndm    = ndm   + 1
          nddm   = nddm  + 1
          nddmu  = nddmu + 1
          ndmd   = ndmd  + 1
          nmu    = nmu   + 1
          num    = num   + 1
          nuum   = nuum  + 1
          numu   = numu  + 1
          nmuu   = nmuu  + 1
          ndmu   = ndmu  + 1
          kspu0k = kspu(nm, 0)*kspu(nm, k)
          !
          ! For an active point and not a gate or plate
          !
          if ( ((kcu(nm)==1) .and. (kfu(nm)==1)) .and. kspu0k /=4 .and. kspu0k /=10) then
             svvv = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
             hl   = real(dps(nm) ,fp) + s0(nm)
             hr   = real(dps(nmu),fp) + s0(nmu)
             if (        ( cstbnd .and. (kcs(nm)==2 .or. kcs(nmu)==2)) &
                  & .or. (kcs(nm)==3 .or. kcs(nmu)==3                ) .or.EXCLouterVEL ) then
                vvv  = (  v(ndm, k)*kfv(ndm) + v(ndmu, k)*kfv(ndmu) &
                     &  + v(nm , k)*kfv(nm)  + v(nmu , k)*kfv(nmu)   ) / svvv
             else
                vvv = 0.25_fp * (v(nm,k)+v(nmu,k)+v(ndm,k)+v(ndmu,k))
             endif
             if (old_corio) then
                vvvc = vvv
             else
                !
                ! Improved implementation Coriolis term for deep areas following
                ! Kleptsova, Pietrzak and Stelling, 2009.
                !
                vvvc = (  (  v(nm,  k)*hv(nm  )*kfv(nm  )      &
                &          + v(ndm, k)*hv(ndm )*kfv(ndm ))/max(drythreshold,hl)  & 
                &       + (  v(nmu, k)*hv(nmu )*kfv(nmu )      &
                &          + v(ndmu,k)*hv(ndmu)*kfv(ndmu))/max(drythreshold,hr)) &
                &      / svvv
             endif
             uuu   = u0(nm, k)
             umod  = sqrt(uuu*uuu + vvv*vvv)
             !
             rhou   = .5*(rho(nm, k) + rho(nmu, k))
             dksi  = gvu(nm) 
             !
             ! FLAG BAROCLINE PRESSURE ON OPEN BOUNDARY (DEFAULT YES IBAROC = 1)
             !
             nbaroc = 1
             if (kcs(nm)*kcs(nmu) == 2) nbaroc = ibaroc
             !
             ! CORIOLIS, GRAVITY PRESSURE TERM and TIDE GENERATING FORCES
             !
             flowresist = 0.5*rttfu(nm, k)*umod
             corioforce = ff*fcorio(nm)*vvvc
             densforce  = - ag*(1. - icreep)/(gvu(nm)*rhow)*nbaroc*(sig(k)*rhou*(hr - hl) + (sumrho(nmu, k)*hr - sumrho(nm, k)*hl))
             !
             ! limit pressure term in case of drying/flooding on steep slopes
             !
             if (slplim) then
                dpsmax = max(-dps(nm),-dps(nmu))
                if (s0(nm) < dpsmax) then
                   pressure = - ag*rhofrac*(s0(nmu) - dpsmax)/dksi
                elseif (s0(nmu) < dpsmax) then
                   pressure = - ag*rhofrac*(dpsmax  - s0(nm))/dksi
                else
                   pressure = - ag*rhofrac*(s0(nmu) - s0(nm))/dksi
                endif
             else
                pressure    = - ag*rhofrac*(s0(nmu) - s0(nm))/dksi
             endif
             pressure   = pressure                                              &
                        & - (patm(nmu) - patm(nm))/(dksi*rhow)               &
                        & - (pship(nmu) - pship(nm))/(dksi*rhow) 
             tidegforce = ag*(tgfsep(nmu) - tgfsep(nm))/dksi
             !
             if (mom_output) then
                if (comparereal(aguu(nm),0._fp)/=0) then
                   !
                   ! exclude u-ghost points with no active edge (we still have dh/dx for active edge)
                   !
                   mom_m_flowresist(nm, k) = mom_m_flowresist(nm, k) &
                                           & - flowresist*u1(nm, k)
                   mom_m_densforce(nm, k)  = mom_m_densforce(nm, k) + densforce
                   mom_m_corioforce(nm, k) = mom_m_corioforce(nm, k) + corioforce
                   if (k==1) then
                         mom_m_pressure(nm)   = mom_m_pressure(nm) + pressure  
                         mom_m_tidegforce(nm) = mom_m_tidegforce(nm) + tidegforce 
                   endif
                endif
             else
                bbk(nm, k) = bbk(nm, k) + flowresist
                ddk(nm, k) = ddk(nm, k) + corioforce + densforce + pressure + tidegforce
             endif
          endif
       enddo
    enddo
    !
    if (periodSURFACE) then
       if ((icx==1.and.PERIODalongM==1).or.(icx/=1.and..not.PERIODalongM==1)) then
          !
          ! turn off kfu and kfv (second argument has to be the location of the tangential velocity)
          !
          CALL OFFonPERvel(kfv,kfu,icy,nlb,nub,mlb,mub,0, gdp)
       else
          !
          ! turn off kfu and kfv (second argument has to be the location of the tangential velocity)
          !
          CALL OFFonPERvel(kfu,kfv,icy,nlb,nub,mlb,mub,0, gdp)
       endif
    endif
    !
    call timer_stop(timer_uzd_rhs, gdp)
    !
    ! energy loss: e.g. local weirs, rigid sheets
    !
    call timer_start(timer_uzd_eloss, gdp)
    if (kmax == 1) then
       call usrbrl2d(icx       ,icy       ,nmmax     ,kmax      ,kfu       , &
                   & kspu      ,guu       ,gvu       ,qxk       ,bbk       , &
                   & ddk       ,ubrlsu    ,dps       ,hkru      ,s0        , &
                   & hu        ,umean     ,thick     ,dteu      ,taubpu    , &
                   & gdp       )
    endif
    call usrbrl(icx       ,icy       ,nmmax     ,kmax      ,kfu       , &
              & kspu      ,gvu       ,u0        ,v         ,bbk       , &
              & ubrlsu    ,diapl     ,rnpl      ,gdp       )
    call timer_stop(timer_uzd_eloss, gdp)
    !
    ndm = -icy
    kdo = max(1, kmax - 1)
    kup = min(kmax, 2)
    !
    call timer_start(timer_uzd_stress, gdp)
    do nm = 1, nmmax
       ndm = ndm + 1
       if ((kcu(nm)==1) .and. (kfu(nm)==1)) then
          h0i = 1.0/hu(nm)
          !
          ! Adaption for inundation is not needed here (no flooding); only in cucnp
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
          ! WAVE FORCE AT SURFACE
          !
          ! physical limit to wsu
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
    call timer_stop(timer_uzd_stress, gdp)
    !
    ! In case of 3D waves:
    ! Added shear stress in wave boundary layer due to streaming
    !
    call timer_start(timer_uzd_shrwav, gdp)
    if (wave .and. kmax>1) then
       call shrwav(nmmax     ,kmax      ,icx       ,dfu       ,deltau    , &
                 & tp        ,rlabda    ,hu        ,kfu       , &
                 & ddk       ,thick     ,gdp       )
    endif
    call timer_stop(timer_uzd_shrwav, gdp)
    !
    ! DISCHARGE ADDITION OF MOMENTUM
    !
    call timer_start(timer_uzd_dismmt, gdp)
    do isrc = 1, nsrc
       nm = (mnksrc(5, isrc) + ddb) + ((mnksrc(4, isrc) - 1) + ddb)*icxy
       nmd = nm - icx
       if (dismmt(isrc)=='Y' .and. disch(isrc)>0.0) then
          if (umdis(isrc) >= 0.0) then
             nmdis = nm
             hugsqs = hu(nm)*gsqs(nm)
          else
             nmdis = nmd
             hugsqs = hu(nmd)*gsqs(nm)
          endif
          kk = mnksrc(6, isrc)
          if (kfu(nmdis) == 1) then
             if (kk == 0) then
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
    call timer_stop(timer_uzd_dismmt, gdp)
    !
    ! HORIZONTAL VISCOSTY
    !
    ! for rough  wall : is computed (explicitly)
    ! for smooth wall : is computed (implicitly)
    !
    if (periodSURFACE) then
       if ((icx==1.and.PERIODalongM==1).or.(icx/=1.and..not.PERIODalongM==1)) then
          !
          ! turn on kfu and kfv (second argument has to be the location of the tangential velocity)
          !
          CALL OFFonPERvel(kfv,kfu,icx,nlb,nub,mlb,mub,1, gdp)
       else
          !
          ! turn on kfu and kfv (second argument has to be the location of the tangential velocity)
          !
          CALL OFFonPERvel(kfu,kfv,icx,nlb,nub,mlb,mub,1, gdp)
       endif
    endif

    call timer_start(timer_uzd_vih, gdp)
    if (irov > 0) then
       !
       ! Stresses due to rigid walls
       !     implemented fully explicit
       !
       call vihrov(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                 & icy       ,kcs       ,kfu       ,kfv       ,kfs       , &
                 & u0        ,v         ,vicuv     ,vnu2d     ,guu       , &
                 & gvv       ,gvu       ,ddk       ,rxx       ,rxy       , &
                 & gdp       ,kWDu      ,kWDv)
    else
       do k = 1, kmax
          ndm  = -icy
          ndmu = -icy + icx
          num  =  icy
          nmd  = -icx
          nmu  =  icx
          do nm = 1, nmmax
             ndm  = ndm  + 1
             ndmu = ndmu + 1
             num  = num  + 1
             nmd  = nmd  + 1
             nmu  = nmu  + 1
             kspu0k = kspu(nm, 0)*kspu(nm, k)
             !
             ! For an active point and not a gate or plate
             !
             if ( ((kcu(nm)==1) .and. (kfu(nm)==1)) .and. kspu0k /=4 .and. kspu0k /=10) then
                gksid = gvz (nm)
                gksiu = gvz (nmu)
                gksi  = gvu (nm)
                getad = gud (ndm)
                getau = gud (nm)
                geta  = guu (nm)
                !
                ! In the following tests, cstbnd should be deleted
                ! It is only added to minimize differences in results
                !
                if     (kcs(nm )==2 .and. cstbnd) then
                   idifu =                        kfv(nmu )*kWDv(nmu,1)  * kfu(num )*kWDu(num,3)
                   idifd =                        kfv(ndmu)*kWDv(ndmu,2) * kfu(ndm )*kWDu(ndm,4)
                elseif (kcs(nmu)==2 .and. cstbnd) then
                   idifu = kfv(nm )*kWDv(nm,1)                           * kfu(num )*kWDu(num,3)
                   idifd = kfv(ndm)*kWDv(ndm,2)                          * kfu(ndm )*kWDu(ndm,4)
                else  
                   idifu = kfv(nm )*kWDv(nm,1)  * kfv(nmu )*kWDv(nmu,1)  * kfu(num )*kWDu(num,3)
                   idifd = kfv(ndm)*kWDv(ndm,2) * kfv(ndmu)*kWDv(ndmu,2) * kfu(ndm )*kWDu(ndm,4)
                endif
                idifc = abs(2 - abs(kcs(nm)))*abs(2 - abs(kcs(nmu)))
                !
                ! EDDY VISCOSITY FOR ALL K USING LAPLACE OPERATOR
                !     (2*VIH*(D2U/DX2 + D2U/DY2) - OPERATOR SPLITTING IS USED )
                !     VISCOSITY TERM HERE IS APPLIED ONLY IN THIS HALF TIMESTEP
                !
                vih = vicuv(nm, k) + vicuv(nmu, k) + vnu2d(nm) + vnu2d(ndm)
                if (HORIZviscZERO) vih = 0._fp
                termc  = 2.*vih/(gksid*gksiu)*idifc
                termux = vih/(gksiu*gksi)*idifc
                termdx = vih/(gksid*gksi)*idifc
                termuy = vih/(getau*geta) *idifu
                termdy = vih/(getad*geta) *idifd
                if (mom_output) then
                   mom_m_visco(nm, k)     = mom_m_visco(nm, k) &
                                          & - (termc + termuy + termdy)*u1(nm, k) &
                                          & + termux*u1(nmu, k) + termdx*u1(nmd, k) &
                                          & + termuy*u1(num, k) + termdy*u1(ndm, k)
                else
                   bbk(nm, k) = bbk(nm, k) + termc + termuy + termdy
                   bux(nm, k) = bux(nm, k) - termux
                   bdx(nm, k) = bdx(nm, k) - termdx
                   buy(nm, k) = buy(nm, k) - termuy
                   bdy(nm, k) = bdy(nm, k) - termdy
                endif
             endif
          enddo
       enddo
    endif
    call timer_stop(timer_uzd_vih, gdp)
    !
    call timer_start(timer_uzd_advdiffv, gdp)
    if (kmax > 1) then
       do k = 1, kmax
          kfw = 0
          kdo = k - 1
          kup = k + 1
          if (k == 1) then
             kfw = -1
             kdo =  1
          endif
          if (k == kmax) then
             kfw = 1
             kup = kmax
          endif
          tsg1 = thick(kdo) + thick(k)
          tsg2 = thick(k) + thick(kup)
          !
          nmu = icx
          do nm = 1, nmmax
             !
             ! ADVECTION AND DIFFUSION IN VERTICAL DIRECTION
             !
             nmu = nmu + 1
             if ((kcu(nm)==1) .and. (kfu(nm)==1)) then
                !
                ! Free slip between open and closed layers of a gate
                !
                iada = 1
                iadc = 1
                if (kspu(nm, 0)==4 .or. kspu(nm, 0)==10) then
                   iada = max(1 - (kspu(nm, kdo) + kspu(nm, k)), 0)
                   iadc = max(1 - (kspu(nm, k) + kspu(nm, kup)), 0)
                endif
                !
                h0i = 1./hu(nm)
                maskval = min(kcs(nm), 2)*min(kcs(nmu), 2)
                !maskvalf = min(kfs(nm), 1)*min(kfs(nmu), 1) !for cut cells
                www = .25*abs(maskval)*(w1(nm, k-1) + w1(nm, k) + w1(nmu, k-1) + w1(nmu, k)) !*maskvalf
                !if (removeW1qzk==2)  www = 0._fp
                if (www < 0.0) then
                   adza = 2.0*www*h0i*tsg2/(tsg1*(tsg1 + tsg2))*(1 - abs(kfw))  &
                        & + kfw*(1 + kfw)*www*h0i/tsg1
                   adzc = -2.0*www*h0i*tsg1/(tsg2*(tsg1 + tsg2))*(1 - abs(kfw))
                else
                   adza =  2.0*www*h0i*tsg2/(tsg1*(tsg1 + tsg2))*(1 - abs(kfw))
                   adzc = -2.0*www*h0i*tsg1/(tsg2*(tsg1 + tsg2))*(1 - abs(kfw)) &
                        & + abs(kfw)*( - 1 + kfw)*www*h0i/tsg2
                endif
                !
                adza = iada*adza
                adzc = iadc*adzc
                adzb = -adza - adzc
                !
                aak(nm, k) = aak(nm, k) + adza
                bbk(nm, k) = bbk(nm, k) + adzb
                cck(nm, k) = cck(nm, k) + adzc
                !
                ! DIFFUSION IN VERTICAL DIRECTION
                !
                if (.not. veg3d) then
                   ap1 = 1.0
                   ap2 = 1.0
                else
                   !
                   ! (Rigid) 3D Vegetation Model
                   !
                   dia = 0.5*(diapl(nm, kdo) + diapl(nmu, kdo))
                   rn  = 0.5*(rnpl (nm, kdo) + rnpl (nmu, kdo))
                   apd = 1.0 - dia*dia*rn*pi*0.25
                   dia = 0.5*(diapl(nm, k) + diapl(nmu, k))
                   rn  = 0.5*(rnpl (nm, k) + rnpl (nmu, k))
                   ap  = 1.0 - dia*dia*rn*pi*0.25
                   dia = 0.5*(diapl(nm, kup) + diapl(nmu, kup))
                   rn  = 0.5*(rnpl (nm, kup) + rnpl (nmu, kup))
                   apu = 1.0 - dia*dia*rn*pi*0.25
                   ap1 = (ap + apd)/2.0
                   ap2 = (ap + apu)/2.0
                endif
                cnurh = h0i * h0i
                !
                ! viz1 calculation 
                ! restriction is moved from TURCLO to here
                !
                viz1  = 0.25 * (2 + kfw*(1 - kfw)) * ap1             &
                      & * (2.0*vicmol + redvic(vicww(nm , kdo), gdp) &
                      &               + redvic(vicww(nmu, kdo), gdp))
                !
                ! viz2 calculation
                ! restriction is moved from TURCLO to here
                !
                viz2 = 0.25 * (2 - kfw*(1 + kfw)) * ap2           &
                     & * (2.0*vicmol + redvic(vicww(nm , k), gdp) &
                     &               + redvic(vicww(nmu, k), gdp))
                ddza  = 2.0 * cnurh * viz1 / (tsg1*thick(k))
                ddzc  = 2.0 * cnurh * viz2 / (tsg2*thick(k))
                !
                ddza  =  iada * ddza
                ddzc  =  iadc * ddzc
                ddzb  = -ddza - ddzc
                !
                aak(nm, k) = aak(nm, k) - ddza
                bbk(nm, k) = bbk(nm, k) - ddzb
                cck(nm, k) = cck(nm, k) - ddzc
                !
                ! Effect of waves, due to Stokes drift 
                !
                if (mom_output) then
                   mom_m_waveforce(nm, k)  = mom_m_waveforce(nm, k) &
                                          - ( ddza*(ustokes(nm,kdo)-ustokes(nm,k  )) -  &
                                           &  ddzc*(ustokes(nm,k  )-ustokes(nm,kup))   )
                else
                   ddk(nm, k) = ddk(nm,k) - ( ddza*(ustokes(nm,kdo)-ustokes(nm,k  )) -  &
                                           &  ddzc*(ustokes(nm,k  )-ustokes(nm,kup))   )
                endif
             endif
          enddo
       enddo
    !
    ! STRESSES DUE TO SECONDARY FLOW (SPIRAL MOTION INTENSITY)
    !     IMPLEMENTED FULLY EXPLICIT, ACCORDING 3D CASE
    !
    elseif (lsecfl /= 0) then
       call vihsec(u0        ,v         ,guu       ,gvu       ,gvv       , &
                 & r0        ,icx       ,icy       ,j         ,nmmaxj    , &
                 & nmmax     ,kmax      ,lsecfl    ,lstsci    ,betac     , &
                 & kfu       ,kfv       ,ddk       ,cfurou    ,cfvrou    , &
                 & rxx       ,rxy       ,gdp       )
    else
    endif
    if (periodSURFACE) then
       if ((icx==1.and.PERIODalongM==1).or.(icx/=1.and..not.PERIODalongM==1)) then
          !
          ! turn off kfu and kfv (second argument has to be the location of the tangential velocity)
          !
          CALL OFFonPERvel(kfv,kfu,icy,nlb,nub,mlb,mub,0, gdp)
       else
          !
          ! turn off kfu and kfv (second argument has to be the location of the tangential velocity)
          !
          CALL OFFonPERvel(kfu,kfv,icy,nlb,nub,mlb,mub,0, gdp)
       endif
    endif
    call timer_stop(timer_uzd_advdiffv, gdp)
    !
    ! We're done if this routine was called just for the momentum terms for output
    !
    if (mom_output) return
    !
    !==========================================================================
    !
    ! BOUNDARY CONDITIONS
    !
    call timer_start(timer_uzd_bouncond, gdp)
    do ic = 1, norow
       !
       n   = irocol(1, ic)
       mf  = irocol(2, ic) - 1
       ml  = irocol(3, ic)
       ibf = irocol(4, ic)
       ibl = irocol(5, ic)
       nmf = (n + ddb)*icy + (mf + ddb)*icx - icxy
       nml = (n + ddb)*icy + (ml + ddb)*icx - icxy
       !
       ! IMPLEMENTATION OF BOUNDARY CONDITIONS
       !
       if ( (kcu(nmf)==1) .and. (kfu(nmf)==1) ) then
          if (ibf==3 .or. ibf==5 .or. ibf==6 .or. ibf==7) then
             do k = 1, kmax
                aak (nmf, k) = 0.0
                bbk (nmf, k) = 1.0/hdt
                buux(nmf, k) = 0.0
                bux (nmf, k) = 0.0
                bdx (nmf, k) = 0.0
                bddx(nmf, k) = 0.0
                buuy(nmf, k) = 0.0
                buy (nmf, k) = 0.0
                bdy (nmf, k) = 0.0
                bddy(nmf, k) = 0.0
                cck (nmf, k) = 0.0
                ddk (nmf, k) = u0(nmf, k)/hdt
             enddo
          endif
       endif
       if ((kcu(nml)==1) .and. (kfu(nml)==1)) then
          if (ibl==3 .or. ibl==5 .or. ibl==6 .or. ibl==7) then
             do k = 1, kmax
                aak (nml, k) = 0.0
                bbk (nml, k) = 1.0/hdt
                buux(nml, k) = 0.0
                bux (nml, k) = 0.0
                bdx (nml, k) = 0.0
                bddx(nml, k) = 0.0
                buuy(nml, k) = 0.0
                buy (nml, k) = 0.0
                bdy (nml, k) = 0.0
                bddy(nml, k) = 0.0
                cck (nml, k) = 0.0
                ddk (nml, k) = u0(nml, k)/hdt
             enddo
          endif
       endif
    enddo
    call timer_stop(timer_uzd_bouncond, gdp)
    !
    ! left hand-side is now set by Delft3D-FLOW instead of the mapper
    !
    call timer_start(timer_uzd_lhs, gdp)
    do nm = 1, nmmax
       if (kcu(nm) == 3 .or. kcu(nm) == -1) then
          do k = 1, kmax
             aak(nm,k) = 0.0
             bbk(nm,k) = 1.0
             cck(nm,k) = 0.0
             ddk(nm,k) = u0(nm,k)
          enddo
       endif
    enddo
    !
    !  cut cell modification
    !
    if (cutcell.gt.0.and.(GhostMethod.eq.1.or.GhostMethod.eq.2)) then
        !
        ! Call forceGHOSTuzd with u0 and iter=0
        !
        call forceGHOSTuzd(icx        ,icy        ,u0         ,&
                        & aak        ,bbk        ,cck        ,ddk        ,&
                        & buux       ,bux        ,bdx        ,&
                        & bddx       ,buuy       ,buy        ,bdy        ,bddy       ,&
                        & mmax       ,nmax       ,kmax       ,&
                        & nst        ,nlb        ,nub        ,mlb        ,mub        ,&
                        & nmlb       ,nmub       ,0          ,.false.    ,gdp)
    endif
    !
    if (periodSURFACE) then
       CALL forcePERvelUZD(u0,bbk,ddk,aak,buux,bux,bdx,bddx,buuy,buy,bdy,bddy,cck,hdt,icx,nlb,nub,mlb,mub,kmax, gdp)
       if ((icx==1.and.PERIODalongM==1).or.(icx/=1.and..not.PERIODalongM==1)) then
          !
          ! turn on kfu and kfv (second argument has to be the location of the tangential velocity)
          !
          CALL OFFonPERvel(kfv,kfu,icy,nlb,nub,mlb,mub,1, gdp)
       else
          !
          ! turn on kfu and kfv (second argument has to be the location of the tangential velocity)
          !
          CALL OFFonPERvel(kfu,kfv,icy,nlb,nub,mlb,mub,1, gdp)
       endif
    endif
    !
    call timer_stop(timer_uzd_lhs, gdp)
    !
    ! Domain decomposition:
    !        end of "build_system_for_velocity",
    !        mapper can build the coupling equations
    !
    call timer_start(timer_uzd_rest, gdp) 
    if (icx == 1) then
       !
       ! D3dFlowMap_Build_V: poke the coupling equations into system
       !
       nhystp = nxtstp(d3dflow_build_v, gdp)
    else
       !
       ! D3dFlowMap_Build_U: poke the coupling equations into system
       !
       nhystp = nxtstp(d3dflow_build_u, gdp)
    endif
    call timer_stop(timer_uzd_rest, gdp) 
    !
    !***SCALE ROWS OF MATRIX/RIGHT HAND SIDE VECTOR
    !
    call timer_start(timer_uzd_rowsc, gdp)
    do k = 1, kmax
       do nm = 1, nmmax
          if (kfu(nm)==1) then
             bi          = 1.0_fp / bbk(nm, k)
             aak (nm, k) = aak (nm, k) * bi
             bbk (nm, k) = 1.0_fp
             buux(nm, k) = buux(nm, k) * bi
             bux (nm, k) = bux (nm, k) * bi
             bdx (nm, k) = bdx (nm, k) * bi
             bddx(nm, k) = bddx(nm, k) * bi
             buuy(nm, k) = buuy(nm, k) * bi
             buy (nm, k) = buy (nm, k) * bi
             bdy (nm, k) = bdy (nm, k) * bi
             bddy(nm, k) = bddy(nm, k) * bi
             cck (nm, k) = cck (nm, k) * bi
             ddk (nm, k) = ddk (nm, k) * bi
          endif
       enddo
    enddo
    call timer_stop(timer_uzd_rowsc, gdp)
    !
    !***SOLUTION PROCEDURE SYSTEM OF EQUATIONS
    !
    call timer_start(timer_uzd_solve1, gdp)
    !
    ! Division by the pivot for k=1 is not needed anymore
    ! because of row scaling
    !
    do k = 2, kmax
       do nm = 1, nmmax
          if (kfu(nm) == 1) then
             bi         = 1./(bbk(nm, k) - aak(nm, k)*cck(nm, k - 1))
             bbk(nm, k) = bi
             cck(nm, k) = cck(nm, k)*bi
          endif
       enddo
    enddo
    call timer_stop(timer_uzd_solve1, gdp)
    !
    ! ITERATION LOOP
    !
    call timer_start(timer_uzd_solve2, gdp)
    iter = 0
    do k = 1, kmax
       do nm = 1, nmmax
          if (kfu(nm) == 1) then
             u1   (nm, k) = u0(nm, k)
             uvdwk(nm, k) = u0(nm, k)
          endif
       enddo
    enddo
    call timer_stop(timer_uzd_solve2, gdp)
    !
    ! ensure that loop starts at point of correct color in own subdomain
    !
    if (mod(mfg+nfg,2) == 1) then
       !
       ! red points
       !
       nmsta = 1
    else
       !
       ! black points
       !
       nmsta = 2
    endif
    !
    ! Domain decomposition:
    !     resume point for next solve
    !
  222 continue
    gdp%dd%uzditer = gdp%dd%uzditer + 1
    !
    ! End Domain decomposition addition
    !
    itr = 1
    loop_iteration: do while (itr == 1 .and. iter < 50)
       iter = iter + 1
       !
       ! ITERATIVE SOLUTION METHOD USING CHECKERBOARD JACOBI
       ! IN HORIZONTAL DIRECTION
       ! ATTENTION : AN ODD NUMBER OF GRIDPOINTS IN V-DIRECTION
       !             ( NMAX ) IS ASSUMED!!!!!
       !
       itr = 0
       if (icx == 1) then
          call timer_start(timer_uzd_solve3u, gdp)
       else
          call timer_start(timer_uzd_solve5v, gdp)
       endif
       !
       ! recompute the transversal velocity point value if periodic is implicit
       !
       if (TRANSVperIMPL.AND.iter.gt.1) then !
          if ((icx==1.and.PERIODalongM==1).or.(icx/=1.and..not.PERIODalongM==1)) then
             !
             ! in this way I copy also the explicit parallel, no big deal
             !
             call velocityPERIOD(v,u1,icx,nlb,nub,mlb,mub,kmax, gdp)
          else
             !
             ! in this way I copy also the explicit parallel, no big deal
             ! 
             call velocityPERIOD(u1,v,icx,nlb,nub,mlb,mub,kmax, gdp)
          endif
          !
          ! call forcePERvelUZD with hdt=1.fp
          !
          CALL forcePERvelUZD(u1,bbk,ddk,aak,buux,bux,bdx,bddx,buuy,buy,bdy,bddy,cck,1._fp,icx,nlb,nub,mlb,mub,kmax, gdp)
       endif
       if (testGHOSTaccur) then
          if (icy==1) then
             !
             ! momentum along x
             !
             call computeU1V1accuratezza(s0,kfs,u0,u1,v,xG_U1,yG_U1,xG_V1,yG_V1,ghostU1,ghostV1,nlb,nub,mlb,mub,icy,kmax,0,gdp)
          else
             !
             ! inverted xG_V1,yG_V1 with xG_U1,yG_U1
             !
             call computeU1V1accuratezza(s0,kfs,u0,u1,v,xG_V1,yG_V1,xG_U1,yG_U1,ghostU1,ghostV1,nlb,nub,mlb,mub,icy,kmax,0,gdp)  
          endif
       endif
       !
       ! recompute the ghost value at u-velocity point if scheme is implicit (note that only u is updated, so s0 hu and hv stay the same)
       !
       if (cutcell.gt.0.and.(GhostMethod.eq.1.or.GhostMethod.eq.2)) then
          if ((GHOSTimpl.eq.1).AND.(iter.gt.1)) then
             call recompGHOSTiterUZD(icx        ,icy        ,u1         ,v          ,u0INTv     ,&
                                   & v0INTu     ,kcs        ,guu        ,xcor       ,ycor       ,&
                                   & gvv        ,mmax       ,nmax       ,kmax                   ,&
                                   & nst        ,nlb        ,nub        ,mlb        ,mub        ,&
                                   & nmlb       ,nmub       ,iter       ,Irov       ,gdp%d%ddbound    ,&
                                   & kcu        ,lunscr     ,nmmax      ,nmaxus     ,gdp)           
             !
             ! Call forceGHOSTuzd with u1
             !
             call forceGHOSTuzd(icx        ,icy        ,u1         ,&  
                              & aak        ,bbk        ,cck        ,ddk        ,&
                              & buux       ,bux        ,bdx        ,&
                              & bddx       ,buuy       ,buy        ,bdy        ,bddy       ,&
                              & mmax       ,nmax       ,kmax       ,&
                              & nst        ,nlb        ,nub        ,mlb        ,mub        ,&
                              & nmlb       ,nmub       ,iter       ,.false.    ,gdp)  
          endif
       endif
       !
       ! loop starts at red or black point depending on own subdomain
       !
       nmsta = 3 - nmsta
       !
       do k = 1, kmax
          do nm = nmsta, nmmax, 2
             !
             ! COMPUTE RIGHT HAND SIDE
             !
             if (kfu(nm) == 1) then
                uvdwk(nm, k) = ddk(nm,k)                     &
                             & - bddx(nm,k)*u1(nm-icx-icx,k) &
                             & - bdx (nm,k)*u1(nm-icx    ,k) &
                             & - bddy(nm,k)*u1(nm-icy-icy,k) &
                             & - bdy (nm,k)*u1(nm-icy    ,k) &
                             & - buy (nm,k)*u1(nm+icy    ,k) &
                             & - buuy(nm,k)*u1(nm+icy+icy,k) &
                             & - bux (nm,k)*u1(nm+icx    ,k) &
                             & - buux(nm,k)*u1(nm+icx+icx,k)
             endif
          enddo
       enddo
       if (icx == 1) then
          call timer_stop(timer_uzd_solve3u, gdp)
          call timer_start(timer_uzd_solve4u, gdp)
       else
          call timer_stop(timer_uzd_solve5v, gdp)
          call timer_start(timer_uzd_solve6v, gdp)
       endif
       do nm = nmsta, nmmax, 2
          if (kfu(nm) == 1) then
             vvdwk(nm, 1) = uvdwk(nm, 1)*bbk(nm, 1)
          endif
       enddo
       do k = 2, kmax
          do nm = nmsta, nmmax, 2
             if (kfu(nm) == 1) then
                vvdwk(nm, k) = (uvdwk(nm, k) - aak(nm, k)*vvdwk(nm, k-1))*bbk(nm, k)
             endif
          enddo
       enddo
       do k = kmax - 1, 1, -1
          do nm = nmsta, nmmax, 2
             if (kfu(nm) == 1) then
                vvdwk(nm, k) = vvdwk(nm, k) - cck(nm, k)*vvdwk(nm, k+1)
             endif
          enddo
       enddo
       !
       ! CHECK FOR CONVERGENCE
       !
       loop_k_1: do k = 1, kmax
          do nm = nmsta, nmmax, 2
             if (kfu(nm)==1 .and. abs(vvdwk(nm,k)-u1(nm,k)) > eps) then
                itr = 1
                exit loop_k_1
             endif
          enddo
       enddo loop_k_1
       if (icx == 1) then
          call timer_stop(timer_uzd_solve4u, gdp)
          call timer_start(timer_uzd_solve3u, gdp)
       else
          call timer_stop(timer_uzd_solve6v, gdp)
          call timer_start(timer_uzd_solve5v, gdp)
       endif
       !
       do k = 1, kmax
          do nm = nmsta, nmmax, 2
             if (kfu(nm) == 1) then
                u1(nm, k) = vvdwk(nm, k)
             endif
          enddo
       enddo
       !
       ! exchange u1 with neighbours for parallel runs
       !
       call dfexchg ( u1, 1, kmax, dfloat, nm_pos, gdp )
       !
       ! loop starts at point of other color now (black respectively red)
       !
       nmsta = 3 - nmsta
       !
       do k = 1, kmax
          do nm = nmsta, nmmax, 2
             !
             ! COMPUTE RIGHT HAND SIDE
             !
             if (kfu(nm) == 1) then
                uvdwk(nm, k) = ddk(nm,k)                     &
                             & - bddx(nm,k)*u1(nm-icx-icx,k) &
                             & - bdx (nm,k)*u1(nm-icx    ,k) &
                             & - bddy(nm,k)*u1(nm-icy-icy,k) &
                             & - bdy (nm,k)*u1(nm-icy    ,k) &
                             & - buy (nm,k)*u1(nm+icy    ,k) &
                             & - buuy(nm,k)*u1(nm+icy+icy,k) &
                             & - bux (nm,k)*u1(nm+icx    ,k) &
                             & - buux(nm,k)*u1(nm+icx+icx,k)
             endif
          enddo
       enddo
       if (icx == 1) then
          call timer_stop(timer_uzd_solve3u, gdp)
          call timer_start(timer_uzd_solve4u, gdp)
       else
          call timer_stop(timer_uzd_solve5v, gdp)
          call timer_start(timer_uzd_solve6v, gdp)
       endif
       do nm = nmsta, nmmax, 2
          if (kfu(nm) == 1) then
             vvdwk(nm,1) = uvdwk(nm,1) * bbk(nm, 1)
          endif
       enddo
       do k = 2, kmax
          do nm = nmsta, nmmax, 2
             if (kfu(nm) == 1) then
                vvdwk(nm,k) = (uvdwk(nm,k) - aak(nm,k)*vvdwk(nm,k-1)) * bbk(nm,k)
             endif
          enddo
       enddo
       do k = kmax - 1, 1, -1
          do nm = nmsta, nmmax, 2
             if (kfu(nm) == 1) then
                vvdwk(nm,k) = vvdwk(nm,k) - cck(nm,k)*vvdwk(nm,k+1)
             endif
          enddo
       enddo
       !
       ! CHECK FOR CONVERGENCE
       !
       loop_k_2: do k = 1, kmax
          do nm = nmsta, nmmax, 2
             if (kfu(nm)==1 .and. abs(vvdwk(nm,k)-u1(nm,k)) > eps) then
               itr = 1
               exit loop_k_2
             endif
          enddo
       enddo loop_k_2
       if (icx == 1) then
          call timer_stop(timer_uzd_solve4u, gdp)
       else
          call timer_stop(timer_uzd_solve6v, gdp)
       endif
       do k = 1, kmax
          do nm = nmsta, nmmax, 2
             if (kfu(nm)==1) u1(nm, k) = vvdwk(nm, k)
          enddo
       enddo
       !
       ! exchange u1 with neighbours for parallel runs
       !
       call dfexchg ( u1, 1, kmax, dfloat, nm_pos, gdp )
       !
       ! set right-hand side to u1 in the halo area so that convergence check can be done safely
       !
       do nm = 1, nmmax
          if ( kcu(nm) == -1 ) then
             do k = 1, kmax
                ddk(nm,k) = u1(nm,k)
             enddo
          endif
       enddo
       !
       ! determine global maximum of 'itr' over all nodes
       ! Note: this enables to synchronize the iteration process
       !
       call dfreduce_gdp( itr, 1, dfint, dfmax, gdp )
       IF (printUZDITERghost) then
          !
          ! hv , dpu , dpv are not defined. I use hu 4 times. Same for VMEAN and v1. I use s0 for s1
          !
          CALL postpr_ghost(nst+1 , s0 , u1 , u1 , qxk , qyk , Umean , Umean , hu , hu , hu , hu , dps , & 
                         & kmax,nlb,nub,mlb,mub,gdp)
       ENDIF
       IF (SingleLOOPuzd) exit loop_iteration
    enddo loop_iteration
    !
    if (periodSURFACE) then
       if ((icx==1.and.PERIODalongM==1).or.(icx/=1.and..not.PERIODalongM==1)) then
          !
          ! turn off kfu and kfv (second argument has to be the location of the tangential velocity)
          !
          CALL OFFonPERvel(kfv,kfu,icy,nlb,nub,mlb,mub,0, gdp)
       else
          !
          ! turn off kfu and kfv (second argument has to be the location of the tangential velocity)
          !
          CALL OFFonPERvel(kfu,kfv,icy,nlb,nub,mlb,mub,0, gdp)
       endif
    endif
    !
    if (iter >= 50) then
       write (errtxt, '(i0)') nst
       call prterr(lundia    ,'S205'    ,trim(errtxt)    )
    endif
    !
    ! Domain decomposition:
    !        end "solve_system_for_velocity"
    !
    if (icx == 1) then
       !
       ! D3dFlowMap_Check_V: Check for convergence
       !
       nhystp = nxtstp(d3dflow_solve_v, gdp)
       if (nhystp == d3dflow_solve_v) goto 222
    else
       !
       ! D3dFlowMap_Check_U: Check for convergence
       !
       nhystp = nxtstp(d3dflow_solve_u, gdp)
       if (nhystp == d3dflow_solve_u) goto 222
    endif
    !
    ! End Domain decomposition addition
    !
    !
    ! DEFINE NEW DEPTH AVERAGED VELOCITY
    !
    call timer_start(timer_uzd_umean, gdp)
    !
    ! Initialise umean for all (nm) 
    !
    umean = 0.0
    !
    do k = 1, kmax
       do nm = 1, nmmax
          umean(nm) = umean(nm) + thick(k)*u1(nm, k)
       enddo
    enddo
    call timer_stop(timer_uzd_umean, gdp)
    !
    if (testGHOSTaccur) then
       if (icy==1) then
          !
          ! momentum along x
          !
          call computeU1V1accuratezza(s0,kfs,u0,u1,v,xG_U1,yG_U1,xG_V1,yG_V1,ghostU1,ghostV1,nlb,nub,mlb,mub,icy,kmax,0,gdp)
       else
          !
          ! inverted xG_V1,yG_V1 with xG_U1,yG_U1
          !
          call computeU1V1accuratezza(s0,kfs,u0,u1,v,xG_V1,yG_V1,xG_U1,yG_U1,ghostU1,ghostV1,nlb,nub,mlb,mub,icy,kmax,0,gdp)  
       endif
    endif
    !
    ! Optionally compute individual momentum terms for output
    !
    if (gdp%gdflwpar%flwoutput%momentum) then
       if (constSOLforCHECKmomTERM) THEN
          u1(:,:) = u0(:,:)
       endif
       gdp%gdflwpar%mom_accum = gdp%gdflwpar%mom_accum + 1.0_fp
       call uzd(icreep    ,dpdksi    ,s0        ,u0        , &
              & u1        ,v         ,u0INTv    ,v0INTu    ,w1        ,umean     , &
              & hu        ,hv        ,guu       ,gvv       ,gvu       ,gsqs      , &
              & gvd       ,gud       ,gvz       ,gsqiu     ,qxk       ,qyk       , &
              & disch     ,umdis     ,dismmt    ,mnksrc    ,kcu       , &
              & kcs       ,kfu       ,kfv       ,kfs       , &
              & kspu      ,kadu      ,kadv      ,norow     ,icx       ,icy       , &
              & irocol    ,j         ,nmmaxj    ,nmmax     ,kmax      , &
              & nmax      ,mmax      ,nmaxus    ,&
              & nsrc      ,lsecfl    ,lstsci    ,betac     ,nst       , &
              & aak       ,bbk       ,cck       ,ddk       ,bddx      , &
              & bdx       ,bux       ,buux      ,bddy      ,bdy       , &
              & buy       ,buuy      ,uvdwk     ,vvdwk     ,ua        , &
              & ub        ,taubpu    ,taubsu    ,rho       ,sumrho    , &
              & thick     ,sig       ,dps       ,wsu       ,fxw       ,wsbodyu   , &
              & vicuv     ,vnu2d     ,vicww     ,rxx       ,rxy       , &
              & dfu       ,deltau    ,tp        ,rlabda    , &
              & diapl     ,rnpl      ,ghostU1   ,ghostV1   , &
              & cfurou    ,cfvrou    ,rttfu     ,r0        ,windsu    , &
              & patm      ,fcorio    ,ubrlsu    ,hkru      , &
              & pship     ,tgfsep    ,dteu      ,ustokes   ,.true., &
              & xG_L      ,yG_L      ,kWDu      ,kWDv      ,aguu      , &
              & xcor      ,ycor      ,gdp       )
    endif
end subroutine uzd
