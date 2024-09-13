subroutine sud(dischy    ,nst       ,icreep    ,betac     ,mmax      , &
             & nmaxus    ,kFLcut, &
             & nmax      ,j         ,nmmaxj    ,nmmax     ,kmax      , &
             & lstsci    ,nsrc      ,lsecfl    ,norow     ,icx       , &
             & icy       ,dismmt    ,irocol               ,mnksrc    , &
             & kfu       ,kfv       ,kfs       ,kcs       ,kspu      , &
             & kadu      ,kadv      ,kcu       ,kfumin    ,kfumax    , &
             & porosu    ,s0        ,s1        ,u0        ,u1        , &
             & v1INTu    ,xcor      ,ycor                            , &
             & v1        ,w1        ,r0        ,qxk       ,qyk       , &
             & qzk       ,guu       ,gvv       ,gvu       ,gsqs      , &
             & gud       ,gvd       ,gvz       ,gsqiu     ,dteu      , &
             & circ2d    ,circ3d    ,disch     ,porosv    , &
             & umdis     ,umean     ,hu        ,hv        ,dpu       ,dzu1      , &
             & dpdksi    ,thick     ,sig       ,dps       ,taubpu    , &
             & taubsu    ,rho       ,sumrho    ,wsu       ,fxw       , &
             & wsbodyu   ,idry      ,crbc      ,vicuv     ,hu0       , &
             & vnu2d     ,vicww     ,rxx       ,rxy       ,dfu       , &
             & deltau    ,tp        ,rlabda    ,cfurou    ,cfvrou    , &
             & rttfu     ,diapl     ,rnpl      , &
             & windsu    ,patm      ,fcorio    ,evap      ,ubrlsu    , &
             & uwtypu    ,hkru      ,pship     ,tgfsep    ,a         , &
             & b         ,c         ,d         ,aa        ,bb        , &
             & cc        ,dd        ,tetau     ,aak       ,bbk       , &
             & cck       ,ddk       ,d0        ,d0k       ,bbka      , &
             & bbkc      ,ua        ,ub        ,soumud    ,dis_nf    , &
             & precip    ,ustokes   ,aguu      ,agvv      ,agsqs     , &
             & qxk_tinyCUT, GHOSTu1 ,GHOSTv1   ,xG_L      ,yG_L      , &
             & kWDu      ,kWDv      ,timnow    , &
             & gdp )
!             
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
!  $Id: sud.f90 7020 2017-02-16 21:50:45Z platzek $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute/sud.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: SUD evaluates/solves the implicitly coupled
!              momentum and continuity equation at each
!              half time step.
!              Special approximation for pressure term,
!              based on limiter to avoid artificial flow.
!              Switch which makes it possible to use
!              upwind-approach for wet cross section in shallow
!              areas or if the model area contains structures.
! Method used: A.D.I.-scheme is used.
!              Upwind-approach for wet cross section in shallow
!              areas or if the model area contains structures.
!              At 2D Weir points:
!              - depth value DPU is not corrected like in general
!                (3D) weir case (see CALDPU)
!              - crest height is explicitly taken into account
!                in drying check
!              - 2D Turbulence model at depth points
!
! UA and UB are workarrays WRKB15 and WRKB16; Used only in CUCNP
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use flow2d3d_timers
    use globaldata
    use dfparall
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'flow_steps_f.inc'
    real(fp)               , pointer :: eps
    integer                , pointer :: maseva
    integer                , pointer :: lundia
    integer                , pointer :: ntstep
    real(fp)               , pointer :: dco
    real(fp)               , pointer :: drycrt
    real(fp)               , pointer :: dryflc
    real(fp)               , pointer :: hdt
    integer                , pointer :: iter1
    character(6)           , pointer :: momsol
    character(8)           , pointer :: dpuopt
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: dzmin
    real(fp)               , pointer :: tstart
    real(fp)               , pointer :: rhofrac    
    integer                , pointer :: iro
    integer                , pointer :: itlfsm
    logical                , pointer :: wind
    logical                , pointer :: culvert
    logical                , pointer :: mudlay
    logical                , pointer :: nfl
    logical                , pointer :: zmodel
    logical                , pointer :: wavcmp
    integer                , pointer :: lunscr
    integer                , pointer :: irov
    integer                , pointer :: nmlb
    integer                , pointer :: nmub
    integer                , pointer :: mlb
    integer                , pointer :: mub
    integer                , pointer :: nlb
    integer                , pointer :: nub
    integer                       , pointer :: dim_nmlist
    integer                       , pointer :: totGHOSTs1
    integer, dimension(:)         , pointer :: mGPs1
    integer, dimension(:)         , pointer :: nGPs1
    integer                       , pointer :: totGHOSTu1
    integer, dimension(:)         , pointer :: mGPu1
    integer, dimension(:)         , pointer :: nGPu1
    integer                       , pointer :: totGHOSTv1
    integer, dimension(:)         , pointer :: mGPv1
    integer, dimension(:)         , pointer :: nGPv1
    integer, dimension(:,:)       , pointer :: por012
    integer                       , pointer :: cutcell
    integer                       , pointer :: GhostMethod
    integer, dimension(:,:)       , pointer :: FREEs1_v
    integer, dimension(:,:)       , pointer :: FREEs1_u
    integer                       , pointer :: continuity_cc
    real(fp)                      , pointer :: THRESextCUTedge
    integer                       , pointer :: extrapGHOST1fluid2
    integer                       , pointer :: idebugCUThardINI
    integer                       , pointer :: idebugCUThardFIN
    real(fp)                      , pointer :: THRESsmallCELL
    logical                       , pointer :: printSUDITERghost
    integer                       , pointer :: free_S1_sud
    logical                       , pointer :: periodSURFACE
    integer                       , pointer :: PERIODalongM
    logical                       , pointer :: TRANSVperIMPL
    logical                       , pointer :: FORCEuAThPERbnd
    real(fp), dimension(:,:,:,:,:), pointer :: EDGExyBANK
    logical                       , pointer :: corrSURFslopeSUD
    integer, dimension(:,:)       , pointer :: kfs_cc
    logical                       , pointer :: virtualMERGEupdDEPTH
    integer                       , pointer :: typeVIRTmergeUPDdepth
    real(fp)                      , pointer :: thresMERGE_d
    integer, dimension(:,:)       , pointer :: NMlistMERGED_d
    integer, dimension(:)         , pointer :: Nmerged_d
    integer, dimension(:)         , pointer :: MERGEDwith_d
    logical                       , pointer :: virtualMERGEdisch
    logical                       , pointer :: callSUBR_WATERlevelPERIOD
    logical                       , pointer :: FORCEdisch
    logical                       , pointer :: changeKFUVcut
    integer, dimension(:,:,:)     , pointer :: EDGEtypeBANK
    integer, dimension(:)         , pointer :: isMERGEDu_d
    integer, dimension(:)         , pointer :: isMERGEDv_d
    logical                       , pointer :: virtualLINK
    real(fp), dimension(:,:)      , pointer :: xG
    real(fp), dimension(:,:)      , pointer :: yG
    real(fp), dimension(:,:)      , pointer :: PSIx
    real(fp), dimension(:,:)      , pointer :: PSIy
    real(fp), dimension(:,:)      , pointer :: ETAx
    real(fp), dimension(:,:)      , pointer :: ETAy
    logical                       , pointer :: constSOLforCHECKmomTERM
    real(fp), dimension(:,:)      , pointer :: Nx
    real(fp), dimension(:,:)      , pointer :: Ny
    real(fp), dimension(:,:,:)    , pointer :: INTx_GRS
    real(fp), dimension(:,:,:)    , pointer :: INTy_GRS
    integer, dimension(:,:)       , pointer :: Ndry_GRS
    integer                       , pointer :: removeW1qzk
    real(fp), dimension(:)        , pointer :: agsqs_link
    real(fp), dimension(:,:)      , pointer :: xCORV1
    real(fp), dimension(:,:)      , pointer :: yCORV1
    real(fp), dimension(:,:)      , pointer :: xG_U1
    real(fp), dimension(:,:)      , pointer :: yG_U1
    real(fp), dimension(:,:)      , pointer :: xCORU1
    real(fp), dimension(:,:)      , pointer :: yCORU1
    real(fp), dimension(:,:)      , pointer :: xG_V1
    real(fp), dimension(:,:)      , pointer :: yG_V1
    real(fp), dimension(:)        , pointer :: frict_sud
    integer                       , pointer :: analSUDcenterACTIVE
    integer                       , pointer :: TYPEofFORCING
    logical                       , pointer :: testGHOSTaccur
    real(fp), dimension(:)        , pointer :: xintU
    real(fp), dimension(:)        , pointer :: yintU
    logical                       , pointer :: analDEFERR
    integer                       , pointer :: typeHUDPU
    real(fp), dimension(:)        , pointer :: sourceU
    real(fp), dimension(:,:)      , pointer :: deltaUcut
    real(fp), dimension(:)        , pointer :: deltaS1cut
    real(fp), dimension(:)        , pointer :: EXPsouL
    real(fp), dimension(:)        , pointer :: EXPsouR
    real(fp), dimension(:,:)      , pointer :: eeC
    logical                       , pointer :: SUDtoCONVERGENCE
    real(fp)                      , pointer :: epsSUD
    logical                       , pointer :: huRHS
    real(fp), dimension(:)        , pointer :: gsqsR
!
! Global variables
!
    real(fp)                                                       ,intent(in)  :: timnow 
    integer                                                                     :: icreep  !  Description and declaration in tricom.igs
    integer                                                       , intent(in)  :: icx     !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                       , intent(in)  :: icy     !!  Increment in the Y-dir. (see ICX)
    integer                                                                     :: idry
    integer                                                                     :: j       !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                                                     :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: lsecfl  !  Description and declaration in dimens.igs
    integer                                                                     :: lstsci  !  Description and declaration in esm_alloc_int.f90
    integer                                                       , intent(in)  :: mmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                       , intent(in)  :: nmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                       , intent(in)  :: nmaxus
    integer                                                                     :: nmmax   !  Description and declaration in dimens.igs
    integer                                                                     :: nmmaxj  !  Description and declaration in dimens.igs
    integer                                                                     :: norow   !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: nsrc    !  Description and declaration in esm_alloc_int.f90
    integer                                                       , intent(in)  :: nst     !!  Time step number
    integer      , dimension(7, norow)                                          :: irocol  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(7, nsrc)                                           :: mnksrc  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kcs     !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kcu     !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfs     !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfu     !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfumax  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfumin  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfv     !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                     :: kspu    !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: kadu    !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: kadv    !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kFLcut
    integer, dimension(gdp%d%nmlb:gdp%d%nmub,4)                   , intent(in)  :: kWDu
    integer, dimension(gdp%d%nmlb:gdp%d%nmub,4)                   , intent(in)  :: kWDv
    real(fp)                                                                    :: betac   !  Description and declaration in tricom.igs
    real(fp)     , dimension(12, norow)                                         :: crbc    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(4, norow)                                          :: circ2d  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: a       !!  Internal work array, tridiagonal matrix water levels lower diagonal
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: aa      !!  Internal work array, coupling mean velocity with water level point in (N,M,K) left (down)
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: b       !!  Internal work array, tridiagonal matrix water levels main diagonal
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: bb      !!  Internal work array, coefficient mean velocity
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: c       !!  Internal work array, tridiagonal matrix water levels upper diagonal
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: cc      !!  Internal work array, coupling mean velocity with water level point right (upper)
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: d       !!  Internal work array, Right Hand side of the Continuity equation
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: d0      !!  Internal work array, Explicit part of the Right Hand side
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: dd      !!  Internal work array, Right hand side of the momentum eq. at (N,M)
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: deltau  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: dfu     !  Description and declaration in esm_alloc_real.f90
    real(prec)   , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: dpu     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: dteu    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: evap    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: fcorio  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: fxw     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: gsqiu   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: gsqs    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: gud     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: guu     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: gvd     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: gvu     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: gvv     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: gvz     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: hkru    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: xG_L
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: yG_L
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: hu0
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: hu      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: hv      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: patm    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: precip  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: pship   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: rlabda  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: s0      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: s1      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: soumud  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: taubpu  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: taubsu  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: tetau   !!  Factor for upwind approach S0 can be 0.0, 0.5 or 1.0 depending on value of HU, DCO, KSPU and UMEAN
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: tgfsep  !!  Water elev. induced by tide gen.force
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: tp      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: umean   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: uwtypu  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: vnu2d   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: windsu  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: wsu     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: wsbodyu !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)       , intent(out) :: qzk     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                     :: vicww   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                     :: w1      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, 3)                          :: cfurou  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, 3)                          :: cfvrou  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: aak     !!  Internal work array (in CUCNP & UZD)
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: bbk     !!  Internal work array (in CUCNP & UZD)
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: bbka    !!  Internal work array
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: bbkc    !!  Internal work array
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: cck     !!  Internal work array (in CUCNP & UZD)
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: d0k     !!  Internal work array
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: ddk     !!  Internal work array, diagonal space at (N,M,K)
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: diapl   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: dpdksi  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: dzu1    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: porosu  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: porosv  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: qxk     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      , intent(inout)  :: qyk     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: rho     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: rnpl    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: rttfu   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: rxx     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: rxy     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: sumrho  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: u0      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: u1      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: ua
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: ub
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: ubrlsu  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: ustokes !  Description and declaration in trisol.igs
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: v1      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: v1INTu
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)                     :: vicuv   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: dis_nf  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)               :: r0      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: ycor
    real(fp)     , dimension(kmax)                                              :: sig     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(kmax)                                              :: thick   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(kmax, 2, norow)                                    :: circ3d  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nsrc)                                              :: disch   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nsrc)                                              :: umdis   !  Description and declaration in esm_alloc_real.f90
    character(1) , dimension(nsrc)                                              :: dismmt  !  Description and declaration in esm_alloc_char.f90
    character(8)                                                                :: dischy  !  Description and declaration in tricom.igs
!   cutcells:
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: GHOSTu1
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: GHOSTv1
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: aguu      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: agvv      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub,1:kmax)                      :: qxk_tinyCUT
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: agsqs
!
! Local variables
!
    integer       :: analSUDcenterACTIVE_loc
    integer       :: Idummy,Idummyy,Idummyyy
    integer       :: icxOK
    integer       :: icyOK 
    integer       :: ddb
    integer       :: i
    integer       :: iii
    integer       :: icxy
    integer       :: icol
    integer       :: ierror
    integer       :: intdir
    integer       :: iter
    integer       :: itr
    integer       :: DOufac
    integer       :: itUfac    
    integer       :: k
    integer       :: kenm
    integer       :: kk
    integer       :: m
    integer       :: mmaxddb
    integer       :: n
    integer       :: nhystp
    integer       :: nm
    integer       :: nmaxddb
    integer       :: nmd
    integer       :: nmf
    integer       :: nmlu
    integer       :: nmu
    integer       :: nmaxOK
    logical       :: error   ! Flag for detection of closure error in mass-balance
    logical       :: Ldummy
    real(fp)      :: Rdummy
    real(hp)      :: bi
    real(hp)      :: fac
    real(fp)      :: epsomb
    real(fp)      :: hdti
    real(fp)      :: hnm
    real(fp),allocatable      ::s1old(:)
    real(fp)      :: hucres
    real(fp)      :: humean  ! Mean value for H in U-points
    real(fp)      :: drytrsh
    real(fp)      :: pr
    real(fp)      :: dxiu
    real(fp)      :: dxid
    real(fp)      :: facCUT
    real(fp)      :: consCUT
    character(80) :: errtxt
    integer       :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
    integer       :: mGP
    integer       :: nGP
    integer       :: CONTINUEiter
    real(fp)     , dimension(1)                                                 :: Rdummy1    
    real(fp), allocatable :: uEXACT(:,:)
    real(fp), allocatable :: uBUTTA(:,:)
    real(fp), allocatable :: u0_exact(:,:)
    real(fp), allocatable :: dpu_exact(:)
    real(fp), allocatable :: dpu_exactC(:)
    real(fp), allocatable :: hu_exact(:)
    real(fp), allocatable :: hu_exactC(:)
    real(fp)      :: LmaxUint
    real(fp)      :: LmaxUint2
    real(fp)      :: tdif
    real(fp)      :: ratFAC
    LOGICAL, save :: firstCALL=.TRUE.
!
!! executable statements -------------------------------------------------------
!
    dim_nmlist                => gdp%gdimbound%dim_nmlist
    totGHOSTs1                => gdp%gdimbound%totGHOSTs1
    mGPs1                     => gdp%gdimbound%mGPs1
    nGPs1                     => gdp%gdimbound%nGPs1
    totGHOSTu1                => gdp%gdimbound%totGHOSTu1
    mGPu1                     => gdp%gdimbound%mGPu1
    nGPu1                     => gdp%gdimbound%nGPu1
    totGHOSTv1                => gdp%gdimbound%totGHOSTv1
    mGPv1                     => gdp%gdimbound%mGPv1
    nGPv1                     => gdp%gdimbound%nGPv1
    por012                    => gdp%gdimbound%por012
    cutcell                   => gdp%gdimbound%cutcell
    GhostMethod               => gdp%gdimbound%GhostMethod
    FREEs1_v                  => gdp%gdimbound%FREEs1_v
    FREEs1_u                  => gdp%gdimbound%FREEs1_u
    continuity_cc             => gdp%gdimbound%continuity_cc
    THRESextCUTedge           => gdp%gdimbound%THRESextCUTedge
    extrapGHOST1fluid2        => gdp%gdimbound%extrapGHOST1fluid2
    idebugCUThardINI          => gdp%gdimbound%idebugCUThardINI
    idebugCUThardFIN          => gdp%gdimbound%idebugCUThardFIN
    THRESsmallCELL            => gdp%gdimbound%THRESsmallCELL
    printSUDITERghost         => gdp%gdimbound%printSUDITERghost
    free_S1_sud               => gdp%gdimbound%free_S1_sud
    periodSURFACE             => gdp%gdimbound%periodSURFACE
    PERIODalongM              => gdp%gdimbound%PERIODalongM
    TRANSVperIMPL             => gdp%gdimbound%TRANSVperIMPL
    FORCEuAThPERbnd           => gdp%gdimbound%FORCEuAThPERbnd
    EDGExyBANK                => gdp%gdimbound%EDGExyBANK
    corrSURFslopeSUD          => gdp%gdimbound%corrSURFslopeSUD
    kfs_cc                    => gdp%gdimbound%kfs_cc
    virtualMERGEupdDEPTH      => gdp%gdimbound%virtualMERGEupdDEPTH
    typeVIRTmergeUPDdepth     => gdp%gdimbound%typeVIRTmergeUPDdepth
    thresMERGE_d              => gdp%gdimbound%thresMERGE_d
    NMlistMERGED_d            => gdp%gdimbound%NMlistMERGED_d
    Nmerged_d                 => gdp%gdimbound%Nmerged_d
    MERGEDwith_d              => gdp%gdimbound%MERGEDwith_d
    virtualMERGEdisch         => gdp%gdimbound%virtualMERGEdisch
    callSUBR_WATERlevelPERIOD => gdp%gdimbound%callSUBR_WATERlevelPERIOD
    FORCEdisch                => gdp%gdimbound%FORCEdisch
    changeKFUVcut             => gdp%gdimbound%changeKFUVcut
    EDGEtypeBANK              => gdp%gdimbound%EDGEtypeBANK
    isMERGEDu_d               => gdp%gdimbound%isMERGEDu_d
    isMERGEDv_d               => gdp%gdimbound%isMERGEDv_d
    virtualLINK               => gdp%gdimbound%virtualLINK
    xG                        => gdp%gdimbound%xG
    yG                        => gdp%gdimbound%yG
    PSIx                      => gdp%gdimbound%PSIx
    PSIy                      => gdp%gdimbound%PSIy
    ETAx                      => gdp%gdimbound%ETAx
    ETAy                      => gdp%gdimbound%ETAy
    constSOLforCHECKmomTERM   => gdp%gdimbound%constSOLforCHECKmomTERM
    Nx                        => gdp%gdimbound%Nx
    Ny                        => gdp%gdimbound%Ny
    INTx_GRS                  => gdp%gdimbound%INTx_GRS
    INTy_GRS                  => gdp%gdimbound%INTy_GRS
    Ndry_GRS                  => gdp%gdimbound%Ndry_GRS
    removeW1qzk               => gdp%gdimbound%removeW1qzk
    agsqs_link                => gdp%gdimbound%agsqs_link
    xCORV1                    => gdp%gdimbound%xCORV1
    yCORV1                    => gdp%gdimbound%yCORV1
    xG_U1                     => gdp%gdimbound%xG_U1
    yG_U1                     => gdp%gdimbound%yG_U1
    xCORU1                    => gdp%gdimbound%xCORU1
    yCORU1                    => gdp%gdimbound%yCORU1
    xG_V1                     => gdp%gdimbound%xG_V1
    yG_V1                     => gdp%gdimbound%yG_V1
    analSUDcenterACTIVE       => gdp%gdimbound%analSUDcenterACTIVE
    TYPEofFORCING             => gdp%gdimbound%TYPEofFORCING
    testGHOSTaccur            => gdp%gdimbound%testGHOSTaccur
    xintU                     => gdp%gdimbound%xintU
    yintU                     => gdp%gdimbound%yintU
    analDEFERR                => gdp%gdimbound%analDEFERR
    typeHUDPU                 => gdp%gdimbound%typeHUDPU
    sourceU                   => gdp%gdimbound%sourceU
    deltaUcut                 => gdp%gdimbound%deltaUcut
    deltaS1cut                => gdp%gdimbound%deltaS1cut
    EXPsouL                   => gdp%gdimbound%EXPsouL
    EXPsouR                   => gdp%gdimbound%EXPsouR
    eeC                       => gdp%gdimbound%eeC
    SUDtoCONVERGENCE          => gdp%gdimbound%SUDtoCONVERGENCE
    epsSUD                    => gdp%gdimbound%epsSUD
    huRHS                     => gdp%gdimbound%huRHS
    gsqsR                     => gdp%gdimbound%Dwrka2
    lunscr                    => gdp%gdinout%lunscr
    irov                      => gdp%gdphysco%irov
    eps                       => gdp%gdconst%eps
    maseva                    => gdp%gdheat%maseva
    lundia                    => gdp%gdinout%lundia
    ntstep                    => gdp%gdinttim%ntstep
    dco                       => gdp%gdnumeco%dco
    drycrt                    => gdp%gdnumeco%drycrt
    dryflc                    => gdp%gdnumeco%dryflc
    hdt                       => gdp%gdnumeco%hdt
    iter1                     => gdp%gdnumeco%iter1
    momsol                    => gdp%gdnumeco%momsol
    dpuopt                    => gdp%gdnumeco%dpuopt
    rhow                      => gdp%gdphysco%rhow
    rhofrac                   => gdp%gdphysco%rhofrac    
    ag                        => gdp%gdphysco%ag
    iro                       => gdp%gdphysco%iro
    wind                      => gdp%gdprocs%wind
    culvert                   => gdp%gdprocs%culvert
    mudlay                    => gdp%gdprocs%mudlay
    nfl                       => gdp%gdprocs%nfl
    zmodel                    => gdp%gdprocs%zmodel
    wavcmp                    => gdp%gdprocs%wavcmp
    nmlb                      => gdp%d%nmlb  
    nmub                      => gdp%d%nmub
    mlb                       => gdp%d%mlb  
    nlb                       => gdp%d%nlb  
    mub                       => gdp%d%mub  
    nub                       => gdp%d%nub  
    dzmin                     => gdp%gdzmodel%dzmin
    tstart                    => gdp%gdexttim%tstart
    itlfsm                    => gdp%gdinttim%itlfsm
    !
    call timer_start(timer_sud_rest, gdp)
    !
    nm_pos  =  1
    ddb     = gdp%d%ddbound
    nmaxddb = nmax + 2*gdp%d%ddbound
    mmaxddb = mmax + 2*gdp%d%ddbound
    hdti    = 1.0_fp / hdt
    icxy    = max(icx, icy)
    drytrsh = drycrt
    tdif    = timnow*hdt*2._fp - tstart
    !
    allocate(s1old(gdp%d%nmlb:gdp%d%nmub))
    !
    if (cutcell==2 .and. virtualLINK) then
       agsqs(nmlb:nmub) = agsqs_link(nmlb:nmub) 
    endif
    !
    if (idry == 1) then
       !
       ! This is necessary because SUD can be repeated in case of drying
       !    in DRYCHK (DRYFLP <> NO)
       !
       do nm = 1, nmmax
          umean(nm) = 0.0
          if (kfu(nm)==1) then
             do k = 1, kmax
                umean(nm) = umean(nm) + thick(k)*u0(nm, k)
             enddo
          endif
       enddo
       call upwhu   (j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                   & zmodel    ,kcs       ,kcu       ,kspu      ,dps       , &
                   & s0        ,dpu       ,umean     ,hu        ,aguu      , &
                   & gdp       )
    endif
    do nm = 1, nmmax
       hu0(nm) = hu(nm)
    enddo
    !
    ! AFTER CALCULATION OF HU FOR CUCNP THE VALUE SHOULD BE CORRECTED
    !     AND TETAU SHOULD BE CALCULATED FOR UPWIND APPROACH
    !
    nmu = +icx
    do nm = 1, nmmax
       nmu       = nmu + 1
       hu(nm)    = max(hu(nm), 0.01_fp)
       tetau(nm) = 0.5_fp
       if (kfu(nm) == 1) then
          humean = 0.5*(s0(nm) + s0(nmu)) + dpu(nm)
          if (humean<dco .or. kspu(nm, 0)>0 .or. dpuopt=='UPW' .or. momsol == 'flood ') then
             if (umean(nm)>=0.001) then
                tetau(nm) = 1.0
             elseif (umean(nm)<= - 0.001) then
                tetau(nm) = 0.0
             else
                tetau(nm) = 1.0
                if (s0(nmu)>s0(nm)) tetau(nm) = 0.0
             endif
          endif
       endif
    enddo
    call timer_stop(timer_sud_rest, gdp)
    !
    ! Note: kfv is turned on and then off for periodic condition in order to have the correct value of vvv 
    ! at boundaries for coriolis and transversal advection
    !
    if (periodSURFACE) then
       if ((icx==1.and.PERIODalongM==1).or.(icx/=1.and..not.PERIODalongM==1)) then
          !
          ! Turn on kfu and kfv (second argument has to be the location of the tangential velocity)
          !
          CALL OFFonPERvel(kfv,kfu,icx,nlb,nub,mlb,mub,1, gdp)
       else
          !
          ! Turn on kfu and kfv (second argument has to be the location of the tangential velocity)
          !
          CALL OFFonPERvel(kfu,kfv,icx,nlb,nub,mlb,mub,1, gdp)
       endif
    endif
    !
    call timer_start(timer_sud_cucnp, gdp)
    !
    !
    ! Note cutcell: while I overwrite the velocity coefficient at ghost veloc points, the points that are neighbours to a
    ! ghost vel point feel the ghost condition on the advective terms since kfu and kfv are 1. 
    !
    call cucnp(dischy    ,icreep    ,dpdksi    ,s0        ,u0        , &
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
             & ub        ,ustokes   ,.false.   ,u1        ,s1        , &
             & nst       ,GHOSTu1   ,GHOSTv1   , &
             & kWDu      ,kWDv      ,xcor      ,ycor      , &
             & aguu      ,gdp       )
    !   
    ! Deactivate kfu here otherwise below the check finds that depth is negative at ghost points => itr=1 => the iterative process is repeated all over  
    ! Turn off kfv,kfu at periodic external boundaries (I needed them on to see them on output, otherwise they were set to zero above)
    !
    if (periodSURFACE) then
       if ((icx==1.and.PERIODalongM==1).or.(icx/=1.and..not.PERIODalongM==1)) then
          CALL OFFonPERvel(kfv,kfu,icy,nlb,nub,mlb,mub,0, gdp)   !turn off kfu and kfv (second argument has to be the location of the tangential velocity)
       else
          CALL OFFonPERvel(kfu,kfv,icy,nlb,nub,mlb,mub,0, gdp)   !turn off kfu and kfv (second argument has to be the location of the tangential velocity)
       endif
    endif    
    if (cutcell.gt.0.and.GhostMethod.le.1.and.changeKFUVcut) then     
      if (icx.eq.1) then   !along y
         call kfuv0_ghost_sud(kfv,kfu,nlb,nub,mlb,mub, gdp) !equivalent to  call kfsuv_ghost, just to avoid to pass Umean and other stuff
      else
         call kfuv0_ghost_sud(kfu,kfv,nlb,nub,mlb,mub, gdp) !equivalent to  call kfsuv_ghost, just to avoid to pass Umean and other stuff
      endif
    endif
    if (periodSURFACE) then
          if (.not.((icx==1.and.PERIODalongM==1).or.(icx/=1.and..not.PERIODalongM==1))) then 
             call forcePERvelSUD(u1,aa,bb,cc,dd,aak,bbk,cck,ddk,thick,icx,nlb,nub,mlb,mub,kmax, gdp) ! I use u1 since i copied the periodic condition there
          endif
    endif
    !
    call timer_stop(timer_sud_cucnp, gdp)
    !
    ! INITIALISATION OF ITERATION OVER CONTINUITY EQUATION
    !
    if (free_S1_sud==0) then
       facCUT = 1._fp
    else
       facCUT = 0._fp
    endif
!   
    call timer_start(timer_sud_rest, gdp)
    do k = 1, kmax
       do nm = 1, nmmax  !NOTE kFLcut IS NEEDED ONLY IF CONTINUITY_CC=0, OTHERWISE agsqs=0 
      !    if ((kcs(nm) > 0).and.(kFLcut(nm)==1)) then !kFLcut(nm) is the masking array that allows not to have flux of water from and to dry cell due to the velocity on the ghost cell
             d0k(nm, k) = agsqs(nm)*gsqs(nm)*thick(k)*s0(nm)*hdti -  qyk(nm, k) +  qyk(nm - icy, k) - qxk_tinyCUT(nm, k) + qxk_tinyCUT(nm - icx, k)
             !note january 2014: I commented kFLcut. If 
      !    elseif (kFLcut(nm)==0) then ! I have to do this, it can be a non-ghost but still cut
      !        d0k(nm, k) = agsqs(nm)*gsqs(nm)*thick(k)*s0(nm)*hdti  
      !    endif
         ! write(3333302+nst,'(6i6)') nst,nm,kFLcut(nm)
       enddo
    enddo
    !
    ! IN LAYER 1 DUE TO PRECIPITATION/EVAPORATION
    !     FOR TIME DEPENDENT INPUT OR HEAT MODEL WITH SPECIAL REQUEST
    !
    if (maseva>0) then
       do nm = 1, nmmax
          if (kcs(nm)==1) then
             d0k(nm, 1) = d0k(nm, 1) + precip(nm)*gsqs(nm)
             if (kfs(nm)==1) then
                d0k(nm, 1) = d0k(nm, 1) - (evap(nm)/rhow)*gsqs(nm) ! for cut-cell this should be fine. All rain/evap in a cut cell goes on the active part (and it is total area*rain). If it rains a lot and s1 becomes bigger then the top of dry cut cell it becomes wet otherwise...to be decided
             endif
          endif
       enddo
    endif
    !
    ! ADDITION OF DISCHARGES (suction only permitted if the point isn't dry)
    !
    do i = 1, nsrc
       nm   = (mnksrc(5, i) + ddb) + ((mnksrc(4, i) - 1) + ddb)*icxy
       k    = mnksrc(6, i)
       if (k .eq. -1) cycle
       kenm = min(1, kfu(nm) + kfu(nm - icx) + kfv(nm) + kfv(nm - icy))
       if (kenm/=0 .or. disch(i)>=0.0) then
          if (k/=0) then
             d0k(nm, k) = d0k(nm, k) + disch(i)
          else
             do kk = 1, kmax
                d0k(nm, kk) = d0k(nm, kk) + disch(i)*thick(kk)
             enddo
          endif
       else
          write (errtxt, '(i0,i3)') nst, i
          call prterr(lundia    ,'S208'    ,trim(errtxt))
       endif
       !
       ! in case of an intake for an intake/outfall combination:
       !
       if (mnksrc(7, i)>=2) then
          nm   = (mnksrc(2, i) + ddb) + ((mnksrc(1, i) - 1) + ddb)*icxy
          k    = mnksrc(3, i)
          kenm = min(1, kfu(nm) + kfu(nm - icx) + kfv(nm) + kfv(nm - icy))
          if (kenm/=0 .or. -disch(i)>=0.0) then
             if (k/=0) then
                d0k(nm, k) = d0k(nm, k) - disch(i)
             else
                do kk = 1, kmax
                   d0k(nm, kk) = d0k(nm, kk) - disch(i)*thick(kk)
                enddo
             endif
          !
          ! in case of a culvert no warning generated
          !
          elseif (mnksrc(7, i)/=3) then
             write (errtxt, '(i0,i3)') nst, i
             call prterr(lundia    ,'S208'    ,trim(errtxt))
          else
          endif
       endif
    enddo
    !
    ! ADDITION OF DISCHARGES from near field model
    !
    if (nfl) then
       do nm = 1, nmmax
          do k = 1, kmax
             d0k(nm,k) = d0k(nm,k) + dis_nf(nm,k)
          enddo
       enddo
    endif
    !
    ! add sources/sinks mud layer if mudlay == .true.
    !
    if (mudlay) then
       do nm = 1, nmmax
          !
          ! kfs mask array removed, since then the layer cannot increase
          ! any more after it has gone dry
          !
          if (kcs(nm)==1) then
             d0k(nm, 1) = d0k(nm, 1) + gsqs(nm)*soumud(nm)
          endif
       enddo
    endif
    !
    ! Initialise arrays a - dd for all (nm)
    !
    a  = 0.0
    b  = 1.0
    c  = 0.0
    d0 = 0.0
    aa = 0.0
    bb = 1.0
    cc = 0.0
    dd = 0.0
    !
    do nm = 1, nmmax
       d(nm)  = s0(nm) ! it seems useless to me, d is never read in cucbp (only written) and then it is overwritten. It should be done only for kcs(nm)=0 (not sure if ever used anyway)
    enddo
    !
    do k = 1, kmax
       do nm = 1, nmmax
          if (kfu(nm)==1) then
             fac    = real(porosu(nm,k),hp) * real(thick(k),hp) / real(bbk(nm,k),hp)
             aa(nm) = real(aa(nm),hp) + real(aak(nm,k),hp)*fac
             cc(nm) = real(cc(nm),hp) + real(cck(nm,k),hp)*fac
             dd(nm) = real(dd(nm),hp) + real(ddk(nm,k),hp)*fac
          endif
       enddo
    enddo
    do k = 1, kmax
       do nm = 1, nmmax
          if (kcs(nm)==1) d0(nm) = d0(nm) + d0k(nm, k)
       enddo
    enddo
    call timer_stop(timer_sud_rest, gdp)
    !
    ! ITERATIVE LOOP OVER CURRENT ROW. CALCULATION OF CONTINUITY EQ.
    !
    itr = 0
 9999 continue
    !
    !note for cutcell: technically since some dry points can be found below (and itr set to 1 and computetion goto 9999) 
    !  I should write itr = 0 before 9999 continue. And here:
    ! if (itr==1) then
    !    call PLIC_VOF_STEP(gsqs,kfs,kfu,kfv,kcs,kcu,kcv,s1,u1,v1,dps,dpU,dpV,xcor,ycor,alfas,&
    !                  lunscr,lundia,Irov,mmax,nmax,nmaxus,kmax,itstrt,nst,nlb,nub,mlb,mub,nmlb,nmub,drycrt,&
    !                  thick,guu,gvv,hu,hv,porosu,porosv,qxk,qyk,Umean,Vmean,stage,dummy0,dummy0,dummy0,dummy0,gdp%d%ddbound,nmmax,Zmodel)
    !    and here recompute all the interpolations (or at least see which one are needed)
    ! I instead suppose that the ghost cells are the same and I interpolate in the stencil. If all the interpolation stancil for the ghosts gets dry, I interpolate a 0 velocity and a zero depth and water surface=bed. I might have some finite values of depth and velocity if it does not dry all the stencil, that could be still ok.
    ! endif
    !
    itr = 0
    !
    
    iter = 0
    CONTINUEiter = 1
    if (SUDtoCONVERGENCE) s1old = s0
    do while (CONTINUEiter == 1 .and. iter < 500)
       CONTINUEiter = 0
       iter         = iter + 1   
       !
       ! BOUNDARY CONDITIONS
       !
       call timer_start(timer_sud_cucbp, gdp)
       call cucbp(kmax      ,norow     ,icx       , &
                & icy       ,zmodel    ,irocol    ,kcs       ,kfu       , &
                & kfumin    ,kfumax    ,s0        ,u0        ,dpu       , &
                & hu        ,umean     ,tetau     ,guu       ,gvu       , &
                & aguu      ,nmmax     ,  &
                & dzu1      ,thick     ,circ2d    ,circ3d    ,a         , &
                & b         ,c         ,d         ,aa        ,bb        , &
                & cc        ,dd        ,aak       ,bbk       ,cck       , &
                & ddk       ,crbc      ,wavcmp    ,GHOSTu1   ,gdp       )
       call timer_stop(timer_sud_cucbp, gdp)
       !
       ! SET UP SYSTEM OF EQUATIONS FOR INTERIOR POINTS
       !
       if (TRANSVperIMPL.AND.iter.gt.1) then !
          
          if (.not.((icx==1.and.PERIODalongM==1).or.(icx/=1.and..not.PERIODalongM==1))) then 
             call uvelocityPERIODsud(u1,icx,nlb,nub,mlb,mub,kmax, gdp) 
             call forcePERvelSUD(u1,aa,bb,cc,dd,aak,bbk,cck,ddk,thick,icx,nlb,nub,mlb,mub,kmax, gdp) 
          endif
    
          !Note periodic water levels are not needed at Q boundary since I have actually to modify NeUmann condition otherwise at the end of the cycle it    computes the wrong hnm.
          !  Something like this (both in nmf and nml):
          !  a(nmf) =  0.0
          !  b(nmf) =  1.0
          !  c(nmf) = -1.0
          !  d(nmf) =  0.0 here the difference between H at the internal Q and external Q. So if s1 at nmf+1 changes, the  one in nmf changes of that    quantity plus d(nmf).
          !                or also the difference of bed elvation might be considered if extrapolated linearly
          !And at H boundary water levels are already prescribed in circ as BC so I can remove it
          !call WATERlevelPERIOD(s0,dps,icx,nlb,nub,mlb,mub,kmax) 
       endif
       if (cutcell.gt.0.and.GhostMethod.le.1) then 
          !force u1 and Umean at ghost points
          call forceGHOSTsud(icx        ,icy        ,u0         ,v1         ,kcs        ,&
                           & xcor       ,ycor       ,guu        ,gvv        ,v1INTu     ,&
                           & hu         ,aguu       ,u1         ,Umean      ,s1         ,&
                           & kfs        ,kfu        ,kfv        ,thick      ,qxk        ,&
                           & aa         ,bb         ,cc         ,dd         ,&
                           & aak        ,bbk        ,cck        ,ddk        ,&
                           & mmax       ,nmax       ,nmmax      ,nmaxus     ,kmax       ,&
                           & nst        ,nlb        ,nub        ,mlb        ,mub        ,&
                           & nmlb       ,nmub       ,gdp%d%ddbound    ,lunscr     ,irov       ,&
                           & iter       ,gdp)
       endif         
!   
       call timer_start(timer_sud_rest, gdp)
       if (momsol=='flood ') then
          nmd  = -icx
          do nm = 1, nmmax
             nmd = nmd + 1
             if (kcs(nm)==1) then
                if (kFLcut(nm)==1) then
                   if (agsqs(nm).gt.THRESsmallCELL) then
                      dxid  = hu0(nmd)*guu(nmd)*aguu(nmd)
                      dxiu  = hu0(nm) *guu(nm)*aguu(nm)
                      a(nm) = dxid*aa(nmd)
                      b(nm) = hdti*agsqs(nm)*gsqs(nm) + dxid*cc(nmd) - dxiu*aa(nm)
                      c(nm) = -dxiu*cc(nm)
                      d(nm) = d0(nm) - dxiu*dd(nm) + dxid*dd(nmd)
                   else
                      a(nm) = 0._fp
                      b(nm) = 1._fp
                      c(nm) = 0._fp
                      d(nm) = s1(nm)  
                   endif                
                elseif(kFLcut(nm)==0) then
                   if (agsqs(nm) > THRESsmallCELL) then
                      a(nm) = 0._fp
                      b(nm) = hdti*gsqs(nm)*agsqs(nm) 
                      c(nm) = 0._fp
                      d(nm) = d0(nm)  
                   else
                      a(nm) = 0._fp
                      b(nm) = 1._fp
                      c(nm) = 0._fp
                      d(nm) = s1(nm)  
                   endif
                endif
            endif
          enddo
       else
          nmd  = -icx
          do nm = 1, nmmax
             nmd  = nmd  + 1
             if (kcs(nm)==1) then
                if (kFLcut(nm)==1) then
                   if (agsqs(nm) > THRESsmallCELL) then
                      a(nm) = aguu(nmd) * guu(nmd)*(hu(nmd)*aa(nmd) - tetau(nmd)*dd(nmd)) 
                      b(nm) = hdti*gsqs(nm)*agsqs(nm)     & !                                        
                            & + aguu(nmd)*guu(nmd)*(hu(nmd)*cc(nmd) - (1._fp - tetau(nmd))*dd(nmd)) &
                            & - aguu(nm) *guu(nm) *(hu(nm) *aa(nm)  - tetau(nm)         *dd(nm) )
                      c(nm) = -aguu(nm)*guu(nm)*(hu(nm)*cc(nm) - (1._fp - tetau(nm))*dd(nm))
                      d(nm) = d0(nm) - (aguu(nm)*guu(nm)*dpu(nm)*dd(nm) - aguu(nmd)*guu(nmd)*dpu(nmd)*dd(nmd))
                   else !if it is zero I have division by 0
                      a(nm) = 0._fp
                      b(nm) = 1._fp
                      c(nm) = 0._fp
                      d(nm) = s1(nm)  
                   endif
                elseif  (kFLcut(nm) == 0) then ! i take out the momentum from b and d and flux in continuity from a and b e d0
                   if (agsqs(nm) > THRESsmallCELL) then
                      a(nm) = 0._fp
                      b(nm) = hdti*gsqs(nm)*agsqs(nm) 
                      c(nm) = 0._fp
                      d(nm) = d0(nm)  
                   else !if it is zero I have division by 0
                      a(nm) = 0._fp
                      b(nm) = 1._fp
                      c(nm) = 0._fp
                      d(nm) = s1(nm)  
                   endif
               endif          
             endif
          enddo
       endif
       if ((cutcell==2.and.momsol/='flood ').or.FORCEdisch) then !it can be done also for flood solver, I just do not need it now.
           call FORCEdischarge(kfu , circ2d, hu    , dpu  , agsqs , gsqs, tetau, &
                             & aguu, guu   , irocol, norow, a, b, c, d, aa  , bb   , cc  , dd , &
                             & d0  , hdti  , icx   , icy  , nmmax     , nmlb, nmub , kmax, nst, &
                             & ddb , wavcmp, gdp   ) 
       endif
       !
       call timer_stop(timer_sud_rest, gdp)
       !
       ! Domain decomposition:
       !        Give Mapper chance to build the coupling equations
       !        Note that this is a two-stage process. First, coupling equations
       !        are built for the coupling points start+1;end-1
       !        Secondly, coupling equations are built for the `end' coupling points,
       !        start and end.
       !
       !
       nhystp = nxtstp(d3dflow_build_adi_zeta, gdp)
       !
       ! End of Domain decomposition addition
       !
       !
       !***SCALE ROWS OF MATRIX/RIGHT HAND SIDE VECTOR
       !
       call timer_start(timer_sud_rowsc, gdp)
       do nm = 1, nmmax
          bi    = 1.0_hp / real(b(nm),hp) ! not need to change since I avoided division by zero above. Also, qsgs is never zero! sign(1._fp,real(b  (nm),hp))  *max(1.0_hp / abs(real(b(nm),hp)),0.0000001_fp)  !for cut cells area can become very small and I dont want NaN !max(1.0_hp / real (b  (nm),hp),0.0000001_fp) !sign(real(b(nm),hp)*max(1.0_hp / abs(real(b(nm),hp)),0.0000001_fp) !for cut cells area can become very small and  I  dont  want NaN
          a(nm) = real(a(nm),hp) * bi
          b(nm) = 1.0_fp
          c(nm) = real(c(nm),hp) * bi
          d(nm) = real(d(nm),hp) * bi
       enddo
       call timer_stop(timer_sud_rowsc, gdp)
       !
       ! SOLUTION TRIDIAGONAL SYSTEM FOR THE WATERLEVELS
       !
       if (nhystp==noneighbors) then
          !
          ! Single domain case without domain decomposition
          ! The next piece of code in this IF-statement works for both serial and parallel runs
          ! In case of parallel runs twisted factorization technique is employed which is
          ! perfectly parallizable for two processors only. In case of more than 2 processors,
          ! this technique is combined with the block Jacobi approach at coupling points between
          ! pairs of "twisted" processors. Improvement in convergence is achieved by means of
          ! alternating the pairs of twisted processors at each iteration.
          !
          if ( nproc > 2 ) then
             icol = mod(iter,2)
          else
             icol = 1
          endif
          !
          call timer_start(timer_sud_solve, gdp)
          if ( mod(inode,2) == icol ) then
             !
             ! FORWARD SWEEP (elimination)
             !
             ! Division by the pivot for nmf is not needed anymore
             ! because of row scaling
             !
             do m = 2, mmaxddb
                nm = m*icx - icxy
                do n = 1, nmaxddb
                   nm = nm + icy
                   if (kcs(nm) > 0) then
                      bi    = 1.0_hp / (real(b(nm),hp) - real(a(nm),hp)*real(c(nm-icx),hp))
                      c(nm) = real(c(nm),hp) * bi
                      d(nm) = (real(d(nm),hp) - real(a(nm),hp)*real(d(nm-icx),hp)) * bi
                   endif
                enddo
             enddo
          else
             !
             ! BACKWARD SWEEP (elimination)
             !
             ! Division by the pivot for nmlu is not needed anymore
             ! because of row scaling
             !
             do m = mmaxddb-1, 1, -1
                nm = m*icx - icxy
                do n = 1, nmaxddb
                   nm = nm + icy
                   if (kcs(nm) > 0) then
                      bi    = 1.0_hp / (real(b(nm),hp) - real(c(nm),hp)*real(a(nm+icx),hp))
                      a(nm) = real(a(nm),hp) * bi
                      d(nm) = (real(d(nm),hp) - real(c(nm),hp)*real(d(nm+icx),hp)) * bi
                   endif
                enddo
             enddo
          endif
          !
          ! exchange coefficients a, b, c and d with neighbours for parallel runs
          !
          call dfexchg ( a, 1, 1, dfloat, nm_pos, gdp )
          call dfexchg ( b, 1, 1, dfloat, nm_pos, gdp )
          call dfexchg ( c, 1, 1, dfloat, nm_pos, gdp )
          call dfexchg ( d, 1, 1, dfloat, nm_pos, gdp )
          call dfsync(gdp)
          !
          if ( mod(inode,2) == icol ) then
             !
             ! FORWARD SWEEP in coupling points (elimination)
             !
             do m = 1, mmaxddb
                nm = m*icx - icxy
                do n = 1, nmaxddb
                   nm = nm + icy
                   if (kcs(nm) == -1) then
                      bi     = 1.0_fp / (real(b(nm),hp) - real(a(nm),hp)*real(c(nm-icx),hp))
                      c (nm) = real(c(nm),hp) * bi
                      d (nm) = (real(d(nm),hp) - real(a(nm),hp)*real(d(nm-icx),hp)) * bi
                      s1(nm) = d(nm)
                   endif
                enddo
             enddo
             !
             ! BACKWARD SWEEP (substitution)
             !
             nmlu = mmaxddb*icx - icxy
             do n = 1, nmaxddb
                nmlu = nmlu + icy
                if (kcs(nmlu) > 0) s1(nmlu) = d(nmlu)
             enddo
             do m = mmaxddb - 1, 1, -1
                nm = m*icx - icxy
                do n = 1, nmaxddb
                   nm = nm + icy
                   if (kcs(nm) > 0) then
                      d(nm)  = d(nm) - c(nm)*d(nm + icx)
                      s1(nm) = d(nm)
                  endif
                enddo
             enddo
          else
             !
             ! BACKWARD SWEEP in coupling points (elimination)
             !
             do m = mmaxddb, 1, -1
                nm = m*icx - icxy
                do n = 1, nmaxddb
                   nm = nm + icy
                   if (kcs(nm) == -1) then
                      bi     = 1.0_hp / (real(b(nm),hp) - real(c(nm),hp)*real(a(nm+icx),hp))
                      a (nm) = real(a(nm),hp) * bi
                      d (nm) = (real(d(nm),hp) - real(c(nm),hp)*real(d(nm+icx),hp)) * bi
                      s1(nm) = d(nm)
                   endif
                enddo
             enddo
             !
             ! FORWARD SWEEP (substitution)
             !
             nmf = icx - icxy
             do n = 1, nmaxddb
                nmf = nmf + icy
                if (kcs(nmf) > 0) s1(nmf) = d(nmf)
             enddo
             do m = 2, mmaxddb
                nm = m*icx - icxy
                do n = 1, nmaxddb
                   nm = nm + icy
                   if (kcs(nm) > 0) then
                      d (nm) = d(nm) - a(nm)*d(nm - icx)
                      s1(nm) = d(nm)
                   endif
                enddo
             enddo
          endif
          !
          ! exchange s1 with neighbours for parallel runs
          !
          call dfexchg ( s1, 1, 1, dfloat, nm_pos, gdp )
          !
          ! insert block Jacobi equation in coupling points
          !
          do nm = 1, nmmax
             if ( kcs(nm) == -1 ) then
                a(nm) = 0.0
                b(nm) = 1.0
                c(nm) = 0.0
                d(nm) = s1(nm)
             endif
          enddo
          call timer_stop(timer_sud_solve, gdp)
       else
          !
          ! Domain decomposition:
          !
          ! Wang solver for subdomains
          !
          ! METHOD OF WANG
          !
          ! First part of Wang's algoritm: pre-elimination:
          !
          call timer_start(timer_sud_wangpre, gdp)
          call wangp1(s1        ,kcs       ,irocol    ,norow     ,icx       , &
                    & icy       ,j         ,nmmaxj    ,a         ,b         , &
                    & c         ,d         ,gdp       )
          call timer_stop(timer_sud_wangpre, gdp)
          !
          ! Now second part of Wang's algoritm: elimination of reduced
          ! system. This is carried out by a global mapper process.
          ! At end of global mapper process:
          ! at coupling point at the left side and
          ! at last computational point at the right side we have that
          ! a=c=0, b=1 and d=zeta=new water elevation
          !
          ! Now Mapper builds and solves the reduced system of equation
          !
          if (icx==1) then
             intdir = 0
          else
             !
             ! intdir = 1 corresponds to left_to_Right direction
             ! (see GAWS routines)
             !
             intdir = 1
          endif
          call timer_start(timer_sud_gwsslv, gdp)
          call gwsslv(intdir    )
          call timer_stop(timer_sud_gwsslv, gdp)
          !
          ! Now third part of Wang's algoritm: back substitution
          !
          call timer_start(timer_sud_wangback, gdp)
          call wangp3(s1        ,kcs       ,irocol    ,norow     ,icx       , &
                    & icy       ,j         ,nmmaxj    ,a         ,b         , &
                    & c         ,d         ,gdp       )
          call timer_stop(timer_sud_wangback, gdp)
          !
          ! in case of Hydra (DD method, Wang approach) array d does
          ! not contain the solution, but the right-hand side evaluation.
          ! Since array d (and also s1) is used in the remainder of SUD
          ! for the new water elevation, we have to copy s1 to d.
          do nm = 1, nmmax
             d(nm) = s1(nm)
          enddo
       endif  
       !
       ! VIRTUAL merging of small cut cells. This improves a bit water surface but velocity gets slightly worst.
       !
       if (cutcell>0 .and. virtualMERGEupdDEPTH .and. .not. (virtualLINK)) then
          if (icx == 1) then  
           !  icxOK = icy
           !  icyOK = icx
            ! aguuOK = agvv
            ! agvvOK = aguu
             !invert u with v
             !
             ! Note aguu and agvv and icx and icy are inverted at each stage. 
             ! For merging of type 3 nothing changes (choose the maximum) but for other types it might
             !
             call COMPUTEmergingCARATT(kcs,kfs,agsqs,agvv,aguu,icy,icx,nmmax,nmlb,nmub,nst,lundia,& 
                                     & virtualMERGEupdDEPTH,typeVIRTmergeUPDdepth,thresMERGE_d,NMlistMERGED_d,Nmerged_d,&
                                     & isMERGEDu_d,isMERGEDv_d,MERGEDwith_d,1._fp,dim_nmlist,gdp)  
          else
            ! icxOK = icx
            ! icyOK = icy
            ! aguuOK = aguu
            ! agvvOK = agvv
             !
             ! Note aguu and agvv and icx and icy are inverted at each stage. 
             ! For merging of type 3 nothing changes (choose the maximum) but for other types it might
             !
             call COMPUTEmergingCARATT(kcs,kfs,agsqs,aguu,agvv,icx,icy,nmmax,nmlb,nmub,nst,lundia,& 
                                     & virtualMERGEupdDEPTH,typeVIRTmergeUPDdepth,thresMERGE_d,NMlistMERGED_d,Nmerged_d,&
                                     & isMERGEDu_d,isMERGEDv_d,MERGEDwith_d,1._fp,dim_nmlist,gdp)  
          endif
    
          call REDUCEgsqs(gsqs,agsqs,gsqsR,nmlb,nmub) !virtMERG wants gsqs*agsqs as actual argument
          !virtual merge of s1 (SINCE dps is the same in 
          call virtMERG(s1,gsqsR,s1,dps,Rdummy1,icxOK,icyOK,nmmax,nmlb,nmub,nst,1,1,1,1,lundia,Ldummy,& !1,1,1,1: ini vector,end vector,iniCYCLE,endCYCLE
                    & Idummy,Idummyy,Idummyyy,0,nmaxddb,gdp%d%ddbound,& !0 do not check large bed variations
                    & NMlistMERGED_d,Nmerged_d, dim_nmlist)
          !
          ! Fix s1 in halo = to cell inside for cells that are merged
          ! At edges separating the merged cells the depth is wrong, but in this way it is mass conservative, 
          ! because below it computes the qxk used for updating s1
          !
          call VIRTmerg_neumann(kfu,s1,d,aguu,irocol,norow,icx,icy,nmmax,nmlb,nmub,nst,ddb,wavcmp, gdp) 
          !call upwhu(j         ,nmmaxj    ,nmmax     ,kma!x      ,icx       , &
          !         & zmodel    ,kcs       ,kcu       ,kspu      ,dps       , &
          !         & s1        ,dpu       ,umean     ,hu        ,aguu      , &
          !         & gdp       )
          !do nm = 1, nmmax
          !   d(nm) = s1(nm)
          !enddo
       endif
       !if (callSUBR       call WATERlevelPERIOD(s1,dps,icx,nlb,nub,mlb,mub,kmax)
       !
       ! Note: it overwrites the value computed by VIRTmerg_neumann (but  clearly   the neuman had an effect on the solution already)
       ! this are needed to compute h below at Q boundary at the second order of accuracy
       ! COMMENTED IT MESSES UP THE MASS CONSERVATION SINCE THE FLUX AT BOUNDARY IS COMPUTED WITH DIFFERENT HU. 
       ! use linear extrapolation on Noumann (explicit) at boundary if I want second order.
       !if ( mod(nst,100)==0) write(*,*) 'use dz for newmann ' !and comment next 4 lines
       ! if (itlfsm<0.or.(itlfsm>0 .and. tdif>=itlfsm*hdt*2._fp)) then
       !    if (periodSURFACE) call WATERlevelPERIOD(s1,dps,icx,nlb,nub,mlb,mub,kmax) !note: if Q is forced instead of u, neuman should not affect   solution
       !    if (periodSURFACE) call WATERlevelPERIOD(d,dps,icx,nlb,nub,mlb,mub,kmax)  !note: if Q is forced instead of u, neuman should not affect   solution
       ! endif
       !
       ! TOTAL WATERDEPTH DRYING IN SUD (it should be ok also for cut-cell, since it is drying at veloctiy point not at water surface point)
       !
       call timer_start(timer_sud_rest, gdp)
       nmu = +icx
       do nm = 1, nmmax
          nmu = nmu + 1
          if (kfu(nm)==1) then
             !
             ! Special approach for 2D weir points:
             ! - depth value DPU is not corrected like in general (3D) weir case
             ! - crest height is explicitly taken into account in drying check
             !
             hucres = 1E9
             if (abs(kspu(nm, 0))==9) then
                if (umean(nm)>=0.001) then
                   hucres = d(nm) + hkru(nm)
                elseif (umean(nm)<= - 0.001) then
                   hucres = d(nm + icx) + hkru(nm)
                else
                   hucres = max(d(nm + icx), d(nm)) + hkru(nm)
                endif
             endif
             hnm = tetau(nm)*s1(nm) + (1._fp - tetau(nm))*s1(nmu) + dpu(nm)        
             !
             ! CHECK FOR DRYING
             !
             ! I should also set kfu to zero at  ghost points with aguu==0 above after the ghost value is used, otherwise itr is set to 1 below and the    iteration is repeated
             if (min(hnm, hucres)<=drytrsh) then !.and..not.(MERGEDwith_d(nm)>0.or.MERGEDwith_d(nmu)>0)) then !if its merged let it go negative. Note    using isMERGEDu would be wrong here, since I am checking if any of the two is a small cut for which it might overdry
                aa(nm)  = 0.0
                bb(nm)  = 1.0
                cc(nm)  = 0.0
                dd(nm)  = 0.0
                kfu(nm) = 0
                itr = 1
                !write(51515,'(a,i0,a,i0,a)') 'Velocity point nm = ',nm,', icx = ',icx,' is dry!!!'
                !write(*,*)'Velocity point nm = ',nm,', icx = ',icx,' is dry!!!'
                !pause
             else
                if (momsol=='flood ') then
                   bb(nm) = 1.0
                else
                   bb(nm) = hu(nm)/hnm
                   if (cutcell>0.and.(virtualMERGEupdDEPTH.or.callSUBR_WATERlevelPERIOD).and..not.(virtualLINK)) then
                      !
                      ! This is only needed for last iter (but i cannot know beforehand if itr is 1). 
                      ! In this way discharge is computed correctly as the flux used in this iteration. 
                      ! Note that bb is never used in the next iteration but only outside the iter cycle to compute u.
                      !
                      bb(nm) = hu(nm)/(tetau(nm)*d(nm) + (1._fp - tetau(nm))*d(nmu) + dpu(nm)) 
                   endif
                endif
                hu(nm) = hnm
          endif
          endif
       enddo
       call timer_stop(timer_sud_rest, gdp)
       !
       ! determine global maximum of 'itr' over all nodes
       ! Note: this enables to synchronize the repeating computation
       !
       call dfreduce_gdp( itr, 1, dfint, dfmax, gdp )
       !
       ! REPEAT COMPUTATION IF POINT IS SET DRY
       !       FIRST RESET HU
       !
       ! Domain decomposition:
       !    Synchronize on drying before finishing solve zeta
       !
       ! Note that if iter<iter1 flow goes always back to Build step
       ! (either because of DD flag or because of iter loop until iter=iter1)
       ! Since no mapping occurs for check_sud_dry, this communication
       ! step could be skipped for iter<iter1.
       !
       nhystp = nxtdry(d3dflow_check_sud_dry, itr, gdp)
       !
       ! repeat computation if point is set dry
       !
       IF (printSUDITERghost) then
            CALL postpr_ghost(nst+1 , s1 , u1 , v1 , qxk , qyk , Umean , Umean , hu , hv , dpu , dpu , dps , & ! I GIVE TWICE Umean and dpu since they  are   not passed
                           & kmax,nlb,nub,mlb,mub,gdp)
       ENDIF
       if (nhystp==d3dflow_build_adi_zeta .or. &
         & (nhystp==noneighbors .and. itr==1)) then
          !
          ! End of Domain decomposition addition
          !
          !
          do nm = 1, nmmax
             hu(nm) = hu0(nm)
          enddo
          goto 9999
       endif
       !
       ! exit cycle and if asked check for convergenze
       !
       if (.NOT.SUDtoCONVERGENCE) then
          CONTINUEiter = 1
          if(iter == iter1) then
              !
              ! End loop for ITER==ITER1 and SUDtoCONVERGENCE=.false. if SUDtoCONVERGENCE=.true. keeps iterating to convergence
              !
              exit
          endif
       else
          do nm = 1, nmmax
             if (kfs(nm)==1 .and. kcs(nm)==1 .and. abs(s1(nm)-s1old(nm))>epsSUD) then
                CONTINUEiter = 1 
                exit
             endif
          enddo
          if (CONTINUEiter==1) then
             s1old = s1
          endif
       endif     
    enddo   !end iteration loop
    !
    if (iter >= 500) then
       write (errtxt, '(i0)') nst
       call prterr(lundia    ,'S209'    ,trim(errtxt)    )
    endif      
    !
    !
    ! Compute the correct hu used to compute fluxes in the last iteration. 
    ! This will be the one needed to compute discharge
    !
    if (cutcell>0 .and. (virtualMERGEupdDEPTH .or. callSUBR_WATERlevelPERIOD) .and. .not. (virtualLINK)) then
       nmu = +icx
       do nm = 1, nmmax
          nmu = nmu + 1
          if (kfu(nm) == 1) then
             !
             ! It should be done only if isMERGEDu_d = true or isMERGEDv_d=true depending on the direction
             !
             hu(nm) = tetau(nm)*d(nm) + (1._fp - tetau(nm))*d(nmu) + dpu(nm)
          endif
       enddo
    endif
    !
    ! exchange kfu with neighbours for parallel runs
    !
    call dfexchg ( kfu, 1, 1, dfint, nm_pos, gdp )
    !
    ! adapt discharge boundary conditions
    !
    call timer_start(timer_sud_cucdp, gdp)
    call cucdp(kfu       ,irocol    ,norow     ,j         ,nmmaxj    , &
             & icx       ,icy       ,bb        ,gdp       )
    call timer_stop(timer_sud_cucdp, gdp)
    !
    ! Computation (layer & depth averaged) velocities and discharges
    !
    call timer_start(timer_sud_veldisch, gdp)
    !if (cutcell>0) call U1bound_fromCONT(kfu,qxk,hu,agsqs,gsqs,guu,aguu,irocol,norow,aak,bbk,cck,ddk,icx,icy,nmmax,nmlb,nmub,kmax,nst,ddb,wavcmp) 
    do k = 1, kmax
       do nm = 1, nmmax
          if (kfu(nm)==1) then
             pr        = (aak(nm,k)*(1._fp+eeC(nm ,1))-cck(nm,k)*eeC(nm+icx,2)) *d(nm) + ( cck(nm,k)*(1._fp+eeC(nm+icx,2))-aak(nm,k)*eeC(nm ,1) ) *d(nm + icx)          
             u1(nm, k) = (ddk(nm,k)+aak(nm,k)*EXPsouR(nm)+cck(nm,k)*EXPsouL(nm + icx) + bbk(nm,k)*deltaUcut(nm,k) +aak(nm,k)*deltaS1cut(nm)+cck(nm,k)*deltaS1cut(nm + icx)- bb(nm)*pr)/bbk(nm, k)          
             !u1(nm, k) = (ddk(nm,k)+aak(nm,k)*EXPsouR(nm) + gradS1 + bb(nm)*bbk(nm,k)*deltaUcut(nm,k) + &
             !          &  aak(nm,k)*deltaS1cut(nm)+cck(nm,k)*deltaS1cut(nm + icx)- bb(nm)*pr)/bbk(nm, k)
             !if (mod(nm,1000)==0) write(*,*) 'rimetti bb(nm)*'
          else
             u1(nm, k) = 0.0
          endif
       enddo
    enddo
    !
    ! Compute u and v at small cut edges
    !
    if (cutcell > 0) then
       if (continuity_cc.eq.1.and.GhostMethod.le.1.and.extrapGHOST1fluid2.eq.3) then
          !
          ! With ghost cells in sud: velocity is already prescribed
          !
          continue
       elseif (continuity_cc.eq.1.and.GhostMethod.eq.2.and.(extrapGHOST1fluid2.eq.2.or.extrapGHOST1fluid2.eq.1)) then
          !
          ! No ghosts in sud, only uzd. Explicit ghosts are used 
          ! Compute the velocity on small edges from explicit discharge plus updated depth
          !
          do nm = 1, nmmax
             if ((comparereal(aguu(nm),THRESextCUTedge)<0) .and. (comparereal(aguu(nm),0._fp)>0)) then
                !
                ! Note > 0 so only cut edges not fully dry (in the latter kfu=0 and so vel=0 from the previous block of code)
                ! 
                do k = 1, kmax
                   u1(nm, k) = qxk_tinyCUT(nm,k)/aguu(nm)/guu(nm)/hu(nm)/thick(k)/porosu(nm,k)
                enddo
                if (nst>=idebugCUThardINI .and. nst<=idebugCUThardFIN)  then
                  call nm_to_n_and_m(nm, n, m, gdp)
                  write(9893000,'(4i6,15f21.15)') nst,nm,n,m,(u1(nm, k),k = 1, kmax)
                endif                
             endif
             if ((comparereal(agvv(nm),THRESextCUTedge)<0) .and. (comparereal(agvv(nm),0._fp)>0)) then
                !
                ! Note > 0 so only cut edges not fully dry (in the latter kfv=0 and so vel=0 from the previous block of code)
                !
                do k = 1, kmax
                   v1(nm, k) = qyk(nm,k)/agvv(nm)/gvv(nm)/hv(nm)/thick(k)/porosv(nm,k)
                enddo
                if (nst>=idebugCUThardINI .and. nst<=idebugCUThardFIN)  then
                  call nm_to_n_and_m(nm, n, m, gdp)
                  write(9893001,'(4i6,15f21.15)') nst,nm,n,m,(v1(nm, k),k = 1, kmax)
                endif   
             endif           
          enddo
       endif
    endif
    !
    ! Could move this right after cucnp, but check results. It might be that it must be here. 
    ! By moving it there we can avoid some (possible?) unnecessary positive dry check in sud at edge inside the periodic halo 
    ! turn off kfv,kfu at periodic external boundaries (needed them on to see them on output, otherwise they were set to zero above) 
    !
    if (periodSURFACE) then
       if ((icx==1.and.PERIODalongM==1).or.(icx/=1.and..not.PERIODalongM==1)) then
          !
          ! Turn off kfu and kfv (second argument has to be the location of the tangential velocity)
          !
          CALL OFFonPERvel(kfv,kfu,icy,nlb,nub,mlb,mub,0, gdp)
       else
          !
          ! Turn off kfu and kfv (second argument has to be the location of the tangential velocity)
          !
          CALL OFFonPERvel(kfu,kfv,icy,nlb,nub,mlb,mub,0, gdp)
       endif
    endif
    !
    ! exchange u1 with neighbours for parallel runs
    !
    call dfexchg ( u1, 1, kmax, dfloat, nm_pos, gdp )
    !
    ! compute horizontal discharge
    !
    do k = 1, kmax
       do nm = 1, nmmax
          if (momsol == 'flood') then
             qxk(nm, k) = aguu(nm)*guu(nm)*hu0(nm)*thick(k)*u1(nm, k)*porosu(nm,k) 
          else
             qxk(nm, k) = aguu(nm)*guu(nm)*hu (nm)*thick(k)*u1(nm, k)*porosu(nm,k)
          endif
       enddo
    enddo
    !
    ! This cannot be removed by forcing aak=cck=1,bbk=1 and ddk=Q/(aguu*guu*hu)/facVEL (if facVEL/=0) inside subroutine FORCEdischarge,
    ! because hu is the old one there!
    !
    if (cutcell==2 .or. FORCEdisch) then 
       call BOUvelFROMdisch(kfumin , kfumax, dzu1, dzmin , thick , u1  , qxk , &
                          & momsol, kfu , circ3d, hu    , aguu, guu , &
                          & irocol , norow , icx , icy   , nmmax , nmlb, nmub, &
                          & kmax   , nst   , ddb , wavcmp, zmodel, gdp ) 
    endif
    !
    ! After computing mass conservative discharges, recompute hu from the virtually-merged water surface s1, 
    ! otherwise it can be way off and affect sediment transport
    ! Also, velocity is recomputed by this new depth, otherwise we could have velocity in the wrong direction 
    ! (it was computed by negative h)
    !
    if (cutcell>0 .and. (virtualMERGEupdDEPTH.or.callSUBR_WATERlevelPERIOD) .and. .not. (virtualLINK)) then
       nmu = +icx
       do nm = 1, nmmax
          nmu = nmu + 1
          if (kfu(nm)==1) then
             !
             ! It should be done only if isMERGEDu_d = true or isMERGEDv_d=true depending on the direction
             !
             hu(nm) = tetau(nm)*s1(nm) + (1._fp - tetau(nm))*s1(nmu) + dpu(nm)
             if (momsol /= 'flood') then
                do k=1,kmax
                   u1(nm, k) = qxk(nm, k)/ ( aguu(nm)*guu(nm)*hu (nm)*thick(k)*porosu(nm,k))
                enddo
             endif
          endif
       enddo
    endif
    !
    ! It can happen that there is huge discharge for configuration with no degree of freedom and flow normal to the wall 
    ! (it implies high water level like 1000 m and high discharge). When then dividing by a correct depth  (obtained after merging)
    ! it gives to me huge velocities. Therefore probably better to do mandatory merge of discharge before computing velicities above. or just merge
    ! dicharge and velocities below always
    !
    ! Virtual merge of discharge in small cut edges
    !
    if (cutcell>0 .and. virtualMERGEdisch .and. .not. (virtualLINK)) then
      if (momsol == 'flood') then
         call virtMERGdisch(hu0,kfu,kcs,aguu,guu,u1,thick,porosu,qxk,icx,icy,nmmax,kmax,nmlb,nmub, gdp)
      else
         call virtMERGdisch(hu,kfu,kcs,aguu,guu,u1,thick,porosu,qxk,icx,icy,nmmax,kmax,nmlb,nmub, gdp)
      endif
    endif
    !
    ! Adapt waterlevels and velocities at coupling boundaries to
    ! prevent mass closure error in case of parallel runs
    ! Note: with 1 or 2 processors there is no need for this adaption
    !
    if ( nproc > 2 ) call dfmassc (s1        ,u1        ,qxk       ,hu        ,d0        , &
                                 & dpu       ,porosu    ,gsqs      ,guu       ,tetau     , &  
                                 & kcs       ,kcu       ,kfu       ,thick     ,nmmax     , &
                                 & kmax      ,icx       ,gdp )
    !
    ! Domain decomposition:
    !
    nhystp = nxtstp(d3dflow_finish_wang, gdp)
    !
    ! End of Domain decomposition addition
    !
    if (kmax>1) then
       !
       ! COMPUTATION VERTICAL VELOCITIES AND DISCHARGES
       !
       ! Initialise arrays qzk and w1 for all (nm,k)
       !
       qzk = 0.0
       w1  = 0.0
       !
       do k = 1, kmax
          do nm = 1, nmmax
             if (kcs(nm)==1) then
                w1(nm, k) = w1(nm, k - 1) + thick(k)*s1(nm)*hdti        &
                          & + (qxk(nm, k) - qxk(nm - icx, k)            &
                          &    - d0k(nm, k)                 ) / (agsqs(nm)*gsqs(nm))
                qzk(nm, k) = w1(nm, k)*gsqs(nm)*agsqs(nm)
                !
                ! For cutcells, qzk are recomputed after merging in RECOMPqkz.f90 (called by trisol.f90)
                !
                if (removeW1qzk==1) then
                   w1(nm, k)  = 0._fp
                   qzk(nm, k) = 0._fp
                endif
             endif
          enddo
       enddo
       !if (cutcell==2.and.virtualLINK) then
       !   call VIRTUALlinkVERT(agsqs,agsqs,gsqs,w1,NMlistMERGED_d,Nmerged_d,icx,icy,nmmax,nmlb,nmub,nst,kmax)
       !endif
       !
       ! exchange w1 with neighbours for parallel runs
       !
       call dfexchg ( w1, 0, kmax, dfloat, nm_pos, gdp )
       !
       ! compute vertical discharge
       !
       epsomb = max(eps, eps*hdt)
       !
       error = .false.
       do nm = 1, nmmax
          if (abs(w1(nm, kmax))>epsomb) then
             error = .true.
             w1(nm, kmax) = 0.0
          endif
       enddo
       ierror = 0
       if (error) ierror = 1
       call dfreduce_gdp( ierror, 1, dfint, dfmax, gdp )
       error = ierror==1
       if (error) then
          write (errtxt, '(a,e12.3,a,i0,a)') 'Mass closure error exceeds ', &
               & epsomb, ' after ', ntstep, ' timesteps.'
          call prterr(lundia, 'U190', trim(errtxt))
       endif
    endif
    call timer_stop(timer_sud_veldisch, gdp)
    !
    ! compute depth-averaged velocity
    !
    do nm = 1, nmmax
       umean(nm) = 0.0
    enddo
    do k = 1, kmax
       do nm = 1, nmmax
          umean(nm) = umean(nm) + thick(k)*u1(nm, k)
       enddo
    enddo
    if (testGHOSTaccur) then
       if (icy==1) then
          !
          ! Momentum along x
          !
          call computeU1V1accuratezza(s1     , kfs    , u0 , u1 , v1 , xG_U1, yG_U1, xG_V1, yG_V1, &
                                    & ghostU1, ghostV1, nlb, nub, mlb, mub  , icy  , kmax , 1    , gdp)
       else
          !
          ! inverted xG_V1,yG_V1 with xG_U1,yG_U1
          !
          call computeU1V1accuratezza(s1,kfs,u0,u1,v1,xG_V1,yG_V1,xG_U1,yG_U1,ghostU1,ghostV1,nlb,nub,mlb,mub,icy,kmax,1,gdp)
       endif
    endif
    if (nst<=1) then
       do nm=1,nmmax
          if (kfu(nm)==1) then
             write(8765432,'(2i9,10f25.15)') nst,nm,U0(nm,1),U1(nm,1),U0(nm,1)-U1(nm,1)
          endif
       enddo
    endif
    !
    ! Optionally compute individual momentum terms for output
    !
    if (gdp%gdflwpar%flwoutput%momentum) then
       if (constSOLforCHECKmomTERM) THEN
          u1(:,:) = u0(:,:)
          s1(:) = s0(:)
       endif
       !
       ! turn on kfu and kfv
       !
       if (cutcell.gt.0.and.GhostMethod.le.1.and.changeKFUVcut) then     
         if (icx.eq.1) then   !along y
            call kfuv1_ghost_sud(kfv,kfu,nlb,nub,mlb,mub, gdp) !equivalent to  call kfsuv_ghost, just to avoid to pass Umean and other stuff
         else
            call kfuv1_ghost_sud(kfu,kfv,nlb,nub,mlb,mub, gdp) !equivalent to  call kfsuv_ghost, just to avoid to pass Umean and other stuff
         endif
       endif       
       if (periodSURFACE) then
          if ((icx==1.and.PERIODalongM==1).or.(icx/=1.and..not.PERIODalongM==1)) then
             CALL OFFonPERvel(kfv,kfu,icy,nlb,nub,mlb,mub,1, gdp)   !turn on  kfu and kfv (second argument has to be the location of the tangential velocity)
          else
             CALL OFFonPERvel(kfu,kfv,icy,nlb,nub,mlb,mub,1, gdp)   !turn on kfu and kfv (second argument has to be the location of the tangential velocity)
          endif
       endif           
       ! pass hu or hu0 ?
       call timer_start(timer_sud_cucnp, gdp)
       call cucnp(dischy    ,icreep    ,dpdksi    ,s0        ,u0        , &
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
                & ub        ,ustokes   ,.true.    ,u1        ,s1        , &
                & nst       ,GHOSTu1   ,GHOSTv1   , &
                & kWDu      ,kWDv      ,xcor      ,ycor      , &
                & aguu      ,gdp       )
       call timer_stop(timer_sud_cucnp, gdp)
       !
       ! turn off kfu and kfv
       !         
       if (cutcell.gt.0.and.GhostMethod.le.1.and.changeKFUVcut) then     
         if (icx.eq.1) then   !along y
            call kfuv0_ghost_sud(kfv,kfu,nlb,nub,mlb,mub, gdp) !equivalent to  call kfsuv_ghost, just to avoid to pass Umean and other stuff
         else
            call kfuv0_ghost_sud(kfu,kfv,nlb,nub,mlb,mub, gdp) !equivalent to  call kfsuv_ghost, just to avoid to pass Umean and other stuff
         endif
       endif    
     
       if (periodSURFACE) then
          if ((icx==1.and.PERIODalongM==1).or.(icx/=1.and..not.PERIODalongM==1)) then
             CALL OFFonPERvel(kfv,kfu,icy,nlb,nub,mlb,mub,0, gdp)   !turn off  kfu and kfv (second argument has to be the location of the tangential velocity)
          else
             CALL OFFonPERvel(kfu,kfv,icy,nlb,nub,mlb,mub,0, gdp)   !turn off kfu and kfv (second argument has to be the location of the tangential velocity)
          endif
       endif         
    endif
    firstCALL=.FALSE.
    deallocate(s1old)
end subroutine sud
