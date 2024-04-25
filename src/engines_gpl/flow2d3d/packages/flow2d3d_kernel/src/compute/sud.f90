subroutine sud(dischy    ,nst       ,icreep    ,betac     ,mmax      , &
             & nmax      ,j         ,nmmaxj    ,nmmax     ,kmax      , &
             & lstsci    ,nsrc      ,lsecfl    ,norow     ,icx       , &
             & icy       ,dismmt    ,irocol               ,mnksrc    , &
             & kfu       ,kfv       ,kfs       ,kcs       ,kspu      , &
             & kadu      ,kadv      ,kcu       ,kfumin    ,kfumax    , &
             & porosu    ,s0        ,s1        ,u0        ,u1        , &
             & v1        ,w1        ,r0        ,qxk       ,qyk       , &
             & qzk       ,guu       ,gvv       ,gvu       ,gsqs      , &
             & gud       ,gvd       ,gvz       ,gsqiu     ,dteu      , &
             & circ2d    ,circ3d    ,disch     ,                       &
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
             & bbkc      ,ua        ,ub        ,soumud    ,            &
             & precip    ,ustokes   ,gdp       )
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
    !
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
    integer                , pointer :: iro
    logical                , pointer :: wind
    logical                , pointer :: culvert
    logical                , pointer :: mudlay
    logical                , pointer :: nfl
    logical                , pointer :: zmodel
    logical                , pointer :: wavcmp

    integer                    , pointer :: no_dis
    integer , dimension(:)     , pointer :: m_intake
    integer , dimension(:)     , pointer :: n_intake
    integer , dimension(:)     , pointer :: k_intake
    real(fp), dimension(:)     , pointer :: q_diff
    real(fp), dimension(:,:,:) , pointer :: disnf
    real(fp), dimension(:,:,:) , pointer :: disnf_intake
!
! Global variables
!
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
    integer                                                                     :: nmmax   !  Description and declaration in dimens.igs
    integer                                                                     :: nmmaxj  !  Description and declaration in dimens.igs
    integer                                                                     :: norow   !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: nsrc    !  Description and declaration in esm_alloc_int.f90
    integer                                                       , intent(in)  :: nst     !!  Time step number
    integer      , dimension(5, norow)                                          :: irocol  !  Description and declaration in esm_alloc_int.f90
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
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: qxk     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: qyk     !  Description and declaration in esm_alloc_real.f90
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
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)                     :: vicuv   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)               :: r0      !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(kmax)                                              :: sig     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(kmax)                                              :: thick   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(kmax, 2, norow)                                    :: circ3d  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nsrc)                                              :: disch   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nsrc)                                              :: umdis   !  Description and declaration in esm_alloc_real.f90
    character(1) , dimension(nsrc)                                              :: dismmt  !  Description and declaration in esm_alloc_char.f90
    character(8)                                                                :: dischy  !  Description and declaration in tricom.igs
!
! Local variables
!
    integer       :: ddb
    integer       :: i
    integer       :: icxy
    integer       :: icol
    integer       :: idis
    integer       :: ierror
    integer       :: intdir
    integer       :: iter
    integer       :: itr
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
    logical       :: error   ! Flag for detection of closure error in mass-balance
    real(hp)      :: bi
    real(hp)      :: fac
    real(fp)      :: epsomb
    real(fp)      :: hdti
    real(fp)      :: hnm
    real(fp)      :: hucres
    real(fp)      :: humean  ! Mean value for H in U-points
    real(fp)      :: drytrsh
    real(fp)      :: pr
    real(fp)      :: dxiu
    real(fp)      :: dxid
    character(80) :: errtxt
    integer       :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
!! executable statements -------------------------------------------------------
!
    eps         => gdp%gdconst%eps
    maseva      => gdp%gdheat%maseva
    lundia      => gdp%gdinout%lundia
    ntstep      => gdp%gdinttim%ntstep
    dco         => gdp%gdnumeco%dco
    drycrt      => gdp%gdnumeco%drycrt
    dryflc      => gdp%gdnumeco%dryflc
    hdt         => gdp%gdnumeco%hdt
    iter1       => gdp%gdnumeco%iter1
    momsol      => gdp%gdnumeco%momsol
    dpuopt      => gdp%gdnumeco%dpuopt
    rhow        => gdp%gdphysco%rhow
    ag          => gdp%gdphysco%ag
    iro         => gdp%gdphysco%iro
    wind        => gdp%gdprocs%wind
    culvert     => gdp%gdprocs%culvert
    mudlay      => gdp%gdprocs%mudlay
    nfl         => gdp%gdprocs%nfl
    zmodel      => gdp%gdprocs%zmodel
    wavcmp      => gdp%gdprocs%wavcmp
    
    no_dis      => gdp%gdnfl%no_dis
    disnf       => gdp%gdnfl%disnf
    disnf_intake=> gdp%gdnfl%disnf_intake
    m_intake    => gdp%gdnfl%m_intake
    n_intake    => gdp%gdnfl%n_intake
    k_intake    => gdp%gdnfl%k_intake
    q_diff      => gdp%gdnfl%q_diff
    !
    call timer_start(timer_sud_rest, gdp)
    !
    ddb     = gdp%d%ddbound
    nmaxddb = nmax + 2*gdp%d%ddbound
    mmaxddb = mmax + 2*gdp%d%ddbound
    hdti    = 1.0_fp / hdt
    icxy    = max(icx, icy)
    drytrsh = drycrt
    !
    nm_pos  = 1
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
                   & s0        ,dpu       ,umean     ,hu        ,gdp       )
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
       tetau(nm) = 0.5
       if (kfu(nm)==1) then
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
             & ub        ,ustokes   ,.false.   ,u1        ,s1        ,gdp       )
    call timer_stop(timer_sud_cucnp, gdp)
    !
    ! INITIALISATION OF ITERATION OVER CONTINUITY EQUATION
    !
    call timer_start(timer_sud_rest, gdp)
    do k = 1, kmax
       do nm = 1, nmmax
          if (kcs(nm) > 0) then
             d0k(nm, k) = gsqs(nm)*thick(k)*s0(nm)*hdti - qyk(nm, k) + qyk(nm - icy, k)
          endif
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
                d0k(nm, 1) = d0k(nm, 1) - (evap(nm)/rhow)*gsqs(nm)
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
             do idis = 1, no_dis
                d0k(nm,k) = d0k(nm,k) + disnf(nm,k,idis) + disnf_intake(nm,k,idis)
             enddo
          enddo
       enddo
       !
       ! Addition of the intake from the near field model
       !
       ! (Op verzoek van Robin uitgezet, onttrekkingen dus via normaal (ontkoppeld) debietpunt
       !
!      do idis = 1, no_dis
!         if (m_intake(idis) > 0) then
!            call n_and_m_to_nm(n_intake(idis), m_intake(idis), nm, gdp)
!            kenm = min(1, kfu(nm) + kfu(nm - icx) + kfv(nm) + kfv(nm - icy))
!            if (kenm/=0 .or. -q_diff(idis)>=0.0) then
!               if (k_intake(idis)/=0) then
!                  d0k(nm, k_intake(idis)) = d0k(nm, k_intake(idis)) - q_diff(idis)
!               else
!                  do kk = 1, kmax
!                     d0k(nm, kk) = d0k(nm, kk) - q_diff(idis)*thick(kk)
!                  enddo
!               endif
                !
                ! Generate warning if trying to subtract from dry cell
                !
 !           else
 !              write (errtxt, '(i0,i3)') nst, i
 !              call prterr(lundia    ,'S208'    ,trim(errtxt))
 !           endif
 !        endif
 !     enddo
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
       d(nm)  = s0(nm)
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
 9999 continue
    itr = 0
    !
    do iter = 1, iter1
       !
       ! BOUNDARY CONDITIONS
       !
       call timer_start(timer_sud_cucbp, gdp)
       call cucbp(kmax      ,norow     ,icx       , &
                & icy       ,zmodel    ,irocol    ,kcs       ,kfu       , &
                & kfumin    ,kfumax    ,s0        ,u0        ,dpu       , &
                & hu        ,umean     ,tetau     ,guu       ,gvu       , &
                & dzu1      ,thick     ,circ2d    ,circ3d    ,a         , &
                & b         ,c         ,d         ,aa        ,bb        , &
                & cc        ,dd        ,aak       ,bbk       ,cck       , &
                & ddk       ,crbc      ,wavcmp    ,gdp       )
       call timer_stop(timer_sud_cucbp, gdp)
       !
       ! SET UP SYSTEM OF EQUATIONS FOR INTERIOR POINTS
       !
       call timer_start(timer_sud_rest, gdp)
       if (momsol=='flood ') then
          nmd  = -icx
          do nm = 1, nmmax
             nmd = nmd + 1
             if (kcs(nm)==1) then
                dxid  = hu0(nmd)*guu(nmd)
                dxiu  = hu0(nm) *guu(nm)
                a(nm) = dxid*aa(nmd)
                b(nm) = hdti*gsqs(nm) + dxid*cc(nmd) - dxiu*aa(nm)
                c(nm) = -dxiu*cc(nm)
                d(nm) = d0(nm) - dxiu*dd(nm) + dxid*dd(nmd)
            endif
          enddo
       else
          nmd  = -icx
          do nm = 1, nmmax
             nmd  = nmd  + 1
             if (kcs(nm)==1) then
                a(nm) = guu(nmd)*(hu(nmd)*aa(nmd) - tetau(nmd)*dd(nmd))
                b(nm) = hdti*gsqs(nm)                                             &
                      & + guu(nmd)*(hu(nmd)*cc(nmd) - (1.0 - tetau(nmd))*dd(nmd)) &
                      & - guu(nm) *(hu(nm) *aa(nm)  - tetau(nm)         *dd(nm) )
                c(nm) = -guu(nm)*(hu(nm)*cc(nm) - (1.0 - tetau(nm))*dd(nm))
                d(nm) = d0(nm) - (guu(nm)*dpu(nm)*dd(nm) - guu(nmd)*dpu(nmd)*dd(nmd))
             endif
          enddo
       endif
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
          bi    = 1.0_hp / real(b(nm),hp)
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
          !
          do nm = 1, nmmax
             d(nm) = s1(nm)
          enddo
       endif
       !
       ! TOTAL WATERDEPTH DRYING IN SUD
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
             hnm = tetau(nm)*d(nm) + (1. - tetau(nm))*d(nmu) + dpu(nm)
             !
             ! CHECK FOR DRYING
             !
             if (min(hnm, hucres)<=drytrsh) then
                aa(nm)  = 0.0
                bb(nm)  = 1.0
                cc(nm)  = 0.0
                dd(nm)  = 0.0
                kfu(nm) = 0
                itr = 1
             else
                if (momsol=='flood ') then
                   bb(nm) = 1.0
                else
                   bb(nm) = hu(nm)/hnm
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
    enddo
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
    ! COMPUTATION (LAYER & DEPTH AVERAGED) VELOCITIES AND DISCHARGES
    !
    call timer_start(timer_sud_veldisch, gdp)
    do k = 1, kmax
       do nm = 1, nmmax
          if (kfu(nm)==1) then
             pr        = aak(nm, k)*d(nm) + cck(nm, k)*d(nm + icx)
             u1(nm, k) = (ddk(nm, k) - bb(nm)*pr)/bbk(nm, k)
          else
             u1(nm, k) = 0.0
          endif
       enddo
    enddo
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
             qxk(nm, k) = guu(nm)*hu0(nm)*thick(k)*u1(nm, k)*porosu(nm,k)
          else
             qxk(nm, k) = guu(nm)*hu (nm)*thick(k)*u1(nm, k)*porosu(nm,k)
          endif
       enddo
    enddo
    !
    ! adapt waterlevels and velocities at coupling boundaries to
    ! prevent mass closure error in case of parallel runs
    ! Note: with 1 or 2 processors there is no need for this adaption
    !
    if ( nproc > 2 ) call dfmassc (s1        ,u1        ,qxk       ,hu        ,d0        , &
                                 & dpu       ,porosu    ,gsqs      ,guu       ,tetau     , &
                                 & kcs       ,kcu       ,kfu       ,thick     ,nmmax     , &
                                 & kmax      ,icx       ,gdp )
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
                          &    - d0k(nm, k)                 ) / gsqs(nm)
                qzk(nm, k) = w1(nm, k)*gsqs(nm)
             endif
          enddo
       enddo
       !
       ! exchange w1 with neighbours for parallel runs
       !
       call dfexchg ( w1, 0, kmax, dfloat, nm_pos, gdp )
       !
       ! compute vertical discharge
       !
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
    ! Optionally compute individual momentum terms for output
    !
    if (gdp%gdflwpar%flwoutput%momentum) then
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
                & ub        ,ustokes   ,.true.    ,u1        ,s1        ,gdp       )
       call timer_stop(timer_sud_cucnp, gdp)
    endif
end subroutine sud
