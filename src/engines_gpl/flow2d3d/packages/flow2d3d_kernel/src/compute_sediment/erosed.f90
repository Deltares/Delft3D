subroutine erosed(nmmax     ,kmax      ,icx       ,icy       ,lundia    , &
                & nst       ,lsed      ,lsedtot   ,lsal      ,ltem      , &
                & lsecfl    ,kfs       ,kfu       ,kfv       ,sig       , &
                & r0        ,u0eul     ,v0eul     ,s0        ,dps       , &
                & z0urou    ,z0vrou    ,sour      ,sink      ,rhowat    , &
                & ws        ,z0ucur    ,z0vcur    ,sigmol    , &
                & taubmx    ,s1        ,uorb      ,tp        , &
                & lstsci    ,thick     ,dicww     ,kcs       , &
                & kcu       ,kcv       ,guv       ,gvu       ,sbuu      , &
                & sbvv      ,seddif    ,hrms      ,ltur      , &
                & teta      ,rlabda    ,saleqs    , &
                & sbuut     ,sbvvt     ,entr      ,wstau     ,hu        , &
                & hv        ,ubot      ,rtur0     , &
                & temeqs    ,gsqs      ,guu       ,gvv       ,dt        , &
                & icall     ,deltau    ,deltav    ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2026.                                
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
!    Function: Computes sediment fluxes at the bed using
!              the Partheniades-Krone formulations.
!              Arrays SOURSE and SINKSE are filled and added
!              to arrays SOUR and SINK
!              Computes bed load transport for sand sediment
!              Arrays SBUU and SBVV are filled.
!              Computes vertical sediment diffusion coefficient
!              Array SEDDIF is filled
!              Includes wave asymmetry effects on sand bed-load
!              transport
!              Bed slope effects computed at the U and V velocity
!              points
! Method used: Attention: pointer ll for 'standard' FLOW
!              arrays is shifted with lstart
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts, only: pi, ee
    use bedcomposition_module
    use morphology_data_module
    use sediment_basics_module
    use compbsskin_module, only: compbsskin, get_alpha_fluff
    use m_sand_mud
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                             , pointer :: gammax
    real(fp)                             , pointer :: ag
    real(fp)                             , pointer :: vicmol
    integer                              , pointer :: nmudfrac
    real(fp)         , dimension(:)      , pointer :: rhosol
    real(fp)         , dimension(:)      , pointer :: cdryb
    real(fp)         , dimension(:,:,:)  , pointer :: logseddia
    real(fp)         , dimension(:)      , pointer :: logsedsig
    real(fp)         , dimension(:)      , pointer :: sedd10
    real(fp)         , dimension(:)      , pointer :: sedd50
    real(fp)         , dimension(:)      , pointer :: sedd90
    real(fp)         , dimension(:)      , pointer :: sedd50fld
    real(fp)         , dimension(:)      , pointer :: dstar
    real(fp)         , dimension(:)      , pointer :: taucr
    real(fp)         , dimension(:)      , pointer :: tetacr
    real(fp)         , dimension(:,:)    , pointer :: dss
    real(fp)         , dimension(:)      , pointer :: mudcnt
    real(fp)         , dimension(:)      , pointer :: pmcrit
    integer          , dimension(:)      , pointer :: nseddia
    integer          , dimension(:)      , pointer :: sedtyp
    integer          , dimension(:)      , pointer :: tratyp
    logical                              , pointer :: anymud
    real(fp)                             , pointer :: thresh
    real(fp)                             , pointer :: bed
    real(fp)                             , pointer :: susw
    real(fp)                             , pointer :: sedthr
    real(fp)                             , pointer :: bedw
    integer                              , pointer :: i10
    integer                              , pointer :: i50
    integer                              , pointer :: i90
    integer                              , pointer :: nxx
    real(fp)         , dimension(:)      , pointer :: xx
    real(fp)                             , pointer :: morfac
    logical                              , pointer :: varyingmorfac
    real(fp)                             , pointer :: factsd
    logical                              , pointer :: multi
    logical                              , pointer :: wave
    real(fp)                             , pointer :: eps
    real(fp)         , dimension(:)      , pointer :: bc_mor_array
    real(fp)         , dimension(:,:)    , pointer :: dbodsd
    real(fp)         , dimension(:)      , pointer :: dcwwlc
    real(fp)         , dimension(:)      , pointer :: dm
    real(fp)         , dimension(:)      , pointer :: dg
    real(fp)         , dimension(:)      , pointer :: dgsd
    real(fp)         , dimension(:,:)    , pointer :: dxx
    real(fp)         , dimension(:)      , pointer :: dzduu
    real(fp)         , dimension(:)      , pointer :: dzdvv
    real(fp)         , dimension(:)      , pointer :: epsclc
    real(fp)         , dimension(:)      , pointer :: epswlc
    real(fp)         , dimension(:,:)    , pointer :: fixfac
    real(fp)         , dimension(:,:)    , pointer :: frac
    integer          , dimension(:)      , pointer :: kfsed
    integer          , dimension(:,:)    , pointer :: kmxsed
    real(fp)         , dimension(:)      , pointer :: mudfrac
    real(fp)         , dimension(:)      , pointer :: sandfrac
    real(fp)         , dimension(:,:)    , pointer :: hidexp
    real(fp)         , dimension(:)      , pointer :: rsdqlc
    real(fp)         , dimension(:,:)    , pointer :: sbcu
    real(fp)         , dimension(:,:)    , pointer :: sbcv
    real(fp)         , dimension(:,:)    , pointer :: sbcuu
    real(fp)         , dimension(:,:)    , pointer :: sbcvv
    real(fp)         , dimension(:,:)    , pointer :: sbwu
    real(fp)         , dimension(:,:)    , pointer :: sbwv
    real(fp)         , dimension(:,:)    , pointer :: sbwuu
    real(fp)         , dimension(:,:)    , pointer :: sbwvv
    real(fp)         , dimension(:)      , pointer :: sddflc
    real(fp)         , dimension(:,:)    , pointer :: srcmax
    real(fp)         , dimension(:,:)    , pointer :: sswu
    real(fp)         , dimension(:,:)    , pointer :: sswv
    real(fp)         , dimension(:,:)    , pointer :: sswuu
    real(fp)         , dimension(:,:)    , pointer :: sswvv
    real(fp)         , dimension(:,:)    , pointer :: sutot
    real(fp)         , dimension(:,:)    , pointer :: svtot
    real(fp)         , dimension(:,:)    , pointer :: aks
    real(fp)         , dimension(:,:)    , pointer :: rca
    real(fp)         , dimension(:,:)    , pointer :: rsedeq
    real(fp)         , dimension(:,:)    , pointer :: sinkse
    real(fp)         , dimension(:,:)    , pointer :: sourse
    real(fp)         , dimension(:,:)    , pointer :: sour_im
    real(fp)         , dimension(:,:)    , pointer :: sinkf
    real(fp)         , dimension(:,:)    , pointer :: sourf
    real(fp)         , dimension(:)      , pointer :: taub
    real(fp)         , dimension(:,:)    , pointer :: taurat
    real(fp)         , dimension(:)      , pointer :: ust2
    real(fp)         , dimension(:)      , pointer :: umod
    real(fp)         , dimension(:)      , pointer :: uuu
    real(fp)         , dimension(:)      , pointer :: vvv
    real(fp)         , dimension(:)      , pointer :: wslc
    real(fp)         , dimension(:)      , pointer :: zumod
    logical                              , pointer :: scour
    integer          , dimension(:)      , pointer :: iform
    real(fp)         , dimension(:,:)    , pointer :: par
    real(fp)                             , pointer :: factcr
    integer                              , pointer :: ihidexp
    real(fp)                             , pointer :: asklhe
    real(fp)                             , pointer :: mwwjhe
    real(fp)                             , pointer :: ffthresh
    real(fp)         , dimension(:)      , pointer :: rksr
    real(fp)                             , pointer :: sus
    real(fp)                             , pointer :: espir
    real(fp)                             , pointer :: vonkar
    logical                              , pointer :: epspar
    logical                              , pointer :: ubot_from_com
    real(fp)                             , pointer :: timsec
    real(fp)                             , pointer :: timhr
    integer                              , pointer :: julday
    real(fp)                             , pointer :: camax
    real(fp)                             , pointer :: aksfac
    real(fp)                             , pointer :: rdc
    real(fp)                             , pointer :: wetslope
    integer                              , pointer :: iopkcw
    integer                              , pointer :: npar
    integer                              , pointer :: max_integers
    integer                              , pointer :: max_reals
    integer                              , pointer :: max_strings
    character(256)   , dimension(:)      , pointer :: dll_function
    integer(pntrsize), dimension(:)      , pointer :: dll_handle
    integer          , dimension(:)      , pointer :: dll_integers
    real(hp)         , dimension(:)      , pointer :: dll_reals
    character(256)   , dimension(:)      , pointer :: dll_strings
    character(256)   , dimension(:)      , pointer :: dll_usrfil
    logical                              , pointer :: bsskin
    real(fp)         , dimension(:)      , pointer :: thcmud
    logical                              , pointer :: oldmudfrac
    logical                              , pointer :: flmd2l
    real(prec)       , dimension(:,:)    , pointer :: bodsed 
    real(fp)         , dimension(:)      , pointer :: sedtrcfac
    integer                              , pointer :: max_mud_sedtyp
    integer                              , pointer :: min_dxx_sedtyp
    integer                              , pointer :: iflufflyr
    integer                              , pointer :: iunderlyr
    real(fp)         , dimension(:,:)    , pointer :: depfac
    real(fp)         , dimension(:,:)    , pointer :: mfluff
    include 'flow_steps_f.inc'
!
! Local parameters
!
    integer, parameter :: kmax2d = 20
!
! Global variables
!
    integer                                                   , intent(in)  :: icall   !  Number of call during time step (1st or 2nd)
    integer                                                   , intent(in)  :: icx     !  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                   , intent(in)  :: icy     !  Increment in the Y-dir. (see ICX)
    integer                                                   , intent(in)  :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                   , intent(in)  :: lsal    !  Description and declaration in dimens.igs
    integer                                                   , intent(in)  :: lsed    !  Description and declaration in esm_alloc_int.f90
    integer                                                   , intent(in)  :: lsedtot !  Description and declaration in esm_alloc_int.f90
    integer                                                   , intent(in)  :: lstsci  !  Description and declaration in esm_alloc_int.f90
    integer                                                   , intent(in)  :: ltem    !  Description and declaration in dimens.igs
    integer                                                   , intent(in)  :: lsecfl  !  Description and declaration in dimens.igs    
    integer                                                                 :: ltur    !  Description and declaration in esm_alloc_int.f90
    integer                                                                 :: lundia  !  Description and declaration in inout.igs
    integer                                                   , intent(in)  :: nmmax   !  Description and declaration in dimens.igs
    integer                                                   , intent(in)  :: nst
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcu     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcv     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfs     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfu     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfv     !  Description and declaration in esm_alloc_int.f90
    real(fp)                                                  , intent(in)  :: dt
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: deltau  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: deltav  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: entr    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guv     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvu     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gsqs    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guu     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvv     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hrms    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: hu      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: hv      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: rlabda  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, ltur), intent(in)  :: rtur0   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s0      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s1      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: sbuut
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: sbvvt
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: taubmx  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: teta    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: tp      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: uorb    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: ubot    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: wstau   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0ucur
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0urou  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0vcur
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0vrou  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: dicww   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, *)   , intent(in)  :: ws      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, lsed)              :: seddif  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: rhowat  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: u0eul   !  EULARIAN U-velocities at old time level
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: v0eul   !  EULARIAN V-velocities at old time level
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, *)                   :: r0      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, *)                   :: sink    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, *)                   :: sour    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot)                   :: sbuu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot)                   :: sbvv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax)                               , intent(in)  :: sig     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax)                               , intent(in)  :: thick   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(lstsci)                                           :: sigmol  !  Description and declaration in esm_alloc_real.f90
    real(fp)                                                  , intent(in)  :: saleqs
    real(fp)                                                  , intent(in)  :: temeqs
!
! Local variables
!
    integer                         :: i
    integer                         :: istat
    integer                         :: j
    integer                         :: k
    integer                         :: k2d
    integer                         :: kbed
    integer                         :: kmaxsd
    integer                         :: kn
    integer                         :: ku
    integer                         :: kv
    integer                         :: l
    integer                         :: ll
    integer                         :: lstart
    integer                         :: m
    integer                         :: n
    integer                         :: ndm
    integer                         :: nhystp
    integer                         :: nm
    integer                         :: nmd
    integer                         :: nm_pos    ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
    integer                         :: nmu
    integer                         :: num
    logical                         :: error
    logical                         :: suspfrac  ! includes suspended transport via advection-diffusion equation
    real(fp)                        :: afluff
    real(fp)                        :: aks_ss3d
    real(fp)                        :: caks
    real(fp)                        :: caks_ss3d
    real(fp)                        :: chezy
    real(fp)                        :: conc2d
    real(fp)                        :: delr
    real(fp)                        :: di50
    real(fp)                        :: difbot
    real(fp)                        :: drho
    real(fp)                        :: dstari
    real(fp)                        :: dtmor
    real(fp)                        :: dzduz
    real(fp)                        :: dzdvz
    real(fp)                        :: fi
    real(fp)                        :: fracf
    real(fp)                        :: grkg
    real(fp)                        :: grm2
    real(fp)                        :: grlyrs
    real(fp)                        :: h0
    real(fp)                        :: h1
    real(fp)                        :: rc
    real(fp)                        :: maxslope
    real(fp)                        :: mfltot
    real(fp)                        :: sag
    real(fp)                        :: salinity
    real(fp)                        :: sinkfluff
    real(fp)                        :: sinktot
    real(fp)                        :: sourfluff
    real(fp)                        :: spirint   ! local variable for spiral flow intensity r0(nm,1,lsecfl)
    real(fp)                        :: taks
    real(fp)                        :: taks0
    real(fp)                        :: tauadd
    real(fp)                        :: tauc
    real(fp)                        :: tdss      ! temporary variable for dss
    real(fp)                        :: temperature
    real(fp)                        :: thick0
    real(fp)                        :: thick1
    real(fp)                        :: trsedeq   ! temporary variable for rsedeq
    real(fp)                        :: tsd
    real(fp)                        :: tsigmol   ! temporary variable for sigmol
    real(fp)                        :: twsk
    real(fp)                        :: u
    real(fp)                        :: ubed
    real(fp)                        :: umean
    real(fp)                        :: ustarc
    real(fp)                        :: utot
    real(fp)                        :: v
    real(fp)                        :: vbed
    real(fp)                        :: velb
    real(fp)                        :: velm
    real(fp)                        :: vmean
    real(fp)                        :: z0cur
    real(fp)                        :: z0rou
    real(fp)                        :: zvelb
    real(fp), dimension(:), pointer :: localpar
    real(fp), dimension(lsedtot)    :: E            ! erosion velocity [m/s]
    real(fp), dimension(0:kmax2d)   :: dcww2d
    real(fp), dimension(0:kmax2d)   :: sddf2d
    real(fp), dimension(0:kmax2d)   :: ws2d
    real(fp), dimension(kmax2d)     :: rsdq2d
    real(fp), dimension(kmax2d)     :: sig2d
    real(fp), dimension(kmax2d)     :: thck2d
    real(fp), dimension(kmax)       :: concin3d
    real(fp), dimension(kmax2d)     :: concin2d
    character(256)                  :: errmsg
    !
    data thck2d/0.1747, 0.1449, 0.1202, 0.0997, 0.0827, 0.0686, 0.0569, 0.0472, &
       & 0.0391, 0.0325, 0.0269, 0.0223, 0.0185, 0.0154, 0.0127, 0.0106, 0.0088,&
       & 0.0073, 0.0060, 0.0050/
    data sig2d/ - 0.0874, -0.2472, -0.3797, -0.4897, -0.5809, -0.6565, -0.7193, &
       & -0.7713, -0.8145, -0.8503, -0.8800, -0.9046, -0.9250, -0.9419, -0.9560,&
       & -0.9676, -0.9773, -0.9854, -0.9920, -0.9975/


!    data thck2d/0.2243, 0.1794, 0.1434, 0.1147, 0.0917, 0.0734, 0.0587, 0.0469, &
!       & 0.0375, 0.0300/
!    data sig2d/-0.1122,-0.3140,-0.4754,-0.6045,-0.7077,-0.7902,-0.8563,-0.9090, &
!       & -0.9512,-0.9850/
    
!
!! executable statements -------------------------------------------------------
!
    wave                => gdp%gdprocs%wave
    nmudfrac            => gdp%gdsedpar%nmudfrac
    rhosol              => gdp%gdsedpar%rhosol
    cdryb               => gdp%gdsedpar%cdryb
    logseddia           => gdp%gdsedpar%logseddia
    logsedsig           => gdp%gdsedpar%logsedsig
    sedd10              => gdp%gdsedpar%sedd10
    sedd50              => gdp%gdsedpar%sedd50
    sedd90              => gdp%gdsedpar%sedd90
    sedd50fld           => gdp%gdsedpar%sedd50fld
    dstar               => gdp%gdsedpar%dstar
    taucr               => gdp%gdsedpar%taucr
    tetacr              => gdp%gdsedpar%tetacr
    dss                 => gdp%gdsedpar%dss
    mudcnt              => gdp%gdsedpar%mudcnt
    pmcrit              => gdp%gdsedpar%pmcrit
    nseddia             => gdp%gdsedpar%nseddia
    sedtyp              => gdp%gdsedpar%sedtyp
    tratyp              => gdp%gdsedpar%tratyp
    anymud              => gdp%gdsedpar%anymud
    sedtrcfac           => gdp%gdsedpar%sedtrcfac
    max_mud_sedtyp      => gdp%gdsedpar%max_mud_sedtyp
    min_dxx_sedtyp      => gdp%gdsedpar%min_dxx_sedtyp
    thresh              => gdp%gdmorpar%thresh
    sedthr              => gdp%gdmorpar%sedthr
    i10                 => gdp%gdmorpar%i10
    i50                 => gdp%gdmorpar%i50
    i90                 => gdp%gdmorpar%i90
    nxx                 => gdp%gdmorpar%nxx
    xx                  => gdp%gdmorpar%xx
    multi               => gdp%gdmorpar%multi
    factcr              => gdp%gdmorpar%factcr
    ihidexp             => gdp%gdmorpar%ihidexp
    asklhe              => gdp%gdmorpar%asklhe
    mwwjhe              => gdp%gdmorpar%mwwjhe
    ffthresh            => gdp%gdmorpar%thresh
    morfac              => gdp%gdmorpar%morfac
    varyingmorfac       => gdp%gdmorpar%varyingmorfac
    factsd              => gdp%gdmorpar%factsd
    ag                  => gdp%gdphysco%ag
    vicmol              => gdp%gdphysco%vicmol
    gammax              => gdp%gdnumeco%gammax
    eps                 => gdp%gdconst%eps
    bc_mor_array        => gdp%gderosed%bc_mor_array
    dbodsd              => gdp%gderosed%dbodsd
    dcwwlc              => gdp%gderosed%dcwwlc
    dm                  => gdp%gderosed%dm
    dg                  => gdp%gderosed%dg
    dgsd                => gdp%gderosed%dgsd
    dxx                 => gdp%gderosed%dxx
    dzduu               => gdp%gderosed%e_dzdn
    dzdvv               => gdp%gderosed%e_dzdt
    epsclc              => gdp%gderosed%epsclc
    epswlc              => gdp%gderosed%epswlc
    fixfac              => gdp%gderosed%fixfac
    frac                => gdp%gderosed%frac
    kfsed               => gdp%gderosed%kfsed
    kmxsed              => gdp%gderosed%kmxsed
    mudfrac             => gdp%gderosed%mudfrac
    sandfrac            => gdp%gderosed%sandfrac
    hidexp              => gdp%gderosed%hidexp
    rsdqlc              => gdp%gderosed%rsdqlc
    sbcu                => gdp%gderosed%sbcx
    sbcv                => gdp%gderosed%sbcy
    sbcuu               => gdp%gderosed%e_sbcn
    sbcvv               => gdp%gderosed%e_sbct
    sbwu                => gdp%gderosed%sbwx
    sbwv                => gdp%gderosed%sbwy
    sbwuu               => gdp%gderosed%e_sbwn
    sbwvv               => gdp%gderosed%e_sbwt
    sddflc              => gdp%gderosed%sddflc
    sswu                => gdp%gderosed%sswx
    sswv                => gdp%gderosed%sswy
    sswuu               => gdp%gderosed%e_sswn
    sswvv               => gdp%gderosed%e_sswt
    sutot               => gdp%gderosed%sxtot
    svtot               => gdp%gderosed%sytot
    aks                 => gdp%gderosed%aks
    rca                 => gdp%gderosed%rca
    rsedeq              => gdp%gderosed%rsedeq
    sinkse              => gdp%gderosed%sinkse
    sourse              => gdp%gderosed%sourse
    sour_im             => gdp%gderosed%sour_im
    sinkf               => gdp%gdmorpar%flufflyr%sinkf
    sourf               => gdp%gdmorpar%flufflyr%sourf
    iflufflyr           => gdp%gdmorpar%flufflyr%iflufflyr
    srcmax              => gdp%gderosed%srcmax
    taub                => gdp%gderosed%taub
    taurat              => gdp%gderosed%taurat
    ust2                => gdp%gderosed%ust2
    umod                => gdp%gderosed%umod
    uuu                 => gdp%gderosed%uuu
    vvv                 => gdp%gderosed%vvv
    wslc                => gdp%gderosed%wslc
    zumod               => gdp%gderosed%zumod
    scour               => gdp%gdscour%scour
    iform               => gdp%gdtrapar%iform
    par                 => gdp%gdtrapar%par
    rksr                => gdp%gdbedformpar%rksr
    sus                 => gdp%gdmorpar%sus
    bed                 => gdp%gdmorpar%bed
    susw                => gdp%gdmorpar%susw
    bedw                => gdp%gdmorpar%bedw
    espir               => gdp%gdmorpar%espir
    epspar              => gdp%gdmorpar%epspar 
    vonkar              => gdp%gdphysco%vonkar
    vicmol              => gdp%gdphysco%vicmol
    timsec              => gdp%gdinttim%timsec
    timhr               => gdp%gdinttim%timhr
    julday              => gdp%gdinttim%julday
    camax               => gdp%gdmorpar%camax
    aksfac              => gdp%gdmorpar%aksfac
    rdc                 => gdp%gdmorpar%rdc
    iopkcw              => gdp%gdmorpar%iopkcw
    ubot_from_com       => gdp%gdprocs%ubot_from_com
    npar                => gdp%gdtrapar%npar
    max_integers        => gdp%gdtrapar%max_integers
    max_reals           => gdp%gdtrapar%max_reals
    max_strings         => gdp%gdtrapar%max_strings
    dll_function        => gdp%gdtrapar%dll_function
    dll_handle          => gdp%gdtrapar%dll_handle
    dll_integers        => gdp%gdtrapar%dll_integers
    dll_reals           => gdp%gdtrapar%dll_reals
    dll_strings         => gdp%gdtrapar%dll_strings
    dll_usrfil          => gdp%gdtrapar%dll_usrfil
    bsskin              => gdp%gdsedpar%bsskin
    thcmud              => gdp%gdsedpar%thcmud
    oldmudfrac          => gdp%gdmorpar%oldmudfrac
    flmd2l              => gdp%gdprocs%flmd2l
    depfac              => gdp%gdmorpar%flufflyr%depfac
    mfluff              => gdp%gdmorpar%flufflyr%mfluff
    wetslope            => gdp%gdmorpar%wetslope
    !
    allocate (localpar (npar), stat = istat)
    !
    if (varyingmorfac .and. icall==1) then
       call updmorfac(gdp%gdmorpar, timhr, julday)
    endif
    !
    nm_pos =  1
    if (scour) then
       !
       ! Second parameter is zero: save taubmx(*) in gdp%gdscour
       !
       call shearx(taubmx, 0, gdp)
    endif
    !
    ! Determine total thickness of the mud layers
    ! to be used in computation of skin friction (Soulsby 2004)
    !
    if (bsskin) then
       call detthcmud(gdp%gdmorlyr, thcmud)
    endif
    !
    ! Initialisation:
    ! reset sediment sources and sinks
    !     set default kmxsed layer
    !     set kfsed
    !
    lstart = max(lsal, ltem)
    sag    = sqrt(ag)
    !
    ! Reset Sourse and Sinkse arrays for all (nm,l)
    !
    kmxsed  = 1
    sinkse  = 0.0_fp
    sourse  = 0.0_fp
    sour_im = 0.0_fp
    ! source and sink terms fluff layer
    if (iflufflyr>0) then
        sinkf = 0.0_fp
        sourf = 0.0_fp
    endif
    !
    ! Reset Sediment diffusion arrays for (nm,k,l)
    !
    seddif = 0.0_fp
    rca    = 0.0_fp
    !
    ! Reset Bed Shear Ratio for all nm and l = 1:lsedtot
    !                        
    taub   = 0.0_fp
    taurat = 0.0_fp
    !
    ! Set zero bedload transport for all nm and l = 1:lsedtot
    !
    sbuu  = 0.0_fp
    sbvv  = 0.0_fp
    sbcu  = 0.0_fp
    sbcv  = 0.0_fp
    sbcuu = 0.0_fp
    sbcvv = 0.0_fp
    sbwu  = 0.0_fp
    sbwv  = 0.0_fp
    sbwuu = 0.0_fp
    sbwvv = 0.0_fp
    sswu  = 0.0_fp
    sswv  = 0.0_fp
    sswuu = 0.0_fp
    sswvv = 0.0_fp
    sutot = 0.0_fp
    svtot = 0.0_fp
    !
    do nm = 1, nmmax
       if ((s1(nm) + real(dps(nm),fp))*kfs(nm) > sedthr) then
          kfsed(nm) = 1
       else
          kfsed(nm) = 0
       endif
    enddo
    !
    ! Determine fractions of all sediments the top layer and
    ! compute the mud fraction.
    !
    if (lsedtot > 1) then
       call getfrac(gdp%gdmorlyr,frac      ,anymud    ,mudcnt    , &
                  & mudfrac     ,gdp%d%nmlb,gdp%d%nmub)
    endif
    !
    ! Calculate velocity components and magnitude at the zeta points
    ! based on velocity in the bottom computational layer
    !
    call dwnvel(nmmax     ,kmax      ,icx       ,kcs       ,kfu       , &
              & kfv       ,kcu       ,kcv       ,s1        ,dps       , &
              & u0eul     ,v0eul     ,uuu       ,vvv       ,umod      , &
              & zumod     ,sig       ,hu        ,hv        ,kfsed     , &
              & deltau    ,deltav    ,gdp       )

    !
    ! Get the reduction factor if thickness of sediment at bed is less than
    ! user specified threshold. Also get maximum erosion source SRCMAX
    ! (used for cohesive sediments).
    !
    dtmor = dt * morfac
    !
    call getfixfac(gdp%gdmorlyr, gdp%d%nmlb, gdp%d%nmub, lsedtot, &
                 & nmmax       , fixfac    , ffthresh  )
    !
    ! Set fixfac to 1.0 for tracer sediments and adjust frac
    !
    istat = bedcomp_getpointer_integer(gdp%gdmorlyr, 'IUnderLyr', iunderlyr)
    if (ffthresh>0.0_hp .or. iunderlyr/=1) then
       srcmax = 1.0e+10_fp
    elseif (iunderlyr==1) then
       istat = bedcomp_getpointer_realprec(gdp%gdmorlyr,'bodsed',bodsed)
       do l = 1, lsed
          if (ffthresh<1.0e-10_fp) then
             !
             ! Compute SRCMAX (only used for cohesive sediments)
             !
             do nm = 1, nmmax
                !
                ! If user-specified THRESH is <= 0.0, the erosion flux is effectively not limited by FIXFAC since ffthresh is 1e-10
                ! but by the amount of sediment that is available
                !
                srcmax(nm, l) = bodsed(l, nm)/dtmor
!                srcmax(nm, l) = bodsed(l, nm)*cdryb(l)/dtmor
             enddo
          endif
          !
          if (sedtrcfac(l)>0.0_fp) then
             grkg = 1.0_fp / (rhosol(l)*pi*sedd50(l)**3/6.0_fp) ! Number of grains per kg
             grm2 = 0.5_fp / (pi*sedd50(l)**2) ! Number of grains per m^2 -- Not quite correct: maximum area of grain is pi*r^2 not pi*d^2, using porosity factor of 0.5
             do nm = 1, nmmax
                fixfac(nm, l) = 1.0_fp
                grlyrs = bodsed(l, nm) * grkg / grm2 ! Number of grain layers
                frac(nm, l) = min(max(0.0_fp, grlyrs), 1.0_fp)*sedtrcfac(l)
             enddo
          endif
       enddo
    endif
    !
    ! in case of multiple (non-mud) fractions, the following quantities
    ! --- that are initialized in INISED --- may be time-dependent and
    ! they must be updated here or after updating the bed levels in
    ! BOTT3D. Since we do it here, these quantities will lag a half time
    ! step behind on the output files. If these statements are moved to
    ! BOTT3D, the GETFRAC call above must be shifted too.
    !
    if (lsedtot-nmudfrac > 1) then
       !
       ! calculate arithmetic mean sediment diameter Dm
       ! calculate geometric mean sediment diameter Dg
       ! calculate percentiles Dxx
       !
       call compdiam(frac      ,sedd50    ,sedd50    ,sedtyp    ,lsedtot   , &
                   & logsedsig ,nseddia   ,logseddia ,nmmax     ,gdp%d%nmlb, &
                   & gdp%d%nmub,xx        ,nxx       ,max_mud_sedtyp, min_dxx_sedtyp, &
                   & sedd50fld ,dm        ,dg        ,dxx       ,dgsd      )
       !
       ! determine hiding & exposure factors
       !
       call comphidexp(frac      ,dm        ,nmmax     ,lsedtot   , &
                     & sedd50    ,hidexp    ,ihidexp   ,asklhe    , &
                     & mwwjhe    ,gdp%d%nmlb,gdp%d%nmub)
       !
       ! compute sand fraction
       !
       call compsandfrac(frac, sedd50, nmmax, lsedtot, sedtyp, &
                    & max_mud_sedtyp, sandfrac, sedd50fld, &
                    & gdp%d%nmlb, gdp%d%nmub)
    endif
    !
    do nm = 1, nmmax
       !
       ! calculate and store bed slopes at U and V points
       !
       nmu = nm + icx
       num = nm + icy
       if (abs(kcu(nm)) > 0) then
          dzduu(nm) = (real(dps(nmu),fp) - real(dps(nm),fp))/gvu(nm)
       else
          dzduu(nm) = 0.0_fp
       endif
       if (abs(kcv(nm)) > 0) then
          dzdvv(nm) = (real(dps(num),fp) - real(dps(nm),fp))/guv(nm)
       else
          dzdvv(nm) = 0.0_fp
       endif
    enddo
    !
    !================================================================
    !    Start of sand part
    !================================================================
    !
    ! Start of main loop over sediment fractions for suspended sediment
    ! sources, sinks, equilibrium concentrations and vertical diffusion
    ! coefficients, and bed-load transport vector components at water
    ! level points
    !
    do nm = 1, nmmax
       if (kfs(nm)/=1 .or. kcs(nm)>2) cycle
       !
       ! do not calculate sediment sources, sinks, and bed load
       ! transport in areas with very shallow water.
       !
       if (kfsed(nm) == 0) then
          !
          ! Very shallow water:
          ! set sediment diffusion coefficient
          ! and set zero equilibrium concentrations
          !
          if (kmax>1) then
             do l = 1, lsed
                do k = 1, kmax
                   seddif(nm, k, l) = dicww(nm, k)
                enddo
                rsedeq(nm, l) = 0.0_fp
             enddo
          endif
          cycle
       endif
       !
       ! kfsed(nm) == 1
       !
       h0   = max(0.01_fp, s0(nm) + real(dps(nm),fp))
       h1   = max(0.01_fp, s1(nm) + real(dps(nm),fp))
       nmd  = nm - icx
       ndm  = nm - icy
       call nm_to_n_and_m(nm, n, m, gdp)
       ! In z-layer use kmaxlc, siglc and thicklc for morphology (like sigma-layer).
       if (kmax>1) then  
          !
          ! 3D CASE
          !
          kbed    = kmax
       else
          kbed    = 1
       endif
       !
       ! Compute bed slopes at cell centre (used in Van Rijn 2004)
       ! Take the average slope from velocity points, but avoid slopes onto dry points
       ! Also compute maximum absolute slope, use for cohesive material (clay) being undercut 
       !
       dzduz = 0.0_fp
       maxslope = 0.0_fp
       if (kfs(nmu)>0) then
           dzduz = dzduz + 0.5_fp*dzduu(nm)
           maxslope = max(maxslope, abs(dzduu(nm)))
       endif
       if (kfs(nmd)>0) then
           dzduz = dzduz + 0.5_fp*dzduu(nmd)
           maxslope = max(maxslope, abs(dzduu(nmd)))
       endif
       dzdvz = 0.0_fp
       if (kfs(num)>0) then
           dzdvz = dzdvz + 0.5_fp*dzdvv(nm)
           maxslope = max(maxslope, abs(dzdvv(nm)))
       endif
       if (kfs(ndm)>0) then
           dzdvz = dzdvz + 0.5_fp*dzdvv(ndm)
           maxslope = max(maxslope, abs(dzdvv(ndm)))
       endif
       !
       ! Compute depth-averaged velocity components at cell centre
       !
       ku = max(1,kfu(nmd) + kfu(nm))
       kv = max(1,kfv(ndm) + kfv(nm))
       umean = 0.0_fp
       vmean = 0.0_fp
       do k = 1, kmax
          umean = umean + thick(k)*(u0eul(nm,k) + u0eul(nmd,k))/ku
          vmean = vmean + thick(k)*(v0eul(nm,k) + v0eul(ndm,k))/kv
       enddo
       velm = sqrt(umean**2+vmean**2)
       !
       ubed = (u0eul(nm,kbed) + u0eul(nmd,kbed))/ku
       vbed = (v0eul(nm,kbed) + v0eul(ndm,kbed))/kv
       velb = sqrt(ubed**2 + vbed**2)
       if (kmax>1) then
          zvelb = 0.5_fp*thick(kmax)*h1
       else
          zvelb = h1/ee
       endif
       !
       ! Calculate current related roughness
       !
       kn    = max(1, kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm))
       z0cur = (  kfu(nmd)*z0ucur(nmd) + kfu(nm)*z0ucur(nm) &
             &  + kfv(ndm)*z0vcur(ndm) + kfv(nm)*z0vcur(nm)  )/kn
       !
       ! Calculate total (possibly wave enhanced) roughness
       !
       z0rou = (  kfu(nmd)*z0urou(nmd) + kfu(nm)*z0urou(nm) &
             &  + kfv(ndm)*z0vrou(ndm) + kfv(nm)*z0vrou(nm)  )/kn
       chezy = sag * log( 1.0_fp + h1/max(1.0e-8_fp,ee*z0rou) ) / vonkar
       !
       ! bed shear stress as used in flow, or
       ! skin friction following Soulsby; "Bed shear stress under
       ! combined waves and currents on rough and smoooth beds"
       ! Estproc report TR137, 2004
       !
       if (bsskin) then
          !
          ! Compute bed stress resulting from skin friction
          !
          if (iflufflyr>0) then
             afluff = get_alpha_fluff(iflufflyr, lsed, nm, mfluff(:,nm), gdp%gdtrapar, gdp%gdsedpar, timhr)
          else
             afluff = 0.0_fp  
          endif
          call compbsskin(umean, vmean, h1, wave, uorb(nm), tp(nm), &
                           & teta(nm), thcmud(nm), mudfrac(nm), taub(nm), &
                           & rhowat(nm,kbed), vicmol, gdp%gdsedpar, afluff)
       else
          !
          ! use max bed shear stress, rather than mean
          !
          taub(nm) = taubmx(nm)
       endif
       !
       ustarc = umod(nm)*vonkar/log(1.0_fp + zumod(nm)/z0rou)
       if (scour) then
          !
          ! Calculate extra stress (tauadd) for point = nm, if so required by
          ! user input. Increment TAUB(MX) and USTARC.
          !
          call shearx(tauadd, nm, gdp)
          taub(nm) = sqrt(taub(nm)**2 + tauadd**2)
          !
          tauc = rhowat(nm,kbed)*ustarc**2
          tauc = sqrt(tauc**2 + tauadd**2)
          ustarc = sqrt(tauc/rhowat(nm,kbed))
       else
          tauadd = 0.0_fp
       endif
       !
       ! Compute effective depth averaged velocity
       !
       utot  = ustarc * chezy / sag
       u     = utot * uuu(nm) / (umod(nm)+eps)
       v     = utot * vvv(nm) / (umod(nm)+eps)
       !
       if (lsal > 0) then
          salinity = r0(nm, kbed, lsal)
       else
          salinity = saleqs
       endif
       if (ltem > 0) then
          temperature = r0(nm, kbed, ltem)
       else
          temperature = temeqs
       endif
       !
       ! Calculate Van Rijn's reference height
       !
       if (iopkcw==1) then
          rc = 30.0_fp*z0cur
       else
          rc = rdc
       endif
       taks0 = max(aksfac*rc, 0.01_fp*h1)
       !
       if (wave) then
          if (tp(nm)>0.0_fp) then
             delr  = 0.025_fp
             taks0 = max(0.5_fp*delr, taks0)
          endif
       endif
       !
       ! Limit maximum aks to 20% of water depth
       ! (may be used when water depth becomes very small)
       !
       taks0 = min(taks0, 0.2_fp*h1)
       !
       ! Input parameters are passed via dll_reals/integers/strings-arrays
       !
       if (max_reals < MAX_RP) then
          write(errmsg,'(a)') 'Insufficient space to pass real values to transport routine.'
          call prterr (lundia,'U021', trim(errmsg))
          call d3stop(1, gdp)
       endif
       dll_reals(RP_TIME ) = real(timsec    ,hp)
       dll_reals(RP_EFUMN) = real(u         ,hp)
       dll_reals(RP_EFVMN) = real(v         ,hp)
       dll_reals(RP_EFVLM) = real(utot      ,hp)
       dll_reals(RP_UCHAR) = real(uuu(nm)   ,hp)
       dll_reals(RP_VCHAR) = real(vvv(nm)   ,hp)
       dll_reals(RP_VELCH) = real(umod(nm)  ,hp)
       dll_reals(RP_ZVLCH) = real(zumod(nm) ,hp)
       dll_reals(RP_DEPTH) = real(h1        ,hp)
       dll_reals(RP_CHEZY) = real(chezy     ,hp)
       if (wave) then
          dll_reals(RP_HRMS ) = real(min(gammax*h1, hrms(nm)) ,hp)
          dll_reals(RP_TPEAK) = real(tp(nm)                   ,hp)
          dll_reals(RP_TETA ) = real(teta(nm)                 ,hp)
          dll_reals(RP_RLAMB) = real(rlabda(nm)               ,hp)
          dll_reals(RP_UORB ) = real(uorb(nm)                 ,hp)
       else
          dll_reals(RP_HRMS ) = 0.0_hp
          dll_reals(RP_TPEAK) = 0.0_hp
          dll_reals(RP_TETA ) = 0.0_hp
          dll_reals(RP_RLAMB) = 0.0_hp
          dll_reals(RP_UORB ) = 0.0_hp
       endif
       dll_reals(RP_D10MX) = real(dxx(nm,i10),hp)
       dll_reals(RP_D90MX) = real(dxx(nm,i90),hp)
       dll_reals(RP_MUDFR) = real(mudfrac(nm),hp)
       dll_reals(RP_RHOWT) = real(rhowat(nm,kbed),hp) ! Density of water
       dll_reals(RP_SALIN) = real(salinity       ,hp)
       dll_reals(RP_TEMP ) = real(temperature    ,hp)
       dll_reals(RP_GRAV ) = real(ag             ,hp)
       dll_reals(RP_VICML) = real(vicmol         ,hp)
       dll_reals(RP_TAUB ) = real(taub(nm)       ,hp)
       dll_reals(RP_UBED ) = real(ubed           ,hp)
       dll_reals(RP_VBED ) = real(vbed           ,hp)
       dll_reals(RP_VELBD) = real(velb           ,hp)
       dll_reals(RP_ZVLBD) = real(zvelb          ,hp)
       dll_reals(RP_VNKAR) = real(vonkar         ,hp)
       dll_reals(RP_Z0CUR) = real(z0cur          ,hp)
       dll_reals(RP_Z0ROU) = real(z0rou          ,hp)
       dll_reals(RP_DG   ) = real(dg(nm)         ,hp)
       dll_reals(RP_SNDFR) = real(sandfrac(nm)   ,hp)
       dll_reals(RP_DGSD ) = real(dgsd(nm)       ,hp)
       if (ltur >= 1) then
          dll_reals(RP_KTUR ) = real(rtur0(nm,kbed,1),hp)
       endif
       dll_reals(RP_UMEAN) = real(umean          ,hp)
       dll_reals(RP_VMEAN) = real(vmean          ,hp)
       dll_reals(RP_VELMN) = real(velm           ,hp)
       dll_reals(RP_USTAR) = real(ustarc         ,hp)
       dll_reals(RP_ZB   ) = real(-dps(nm)       ,hp)
       !
       if (max_integers < MAX_IP) then
          write(errmsg,'(a)') 'Insufficient space to pass integer values to transport routine.'
          call prterr (lundia,'U021', trim(errmsg))
          call d3stop(1, gdp)
       endif
       dll_integers(IP_NM   ) = nm
       dll_integers(IP_N    ) = n
       dll_integers(IP_M    ) = m
       dll_integers(IP_NST ) = nst
       !
       if (max_strings < MAX_SP) then
          write(errmsg,'(a)') 'Insufficient space to pass strings to transport routine.'
          call prterr (lundia,'U021', trim(errmsg))
          call d3stop(1, gdp)
       endif
       dll_strings(SP_RUNID) = gdp%runid
       !
       ! total mass in fluff layer
       !
       mfltot = 0.0_fp
       if (iflufflyr>0) then
            do l = 1, lsedtot
                mfltot = mfltot + max(0.0_fp,mfluff(l,nm))
            enddo
       endif
       !
       do l = 1, lsedtot
          ll = lstart + l
          !
          ! Copy the globally defined l-dependent parameters of array par to localpar.
          ! All nm-/l-based redefinitions of these parameters are performed
          ! on localpar, thus ensuring that the global array par is not
          ! messed up with specific, nm-/l-dependent data.
          !
          call get_transport_parameters(gdp%gdtrapar, l, nm, timhr, localpar)
          !
          ! fraction specific quantities
          !
          dll_reals(RP_HIDEX)    = real(hidexp(nm,l) ,hp)
          dll_reals(RP_RHOSL)    = real(rhosol(l) ,hp)
          dll_integers(IP_ISED ) = l
          dll_strings(SP_USRFL)  = dll_usrfil(l)
          !
          if (.not.has_bedload(tratyp(l))) then
             !
             ! sediment transport governed by erosion and deposition fluxes
             !
             dll_reals(RP_D50  ) = 0.0_hp
             dll_reals(RP_DSS  ) = 0.0_hp
             dll_reals(RP_DSTAR) = 0.0_hp
             !
             ! l <= lsed for fractions with advection-diffusion transport
             !
             dll_reals(RP_SETVL) = real(ws(nm, kbed, l)  ,hp) ! Vertical velocity near bedlevel
             if (flmd2l) then
                 localpar(11) = entr(nm)
             endif
             !
             do k = 0, kmax
                wslc(k)   = ws(nm, k, l)
             enddo
             !
             ! Fluff layer parameters
             !
             fracf   = 0.0_fp
             if (iflufflyr>0) then
                if (mfltot>0.0_fp) fracf   = max(0.0_fp,mfluff(l,nm))/mfltot
             endif
             !
             kmaxsd = kmax ! for mud fractions kmaxsd points to the grid cell at the bottom of the water column
             thick0        = thick(kmaxsd) * h0
             thick1        = thick(kmaxsd) * h1
             call erosilt(thick       ,kmax        ,wslc        ,lundia       , &
                        & thick0      ,thick1      ,fixfac(nm,l),srcmax(nm, l), &
                        & frac(nm,l)  ,oldmudfrac  ,flmd2l      ,iform(l)     , &
                        & npar        ,localpar    ,max_integers,max_reals    , &
                        & max_strings ,dll_function(l),dll_handle(l),dll_integers, &
                        & dll_reals   ,dll_strings ,iflufflyr   ,mfltot       , &
                        & fracf       ,maxslope    ,wetslope    , &
                        & error ,wstau(nm) ,sinktot ,sourse(nm,l), sourfluff)
             if (error) call d3stop(1, gdp)
             !
             if (iflufflyr>0) then
                if (iflufflyr==2) then
                   sinkf(l,nm)  = sinktot*(1.0_fp - depfac(l,nm))
                   sinkse(nm,l) = sinktot*depfac(l,nm)
                else
                   sinkf(l,nm)  = sinktot
                   sinkse(nm,l) = 0.0_fp
                endif
                !
                ! prevent fluff layer source exceeding available mass
                !
                if (mfltot <= 0.0_fp) then
                   sourf(l,nm) = 0.0_fp
                else
                   sourf(l,nm) = min(sourfluff, mfltot/dt)
                end if
             else
                sinkse(nm,l) = sinktot
                ! sourse(nm,l) already set (sourfluff = 0)
             endif
             !
             if (kmax > 1) then 
                !
                ! For 3D model set sediment diffusion coefficient
                ! NOTE THAT IF ALGEBRAIC OR K-L TURBULENCE MODEL IS USED THEN WAVES
                ! ONLY AFFECT THE VERTICAL TURBULENT MIXING VIA THE ENHANCED BED
                ! ROUGHNESS
                !
                do k = 0, kmax
                   seddif(nm, k, l) = dicww(nm, k)
                enddo
             endif
             !
             ! l runs from 1 to lsedtot, kmxsed is defined for 1:lsed
             ! The first lsed fractions are the suspended fractions (including cohesive ones),
             ! so this goes right
             !
             kmxsed(nm, l) = kmax
             cycle
          endif
          !
          ! sediment transport governed by bedoad vector and reference concentration
          !
          suspfrac = has_advdiff(tratyp(l))
          !
          ! (Re)set of Prandtl-Schmidt number moved to TKECOF
          tsd  = -999.0_fp
          di50 = sedd50(l)
          if (di50 < 0.0_fp) then
             !
             ! Space varying sedd50 specified in array sedd50fld:
             ! Recalculate dstar, tetacr and taucr for each nm,l - point
             ! This code is copied from inised (uniform sedd50)
             !
             di50     = sedd50fld(nm)
             drho     = (rhosol(l)-rhowat(nm,kbed)) / rhowat(nm,kbed)
             dstar(l) = di50 * (drho*ag/vicmol**2)**0.3333_fp
             if (dstar(l) < 1.0_fp) then
                if (iform(l) == -2 .or. iform(l) == -4) then
                   tetacr(l) = 0.115_fp / (dstar(l)**0.5_fp)
                else
                   tetacr(l) = 0.24_fp / dstar(l)
                endif
             elseif (dstar(l) <= 4.0_fp) then
                if (iform(l) == -2 .or. iform(l) == -4) then
                   tetacr(l) = 0.115_fp / (dstar(l)**0.5_fp)
                else
                   tetacr(l) = 0.24_fp / dstar(l)
                endif
             elseif (dstar(l)>4.0_fp .and. dstar(l)<=10.0_fp) then
                tetacr(l) = 0.14_fp  / (dstar(l)**0.64_fp)
             elseif (dstar(l)>10.0_fp .and. dstar(l)<=20.0_fp) then
                tetacr(l) = 0.04_fp  / (dstar(l)**0.1_fp)
             elseif (dstar(l)>20.0_fp .and. dstar(l)<=150.0_fp) then
                tetacr(l) = 0.013_fp * (dstar(l)**0.29_fp)
             else
                tetacr(l) = 0.055_fp
             endif
             taucr(l) = factcr * (rhosol(l)-rhowat(nm,kbed)) * ag * di50 * tetacr(l)
          endif
          !
          if (suspfrac) then
             tsigmol = sigmol(ll)
             tdss    = dss(nm, l)
             !
             ! Assumption: l <= lsed (which should hold for suspfrac)
             !
             twsk    = ws(nm, kbed, l)
          else
             !
             ! use dummy values for bedload fractions
             !
             tsigmol =  1.0_fp
             tdss    = di50
             twsk    =  0.0_fp
          endif
          !
          ! NONCOHESIVE fraction specific quantities
          !
          dll_reals(RP_D50  ) = real(di50    ,hp)
          dll_reals(RP_DSS  ) = real(tdss    ,hp)
          dll_reals(RP_DSTAR) = real(dstar(l),hp)
          dll_reals(RP_SETVL) = real(twsk    ,hp) ! Vertical velocity near bedlevel
          localpar(1) = ag
          localpar(2) = rhowat(nm,kbed) ! rhow
          localpar(3) = rhosol(l)
          localpar(4) = (rhosol(l)-rhowat(nm,kbed)) / rhowat(nm,kbed)
          localpar(5) = 1.0E-6     ! rnu    from md-tran.*
          localpar(6) = di50
          !
          ! SWITCH 2DH/3D SIMULATIONS
          !
          if (kmax > 1) then
             !
             ! 3D CASE
             !
             if (suspfrac) then
                !
                ! Fill local 1dv arrays with fall velocity and diffusivity
                !
                !
                ! Assumption: l <= lsed (which should hold for suspfrac)
                !
                do k = 0, kmax
                   wslc(k)   = ws(nm, k, l)
                   dcwwlc(k) = dicww(nm, k)
                enddo
             endif
             taks = taks0
             !
             do k = 1, kmax
                concin3d(k) = max(0.0_fp , r0(nm,k,ll))
             enddo
             !
             ! Solve equilibrium concentration vertical and
             ! integrate over vertical
             !
             call eqtran(sig       ,thick     ,kmax      ,wslc      ,ltur      , &
                       & frac(nm,l),tsigmol   ,dcwwlc    ,lundia    ,taucr(l)  , &
                       & rksr(nm)  ,3         ,lsecfl    ,spirint   ,suspfrac  , &
                       & tetacr(l) ,concin3d  , &
                       & dzduz     ,dzdvz     ,ubot(nm)  ,tauadd    ,sus       , &
                       & bed       ,susw      ,bedw      ,espir     ,wave      , &
                       & scour     ,ubot_from_com        ,camax     ,eps       , &
                       & iform(l)  ,npar      ,localpar  ,max_integers,max_reals, &
                       & max_strings,dll_function(l),dll_handle(l),dll_integers,dll_reals, &
                       & dll_strings, &
                       & taks      ,caks      ,taurat(nm,l),sddflc  ,rsdqlc    , &
                       & kmaxsd    ,conc2d    ,sbcu(nm,l ),sbcv(nm,l),sbwu(nm,l), &
                       & sbwv(nm,l),sswu(nm,l),sswv(nm,l),tdss      ,caks_ss3d , &
                       & aks_ss3d  ,ust2(nm)  ,tsd       ,error     )
             if (error) call d3stop(1, gdp)
             if (gdp%gdmorpar%moroutput%sedpar) then
                 do i = 1,gdp%gdtrapar%noutpar(l)
                     j = gdp%gdtrapar%ioutpar(i,l)
                     gdp%gdtrapar%outpar(j, nm) = localpar(i)
                 enddo
             endif
             if (suspfrac) then
                aks(nm, l) = taks
                dss(nm, l) = tdss
                !
                ! Copy results into arrays
                !
                kmxsed(nm, l) = kmaxsd
                rsedeq(nm, l) = rsdqlc(kmaxsd)
                do k = 1, kmax
                   seddif(nm, k, l) = sddflc(k)
                enddo 
                !
                ! Source and sink terms for main 3d computation
                ! note: terms are part explicit, part implicit, see
                ! thesis of Giles Lesser, May 2000
                !
                thick0        = thick(kmaxsd) * h0
                thick1        = thick(kmaxsd) * h1
                call soursin_3d  (h1                ,thick0         ,thick1             , &
                               &  sig(kmaxsd)       ,thick(kmaxsd)  ,r0(nm,kmaxsd,ll)   , &
                               &  vicmol            ,sigmol(ll)     ,seddif(nm,kmaxsd,l), &
                               &  rhosol(l)         ,caks_ss3d      ,ws(nm,kmaxsd,l)    , &
                               &  aks_ss3d          ,sourse(nm,l)   ,sour_im(nm,l)      , &
                               &  sinkse(nm,l) )
                ! Impose relatively large vertical diffusion
                ! coefficients for sediment in layer interfaces from
                ! bottom of reference cell downwards, to ensure little
                ! gradient in sed. conc. exists in this area.
                !
                difbot = 10.0_fp * ws(nm,kmaxsd,l) * thick1
                do k = kmaxsd, kmax
                   seddif(nm, k, l) = difbot
                enddo
             endif ! suspfrac
          else
             !
             ! kmax = 1
             ! 2D CASE (Numerical approximation)
             !
             if (suspfrac) then
                !
                ! Fill local 1dv arrays with fall velocity and
                ! diffusivity
                !
                do k2d = 0, kmax2d
                   ws2d(k2d)   = ws(nm, kbed, l)
                   dcww2d(k2d) = 0.0_fp
                enddo
             endif
             trsedeq = 0.0_fp
             taks    = taks0
             !
             if (lsecfl > 0) then
                spirint = r0(nm, kbed, lsecfl)
             else
                spirint = 0.0_fp
             endif
             !
             ! Solve equilibrium concentration vertical and
             ! integrate over vertical; compute bedload
             ! transport excluding slope effects.
             !
             call eqtran(sig2d     ,thck2d    ,kmax2d    ,ws2d      ,ltur      , &
                       & frac(nm,l),tsigmol   ,dcww2d    ,lundia    ,taucr(l)  , &
                       & rksr(nm)  ,2         ,lsecfl    ,spirint   ,suspfrac  , &
                       & tetacr(l) ,concin2d  , &
                       & dzduz     ,dzdvz     ,ubot(nm)  ,tauadd    ,sus       , &
                       & bed       ,susw      ,bedw      ,espir     ,wave      , &
                       & scour     ,ubot_from_com        ,camax     ,eps       , &
                       & iform(l)  ,npar      ,localpar  ,max_integers,max_reals, &
                       & max_strings,dll_function(l),dll_handle(l),dll_integers,dll_reals, &
                       & dll_strings, &
                       & taks      ,caks      ,taurat(nm,l),sddf2d  ,rsdq2d    , &
                       & kmaxsd    ,trsedeq   ,sbcu(nm,l),sbcv(nm,l),sbwu(nm,l), &
                       & sbwv(nm,l),sswu(nm,l),sswv(nm,l),tdss      ,caks_ss3d , &
                       & aks_ss3d  ,ust2(nm)  ,tsd       ,error     )
             if (error) call d3stop(1, gdp)
             if (gdp%gdmorpar%moroutput%sedpar) then
                 do i = 1,gdp%gdtrapar%noutpar(l)
                     j = gdp%gdtrapar%ioutpar(i,l)
                     gdp%gdtrapar%outpar(j, nm) = localpar(i)
                 enddo
             endif
             if (suspfrac) then
                aks   (nm, l)    = taks
                dss   (nm, l)    = tdss
                rsedeq(nm, l)    = trsedeq
                kmxsed(nm, l)    = kbed
                !
                ! Galappatti time scale and source and sink terms
                !
                call soursin_2d(umod(nm)      ,ustarc        ,h0            ,h1        , &
                              & ws(nm,kbed,l) ,tsd           ,trsedeq       ,factsd    , &
                              & sourse(nm,l)  ,sour_im(nm,l) ,sinkse(nm,l)  )
             endif ! suspfrac
          endif ! kmax = 1
          if (suspfrac) then
             rca(nm, l) = caks * rhosol(l)
          endif
       enddo ! next sediment fraction
    enddo ! next nm point
    !
    ! Reduce the source and sink terms to avoid large bed level changes
    ! Note: previous implementation forgot to multiply source/
    !       sink terms with the thickness for the 2Dh case
    !
    call   red_soursin (nmmax     ,kmax      ,thick     , &
                      & lsal      ,ltem      ,lsed      ,lsedtot   , &
                      & dps       ,s0        ,s1        ,r0        , &
                      & nst       ,gdp       )
    !
    ! Fill sutot and svtot
    !
    do l = 1,lsedtot
       if (has_bedload(tratyp(l))) then
          do nm = 1, nmmax
             sutot(nm, l) = sbcu(nm, l) + sbwu(nm, l) + sswu(nm, l)
             svtot(nm, l) = sbcv(nm, l) + sbwv(nm, l) + sswv(nm, l)
          enddo
       endif
    enddo
    !
    ! Upwind scheme for bed load and wave driven transport
    ! Convert sand bed load transport to U and V points using upwind scheme
    !
    if (bed > 0.0_fp) then
       !
       ! Upwind bed load transport
       !
       call upwbed(sbcu      ,sbcv      ,sbcuu     ,sbcvv     ,kfu       , &
                 & kfv       ,kcs       ,kfsed     ,lsedtot   , &
                 & nmmax     ,icx       ,icy       ,sutot     ,svtot     , &
                 & gdp       )
    endif
    !
    if (bedw>0.0_fp .and. wave) then
       !
       ! Upwind wave-related bed load load transports
       !
       call upwbed(sbwu      ,sbwv      ,sbwuu     ,sbwvv     ,kfu       , &
                 & kfv       ,kcs       ,kfsed     ,lsedtot   , &
                 & nmmax     ,icx       ,icy       ,sutot     ,svtot     , &
                 & gdp       )
    endif
    !
    if (susw>0.0_fp .and. wave) then
       !
       ! Upwind wave-related suspended load transports
       !
       call upwbed(sswu      ,sswv      ,sswuu     ,sswvv     ,kfu       , &
                 & kfv       ,kcs       ,kfsed     ,lsedtot   , &
                 & nmmax     ,icx       ,icy       ,sutot     ,svtot     , &
                 & gdp       )
    endif
    !

    !
    ! Bed-slope and sediment availability effects for
    ! current-related bed load transport
    !
    if (bed > 0.0_fp) then
       call adjust_bedload(nmmax     ,icx       ,icy       ,kcs       , &
              & kcu       ,kcv       ,kfu       ,kfv       ,lsedtot   , &
              & sbcuu     ,sbcvv     ,sbuut     ,sbvvt     ,dzduu     , &
              & dzdvv     ,taurat    ,frac      ,fixfac    ,ust2      , &
              & hu        ,hv        ,dm        ,hidexp    ,.true.    , &
              & .true.    ,rhowat    ,kmax      ,dps       ,gsqs      , &
              & guu       ,gvv       ,guv       ,gvu       ,kbed      , &
              & gdp       )
    endif
    !
    ! Bed-slope and sediment availability effects for
    ! wave-related bed load transport
    !
    if (bedw>0.0_fp .and. wave) then
       call adjust_bedload(nmmax     ,icx       ,icy       ,kcs       , &
              & kcu       ,kcv       ,kfu       ,kfv       ,lsedtot   , &
              & sbwuu     ,sbwvv     ,sbuut     ,sbvvt     ,dzduu     , &
              & dzdvv     ,taurat    ,frac      ,fixfac    ,ust2      , &
              & hu        ,hv        ,dm        ,hidexp    ,.true.    , &
              & .false.   ,rhowat    ,kmax      ,dps       ,gsqs      , &
              & guu       ,gvv       ,guv       ,gvu       ,kbed      , &
              & gdp       )
    endif
    !
    ! Sediment availability effects for
    ! wave-related suspended load transport
    !
    if (susw>0.0_fp .and. wave) then
       call adjust_bedload(nmmax     ,icx       ,icy       ,kcs       , &
              & kcu       ,kcv       ,kfu       ,kfv       ,lsedtot   , &
              & sswuu     ,sswvv     ,sbuut     ,sbvvt     ,dzduu     , &
              & dzdvv     ,taurat    ,frac      ,fixfac    ,ust2      , &
              & hu        ,hv        ,dm        ,hidexp    ,.false.   , &
              & .false.   ,rhowat    ,kmax      ,dps       ,gsqs      , &
              & guu       ,gvv       ,guv       ,gvu       ,kbed      , &
              & gdp       )
    endif
    !
    ! Summation of current-related and wave-related transports
    !
    do l = 1,lsedtot
       if (has_bedload(tratyp(l))) then
          do nm = 1, nmmax
             sbuu(nm, l) = sbcuu(nm, l) + sbwuu(nm, l) + sswuu(nm, l)
             sbvv(nm, l) = sbcvv(nm, l) + sbwvv(nm, l) + sswvv(nm, l)
          enddo
       endif
    enddo
    !
    ! Update sourse fluxes due to sand-mud interaction
    !
    do nm = 1, nmmax
        if (pmcrit(nm)<0.0_fp) cycle
        if (mudfrac(nm)<=0.0_fp .or. mudfrac(nm)>=1.0_fp) cycle
        !
        ! Compute erosion velocities
        !
        E = 0.0_fp        
        do l = 1, lsed
            ll = lstart + l
            kmaxsd = kmxsed (nm,l)
            if (frac(nm,l)>0.0_fp)  E(l) = (sourse(nm,l) - sour_im(nm,l)*r0(nm,kmaxsd,ll))/(cdryb(l)*frac(nm,l))
        enddo
        !
        ! Recompute erosion velocities
        !
        call sand_mud(lsed, E, frac(nm,:), mudfrac(nm), sedtyp, max_mud_sedtyp, pmcrit(nm))
        !
        ! Recompute erosion fluxes
        ! only explicit part of erosion flux is changed 
        ! (is effectively the same as changing the equilibrium concentration)
        !
        do l = 1, lsed
            ll = lstart + l
            kmaxsd = kmxsed (nm,l)
            sourse(nm,l) = frac(nm,l)*cdryb(l)*E(l) + sour_im(nm,l)*r0(nm,kmaxsd,ll)
        enddo
    enddo
    !
    ! Add implicit part of source term to sinkse
    !
    do l = 1, lsed
        do nm = 1, nmmax
            sinkse(nm, l) = sinkse(nm, l) + sour_im(nm, l)
        enddo
    enddo
    !
    ! Finally fill sour and sink arrays for both sand and silt
    ! note that sourse/sinkse and sourf/sinkf arrays are required for BOTT3D
    !
    do l = 1, lsed
       ll = lstart + l
       do nm = 1, nmmax
          k = kmxsed(nm,l)
          !
          !  sourse/sinkse/sourf/sinkf are defined per unit volume
          !  in sigma-model the sour/sink terms are defined per content of cell 
          !
          sour(nm, k, ll) = sour(nm, k, ll) + sourse(nm, l)
          sink(nm, k, ll) = sink(nm, k, ll) + sinkse(nm, l)
          if (iflufflyr>0) then
             sour(nm, k, ll) = sour(nm, k, ll) + sourf(l, nm)
             sink(nm, k, ll) = sink(nm, k, ll) + sinkf(l, nm)
          endif
       enddo
    enddo
    !
    ! DD-Mapper: copy sbuu and sbvv
    !
    nhystp = nxtstp(d3dflow_sediment, gdp)
    deallocate (localpar, stat = istat)
    end subroutine erosed