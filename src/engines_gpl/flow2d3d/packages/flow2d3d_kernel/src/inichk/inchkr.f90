subroutine inchkr(lundia    ,error     ,runid     ,timhr     ,dischy    , &
                & cyclic    ,sferic    ,grdang    ,temeqs    ,saleqs    , &
                & lturi     ,rouflo    ,rouwav    ,ktemp     ,temint    , &
                & evaint    ,gdp       )
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
!    Function: Initialises and checks various params. and arrays
!              were arrays can be initialized in INCHKI or come
!              from restart data
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use meteo
    use flow_tables
    !
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'fsm.i'
    integer(pntrsize)                    , pointer :: wrka1
    integer(pntrsize)                    , pointer :: wrka2
    integer(pntrsize)                    , pointer :: wrka3
    integer(pntrsize)                    , pointer :: wrkb1
    integer(pntrsize)                    , pointer :: wrkb2
    integer(pntrsize)                    , pointer :: wrkb3
    integer(pntrsize)                    , pointer :: wrkb4
    integer(pntrsize)                    , pointer :: zwork
    integer                              , pointer :: nmax
    integer                              , pointer :: mmax
    integer                              , pointer :: nlb
    integer                              , pointer :: nub
    integer                              , pointer :: mlb
    integer                              , pointer :: mub
    integer                              , pointer :: nmlb
    integer                              , pointer :: nmub
    integer                              , pointer :: ddbound
    integer                              , pointer :: nmaxus
    integer                              , pointer :: kmax
    integer                              , pointer :: nmaxd
    integer                              , pointer :: jstart
    integer                              , pointer :: nmmaxj
    integer                              , pointer :: nmmax
    integer                              , pointer :: lsts
    integer                              , pointer :: lstsc
    integer                              , pointer :: lstsci
    integer                              , pointer :: lsal
    integer                              , pointer :: lsed
    integer                              , pointer :: lsedtot
    integer                              , pointer :: ltem
    integer                              , pointer :: lsecfl
    integer                              , pointer :: lsec
    integer                              , pointer :: ltur
    integer                              , pointer :: nrob 
    integer                              , pointer :: nto
    integer                              , pointer :: ntof
    integer                              , pointer :: ntoq
    integer                              , pointer :: ntot
    integer                              , pointer :: kc
    integer                              , pointer :: kcd
    integer                              , pointer :: nsrc
    integer                              , pointer :: nsrcd
    integer                              , pointer :: ndro
    integer                              , pointer :: upwsrc
    integer                              , pointer :: itstrt
    integer                              , pointer :: itfinish
    integer                              , pointer :: itdrof
    integer                              , pointer :: itdroi
    integer                              , pointer :: itdrol
    integer                              , pointer :: julday
    real(fp)                             , pointer :: cp
    real(fp)                             , pointer :: gapres
    real(fp)                             , pointer :: rhum
    real(fp)                             , pointer :: tair
    real(fp)                             , pointer :: evapor
    real(fp)                             , pointer :: precipt
    real(fp), dimension(:)               , pointer :: rhumarr
    real(fp), dimension(:)               , pointer :: tairarr
    real(fp), dimension(:)               , pointer :: clouarr
    real(fp), dimension(:)               , pointer :: swrfarr
    real(fp), dimension(:)               , pointer :: secchi
    logical                              , pointer :: rhum_file
    logical                              , pointer :: tair_file
    logical                              , pointer :: clou_file
    logical                              , pointer :: prcp_file
    logical                              , pointer :: swrf_file
    logical                              , pointer :: scc_file
    real(fp)                             , pointer :: morfac
    integer                              , pointer :: morfacpar
    integer                              , pointer :: morfacrec
    integer                              , pointer :: morfactable
    type (handletype)                    , pointer :: morfacfile
    logical                              , pointer :: densin
    logical                              , pointer :: varyingmorfac
    integer                              , pointer :: nh_level
    real(fp)                             , pointer :: rhow
    real(fp)                             , pointer :: ag
    real(fp)                             , pointer :: z0
    real(fp)                             , pointer :: z0v
    integer                              , pointer :: iro
    integer                              , pointer :: irov
    logical                              , pointer :: wind
    logical                              , pointer :: temp
    logical                              , pointer :: const
    logical                              , pointer :: drogue
    logical                              , pointer :: wave
    logical                              , pointer :: struct
    logical                              , pointer :: cdwstruct
    logical                              , pointer :: sedim
    logical                              , pointer :: htur2d
    logical                              , pointer :: zmodel
    logical                              , pointer :: nonhyd
    logical                              , pointer :: roller
    logical                              , pointer :: lftrto
    logical                              , pointer :: veg3d
    logical                              , pointer :: bubble
    logical                              , pointer :: lfsdu
    integer(pntrsize)                    , pointer :: alfas
    integer(pntrsize)                    , pointer :: areau
    integer(pntrsize)                    , pointer :: areav
    integer(pntrsize)                    , pointer :: bruvai
    integer(pntrsize)                    , pointer :: c
    integer(pntrsize)                    , pointer :: cdwlsu
    integer(pntrsize)                    , pointer :: cdwlsv
    integer(pntrsize)                    , pointer :: cdwzbu
    integer(pntrsize)                    , pointer :: cdwzbv
    integer(pntrsize)                    , pointer :: cdwztu
    integer(pntrsize)                    , pointer :: cdwztv
    integer(pntrsize)                    , pointer :: cfurou
    integer(pntrsize)                    , pointer :: cfvrou
    integer(pntrsize)                    , pointer :: cvalu0
    integer(pntrsize)                    , pointer :: cvalv0
    integer(pntrsize)                    , pointer :: dddeta
    integer(pntrsize)                    , pointer :: dddksi
    integer(pntrsize)                    , pointer :: disch0
    integer(pntrsize)                    , pointer :: disch1
    integer(pntrsize)                    , pointer :: deltau
    integer(pntrsize)                    , pointer :: deltav
    integer(pntrsize)                    , pointer :: dfu
    integer(pntrsize)                    , pointer :: dfv
    integer(pntrsize)                    , pointer :: diapl
    integer(pntrsize)                    , pointer :: dicuv
    integer(pntrsize)                    , pointer :: dis
    integer(pntrsize)                    , pointer :: df
    integer(pntrsize)                    , pointer :: disch
    integer(pntrsize)                    , pointer :: discum
    integer(pntrsize)                    , pointer :: dp
    integer(pntrsize)                    , pointer :: dps
    integer(pntrsize)                    , pointer :: dpu
    integer(pntrsize)                    , pointer :: dpv
    integer(pntrsize)                    , pointer :: rint0
    integer(pntrsize)                    , pointer :: rint1
    integer(pntrsize)                    , pointer :: sink
    integer(pntrsize)                    , pointer :: sour
    integer(pntrsize)                    , pointer :: umdis0
    integer(pntrsize)                    , pointer :: umdis1
    integer(pntrsize)                    , pointer :: vmdis0
    integer(pntrsize)                    , pointer :: vmdis1
    integer(pntrsize)                    , pointer :: dxydro
    integer(pntrsize)                    , pointer :: dzdeta
    integer(pntrsize)                    , pointer :: dzdksi
    integer(pntrsize)                    , pointer :: enstro
    integer(pntrsize)                    , pointer :: entr
    integer(pntrsize)                    , pointer :: eroll0
    integer(pntrsize)                    , pointer :: eroll1
    integer(pntrsize)                    , pointer :: evap
    integer(pntrsize)                    , pointer :: ewabr0
    integer(pntrsize)                    , pointer :: ewabr1
    integer(pntrsize)                    , pointer :: ewave0
    integer(pntrsize)                    , pointer :: ewave1
    integer(pntrsize)                    , pointer :: facdss
    integer(pntrsize)                    , pointer :: grmasu
    integer(pntrsize)                    , pointer :: grmasv
    integer(pntrsize)                    , pointer :: grmsur
    integer(pntrsize)                    , pointer :: grmsvr
    integer(pntrsize)                    , pointer :: grfacu
    integer(pntrsize)                    , pointer :: grfacv
    integer(pntrsize)                    , pointer :: gsqs
    integer(pntrsize)                    , pointer :: guu
    integer(pntrsize)                    , pointer :: guv
    integer(pntrsize)                    , pointer :: gvu
    integer(pntrsize)                    , pointer :: gvv
    integer(pntrsize)                    , pointer :: hkru
    integer(pntrsize)                    , pointer :: hkrv
    integer(pntrsize)                    , pointer :: hrms
    integer(pntrsize)                    , pointer :: hu
    integer(pntrsize)                    , pointer :: hu0
    integer(pntrsize)                    , pointer :: hv
    integer(pntrsize)                    , pointer :: hv0
    integer(pntrsize)                    , pointer :: hydrbc
    integer(pntrsize)                    , pointer :: patm
    integer(pntrsize)                    , pointer :: porosu
    integer(pntrsize)                    , pointer :: porosv
    integer(pntrsize)                    , pointer :: precip
    integer(pntrsize)                    , pointer :: procbc
    integer(pntrsize)                    , pointer :: qu
    integer(pntrsize)                    , pointer :: qv
    integer(pntrsize)                    , pointer :: qxk
    integer(pntrsize)                    , pointer :: qyk
    integer(pntrsize)                    , pointer :: qzk
    integer(pntrsize)                    , pointer :: r0
    integer(pntrsize)                    , pointer :: r1
    integer(pntrsize)                    , pointer :: rho
    integer(pntrsize)                    , pointer :: rhowat
    integer(pntrsize)                    , pointer :: rich
    integer(pntrsize)                    , pointer :: rint
    integer(pntrsize)                    , pointer :: rintsm
    integer(pntrsize)                    , pointer :: rlabda
    integer(pntrsize)                    , pointer :: rnpl
    integer(pntrsize)                    , pointer :: rob
    integer(pntrsize)                    , pointer :: rtur0
    integer(pntrsize)                    , pointer :: rtur1
    integer(pntrsize)                    , pointer :: s0
    integer(pntrsize)                    , pointer :: s1
    integer(pntrsize)                    , pointer :: sbuu
    integer(pntrsize)                    , pointer :: sbvv
    integer(pntrsize)                    , pointer :: sig
    integer(pntrsize)                    , pointer :: sumrho
    integer(pntrsize)                    , pointer :: taubmx
    integer(pntrsize)                    , pointer :: taubpu
    integer(pntrsize)                    , pointer :: taubpv
    integer(pntrsize)                    , pointer :: taubsu
    integer(pntrsize)                    , pointer :: taubsv
    integer(pntrsize)                    , pointer :: teta
    integer(pntrsize)                    , pointer :: thick
    integer(pntrsize)                    , pointer :: tp
    integer(pntrsize)                    , pointer :: u0
    integer(pntrsize)                    , pointer :: u1
    integer(pntrsize)                    , pointer :: ubrlsu
    integer(pntrsize)                    , pointer :: ubrlsv
    integer(pntrsize)                    , pointer :: umdis
    integer(pntrsize)                    , pointer :: umean
    integer(pntrsize)                    , pointer :: umnflc
    integer(pntrsize)                    , pointer :: umnldf
    integer(pntrsize)                    , pointer :: uorb
    integer(pntrsize)                    , pointer :: v0
    integer(pntrsize)                    , pointer :: v1
    integer(pntrsize)                    , pointer :: vicuv
    integer(pntrsize)                    , pointer :: vmdis
    integer(pntrsize)                    , pointer :: vmean
    integer(pntrsize)                    , pointer :: vmnflc
    integer(pntrsize)                    , pointer :: vmnldf
    integer(pntrsize)                    , pointer :: volum0
    integer(pntrsize)                    , pointer :: volum1
    integer(pntrsize)                    , pointer :: vortic
    integer(pntrsize)                    , pointer :: w1
    integer(pntrsize)                    , pointer :: w10mag
    integer(pntrsize)                    , pointer :: windsu
    integer(pntrsize)                    , pointer :: windsv
    integer(pntrsize)                    , pointer :: windcd
    integer(pntrsize)                    , pointer :: windu
    integer(pntrsize)                    , pointer :: windv
    integer(pntrsize)                    , pointer :: ws
    integer(pntrsize)                    , pointer :: wsu
    integer(pntrsize)                    , pointer :: wsv
    integer(pntrsize)                    , pointer :: xcor
    integer(pntrsize)                    , pointer :: xydro
    integer(pntrsize)                    , pointer :: xyzsrc
    integer(pntrsize)                    , pointer :: xz
    integer(pntrsize)                    , pointer :: ycor
    integer(pntrsize)                    , pointer :: yz
    integer(pntrsize)                    , pointer :: z0ucur
    integer(pntrsize)                    , pointer :: z0vcur
    integer(pntrsize)                    , pointer :: z0urou
    integer(pntrsize)                    , pointer :: z0vrou
    integer(pntrsize)                    , pointer :: zstep
    integer(pntrsize)                    , pointer :: drhodx
    integer(pntrsize)                    , pointer :: drhody
    integer(pntrsize)                    , pointer :: dzs0
    integer(pntrsize)                    , pointer :: dzs1
    integer(pntrsize)                    , pointer :: dzu0
    integer(pntrsize)                    , pointer :: dzu1
    integer(pntrsize)                    , pointer :: dzv0
    integer(pntrsize)                    , pointer :: dzv1
    integer(pntrsize)                    , pointer :: res
    integer(pntrsize)                    , pointer :: fact
    integer(pntrsize)                    , pointer :: rl
    integer(pntrsize)                    , pointer :: xj
    integer(pntrsize)                    , pointer :: p1
    integer(pntrsize)                    , pointer :: p0
    integer(pntrsize)                    , pointer :: w0
    integer(pntrsize)                    , pointer :: s00
    integer(pntrsize)                    , pointer :: guz
    integer(pntrsize)                    , pointer :: gvz
    integer(pntrsize)                    , pointer :: gud
    integer(pntrsize)                    , pointer :: gvd
    integer(pntrsize)                    , pointer :: gsqiu
    integer(pntrsize)                    , pointer :: gsqiv
    integer(pntrsize)                    , pointer :: itbcc
    integer(pntrsize)                    , pointer :: itbct
    integer(pntrsize)                    , pointer :: itdis
    integer(pntrsize)                    , pointer :: itdro
    integer(pntrsize)                    , pointer :: kcs
    integer(pntrsize)                    , pointer :: kcu
    integer(pntrsize)                    , pointer :: kcv
    integer(pntrsize)                    , pointer :: kfs
    integer(pntrsize)                    , pointer :: kfu
    integer(pntrsize)                    , pointer :: kfv
    integer(pntrsize)                    , pointer :: kspu
    integer(pntrsize)                    , pointer :: kspv
    integer(pntrsize)                    , pointer :: mnbnd 
    integer(pntrsize)                    , pointer :: mndro
    integer(pntrsize)                    , pointer :: mnksrc
    integer(pntrsize)                    , pointer :: kcshyd
    integer(pntrsize)                    , pointer :: kfumin
    integer(pntrsize)                    , pointer :: kfvmin
    integer(pntrsize)                    , pointer :: kfsmin
    integer(pntrsize)                    , pointer :: kfumax
    integer(pntrsize)                    , pointer :: kfvmax
    integer(pntrsize)                    , pointer :: kfsmax
    integer(pntrsize)                    , pointer :: kfumx0
    integer(pntrsize)                    , pointer :: kfvmx0
    integer(pntrsize)                    , pointer :: kfsmx0
    integer(pntrsize)                    , pointer :: kfumn0
    integer(pntrsize)                    , pointer :: kfvmn0
    integer(pntrsize)                    , pointer :: kfsmn0
    integer(pntrsize)                    , pointer :: kfsz0
    integer(pntrsize)                    , pointer :: kfsz1
    integer(pntrsize)                    , pointer :: kfuz0
    integer(pntrsize)                    , pointer :: kfuz1
    integer(pntrsize)                    , pointer :: kfvz0
    integer(pntrsize)                    , pointer :: kfvz1
    integer(pntrsize)                    , pointer :: kcscut
    integer(pntrsize)                    , pointer :: kcu45
    integer(pntrsize)                    , pointer :: kcv45
    integer(pntrsize)                    , pointer :: nob
    integer(pntrsize)                    , pointer :: disint
    integer(pntrsize)                    , pointer :: dismmt
    integer(pntrsize)                    , pointer :: nambnd
    integer(pntrsize)                    , pointer :: namcon
    integer(pntrsize)                    , pointer :: namdro
    integer(pntrsize)                    , pointer :: namsrc
    integer(pntrsize)                    , pointer :: tprofc
    integer(pntrsize)                    , pointer :: tprofu
    integer(pntrsize)                    , pointer :: typbnd
    logical                              , pointer :: flcut
    logical                              , pointer :: fl45
    logical                              , pointer :: flbct
    logical                              , pointer :: flbcc
    logical                              , pointer :: fldis
    include 'tri-dyn.igd'
    real(fp)      , dimension(:)         , pointer :: rhosol
!
! Global variables
!
    integer              :: ktemp       !  Description and declaration in tricom.igs
    integer              :: lturi       !  Description and declaration in tricom.igs
    integer              :: lundia      !  Description and declaration in inout.igs
    logical              :: cyclic      !!  Flag = TRUE if cyclic system assumed
    logical              :: error       !!  Flag=TRUE if an erroris encountered
    logical              :: sferic      !  Description and declaration in tricom.igs
    real(fp)             :: grdang      !  Description and declaration in tricom.igs
    real(fp)             :: saleqs      !  Description and declaration in tricom.igs
    real(fp)             :: temeqs      !  Description and declaration in tricom.igs
    real(fp), intent(in) :: timhr       !  Current timestep (in hours) TIMNOW * 2 * HDT / 3600.
    character(*)         :: runid
    character(1)         :: evaint      !  Description and declaration in tricom.igs
    character(1)         :: temint      !  Description and declaration in tricom.igs
    character(4)         :: rouflo      !  Description and declaration in esm_alloc_char.f90
    character(4)         :: rouwav      !  Description and declaration in tricom.igs
    character(8)         :: dischy      !  Description and declaration in tricom.igs
!
! Local variables
!
    integer                            :: icx
    integer                            :: icy
    integer                            :: ierror
    integer                            :: itype
    integer                            :: nmaxddb
    integer                            :: nst          ! Current time step counter
    integer                            :: ntoftoq      ! Number of open boundary sections before the time series type:
                                                       ! ntoftoq = ntof + ntoq
    integer                            :: ifirst_dens  ! Flag to initialize the water density array
    integer, dimension(:), allocatable :: kcucopy
    integer, dimension(:), allocatable :: kcvcopy
    real(fp)                           :: timnow       ! Current timestep (multiples of dt)
    real(fp), dimension(1)             :: value
    logical                            :: success
    character(8)                       :: stage        ! First or second half time step
                                                       ! Stage = 'both' means that in F0ISF1 the layering administration
                                                       ! is copied for both the U- and the V-direction
!
!! executable statements -------------------------------------------------------
!
    wrka1               => gdp%gdaddress%wrka1
    wrka2               => gdp%gdaddress%wrka2
    wrka3               => gdp%gdaddress%wrka3
    wrkb1               => gdp%gdaddress%wrkb1
    wrkb2               => gdp%gdaddress%wrkb2
    wrkb3               => gdp%gdaddress%wrkb3
    wrkb4               => gdp%gdaddress%wrkb4
    zwork               => gdp%gdaddress%zwork
    nmax                => gdp%d%nmax
    mmax                => gdp%d%mmax
    nlb                 => gdp%d%nlb
    nub                 => gdp%d%nub
    mlb                 => gdp%d%mlb
    mub                 => gdp%d%mub
    nmlb                => gdp%d%nmlb
    nmub                => gdp%d%nmub
    ddbound             => gdp%d%ddbound
    nmaxus              => gdp%d%nmaxus
    kmax                => gdp%d%kmax
    nmaxd               => gdp%d%nmaxd
    jstart              => gdp%d%jstart
    nmmaxj              => gdp%d%nmmaxj
    nmmax               => gdp%d%nmmax
    lsts                => gdp%d%lsts
    lstsc               => gdp%d%lstsc
    lstsci              => gdp%d%lstsci
    lsal                => gdp%d%lsal
    lsed                => gdp%d%lsed
    lsedtot             => gdp%d%lsedtot
    ltem                => gdp%d%ltem
    lsecfl              => gdp%d%lsecfl
    lsec                => gdp%d%lsec
    ltur                => gdp%d%ltur
    nrob                => gdp%d%nrob 
    nto                 => gdp%d%nto
    ntof                => gdp%d%ntof
    ntoq                => gdp%d%ntoq
    ntot                => gdp%d%ntot
    kc                  => gdp%d%kc
    kcd                 => gdp%d%kcd
    nsrc                => gdp%d%nsrc
    nsrcd               => gdp%d%nsrcd
    ndro                => gdp%d%ndro
    upwsrc              => gdp%d%upwsrc
    itstrt              => gdp%gdinttim%itstrt
    itfinish            => gdp%gdinttim%itfinish
    itdrof              => gdp%gdinttim%itdrof
    itdroi              => gdp%gdinttim%itdroi
    itdrol              => gdp%gdinttim%itdrol
    julday              => gdp%gdinttim%julday
    cp                  => gdp%gdheat%cp
    gapres              => gdp%gdheat%gapres
    rhum                => gdp%gdheat%rhum
    tair                => gdp%gdheat%tair
    evapor              => gdp%gdheat%evapor
    precipt             => gdp%gdheat%precipt
    rhumarr             => gdp%gdheat%rhumarr
    tairarr             => gdp%gdheat%tairarr
    clouarr             => gdp%gdheat%clouarr
    swrfarr             => gdp%gdheat%swrfarr
    secchi              => gdp%gdheat%secchi
    rhum_file           => gdp%gdheat%rhum_file
    tair_file           => gdp%gdheat%tair_file
    clou_file           => gdp%gdheat%clou_file
    prcp_file           => gdp%gdheat%prcp_file
    swrf_file           => gdp%gdheat%swrf_file
    scc_file            => gdp%gdheat%scc_file
    morfac              => gdp%gdmorpar%morfac
    morfacpar           => gdp%gdmorpar%morfacpar
    morfacrec           => gdp%gdmorpar%morfacrec
    morfactable         => gdp%gdmorpar%morfactable
    morfacfile          => gdp%gdmorpar%morfacfile
    densin              => gdp%gdmorpar%densin
    varyingmorfac       => gdp%gdmorpar%varyingmorfac
    nh_level            => gdp%gdnonhyd%nh_level
    rhow                => gdp%gdphysco%rhow
    ag                  => gdp%gdphysco%ag
    z0                  => gdp%gdphysco%z0
    z0v                 => gdp%gdphysco%z0v
    iro                 => gdp%gdphysco%iro
    irov                => gdp%gdphysco%irov
    wind                => gdp%gdprocs%wind
    temp                => gdp%gdprocs%temp
    const               => gdp%gdprocs%const
    drogue              => gdp%gdprocs%drogue
    wave                => gdp%gdprocs%wave
    struct              => gdp%gdprocs%struct
    cdwstruct           => gdp%gdprocs%cdwstruct
    sedim               => gdp%gdprocs%sedim
    htur2d              => gdp%gdprocs%htur2d
    zmodel              => gdp%gdprocs%zmodel
    nonhyd              => gdp%gdprocs%nonhyd
    roller              => gdp%gdprocs%roller
    lftrto              => gdp%gdprocs%lftrto
    veg3d               => gdp%gdprocs%veg3d
    bubble              => gdp%gdprocs%bubble
    lfsdu               => gdp%gdprocs%lfsdu
    alfas               => gdp%gdr_i_ch%alfas
    areau               => gdp%gdr_i_ch%areau
    areav               => gdp%gdr_i_ch%areav
    bruvai              => gdp%gdr_i_ch%bruvai
    c                   => gdp%gdr_i_ch%c
    cdwlsu              => gdp%gdr_i_ch%cdwlsu
    cdwlsv              => gdp%gdr_i_ch%cdwlsv
    cdwzbu              => gdp%gdr_i_ch%cdwzbu
    cdwzbv              => gdp%gdr_i_ch%cdwzbv
    cdwztu              => gdp%gdr_i_ch%cdwztu
    cdwztv              => gdp%gdr_i_ch%cdwztv
    cfurou              => gdp%gdr_i_ch%cfurou
    cfvrou              => gdp%gdr_i_ch%cfvrou
    cvalu0              => gdp%gdr_i_ch%cvalu0
    cvalv0              => gdp%gdr_i_ch%cvalv0
    dddeta              => gdp%gdr_i_ch%dddeta
    dddksi              => gdp%gdr_i_ch%dddksi
    disch0              => gdp%gdr_i_ch%disch0
    disch1              => gdp%gdr_i_ch%disch1
    deltau              => gdp%gdr_i_ch%deltau
    deltav              => gdp%gdr_i_ch%deltav
    dfu                 => gdp%gdr_i_ch%dfu
    dfv                 => gdp%gdr_i_ch%dfv
    diapl               => gdp%gdr_i_ch%diapl
    dicuv               => gdp%gdr_i_ch%dicuv
    dis                 => gdp%gdr_i_ch%dis
    df                  => gdp%gdr_i_ch%df
    disch               => gdp%gdr_i_ch%disch
    discum              => gdp%gdr_i_ch%discum
    dp                  => gdp%gdr_i_ch%dp
    dps                 => gdp%gdr_i_ch%dps
    dpu                 => gdp%gdr_i_ch%dpu
    dpv                 => gdp%gdr_i_ch%dpv
    rint0               => gdp%gdr_i_ch%rint0
    rint1               => gdp%gdr_i_ch%rint1
    sink                => gdp%gdr_i_ch%sink
    sour                => gdp%gdr_i_ch%sour
    umdis0              => gdp%gdr_i_ch%umdis0
    umdis1              => gdp%gdr_i_ch%umdis1
    vmdis0              => gdp%gdr_i_ch%vmdis0
    vmdis1              => gdp%gdr_i_ch%vmdis1
    dxydro              => gdp%gdr_i_ch%dxydro
    dzdeta              => gdp%gdr_i_ch%dzdeta
    dzdksi              => gdp%gdr_i_ch%dzdksi
    enstro              => gdp%gdr_i_ch%enstro
    entr                => gdp%gdr_i_ch%entr
    eroll0              => gdp%gdr_i_ch%eroll0
    eroll1              => gdp%gdr_i_ch%eroll1
    evap                => gdp%gdr_i_ch%evap
    ewabr0              => gdp%gdr_i_ch%ewabr0
    ewabr1              => gdp%gdr_i_ch%ewabr1
    ewave0              => gdp%gdr_i_ch%ewave0
    ewave1              => gdp%gdr_i_ch%ewave1
    grmasu              => gdp%gdr_i_ch%grmasu
    grmasv              => gdp%gdr_i_ch%grmasv
    grmsur              => gdp%gdr_i_ch%grmsur
    grmsvr              => gdp%gdr_i_ch%grmsvr
    grfacu              => gdp%gdr_i_ch%grfacu
    grfacv              => gdp%gdr_i_ch%grfacv
    gsqs                => gdp%gdr_i_ch%gsqs
    guu                 => gdp%gdr_i_ch%guu
    guv                 => gdp%gdr_i_ch%guv
    gvu                 => gdp%gdr_i_ch%gvu
    gvv                 => gdp%gdr_i_ch%gvv
    hkru                => gdp%gdr_i_ch%hkru
    hkrv                => gdp%gdr_i_ch%hkrv
    hrms                => gdp%gdr_i_ch%hrms
    hu                  => gdp%gdr_i_ch%hu
    hu0                 => gdp%gdr_i_ch%hu0
    hv                  => gdp%gdr_i_ch%hv
    hv0                 => gdp%gdr_i_ch%hv0
    hydrbc              => gdp%gdr_i_ch%hydrbc
    patm                => gdp%gdr_i_ch%patm
    porosu              => gdp%gdr_i_ch%porosu
    porosv              => gdp%gdr_i_ch%porosv
    precip              => gdp%gdr_i_ch%precip
    procbc              => gdp%gdr_i_ch%procbc
    qu                  => gdp%gdr_i_ch%qu
    qv                  => gdp%gdr_i_ch%qv
    qxk                 => gdp%gdr_i_ch%qxk
    qyk                 => gdp%gdr_i_ch%qyk
    qzk                 => gdp%gdr_i_ch%qzk
    r0                  => gdp%gdr_i_ch%r0
    r1                  => gdp%gdr_i_ch%r1
    rho                 => gdp%gdr_i_ch%rho
    rhowat              => gdp%gdr_i_ch%rhowat
    rich                => gdp%gdr_i_ch%rich
    rint                => gdp%gdr_i_ch%rint
    rintsm              => gdp%gdr_i_ch%rintsm
    rlabda              => gdp%gdr_i_ch%rlabda
    rnpl                => gdp%gdr_i_ch%rnpl
    rob                 => gdp%gdr_i_ch%rob
    rtur0               => gdp%gdr_i_ch%rtur0
    rtur1               => gdp%gdr_i_ch%rtur1
    s0                  => gdp%gdr_i_ch%s0
    s1                  => gdp%gdr_i_ch%s1
    sbuu                => gdp%gdr_i_ch%sbuu
    sbvv                => gdp%gdr_i_ch%sbvv
    sig                 => gdp%gdr_i_ch%sig
    sumrho              => gdp%gdr_i_ch%sumrho
    taubmx              => gdp%gdr_i_ch%taubmx
    taubpu              => gdp%gdr_i_ch%taubpu
    taubpv              => gdp%gdr_i_ch%taubpv
    taubsu              => gdp%gdr_i_ch%taubsu
    taubsv              => gdp%gdr_i_ch%taubsv
    teta                => gdp%gdr_i_ch%teta
    thick               => gdp%gdr_i_ch%thick
    tp                  => gdp%gdr_i_ch%tp
    u0                  => gdp%gdr_i_ch%u0
    u1                  => gdp%gdr_i_ch%u1
    ubrlsu              => gdp%gdr_i_ch%ubrlsu
    ubrlsv              => gdp%gdr_i_ch%ubrlsv
    umdis               => gdp%gdr_i_ch%umdis
    umean               => gdp%gdr_i_ch%umean
    umnflc              => gdp%gdr_i_ch%umnflc
    umnldf              => gdp%gdr_i_ch%umnldf
    uorb                => gdp%gdr_i_ch%uorb
    v0                  => gdp%gdr_i_ch%v0
    v1                  => gdp%gdr_i_ch%v1
    vicuv               => gdp%gdr_i_ch%vicuv
    vmdis               => gdp%gdr_i_ch%vmdis
    vmean               => gdp%gdr_i_ch%vmean
    vmnflc              => gdp%gdr_i_ch%vmnflc
    vmnldf              => gdp%gdr_i_ch%vmnldf
    volum0              => gdp%gdr_i_ch%volum0
    volum1              => gdp%gdr_i_ch%volum1
    vortic              => gdp%gdr_i_ch%vortic
    w1                  => gdp%gdr_i_ch%w1
    w10mag              => gdp%gdr_i_ch%w10mag
    windsu              => gdp%gdr_i_ch%windsu
    windsv              => gdp%gdr_i_ch%windsv
    windcd              => gdp%gdr_i_ch%windcd
    windu               => gdp%gdr_i_ch%windu
    windv               => gdp%gdr_i_ch%windv
    ws                  => gdp%gdr_i_ch%ws
    wsu                 => gdp%gdr_i_ch%wsu
    wsv                 => gdp%gdr_i_ch%wsv
    xcor                => gdp%gdr_i_ch%xcor
    xydro               => gdp%gdr_i_ch%xydro
    xyzsrc              => gdp%gdr_i_ch%xyzsrc
    xz                  => gdp%gdr_i_ch%xz
    ycor                => gdp%gdr_i_ch%ycor
    yz                  => gdp%gdr_i_ch%yz
    z0ucur              => gdp%gdr_i_ch%z0ucur
    z0vcur              => gdp%gdr_i_ch%z0vcur
    z0urou              => gdp%gdr_i_ch%z0urou
    z0vrou              => gdp%gdr_i_ch%z0vrou
    zstep               => gdp%gdr_i_ch%zstep
    drhodx              => gdp%gdr_i_ch%drhodx
    drhody              => gdp%gdr_i_ch%drhody
    dzs0                => gdp%gdr_i_ch%dzs0
    dzs1                => gdp%gdr_i_ch%dzs1
    dzu0                => gdp%gdr_i_ch%dzu0
    dzu1                => gdp%gdr_i_ch%dzu1
    dzv0                => gdp%gdr_i_ch%dzv0
    dzv1                => gdp%gdr_i_ch%dzv1
    res                 => gdp%gdr_i_ch%res
    fact                => gdp%gdr_i_ch%fact
    rl                  => gdp%gdr_i_ch%rl
    xj                  => gdp%gdr_i_ch%xj
    p1                  => gdp%gdr_i_ch%p1
    p0                  => gdp%gdr_i_ch%p0
    w0                  => gdp%gdr_i_ch%w0
    s00                 => gdp%gdr_i_ch%s00
    guz                 => gdp%gdr_i_ch%guz
    gvz                 => gdp%gdr_i_ch%gvz
    gud                 => gdp%gdr_i_ch%gud
    gvd                 => gdp%gdr_i_ch%gvd
    gsqiu               => gdp%gdr_i_ch%gsqiu
    gsqiv               => gdp%gdr_i_ch%gsqiv
    itbcc               => gdp%gdr_i_ch%itbcc
    itbct               => gdp%gdr_i_ch%itbct
    itdis               => gdp%gdr_i_ch%itdis
    itdro               => gdp%gdr_i_ch%itdro
    kcs                 => gdp%gdr_i_ch%kcs
    kcu                 => gdp%gdr_i_ch%kcu
    kcv                 => gdp%gdr_i_ch%kcv
    kfs                 => gdp%gdr_i_ch%kfs
    kfu                 => gdp%gdr_i_ch%kfu
    kfv                 => gdp%gdr_i_ch%kfv
    kspu                => gdp%gdr_i_ch%kspu
    kspv                => gdp%gdr_i_ch%kspv
    mnbnd               => gdp%gdr_i_ch%mnbnd 
    mndro               => gdp%gdr_i_ch%mndro
    mnksrc              => gdp%gdr_i_ch%mnksrc
    kcshyd              => gdp%gdr_i_ch%kcshyd
    kfumin              => gdp%gdr_i_ch%kfumin
    kfvmin              => gdp%gdr_i_ch%kfvmin
    kfsmin              => gdp%gdr_i_ch%kfsmin
    kfumax              => gdp%gdr_i_ch%kfumax
    kfvmax              => gdp%gdr_i_ch%kfvmax
    kfsmax              => gdp%gdr_i_ch%kfsmax
    kfumx0              => gdp%gdr_i_ch%kfumx0
    kfvmx0              => gdp%gdr_i_ch%kfvmx0
    kfsmx0              => gdp%gdr_i_ch%kfsmx0
    kfumn0              => gdp%gdr_i_ch%kfumn0
    kfvmn0              => gdp%gdr_i_ch%kfvmn0
    kfsmn0              => gdp%gdr_i_ch%kfsmn0    
    kfsz0               => gdp%gdr_i_ch%kfsz0
    kfsz1               => gdp%gdr_i_ch%kfsz1
    kfuz0               => gdp%gdr_i_ch%kfuz0
    kfuz1               => gdp%gdr_i_ch%kfuz1
    kfvz0               => gdp%gdr_i_ch%kfvz0
    kfvz1               => gdp%gdr_i_ch%kfvz1
    kcscut              => gdp%gdr_i_ch%kcscut
    kcu45               => gdp%gdr_i_ch%kcu45
    kcv45               => gdp%gdr_i_ch%kcv45
    nob                 => gdp%gdr_i_ch%nob
    disint              => gdp%gdr_i_ch%disint
    dismmt              => gdp%gdr_i_ch%dismmt
    nambnd              => gdp%gdr_i_ch%nambnd
    namcon              => gdp%gdr_i_ch%namcon
    namdro              => gdp%gdr_i_ch%namdro
    namsrc              => gdp%gdr_i_ch%namsrc
    tprofc              => gdp%gdr_i_ch%tprofc
    tprofu              => gdp%gdr_i_ch%tprofu
    typbnd              => gdp%gdr_i_ch%typbnd
    flcut               => gdp%gdtmpfil%flcut
    fl45                => gdp%gdtmpfil%fl45
    flbct               => gdp%gdtmpfil%flbct
    flbcc               => gdp%gdtmpfil%flbcc
    fldis               => gdp%gdtmpfil%fldis
    rhosol              => gdp%gdsedpar%rhosol
    !
    icx     = 0
    icy     = 0
    nmaxddb = nmax + 2*gdp%d%ddbound
    allocate(kcucopy(nmlb:nmub))
    allocate(kcvcopy(nmlb:nmub))
    call copykcuv(i(kcu), kcucopy, gdp)
    call copykcuv(i(kcv), kcvcopy, gdp)
    !
    ierror = 0
    if (nsrcd > 0) then
       call chkdis(lundia    ,error     ,nsrcd     ,zmodel    ,nmax      , &
                 & mmax      ,nmaxus    ,kmax      ,ch(namsrc),i(mnksrc) , &
                 & i(kcs)    ,r(xyzsrc) ,r(sig)    ,r(sig)    ,d(dps)    , &
                 & r(s1)     ,r(xz)     ,r(yz)     ,gdp       )
       if (error) ierror = 1
       call dfreduce_gdp( ierror, 1, dfint, dfmax, gdp )
       error = ierror==1
       if (error) goto 9999
    endif
    !
    ! CHKDRO: check drogue input if DROGUE = .true.
    ! if ITDROF > ITDROL then drogue will be reset to .false.
    !
    if (drogue) then
       call chkdro(lundia    ,itstrt    ,itfinish  ,drogue    ,itdrof    , &
                 & itdrol    ,itdroi    ,ndro      ,nmax      ,mmax      , &
                 & nmaxus    ,ch(namdro),i(mndro)  ,i(itdro)  ,i(kcs)    , &
                 & r(dxydro) ,r(xydro)  ,r(xcor)   ,r(ycor)   ,gdp       )
    endif
    !
    ! INITDD is obsolete, checking of time frame is now handled within TDATOM
    ! only initialization of TIMNOW still needed.
    !
    timnow = real(itstrt,fp)
    !
    ! call iniwnd is replaced by the meteo module
    !
    ! INIBCT: read initial arrays values for time dependent data for
    ! at open boundaries (hydrodynamic input)
    !
    if (flbct) then
       ntoftoq = ntof + ntoq
       call inibct(lundia    ,error     ,runid     , &
                 & i(itbct)  ,nto       ,ntoftoq   , &
                 & kmax      ,kcd       ,ch(nambnd),ch(typbnd),ch(tprofu), &
                 & r(hydrbc) ,bubble    ,gdp       )
       if (error) goto 9999
    endif
    !
    ! INIBCQ: read initial arrays values for QH relations at boundaries
    !
    if (ntoq /= 0) then
       call inibcq(lundia    ,error     ,runid     ,i(itbct)  ,nto       , &
                 & ntof      ,ntoq      ,kcd       ,ch(nambnd),r(hydrbc) , &
                 & bubble    ,kmax      ,gdp       )
       if (error) goto 9999
    endif
    !
    ! INIBCC: read initial arrays values for time dependent data for
    ! constituents at open boundaries
    !
    if (flbcc) then
       call inibcc(lundia    ,error     ,runid     ,timnow    , &
                 & i(itbcc)  ,itstrt    ,itfinish  ,nto       ,lstsc     , &
                 & kmax      ,ch(nambnd),ch(namcon),ch(tprofc),r(procbc) , &
                 & r(zstep)  ,bubble    ,gdp       )
       if (error) goto 9999
    endif
    call inibcparl(nto    ,nrob      ,i(mnbnd)  ,i(nob)     ,ch(typbnd), &
                 & r(guu) ,r(gvv)    ,gdp       )
    !
    ! INIDIS: read initial arrays values for time dependent data for
    ! discharges
    ! subroutine parameter(10) = ICX := NMAX
    ! subroutine parameter(11) = ICY := 1
    !
    if (fldis) then
       icx = nmaxddb
       icy = 1
       call inidis(lundia    ,error     ,runid     ,cyclic    ,timnow    , &
                 & i(itdis)  ,itstrt    ,itfinish  ,sferic    ,grdang    , &
                 & nsrc      ,nsrcd     ,lstsc     ,jstart    ,nmmaxj    , &
                 & icx       ,icy       ,ch(namsrc),ch(disint),ch(dismmt), &
                 & ch(namcon),i(mnksrc) ,r(alfas)  ,r(disch)  , &
                 & r(disch0) ,r(disch1) ,r(rint)   ,r(rint0)  ,r(rint1)  , &
                 & r(umdis)  ,r(umdis0) ,r(umdis1) ,r(vmdis)  ,r(vmdis0) , &
                 & r(vmdis1) ,bubble    ,kmax      ,i(kspu)   ,i(kspv)   , &
                 & upwsrc    ,gdp       )
       if (error) goto 9999
    endif
    !
    ! The global atmospheric pressure gapres (read and/or specified in rdporc.f90)
    ! is used as default value for patm in the meteo module.
    ! The possible space varying pressure is read via incmeteo.
    ! This must be done before the call to caleva.
    !
    !
    success = setmeteodefault('patm', gapres)
    call checkmeteoresult(success, gdp)
    !
    ! INITEM: read initial arrays values for time dependent data for
    ! heat models
    ! Also when FLTEM = .false. some parameters are defined
    ! in INITEM, hence always enter
    !
    call initem(runid, cyclic, timnow, ktemp, temint, r(patm), gdp)
    !
    ! The following arrays must be filled (when relevant)
    ! before the first call to postpr.
    ! - windu, windv, patm
    ! - rhumarr, tairarr, clouarr, swrfarr, secchi
    ! - sdu_t0 (subsidence/uplift) 
    ! 
    if (wind) then
       call incmeteo(timhr     , grdang   , &
                   & r (windu ),r (windv ),r (patm  ), &
                   & i (kcs   ),r (alfas ), r(windcd), &
                   & r (windsu),r (windsv),r (w10mag), gdp)
    endif
    if (rhum_file) then
       success = getmeteoval(gdp%runid, 'relhum', timhr * 60.0_fp, &
                           & gdp%gdparall%mfg, gdp%gdparall%nfg, nlb, nub, mlb, mub, rhumarr , 0)
       call checkmeteoresult(success, gdp)
    endif
    if (tair_file) then
       success = getmeteoval(gdp%runid, 'airtemp', timhr * 60.0_fp, &
                           &gdp%gdparall%mfg, gdp%gdparall%nfg,  nlb, nub, mlb, mub, tairarr , 0)
       call checkmeteoresult(success, gdp)
    endif
    if (clou_file) then
       success = getmeteoval(gdp%runid, 'cloud', timhr * 60.0_fp, &
                           &gdp%gdparall%mfg, gdp%gdparall%nfg, nlb, nub, mlb, mub, clouarr , 0)
       call checkmeteoresult(success, gdp)
    endif
    if (prcp_file) then
       success = getmeteoval(gdp%runid, 'precipitation', timhr * 60.0_fp, &
                           &gdp%gdparall%mfg, gdp%gdparall%nfg, nlb, nub, mlb, mub, r(precip) , 0)
       call checkmeteoresult(success, gdp)
    endif
    if (swrf_file) then
       success = getmeteoval(gdp%runid, 'swrf', timhr * 60.0_fp, &
                           &gdp%gdparall%mfg, gdp%gdparall%nfg, nlb, nub, mlb, mub, swrfarr , 0)
       call checkmeteoresult(success, gdp)
    endif
    if (scc_file) then
       success = getmeteoval(gdp%runid, 'Secchi_depth', timhr * 60.0_fp, &
                           &gdp%gdparall%mfg, gdp%gdparall%nfg, nlb, nub, mlb, mub, secchi , 0)
       call checkmeteoresult(success, gdp)
    endif
    if (lfsdu) then 
       call incsdu(timhr  ,d(dps)   ,r(s1)   ,i(kcs)  ,i(kfs) ,gdp    )
    endif                  

    !
    ! INIEVA: read initial arrays values for time dependent data for
    ! rainfall / evaporation model
    ! Also when FLEVA = .false. some parameters are defined
    ! in INIEVA, hence always entre
    !
    call inieva(runid     ,cyclic    ,timnow    ,evaint    ,jstart    , &
              & nmmaxj    ,nmmax     ,r(evap)   ,r(precip) ,gdp       )
    !
    ! Input values depend on local situations (e.g. floating structures)
    ! WARNING: structures filter w.r.t. radiation is handled in HEATU
    !
    icx = nmaxddb
    icy = 1
    call filterstructures(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                        & icy       ,i(kspu)   ,i(kspv)   ,r(evap)   ,r(windsu) , &
                        & r(windsv) ,r(w10mag) ,r(uorb)   ,r(tp)     ,r(teta)   , &
                        & r(dis)    ,r(wsu)    ,r(wsv)    ,r(grmasu) ,r(grmasv) , &
                        & r(df)     ,gdp       )
    !
    ! INISED: set initial array values for sediment
    ! only initialise sediment at beginning of morsys simulation
    !
    if (sedim) then
       call inised(lundia    ,error     ,nmax      ,mmax      ,nmaxus    , &
                 & nmmax     ,lsed      ,lsedtot   ,i(kcs)    ,gdp       )
       if (error) goto 9999
    endif
    !
    ! Z_INIZM: Z-Model; set initial depth at velocity points
    ! define mask arrays for velocity points
    ! initialize QXK and QYK arrays. USE SIG array for ZK
    ! subroutine parameter(5) = ICX := NMAX
    ! subroutine parameter(6) = ICY := 1
    !
    if (zmodel) then
       icx = nmaxddb
       icy = 1
       call z_inizm(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                  & icy       ,error     ,i(kcu)    ,i(kcv)    ,i(kcs)    , &
                  & i(kfu)    ,i(kfv)    ,i(kfs)    ,i(kfsz1)  ,i(kfuz1)  , &
                  & i(kfvz1)  ,i(kfsmin) ,i(kfsmax) ,i(kfumin) ,i(kfumax) , &
                  & i(kfvmin) ,i(kfvmax) ,i(kspu)   ,i(kspv)   ,i(kcshyd) , &
                  & d(dps)    ,r(dpu)    ,r(dpv)    ,r(s1)     ,r(thick)  , &
                  & r(hu)     ,r(hv)     ,r(dzu1)   ,r(dzu0)   ,r(dzv1)   , &
                  & r(dzv0)   ,r(dzs1)   ,r(dzs0)   ,r(sig)    ,r(r1)     , &
                  & lstsci    ,r(gsqs)   ,r(qzk)    ,r(umean)  ,r(vmean)  , &
                  & gdp       )
       if (error) goto 9999
       call inicut(lundia    ,error     ,runid     ,nmax      ,mmax      , &
                 & nmaxus    ,kmax      ,flcut     ,fl45      ,i(kcu)    , &
                 & i(kcv)    ,i(kcs)    ,i(kfsmin) ,i(kfsmax) ,i(kcu45)  , &
                 & i(kcv45)  ,i(kcscut) ,r(xcor)   ,r(ycor)   ,r(gud)    , &
                 & r(guu)    ,r(guv)    ,r(guz)    ,r(gvd)    ,r(gvu)    , &
                 & r(gvv)    ,r(gvz)    ,r(gsqs)   ,r(gsqiu)  ,r(gsqiv)  , &
                 & gdp       )
    endif
    !
    ! CHKDRY: set initial depth at velocity points
    ! define mask arrays for velocity points
    ! initialize QXK and QYK arrays
    ! subroutine parameter(7) = ICX := NMAX
    ! subroutine parameter(8) = ICY := 1
    ! ONLY FOR SIGMA LAYER MODEL ! FOR Z_MODEL CALL Z_CHKDRY
    !
    if (.not.zmodel) then
       icx = nmaxddb
       icy = 1
       call chkdry(jstart    ,nmmaxj    ,nmmax     ,kmax      ,lsec      , &
                 & lsecfl    ,lstsci    ,ltur      ,icx       ,icy       , &
                 & i(kcu)    ,i(kcv)    ,i(kcs)    ,i(kfu)    , &
                 & i(kfv)    ,i(kfs)    ,i(kspu)   ,i(kspv)   ,r(dpu)    , &
                 & r(dpv)    ,r(hu)     ,r(hv)     ,r(hkru)   ,r(hkrv)   , &
                 & r(thick)  ,r(s1)     ,d(dps)    ,r(u1)     ,r(v1)     , &
                 & r(umean)  ,r(vmean)  ,r(r1)     ,r(rtur1)  ,r(guu)    , &
                 & r(gvv)    ,r(qxk)    ,r(qyk)    ,gdp       )
    else
       icx = nmaxddb
       icy = 1
       call z_chkdry(jstart    ,nmmaxj    ,nmmax     ,kmax      ,lstsci    , &
                   & ltur      ,icx       ,icy       ,i(kcu)    , &
                   & i(kcv)    ,i(kcs)    ,i(kfu)    ,i(kfv)    ,i(kfs)    , &
                   & i(kspu)   ,i(kspv)   ,i(kfuz1)  ,i(kfvz1)  ,i(kfsz1)  , &
                   & i(kfumin) ,i(kfumax) ,i(kfvmin) ,i(kfvmax) ,i(kfsmin) , &
                   & i(kfsmax) ,r(dpu)    ,r(dpv)    ,r(hu)     ,r(hv)     , &
                   & r(hkru)   ,r(hkrv)   ,r(s1)     ,d(dps)    ,r(u1)     , &
                   & r(v1)     ,r(umean)  ,r(vmean)  ,r(r1)     ,r(rtur1)  , &
                   & r(guu)    ,r(gvv)    ,r(qxk)    ,r(qyk)    ,r(dzu1)   , &
                   & r(dzv1)   ,r(dzs1)   ,r(sig)    ,gdp       )
    endif
    !
    ! Convert the coordinates of the fixed gate using DPU/DPV as reference
    ! Initialise the porosity factor POROSU/V (== 1). Initialisation
    ! of porosity may not be skipped (later initialisation maybe moved to
    ! other routines)
    !
    call inicdw(lundia    ,nmax      ,mmax      ,nmaxus    ,kmax      , &
              & i(kspu)   ,i(kspv)   ,r(dpu)    ,r(dpv)    , &
              & r(porosu) ,r(porosv) ,r(cdwztu) ,r(cdwzbu) ,r(cdwztv) , &
              & r(cdwzbv) ,gdp       )
    if (cdwstruct) then
       !
       ! Define KSPU/V and POROSU/V for CDW type of structure (fixed gate with
       ! - OPTIONALLY - layers with enhanced friction below it).
       ! Array SIG is passed on twice; the first one represents the SIGma coordinates
       ! (zmodel == .FALSE.) the second represent the Z-coordinates (zmodel == .TRUE.).
       ! This is a trick to enable CDWKAD routine to be used for both coordinate types.
       ! Work array ZWORK has the length of 5*KMAX
       !
       call cdwkad(nmmax     ,kmax      ,zmodel    ,i(kspu)   ,i(kfsmax) , &
                 & i(kfsmin) ,i(kfumax) ,i(kfumin) ,r(sig)    ,r(thick)  , &
                 & r(sig)    ,r(zwork)  ,r(zwork+kmax)  ,r(zwork+2*kmax) , &
                 & r(dpu)    ,r(hu)     ,r(dzu1)   ,r(porosu) ,r(ubrlsu) , &
                 & r(cdwztu) ,r(cdwzbu) ,r(cdwlsu) ,gdp       )
       call cdwkad(nmmax     ,kmax      ,zmodel    ,i(kspv)   ,i(kfsmax) , &
                 & i(kfsmin) ,i(kfvmax) ,i(kfvmin) ,r(sig)    ,r(thick)  , &
                 & r(sig)    ,r(zwork)  ,r(zwork+kmax)  ,r(zwork+2*kmax) , &
                 & r(dpv)    ,r(hv)     ,r(dzv1)   ,r(porosv) ,r(ubrlsv) , &
                 & r(cdwztv) ,r(cdwzbv) ,r(cdwlsv) ,gdp       )
    endif
    !
    ! Compute volumes and areas
    !
    call inivol(jstart    ,nmmaxj    ,nmmax     ,kmax      ,zmodel    , &
              & i(kcs)    ,i(kcu)    ,i(kcv)    ,i(kfsmin) ,i(kfsmax) , &
              & i(kfumin) ,i(kfumax) ,i(kfvmin) ,i(kfvmax) ,r(thick)  , &
              & r(s1)     ,d(dps)    ,r(gsqs)   ,r(guu)    ,r(gvv)    , &
              & r(hu)     ,r(hv)     ,r(dzs1)   ,r(dzu1)   ,r(dzv1)   , &
              & r(volum1) ,r(porosu) ,r(porosv) ,r(areau)  ,r(areav)  ,gdp       )
    call updmassbal(1         ,r(qxk)    ,r(qyk)    ,i(kcs)    ,r(r1)     , &
                  & r(volum0) ,r(volum1) ,r(sbuu)   ,r(sbvv)   ,r(disch)  , &
                  & i(mnksrc) ,r(sink)   ,r(sour)   ,r(gsqs)   ,r(guu)    , &
                  & r(gvv)    ,d(dps)    ,r(rintsm) ,0.0_fp    ,gdp       )
    !
    ! F0ISF1: copy old (1) in new arrays (0)
    ! N.B.:
    ! check on stability not in initialisation
    ! herefore NST = -1
    ! Note:
    ! HU0 and HV0 obtain their values for the first time in F0ISF1
    ! Call f0isf1 as such that the complete layering administration is copied: stage = 'both'
    !
    nst = -1
    !
    ! Copy both U- and V-components
    !
    stage = 'both'
    call f0isf1(stage     ,dischy    ,nst       ,zmodel    ,jstart    , &
              & nmmax     ,nmmaxj    ,nmax      ,kmax      ,lstsci    , &
              & ltur      ,nsrc      ,i(kcu)    ,i(kcv)    ,i(kcs)    , &
              & i(kfs)    ,i(kfu)    ,i(kfv)    ,i(kfsmin) ,i(kfsmax) , &
              & i(kfumin) ,i(kfumax) ,i(kfvmin) ,i(kfvmax) ,i(kfsmn0) , &
              & i(kfumn0) ,i(kfvmn0) ,i(kfsmx0) ,i(kfumx0) ,i(kfvmx0) , &
              & i(kfsz0)  ,i(kfuz0)  ,i(kfvz0)  , &
              & i(kfsz1)  ,i(kfuz1)  ,i(kfvz1)  , &
              & r(s0)     ,r(s1)     ,r(u0)     , &
              & r(u1)     ,r(v0)     ,r(v1)     ,r(volum0) ,r(volum1) , &
              & r(r0)     ,r(r1)     ,r(rtur0)  ,r(rtur1)  ,r(disch)  , &
              & r(discum) ,r(hu)     ,r(hv)     ,r(dzu1)   ,r(dzv1)   , &
              & r(dzs1)   ,r(dzu0)   ,r(dzv0)   ,r(dzs0)   ,r(qxk)    , &
              & r(qyk)    ,r(s00)    ,r(w0)     , &
              & r(w1)     ,r(p0)     ,r(p1)     ,r(hu0)    ,r(hv0)    , &
              & r(ewabr0) ,r(ewabr1) , &
              & r(ewave0) ,r(ewave1) ,r(eroll0) ,r(eroll1) ,roller    , &
              & gdp       )
    !
    ! DENS  : compute densities for the first time
    !
    ifirst_dens = 1
    call dens(jstart    ,nmmaxj    ,nmmax     ,kmax       ,lstsci    , &
            & lsal      ,ltem      ,lsed      ,i(kcs)     ,saleqs    ,temeqs    , &
            & densin    ,zmodel    ,r(thick)  ,r(r0)      ,r(rho)    , &
            & r(sumrho) ,r(rhowat) ,rhosol    ,ifirst_dens,gdp       )
    !
    ! Z_DENGRA: compute DRHODX/DRHODY terms (only in Z-MODEL)
    !
    if (zmodel) then
       icx = nmaxddb
       icy = 1
       call z_dengra(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                   & icy       ,i(kfsz1)  ,i(kfumin) ,i(kfumax) ,i(kfvmin) , &
                   & i(kfvmax) ,i(kfu)    ,i(kfv)    , &
                   & r(rho)    ,r(gvu)    ,r(guv)    ,r(drhodx) , &
                   & r(drhody) ,r(dzu1)   ,r(dzv1)   ,gdp       )
    endif
    !
    ! TRTROU: calculate rougness due to trachytopes.
    !         called for U/V-direction.
    !
    if (lftrto) then
       call trtrou(lundia    ,nmax      ,mmax      ,nmaxus    ,kmax      , &
                 & r(cfurou) ,rouflo    ,.true.    ,r(guu)    ,r(gvu)    , &
                 & r(hu)     ,i(kcu)    ,r(u1)     ,r(v1)     ,r(sig)    , &
                 & r(z0urou) ,r(deltau) ,1         ,gdp       )
       if (error) goto 9999
       call trtrou(lundia    ,nmax      ,mmax      ,nmaxus    ,kmax      , &
                 & r(cfvrou) ,rouflo    ,.true.    ,r(gvv)    ,r(guv)    , &
                 & r(hv)     ,i(kcv)    ,r(v1)     ,r(u1)     ,r(sig)    , &
                 & r(z0vrou) ,r(deltav) ,2         ,gdp       )
       if (error) goto 9999
    endif
    !
    ! INITAU: calculate inital roughness heights Z0U(V)ROU
    ! for HU and HV use work array WRKB1 for this purpose
    ! subroutine parameter(5) = ICX := NMAX and := 1    (second call)
    !
    icx = nmaxddb
    icy = 1
    call initau(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
              & rouflo    ,zmodel    , &
              & i(kcs)    ,i(kcu)    ,i(kfu)    ,i(kspu)   , &
              & r(s1)     ,r(dpu)    ,r(umean)  ,r(wrkb1)  ,d(dps)    , &
              & r(cfurou) ,r(z0urou) ,gdp       )
    icx = 1
    icy = nmaxddb
    call initau(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
              & rouflo    ,zmodel    , &
              & i(kcs)    ,i(kcv)    ,i(kfv)    ,i(kspv)   , &
              & r(s1)     ,r(dpv)    ,r(vmean)  ,r(wrkb2)  ,d(dps)    , &
              & r(cfvrou) ,r(z0vrou) ,gdp       )
    !
    ! EULER: calculate adjusted velocities for mass flux
    ! NOTE: Array SIG has a different meaning (ZK) in case
    ! of ZMODEL
    !
    icx = nmaxddb
    icy = 1
    call euler(jstart    ,nmmax     ,nmmaxj    ,kmax      ,icx       , &
             & i(kcu)    ,i(kcv)    ,i(kfu)    ,i(kfv)    ,i(kfumax) , &
             & i(kfumin) ,i(kfvmax) ,i(kfvmin) ,r(dzu1)   ,r(dzv1)   , &
             & r(u1)     ,r(wrkb3)  ,r(v1)     ,r(wrkb4)  , &
             & r(grmasu) ,r(grmasv) ,r(hu)     ,r(hv)     , &
             & r(tp)     ,r(hrms)   ,r(sig)    ,r(thick)  ,r(teta)   , &
             & r(grmsur) ,r(grmsvr) ,r(grfacu) ,r(grfacv) ,gdp       )
    !
    ! TAUBOT: calculate bottom stress coefficients
    ! to calculate tau_bottom values using local values
    ! For HU and HV use work array WRKB1 for this purpose
    ! For adjusted velocities use work array WRKB3 (U1) and
    ! WRKB4 (V1)
    ! For calculation of TAUBMX use work array WRKA1 resp.
    ! WRKA2
    ! For Chezy coeff. use work array WRKA4 resp. WRKA5 (used
    ! in DETVIC)
    ! subroutine parameter(5) = ICX := NMAX and := 1    (second call)
    ! subroutine parameter(6) = ICY := 1    and := NMAX (second call)
    !
    ! TAUBOT is called here with kcu/v instead of kfu/v, to ensure that
    ! also the temporary dry points contain a relevant cfurou(nm,1) value.
    ! These values are used when the point becomes wet.
    ! kcu/v is used inside TAUBOT as weight factor to calculate v(u)
    ! in u(v) points. Therefore, kcu/v should not contain the value
    ! 2 (open boundary) or 3 (dd boundary). That's why the (cleaned)
    ! copies of kcu/v are used.
    !
    icx = nmaxddb
    icy = 1
    if (.not. zmodel) then
       call upwhu(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                & zmodel    ,i(kcs)    ,i(kcu)    ,i(kspu)   ,d(dps)    , &
                & r(s1)     ,r(dpu)    ,r(umean)  ,r(wrkb1)  ,gdp       )
    endif
    call taubot(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
              & icy       ,rouflo    ,rouwav    ,kcucopy   ,kcvcopy   , &
              & i(kfumin) ,i(kfumax) ,i(kspu)   ,i(kcs)    ,i(kcscut) , &
              & d(dps)    ,r(s1)     ,r(wrkb3)  ,r(wrkb4)  , &
              & r(guu)    ,r(xcor)   ,r(ycor)   ,r(rho)    , &
              & r(taubpu) ,r(taubsu) ,r(wrka1)  ,r(dis)    ,r(rlabda) , &
              & r(teta)   ,r(uorb)   ,r(tp)     ,r(wsu)    ,r(wsv)    , &
              & r(grmasu) ,r(dfu)    ,r(deltau) ,r(hrms)   , &
              & r(cfurou) ,r(z0urou) ,r(wrkb1)  ,r(dzu1)   ,r(sig)    , &
              & r(z0ucur) ,r(cvalu0) ,r(grmsur) ,r(grfacu) ,gdp       )
    icx = 1
    icy = nmaxddb
    if (.not. zmodel) then
       call upwhu(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                & zmodel    ,i(kcs)    ,i(kcv)    ,i(kspv)   ,d(dps)    , &
                & r(s1)     ,r(dpv)    ,r(vmean)  ,r(wrkb2)  ,gdp       )
    endif
    call taubot(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
              & icy       ,rouflo    ,rouwav    ,kcvcopy   ,kcucopy   , &
              & i(kfvmin) ,i(kfvmax) ,i(kspv)   ,i(kcs)    ,i(kcscut) , &
              & d(dps)    ,r(s1)     ,r(wrkb4)  ,r(wrkb3)  , &
              & r(gvv)    ,r(ycor)   ,r(xcor)   ,r(rho)    , &
              & r(taubpv) ,r(taubsv) ,r(wrka2)  ,r(dis)    ,r(rlabda) , &
              & r(teta)   ,r(uorb)   ,r(tp)     ,r(wsv)    ,r(wsu)    , &
              & r(grmasv) ,r(dfv)    ,r(deltav) ,r(hrms)   , &
              & r(cfvrou) ,r(z0vrou) ,r(wrkb2)  ,r(dzv1)   ,r(sig)    , &
              & r(z0vcur) ,r(cvalv0) ,r(grmsvr) ,r(grfacv) ,gdp       )
    icx = nmaxddb
    icy = 1
    call caltmx(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
              & icy       ,zmodel    ,i(kfu)    ,i(kfv)    ,i(kfs)    , &
              & i(kfuz1)  ,i(kfvz1)  ,i(kfsmin) ,r(wrka1)  ,r(wrka2)  , &
              & r(taubmx) ,r(hu)     ,r(hv)     ,d(dps)    ,r(s1)     , &
              & gdp       )
    if (htur2d .or. irov>0 .or. zmodel) then
       !
       ! Check horizontal Eddy Viscosity and Diffusivity
       !
       itype = 1
       call chkvic(lundia    ,jstart    ,nmmaxj    ,nmmax     ,kmax      , &
                 & icx       ,icy       ,timnow    ,i(kfs)    ,i(kfu)    , &
                 & i(kfv)    ,i(kcs)    ,lstsci    ,r(guv)    ,r(gvu)    , &
                 & r(vicuv)  ,r(dicuv)  ,itype     ,i(kfsmin) ,i(kfsmax) , &
                 & gdp       )
    endif
    if (htur2d) then
       !
       ! HLES/Smagorinsky with bottom friction
       ! Calculate fluctuating velocity components using lp filter
       !
       call lpfluc(jstart    ,nmmaxj    ,nmmax     ,i(kfu)    ,i(kfv)    , &
                 & r(umean)  ,r(vmean)  ,r(umnldf) ,r(vmnldf) ,r(umnflc) , &
                 & r(vmnflc) ,gdp       )
       !
       ! Calculate Turbulent Kinetic Energy production due to velocity
       ! fluctuation
       ! wrka3 is used to store the result (S2) to be used in DETVIC
       !
       icx = nmaxddb
       icy = 1
       call protke(jstart    ,nmmaxj    ,nmmax     ,icx       ,icy       , &
                 & i(kfs)    ,i(kfu)    ,i(kfv)    ,i(kcs)    ,r(umnflc) , &
                 & r(vmnflc) ,r(guu)    ,r(gvv)    ,r(wrka1)  ,r(wrka2)  , &
                 & r(wrka3)  ,gdp       )
       !
       ! Calculate subgridscale eddy viscosity/diffusivity
       ! CVALU0 and CVALV0 contain actual 2D-chezy values
       ! WRKA3 contains TKE production (S2)
       ! result is put in vicuv/dicuv in layer kmax+2
       !
       icx = nmaxddb
       icy = 1
       call detvic(lundia    ,jstart    ,nmmaxj    ,nmmax     ,kmax      , &
                 & icx       ,icy       ,i(kfs)    ,i(kfu)    , &
                 & i(kfv)    ,i(kcs)    ,d(dps)    ,r(s1)     ,r(umean)  , &
                 & r(vmean)  ,r(cvalu0) ,r(cvalv0) ,r(guv)    ,r(gvu)    , &
                 & r(gsqs)   ,r(wrka3)  ,r(vicuv)  ,r(dicuv)  , &
                 & gdp       )
    endif
    !
    ! To avoid problems with GPP, arrays VORTIC and ENSTRO are always
    ! computed and stored in HIS and MAP files even when HLES is not
    ! activated. These arrays were meant for post-processing only
    !
    call c_vort(mmax      ,nmax      ,kmax      ,nmaxus    ,i(kcs)    ,i(kfu)    , &
              & i(kfv)    ,r(u1)     ,r(v1)     ,r(gud)    ,r(gvd)    , &
              & r(vortic) ,r(enstro) ,r(wrkb1)  ,gdp       )
    !
    ! INITUR: calculate initial turbulent energy and/or turbulent
    ! dissipation depending on the value of lturi
    ! subroutine parameter(5) = ICX := NMAX
    ! subroutine parameter(6) = ICY := 1
    !
    if (lturi /= 0) then
       if (.not.zmodel) then
          icx = nmaxddb
          icy = 1
          !
          ! If in trisol f0isf1 is called at the beginning of each half timestep:
          ! Call initur with argument rtur1 (f0isf1 will copy this to rtur0)
          !
          ! If in trisol f0isf1 is called at the end of each half timestep:
          ! Call initur with argument rtur0
          !
          call initur(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                    & icy       ,ltur      ,lturi     ,r(rtur1)  , &
                    & r(s1)     ,d(dps)    ,r(hu)     ,r(hv)     ,r(u1)     , &
                    & r(v1)     ,r(thick)  ,r(windsu) ,r(windsv) ,r(z0urou) , &
                    & r(z0vrou) ,i(kfu)    ,i(kfv)    ,i(kfs)    ,i(kcs)    , &
                    & r(wrkb1)  ,r(wrkb2)  ,r(bruvai) ,r(rich)   ,r(rho)    , &
                    & gdp       )
       else
          icx = nmaxddb
          icy = 1
          call z_initur(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                      & icy       ,ltur      ,lturi     ,i(kfu)    ,i(kfv)    , &
                      & i(kfs)    ,i(kcs)    ,i(kfumin) ,i(kfumax) ,i(kfvmin) , &
                      & i(kfvmax) ,i(kfsmin) ,i(kfsmax) ,r(rtur1)  , &
                      & r(s1)     ,d(dps)    ,r(u1)     ,r(v1)     ,r(windsu) , &
                      & r(windsv) ,r(z0urou) ,r(z0vrou) ,r(wrkb1)  ,r(wrkb2)  , &
                      & r(dzu1)   ,r(dzv1)   ,r(dzs1)   ,r(bruvai) ,r(rich)   , &
                      & r(rho)    ,gdp       )
       endif
    endif
    !
    ! DERSIG: computes transformation coefficients for the sigma trans-
    ! formation: DZDKSI, DZDETA, DDDKSI, DDDETA
    ! subroutine parameter(4) = ICX := NMAX
    ! subroutine parameter(5) = ICY := 1
    !
    if (.not.zmodel) then
       icx = nmaxddb
       icy = 1
       call dersig(jstart    ,nmmaxj    ,nmmax     ,icx       ,icy       , &
                 & i(kfu)    ,i(kfv)    ,r(dp)     ,r(s1)     ,r(dddksi) , &
                 & r(dddeta) ,r(dzdksi) ,r(dzdeta) ,gdp       )
    endif
    !
    ! (Rigid) 3D Vegetation Model
    !
    if (veg3d) then
       call updveg3d(mmax      ,nmax      ,kmax      ,r(sig)    ,r(thick)  , &
                   & d(dps)    ,i(kfs)    ,r(s0)     ,r(u1)     ,r(v1)     , &
                   & r(diapl)  ,r(rnpl)   ,gdp       )
    endif
    if (varyingmorfac) then
       !
       ! Varying MorFac
       ! First update of MorFac must be done before the first call to postpr
       !
       call flw_gettabledata(morfacfile ,morfactable,            &
                           & morfacpar  , 1         , morfacrec, &
                           & value(1:1) , timhr     , julday,    gdp )
       morfac = value(1)
    endif
 9999 continue
    deallocate(kcucopy)
    deallocate(kcvcopy)
end subroutine inchkr
