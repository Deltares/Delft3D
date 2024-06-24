subroutine readmd(lunmd     ,lundia    ,lunscr    ,error     ,runid     ,runtxt    , &
                & filrgf    ,dx        ,dy        ,sferic    ,anglat    ,anglon    , &
                & grdang    ,tgfcmp    ,roumet    ,rouwav    ,temeqs    ,saleqs    , &
                & betac     ,dml       ,restid    ,icreep    ,trasol    ,forfuv    , &
                & forfww    ,ktemp     ,keva      ,temint    ,evaint    ,lturi     , &
                & tkemod    ,riglid    ,tstprt    ,prsmap    ,prshis    ,selmap    , &
                & selhis    ,filrol    ,kmax      ,gdp       )
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
!    Function: - Reads all records from the MD-file
!              - voor arrays waarden inlezen in subroutines
!                ivm indexering ????
!              - kopieer indien comgrid informatie naar
!                unformatted scratch files ????
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    use dfparall
    !use ec_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'fsm.i'
    include 'tri-dyn.igd'
    integer(pntrsize)             , pointer :: alpha
    integer(pntrsize)             , pointer :: cbuv
    integer(pntrsize)             , pointer :: cdwlsu
    integer(pntrsize)             , pointer :: cdwlsv
    integer(pntrsize)             , pointer :: cdwzbu
    integer(pntrsize)             , pointer :: cdwzbv
    integer(pntrsize)             , pointer :: cdwztu
    integer(pntrsize)             , pointer :: cdwztv
    integer(pntrsize)             , pointer :: cfurou
    integer(pntrsize)             , pointer :: cfvrou
    integer(pntrsize)             , pointer :: decay
    integer(pntrsize)             , pointer :: dicuv
    integer(pntrsize)             , pointer :: dpd
    integer(pntrsize)             , pointer :: dpu
    integer(pntrsize)             , pointer :: dpv
    integer(pntrsize)             , pointer :: drodep
    integer(pntrsize)             , pointer :: dxydro
    integer(pntrsize)             , pointer :: fcorio
    integer(pntrsize)             , pointer :: hkru
    integer(pntrsize)             , pointer :: hkrv
    integer(pntrsize)             , pointer :: hydrbc
    integer(pntrsize)             , pointer :: kcs
    integer(pntrsize)             , pointer :: omega
    integer(pntrsize)             , pointer :: pship
    integer(pntrsize)             , pointer :: r1
    integer(pntrsize)             , pointer :: rettim
    integer(pntrsize)             , pointer :: rtubnd
    integer(pntrsize)             , pointer :: rtur1
    integer(pntrsize)             , pointer :: s1
    integer(pntrsize)             , pointer :: thick
    integer(pntrsize)             , pointer :: u1
    integer(pntrsize)             , pointer :: ubrlsu
    integer(pntrsize)             , pointer :: ubrlsv
    integer(pntrsize)             , pointer :: umnldf
    integer(pntrsize)             , pointer :: uwtypu
    integer(pntrsize)             , pointer :: uwtypv
    integer(pntrsize)             , pointer :: v1
    integer(pntrsize)             , pointer :: vicuv
    integer(pntrsize)             , pointer :: vmnldf
    integer(pntrsize)             , pointer :: xcor
    integer(pntrsize)             , pointer :: ycor
    integer(pntrsize)             , pointer :: itdro
    integer(pntrsize)             , pointer :: kfu
    integer(pntrsize)             , pointer :: kfv
    integer(pntrsize)             , pointer :: kspu
    integer(pntrsize)             , pointer :: kspv
    integer(pntrsize)             , pointer :: mnbar
    integer(pntrsize)             , pointer :: mnbnd
    integer(pntrsize)             , pointer :: mndro
    integer(pntrsize)             , pointer :: mnksrc
    integer(pntrsize)             , pointer :: disint
    integer(pntrsize)             , pointer :: nambar
    integer(pntrsize)             , pointer :: nambnd
    integer(pntrsize)             , pointer :: namcon
    integer(pntrsize)             , pointer :: namdro
    integer(pntrsize)             , pointer :: namsrc
    integer(pntrsize)             , pointer :: tprofu
    integer(pntrsize)             , pointer :: typbnd
    integer                       , pointer :: rtcmod
    include 'pardef.igd'
    integer                       , pointer :: ncmax
    integer                       , pointer :: nmax
    integer                       , pointer :: mmax
    integer                       , pointer :: nmaxus
    integer                       , pointer :: nmmax
    integer                       , pointer :: lstsc
    integer                       , pointer :: lstsci
    integer                       , pointer :: lsal
    integer                       , pointer :: lsed
    integer                       , pointer :: lsedtot
    integer                       , pointer :: ltem
    integer                       , pointer :: lsec
    integer                       , pointer :: ltur
    integer                       , pointer :: ltur2d
    integer                       , pointer :: kmxdt
    integer                       , pointer :: npiwe
    integer                       , pointer :: kmxt
    integer                       , pointer :: nbub
    integer                       , pointer :: nxbub
    integer                       , pointer :: nfreqs
    integer                       , pointer :: nto
    integer                       , pointer :: ntof
    integer                       , pointer :: ntoq
    integer                       , pointer :: ntot
    integer                       , pointer :: kc
    integer                       , pointer :: kcd
    integer                       , pointer :: nsrc
    integer                       , pointer :: nsrcd
    integer                       , pointer :: nostat
    integer                       , pointer :: ntruv
    integer                       , pointer :: nofou
    integer                       , pointer :: ndro
    integer                       , pointer :: nsluv
    integer                       , pointer :: upwsrc
    integer                       , pointer :: nudge
    real(fp)                      , pointer :: chzmin
    real(fp)                      , pointer :: dco
    real(fp)                      , pointer :: dgcuni
    real(fp)                      , pointer :: drycrt
    real(fp)                      , pointer :: dryflc
    real(fp)                      , pointer :: fwfac
    real(fp)                      , pointer :: ftauw
    real(fp)                      , pointer :: gammax
    real(fp)                      , pointer :: rmincf
    real(fp)                      , pointer :: thetqh
    real(fp)                      , pointer :: thetqt
    real(fp)                      , pointer :: nudvic
    integer                       , pointer :: ibaroc
    integer                       , pointer :: iter1
    logical                       , pointer :: bndneu
    logical                       , pointer :: cstbnd
    logical                       , pointer :: v2dwbl
    character(1)                  , pointer :: ascon
    character(8)                  , pointer :: dpsopt
    character(8)                  , pointer :: dpuopt
    character(6)                  , pointer :: momsol
    real(fp)                      , pointer :: paver
    logical                       , pointer :: pcorr
    logical                       , pointer :: old_corio
    real(fp)                      , pointer :: rhow
    real(fp)                      , pointer :: rhoa
    real(fp)                      , pointer :: rhofrac
    real(fp)                      , pointer :: ag
    real(fp)                      , pointer :: z0v
    real(fp)                      , pointer :: vicouv
    real(fp)                      , pointer :: vicoww
    real(fp)                      , pointer :: dicouv
    real(fp)                      , pointer :: dicoww
    real(fp)      , dimension(:)  , pointer :: wstcof
    integer                       , pointer :: idensform
    integer                       , pointer :: irov
    real(fp)                      , pointer :: xlo
    real(fp)                      , pointer :: ck
    logical                       , pointer :: wind
    logical                       , pointer :: salin
    logical                       , pointer :: temp
    logical                       , pointer :: const
    logical                       , pointer :: drogue
    logical                       , pointer :: wave
    logical                       , pointer :: threed
    logical                       , pointer :: secflo
    logical                       , pointer :: iweflg
    logical                       , pointer :: struct
    logical                       , pointer :: sedim
    logical                       , pointer :: htur2d
    logical                       , pointer :: flmd2l
    logical                       , pointer :: mudlay
    logical                       , pointer :: couplemod
    logical                       , pointer :: zmodel
    logical                       , pointer :: nonhyd
    logical                       , pointer :: roller
    logical                       , pointer :: wavcmp
    logical                       , pointer :: lftrto
    integer                       , pointer :: itstrt
    integer                       , pointer :: itfinish
    integer                       , pointer :: itlfsm
    integer                       , pointer :: itmapf
    integer                       , pointer :: itmapi
    integer                       , pointer :: itmapl
    integer                       , pointer :: ithisf
    integer                       , pointer :: ithisi
    integer                       , pointer :: ithisl
    integer                       , pointer :: itcomf
    integer                       , pointer :: itcomi
    integer                       , pointer :: itcoml
    integer                       , pointer :: itnflf
    integer                       , pointer :: itnfli
    integer                       , pointer :: itnfll
    integer                       , pointer :: itnflrf
    integer                       , pointer :: itnflri
    integer                       , pointer :: itrsti
    integer                       , pointer :: iphisf
    integer                       , pointer :: iphisi
    integer                       , pointer :: iphisl
    integer       , dimension(:)  , pointer :: ipmap
    integer                       , pointer :: itimtt
    integer                       , pointer :: itiwei
    integer                       , pointer :: julday
    integer                       , pointer :: itdate
    integer                       , pointer :: gridECItemId
    real(fp)                      , pointer :: tstart
    real(fp)                      , pointer :: tstop
    real(fp)                      , pointer :: dt
    real(fp)                      , pointer :: tunit
    real(fp)                      , pointer :: tzone
    logical                       , pointer :: flgrd
    logical                       , pointer :: fldry
    logical                       , pointer :: fltd
    logical                       , pointer :: flcut
    logical                       , pointer :: fl45
    logical                       , pointer :: flbct
    logical                       , pointer :: flbcc
    logical                       , pointer :: fldis
    logical                       , pointer :: fltem
    logical                       , pointer :: fleva
    integer                       , pointer :: iopsus
    logical                       , pointer :: bedupd
    real(fp)                      , pointer :: zbot
    real(fp)                      , pointer :: ztop
!    type(tECHandle)               , pointer :: ECHandle
!
! Global variables
!
    integer                                     :: icreep  !  Description and declaration in tricom.igs
    integer                                     :: keva    !  Description and declaration in tricom.igs
    integer                                     :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                     :: ktemp   !  Description and declaration in tricom.igs
    integer                                     :: lturi   !  Description and declaration in tricom.igs
    integer                                     :: lundia  !  Description and declaration in inout.igs
    integer                                     :: lunmd   !  Description and declaration in inout.igs
    integer                                     :: lunscr  !  Description and declaration in inout.igs
    logical                                     :: error   !!  Flag=TRUE if an error is encountered
    logical                                     :: success
    logical                                     :: sferic  !  Description and declaration in tricom.igs
    logical                                     :: tstprt  !  Description and declaration in tricom.igs
    real(fp)                                    :: anglat  !!  - Angle of latitude of the model centre (used to determine the coef. for the coriolis force)
                                                           !!  - In spherical coordinates this parameter equals the angle of latitude
                                                           !!    for the origin (water level point) after INIPHY anglat = 0.
    real(fp)                                    :: anglon  !!  - Angle of longitude of the model centre (used to determine solar radiation)
    real(fp)                                    :: betac   !  Description and declaration in tricom.igs
    real(fp)                      , intent(out) :: dml     !  Description and declaration in tricom.igs
    real(fp)                                    :: dx      !!  Uniform grid-distance in the x-dir. in meters & in decimals
                                                           !!  if sferic = T dx := dphi in degrees & in decimals
    real(fp)                                    :: dy      !!  Uniform grid-distance in the y-dir. in meters & in decimals
                                                           !!  if sferic = T dy := dtheta in degrees & in decimals
    real(fp)                                    :: grdang  !  Description and declaration in tricom.igs
    real(fp)                                    :: riglid  !!  Rigid lid factor to reduce horizontal wet area (incompressible)
    real(fp)                                    :: saleqs  !  Description and declaration in tricom.igs
    real(fp)                                    :: temeqs  !  Description and declaration in tricom.igs
    character(*)                                :: filrgf  !!  File name for the curvi-linear grid file (telmcrgf.xxx)
                                                           !!  !! file will be read formatted !!
    character(*)                                :: filrol
    character(*)                                :: restid  !!  Run identification of the restart file. If RESTID = non-blank then
                                                           !!  current simulation will use this file to for setting the initial conditions
    character(*)                                :: runid   !!  Run identification code for the current simulation (used to determine
                                                           !!  the names of the in- /output files used by the system)
    character(*)                                :: selmap  !  Description and declaration in tricom.igs
    character(1)                                :: evaint  !  Description and declaration in tricom.igs
    character(1)                                :: forfuv  !  Description and declaration in tricom.igs
    character(1)                                :: forfww  !  Description and declaration in tricom.igs
    character(1)                                :: roumet  !!  Bed stress formulation specified:
                                                           !!   C : Chezy    W : White Colebrook
                                                           !!   M : Manning  Z : roughness par.
    character(1)                                :: temint  !  Description and declaration in tricom.igs
    character(12)                               :: tkemod  !  Description and declaration in tricom.igs
    character(13)                               :: trasol  !  Description and declaration in tricom.igs
    character(19)                               :: prsmap  !  Description and declaration in tricom.igs
    character(23)                               :: prshis  !  Description and declaration in tricom.igs
    character(23)                               :: selhis  !  Description and declaration in tricom.igs
    character(30) , dimension(10)               :: runtxt  !!  Textual description of model input
    character(36)                               :: tgfcmp  !  Description and declaration in tricom.igs
    character(4)                                :: rouwav  !  Description and declaration in tricom.igs
!
! Local variables
!
    integer                               :: iofset      ! Offset to get begin pointer of array at point (1,1) instead of (1,-1)
    integer                               :: ivapop      ! Flag specifying whether vapour pressure data are to be computed or specified Only for KTEMP=4 IVAPOP can be 1
    integer                               :: mxdnto      ! Array dimension for opening boundary definition arrays, which are dynamic declared (NTO in call to RDBNDD)
    integer                               :: nmaxddb
    integer                               :: sizenm
    integer                               :: nprttm      ! Number of print times steps
    integer                               :: nrrec       ! Pointer to the record number in the MD-file
    integer                               :: ntofgl      ! total number of H/A-type boundary sections of entire domain
    integer        , dimension(mxnto)     :: nhsub       ! integer array to store sequence numbers of harmonic boundary condition in own subdomain
    logical                               :: solrad_read ! Flag=TRUE means Nett Solar Radiation is to be read from .tem file
    logical                               :: yestdd      ! Flag for call from TDATOM (.true.) for time varying data
    logical                               :: ecwind      ! Temporary flag to switch between meteo and ec module
    real(fp)                              :: ccofu       ! Array containing the uniform bottom roughness coefficient in the x- dir. (value depends on ROUMET)
    real(fp)                              :: ccofv       ! Array containing the uniform bottom roughness coefficient in the y- dir. (value depends on ROUMET)
    real(fp)                              :: depuni      ! Initial depth value at depth points
    real(fp)                              :: tinciw      ! Time in UNIT's to activate the Internal Wave Energy calculation
    real(fp)                              :: tlfsmo      ! Timespan for smoothing (in minutes)
    real(fp)                              :: zini        ! Initial water elevation in the model
    character(1)                          :: equili      ! Equilibrium or advection and diffusion default = no equilibrium ('N') which means LSEC = 1
    character(1)                          :: sphere      ! Flag Yes / No spherical coordinates
    character(1)   , dimension(mxnto)     :: datbnd      ! Type of open boundary: -'H'(armonic/Tide) -'T'(ime series/time dependent)
    character(2)                          :: fmtfil      ! File format for attribute files
    character(10)                         :: citdat      ! Reference date for the simulation times. Format: "DD MMM 'YY"
    character(12)  , dimension(mxnto, 2)  :: statns      ! References to tidal stations at boundary support points
    character(100)                        :: message
    character(256)                        :: filic       !  File name of initial condition file
    character(256)                        :: filnam      ! File name for attribute files
    character(300)                        :: mdfrec      ! Standard rec. length in MD-file (300) 300 = 256 + a bit (field, =, ##, etc.)
!
!! executable statements -------------------------------------------------------
!
    ncmax               => gdp%d%ncmax
    nmax                => gdp%d%nmax
    mmax                => gdp%d%mmax
    nmaxus              => gdp%d%nmaxus
    nmmax               => gdp%d%nmmax
    lstsc               => gdp%d%lstsc
    lstsci              => gdp%d%lstsci
    lsal                => gdp%d%lsal
    lsed                => gdp%d%lsed
    lsedtot             => gdp%d%lsedtot
    ltem                => gdp%d%ltem
    lsec                => gdp%d%lsec
    ltur                => gdp%d%ltur
    ltur2d              => gdp%d%ltur2d
    kmxdt               => gdp%d%kmxdt
    npiwe               => gdp%d%npiwe
    kmxt                => gdp%d%kmxt
    nbub                => gdp%d%nbub
    nxbub               => gdp%d%nxbub
    nfreqs              => gdp%d%nfreqs
    nto                 => gdp%d%nto
    ntof                => gdp%d%ntof
    ntoq                => gdp%d%ntoq
    ntot                => gdp%d%ntot
    kc                  => gdp%d%kc
    kcd                 => gdp%d%kcd
    nsrc                => gdp%d%nsrc
    nsrcd               => gdp%d%nsrcd
    nostat              => gdp%d%nostat
    ntruv               => gdp%d%ntruv
    nofou               => gdp%d%nofou
    ndro                => gdp%d%ndro
    nsluv               => gdp%d%nsluv
    upwsrc              => gdp%d%upwsrc
    itdate              => gdp%gdexttim%itdate
    tstart              => gdp%gdexttim%tstart
    tstop               => gdp%gdexttim%tstop
    dt                  => gdp%gdexttim%dt
    tunit               => gdp%gdexttim%tunit
    tzone               => gdp%gdexttim%tzone
    itstrt              => gdp%gdinttim%itstrt
    itfinish            => gdp%gdinttim%itfinish
    itlfsm              => gdp%gdinttim%itlfsm
    itmapf              => gdp%gdinttim%itmapf
    itmapi              => gdp%gdinttim%itmapi
    itmapl              => gdp%gdinttim%itmapl
    ithisf              => gdp%gdinttim%ithisf
    ithisi              => gdp%gdinttim%ithisi
    ithisl              => gdp%gdinttim%ithisl
    itcomf              => gdp%gdinttim%itcomf
    itcomi              => gdp%gdinttim%itcomi
    itcoml              => gdp%gdinttim%itcoml
    itnflf              => gdp%gdinttim%itnflf
    itnfli              => gdp%gdinttim%itnfli
    itnfll              => gdp%gdinttim%itnfll
    itnflrf             => gdp%gdinttim%itnflrf
    itnflri             => gdp%gdinttim%itnflri
    itrsti              => gdp%gdinttim%itrsti
    iphisf              => gdp%gdinttim%iphisf
    iphisi              => gdp%gdinttim%iphisi
    iphisl              => gdp%gdinttim%iphisl
    ipmap               => gdp%gdinttim%ipmap
    itimtt              => gdp%gdinttim%itimtt
    itiwei              => gdp%gdinttim%itiwei
    julday              => gdp%gdinttim%julday
    iopsus              => gdp%gdmorpar%iopsus
    bedupd              => gdp%gdmorpar%bedupd
    chzmin              => gdp%gdnumeco%chzmin
    dco                 => gdp%gdnumeco%dco
    dgcuni              => gdp%gdnumeco%dgcuni
    drycrt              => gdp%gdnumeco%drycrt
    dryflc              => gdp%gdnumeco%dryflc
    fwfac               => gdp%gdnumeco%fwfac
    ftauw               => gdp%gdnumeco%ftauw
    gammax              => gdp%gdnumeco%gammax
    rmincf              => gdp%gdnumeco%rmincf
    thetqh              => gdp%gdnumeco%thetqh
    thetqt              => gdp%gdnumeco%thetqt
    ibaroc              => gdp%gdnumeco%ibaroc
    iter1               => gdp%gdnumeco%iter1
    bndneu              => gdp%gdnumeco%bndneu
    cstbnd              => gdp%gdnumeco%cstbnd
    dpsopt              => gdp%gdnumeco%dpsopt
    dpuopt              => gdp%gdnumeco%dpuopt
    momsol              => gdp%gdnumeco%momsol
    paver               => gdp%gdnumeco%paver
    pcorr               => gdp%gdnumeco%pcorr
    old_corio           => gdp%gdnumeco%old_corio
    nudge               => gdp%gdnumeco%nudge
    nudvic              => gdp%gdnumeco%nudvic
    v2dwbl              => gdp%gdnumeco%v2dwbl
    rhow                => gdp%gdphysco%rhow
    rhoa                => gdp%gdphysco%rhoa
    rhofrac             => gdp%gdphysco%rhofrac
    ag                  => gdp%gdphysco%ag
    z0v                 => gdp%gdphysco%z0v
    vicouv              => gdp%gdphysco%vicouv
    vicoww              => gdp%gdphysco%vicoww
    dicouv              => gdp%gdphysco%dicouv
    dicoww              => gdp%gdphysco%dicoww
    wstcof              => gdp%gdphysco%wstcof
    idensform           => gdp%gdphysco%idensform
    irov                => gdp%gdphysco%irov
    wind                => gdp%gdprocs%wind
    salin               => gdp%gdprocs%salin
    temp                => gdp%gdprocs%temp
    const               => gdp%gdprocs%const
    drogue              => gdp%gdprocs%drogue
    wave                => gdp%gdprocs%wave
    threed              => gdp%gdprocs%threed
    secflo              => gdp%gdprocs%secflo
    iweflg              => gdp%gdprocs%iweflg
    struct              => gdp%gdprocs%struct
    sedim               => gdp%gdprocs%sedim
    htur2d              => gdp%gdprocs%htur2d
    flmd2l              => gdp%gdprocs%flmd2l
    mudlay              => gdp%gdprocs%mudlay
    couplemod           => gdp%gdprocs%couplemod
    zmodel              => gdp%gdprocs%zmodel
    nonhyd              => gdp%gdprocs%nonhyd
    roller              => gdp%gdprocs%roller
    wavcmp              => gdp%gdprocs%wavcmp
    lftrto              => gdp%gdprocs%lftrto
    alpha               => gdp%gdr_i_ch%alpha
    cbuv                => gdp%gdr_i_ch%cbuv
    cdwlsu              => gdp%gdr_i_ch%cdwlsu
    cdwlsv              => gdp%gdr_i_ch%cdwlsv
    cdwzbu              => gdp%gdr_i_ch%cdwzbu
    cdwzbv              => gdp%gdr_i_ch%cdwzbv
    cdwztu              => gdp%gdr_i_ch%cdwztu
    cdwztv              => gdp%gdr_i_ch%cdwztv
    cfurou              => gdp%gdr_i_ch%cfurou
    cfvrou              => gdp%gdr_i_ch%cfvrou
    decay               => gdp%gdr_i_ch%decay
    dicuv               => gdp%gdr_i_ch%dicuv
    dpd                 => gdp%gdr_i_ch%dpd
    dpu                 => gdp%gdr_i_ch%dpu
    dpv                 => gdp%gdr_i_ch%dpv
    drodep              => gdp%gdr_i_ch%drodep
    dxydro              => gdp%gdr_i_ch%dxydro
    fcorio              => gdp%gdr_i_ch%fcorio
    hkru                => gdp%gdr_i_ch%hkru
    hkrv                => gdp%gdr_i_ch%hkrv
    hydrbc              => gdp%gdr_i_ch%hydrbc
    kcs                 => gdp%gdr_i_ch%kcs
    omega               => gdp%gdr_i_ch%omega
    pship               => gdp%gdr_i_ch%pship
    r1                  => gdp%gdr_i_ch%r1
    rettim              => gdp%gdr_i_ch%rettim
    rtubnd              => gdp%gdr_i_ch%rtubnd
    rtur1               => gdp%gdr_i_ch%rtur1
    s1                  => gdp%gdr_i_ch%s1
    thick               => gdp%gdr_i_ch%thick
    u1                  => gdp%gdr_i_ch%u1
    ubrlsu              => gdp%gdr_i_ch%ubrlsu
    ubrlsv              => gdp%gdr_i_ch%ubrlsv
    umnldf              => gdp%gdr_i_ch%umnldf
    uwtypu              => gdp%gdr_i_ch%uwtypu
    uwtypv              => gdp%gdr_i_ch%uwtypv
    v1                  => gdp%gdr_i_ch%v1
    vicuv               => gdp%gdr_i_ch%vicuv
    vmnldf              => gdp%gdr_i_ch%vmnldf
    xcor                => gdp%gdr_i_ch%xcor
    ycor                => gdp%gdr_i_ch%ycor
    itdro               => gdp%gdr_i_ch%itdro
    kfu                 => gdp%gdr_i_ch%kfu
    kfv                 => gdp%gdr_i_ch%kfv
    kspu                => gdp%gdr_i_ch%kspu
    kspv                => gdp%gdr_i_ch%kspv
    mnbar               => gdp%gdr_i_ch%mnbar
    mnbnd               => gdp%gdr_i_ch%mnbnd
    mndro               => gdp%gdr_i_ch%mndro
    mnksrc              => gdp%gdr_i_ch%mnksrc
    disint              => gdp%gdr_i_ch%disint
    nambar              => gdp%gdr_i_ch%nambar
    nambnd              => gdp%gdr_i_ch%nambnd
    namcon              => gdp%gdr_i_ch%namcon
    namdro              => gdp%gdr_i_ch%namdro
    namsrc              => gdp%gdr_i_ch%namsrc
    tprofu              => gdp%gdr_i_ch%tprofu
    typbnd              => gdp%gdr_i_ch%typbnd
    rtcmod              => gdp%gdrtc%rtcmod
    flgrd               => gdp%gdtmpfil%flgrd
    fldry               => gdp%gdtmpfil%fldry
    fltd                => gdp%gdtmpfil%fltd
    flcut               => gdp%gdtmpfil%flcut
    fl45                => gdp%gdtmpfil%fl45
    flbct               => gdp%gdtmpfil%flbct
    flbcc               => gdp%gdtmpfil%flbcc
    fldis               => gdp%gdtmpfil%fldis
    fltem               => gdp%gdtmpfil%fltem
    fleva               => gdp%gdtmpfil%fleva
    xlo                 => gdp%gdturcoe%xlo
    ck                  => gdp%gdturcoe%ck
    zbot                => gdp%gdzmodel%zbot
    ztop                => gdp%gdzmodel%ztop
    ascon               => gdp%gdbcdat%ascon
    !ECHandle            => gdp%gd_ECHandle
    !gridECItemId        => gdp%gridECItemId
    !!
    rewind (lunmd)
    read (lunmd, '(a300)') mdfrec
    !
    ! DML not used anymore
    !
    dml = 1.0
    !
    ! local parameters
    !
    nmaxddb = nmax + 2*gdp%d%ddbound
    iofset  = 2*nmaxddb
    nrrec   = 1
    yestdd  = .false.
    mxdnto  = nto
    !
    ! initialize global parameters & work/help arrays
    !
    flbct   = .false.
    flbcc   = .false.
    fldis   = .false.
    fltem   = .false.
    fleva   = .false.
    rhofrac = 1.0
    !
    ! Read model description
    !
    call rdrund(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
              & runtxt    ,gdp       )
    if (error) goto 9999
    !
    if (zmodel) call prterr(lundia    ,'G051'    ,'Z-model used' )
    !
    ! Read grid information
    !
    call rdxyzo(lunmd     ,lundia         ,error          ,nrrec     ,mdfrec    , &
              & kmax      ,zbot           ,ztop           ,dx        ,dy        , &
              & filrgf    ,fmtfil         ,r(thick)       ,anglat    ,anglon    , &
              & grdang    ,sphere         ,sferic         ,zmodel    ,mmax      , &
              & nmax      ,r(xcor+iofset) ,r(ycor+iofset) ,gdp       )
    if (error) goto 9999
    !
    ! GRID dimensions read in a group
    !
    call rdgrid(lunmd     ,lundia    ,error     ,zmodel    ,nrrec     , &
              & mdfrec    ,runid     ,mmax      ,nmaxus    ,filnam    , &
              & fmtfil    ,flgrd     ,filnam    ,fmtfil    ,fldry     , &
              & filnam    ,fmtfil    ,fltd      ,filnam    ,flcut     , &
              & filnam    ,fl45      ,gdp       )
    if (error) goto 9999
    !
    ! Special points, discharge sources ,barriers and weir losses
    ! (barriers and local weir losses not yet implemented)
    !
    call rdspec(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
              & yestdd    ,filnam    ,fmtfil    ,nsrcd     ,mmax      , &
              & nmax      ,nmaxus    ,i(mnksrc) ,ch(namsrc),ch(disint), &
              & upwsrc    ,gdp       )
    if (error) goto 9999
    !
    ! Depth information
    !
    call rddept(lundia    ,error     , &
              & filnam    ,fmtfil    ,depuni    ,mmax      , &
              & nmax      ,nmaxus    ,r(dpd)    ,gdp       )
    if (error) goto 9999
    !
    ! Open boundary definition
    ! only if nto > 0
    ! initialize local parameters first
    !
    ntofgl = ntof
    if (nto > 0) then
       call rdbndd(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
                 & nto       ,ntof      ,ntoq      ,mmax      ,nmaxus    , &
                 & kmax      ,mxdnto    ,mxnto     ,filnam    ,fmtfil    , &
                 & ascon     ,ch(nambnd),ch(typbnd),datbnd    ,i(mnbnd)  , &
                 & r(alpha)  ,ch(tprofu),statns    ,nhsub     ,yestdd    , &
                 & gdp       )
       if (error) goto 9999
    endif
    !
    ! Initial Run Time parameters and define Julian Day from ITDATE
    !
    call rdirt(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
             & citdat    ,tstart    ,tstop     ,tzone     ,itdate    , &
             & julday    ,itstrt    ,itfinish  ,dt        ,tunit     , &
             & gdp       )
    if (error) goto 9999
    !
    ! Open boundary correction
    !
    if (nto>0) then
       call fbcorr(lundia    ,nto       ,ch(nambnd),ch(typbnd),gdp)
    endif
    !
    ! Restart file or Initial Conditions and names of constituents
    !
    call rdic(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
            & runid     ,restid    ,filic     ,fmtfil    ,salin     , &
            & temp      ,const     ,secflo    ,lturi     ,lsal      , &
            & ltem      ,lstsc     ,zini      ,mmax      , &
            & nmax      ,nmaxus    ,kmax      ,lstsci    ,ltur      , &
            & ch(namcon),r(s1)     ,r(u1)     ,r(v1)     ,r(r1)     , &
            & r(rtur1)  ,r(decay)  ,r(umnldf) ,r(vmnldf) ,i(kfu)    , &
            & i(kfv)    ,r(dpd)    ,lsed      ,gdp       )
    if (error) goto 9999
    !
    ! Boundary conditions general (only if nto > 0)
    ! smoothing and Thatcher Harleman return times
    !
    if (nto > 0) then
       call rdbcg(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
                & itlfsm    ,tlfsmo    ,dt        ,tunit     ,nto       , &
                & lstsc     ,bndneu    ,cstbnd    ,ch(nambnd),ch(typbnd), &
                & r(rettim) ,ntoq      ,thetqh    ,thetqt    ,restid    , &
                & filic     ,paver     ,pcorr     ,tstart    ,tstop     , &
                & mxdnto    ,gdp       )
       if (error) goto 9999
    endif
    !
    ! Harmonic boundary conditions always read from file
    ! only if ntof > 0 and kc > 0 (nr. of kc has been tested in dimrd)
    !
    if (ntof > 0) then
        if (ascon == 'Y') then
            call read_triana(lundia    ,error     ,kc        ,statns    ,ntof      ,gdp)
            if (error) goto 9999
        else
            call rdibch(lundia    ,error     ,runid     ,ntof      ,nto       , &
                      & kc        ,kcd       ,r(omega)  ,r(hydrbc) ,nhsub     , &
                      & ntofgl    ,gdp       )
            if (error) goto 9999
        endif
    endif
    !
    ! Boundary conditions Time series for hydrodynamics, only if ntot> 0
    ! Boundary conditions Time series for constituents,
    ! only if nto  > 0 and lstsc > 0
    !
    if (parll) ntot = nto - ntof - ntoq
    if (ntot > 0) flbct = .true.
    if (nto>0 .and. lstsc>0) flbcc = .true.
    !
    ! Boundary condition for 2D turbulence parameters in case LTUR2D > 0
    !
    if (ltur2d > 0) then
       call rdq2eb(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
                 & filnam    ,nto       ,mmax      ,nmax      ,nmaxus    , &
                 & i(mnbnd)  ,r(rtubnd) ,gdp       )
       if (error) goto 9999
    endif
    !
    ! Physical Coefficients, Hydrodynamic General
    !
    call rdhyg(ag, rhow, gdp)
    !
    ! Physical Coefficients, Hydrodynamic Bedstress
    !
    call rdhyb(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
             & roumet    ,threed    ,filnam    ,fmtfil    ,ccofu     , &
             & ccofv     ,wave      ,rouwav    ,mmax      ,nmax      , &
             & nmaxus    ,r(cfurou) ,r(cfvrou) ,gdp       )
    if (error) goto 9999
    !
    ! Physical Coefficients, Hydrodynamic Viscosity and Diffusion
    ! initialize global and local parameters first
    !
    call rdhyvd(error     ,nrrec     ,mdfrec    ,filnam    ,fmtfil    , &
              & tkemod    ,xlo       ,vicouv    ,dicouv    ,vicoww    , &
              & dicoww    ,mmax      ,nmax      ,nmaxus    ,kmax      , &
              & lstsci    ,r(vicuv)  ,r(dicuv)  ,gdp       )
    if (error) goto 9999
    !
    ! Physical Coefficients, Process
    ! Salin   - temeqs and saleqs
    ! Temp    - ktemp, keva
    ! Wind    - wstcof(4) and rhoa
    ! Drogue  - drogues
    !
    call rdproc(error     ,nrrec     ,mdfrec    ,htur2d     ,salin    , &
              & temp      ,wind      ,ktemp     ,keva       ,ivapop   , &
              & irov      ,z0v       ,sferic     ,tgfcmp   , &
              & temeqs    ,saleqs    ,wstcof    ,rhoa       ,secflo   , &
              & betac     ,equili    ,lsec      ,chzmin     ,rmincf   , &
              & rtcmod    ,couplemod ,nonhyd    ,mmax       ,nmax     , &
              & nmaxus    ,sedim     ,idensform ,solrad_read, gdp)
    if (error) goto 9999
    !
    ! Physical Coefficients, Special
    ! Barrier  - not yet implemented
    ! Weirloss - not yet implemented
    !
    ! Physical Numerical parameters
    !
    call rdnum(lunmd     ,lundia    ,nrrec     ,mdfrec    , &
             & iter1     ,dryflc    ,drycrt    ,dco       ,ibaroc    , &
             & kmax      , &
             & lstsci    ,icreep    ,trasol    ,momsol    ,dgcuni    , &
             & forfuv    ,forfww    ,ktemp     ,temint    , &
             & keva      ,evaint    ,old_corio , &
             & dpsopt    ,dpuopt    ,zmodel    ,gammax    ,fwfac     , &
             & nudge     ,nudvic    ,v2dwbl    ,ftauw     ,gdp       )
    !
    ! Space varying coriolis field or
    ! calculate for SFERIC = .true. depending on ANGLAT and DY or
    ! for all other cases on ANGLAT
    !
    call rdfcio(lunmd     ,lundia    ,error     ,mdfrec    ,nrrec     , &
              & sferic    ,anglat    ,dy        ,filnam    ,fmtfil    , &
              & mmax      ,nmax      ,nmaxus    ,r(fcorio) ,gdp       )
    if (error) goto 9999
    !
    ! Special points
    ! Local Weirs, Gates, Rigid Sheets and Floating Structures
    !
    call rdstru(lunmd     ,lundia    ,error     ,mdfrec    ,nrrec     , &
              & riglid    ,struct    ,nsluv     ,mmax      ,nmax      , &
              & nmaxus    ,kmax      ,ch(nambar),i(kspu)   ,i(kspv)   , &
              & i(mnbar)  ,r(cbuv)   ,r(pship)  ,r(ubrlsu) ,r(ubrlsv) , &
              & nxbub     ,nbub      ,nsrc      ,i(mnksrc) ,ch(namsrc), &
              & r(uwtypu) ,r(uwtypv) ,r(dpu)    ,r(dpv)    ,r(hkru)   , &
              & r(hkrv)   ,r(cdwztu) ,r(cdwzbu) ,r(cdwztv) ,r(cdwzbv) , &
              & r(cdwlsu) ,r(cdwlsv) ,gdp)
    if (error) goto 9999
    !
    ! Discharge sources Time series        , only if NSRC  > 0
    ! Temperature Time series              , only if KTEMP > 0
    ! Precipitation/Evaporation Time series, only if KEVA  > 0
    ! Wind-forcing Time series             , only if WIND  = .true.
    !
    if (nsrc  > 0) fldis = .true.
    if (ktemp > 0) fltem = .true.
    if (keva  > 0) fleva = .true.
    !
    ! Site: monitoring station + monitoring cross-section + RTC input station
    !
    call rdsite(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
              & dt        ,filnam    ,fmtfil    ,nostat    ,filnam    , &
              & fmtfil    ,ntruv     ,filnam    ,fmtfil    ,ndro      , &
              & drogue    ,ch(namdro),i(mndro)  ,i(itdro)  ,r(dxydro) , &
              & r(drodep) ,gdp       )
    if (error) goto 9999
    !
    ! Output Site Quantity
    !
    call rdprfl(lunmd     ,lundia    ,nrrec     ,mdfrec    ,tstprt    , &
              & kmax      ,lstsci    ,ltur      ,lsal      ,ltem      , &
              & nostat    ,ntruv     ,prsmap    ,prshis    ,selmap    , &
              & selhis    ,lsed      ,gdp       )
    !
    ! Read  fourier input file
    !
    if (nofou > 0) then
       call rdfour(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
                 & nofou     ,kmax      ,lstsc     ,lsal      ,ltem      , &
                 & gdp       )
       if (error) goto 9999
    endif
    !
    ! Output files Print and Store times
    ! initialize global and local parameters first
    ! If no Print flag found or the first value should be -1
    !
    call rdtimo(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
              & tstop     ,dt        ,ipmap     ,mxprt     , &
              & nprttm    ,itfinish  ,iphisf    ,iphisi    ,iphisl    , &
              & itmapf    ,itmapi    ,itmapl    ,ithisf    ,ithisi    , &
              & ithisl    ,itcomf    ,itcomi    ,itcoml    ,itrsti    , &
              & itnflf    ,itnfli    ,itnfll    ,itnflrf   ,itnflri   , &
              & lstsci    ,gdp       )
    if (error) goto 9999
    !
    ! Read waq parameters, such as output flag for writing binary waq files
    !
    call rdwaqpar(lundia, error, kmax, lsed, dt, itcomf, itcomi, itcoml, gdp)
    if (error) goto 9999
    !
    ! Physical Coefficients, Trachytope Roughness Description
    ! Trachytope reading should be called after rdwaqpar since the WAQOL is read in rdwaqpar.
    !
    call rdtrt(lundia    ,error     ,lftrto    ,dt        ,mmax      , &
             & nmax      ,nmaxus    ,kmax      ,itimtt    ,gdp       )
    if (error) goto 9999
    !
    ! Read bedform characteristics
    !
    call rdbedformpar(lundia    ,error     ,nmax      ,mmax      ,nmaxus    , &
                    & nmmax     ,sedim     ,gdp       )
    if (error) goto 9999
    !
    ! Sediment input if flag SEDIM = .true.
    ! Force certain valus of DPUOPT
    !
    if (sedim) then
       call rdsedmortra(lundia    ,error     ,lsal      ,ltem      ,lsed      , &
                      & lsedtot   ,lstsci    ,ltur      ,ch(namcon),iopsus    , &
                      & mmax      ,nmax      ,nmaxus    ,nmmax     ,nto       , &
                      & ch(nambnd),lsec      ,tstart    ,tunit     ,gdp       )
       if (error) goto 9999
    endif
    !
    ! Check values of DPSOPT and DPUOPT
    !
    call ck_dpopt(lundia    ,lsedtot  ,zmodel   ,bedupd    ,dpsopt    , &
                & dpuopt    ,gdp      )
    !
    ! Roller Energy input if flag roller = .true.
    !
    if (roller) then
       call rdroll(lunmd     ,lundia    ,lunscr    ,error     ,nrrec     , &
                 & mdfrec    ,wavcmp    ,filrol    ,ncmax     ,gdp       )
       if (error) goto 9999
    endif
    !
    ! FluidMud input if flag mudlay = .true.
    !
    if (mudlay) then
       call rdmud(lunmd     ,lundia    ,error     ,nrrec     ,gdp       )
       if (error) goto 9999
    endif
    !
    ! Calculate rhofrac
    ! rhofrac should only be used (<>1.0) in the mud layer in case of
    ! a fluidmud calculation (flmd2l = true and mudlay = true).
    ! In the research version of fluidmud, ag was replaced by:
    ! agm = ag * rhofrac
    ! locally in cucnp and uzd. This caused some problems.....
    !
    if (flmd2l .and. mudlay) then
       rhofrac = (rhow - rhoa)/rhow
    else
       rhofrac = 1.0
    endif
    !
    ! IWE input if flag IWEFLG = .true.
    !
    filnam = ' '
    if (iweflg) then
       call rdiwe(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
                & dt        ,itiwei    ,tinciw    ,kmxt      ,kmxdt     , &
                & nfreqs    ,npiwe     ,filnam    ,gdp       )
       if (error) goto 9999
    endif
    !
    ! User defined Input
    ! First number of User Defined processes. Then user defined files
    ! and user defined constants related to these processes
    !
    call rdusrp(lunmd     ,lundia    ,error     ,mdfrec    ,nrrec     , &
              & gdp       )
    if (error) goto 9999
    !
    call rdusrf(lunmd     ,lundia    ,error     ,mdfrec    ,nrrec     , &
              & gdp       )
    if (error) goto 9999
    !
    call rdusrc(lunmd     ,lundia    ,error     ,mdfrec    ,nrrec     , &
              & gdp       )
    if (error) goto 9999
    !
    ! Real User defined data
    !
    call u_rdat(lundia    ,error     ,gdp       )
    !
    ! Initialization of ec and meteo module
    !
    ecwind = .false.
    !success = create(ECHandle, .true.)
    !call checkResult(ECHandle, success)
    !if (ecwind) then
    !   sizenm = nmax*(1 + gdp%d%mub - gdp%d%mlb)
    !   gridECItemId = addElementSet(ECHandle, r(xcor+iofset:xcor+iofset+sizenm), &
    !                                        & r(ycor+iofset:ycor+iofset+sizenm), &
    !                                        & i(kcs:kcs+sizenm), sferic, nmax, &
    !                                        & mmax, gdp%d%mub, gdp%d%mlb)
    !   if (gridECItemId == 0) call checkResult(ECHandle)
    !   success = setInternalGrid(ECHandle, gridECItemId)
    !endif
    call rdmeteo(gdp, ecwind)
    !
    if (ecwind) then
    !   success = ECCheck(ECHandle, 60d0) ! todo: real(ntstop,hp) * dt)
    !   call checkResult(ECHandle, success)
    endif
    !
    ! Limit fwfac
    ! fwfac can be read in rdnum and in rdmor
    ! This limitation must be done after both calls
    !
    fwfac = max(0.0_fp, fwfac)
    !
    ! Echo fwfac when it is not the default value (1.0)
    !
    if (comparereal(fwfac,1.0_fp) /= 0) then
       write (message, '(a,f8.4)') 'Tuning parameter for wave streaming (fwfac) :', fwfac
       call prterr(lundia, 'G051', trim(message))
    endif
    !
    ! ftauw limiter on tauwav in taubot.f90
    !
    ftauw = max(0.0_fp, ftauw)
    !
    ! Echo ftauw when it is not the default value (1.0)
    !
    if (comparereal(ftauw,1.0_fp) /= 0) then
       write (message, '(a,f8.4)') 'Tuning parameter for wave-induced friction (ftauw) :', ftauw
       call prterr(lundia, 'G051', trim(message))
    endif
    !
 9999 continue
    close (lunmd)
end subroutine readmd
