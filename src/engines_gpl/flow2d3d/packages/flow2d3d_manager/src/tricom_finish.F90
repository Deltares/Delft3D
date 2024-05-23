subroutine tricom_finish(olv_handle, gdp)
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
!    Function: - Finalize Delft3D-FLOW computation
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use meteo
    use SyncRtcFlow
    use sync_flm
    use sync_flowcouple
    use sync_flowwave
    use flow2d3d_timers
    use d3d_olv_class
    use D3D_Sobek 
    use D3DPublish
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat), target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'fsm.i'
    include 'tri-dyn.igd'
    real(fp)                            , pointer :: thr
    integer                             , pointer :: ncmax
    integer                             , pointer :: nmax
    integer                             , pointer :: mmax
    integer                             , pointer :: nlb
    integer                             , pointer :: nub
    integer                             , pointer :: mlb
    integer                             , pointer :: mub
    integer                             , pointer :: ddbound
    integer                             , pointer :: nmaxus
    integer                             , pointer :: kmax
    integer                             , pointer :: nmaxd
    integer                             , pointer :: mmaxd
    integer                             , pointer :: jstart
    integer                             , pointer :: nmmaxj
    integer                             , pointer :: nmmax
    integer                             , pointer :: lmax
    integer                             , pointer :: lsal
    integer                             , pointer :: lsts
    integer                             , pointer :: lstsc
    integer                             , pointer :: lstsci
    integer                             , pointer :: lsed
    integer                             , pointer :: lsedtot
    integer                             , pointer :: lsecfl
    integer                             , pointer :: lsec
    integer                             , pointer :: ltur
    integer                             , pointer :: ltur2d
    integer                             , pointer :: noroco
    integer                             , pointer :: norow
    integer                             , pointer :: nocol
    integer                             , pointer :: nto
    integer                             , pointer :: kc
    integer                             , pointer :: nrob
    integer                             , pointer :: nsrc
    integer                             , pointer :: nostat
    integer                             , pointer :: ntruv
    integer                             , pointer :: ntru
    integer                             , pointer :: nsluv
    integer                             , pointer :: itdate
    real(fp)                            , pointer :: tstart
    real(fp)                            , pointer :: tstop
    real(fp)                            , pointer :: dt
    real(fp)                            , pointer :: tunit
    real(fp)          , dimension(:)    , pointer :: dm
    real(fp)          , dimension(:)    , pointer :: dg
    real(fp)          , dimension(:,:)  , pointer :: frac
    real(fp)                            , pointer :: cp
    real(fp)                            , pointer :: sarea
    real(fp)                            , pointer :: fclou
    integer                             , pointer :: lunmd
    integer                             , pointer :: lundia
    integer                             , pointer :: lunprt
    integer                             , pointer :: lunscr
    real(fp)                            , pointer :: timsec
    real(fp)                            , pointer :: timmin
    real(fp)                            , pointer :: timhr
    integer                             , pointer :: itstrt
    integer                             , pointer :: itfinish
    integer                             , pointer :: itlfsm
    integer                             , pointer :: itmapf
    integer                             , pointer :: itmapi
    integer                             , pointer :: itmapl
    integer                             , pointer :: ithisi
    integer                             , pointer :: itcomf
    integer                             , pointer :: itcomi
    integer                             , pointer :: itcoml
    integer                             , pointer :: itdrof
    integer                             , pointer :: itdroi
    integer                             , pointer :: itrsti
    integer                             , pointer :: iphisf
    integer                             , pointer :: iphisi
    integer                             , pointer :: iphisl
    integer           , dimension(:)    , pointer :: ipmap
    integer                             , pointer :: itimtt
    integer                             , pointer :: ittrtu
    integer                             , pointer :: itiwei
    integer                             , pointer :: itdiag
    integer                             , pointer :: julday
    integer                             , pointer :: ntstep
    logical                             , pointer :: multi
    character(256)                      , pointer :: mmsyncfilnam
    real(fp)                            , pointer :: rhow
    integer                             , pointer :: iro
    logical                             , pointer :: wind
    integer                             , pointer :: waveol
    logical                             , pointer :: mudlay
    logical                             , pointer :: mudwave
    logical                             , pointer :: coupleact
    logical                             , pointer :: couplemod
    logical                             , pointer :: zmodel
    logical                             , pointer :: nonhyd
    logical                             , pointer :: roller
    logical                             , pointer :: wavcmp
    logical                             , pointer :: cnstwv
    logical                             , pointer :: lftrto
    logical                             , pointer :: sbkol
    integer                             , pointer :: numdomains
    integer                             , pointer :: nummappers
    integer(pntrsize)                   , pointer :: alfas
    integer(pntrsize)                   , pointer :: ampbc
    integer(pntrsize)                   , pointer :: c
    integer(pntrsize)                   , pointer :: cgc
    integer(pntrsize)                   , pointer :: ctr
    integer(pntrsize)                   , pointer :: dircom
    integer(pntrsize)                   , pointer :: dircos
    integer(pntrsize)                   , pointer :: dirsin
    integer(pntrsize)                   , pointer :: dis
    integer(pntrsize)                   , pointer :: disch
    integer(pntrsize)                   , pointer :: discom
    integer(pntrsize)                   , pointer :: dpd
    integer(pntrsize)                   , pointer :: dpc
    integer(pntrsize)                   , pointer :: dps
    integer(pntrsize)                   , pointer :: dpu
    integer(pntrsize)                   , pointer :: dpv
    integer(pntrsize)                   , pointer :: ewabr0
    integer(pntrsize)                   , pointer :: ewabr1
    integer(pntrsize)                   , pointer :: ewave0
    integer(pntrsize)                   , pointer :: ewave1
    integer(pntrsize)                   , pointer :: grmasu
    integer(pntrsize)                   , pointer :: grmasv
    integer(pntrsize)                   , pointer :: gro
    integer(pntrsize)                   , pointer :: gsqd
    integer(pntrsize)                   , pointer :: gsqs
    integer(pntrsize)                   , pointer :: guu
    integer(pntrsize)                   , pointer :: guv
    integer(pntrsize)                   , pointer :: gvu
    integer(pntrsize)                   , pointer :: gvv
    integer(pntrsize)                   , pointer :: hkru
    integer(pntrsize)                   , pointer :: hkrv
    integer(pntrsize)                   , pointer :: hrmcom
    integer(pntrsize)                   , pointer :: hrms
    integer(pntrsize)                   , pointer :: hu
    integer(pntrsize)                   , pointer :: huvw
    integer(pntrsize)                   , pointer :: hv
    integer(pntrsize)                   , pointer :: msucom
    integer(pntrsize)                   , pointer :: msvcom
    integer(pntrsize)                   , pointer :: ombc
    integer(pntrsize)                   , pointer :: phibc
    integer(pntrsize)                   , pointer :: qu
    integer(pntrsize)                   , pointer :: qxk
    integer(pntrsize)                   , pointer :: qxkr
    integer(pntrsize)                   , pointer :: qxkw
    integer(pntrsize)                   , pointer :: qyk
    integer(pntrsize)                   , pointer :: qykr
    integer(pntrsize)                   , pointer :: qykw
    integer(pntrsize)                   , pointer :: r0
    integer(pntrsize)                   , pointer :: r1
    integer(pntrsize)                   , pointer :: rbnd
    integer(pntrsize)                   , pointer :: rbuff
    integer(pntrsize)                   , pointer :: rho
    integer(pntrsize)                   , pointer :: rlabda
    integer(pntrsize)                   , pointer :: rob
    integer(pntrsize)                   , pointer :: rtur1
    integer(pntrsize)                   , pointer :: s0
    integer(pntrsize)                   , pointer :: s1
    integer(pntrsize)                   , pointer :: sig
    integer(pntrsize)                   , pointer :: teta
    integer(pntrsize)                   , pointer :: thetbc
    integer(pntrsize)                   , pointer :: thick
    integer(pntrsize)                   , pointer :: tp
    integer(pntrsize)                   , pointer :: tpcom
    integer(pntrsize)                   , pointer :: u1
    integer(pntrsize)                   , pointer :: umean
    integer(pntrsize)                   , pointer :: uorb
    integer(pntrsize)                   , pointer :: ubot
    integer(pntrsize)                   , pointer :: ubcom
    integer(pntrsize)                   , pointer :: uvdist
    integer(pntrsize)                   , pointer :: v1
    integer(pntrsize)                   , pointer :: vmean
    integer(pntrsize)                   , pointer :: voldis
    integer(pntrsize)                   , pointer :: wlen
    integer(pntrsize)                   , pointer :: wlcom
    integer(pntrsize)                   , pointer :: wphy
    integer(pntrsize)                   , pointer :: ws
    integer(pntrsize)                   , pointer :: wsu
    integer(pntrsize)                   , pointer :: wsucom
    integer(pntrsize)                   , pointer :: wsv
    integer(pntrsize)                   , pointer :: wsvcom
    integer(pntrsize)                   , pointer :: xcor
    integer(pntrsize)                   , pointer :: xz
    integer(pntrsize)                   , pointer :: ycor
    integer(pntrsize)                   , pointer :: yz
    integer(pntrsize)                   , pointer :: zdist
    integer(pntrsize)                   , pointer :: dzs1
    integer(pntrsize)                   , pointer :: res
    integer(pntrsize)                   , pointer :: rl
    integer(pntrsize)                   , pointer :: xj
    integer(pntrsize)                   , pointer :: guz
    integer(pntrsize)                   , pointer :: gvz
    integer(pntrsize)                   , pointer :: gud
    integer(pntrsize)                   , pointer :: gvd
    integer(pntrsize)                   , pointer :: gsqiu
    integer(pntrsize)                   , pointer :: gsqiv
    integer(pntrsize)                   , pointer :: ibuff
    integer(pntrsize)                   , pointer :: irocol
    integer(pntrsize)                   , pointer :: iroll
    integer(pntrsize)                   , pointer :: itdro
    integer(pntrsize)                   , pointer :: kcs
    integer(pntrsize)                   , pointer :: kcu
    integer(pntrsize)                   , pointer :: kcv
    integer(pntrsize)                   , pointer :: kfs
    integer(pntrsize)                   , pointer :: kfu
    integer(pntrsize)                   , pointer :: kfv
    integer(pntrsize)                   , pointer :: kspu
    integer(pntrsize)                   , pointer :: kspv
    integer(pntrsize)                   , pointer :: kzs
    integer(pntrsize)                   , pointer :: kzu
    integer(pntrsize)                   , pointer :: kzv
    integer(pntrsize)                   , pointer :: mnbnd
    integer(pntrsize)                   , pointer :: mnksrc
    integer(pntrsize)                   , pointer :: kfsmin
    integer(pntrsize)                   , pointer :: kfsmax
    integer(pntrsize)                   , pointer :: izmodl
    integer(pntrsize)                   , pointer :: nambar
    integer(pntrsize)                   , pointer :: nambnd
    integer(pntrsize)                   , pointer :: namcon
    integer(pntrsize)                   , pointer :: namsrc
    character(256)                      , pointer :: restid
    integer                             , pointer :: rtcmod
    integer                             , pointer :: rtcact
    integer                             , pointer :: rtc_domainnr
    character(256)                      , pointer :: sbkConfigFile
    logical                             , pointer :: tstprt
    logical                             , pointer :: sferic
    integer                             , pointer :: ditcof
    integer                             , pointer :: ditcol
    integer                             , pointer :: keva
    integer                             , pointer :: ktemp
    integer                             , pointer :: lturi
    integer                             , pointer :: nfltyp
    integer                             , pointer :: icreep
    integer                             , pointer :: nh_level
    real(fp)                            , pointer :: betac
    real(fp)                            , pointer :: dml
    real(fp)                            , pointer :: grdang
    real(fp)                            , pointer :: saleqs
    real(fp)                            , pointer :: temeqs
    character(1)                        , pointer :: temint
    character(1)                        , pointer :: evaint
    character(1)                        , pointer :: forfuv
    character(1)                        , pointer :: forfww
    character(4)                        , pointer :: rouflo
    character(4)                        , pointer :: rouwav
    character(8)                        , pointer :: method
    character(8)                        , pointer :: dischy
    character(8)                        , pointer :: solver
    character(8)                        , pointer :: disctr
    character(12)                       , pointer :: tkemod
    character(13)                       , pointer :: trasol
    character(19)                       , pointer :: prsmap
    character(21)                       , pointer :: selmap
    character(23)                       , pointer :: prshis
    character(23)                       , pointer :: selhis
    character(36)                       , pointer :: tgfcmp
    integer                             , pointer :: itima         !!  Time to start simulation (N * tscale) according to DELFT3D conventions
    integer                             , pointer :: itlen         !  Description and declaration in esm_alloc_int.f90
    character(256)                      , pointer :: comfil        !!  Communication file name
    character(256)                      , pointer :: runid         !!  Run identification code for the current simulation (used to determine the names of the in- /output files used by the system)
    character(256)                      , pointer :: trifil        !!  File name for TRISULA NEFIS output files (tri"h/m"-"casl""labl".dat/def)
    character(5)                        , pointer :: versio        !!  Version nr. of the current package
    integer                             , pointer :: itcomc        ! Current time counter for the communication file 
    integer                             , pointer :: itcur         ! Current time counter for the communication file, where starting point depend on CYCLIC 
    integer                             , pointer :: itdroc        ! Current time counter for the drogue data file 
    integer                             , pointer :: ithisc        ! Current time counter for the history file 
    integer                             , pointer :: itimc         ! Current time step counter for 2D system 
    integer                             , pointer :: itiwec        ! Current time counter for the calibration of internal wave energy 
    integer                             , pointer :: itmapc        ! Current time counter for the map file 
    integer                             , pointer :: itp           ! Timestep for computation 2D system 
    integer                             , pointer :: itrstc        ! Current time counter for the restart file. Start writing after first interval is passed. Last time will always be written to file for ITRSTI > 0 
    integer                             , pointer :: itwav         ! Current time counter for executation of a wave computation (online coupling with wave)
    integer                             , pointer :: itrw          ! Time to read the wave information in case of online wave coupling
    integer                             , pointer :: iphisc        ! Current time counter for printing history data 
    integer                             , pointer :: maxmn         ! Maximum of MMAX and NMAX 
    integer                             , pointer :: npmap         ! Current array counter for printing map data 
    integer                             , pointer :: ntcur         ! Total number of timesteps on comm. file (to write to) 
    logical                             , pointer :: lrdok         ! Logical to check if reading phase has been passed. 
    real(fp)                            , pointer :: dtsec         ! DT in seconds 
!
! Global variables
!
    type(olvhandle) :: olv_handle
!
! Local variables
!
    integer                                       :: ierror        ! Value is non-zero when an error is encountered
    integer                                       :: istat
    integer                                       :: lunfil
    integer                            , external :: modlen
    integer                            , external :: newlun
    integer                                       :: nst           ! Current time step counter 
    integer                                       :: nst2go        ! Number of timesteps left 
    integer(pntrsize)                  , external :: gtcpnt
    integer(pntrsize)                  , external :: gtipnt
    integer(pntrsize)                  , external :: gtrpnt
    logical                                       :: error         ! Flag=TRUE if an error is encountered 
    real(fp)                                      :: zini
    character(60)                                 :: txtput        ! Text to be print
!
!! executable statements -------------------------------------------------------
!
    iphisc              => gdp%gdtricom%iphisc
    itima               => gdp%gdtricom%itima
    itlen               => gdp%gdtricom%itlen
    comfil              => gdp%gdtricom%comfil
    runid               => gdp%runid
    trifil              => gdp%gdtricom%trifil
    versio              => gdp%gdtricom%versio
    maxmn               => gdp%gdtricom%maxmn
    npmap               => gdp%gdtricom%npmap
    ntcur               => gdp%gdtricom%ntcur
    dtsec               => gdp%gdtricom%dtsec
    itcomc              => gdp%gdtricom%itcomc
    itcur               => gdp%gdtricom%itcur
    itdroc              => gdp%gdtricom%itdroc
    ithisc              => gdp%gdtricom%ithisc
    itimc               => gdp%gdtricom%itimc
    itiwec              => gdp%gdtricom%itiwec
    itmapc              => gdp%gdtricom%itmapc
    itp                 => gdp%gdtricom%itp
    itrstc              => gdp%gdtricom%itrstc
    itwav               => gdp%gdtricom%itwav
    itrw                => gdp%gdtricom%itrw    
    thr                 => gdp%gdbetaro%thr
    ncmax               => gdp%d%ncmax
    nmax                => gdp%d%nmax
    mmax                => gdp%d%mmax
    nlb                 => gdp%d%nlb
    nub                 => gdp%d%nub
    mlb                 => gdp%d%mlb
    mub                 => gdp%d%mub
    ddbound             => gdp%d%ddbound
    nmaxus              => gdp%d%nmaxus
    kmax                => gdp%d%kmax
    nmaxd               => gdp%d%nmaxd
    mmaxd               => gdp%d%mmaxd
    jstart              => gdp%d%jstart
    nmmaxj              => gdp%d%nmmaxj
    nmmax               => gdp%d%nmmax
    lmax                => gdp%d%lmax
    lsal                => gdp%d%lsal
    lsts                => gdp%d%lsts
    lstsc               => gdp%d%lstsc
    lstsci              => gdp%d%lstsci
    lsed                => gdp%d%lsed
    lsedtot             => gdp%d%lsedtot
    lsecfl              => gdp%d%lsecfl
    lsec                => gdp%d%lsec
    ltur                => gdp%d%ltur
    ltur2d              => gdp%d%ltur2d
    noroco              => gdp%d%noroco
    norow               => gdp%d%norow
    nocol               => gdp%d%nocol
    nto                 => gdp%d%nto
    kc                  => gdp%d%kc
    nrob                => gdp%d%nrob
    nsrc                => gdp%d%nsrc
    nostat              => gdp%d%nostat
    ntruv               => gdp%d%ntruv
    ntru                => gdp%d%ntru
    nsluv               => gdp%d%nsluv
    itdate              => gdp%gdexttim%itdate
    tstart              => gdp%gdexttim%tstart
    tstop               => gdp%gdexttim%tstop
    dt                  => gdp%gdexttim%dt
    tunit               => gdp%gdexttim%tunit
    dm                  => gdp%gderosed%dm
    dg                  => gdp%gderosed%dg
    frac                => gdp%gderosed%frac
    cp                  => gdp%gdheat%cp
    sarea               => gdp%gdheat%sarea
    fclou               => gdp%gdheat%fclou
    lunmd               => gdp%gdinout%lunmd
    lundia              => gdp%gdinout%lundia
    lunprt              => gdp%gdinout%lunprt
    lunscr              => gdp%gdinout%lunscr
    timsec              => gdp%gdinttim%timsec
    timmin              => gdp%gdinttim%timmin
    timhr               => gdp%gdinttim%timhr
    itstrt              => gdp%gdinttim%itstrt
    itfinish            => gdp%gdinttim%itfinish
    itlfsm              => gdp%gdinttim%itlfsm
    itmapf              => gdp%gdinttim%itmapf
    itmapi              => gdp%gdinttim%itmapi
    itmapl              => gdp%gdinttim%itmapl
    ithisi              => gdp%gdinttim%ithisi
    itcomf              => gdp%gdinttim%itcomf
    itcomi              => gdp%gdinttim%itcomi
    itcoml              => gdp%gdinttim%itcoml
    itdrof              => gdp%gdinttim%itdrof
    itdroi              => gdp%gdinttim%itdroi
    itrsti              => gdp%gdinttim%itrsti
    iphisf              => gdp%gdinttim%iphisf
    iphisi              => gdp%gdinttim%iphisi
    iphisl              => gdp%gdinttim%iphisl
    ipmap               => gdp%gdinttim%ipmap
    itimtt              => gdp%gdinttim%itimtt
    ittrtu              => gdp%gdinttim%ittrtu
    itiwei              => gdp%gdinttim%itiwei
    itdiag              => gdp%gdinttim%itdiag
    julday              => gdp%gdinttim%julday
    ntstep              => gdp%gdinttim%ntstep
    multi               => gdp%gdmorpar%multi
    mmsyncfilnam        => gdp%gdmorpar%mmsyncfilnam
    rhow                => gdp%gdphysco%rhow
    iro                 => gdp%gdphysco%iro
    wind                => gdp%gdprocs%wind
    waveol              => gdp%gdprocs%waveol
    mudlay              => gdp%gdprocs%mudlay
    mudwave             => gdp%gdprocs%mudwave
    coupleact           => gdp%gdprocs%coupleact
    couplemod           => gdp%gdprocs%couplemod
    zmodel              => gdp%gdprocs%zmodel
    nonhyd              => gdp%gdprocs%nonhyd
    roller              => gdp%gdprocs%roller
    wavcmp              => gdp%gdprocs%wavcmp
    cnstwv              => gdp%gdprocs%cnstwv
    lftrto              => gdp%gdprocs%lftrto
    sbkol               => gdp%gdprocs%sbkol
    numdomains          => gdp%gdprognm%numdomains
    nummappers          => gdp%gdprognm%nummappers
    alfas               => gdp%gdr_i_ch%alfas
    ampbc               => gdp%gdr_i_ch%ampbc
    c                   => gdp%gdr_i_ch%c
    cgc                 => gdp%gdr_i_ch%cgc
    ctr                 => gdp%gdr_i_ch%ctr
    dircom              => gdp%gdr_i_ch%dircom
    dircos              => gdp%gdr_i_ch%dircos
    dirsin              => gdp%gdr_i_ch%dirsin
    dis                 => gdp%gdr_i_ch%dis
    disch               => gdp%gdr_i_ch%disch
    discom              => gdp%gdr_i_ch%discom
    dpd                 => gdp%gdr_i_ch%dpd
    dpc                 => gdp%gdr_i_ch%dpc
    dps                 => gdp%gdr_i_ch%dps
    dpu                 => gdp%gdr_i_ch%dpu
    dpv                 => gdp%gdr_i_ch%dpv
    ewabr0              => gdp%gdr_i_ch%ewabr0
    ewabr1              => gdp%gdr_i_ch%ewabr1
    ewave0              => gdp%gdr_i_ch%ewave0
    ewave1              => gdp%gdr_i_ch%ewave1
    grmasu              => gdp%gdr_i_ch%grmasu
    grmasv              => gdp%gdr_i_ch%grmasv
    gro                 => gdp%gdr_i_ch%gro
    gsqd                => gdp%gdr_i_ch%gsqd
    gsqs                => gdp%gdr_i_ch%gsqs
    guu                 => gdp%gdr_i_ch%guu
    guv                 => gdp%gdr_i_ch%guv
    gvu                 => gdp%gdr_i_ch%gvu
    gvv                 => gdp%gdr_i_ch%gvv
    hkru                => gdp%gdr_i_ch%hkru
    hkrv                => gdp%gdr_i_ch%hkrv
    hrmcom              => gdp%gdr_i_ch%hrmcom
    hrms                => gdp%gdr_i_ch%hrms
    hu                  => gdp%gdr_i_ch%hu
    huvw                => gdp%gdr_i_ch%huvw
    hv                  => gdp%gdr_i_ch%hv
    msucom              => gdp%gdr_i_ch%msucom
    msvcom              => gdp%gdr_i_ch%msvcom
    ombc                => gdp%gdr_i_ch%ombc
    phibc               => gdp%gdr_i_ch%phibc
    qu                  => gdp%gdr_i_ch%qu
    qxk                 => gdp%gdr_i_ch%qxk
    qxkr                => gdp%gdr_i_ch%qxkr
    qxkw                => gdp%gdr_i_ch%qxkw
    qyk                 => gdp%gdr_i_ch%qyk
    qykr                => gdp%gdr_i_ch%qykr
    qykw                => gdp%gdr_i_ch%qykw
    r0                  => gdp%gdr_i_ch%r0
    r1                  => gdp%gdr_i_ch%r1
    rbnd                => gdp%gdr_i_ch%rbnd
    rbuff               => gdp%gdr_i_ch%rbuff
    rho                 => gdp%gdr_i_ch%rho
    rlabda              => gdp%gdr_i_ch%rlabda
    rob                 => gdp%gdr_i_ch%rob
    rtur1               => gdp%gdr_i_ch%rtur1
    s0                  => gdp%gdr_i_ch%s0
    s1                  => gdp%gdr_i_ch%s1
    sig                 => gdp%gdr_i_ch%sig
    teta                => gdp%gdr_i_ch%teta
    thetbc              => gdp%gdr_i_ch%thetbc
    thick               => gdp%gdr_i_ch%thick
    tp                  => gdp%gdr_i_ch%tp
    tpcom               => gdp%gdr_i_ch%tpcom
    u1                  => gdp%gdr_i_ch%u1
    umean               => gdp%gdr_i_ch%umean
    uorb                => gdp%gdr_i_ch%uorb
    ubot                => gdp%gdr_i_ch%ubot
    ubcom               => gdp%gdr_i_ch%ubcom
    uvdist              => gdp%gdr_i_ch%uvdist
    v1                  => gdp%gdr_i_ch%v1
    vmean               => gdp%gdr_i_ch%vmean
    voldis              => gdp%gdr_i_ch%voldis
    wlen                => gdp%gdr_i_ch%wlen
    wlcom               => gdp%gdr_i_ch%wlcom
    wphy                => gdp%gdr_i_ch%wphy
    ws                  => gdp%gdr_i_ch%ws
    wsu                 => gdp%gdr_i_ch%wsu
    wsucom              => gdp%gdr_i_ch%wsucom
    wsv                 => gdp%gdr_i_ch%wsv
    wsvcom              => gdp%gdr_i_ch%wsvcom
    xcor                => gdp%gdr_i_ch%xcor
    xz                  => gdp%gdr_i_ch%xz
    ycor                => gdp%gdr_i_ch%ycor
    yz                  => gdp%gdr_i_ch%yz
    zdist               => gdp%gdr_i_ch%zdist
    dzs1                => gdp%gdr_i_ch%dzs1
    res                 => gdp%gdr_i_ch%res
    rl                  => gdp%gdr_i_ch%rl
    xj                  => gdp%gdr_i_ch%xj
    guz                 => gdp%gdr_i_ch%guz
    gvz                 => gdp%gdr_i_ch%gvz
    gud                 => gdp%gdr_i_ch%gud
    gvd                 => gdp%gdr_i_ch%gvd
    gsqiu               => gdp%gdr_i_ch%gsqiu
    gsqiv               => gdp%gdr_i_ch%gsqiv
    ibuff               => gdp%gdr_i_ch%ibuff
    irocol              => gdp%gdr_i_ch%irocol
    iroll               => gdp%gdr_i_ch%iroll
    itdro               => gdp%gdr_i_ch%itdro
    kcs                 => gdp%gdr_i_ch%kcs
    kcu                 => gdp%gdr_i_ch%kcu
    kcv                 => gdp%gdr_i_ch%kcv
    kfs                 => gdp%gdr_i_ch%kfs
    kfu                 => gdp%gdr_i_ch%kfu
    kfv                 => gdp%gdr_i_ch%kfv
    kspu                => gdp%gdr_i_ch%kspu
    kspv                => gdp%gdr_i_ch%kspv
    kzs                 => gdp%gdr_i_ch%kzs
    kzu                 => gdp%gdr_i_ch%kzu
    kzv                 => gdp%gdr_i_ch%kzv
    mnbnd               => gdp%gdr_i_ch%mnbnd
    mnksrc              => gdp%gdr_i_ch%mnksrc
    kfsmin              => gdp%gdr_i_ch%kfsmin
    kfsmax              => gdp%gdr_i_ch%kfsmax
    izmodl              => gdp%gdr_i_ch%izmodl
    nambar              => gdp%gdr_i_ch%nambar
    nambnd              => gdp%gdr_i_ch%nambnd
    namcon              => gdp%gdr_i_ch%namcon
    namsrc              => gdp%gdr_i_ch%namsrc
    rtcmod              => gdp%gdrtc%rtcmod
    rtcact              => gdp%gdrtc%rtcact
    rtc_domainnr        => gdp%gdrtc%rtc_domainnr
    restid              => gdp%gdrestart%restid
    sbkConfigFile       => gdp%gdsobek%sbkConfigFile
    tstprt              => gdp%gdtricom%tstprt
    sferic              => gdp%gdtricom%sferic
    ditcof              => gdp%gdtricom%ditcof
    ditcol              => gdp%gdtricom%ditcol
    keva                => gdp%gdtricom%keva
    ktemp               => gdp%gdtricom%ktemp
    lturi               => gdp%gdtricom%lturi
    nfltyp              => gdp%gdtricom%nfltyp
    icreep              => gdp%gdtricom%icreep
    nh_level            => gdp%gdnonhyd%nh_level
    betac               => gdp%gdtricom%betac
    dml                 => gdp%gdtricom%dml
    grdang              => gdp%gdtricom%grdang
    saleqs              => gdp%gdtricom%saleqs
    temeqs              => gdp%gdtricom%temeqs
    temint              => gdp%gdtricom%temint
    evaint              => gdp%gdtricom%evaint
    forfuv              => gdp%gdtricom%forfuv
    forfww              => gdp%gdtricom%forfww
    rouflo              => gdp%gdtricom%rouflo
    rouwav              => gdp%gdtricom%rouwav
    method              => gdp%gdtricom%method
    dischy              => gdp%gdtricom%dischy
    solver              => gdp%gdtricom%solver
    disctr              => gdp%gdtricom%disctr
    tkemod              => gdp%gdtricom%tkemod
    trasol              => gdp%gdtricom%trasol
    prsmap              => gdp%gdtricom%prsmap
    selmap              => gdp%gdtricom%selmap
    prshis              => gdp%gdtricom%prshis
    selhis              => gdp%gdtricom%selhis
    tgfcmp              => gdp%gdtricom%tgfcmp
    lrdok               => gdp%gdtricom%lrdok
    !
    call timer_start(timer_close, gdp)
    !
    error = .false.
    !
    ! Finish using a semaphore
    ! Related vsemnefis is in trisim.F90
    ! This used to be a psemfinish
    !
    call psemfinish
    !
    if (error) goto 9999
    !
    ! End of synchronisation point 3
    ! ==============================
    !
    ! Test routine print at last time step:
    !
    if (tstprt) then
       call pppsol(mmax      ,nmax      ,kmax      ,lstsci    ,ltur      , &
                 & runid     ,i(kcu)    ,i(kcv)    ,i(kcs)    ,i(kfu)    , &
                 & i(kfv)    ,i(kfs)    ,r(u1)     ,r(v1)     ,r(s1)     , &
                 & r(r1)     ,r(rtur1)  ,d(dps)    ,gdp       )
    endif
    !
    ! Output data of last time step to file if required
    !
    nst    = itfinish
    nst2go = 0
    itimc  = modlen(itfinish*itp, itlen)
    !
    ! Write (conditionaly) updated bathymetry to com-file
    !
    if (lsed > 0) then
       call rwbotc(comfil    ,lundia    ,error     ,itima     , &
                 & itcomi    ,mmax      ,nmax      ,nmaxus    ,r(dpd)    , &
                 & r(rbuff)  ,gdp       )
       if (error) goto 9999
    endif
    !
    ! Calculate post processing info and write to files if required
    !
    call postpr(lundia    ,lunprt    ,error     ,versio    ,comfil    , &
              & trifil    ,runid     ,prsmap    ,prshis    ,selmap    , &
              & selhis    ,rhow      ,grdang    ,dtsec     , &
              & nst       ,iphisc    ,npmap     ,itcomc    ,itimc     , &
              & itcur     ,ntcur     ,ithisc    ,itmapc    ,itdroc    , &
              & itrstc    ,ktemp     ,.false.   ,gdp       )
    if (waveol==2) then
       !
       ! Perform (last) wave computation (if required) and close communication with
       ! wave module
       !
       if (nst == itwav) then
          if (prec == hp) then
             call rwbotc_double(comfil    ,lundia    ,error     ,itima     , &
                              & itcomi    ,mmax      ,nmax      ,nmaxus    ,d(dps)    , &
                              & r(rbuff)  ,gdp       )
          else
             call rwbotc(comfil    ,lundia    ,error     ,itima     , &
                       & itcomi    ,mmax      ,nmax      ,nmaxus    ,d(dps)    , &
                       & r(rbuff)  ,gdp       )
          endif
          call timer_start(timer_wait, gdp)
          ierror = flow_to_wave_command(flow_wave_comm_finalize, &
                                        & numdomains, mudlay, nst)
          call timer_stop(timer_wait, gdp)
       else
          call timer_start(timer_wait, gdp)
          ierror = flow_to_wave_command(flow_wave_comm_finalize, &
                                        & numdomains, mudlay, -1)
          call timer_stop(timer_wait, gdp)
       endif
       if (ierror /= 0) then
          txtput = 'Delftio command to waves failed'
          write(lunscr,'(a)') trim(txtput)
          call prterr(lundia    ,'P004'    ,trim(txtput)    )
          call d3stop(1, gdp)
       endif
    endif
    if (mudwave) then
       !
       ! Perform (last) wave computation (if required) and close communication with
       ! wave module
       !
       call timer_start(timer_wait, gdp)
       if (nst == itwav) then
          ierror = flow_to_wave_command(flow_wave_comm_finalize, &
                                        & numdomains, mudlay, nst)
       else
          ierror = flow_to_wave_command(flow_wave_comm_finalize, &
                                        & numdomains, mudlay, -1)
       endif
       call timer_stop(timer_wait, gdp)
       if (ierror /= 0) then
          txtput = 'Delftio command to waves failed'
          write(lunscr,'(a)') trim(txtput)
          call prterr(lundia    ,'P004'    ,trim(txtput)    )
          call d3stop(1, gdp)
       endif
    endif
    !
 9999 continue
    if (error) then
       !
       ! Check if reading phase was passed
       !
       if (.not. lrdok) then
          write(*,*) 'error occured during reading phase, see tri-diag file for details.'
          !
          ! Send message to RTC and Couple to shutdown
          !
          if (rtcmod == dataFromRTCToFLOW .and. rtc_domainnr == 1) then
             write(*,*) 'Sending quit signal to RTC ...'
             call timer_start(timer_wait, gdp)
             call syncflowrtc_quit
             call timer_stop(timer_wait, gdp)
             write(*,*) '... continue'
          endif
          if (couplemod) then
             write(*,*) 'Sending quit signal to COUPLE ...'
             call timer_start(timer_wait, gdp)
             call syncflowcouple_quit(gdp%runid   , gdp%gdcoup%flowtocouple, &
                                    & gdp%gdcoup%pars    , gdp%gdcoup%locs        , &
                                    & gdp%gdcoup%putvalue)
             call timer_stop(timer_wait, gdp)
             write(*,*) '... continue'
          endif
       endif
       gdp%errorcode = 2
    endif
    !
    ! Close Communication with RTC if active after normal end
    !
    if (rtcact==RTCmodule .and. rtcmod==dataFromRTCToFLOW .and. rtc_domainnr==1) then
       write(*,*) 'Sending close signal to  RTC ...'
       call timer_start(timer_wait, gdp)
       call syncflowrtc_close
       call timer_stop(timer_wait, gdp)
       write(*,*) '... continue'
       rtcact = noRTC
    endif
    !
    ! Close Communication with Couple if active after normal end    
    !
    if (couplemod .and. coupleact) then
       write(*,*) 'Sending close signal to  COUPLE ...'
       call timer_start(timer_wait, gdp)
       call syncflowcouple_close(gdp%gdcoup%flowtocouple, gdp%gdcoup%coupletoflow)
       call timer_stop(timer_wait, gdp)
       write(*,*) '... continue'
       coupleact = .false.
    endif
    !
    ! Close Communication with Sobek if active after normal end
    !
    if (sbkol) then
       write(*,*) 'Sending finalize signal to  Sobek ...'
       call timer_start(timer_wait, gdp)
       call D3S_Finalize('D3D-FLOW', 'Sobek')
       call timer_stop(timer_wait, gdp)
       write(*,*) '... continue'
    endif
    !
    ! Close Communication with delftonline
    !
    call setEndFlag(olv_handle, 1) !Tells the DOL client that the simulation has ended by passing an exception
    call free_olv(olv_handle)
    !
    ! Mormerge synchronisation
    !
    if (multi) then
       open (newunit=lunfil, file=mmsyncfilnam, position='append', action='write', iostat=istat)
       if (istat /= 0) then
          write(*,*)' *** WARNING: unable to write in file ',trim(mmsyncfilnam)
       else
          write(lunfil,'(a)') 'Finished'
          close(lunfil)
       endif
    endif
    !
    call timer_stop(timer_close, gdp)
    call timer_stop(timer_tricomtot, gdp)
end subroutine tricom_finish
