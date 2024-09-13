subroutine tricom_step(olv_handle, gdp)
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
!  $Id: tricom_step.F90 6133 2016-05-13 14:53:11Z platzek $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/manager/src/tricom_step.F90 $
!!--description-----------------------------------------------------------------
!
!    Function: 
!              - Perform a Delft3D-FLOW computation.
!                If WAVE = TRUE then ITLEN and TSCALE
!                will be read from the communication
!                file, else they will also be set 0.
!              - Update the communication file
!              NOTE: TRICOM called by main module of DELFT3D then
!                    ITLEN and TSCALE always defined
!                    TRICOM called by module TRISIM then ITLEN=0
!                    and ITLEN can be read from comm. file or are
!                    defined by the trisula time frame
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
    use D3DOnline
    use D3DPublish
    use D3D_Sobek 
    use globaldata
    use dfparall
    use d3d_olv_class
    !
    implicit none
    !
    type(globdat),target :: gdp
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
    integer                             , pointer :: itinit
    integer                             , pointer :: itstop
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
    integer                             , pointer :: itnflf
    integer                             , pointer :: itnfli
    integer                             , pointer :: itnfll
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
    real(fp)                            , pointer :: tmor
    real(fp)                            , pointer :: rdc
    integer                             , pointer :: itmor
    type (bedbndtype) , dimension(:)    , pointer :: morbnd
    logical                             , pointer :: densin
    logical                             , pointer :: multi
    character(256)                      , pointer :: mmsyncfilnam
    real(fp)                            , pointer :: hdt
    character(6)                        , pointer :: momsol
    real(fp)                            , pointer :: rhow
    real(fp)                            , pointer :: ag
    integer                             , pointer :: iro
    logical                             , pointer :: wind
    logical                             , pointer :: temp
    logical                             , pointer :: const
    logical                             , pointer :: culvert
    logical                             , pointer :: dredge
    logical                             , pointer :: drogue
    logical                             , pointer :: wave
    logical                             , pointer :: waveol
    logical                             , pointer :: threed
    logical                             , pointer :: secflo
    logical                             , pointer :: struct
    logical                             , pointer :: sedim
    logical                             , pointer :: htur2d
    logical                             , pointer :: flmd2l
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
    logical                             , pointer :: snelli
    logical                             , pointer :: sbkol
    logical                             , pointer :: xbeach
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
    integer(pntrsize)                   , pointer :: dp
    integer(pntrsize)                   , pointer :: dpc
    integer(pntrsize)                   , pointer :: dps
    integer(pntrsize)                   , pointer :: dpu
    integer(pntrsize)                   , pointer :: dpv
    integer(pntrsize)                   , pointer :: ewabr0
    integer(pntrsize)                   , pointer :: ewabr1
    integer(pntrsize)                   , pointer :: ewave0
    integer(pntrsize)                   , pointer :: ewave1
    integer(pntrsize)                   , pointer :: fxw
    integer(pntrsize)                   , pointer :: fyw
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
    integer(pntrsize)                   , pointer :: sbuu
    integer(pntrsize)                   , pointer :: sbvv
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
    integer(pntrsize)                   , pointer :: volum1
    integer(pntrsize)                   , pointer :: wlen
    integer(pntrsize)                   , pointer :: wlcom
    integer(pntrsize)                   , pointer :: wphy
    integer(pntrsize)                   , pointer :: ws
    integer(pntrsize)                   , pointer :: wsu
    integer(pntrsize)                   , pointer :: wsucom
    integer(pntrsize)                   , pointer :: wsv
    integer(pntrsize)                   , pointer :: wsvcom
    integer(pntrsize)                   , pointer :: wsbodyu
    integer(pntrsize)                   , pointer :: wsbodyucom
    integer(pntrsize)                   , pointer :: wsbodyv
    integer(pntrsize)                   , pointer :: wsbodyvcom
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
    logical                             , pointer :: rtcact
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
    integer                             , pointer :: itlen   !  Description and declaration in esm_alloc_int.f90
    character(256)                      , pointer :: comfil  !!  Communication file name
    character(256)                      , pointer :: runid   !!  Run identification code for the current simulation (used to determine the names of the in- /output files used by the system)
    character(256)                      , pointer :: trifil  !!  File name for TRISULA NEFIS output files (tri"h/m"-"casl""labl".dat/def)
    character(5)                        , pointer :: versio  !!  Version nr. of the current package
    integer                             , pointer :: initi   ! Control parameter 
    integer                             , pointer :: iphisc  ! Current time counter for printing history data 
    integer                             , pointer :: itcomc  ! Current time counter for the communication file 
    integer                             , pointer :: itcur   ! Current time counter for the communication file, where starting point depend on CYCLIC 
    integer                             , pointer :: itdroc  ! Current time counter for the drogue data file 
    integer                             , pointer :: ithisc  ! Current time counter for the history file 
    integer                             , pointer :: itimc   ! Current time step counter for 2D system 
    integer                             , pointer :: itiwec  ! Current time counter for the calibration of internal wave energy 
    integer                             , pointer :: itmapc  ! Current time counter for the map file 
    integer                             , pointer :: itp     ! Timestep for computation 2D system 
    integer                             , pointer :: itrstc  ! Current time counter for the restart file. Start writing after first interval is passed. Last time will always be written to file for ITRSTI > 0 
    integer                             , pointer :: itwav   ! Current time counter for executation of a wave computation (online coupling with wave)
    integer                             , pointer :: itrw    ! Time to read the wave information in case of online wave coupling
    integer                             , pointer :: maxmn   ! Maximum of MMAX and NMAX 
    integer                             , pointer :: npmap   ! Current array counter for printing map data 
    integer                             , pointer :: ntcur   ! Total number of timesteps on comm. file (to write to) 
    integer                             , pointer :: ntwav   ! Total number of timesteps on comm. file (to read from) for waves 
    integer              , dimension(:) , pointer :: timwav  ! Array with time steps on comm. file for wave results 
    logical                             , pointer :: waverd  ! Flag = TRUE if wave process and communication file exist 
    real(fp)                            , pointer :: anglat  ! Angle of latitude of the model centre (used to determine the coeff. for the coriolis force) 
    real(fp)                            , pointer :: anglon  ! Angle of longitude of the model centre (used to determine solar radiation) 
    real(fp)                            , pointer :: dtsec   ! DT in seconds 
    real(fp)                            , pointer :: timnow  ! Current timestep (multiples of dt)  = number of time steps since itdate, 00:00:00 hours
!
! Local parameters
!
    integer, parameter :: maxtim = 1500
    integer, pointer :: cutcell
    integer, pointer :: GhostMethod
!
! Global variables: NONE
!
! Local variables
!

  
    integer                                       :: ierror        ! Value is non-zero when an error is encountered
    integer                                       :: istat
    integer                                       :: iofset        ! Shift of inner part of matrix to remove strips
    integer                                       :: lunfil
    integer                            , external :: modlen
    integer                            , external :: newlun
    integer                                       :: nhystp
    integer                                       :: nst           ! Current time step counter 
    integer                                       :: nst2go        ! Number of timesteps left 
    integer          , dimension(2)               :: ifcore        ! Time indices (cell id's) of the wave functions which are in core available 
    integer(pntrsize)                  , external :: gtcpnt
    integer(pntrsize)                  , external :: gtipnt
    integer(pntrsize)                  , external :: gtrpnt
    logical                                       :: error         ! Flag=TRUE if an error is encountered 
    logical                                       :: ex            ! Help flag = TRUE when file is found 
    real(fp)                                      :: zini
    character(60)                                 :: txtput        ! Text to be print
    type(olvhandle)                               :: olv_handle
!
!! executable statements -------------------------------------------------------
!
    cutcell     => gdp%gdimbound%cutcell
    GhostMethod => gdp%gdimbound%GhostMethod
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
    timnow              => gdp%gdinttim%timnow
    timmin              => gdp%gdinttim%timmin
    timhr               => gdp%gdinttim%timhr
    itstrt              => gdp%gdinttim%itstrt
    itinit              => gdp%gdinttim%itinit
    itstop              => gdp%gdinttim%itstop
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
    itnflf              => gdp%gdinttim%itnflf
    itnfli              => gdp%gdinttim%itnfli
    itnfll              => gdp%gdinttim%itnfll
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
    tmor                => gdp%gdmorpar%tmor
    rdc                 => gdp%gdmorpar%rdc
    itmor               => gdp%gdmorpar%itmor
    morbnd              => gdp%gdmorpar%morbnd
    densin              => gdp%gdmorpar%densin
    multi               => gdp%gdmorpar%multi
    mmsyncfilnam        => gdp%gdmorpar%mmsyncfilnam
    nh_level            => gdp%gdnonhyd%nh_level
    hdt                 => gdp%gdnumeco%hdt
    momsol              => gdp%gdnumeco%momsol
    rhow                => gdp%gdphysco%rhow
    ag                  => gdp%gdphysco%ag
    iro                 => gdp%gdphysco%iro
    wind                => gdp%gdprocs%wind
    temp                => gdp%gdprocs%temp
    const               => gdp%gdprocs%const
    culvert             => gdp%gdprocs%culvert
    dredge              => gdp%gdprocs%dredge
    drogue              => gdp%gdprocs%drogue
    wave                => gdp%gdprocs%wave
    waveol              => gdp%gdprocs%waveol
    threed              => gdp%gdprocs%threed
    secflo              => gdp%gdprocs%secflo
    struct              => gdp%gdprocs%struct
    sedim               => gdp%gdprocs%sedim
    htur2d              => gdp%gdprocs%htur2d
    flmd2l              => gdp%gdprocs%flmd2l
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
    snelli              => gdp%gdprocs%snelli
    sbkol               => gdp%gdprocs%sbkol
    xbeach              => gdp%gdprocs%xbeach
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
    dp                  => gdp%gdr_i_ch%dp
    dpc                 => gdp%gdr_i_ch%dpc
    dps                 => gdp%gdr_i_ch%dps
    dpu                 => gdp%gdr_i_ch%dpu
    dpv                 => gdp%gdr_i_ch%dpv
    ewabr0              => gdp%gdr_i_ch%ewabr0
    ewabr1              => gdp%gdr_i_ch%ewabr1
    ewave0              => gdp%gdr_i_ch%ewave0
    ewave1              => gdp%gdr_i_ch%ewave1
    fxw                 => gdp%gdr_i_ch%fxw
    fyw                 => gdp%gdr_i_ch%fyw
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
    sbuu                => gdp%gdr_i_ch%sbuu
    sbvv                => gdp%gdr_i_ch%sbvv
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
    volum1              => gdp%gdr_i_ch%volum1
    wlen                => gdp%gdr_i_ch%wlen
    wlcom               => gdp%gdr_i_ch%wlcom
    wphy                => gdp%gdr_i_ch%wphy
    ws                  => gdp%gdr_i_ch%ws
    wsu                 => gdp%gdr_i_ch%wsu
    wsucom              => gdp%gdr_i_ch%wsucom
    wsv                 => gdp%gdr_i_ch%wsv
    wsvcom              => gdp%gdr_i_ch%wsvcom
    wsbodyu             => gdp%gdr_i_ch%wsbodyu
    wsbodyucom          => gdp%gdr_i_ch%wsbodyucom
    wsbodyv             => gdp%gdr_i_ch%wsbodyv
    wsbodyvcom          => gdp%gdr_i_ch%wsbodyvcom
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
    itlen               => gdp%gdtricom%itlen
    comfil              => gdp%gdtricom%comfil
    runid               => gdp%runid
    trifil              => gdp%gdtricom%trifil
    versio              => gdp%gdtricom%versio
    initi               => gdp%gdtricom%initi
    iphisc              => gdp%gdtricom%iphisc
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
    maxmn               => gdp%gdtricom%maxmn
    npmap               => gdp%gdtricom%npmap
    ntcur               => gdp%gdtricom%ntcur
    ntwav               => gdp%gdtricom%ntwav    
    timwav              => gdp%gdtricom%timwav
    waverd              => gdp%gdtricom%waverd
    anglat              => gdp%gdtricom%anglat
    anglon              => gdp%gdtricom%anglon
    dtsec               => gdp%gdtricom%dtsec
    !  
    call timer_start(timer_simulation, gdp)
    !
    error = .false.
    ifcore(1) = 0
    ifcore(2) = 0
    !
    ! Simulation time loop
    !
    call setRunningFlag(olv_handle, 0, itstrt)    !status is: started

    do nst = itstrt, itstop - 1, 1
       call timer_start(timer_timeintegr, gdp)
       !
       nst2go = itfinish - nst   !note: we want the steps for the entire simulation
       ntstep = ntstep + 1
       !
       call timer_start(timer_step2screen, gdp)
       call psemnefis
       if (.not.parll .or. (parll .and. inode == master)) then
          call step_to_screen(nst2go, itinit, itfinish, nst, gdp)
       endif
       call vsemnefis
       call timer_stop(timer_step2screen, gdp)

       call FLOWOL_Timestep (nst)

       !
       ! Status is: simulation is running / iteration
       !
       call setRunningFlag(olv_handle, 1, nst)
       !
       ! Set timsec and current date and time.
       !
       itimc  = modlen(nst*itp, itlen)
       itwav  = itcomc
       !
       ! Mormerge synchronisation
       !
       if (multi) then
          if (mod(nst2go,5) == 0) then
             call timer_start(timer_tricom_rest, gdp)
             lunfil = newlun(gdp)
             open (lunfil, file=mmsyncfilnam, position='append', action='write', iostat=istat)
             if (istat /= 0) then
                write(*,*)' *** WARNING: unable to write in file ',trim(mmsyncfilnam)
             else
                write(lunfil,'(i0)') nst2go
                close(lunfil)
             endif
             call timer_stop(timer_tricom_rest, gdp)
          endif
       endif
       !
       ! Calculate post processing info and write to files if required.
       !
       ! WARNING: The following semaphore and the next semaphore MUST be of the same semaphore type
       ! Otherwise, DD with WAVE online will crash: semaphores are NOT synchronisation points!
       !
       call psemnefis
       call timer_start(timer_postpr, gdp)
      ! if (cutcell.gt.0.and.GhostMethod.ge.1) call kfsuv_ghost(r(Umean),r(Vmean),r(qxk),r(qyk),r(hu),r(hv),r(dpu),r(dpv),r(gsqs),i(kfs),i(kfu),i(kfv),i(kcs),i(kcu),i(kcv),r(s1),r(u1),r(v1),r(s1),r(u1),r(v1),d(dps),mmax,nmax,kmax,nmaxus,1,0,nlb,nub,mlb,mub,nmlb,nmub) !set kfs,kfu,kfv active in ghost points     
       call postpr(lundia    ,lunprt    ,error     ,versio    ,comfil    , &
                 & trifil    ,runid     ,prsmap    ,prshis    ,selmap    , &
                 & selhis    ,rhow      ,grdang    ,initi     ,dtsec     , &
                 & nst       ,iphisc    ,npmap     ,itcomc    ,itimc     , &
                 & itcur     ,ntcur     ,ithisc    ,itmapc    ,itdroc    , &
                 & itrstc    ,ktemp     ,.false.   ,gdp       )
    !   if (cutcell.gt.0.and.GhostMethod.ge.1) call kfsuv_ghost(r(Umean),r(Vmean),r(qxk),r(qyk),r(hu),r(hv),r(dpu),r(dpv),r(gsqs),i(kfs),i(kfu),i(kfv),i(kcs),i(kcu),i(kcv),r(s1),r(u1),r(v1),r(s1),r(u1),r(v1),d(dps),mmax,nmax,kmax,nmaxus,0,0,nlb,nub,mlb,mub,nmlb,nmub) !set kfs,kfu,kfv active in ghost points     
       call timer_stop(timer_postpr, gdp)
       call vsemnefis
       if (error) goto 9998
       !
       ! WARNING: The following semaphore and the previous semaphore MUST be of the same semaphore type
       ! Otherwise, DD with WAVE online will crash: semaphores are NOT synchronisation points!
       !
       call psemnefis
       if (wave .and. xbeach) then
          ! call xbeach_in_delft3d(r(xz+iofset),r(yz+iofset), &
          !    &              r(dps+iofset),r(s1+iofset),r(u1+iofset),r(v1+iofset),     &
          !                   r(uorb+iofset),r(tp+iofset),r(teta+iofset),r(dis+iofset), &
          !                   r(wsu+iofset),r(wsv+iofset),r(fxw+iofset),r(fyw+iofset),    &
          !                   r(grmasu+iofset),r(grmasv+iofset),                        &
          !                   r(hrms+iofset),r(rlabda+iofset),                            &
          !                    mmax,nmax,nmaxus,2*timnow*hdt)
       endif
       if (wave .and. waveol .and. (.not.xbeach)) then
          !
          ! Command to wave module to execute wave computation on
          ! times for writing to com file
          !
          if (nst == itwav) then
             call timer_start(timer_tricom_rest, gdp)
             if (prec == hp) then
                call rwbotc_double(comfil    ,lundia    ,error     ,initi     ,nst     , &
                                 & itcomi    ,mmax      ,nmax      ,nmaxus    ,d(dps)  , &
                                 & r(rbuff)  ,gdp       )
             else
                call rwbotc(comfil    ,lundia    ,error     ,initi     ,nst     , &
                          & itcomi    ,mmax      ,nmax      ,nmaxus    ,d(dps)  , &
                          & r(rbuff)  ,gdp       )
             endif
             call timer_stop(timer_tricom_rest, gdp)
             !
             ! Wave calculation is only executed after updating the comm-files
             ! of all sub domains.
             ! That's why flow_to_wave_command needs numdomains.
             !
             call timer_start(timer_wait, gdp)
             if (.not.parll .or. (parll .and. inode == master)) then
                ierror = flow_to_wave_command(flow_wave_comm_perform_step, &
                                             & numdomains, mudlay, nst)
             else
                ierror = 0
             endif
             call dfreduce_gdp( ierror, 1, dfint, dfmax, gdp )
             call timer_stop(timer_wait, gdp)
             if (ierror /= 0) then
                txtput = 'Delftio command to waves failed'
                write(lunscr,'(a)') trim(txtput)
                call prterr(lundia    ,'P004'    ,trim(txtput)    )
                call d3stop(1, gdp)
             endif
             itrw = nst + 1
          endif
          if (nst == itrw) then
             call rdtimw(comfil    ,lundia    ,error     ,ntwav     ,timwav    , &
                       & maxtim    ,waverd    ,nmaxus    ,mmax      ,gdp       )
             waverd = .true.
             ifcore(1) = 0
             ifcore(2) = 0
          else
             waverd = .false.
          endif
       endif
       if (mudwave) then
          !
          ! Command to wave module to execute wave computation on
          ! times for writing to com file
          !
          if (nst == itwav) then
             !
             ! Wave calculation is only executed after updating the comm-files
             ! of all sub domains.
             ! That's why flow_to_wave_command needs numdomains.
             !
             call timer_start(timer_wait, gdp)
             ierror = flow_to_wave_command(flow_wave_comm_perform_step, &
                                          & numdomains, mudlay, nst)
             call timer_stop(timer_wait, gdp)
             if (ierror /= 0) then
                txtput = 'Delftio command to waves failed'
                write(lunscr,'(a)') trim(txtput)
                call prterr(lundia    ,'P004'    ,trim(txtput)    )
                call d3stop(1, gdp)
             endif
          endif
       endif
       !
       ! Read and interpolate the wave information
       ! some array names are different in wave group on comm. file
       ! DIR := teta  , DISS := dis   , FX := wsu, FY := wsv,
       ! MX  := grmasu, MY   := grmasv
       !
       if (waverd) then
          call timer_start(timer_tricom_rest, gdp)
          call setwav(comfil    ,lundia     ,error      ,mmax          ,nmax          , &
                    & nmaxus    ,itimc      ,ntwav      ,itlen         ,timwav        , &
                    & norow     ,noroco     ,i(irocol)  ,ifcore        ,d(dps)        , &
                    & r(s1)     ,r(uorb)    ,r(tp)      ,r(teta)       ,r(dis)        , &
                    & r(wsu)    ,r(wsv)     ,r(grmasu)  ,r(grmasv)     ,r(hrms)       , &
                    & r(ubot)   ,r(wlen)    ,r(hrmcom)  ,r(tpcom)      , &
                    & r(dircom) ,r(discom)  ,r(wsucom)  ,r(wsvcom)     ,r(msucom)     , &
                    & r(msvcom) ,r(ubcom)   ,r(wlcom)   ,r(rlabda)     , &
                    & r(dircos) ,r(dirsin)  ,r(ewave1)  ,roller        ,wavcmp        , &
                    & r(ewabr1) ,r(wsbodyu) ,r(wsbodyv) ,r(wsbodyucom) ,r(wsbodyvcom) , &
                    & gdp       )
          call timer_stop(timer_tricom_rest, gdp)
          if (error) goto 9998
       endif
       call vsemnefis
       if (roller) then
          call timer_start(timer_tricom_rest, gdp)
          if (snelli) then
             call snel(mmax      ,nmax      ,norow     ,noroco   ,r(ubot)  , &
                     & i(irocol) ,d(dps)    ,r(s1)     ,r(alfas) ,r(uorb)  , &
                     & r(tp)     ,r(teta)   ,r(hrms)   ,r(rlabda), &
                     & r(ewave1) ,r(wlen)   ,gdp      )
          endif
          call qkwcg(r(tp)     ,r(rlabda) ,r(teta)   ,r(qxkw)   ,r(qykw)   , &
                   & r(qxkr)   ,r(qykr)   ,d(dps)    ,r(s1)     ,i(kfs)    , &
                   & r(guu)    ,r(gvv)    ,r(cgc)    ,r(c)      ,i(irocol) , &
                   & norow     ,nocol     ,i(kfu)    ,i(kfv)    ,nmax      , &
                   & mmax      ,gdp       )
          call timer_stop(timer_tricom_rest, gdp)
          if (error) goto 9998
       endif
       call timer_start(timer_tricom_rest, gdp)
       call psemnefis
       if (cnstwv) then
          call constwave(nmmax     ,d(dps)    ,r(s1)     ,r(alfas)  ,r(ubot)   , &
                       & r(uorb)   ,r(tp)     ,r(teta)   ,r(hrms)   ,r(rlabda) , &
                       & r(wlen)   ,gdp       )
       endif
       call vsemnefis
       call timer_stop(timer_tricom_rest, gdp)
       !
       ! Solve FLOW-hydrodynamic
       !
       call timer_start(timer_trisol, gdp)
       if (.not.zmodel) then
          call trisol(dischy    ,solver    ,icreep    ,ithisc    , &
                    & timnow    ,nst       ,itiwec    ,trasol    ,forfuv    , &
                    & forfww    ,nfltyp    , &
                    & saleqs    ,temeqs    , &
                    & sferic    ,grdang    ,ktemp     ,temint    ,keva      , &
                    & evaint    ,anglat    ,anglon    ,rouflo    ,rouwav    , &
                    & betac     ,tkemod    ,comfil    , &
                    & error     ,gdp       )
       else
          if (nh_level == nh_full) then
             call z_trisol_nhfull(dischy    ,solver    ,icreep    , &
                                & timnow    ,nst       ,itiwec    ,trasol    ,forfuv    , &
                                & forfww    ,nfltyp    , &
                                & saleqs    ,temeqs    , &
                                & sferic    ,grdang    ,ktemp     ,temint    ,keva      , &
                                & evaint    ,anglat    ,anglon    ,rouflo    ,rouwav    , &
                                & betac     ,tkemod    ,gdp       )
          else
             call z_trisol(dischy    ,solver    ,icreep    , &
                         & timnow    ,nst       ,itiwec    ,trasol    ,forfuv    , &
                         & forfww    ,nfltyp    , &
                         & saleqs    ,temeqs    , &
                         & sferic    ,grdang    ,ktemp     ,temint    ,keva      , &
                         & evaint    ,anglat    ,anglon    ,rouflo    ,rouwav    , &
                         & betac     ,tkemod    ,gdp       )
          endif
       endif
       call timer_stop(timer_trisol, gdp)
       call timer_stop(timer_timeintegr, gdp)
    enddo
    !
    ! print u1,v1,s1 on text file
    ! 
    !call outputDELFT(r(s1),r(u1),r(v1),mmax,nmax,nlb,nub,mlb,mub,kmax)
    !
    ! The sequence of the 2 next calls is important for the OLV client.
    !
    call FLOWOL_Timestep (nst)
    !
    call timer_stop(timer_simulation, gdp)
    !
    ! Synchronisation point 3
    ! =======================
    ! - start finish semaphore
    ! - safe jump to error handling/communication closing (9999), inside the new semaphore
    !
  9998 continue

    if (error) then
      gdp%errorcode = -1 
    endif
  
  end subroutine tricom_step
