subroutine tricom_init(olv_handle, gdp)
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
!    Function: - Read md-file
!              - Initialize input arrays and verify input
!              
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
    use time_module
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
    include 'flow_steps_f.inc'
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
    integer                             , pointer :: itstop
    integer                             , pointer :: itfinish
    integer                             , pointer :: itlfsm
    integer                             , pointer :: itmapf
    integer                             , pointer :: itmapi
    integer                             , pointer :: itmapl
    integer                             , pointer :: ithisf
    integer                             , pointer :: ithisi
    integer                             , pointer :: ithisl
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
    real(fp)                            , pointer :: tcmp
    real(fp)                            , pointer :: tmor
    real(fp)                            , pointer :: rdc
    integer                             , pointer :: itcmp
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
    integer                             , pointer :: waveol
    logical                             , pointer :: threed
    logical                             , pointer :: secflo
    logical                             , pointer :: struct
    logical                             , pointer :: sedim
    logical                             , pointer :: htur2d
    logical                             , pointer :: flmd2l
    logical                             , pointer :: mudlay
    logical                             , pointer :: mudwave
    logical                             , pointer :: nfl
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
    integer(pntrsize)                   , pointer :: cbuv
    integer(pntrsize)                   , pointer :: cbuvrt
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
    integer(pntrsize)                   , pointer :: rint
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
    integer                             , pointer :: it01    !  Description and declaration in esm_alloc_int.f90
    integer                             , pointer :: it02    !  Description and declaration in esm_alloc_int.f90
    integer                             , pointer :: itima   !!  Time to start simulation (N * tscale) according to DELFT3D conventions
    integer                             , pointer :: itlen   !  Description and declaration in esm_alloc_int.f90
    real(fp)                            , pointer :: tscale  !  Description and declaration in esm_alloc_real.f90
    character(256)                      , pointer :: comfil  !!  Communication file name
    character(256)                      , pointer :: runid   !!  Run identification code for the current simulation (used to determine the names of the in- /output files used by the system)
    character(256)                      , pointer :: trifil  !!  File name for FLOW NEFIS output files (tri"h/m"-"casl""labl".dat/def)
    character(5)                        , pointer :: versio  !!  Version nr. of the current package
    integer                             , pointer :: iphisc        ! Current time counter for printing history data 
    integer                             , pointer :: itcomc        ! Current time counter for the communication file 
    integer                             , pointer :: itcur         ! Current time counter for the communication file, where starting point depend on CYCLIC 
    integer                             , pointer :: itdroc        ! Current time counter for the drogue data file 
    integer                             , pointer :: ithisc        ! Current time counter for the history file 
    integer                             , pointer :: itimc         ! Current time step counter for 2D system 
    integer                             , pointer :: itiwec        ! Current time counter for the calibration of internal wave energy 
    integer                             , pointer :: itlent        ! Lenght of the tide cycle in steps of ITP 
    integer                             , pointer :: itmapc        ! Current time counter for the map file 
    integer                             , pointer :: itp           ! Timestep for computation 2D system 
    integer                             , pointer :: itrstc        ! Current time counter for the restart file. Start writing after first interval is passed. Last time will always be written to file for ITRSTI > 0 
    integer                             , pointer :: itrw          ! Time to read the wave information in case of online wave coupling
    integer                             , pointer :: maxmn         ! Maximum of MMAX and NMAX 
    integer                             , pointer :: npmap         ! Current array counter for printing map data 
    integer                             , pointer :: ntcur         ! Total number of timesteps on comm. file (to write to) 
    integer                             , pointer :: ntwav         ! Total number of timesteps on comm. file (to read from) for waves 
    integer , dimension(:)              , pointer :: timwav        ! Array with time steps on comm. file for wave results 
    logical                             , pointer :: lrdok         ! Logical to check if reading phase has been passed. 
    logical                             , pointer :: waverd        ! Flag = TRUE if wave process and communication file exist 
    real(fp)                            , pointer :: anglat        ! Angle of latitude of the model centre (used to determine the coeff. for the coriolis force) 
    real(fp)                            , pointer :: anglon        ! Angle of longitude of the model centre (used to determine solar radiation) 
    real(fp)                            , pointer :: dtsec         ! DT in seconds 
    real(fp)                            , pointer :: timnow        ! Current timestep (multiples of dt)  = number of time steps since itdate, 00:00:00 hours
    real(fp)                            , pointer :: ti_sedtrans   ! Sediment transport start time
    integer                             , pointer :: iti_sedtrans  ! Sediment transport start time step
    
!
! Global variables
!
    type(olvhandle) :: olv_handle
!
! Local variables
!
    integer                                       :: icx
    integer                                       :: icy
    integer                                       :: ierror        ! Value is non-zero when an error is encountered
    integer                                       :: iplus
    integer                                       :: istat
    integer                                       :: mmaxddb
    integer                            , external :: modlen
    integer                                       :: mp
    integer                            , external :: newlun
    integer                                       :: nhystp
    integer                                       :: nmaxddb
    integer                                       :: nst           ! Current time step counter 
    integer                                       :: ntmin
    integer                                       :: numtimesteps  ! total nr of timesteps 
    integer                                       :: timrst        ! Restart time in combination with restart option from comm. file 
    integer                                       :: trilen        ! Length of trifil 
    integer        , dimension(2)                 :: ifcore        ! Time indices (cell id's) of the wave functions which are in core available 
    integer(pntrsize)                  , external :: gtcpnt
    integer(pntrsize)                  , external :: gtipnt
    integer(pntrsize)                  , external :: gtrpnt
    logical                                       :: commrd
    logical                                       :: cyclic        ! Flag = TRUE if cyclic system assumed 
    logical                                       :: error         ! Flag=TRUE if an error is encountered 
    logical                                       :: success       ! Flag = false when an error is encountered
    logical                                       :: ex            ! Help flag = TRUE when file is found 
    real(fp)                                      :: dtmin         ! DT in minutes
    real(fp)                                      :: dx            ! Uniform grid-distance in the x-dir. 
    real(fp)                                      :: dy            ! Uniform grid-distance in the y-dir. 
    real(fp)                                      :: riglid        ! Rigid lid factor to reduce horizontal wet area (incompressible) 
    real(fp)                                      :: tdif
    real(fp)                                      :: vscale        ! Difference between the integer value of (dt*tunit) / tscale and it's real value (should be at most correct to machine accuarcy). 
    real(hp)                                      :: delta_T
    real(hp)                                      :: Tstart_Julian
    character(1)                                  :: roumet        ! Bed stress formulation specified: C : Chezy    W : White Colebrook M : Manning  Z : roughness par. 
    character(16)                                 :: simdat        ! Simulation date representing the flow condition at this date 
    character(256)                                :: filrgf        ! File name for the curvi-linear grid file (telmcrgf.xxx) !! file will be read formatted !! 
    character(256)                                :: filrol
    character(6)                                  :: ftype         ! String containing to which output file version group or to diagnostic file should be written 
    character(30)  , dimension(10)                :: runtxt        ! Textual description of model input 
    character(60)                                 :: txtput        ! Text to be print
    character(300)                                :: message
    character(256)                                :: errstring
!
!! executable statements -------------------------------------------------------
!
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
    itstop              => gdp%gdinttim%itstop
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
    ti_sedtrans         => gdp%gdmorpar%ti_sedtrans
    tcmp                => gdp%gdmorpar%tcmp
    tmor                => gdp%gdmorpar%tmor
    rdc                 => gdp%gdmorpar%rdc
    iti_sedtrans        => gdp%gdmorpar%iti_sedtrans
    itcmp               => gdp%gdmorpar%itcmp
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
    nfl                 => gdp%gdprocs%nfl
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
    cbuv                => gdp%gdr_i_ch%cbuv
    cbuvrt              => gdp%gdr_i_ch%cbuvrt
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
    it01                => gdp%gdtricom%it01
    it02                => gdp%gdtricom%it02
    itima               => gdp%gdtricom%itima
    itlen               => gdp%gdtricom%itlen
    tscale              => gdp%gdtricom%tscale
    comfil              => gdp%gdtricom%comfil
    runid               => gdp%runid
    trifil              => gdp%gdtricom%trifil
    versio              => gdp%gdtricom%versio
    lrdok               => gdp%gdtricom%lrdok
    iphisc              => gdp%gdtricom%iphisc
    itcomc              => gdp%gdtricom%itcomc
    itcur               => gdp%gdtricom%itcur
    itdroc              => gdp%gdtricom%itdroc
    ithisc              => gdp%gdtricom%ithisc
    itimc               => gdp%gdtricom%itimc
    itiwec              => gdp%gdtricom%itiwec
    itlent              => gdp%gdtricom%itlent
    itmapc              => gdp%gdtricom%itmapc
    itp                 => gdp%gdtricom%itp
    itrstc              => gdp%gdtricom%itrstc
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
    call timer_start(timer_tricomtot, gdp)
    !
    icx       = 0
    icy       = 0
    mmaxddb   = mmax + 2*gdp%d%ddbound
    nmaxddb   = nmax + 2*gdp%d%ddbound
    !
    error     = .false.
    commrd    = .true.
    !
    itstrt    = -1
    itfinish  = -1
    !
    ifcore(1) = 0
    ifcore(2) = 0
    !
    maxmn     = max(mmax, nmax)
    !
    rtcact    = noRTC
    lrdok     = .false.
    !
    ! Define pointers
    !
    call gtptrs(gdp)
    !
    ! Put header on the screen
    !
    txtput = 'Part IV   - Reading complete MD-file...'
    if (.not.parll .or. (parll .and. inode == master)) then
       write (lunscr, '(a)') txtput
    endif
    !
    ! Read md-file, only if initialisation is wanted
    !
    call readmd(lunmd     ,lundia    ,lunscr    ,error     ,runid     ,runtxt    , &
              & filrgf    ,dx        ,dy        ,sferic    , &
              & anglat    ,anglon    ,grdang    ,tgfcmp    ,roumet    ,rouwav    , &
              & temeqs    ,saleqs    ,betac     ,dml       , &
              & restid    ,icreep    ,trasol    , &
              & forfuv    ,forfww    ,ktemp     ,keva      , &
              & temint    ,evaint    , &
              & lturi     ,tkemod    ,riglid    , &
              & tstprt    ,prsmap    ,prshis    ,selmap    , &
              & selhis    ,filrol    ,kmax      ,gdp       )
    call dfsync (gdp)
    if (error) goto 9996
    !
    ! Initialize 2-Layer Fluid Mud communication
    !
    if (flmd2l) then
       call timer_start(timer_wait, gdp)
       call syncom_init(mudlay, densin, mlb, mub, nlb, nub)
       call timer_stop(timer_wait, gdp)
    endif
    !
    ! Initialize Couple-communication
    !
    if (couplemod) then
       call timer_start(timer_wait, gdp)
       write(*,*) 'Initializing communication with COUPLE ...'
       call syncflowcouple_init(error, gdp%runid       , gdp%gdcoup%flowtocouple, &
                              &       gdp%gdcoup%pars , gdp%gdcoup%locs        )
       write(*,*) '... continuing'
       call timer_stop(timer_wait, gdp)
       if (error) then
          coupleact = .false.
          call prterr(lundia    ,'J020'    ,'SyncFlowCouple_Init')
          goto 9996
       else
          coupleact = .true.
       endif
    endif
    !
    ! Initialises arrays for nearfield/farfield coupling
    !
    if (nfl) then
       call init_nfl(kmax,lstsci, gdp)
    else
       coupleact = .true.
       gdp%gdnfl%nf_src_mom = .false.
    endif
    !
    ! Define time differences for writing output files
    ! for MAP print the times should be defined ( >= 0)
    !
    ! No resetting of MAP and HIS times
    !
    ditcof = itfinish - itcomf
    ditcol = itfinish - itcoml
    !
    ! Initialize numerical parameters
    !
    call inimet(lundia    ,error     ,versio    ,wave      ,trasol    , &
              & momsol    ,method    ,dischy    ,solver    ,icreep    , &
              & disctr    ,gdp       )
    if (error) goto 9996
    !
    ! Check conflicting settings
    !
    call chkset(lundia    ,error     ,sferic    ,method    ,trasol    , &
              & dischy    ,solver    ,disctr    ,ktemp     , &
              & keva      ,iphisi    ,gdp       )
    if (error) goto 9996
    !
    ! Test Comm. file version for consistency
    !
    ftype = 'com'
    call chkcom(lundia    ,error     ,comfil    ,ftype     ,gdp       )
    if (error) goto 9996
    !
    ! Read TSCALE and ITLEN from comm. file for ITLEN = 0,
    ! writing will be done in WRPARM (TRICOM-INIPPR-WRCOMI-WRPARM)
    ! for ITCOMI > 0
    !
    if (itlen==0) then
       call rdtimc(comfil    ,lundia    ,error     ,commrd    ,itlen     , &
                 & tscale    ,gdp       )
       if (error) goto 9996
       !
       ! Test value COMMRD
       !
       if (.not.commrd) then
          tscale = dt*tunit
          if (waveol==0) then
             itlen  = itfinish + 1
          endif
       else
          !
          ! setting itlen to 0 here:
          ! - makes a mess of time management in tricom and the use of periodic input
          ! - enables the use of offline wave results from the com-file
          !
          itlen = 0
       endif
       itima = itstrt
    endif
    !
    ! Test if DT*TUNIT is an integer multiple of TSCALE as defined,
    ! relative test
    !
    vscale = dt*tunit - int((dt*tunit)/tscale + 1.E-6)*tscale
    if (abs(vscale) > tscale*1.0e-6) then
       call prterr(lundia    ,'D002'    ,' '       )
       if (commrd) then
          write(lundia,'(3a)') '          Tscale is read from files ''com-', trim(runid), '.def'' and ''.dat''. Remove them if not intended.'
       endif
       error = .true.
       goto 9996
    endif
    !
    ! Initialize time parameters for stand alone (and 2D/3D system)
    ! stand alone and no initialisation not permitted !!
    !
    itp = nint(dt*tunit/tscale)
    it01 = itdate
    it02 = 0
    !
    ! Initialize time parameters regarding to 2D/3D system parameters
    ! For evaluating single time steps (for example, for OMI or OpenDA purposes), 
    ! itstop will be reset to itstrt+1
    !
    itlent = itlen/itp
    itstop = itfinish
    !
    ! Initialize itrw on a not used value
    ! itrw will get a relevant value when the wave executable has run
    !
    itrw   = itfinish + 1
    !
    ! Calculate Julian start day for use in TRISOL
    !
    julday = ymd2jul(itdate)
    !
    ! Define start time and timesteps (in seconds)
    !
    dtsec = dt * tunit
    dtmin = dtsec / 60.0_fp
    hdt   = 0.5_fp * dtsec
    !
    timnow = real(itstrt,fp)
    call setcurrentdatetime(timnow, gdp)
    !
    ! According to this new time frame the related times must be redefined
    ! No resetting of MAP and HIS
    !
    itcomf = itfinish - ditcof
    itcoml = itfinish - ditcol
    !
    ! If ITLENT =< itfinish the simulation is cyclic.
    ! NOTE: notify that the time-dependent input is checked and the
    !       corresponding files are created according to the timeframe
    !       of the MD-file. If the input is cyclic this will not give
    !       problems (assuming that the data starts before the beginning
    !       of the first cycle (t=0), and will be large enough to
    !       cover the total simulation period).
    !
    cyclic = (itfinish>=itlent) .and. (itlent /= 0)
    if (itcomi == 0) then
       if (cyclic) then
          call prterr(lundia    ,'D005'    ,' '       )
       endif
    endif
    if (.not.cyclic) then
       if (it01 /= itdate) then
          call prterr(lundia    ,'D006'    ,' '       )
          error = .true.
          goto 9996
       endif
    endif
    !
    ! Initialisation and Checking
    !
    ! Put header on the screen
    !
    txtput = 'Part V    - Initialisation & checking input...'
    if (.not.parll .or. (parll .and. inode == master)) then
       write (lunscr, '(a)') txtput
    endif
    !
    ! calculate timestep number for starting morphological changes
    ! Need to have delay from simulation start so that can have a
    ! spin-up time each time simulation starts if have multiple
    ! continuing simulations
    !
    if (sedim) then
       tdif  = tcmp + itstrt*dt
       itcmp = nint(tdif/dt)
       if (abs(itcmp*dt-tdif) > (0.1*dt)) then
          error  = .true.
          txtput = 'Bed composition updating start time'
          call prterr(lundia, 'U044', txtput)
       endif
       write(txtput,'(a,i0)') 'Bed composition updating starts at (step) : ',itcmp
       call prterr(lundia, 'G051', txtput)
       !
       tdif  = tmor + itstrt*dt
       itmor = nint(tdif/dt)
       if (abs(itmor*dt-tdif) > (0.1*dt)) then
          error  = .true.
          txtput = 'Bed level updating start time'
          call prterr(lundia, 'U044', txtput)
       endif
       write(txtput,'(a,i0)') 'Bed level updating starts at (step) : ',itmor
       call prterr(lundia, 'G051', txtput)
       
       !
       tdif  = ti_sedtrans + itstrt*dt
       iti_sedtrans = nint(tdif/dt)
       if (abs(iti_sedtrans*dt-tdif) > (0.1*dt)) then
          error  = .true.
          txtput = 'Sediment transport start time'
          call prterr(lundia, 'U044', txtput)
       endif
       write(txtput,'(a,i0)') 'Sediment transport starts at (step) : ',iti_sedtrans
       call prterr(lundia, 'G051', txtput)       
       !
    endif
    !
    ! Initialize input arrays and verify input
    ! INCHKI is placed between semaphores; in case of a fluidmud
    ! calculation and when both water and mud fraction use the same
    ! grid-file, they may try to read the grid-file at the same time.
    ! This was first noticed on an XP-platform. The use of semaphores
    ! reduces the collapse chance, but does not guarantee that it
    ! will not happen.
    !
    call inchki(lundia    ,error     ,runid     ,sferic    ,filrgf    , &
              & dx        ,dy        ,anglat    ,anglon    ,grdang    , &
              & tgfcmp    ,riglid    ,keva      , &
              & temeqs    ,saleqs    ,ktemp     ,fclou     , &
              & sarea     ,roumet    ,rouflo    ,restid    , &
              & lturi     ,gdp       )
    if (error) goto 9996
    !
    ! Set some single variables in the memory
    ! comform how they are declared in the initial part of the
    ! communication file
    !
    i(gtipnt('NMAX', gdp))        = nmax
    i(gtipnt('NMAXUS', gdp))      = nmaxus
    i(gtipnt('MMAX', gdp))        = mmax
    i(gtipnt('NOROW', gdp))       = norow
    i(gtipnt('NOCOL', gdp))       = nocol
    i(gtipnt('NOROCO', gdp))      = noroco
    i(gtipnt('NTO', gdp))         = nto
    i(gtipnt('NROB', gdp))        = nrob
    i(gtipnt('KMAX', gdp))        = kmax
    i(gtipnt('NSRC', gdp))        = nsrc
    i(gtipnt('ITLEN', gdp))       = itlen
    i(gtipnt('IT01', gdp))        = it01
    i(gtipnt('IT02', gdp))        = it02
    r(gtrpnt('AG', gdp))          = ag
    r(gtrpnt('RHOW', gdp))        = rhow
    r(gtrpnt('DT', gdp))          = dt
    r(gtrpnt('TSCALE', gdp))      = tscale
    ch(gtcpnt('ROUFLO', gdp))     = rouflo(1:1)
    ch(gtcpnt('ROUFLO', gdp) + 1) = rouflo(2:2)
    ch(gtcpnt('ROUFLO', gdp) + 2) = rouflo(3:3)
    ch(gtcpnt('ROUFLO', gdp) + 3) = rouflo(4:4)
    !
    ! User defined functions (reading and checking)
    !
    call usrdef(lundia    ,error     ,grdang    ,secflo    ,gdp       )
    if (error) goto 9996
    !
    ! Balance output?
    !
    call rdmassbal(r(xz)     ,r(yz)     ,i(kcs)    ,r(gsqs)   , &
                 & mmax      ,nmax      ,nmaxus    ,nmmax     , &
                 & lsedtot   ,gdp       )
    !
    ! Read the file with wave components
    !
    if (roller) then
       if (wavcmp) then
          call rbsig(ncmax     ,r(ampbc)  ,r(ombc)   ,r(phibc)  ,r(thetbc) , &
                   & filrol    ,lundia    ,gdp       )
       endif
    endif
    if (dredge) then
       !
       ! Read dredge input and initialize related data
       !
       call rddredge_d3d4(r(gsqs)   ,gdp       )
    endif
    if (multi) then
       !
       ! Initialise parallel online mor run for multiple conditions
       !
       call initmerge(nmmax, lsedtot, runid, gdp)
    endif
    !
    ! Read culvert input and initialize related data
    !
    if (culvert) then
       call rdcul(nsrc, ch(namsrc), i(mnksrc) ,r(voldis), gdp)       
    endif
    !
    ! Put header on the screen
    !
    txtput = 'Part VI   - Initialisation & checking second part...'
    if (.not.parll .or. (parll .and. inode == master)) then
       write (lunscr, '(a)') txtput
    endif
    !
    ! Read time array in case of wave interaction only if waverd
    ! Initial value of WAVERD := WAVE .and. COMMRD
    ! If an error occurred while reading the communication file then
    ! WAVERD will be set .false.
    !
    waverd = wave .and. commrd
    if (waverd) then
       !
       ! Initialize NTWAV = 0 (this is the number of wave times read so far).
       !
       ntwav = 0
       call rdtimw(comfil    ,lundia    ,error     ,ntwav     , &
                 & waverd    ,nmaxus    ,mmax      ,gdp       )
       if (error) goto 9996
       !
       ! Don't read waves during first time step when online coupling to wave (WAVEOL=2) (or mimicking that; WAVEOL=1)
       ! If WAVEOL=2 we probably have COMMRD=FALSE, but in case an old com-file exists we don't want the program to read it (unless we are restarting?).
       ! If WAVEOL=1 the com-file will exist, but we shouldn't read it yet for consistency.
       !
       if (waveol>0) waverd = .false.
    endif
    !
    ! Wave is true and not able to read wave information from comm-file:
    ! - Stop when running stand alone
    ! - Continue when running as part of MOR
    ! - Continue when online coupling with waves is applied
    !
    if (wave .and. .not.waverd) then
       if (.not. (cnstwv .or. snelli) .and. waveol==0 .and. .not. xbeach) then
          error = .true.
          call prterr(lundia    ,'D007'    ,' '       )
          goto 9996
       else
          ! Warning about missing COM-file removed.
       endif
    endif
    !
    ! Check timeframe before reading/writing to comm. files.
    ! This check should be executed for all possible initial values
    !
    call chktim(lundia    ,nostat    ,ntruv     ,itstrt    ,itfinish  , &
              & prsmap    ,prshis    ,selmap    ,selhis    ,ipmap     , &
              & mxprt     ,itmapf    ,itmapl    ,itmapi    ,iphisf    , &
              & iphisl    ,iphisi    ,ithisf    ,ithisl    ,ithisi    , &
              & itcomf    ,itcoml    ,itcomi    ,itrsti    ,itnflf    , &
              & itnfli    ,itnfll    ,error     ,gdp       )
    if (error) goto 9996
    !
    ! Write DP to comm. file for ITCOMI > 0
    !
    ! Calculate DPS depending on DPSOPT
    ! NFLTYP must be set in CALDPS because it is being used by
    ! MOR-transport
    !
    icx = nmaxddb
    icy = 1
    call caldps(nmmax     ,nfltyp    ,icx       , &
              & icy       ,i(kcs)    ,r(dp)     ,d(dps)    ,gdp       )
    if (waveol>0) then
       !
       ! In case of wave online: write DPS to comm-file instead of DP
       !
       if (prec == hp) then
          call rwbotc_double(comfil    ,lundia    ,error     ,itima     , &
                           & itcomi    ,mmax      ,nmax      ,nmaxus    ,d(dps)    , &
                           & r(rbuff)  ,gdp       )
       else
          call rwbotc(comfil    ,lundia    ,error     ,itima     , &
                    & itcomi    ,mmax      ,nmax      ,nmaxus    ,d(dps)    , &
                    & r(rbuff)  ,gdp       )
       endif
    else
       call rwbotc(comfil    ,lundia    ,error     ,itima     , &
                 & itcomi    ,mmax      ,nmax      ,nmaxus    ,r(dp)     , &
                 & r(rbuff)  ,gdp       )
    endif
    if (error) goto 9996
    !
    ! DD code added:
    ! This code must be performed before inchkr and caldpu is called
    !
    i(gtipnt('lstsci', gdp)) = lstsci
    i(gtipnt('ltur'  , gdp)) = ltur
    i(gtipnt('lsed'  , gdp)) = lsed
    i(gtipnt('lsedtt', gdp)) = lsedtot
    r(gtrpnt('hdt'   , gdp)) = hdt
    i(gtipnt('ddb'   , gdp)) = gdp%d%ddbound
    i(gtipnt('mmaxdb', gdp)) = mmaxddb
    i(gtipnt('nmaxdb', gdp)) = nmaxddb
    !
    if (zmodel) then
       izmodl = 1
    else
       izmodl = 0
    endif
    i(gtipnt('izmodl', gdp)) = izmodl
    !
    if (roller) then
       iroll = 1
    else
       iroll = 0
    endif
    i(gtipnt('iroll', gdp)) = iroll
    !
    ! Synchronisation point 1
    ! =======================
    ! - close semaphore
    ! - DD synchronisation via nxtstp
    ! - put new semaphore
    ! - safe jump to the next synchronisation point (9997) in case of error, inside the new semaphore
    !
 9996 continue
    !
    ! Related pseminit is in trisim.f90
    !
    call vseminit
    !
    ! The call to nxtstp:
    ! - synchronises all subdomains up to this point
    ! - signals the mappers that they can initialize themselve
    ! - continues execution of the subdomains after all mappers are initialized
    !
    call timer_start(timer_d3dflowinit, gdp)
    nhystp = nxtstp(d3dflow_init, gdp)
    call timer_stop(timer_d3dflowinit, gdp)
    !
    if (dredge) call dredge_initialize_d3d4(gdp) ! Initialize dredging data across partitions
    !
    ! related vseminit is in tricom.f90, at label 9997
    !
    call pseminit
    !
    if (error) goto 9997
    !
    ! End of synchronisation point 1
    ! ==============================
    !
    ! Calculate DPU/DPV (depth at velocity points)
    !
    call caldpu(lundia    ,mmax      ,nmaxus    ,kmax      , &
              & zmodel    , &
              & i(kcs)    ,i(kcu)    ,i(kcv)    , &
              & i(kspu)   ,i(kspv)   ,r(hkru)   ,r(hkrv)   , &
              & r(umean)  ,r(vmean)  ,r(dp)     ,r(dpu)    ,r(dpv)    , &
              & d(dps)    ,r(dzs1)   ,r(u1)     ,r(v1)     ,r(s1)     , &
              & r(thick)  ,gdp       )
    !
    ! After Flowmapper has adjusted the KC[UVS] arrays, recalculate some
    ! geometric information in D3dFlow in vicinity of interfaces
    ! inigeo_dd must be called before xzyz_dd
    !
    if (.not.parll) then
       call inigeo_dd(lundia    ,mmax      ,nmax      ,nmaxus    ,r(guu)    , &
                    & r(gvv)    ,r(guv)    ,r(gvu)    ,r(gsqs)   ,r(gsqd)   , &
                    & r(guz)    ,r(gvz)    ,r(gud)    ,r(gvd)    ,r(gsqiu)  , &
                    & r(gsqiv)  ,i(kfu)    ,i(kfv)    ,i(kfs)    ,i(kcu)    , &
                    & i(kcv)    ,i(kcs)    ,gdp       )
    endif
    !
    ! Compute coordinates of water level points at internal subdomain boundaries
    ! Needed for WaveOL and coarse wind grid
    !
    call xzyz_dd (r(xz)  ,r(yz)  ,i(kcs) ,nmax  ,mmax  ,&
               & nmaxus ,r(xcor),r(ycor),gdp          )
    !
    ! xz and yz are needed by the meteo module
    !
    success   = gridtometeo(gdp%runid, nmaxus   , mmax     , &
                          & gdp%d%nlb, gdp%d%nub, gdp%d%mlb, gdp%d%mub, &
                          & i(kcs)   , r(xz)    , r(yz)    )
    call checkmeteoresult(success, gdp)
    !
    ! Set single variable NFLTYP in the memory (initialized in CALDPS)
    ! comform how they are declared in the initial part of the
    ! communication file
    !
    i(gtipnt('NFLTYP', gdp)) = nfltyp
    !
    ! Read and interpolate the wave information
    ! some array names are different in wave group on comm. file
    ! DIR := teta  , DISS := dis   , FX := wsu, FY := wsv,
    ! MX  := grmasu, MY   := grmasv
    !
    itimc = modlen(itstrt*itp, itlen)
    if (waverd) then
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
                 & waveol    ,gdp       )
       if (error) goto 9997
    endif
    !
    ! DD code added end
    !
    ! Initialize arithmetic arrays, depending on initial input as well
    ! as on previous defined input (read from comm. file or in arrays)
    !
    call inchkr(lundia    ,error     ,runid     ,timhr     ,dischy    , &
              & cyclic    ,sferic    ,grdang    ,temeqs    ,saleqs    , &
              & lturi     ,rouflo    ,rouwav    ,ktemp     ,temint    , &
              & evaint    ,gdp       )
    if (error) goto 9997
    !
    ! Initialize Delft3D-Sobek after coordinates etc have been set 
    !
    if (sbkol) then
       numTimeSteps  = itfinish-itstrt 
       delta_T       = dt*60.0_hp
       Tstart_Julian = real(julday,hp) + real(tstart,hp)/1440.0_hp - 0.5_hp
       write (message, '(3a)') 'Reading Sobek-Online configuration file "', trim(sbkConfigFile), '"'
       call prterr(lundia, 'G051', trim(message))
       call timer_start(timer_wait, gdp)
       call D3S_setup('D3D-FLOW'    ,    runid   , lundia        , sbkConfigFile , &
                      gdp%d%mlb     , gdp%d%mub  , gdp%d%nlb     , gdp%d%nub     , &
                      r(xcor)       , r(ycor)    , nto           , ch(nambnd)    , i(mnbnd)    , &
                      nostat        , gdp%gdstations%namst       , gdp%gdstations%mnstat       , gdp%gdbcdat%ext_bnd, &
                      Tstart_Julian , delta_T    , numTimeSteps  , errstring     , success       )
       call timer_stop(timer_wait, gdp)
       !
       if (.not. success) then
          write(message,'(2a)') '*** error D3S_Init: ', trim(errstring)
          call prterr(lundia, 'P004', trim(message))
          call d3stop(1, gdp)
       endif
       !
       call timer_start(timer_wait, gdp)
       write(*,*) 'Initialisation: D3S_put_levels'
       call D3S_put_levels(0        , &
                           gdp%d%mlb, gdp%d%mub, &
                           gdp%d%nlb, gdp%d%nub, &
                           r(s1)    , i(kfs)     )
       write(*,*) 'Initialisation: D3S_put_levels DONE'
       call timer_stop(timer_wait, gdp)
    endif
    !
    ! Initial reading phase has been passed
    !
    lrdok = .true.
    !
    ! Put header on the screen
    !
    txtput = 'Part VII  - Initialisation output...'
    if (.not.parll .or. (parll .and. inode == master)) then
       write (lunscr, '(a)') txtput
    endif
    !
    ! The following files may be generated for "debug" purpose
    ! Remove them if they are already there from a previous run
    !
    call rmdel('tstprt.'//trim(gdp%runid), gdp) 
    call rmdel('discharge.'//trim(gdp%runid), gdp) 
    call rmdel('averagetemp.'//trim(gdp%runid), gdp) 
    call rmdel('pdfs.'//trim(gdp%runid), gdp) 
    !
    ! Initialise trachytope roughness time parameter for periodic calculations
    !
    ittrtu = itstrt + itimtt
    !
    ! Initialise FILE time parameters and test values
    !
    npmap = 1
    do mp = 2, mxprt
       if (ipmap(mp) == 0) then
          exit
       endif
       if (ipmap(mp) <= itstrt) npmap = mp
    enddo
    iphisc = iphisf
    !
    if (itmapi == 0) then
       itmapc = -1
    elseif (itmapf < itstrt) then
       iplus = ((itstrt - itmapf)/itmapi)*itmapi
       if (iplus < (itstrt-itmapf)) iplus = iplus + itmapi
       itmapc = min(itmapf + iplus, itmapl)
    else
       itmapc = min(itmapf, itmapl)
    endif
    !
    if (ithisi == 0) then
       ithisc = -1
    elseif (ithisf < itstrt) then
       iplus = ((itstrt - ithisf)/ithisi)*ithisi
       if (iplus < (itstrt-ithisf)) iplus = iplus + ithisi
       ithisc = min(ithisf + iplus, itfinish)
    else
       ithisc = min(ithisf, itfinish)
    endif
    !
    if (ithisi == 0) ithisc = -1
    itdroc = itdrof
    if (itdroi == 0) itdroc = -1
    itrstc = min(itstrt + itrsti, itfinish)
    if (itrsti == 0) itrstc = -1
    itiwec = itstrt
    if (itiwei == 0) itiwec = -1
    !
    itcomc = itcomf
    if (itcomi > 0) then
       if (cyclic) then
          ntcur = (itlent - 1)/itcomi + 1
          ! Check if all time points on com-file will be filled
          ! Note: error message has to be tidied up.
          ntmin = (itcoml - itcomf)/itcomi + 1
          if (ntmin < ntcur) then
             write (lundia, '(a,a)') '*** error ',                              &
                              & 'not all time points on com-file will be filled '
             error = .true.
             goto 9997
          endif
          itcur = itcomc/itcomi + 1
          if (itcur >  ntcur) itcur = mod(itcur, ntcur)
          if (itcur == 0    ) itcur = ntcur
       else
          ntcur = (itcoml - itcomf)/itcomi + 1
          itcur = 1
       endif
    endif
    !
    ! Write initial input to output files after checking output file times
    !
    call inippr(lundia    ,error     ,trifil    ,comfil    , &
              & selhis    ,selmap    ,tscale    ,commrd    , &
              & itlen     ,itcur     ,itimc     , &
              & it01      ,it02      ,sferic    ,grdang    , &
              & rouflo    ,nfltyp    , &
              & runtxt    ,gdp       )
    if (error) goto 9997
    !
    ! Re-define default values in depth array for online visualisation
    ! Hence all 999.999 will be set to 0.0
    !
    call dp999(r(dp)     ,nmax      ,mmax      ,gdp       )
    !
    ! DD code added:
    !
    ! The new geometric information influences the check on dry points
    ! A check is necessary at couple boundaries
    !
    icx = nmaxddb
    icy = 1
    call chkdry_dd(jstart    ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                 & icy       ,i(kcu)    ,i(kcv)    ,i(kcs)    ,i(kfu)    , &
                 & i(kfv)    ,i(kfs)    ,r(hu)     ,r(hv)     ,r(guu)    , &
                 & r(gvv)    ,r(thick)  ,r(u1)     ,r(v1)     ,r(qxk)    , &
                 & r(qyk)    ,gdp       )
    call chkrefinement(gdp)
    !
    ! DD code added end
    !
    if (wave .and. waveol==2) then
       !
       ! Onlinecoupling with waves
       ! Initialise communication with Waves
       ! Waves needs numdomains, not nummappers
       !
       call timer_start(timer_wait, gdp)
       if (parll) then
           ! when parallel, dfsync is needed to make sure the master process is the last one to be finished.
           call dfsync (gdp)
           if (inode==master) then
               ierror = flow_to_wave_init(runid     , it01   , tscale       , &
                                        & nproc     , mudlay ,.true. )
           endif
           call dfbroadc_gdp ( ierror, 1, dfint, gdp )
       else
          ierror = flow_to_wave_init(runid     , it01   , tscale       , &
                                   & numdomains, mudlay ,.false. )
       endif
       call timer_stop(timer_wait, gdp)
       if (ierror /= 0) then
          txtput = 'Initialization of DelftIO communication with waves failed'
          write(lunscr,'(a)') trim(txtput)
          call prterr(lundia, 'P004', trim(txtput))
          call d3stop(1, gdp)
       endif
    endif
    if (mudwave) then
       !
       ! Onlinecoupling between mud and waves
       ! Initialise wave pointering
       !
       call timer_start(timer_wait, gdp)
       ierror = flow_to_wave_init(runid     , it01   , tscale, &
                                & numdomains, mudlay ,.false. )
       call timer_stop(timer_wait, gdp)
       if (ierror /= 0) then
          txtput = 'Initialization of DelftIO communication with waves failed'
          write(lunscr,'(a)') trim(txtput)
          call prterr(lundia, 'P004', trim(txtput))
          call d3stop(1, gdp)
       endif
    endif
    !
    if (lsedtot>0) then
       call morbndfill(i(kcs)    ,r(guu)    ,r(gvv)    ,icx       , &
                     & icy       ,i(mnbnd)  ,nto       ,gdp       )
    endif
    !
    ! Write conversion from "n and m" to "nm" and vice versa to diag file
    !
    if (tstprt) then
       call nm_to_diag(gdp)
    endif
    !
    ! Make D3D data available to online applications
    !
    call new_olv(olv_handle)
    call publishGDP(olv_handle, gdp, runid, zmodel)
    !
    ! Not multi threaded
    !
    call publishUtils(olv_handle)
    call setEndTimeStep(olv_handle, itstop)
    !
    ! Synchronisation point 2
    ! =======================
    ! - close semaphore
    ! - DD synchronisation via initfinished
    ! - safe jump to the next synchronisation point (9998) in case of error, outside any semaphore
    !
 9997 continue
    !
    ! related pseminit is in tricom.f90 at label 9996
    !
    call vseminit
    !
    ! The call to initfinished synchronises all subdomains up to this point
    ! (necessary in case of multiple domains and wave online)
    !
    call timer_start(timer_d3dflowinit, gdp)
    call initfinished(numdomains)
    call timer_stop(timer_d3dflowinit, gdp)
    !
    if (error) goto 9998
    !
    ! Initialize RTC-communication
    !
    call rtc_comm_init(error     ,ch(nambar),ch(namcon),ch(namsrc), &
                     & r(cbuvrt) ,gdp)
    if (error) goto 9998
    call rtc_comm_put (i(kfs)    ,i(kfsmin) ,i(kfsmax) ,r(sig)    , &
                     & r(sig)    ,r(s1)     ,d(dps)    ,r(r0)     , &
                     & nsluv     ,r(cbuv)   ,nsrc      ,r(disch)  , &
                     & gdp)
    !
    ! End of synchronisation point 2
    ! ==============================
    !
    ! Put header on the screen
    !
    txtput = 'Part VIII - Start Simulation...'
    if (.not.parll .or. (parll .and. inode == master)) then
       call psemnefis
       write (lunscr, '(a)') txtput
       write (lunscr, *)
       call vsemnefis
    endif
    !
    call timer_stop(timer_init, gdp)
    !
9998 continue
    if (error) then
      gdp%errorcode = -1
      call d3stop(1, gdp)
    endif
    
end subroutine tricom_init

