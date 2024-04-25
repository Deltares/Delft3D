subroutine postpr(lundia    ,lunprt    ,error     ,versio    ,comfil    , &
                & trifil    ,runid     ,prsmap    ,prshis    ,selmap    , &
                & selhis    ,rhow      ,grdang    ,dtsec     , &
                & nst       ,iphisc    ,npmap     ,itcomc    ,itimc     , &
                & itcur     ,ntcur     ,ithisc    ,itmapc    ,itdroc    , &
                & itrstc    ,ktemp     ,halftime  ,gdp       )
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
!    Function: - Checks whether the current time step requires an
!                output
!              - Updates the cross sections informations at each
!                time step
!              - Activates the output routines
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use sync_flowcouple
    use precision
    use dfparall
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
    include 'fsm.i'
    integer(pntrsize)                    , pointer :: wrka1
    integer(pntrsize)                    , pointer :: wrka2
    integer(pntrsize)                    , pointer :: wrka3
    integer(pntrsize)                    , pointer :: wrka4
    integer(pntrsize)                    , pointer :: wrka5
    integer(pntrsize)                    , pointer :: wrkb1
    integer(pntrsize)                    , pointer :: wrkb2
    integer(pntrsize)                    , pointer :: wrkb3
    integer(pntrsize)                    , pointer :: wrkb4
    integer                              , pointer :: nmax
    integer                              , pointer :: mmax
    integer                              , pointer :: nlb
    integer                              , pointer :: nub
    integer                              , pointer :: mlb
    integer                              , pointer :: mub
    integer                              , pointer :: nmaxus
    integer                              , pointer :: kmax
    integer                              , pointer :: jstart
    integer                              , pointer :: nmmaxj
    integer                              , pointer :: nmmax
    integer                              , pointer :: lmax
    integer                              , pointer :: lmaxd
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
    integer                              , pointer :: kc
    integer                              , pointer :: nsrc
    integer                              , pointer :: nostat
    integer                              , pointer :: ntruv
    integer                              , pointer :: ntru
    integer                              , pointer :: nofou
    integer                              , pointer :: ndro
    integer          , dimension(:)      , pointer :: fconno
    integer          , dimension(:)      , pointer :: flayno
    integer          , dimension(:)      , pointer :: fnumcy
    integer                              , pointer :: fouwrt
    integer          , dimension(:)      , pointer :: ftmsto
    integer          , dimension(:)      , pointer :: ftmstr
    integer(pntrsize), dimension(:)      , pointer :: ifoupt
    integer          , dimension(:)      , pointer :: iofset
    real(fp)         , dimension(:)      , pointer :: fknfac
    real(fp)         , dimension(:,:,:)  , pointer :: foucomp
    real(fp)         , dimension(:)      , pointer :: foufas
    real(fp)         , dimension(:,:,:)  , pointer :: fousma
    real(fp)         , dimension(:,:,:)  , pointer :: fousmb
    real(fp)         , dimension(:,:,:)  , pointer :: fouvec
    real(fp)         , dimension(:)      , pointer :: fv0pu
    character(1)     , dimension(:)      , pointer :: fouelp
    character(16)    , dimension(:)      , pointer :: founam
    character(1)     , dimension(:)      , pointer :: foutyp
    integer                              , pointer :: itstrt
    integer                              , pointer :: itfinish
    integer                              , pointer :: itmapi
    integer                              , pointer :: itmapl
    integer                              , pointer :: ithisi
    integer                              , pointer :: ithisl
    integer                              , pointer :: itcomf
    integer                              , pointer :: itcomi
    integer                              , pointer :: itcoml
    integer                              , pointer :: itdrof
    integer                              , pointer :: itdroi
    integer                              , pointer :: itdrol
    integer                              , pointer :: itrsti
    integer                              , pointer :: iphisi
    integer                              , pointer :: iphisl
    integer          , dimension(:)      , pointer :: ipmap
    integer                              , pointer :: julday
    integer                              , pointer :: ntstep
    integer                              , pointer :: nto
    logical                              , pointer :: first
    integer                              , pointer :: nuprpg
    integer                              , pointer :: nuprln
    character(131)   , dimension(:)      , pointer :: header
    logical                              , pointer :: wind
    logical                              , pointer :: drogue
    logical                              , pointer :: wave
    integer                              , pointer :: waveol
    logical                              , pointer :: coupleact
    logical                              , pointer :: couplemod
    logical                              , pointer :: zmodel
    integer(pntrsize)                    , pointer :: alfas
    integer(pntrsize)                    , pointer :: areau
    integer(pntrsize)                    , pointer :: areav
    integer(pntrsize)                    , pointer :: atr
    integer(pntrsize)                    , pointer :: cfurou
    integer(pntrsize)                    , pointer :: cfvrou
    integer(pntrsize)                    , pointer :: ctr
    integer(pntrsize)                    , pointer :: dicuv
    integer(pntrsize)                    , pointer :: dicww
    integer(pntrsize)                    , pointer :: discum
    integer(pntrsize)                    , pointer :: dp
    integer(pntrsize)                    , pointer :: dps
    integer(pntrsize)                    , pointer :: dpsed
    integer(pntrsize)                    , pointer :: dpu
    integer(pntrsize)                    , pointer :: dpv
    integer(pntrsize)                    , pointer :: dtr
    integer(pntrsize)                    , pointer :: enstro
    integer(pntrsize)                    , pointer :: entr
    integer(pntrsize)                    , pointer :: eroll1
    integer(pntrsize)                    , pointer :: evap
    integer(pntrsize)                    , pointer :: ewave1
    integer(pntrsize)                    , pointer :: fltr
    integer(pntrsize)                    , pointer :: fxw
    integer(pntrsize)                    , pointer :: fyw
    integer(pntrsize)                    , pointer :: grmasu
    integer(pntrsize)                    , pointer :: grmasv
    integer(pntrsize)                    , pointer :: grmsur
    integer(pntrsize)                    , pointer :: grmsvr
    integer(pntrsize)                    , pointer :: grfacu
    integer(pntrsize)                    , pointer :: grfacv
    integer(pntrsize)                    , pointer :: gro
    integer(pntrsize)                    , pointer :: gsqs
    integer(pntrsize)                    , pointer :: guu
    integer(pntrsize)                    , pointer :: guv
    integer(pntrsize)                    , pointer :: gvu
    integer(pntrsize)                    , pointer :: gvv
    integer(pntrsize)                    , pointer :: hkru
    integer(pntrsize)                    , pointer :: hkrv
    integer(pntrsize)                    , pointer :: hrms
    integer(pntrsize)                    , pointer :: hu
    integer(pntrsize)                    , pointer :: hv
    integer(pntrsize)                    , pointer :: mndro
    integer(pntrsize)                    , pointer :: patm
    integer(pntrsize)                    , pointer :: precip
    integer(pntrsize)                    , pointer :: qu
    integer(pntrsize)                    , pointer :: qv
    integer(pntrsize)                    , pointer :: qxk
    integer(pntrsize)                    , pointer :: qxkr
    integer(pntrsize)                    , pointer :: qxkw
    integer(pntrsize)                    , pointer :: qyk
    integer(pntrsize)                    , pointer :: qykr
    integer(pntrsize)                    , pointer :: qykw
    integer(pntrsize)                    , pointer :: r1
    integer(pntrsize)                    , pointer :: rbuff
    integer(pntrsize)                    , pointer :: rho
    integer(pntrsize)                    , pointer :: rich
    integer(pntrsize)                    , pointer :: rlabda
    integer(pntrsize)                    , pointer :: rsed
    integer(pntrsize)                    , pointer :: rtur1
    integer(pntrsize)                    , pointer :: s1
    integer(pntrsize)                    , pointer :: sbtr
    integer(pntrsize)                    , pointer :: sbtrc
    integer(pntrsize)                    , pointer :: sbuu
    integer(pntrsize)                    , pointer :: sbvv
    integer(pntrsize)                    , pointer :: seddif
    integer(pntrsize)                    , pointer :: sig
    integer(pntrsize)                    , pointer :: sstr
    integer(pntrsize)                    , pointer :: sstrc
    integer(pntrsize)                    , pointer :: taubmx
    integer(pntrsize)                    , pointer :: taubpu
    integer(pntrsize)                    , pointer :: taubpv
    integer(pntrsize)                    , pointer :: taubsu
    integer(pntrsize)                    , pointer :: taubsv
    integer(pntrsize)                    , pointer :: teta
    integer(pntrsize)                    , pointer :: thick
    integer(pntrsize)                    , pointer :: tp
    integer(pntrsize)                    , pointer :: u1
    integer(pntrsize)                    , pointer :: umean
    integer(pntrsize)                    , pointer :: umnldf
    integer(pntrsize)                    , pointer :: uorb
    integer(pntrsize)                    , pointer :: v0
    integer(pntrsize)                    , pointer :: v1
    integer(pntrsize)                    , pointer :: vicuv
    integer(pntrsize)                    , pointer :: vicww
    integer(pntrsize)                    , pointer :: vmean
    integer(pntrsize)                    , pointer :: vmnldf
    integer(pntrsize)                    , pointer :: voldis
    integer(pntrsize)                    , pointer :: volum1
    integer(pntrsize)                    , pointer :: vortic
    integer(pntrsize)                    , pointer :: w1
    integer(pntrsize)                    , pointer :: windcd
    integer(pntrsize)                    , pointer :: windu
    integer(pntrsize)                    , pointer :: windv
    integer(pntrsize)                    , pointer :: wphy
    integer(pntrsize)                    , pointer :: ws
    integer(pntrsize)                    , pointer :: wsu
    integer(pntrsize)                    , pointer :: wsv
    integer(pntrsize)                    , pointer :: xcor
    integer(pntrsize)                    , pointer :: xydro
    integer(pntrsize)                    , pointer :: xz
    integer(pntrsize)                    , pointer :: ycor
    integer(pntrsize)                    , pointer :: yz
    integer(pntrsize)                    , pointer :: zalfas
    integer(pntrsize)                    , pointer :: zbdsed
    integer(pntrsize)                    , pointer :: z0ucur
    integer(pntrsize)                    , pointer :: z0vcur
    integer(pntrsize)                    , pointer :: z0urou
    integer(pntrsize)                    , pointer :: z0vrou
    integer(pntrsize)                    , pointer :: zcuru
    integer(pntrsize)                    , pointer :: zcurv
    integer(pntrsize)                    , pointer :: zcurw
    integer(pntrsize)                    , pointer :: zdicww
    integer(pntrsize)                    , pointer :: zdps
    integer(pntrsize)                    , pointer :: zdpsed
    integer(pntrsize)                    , pointer :: zenst
    integer(pntrsize)                    , pointer :: zfixfac
    integer(pntrsize)                    , pointer :: zfrac
    integer(pntrsize)                    , pointer :: zhidexp
    integer(pntrsize)                    , pointer :: zkfs
    integer(pntrsize)                    , pointer :: zmudfrac
    integer(pntrsize)                    , pointer :: zqxk
    integer(pntrsize)                    , pointer :: zqyk
    integer(pntrsize)                    , pointer :: zrca
    integer(pntrsize)                    , pointer :: zrho
    integer(pntrsize)                    , pointer :: zrich
    integer(pntrsize)                    , pointer :: zrsdeq
    integer(pntrsize)                    , pointer :: zsandfrac
    integer(pntrsize)                    , pointer :: zsbu
    integer(pntrsize)                    , pointer :: zsbv
    integer(pntrsize)                    , pointer :: zseddif
    integer(pntrsize)                    , pointer :: zsinkse
    integer(pntrsize)                    , pointer :: zsourse
    integer(pntrsize)                    , pointer :: zssu
    integer(pntrsize)                    , pointer :: zssv
    integer(pntrsize)                    , pointer :: ztaub
    integer(pntrsize)                    , pointer :: ztauet
    integer(pntrsize)                    , pointer :: ztauks
    integer(pntrsize)                    , pointer :: ztur
    integer(pntrsize)                    , pointer :: zvicww
    integer(pntrsize)                    , pointer :: zvort
    integer(pntrsize)                    , pointer :: zwl
    integer(pntrsize)                    , pointer :: zws
    integer(pntrsize)                    , pointer :: zwndsp
    integer(pntrsize)                    , pointer :: zwndcd
    integer(pntrsize)                    , pointer :: zwnddr
    integer(pntrsize)                    , pointer :: zairp
    integer(pntrsize)                    , pointer :: zprecp
    integer(pntrsize)                    , pointer :: zevap
    integer(pntrsize)                    , pointer :: dzs1
    integer(pntrsize)                    , pointer :: dzu1
    integer(pntrsize)                    , pointer :: dzv1
    integer(pntrsize)                    , pointer :: res
    integer(pntrsize)                    , pointer :: rl
    integer(pntrsize)                    , pointer :: xj
    integer(pntrsize)                    , pointer :: p1
    integer(pntrsize)                    , pointer :: hydprs
    integer(pntrsize)                    , pointer :: ibuff
    integer(pntrsize)                    , pointer :: itdro
    integer(pntrsize)                    , pointer :: kcs
    integer(pntrsize)                    , pointer :: kcu
    integer(pntrsize)                    , pointer :: kcv
    integer(pntrsize)                    , pointer :: kfs
    integer(pntrsize)                    , pointer :: kfu
    integer(pntrsize)                    , pointer :: kfv
    integer(pntrsize)                    , pointer :: kspu
    integer(pntrsize)                    , pointer :: kspv
    integer(pntrsize)                    , pointer :: mnksrc
    integer(pntrsize)                    , pointer :: kfumin
    integer(pntrsize)                    , pointer :: kfvmin
    integer(pntrsize)                    , pointer :: kfsmin
    integer(pntrsize)                    , pointer :: kfumax
    integer(pntrsize)                    , pointer :: kfvmax
    integer(pntrsize)                    , pointer :: kfsmax
    integer(pntrsize)                    , pointer :: kfuz1
    integer(pntrsize)                    , pointer :: kfvz1
    integer(pntrsize)                    , pointer :: namcon
    integer(pntrsize)                    , pointer :: namsrc
    integer(pntrsize)                    , pointer :: nambnd
    integer(pntrsize)                    , pointer :: mnbnd
    include 'tri-dyn.igd'
    integer                              , pointer :: itdate
    real(fp)                             , pointer :: tstart
    real(fp)                             , pointer :: tstop
    real(fp)                             , pointer :: dt
    real(fp)                             , pointer :: timhr
    type (flwoutputtype)                 , pointer :: flwoutput
    integer          , dimension(:, :)   , pointer :: mnit
    integer          , dimension(:, :)   , pointer :: mnstat
    character(4)                         , pointer :: rouflo
    character(20)    , dimension(:)      , pointer :: namst
    character(20)    , dimension(:)      , pointer :: namtra
    logical                              , pointer :: sferic
    logical                              , pointer :: firstwaq
    logical                              , pointer :: waqfil
    logical                              , pointer :: waqol
    logical                              , pointer :: lfbedfrm
    integer                              , pointer :: itwqff
    integer                              , pointer :: itwqfi
    integer                              , pointer :: itwqfl
    real(fp)                             , pointer :: zbot
    real(fp)                             , pointer :: ztop
!
! Global variables
!
    integer                                    :: iphisc !!  Current time counter for printing history data
    integer                                    :: itcomc
    integer                                    :: itcur  !!  Current time counter for the communication file, where starting point depend on CYCLIC
    integer                                    :: itdroc !!  Current time counter for the drogue data file
    integer                                    :: ithisc !!  Current time counter for the history data file
    integer                                    :: itimc  !!  Current time step counter for 2D system
    integer                                    :: itmapc !!  Current time counter for the MAP
    integer                                    :: itrstc !!  Current time counter for the restart file.
                                                         !!  Start writing after first interval is passed.
                                                         !!  Last time will always be written to file for ITRSTI > 0
    integer                                    :: ktemp  !! Description and declaration in tricom.igs
    integer                                    :: lundia !  Description and declaration in inout.igs
    integer                                    :: lunprt !  Description and declaration in inout.igs
    integer                                    :: npmap
    integer                                    :: nst    !!  Current time step counter
    integer                                    :: ntcur  !!  Total number of timesteps on communication file (to write to)
    logical                                    :: error  !!  Flag=TRUE if an error is encountered
    logical                      , intent(in)  :: halftime !!  Update time of next write if Flag=TRUE
    real(fp)                                   :: dtsec  !!  Integration time step [in seconds]
    real(fp)                                   :: grdang !  Description and declaration in tricom.igs
    real(fp)                                   :: rhow   !  Description and declaration in esm_alloc_real.f90
    character(*)                               :: comfil !!  Name for communication file com-<case><label>
    character(*)                               :: runid  !!  Run identification code for the current simulation
                                                         !!  (used to determine the names of the in- /output files used by the system)
    character(*)                               :: selmap !  Description and declaration in tricom.igs
    character(*)                               :: trifil !!  File name for FLOW NEFIS output files (tri"h/m"-"casl""labl".dat/def)
    character(19)                              :: prsmap !  Description and declaration in tricom.igs
    character(23)                              :: prshis !  Description and declaration in tricom.igs
    character(23)                              :: selhis !  Description and declaration in tricom.igs
    character(5)                               :: versio !!  Version nr. of the current package
!
! Local variables
!
    integer                  :: couplestatus
    integer                  :: filwri
    integer                  :: icel
    integer                  :: icx
    integer                  :: icy
    integer                  :: ifou           ! Loop counter for NOFOU 
    integer                  :: ilin           ! Loop counter for HEADER (5) 
    integer                  :: ipmapc         ! Current time counter for printing map data (IPMAP (NPMAPC)) 
    integer                  :: kmaxz          ! = KMAX for Z-model, = 0 for sigma-model
                                               ! Needed for correct dimensioning of DZU1 and DZV1
    integer                  :: nmaxddb
    integer(pntrsize)        :: velu           ! U velocity array (FSM r-index)
    integer(pntrsize)        :: velv           ! V velocity array (FSM r-index)
    integer       , external :: newlun
    logical                  :: flupd          ! Flag to update (true) or initialize (false) the discharge arrays 
    logical                  :: ftcros         ! Flag set when TCROSS is invoked 
    logical                  :: ftstat         ! Flag set when TSTAT  is invoked 
    real(fp)                 :: dstep          ! 1. / total number of timesteps (interval to write comm. file) 
    character(10)            :: velt           ! Velocity type 'Eulerian' or 'GLM'
    logical                  :: chez           ! if true there is a chezy value
    logical                  :: divByCellWidth !  Flag for scaling parameters to the correct dimensions in uv2zeta.f90
                                               !  Here used for scaling discharges to unit discharges for Fourier Analysis
    character(30) , dimension(10)              :: runtxt ! Dummy variable for wrh_main call (actual parameter needed in INIPPR)
!
!! executable statements -------------------------------------------------------
!
    wrka1               => gdp%gdaddress%wrka1
    wrka2               => gdp%gdaddress%wrka2
    wrka3               => gdp%gdaddress%wrka3
    wrka4               => gdp%gdaddress%wrka4
    wrka5               => gdp%gdaddress%wrka5
    wrkb1               => gdp%gdaddress%wrkb1
    wrkb2               => gdp%gdaddress%wrkb2
    wrkb3               => gdp%gdaddress%wrkb3
    wrkb4               => gdp%gdaddress%wrkb4
    nmax                => gdp%d%nmax
    mmax                => gdp%d%mmax
    nlb                 => gdp%d%nlb
    nub                 => gdp%d%nub
    mlb                 => gdp%d%mlb
    mub                 => gdp%d%mub
    nmaxus              => gdp%d%nmaxus
    kmax                => gdp%d%kmax
    jstart              => gdp%d%jstart
    nmmaxj              => gdp%d%nmmaxj
    nmmax               => gdp%d%nmmax
    lmax                => gdp%d%lmax
    lmaxd               => gdp%d%lmaxd
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
    kc                  => gdp%d%kc
    nsrc                => gdp%d%nsrc
    nostat              => gdp%d%nostat
    ntruv               => gdp%d%ntruv
    ntru                => gdp%d%ntru
    nofou               => gdp%d%nofou
    ndro                => gdp%d%ndro
    fconno              => gdp%gdfourier%fconno
    flayno              => gdp%gdfourier%flayno
    fnumcy              => gdp%gdfourier%fnumcy
    fouwrt              => gdp%gdfourier%fouwrt
    ftmsto              => gdp%gdfourier%ftmsto
    ftmstr              => gdp%gdfourier%ftmstr
    ifoupt              => gdp%gdfourier%ifoupt
    iofset              => gdp%gdfourier%iofset
    fknfac              => gdp%gdfourier%fknfac
    foucomp             => gdp%gdfourier%foucomp
    foufas              => gdp%gdfourier%foufas
    fousma              => gdp%gdfourier%fousma
    fousmb              => gdp%gdfourier%fousmb
    fouvec              => gdp%gdfourier%fouvec
    fv0pu               => gdp%gdfourier%fv0pu
    fouelp              => gdp%gdfourier%fouelp
    founam              => gdp%gdfourier%founam
    foutyp              => gdp%gdfourier%foutyp
    itstrt              => gdp%gdinttim%itstrt
    itfinish            => gdp%gdinttim%itfinish
    itmapi              => gdp%gdinttim%itmapi
    itmapl              => gdp%gdinttim%itmapl
    ithisi              => gdp%gdinttim%ithisi
    ithisl              => gdp%gdinttim%ithisl
    itcomf              => gdp%gdinttim%itcomf
    itcomi              => gdp%gdinttim%itcomi
    itcoml              => gdp%gdinttim%itcoml
    itdrof              => gdp%gdinttim%itdrof
    itdroi              => gdp%gdinttim%itdroi
    itdrol              => gdp%gdinttim%itdrol
    itrsti              => gdp%gdinttim%itrsti
    iphisi              => gdp%gdinttim%iphisi
    iphisl              => gdp%gdinttim%iphisl
    ipmap               => gdp%gdinttim%ipmap
    julday              => gdp%gdinttim%julday
    ntstep              => gdp%gdinttim%ntstep
    nuprpg              => gdp%gdpostpr%nuprpg
    nuprln              => gdp%gdpostpr%nuprln
    header              => gdp%gdpostpr%header
    wind                => gdp%gdprocs%wind
    drogue              => gdp%gdprocs%drogue
    wave                => gdp%gdprocs%wave
    waveol              => gdp%gdprocs%waveol
    coupleact           => gdp%gdprocs%coupleact
    couplemod           => gdp%gdprocs%couplemod
    zmodel              => gdp%gdprocs%zmodel
    alfas               => gdp%gdr_i_ch%alfas
    areau               => gdp%gdr_i_ch%areau
    areav               => gdp%gdr_i_ch%areav
    atr                 => gdp%gdr_i_ch%atr
    cfurou              => gdp%gdr_i_ch%cfurou
    cfvrou              => gdp%gdr_i_ch%cfvrou
    ctr                 => gdp%gdr_i_ch%ctr
    dicuv               => gdp%gdr_i_ch%dicuv
    dicww               => gdp%gdr_i_ch%dicww
    discum              => gdp%gdr_i_ch%discum
    dp                  => gdp%gdr_i_ch%dp
    dps                 => gdp%gdr_i_ch%dps
    dpsed               => gdp%gdr_i_ch%dpsed
    dpu                 => gdp%gdr_i_ch%dpu
    dpv                 => gdp%gdr_i_ch%dpv
    dtr                 => gdp%gdr_i_ch%dtr
    enstro              => gdp%gdr_i_ch%enstro
    entr                => gdp%gdr_i_ch%entr
    eroll1              => gdp%gdr_i_ch%eroll1
    evap                => gdp%gdr_i_ch%evap
    ewave1              => gdp%gdr_i_ch%ewave1
    fltr                => gdp%gdr_i_ch%fltr
    fxw                 => gdp%gdr_i_ch%fxw
    fyw                 => gdp%gdr_i_ch%fyw
    grmasu              => gdp%gdr_i_ch%grmasu
    grmasv              => gdp%gdr_i_ch%grmasv
    grmsur              => gdp%gdr_i_ch%grmsur
    grmsvr              => gdp%gdr_i_ch%grmsvr
    grfacu              => gdp%gdr_i_ch%grfacu
    grfacv              => gdp%gdr_i_ch%grfacv
    gro                 => gdp%gdr_i_ch%gro
    gsqs                => gdp%gdr_i_ch%gsqs
    guu                 => gdp%gdr_i_ch%guu
    guv                 => gdp%gdr_i_ch%guv
    gvu                 => gdp%gdr_i_ch%gvu
    gvv                 => gdp%gdr_i_ch%gvv
    hkru                => gdp%gdr_i_ch%hkru
    hkrv                => gdp%gdr_i_ch%hkrv
    hrms                => gdp%gdr_i_ch%hrms
    hu                  => gdp%gdr_i_ch%hu
    hv                  => gdp%gdr_i_ch%hv
    qu                  => gdp%gdr_i_ch%qu
    qv                  => gdp%gdr_i_ch%qv
    qxk                 => gdp%gdr_i_ch%qxk
    qxkr                => gdp%gdr_i_ch%qxkr
    qxkw                => gdp%gdr_i_ch%qxkw
    qyk                 => gdp%gdr_i_ch%qyk
    qykr                => gdp%gdr_i_ch%qykr
    qykw                => gdp%gdr_i_ch%qykw
    patm                => gdp%gdr_i_ch%patm
    precip              => gdp%gdr_i_ch%precip
    r1                  => gdp%gdr_i_ch%r1
    rbuff               => gdp%gdr_i_ch%rbuff
    rho                 => gdp%gdr_i_ch%rho
    rich                => gdp%gdr_i_ch%rich
    rlabda              => gdp%gdr_i_ch%rlabda
    rsed                => gdp%gdr_i_ch%rsed
    rtur1               => gdp%gdr_i_ch%rtur1
    s1                  => gdp%gdr_i_ch%s1
    sbtr                => gdp%gdr_i_ch%sbtr
    sbtrc               => gdp%gdr_i_ch%sbtrc
    sbuu                => gdp%gdr_i_ch%sbuu
    sbvv                => gdp%gdr_i_ch%sbvv
    seddif              => gdp%gdr_i_ch%seddif
    sig                 => gdp%gdr_i_ch%sig
    sstr                => gdp%gdr_i_ch%sstr
    sstrc               => gdp%gdr_i_ch%sstrc
    taubmx              => gdp%gdr_i_ch%taubmx
    taubpu              => gdp%gdr_i_ch%taubpu
    taubpv              => gdp%gdr_i_ch%taubpv
    taubsu              => gdp%gdr_i_ch%taubsu
    taubsv              => gdp%gdr_i_ch%taubsv
    teta                => gdp%gdr_i_ch%teta
    thick               => gdp%gdr_i_ch%thick
    tp                  => gdp%gdr_i_ch%tp
    u1                  => gdp%gdr_i_ch%u1
    umean               => gdp%gdr_i_ch%umean
    umnldf              => gdp%gdr_i_ch%umnldf
    uorb                => gdp%gdr_i_ch%uorb
    v0                  => gdp%gdr_i_ch%v0
    v1                  => gdp%gdr_i_ch%v1
    vicuv               => gdp%gdr_i_ch%vicuv
    vicww               => gdp%gdr_i_ch%vicww
    vmean               => gdp%gdr_i_ch%vmean
    vmnldf              => gdp%gdr_i_ch%vmnldf
    voldis              => gdp%gdr_i_ch%voldis
    volum1              => gdp%gdr_i_ch%volum1
    vortic              => gdp%gdr_i_ch%vortic
    windcd              => gdp%gdr_i_ch%windcd
    w1                  => gdp%gdr_i_ch%w1
    windu               => gdp%gdr_i_ch%windu
    windv               => gdp%gdr_i_ch%windv
    wphy                => gdp%gdr_i_ch%wphy
    ws                  => gdp%gdr_i_ch%ws
    wsu                 => gdp%gdr_i_ch%wsu
    wsv                 => gdp%gdr_i_ch%wsv
    xcor                => gdp%gdr_i_ch%xcor
    mndro               => gdp%gdr_i_ch%mndro
    xydro               => gdp%gdr_i_ch%xydro
    xz                  => gdp%gdr_i_ch%xz
    ycor                => gdp%gdr_i_ch%ycor
    yz                  => gdp%gdr_i_ch%yz
    z0ucur              => gdp%gdr_i_ch%z0ucur
    z0vcur              => gdp%gdr_i_ch%z0vcur
    z0urou              => gdp%gdr_i_ch%z0urou
    z0vrou              => gdp%gdr_i_ch%z0vrou
    zalfas              => gdp%gdr_i_ch%zalfas
    zbdsed              => gdp%gdr_i_ch%zbdsed
    zcuru               => gdp%gdr_i_ch%zcuru
    zcurv               => gdp%gdr_i_ch%zcurv
    zcurw               => gdp%gdr_i_ch%zcurw
    zdicww              => gdp%gdr_i_ch%zdicww
    zdps                => gdp%gdr_i_ch%zdps
    zdpsed              => gdp%gdr_i_ch%zdpsed
    zenst               => gdp%gdr_i_ch%zenst
    zfixfac             => gdp%gdr_i_ch%zfixfac
    zfrac               => gdp%gdr_i_ch%zfrac
    zhidexp             => gdp%gdr_i_ch%zhidexp
    zkfs                => gdp%gdr_i_ch%zkfs
    zmudfrac            => gdp%gdr_i_ch%zmudfrac
    zqxk                => gdp%gdr_i_ch%zqxk
    zqyk                => gdp%gdr_i_ch%zqyk
    zrca                => gdp%gdr_i_ch%zrca
    zrho                => gdp%gdr_i_ch%zrho
    zrich               => gdp%gdr_i_ch%zrich
    zrsdeq              => gdp%gdr_i_ch%zrsdeq
    zsandfrac           => gdp%gdr_i_ch%zsandfrac
    zsbu                => gdp%gdr_i_ch%zsbu
    zsbv                => gdp%gdr_i_ch%zsbv
    zseddif             => gdp%gdr_i_ch%zseddif
    zsinkse             => gdp%gdr_i_ch%zsinkse
    zsourse             => gdp%gdr_i_ch%zsourse
    zssu                => gdp%gdr_i_ch%zssu
    zssv                => gdp%gdr_i_ch%zssv
    ztaub               => gdp%gdr_i_ch%ztaub
    ztauet              => gdp%gdr_i_ch%ztauet
    ztauks              => gdp%gdr_i_ch%ztauks
    ztur                => gdp%gdr_i_ch%ztur
    zvicww              => gdp%gdr_i_ch%zvicww
    zvort               => gdp%gdr_i_ch%zvort
    zwl                 => gdp%gdr_i_ch%zwl
    zws                 => gdp%gdr_i_ch%zws
    zwndsp              => gdp%gdr_i_ch%zwndsp
    zwndcd              => gdp%gdr_i_ch%zwndcd
    zwnddr              => gdp%gdr_i_ch%zwnddr
    zairp               => gdp%gdr_i_ch%zairp
    zprecp              => gdp%gdr_i_ch%zprecp
    zevap               => gdp%gdr_i_ch%zevap
    dzs1                => gdp%gdr_i_ch%dzs1
    dzu1                => gdp%gdr_i_ch%dzu1
    dzv1                => gdp%gdr_i_ch%dzv1
    res                 => gdp%gdr_i_ch%res
    rl                  => gdp%gdr_i_ch%rl
    xj                  => gdp%gdr_i_ch%xj
    p1                  => gdp%gdr_i_ch%p1
    hydprs              => gdp%gdr_i_ch%hydprs
    ibuff               => gdp%gdr_i_ch%ibuff
    itdro               => gdp%gdr_i_ch%itdro
    kcs                 => gdp%gdr_i_ch%kcs
    kcu                 => gdp%gdr_i_ch%kcu
    kcv                 => gdp%gdr_i_ch%kcv
    kfs                 => gdp%gdr_i_ch%kfs
    kfu                 => gdp%gdr_i_ch%kfu
    kfv                 => gdp%gdr_i_ch%kfv
    kspu                => gdp%gdr_i_ch%kspu
    kspv                => gdp%gdr_i_ch%kspv
    mnksrc              => gdp%gdr_i_ch%mnksrc
    kfumin              => gdp%gdr_i_ch%kfumin
    kfvmin              => gdp%gdr_i_ch%kfvmin
    kfsmin              => gdp%gdr_i_ch%kfsmin
    kfumax              => gdp%gdr_i_ch%kfumax
    kfvmax              => gdp%gdr_i_ch%kfvmax
    kfsmax              => gdp%gdr_i_ch%kfsmax
    kfuz1               => gdp%gdr_i_ch%kfuz1
    kfvz1               => gdp%gdr_i_ch%kfvz1
    nambnd              => gdp%gdr_i_ch%nambnd
    mnbnd               => gdp%gdr_i_ch%mnbnd
    namcon              => gdp%gdr_i_ch%namcon
    namsrc              => gdp%gdr_i_ch%namsrc
    itdate              => gdp%gdexttim%itdate
    tstart              => gdp%gdexttim%tstart
    tstop               => gdp%gdexttim%tstop
    dt                  => gdp%gdexttim%dt
    timhr               => gdp%gdinttim%timhr
    flwoutput           => gdp%gdflwpar%flwoutput
    mnit                => gdp%gdstations%mnit
    mnstat              => gdp%gdstations%mnstat
    namst               => gdp%gdstations%namst
    namtra              => gdp%gdstations%namtra
    firstwaq            => gdp%gdwaqpar%firstwaq
    waqfil              => gdp%gdwaqpar%waqfil
    waqol               => gdp%gdwaqpar%waqol
    lfbedfrm            => gdp%gdbedformpar%lfbedfrm
    rouflo              => gdp%gdtricom%rouflo
    sferic              => gdp%gdtricom%sferic
    itwqff              => gdp%gdwaqpar%itwqff
    itwqfi              => gdp%gdwaqpar%itwqfi
    itwqfl              => gdp%gdwaqpar%itwqfl
    nto                 => gdp%d%nto
    zbot                => gdp%gdzmodel%zbot
    ztop                => gdp%gdzmodel%ztop
    
    !
    ! Initialisation
    !
    icx     = 0
    icy     = 0
    nmaxddb = nmax + 2*gdp%d%ddbound
    ftstat = .false.
    ftcros = .false.
    !
    if (halftime) goto 100
    !
    ! Communication file
    !
    if (itcomi > 0) then
       dstep = 1./(2.*itcomi)
       !
       ! The timestep NST = ITCOMF - ITCOMI there will be
       ! be a reset of QU and QV to 0.
       !
       if (nst == itcomf-itcomi) then
          flupd = .false.
          call updbnddps(flupd     ,dstep     ,jstart    ,nmmaxj    ,kmax      , &
                   & nsrc      ,r(qu)     ,r(qv)     ,r(discum) ,gdp       )
       !
       ! For timestep NST = ITCOMC
       ! devide QU, QV and DISCUM by 2 * ITCOMI; QU, QV and DISCUM are
       ! calculated every half DT; ITCOMC starts at ITCOMF
       !
       elseif (nst == itcomc) then
          flupd = .true.
          call updbnddps(flupd     ,dstep     ,jstart    ,nmmaxj    ,kmax      , &
                   & nsrc      ,r(qu)     ,r(qv)     ,r(discum) ,gdp       )
          !
          ! Write time dependent info to the communication file
          ! Online communication with couple:
          ! - wait for permission to write in com-file
          ! - always write in first cel
          ! - give permission to Couple
          !
          if (couplemod .and. coupleact) then
             if (itcur/=1) then
                call timer_start(timer_wait, gdp)
                write(*,*) '--------------'
                write(*,*) 'FLOW: GET call waiting for COUPLE'
                call syncflowcouple_get(couplestatus           , &
                                      & gdp%gdcoup%coupletoflow, gdp%gdcoup%getvalue, &
                                      & gdp%runid              , gdp%gdcoup%firstget)
                write(*,*) 'FLOW: GET call finished'
                write(*,*) '--------------'
                call timer_stop(timer_wait, gdp)
                if (couplestatus<0) then
                   write (*, '(a)') ' '
                   write (*, '(a)') ' Stop signal from Couple '
                   call prterr(lundia    ,'P004'    ,'Stop signal from Couple '      )
                   call d3stop(1         ,gdp       )
                endif
             endif
             icel = 1
          else
             icel = itcur
          endif
          if (wave .and. waveol>0) then
             !
             ! keep overwriting first record to avoid huge com-file
             !
             icel = 1
          endif
          if (zmodel) then
             kmaxz = kmax
          else
             kmaxz = 0
          endif
          call wrcomt(comfil    ,lundia    ,error     ,icel      ,ntcur     , &
                    & itimc     ,mmax      ,nmax      ,kmax      ,nmaxus    , &
                    & nsrc      ,i(mnksrc) ,lstsci    ,lsal      ,ltem      , &
                    & lsecfl    ,i(kfu)    ,i(kfv)    ,i(ibuff)  ,r(s1)     , &
                    & r(u1)     ,r(v1)     ,r(qu)     ,r(qv)     ,r(taubmx) , &
                    & r(r1)     ,r(dicuv)  ,r(dicww)  ,r(discum) ,r(windu)  , &
                    & r(windv)  ,r(dzu1)   ,r(dzv1)   ,kmaxz     ,r(hu)     , &
                    & r(hv)     ,r(thick)  ,gdp       )
          ! when parallel, dfsync is needed to make sure all the com files (from each domain) are completed.
          call dfsync (gdp) 
          if (couplemod .and. coupleact) then
             call timer_start(timer_wait, gdp)
             call syncflowcouple_send(1, gdp%gdcoup%flowtocouple, &
                                    &    gdp%gdcoup%putvalue  )
             call timer_stop(timer_wait, gdp)
          endif
          !
          if (error) goto 9999
          !
          ! Set QU, QV and DISCUM to 0.
          !
          flupd = .false.
          call updbnddps(flupd     ,dstep     ,jstart    ,nmmaxj    ,kmax      , &
                   & nsrc      ,r(qu)     ,r(qv)     ,r(discum) ,gdp       )
          !
          ! Update timestep to write Communication file
          ! ITSTRT <= ITCOMF <= ITCOMC <= ITCOML <= ITFINISH
          !
          if (itcomc+itcomi <= itcoml) then
             itcomc = itcomc + itcomi
             itcur  = itcur + 1
             if (itcur > ntcur) itcur = 1
          endif
       else
       endif
    endif
    !
    ! Write binary waq files.
    !
    chez = gdp%gdtricom%rouflo .eq. 'CHEZ'
    call wrwaqfil( mmax      , kmax      , nlb       , nub       , mlb       ,  &
                &  mub       , nmaxus    , nsrc      , i(kcs)    , i(kcu)    , i(kcv)    , i(kfsmin) ,  &
                &  i(kfsmax) , nst       , runid     , r(xcor)   , r(ycor)   ,  &
                &  r(xz)     , r(yz)     , r(guv)    , r(gvu)    , r(guu)    ,  &
                &  r(gvv)    , r(gsqs)   , r(volum1) , dtsec     , itdate    ,  &
                &  tstart    , tstop     , dt        , r(thick)  , lsal      ,  &
                &  ltem      , lsed      , r(r1)     , r(areau)  , r(areav)  ,  &
                &  r(taubmx) , r(dicww)  , d(dps)    , r(dp)     , r(cfurou) , r(cfvrou) , &
                &  chez      , i(mnksrc) , ch(namsrc), nto       , ch(nambnd), i(mnbnd)  , &
                &  zmodel    , ztop      , zbot      , gdp       )
    !
    ! Create the stream for FLOW to get the answer from WAQ
    !
    if (waqol) then
       !
       !  Take the coupling aggregation time steps into account.
       !
       if ((nst >= itwqff) .and. (nst <= itwqfl) .and. (mod(nst-itwqff,itwqfi) == 0)) then
          if (firstwaq) then
             !
             ! Skip getting the bedlevel the very first time
             ! (Nothing has been calculated yet)
             !
             firstwaq = .false.
          else
             call timer_start(timer_wait, gdp)
             call waq2flow(d(dps), mmax, nmaxus, kmax, lundia, mlb, mub, nlb, nub, gdp)
             call timer_stop(timer_wait, gdp)
             call wribot(comfil, lundia, error , mmax    , nmax,  &
                       & nmaxus, r(dp) , d(dps), r(rbuff), gdp     )
          endif
          !
          ! Set DPS at the boundary cell which is not taken care of by WAQ (depth at water level points)
          !
          call upbdps(mmax      , nmax         , i(kcs), &
                    & nmaxus    , r(dp)        , d(dps), gdp       )
          !
          ! Recalculate DPU/DPV (depth at velocity points)
          !
          call caldpu( lundia    ,mmax      ,nmaxus    ,kmax      , &
                    &  zmodel    , &
                    &  i(kcs)    ,i(kcu)    ,i(kcv)    , &
                    &  i(kspu)   ,i(kspv)   ,r(hkru)   ,r(hkrv)   , &
                    &  r(umean)  ,r(vmean)  ,r(dp)     ,r(dpu)    ,r(dpv)   , &
                    &  d(dps)    ,r(dzs1)   ,r(u1)     ,r(v1)     ,r(s1)    , &
                    &  r(thick)  ,gdp       )
          if (nst == itmapc) then
             !
             ! for waq mor only...
             ! Not enabled now. Will be used when morphology is moved from FLOW to WAQ
             !
             ! call wrsedwaqm(lundia , error     , trifil   , itmapc    , &
             !             & mmax   , nmaxus    , d(dps)   , gdp       )
             !
             ! for waq veg only....
             !
             !if (lsedtot > 0) then 
             !   !
             !   ! for waq veg and without sediment only...
             !   !
             !   call wrsedmgrp(lundia    ,error     ,trifil    ,itmapc    ,mmax      , &
             !                & kmax      ,nmaxus    ,lsed      ,lsedtot   , &
             !                & gdp       ,i(kfsmin) ,i(kfsmax) )
             !endif
          endif
       endif
    endif
    !
100 continue
    !
    ! Calculate adjusted velocities for mass flux
    !
    icx   = nmaxddb
    icy   = 1
    call euler(jstart    ,nmmax     ,nmmaxj    ,kmax      ,icx       , &
             & i(kcu)    ,i(kcv)    ,i(kfu)    ,i(kfv)    ,i(kfumax) , &
             & i(kfumin) ,i(kfvmax) ,i(kfvmin) ,r(dzu1)   ,r(dzv1)   , &
             & r(u1)     ,r(wrkb3)  ,r(v1)     ,r(wrkb4)  , &
             & r(grmasu) ,r(grmasv) ,r(hu)     ,r(hv)     , &
             & r(tp)     ,r(hrms)   ,r(sig)    ,r(thick)  ,r(teta)   , &
             & r(grmsur) ,r(grmsvr) ,r(grfacu) ,r(grfacv) ,gdp       )
    if (flwoutput%veuler) then
       velu = wrkb3
       velv = wrkb4
       velt = 'Eulerian'
    else
       velu = u1
       velv = v1
       velt = 'GLM'
    endif
    !
    ! Output for specific FLOW files
    !
    ! HIS file
    !
    ! Store flow- and concentration fluxes in defined cross-sections
    ! for every NST when or ITHISI or IPHISI or both are > 0
    !
    if (ithisi+iphisi > 0) then
       call tcross(dtsec     ,prshis    ,selhis    ,ntruv     ,ntru      , &
                 & lstsci    ,nmaxus    ,nmax      ,mmax      ,kmax      , &
                 & i(kfu)    ,i(kfv)    ,r(ctr)    ,r(fltr)   , &
                 & r(atr)    ,r(dtr)    ,r(guu)    ,r(gvv)    ,r(guv)    , &
                 & r(gvu)    ,r(thick)  ,r(r1)     ,r(qxk)    ,r(qyk)    , &
                 & r(hu)     ,r(hv)     ,r(dicuv)  ,lsed      ,lsedtot   , &
                 & r(sbtr)   ,r(sstr)   ,r(sbtrc)  ,r(sstrc)  ,r(sbuu)   , &
                 & r(sbvv)   ,gdp       )
       ftcros = .true.
    endif
    !
    ! Store water-levels and concentrations in defined stations
    ! and calculated velocities and discharges to zeta points
    ! for defined stations
    ! Only in case NST = ITHISC or NST = IPHISC
    ! The following workarrays are used to transport results to wrwavh:
    ! wrka1 zhs
    ! wrka2 ztp
    ! wrka3 zdir
    ! wrka4 zrlabd
    ! wrka5 zuorb
    !
    if (nst==ithisc .or. nst==iphisc) then
       call update_stat_locations(nostat    ,ndro      ,i(mndro)  , &
                                & r(xydro)  ,timhr     ,julday    , &
                                & lundia    ,gdp       )
       call tstat(prshis    ,selhis    ,rhow      ,zmodel    ,nostat    , &
                & nmax      ,mmax      ,kmax      ,lmax      ,lstsci    , &
                & ltur      ,lsal      ,ltem      ,i(kfs)    ,i(kfu)    , &
                & i(kfv)    ,i(kcs)    ,i(kfuz1)  ,i(kfvz1)  ,i(kfumin) , &
                & i(kfumax) ,i(kfvmin) ,i(kfvmax) ,i(kfsmin) ,i(kfsmax) , &
                & i(zkfs)   ,r(s1)     ,r(velu)   ,r(velv)   ,r(r1)     , &
                & r(rtur1)  ,r(wphy)   ,r(qxk)    ,r(qyk)    ,r(taubpu) , &
                & r(taubpv) ,r(taubsu) ,r(taubsv) ,r(alfas)  ,r(vicww)  , &
                & r(dicww)  ,r(rich)   ,r(rho)    ,d(dps)    ,r(zwl)    , &
                & r(zalfas) ,r(zcuru)  ,r(zcurv)  ,r(zcurw)  ,r(zqxk)   , &
                & r(zqyk)   ,r(gro)    ,r(ztur)   ,r(ztauks) ,r(ztauet) , &
                & r(zvicww) ,r(zdicww) ,r(zrich)  ,r(zrho)   ,r(zdps)   , &
                & r(hydprs) ,r(p1)     ,r(vortic) ,r(enstro) ,r(zvort)  , &
                & r(zenst)  ,r(wrka1)  ,r(wrka2)  ,r(wrka3)  ,r(wrka4)  , &
                & r(wrka5)  ,r(hrms)   ,r(tp)     ,r(teta)   ,r(rlabda) , &
                & r(uorb)   ,wave      ,r(windu)  ,r(windv)  ,r(windcd) , &
                & r(zwndsp) ,r(zwnddr) ,r(patm)   ,r(zairp)  ,wind      , &
                & r(precip) ,r(evap)   ,r(zprecp) ,r(zevap)  ,r(zwndcd) , &
                & gdp       )
       call tstat_sed(nostat    ,nmax      ,mmax      ,kmax      , &
                & lsed      ,lsedtot   ,i(kfu)    ,i(kfv)    ,i(kcs)    , &
                & r(ws)     ,r(zbdsed) ,r(zrsdeq) ,r(zdpsed) ,r(zws)    , &
                & r(zsbu)   ,r(zsbv)   ,r(zssu)   ,r(zssv)   ,r(sbuu)   , &
                & r(sbvv)   ,r(zrca)   ,r(zsourse),r(zsinkse),r(zfrac)  , &
                & r(zmudfrac),r(zsandfrac),r(zfixfac),r(ztaub)  ,r(zhidexp), &
                & r(seddif) ,r(zseddif),gdp       )
       ftstat = .true.
    endif
    !
    ! Call USER OUTPUT ROUTINE
    !
    call u_ppr(lundia    ,lunprt    ,error     ,versio    ,prsmap    , &
             & prshis    ,selmap    ,selhis    ,runid     ,rhow      , &
             & grdang    ,dtsec     ,nst       ,iphisc    , &
             & npmap     ,ithisc    ,itmapc    ,itdroc    ,itrstc    , &
             & ftstat    ,ftcros    ,gdp       )
    if (error) goto 9999
    !
    ! Print water-levels and concentrations in defined stations
    ! and calculated velocities to zeta points for defined stations
    ! Only in case NST = IPHISC
    !
    if (nst == iphisc) then
       call prthis(lundia    ,error     ,prshis    ,grdang    ,lunprt    , &
                 & nuprpg    ,nuprln    ,header    ,iphisc    ,julday    , &
                 & dtsec     ,nostat    ,ntruv     ,ntru      ,kmax      , &
                 & lstsci    ,lsal      ,ltem      ,ltur      ,lmax      , &
                 & namst     ,namtra    ,ch(namcon),mnstat    ,mnit      , &
                 & r(zwl)    ,r(zalfas) ,r(zcuru)  ,r(zcurv)  ,r(zcurw)  , &
                 & r(zvicww) ,r(zdicww) ,r(zrich)  ,r(zrho)   ,r(gro)    , &
                 & r(ztur)   ,r(ctr)    ,r(fltr)   ,r(atr)    ,r(dtr)    )
       if (error) goto 9999
       !
       ! Update timestep to print HIS data
       ! ITSTRT <= IPHISF <= IPHISC <= IPHISL <= ITFINISH
       !
       if (.not.halftime .and. iphisc+iphisi <= iphisl) iphisc = iphisc + iphisi
    endif
    !
    ! Write to binary HIS file
    ! only in case NST = ITHISC
    !
    if (nst == ithisc) then
       call wrh_main(lundia    ,error     ,selhis    ,grdang    ,dtsec     , &
                   & ithisc    ,runtxt    ,trifil    ,gdp       )
       if (error) goto 9999
       !
       ! Update timestep to write HIS files
       ! ITSTRT <= ITHISF <= ITHISC <= ITHISL <= ITFINISH
       !
       if (.not.halftime .and. nst==ithisc .and. ithisc+ithisi<=ithisl) then
          ithisc = ithisc + ithisi
       endif
    endif
    !
    ! MAP file
    !
    ! Print water-levels and concentrations and calculated velocities
    ! to zeta points for 2dH maps
    ! Only in case NST = IPMAP (NPMAPC)
    !
    ipmapc = ipmap(npmap)
    if (nst == ipmapc) then
       call prtmap(lundia    ,error     ,prsmap    ,lunprt    ,nuprpg    , &
                 & nuprln    ,header    ,ipmapc    ,julday    ,dtsec     , &
                 & grdang    ,nmax      ,mmax      ,kmax      ,nmaxus    , &
                 & lstsci    ,ltur      ,lmaxd     ,lsal      ,ltem      , &
                 & ch(namcon),i(kfu)    ,i(kfv)    ,i(kcs)    ,r(s1)     , &
                 & r(velu)   ,r(velv)   ,r(wphy)   ,r(alfas)  ,r(r1)     , &
                 & r(rtur1)  ,r(vicww)  ,r(dicww)  ,r(rich)   ,r(rho)    , &
                 & r(rbuff)  ,r(rbuff)  ,velt      ,gdp       )
       if (error) goto 9999
       !
       ! Update timestep to print MAP data
       ! ITSTRT <= IPMAP (NPMAP) <= IPMAP (NPMAP+1) <= ITFINISH
       !
       if (.not.halftime) then
          npmap = npmap + 1
       endif
    endif
    !
    ! Write to binary MAP file
    ! Only in case ITMAPI>0 and NST = ITMAPC
    !
    if (itmapi > 0) then
       if (nst == itmapc) then
          call wrm_main(lundia    ,error     ,selmap    ,grdang    ,dtsec     , &
                      & itmapc    ,runtxt    ,trifil    ,.false.   ,gdp       )
          if (error) goto 9999
       endif

       if (wave .and. waveol==2 .and. nst==itmapc) then
          !
          ! Create file TMP_write_wavm
          ! waves.exe will only write wave maps if file TMP_write_wavm exists 
          ! The file will be deleted by waves.exe after the map is written
          ! Note: the creation of file TMP_write_wavm will be repeated for each domain, but doesn't matter
          !
          open(newunit=filwri, file='TMP_write_wavm', status='unknown')
          close(filwri, status='keep')
       endif
       !
       ! Update timestep to write MAP files
       ! ITSTRT <= ITMAPF <= ITMAPC <= ITMAPL <= ITFINISH
       !
       if (.not.halftime .and. nst==itmapc .and. itmapc+itmapi<=itmapl) then
          itmapc = itmapc + itmapi
       endif
    endif
    !
    ! DRO file
    !
    ! Write to binary DRO file
    ! Only in case NST = ITDROC
    !
    if (drogue .and. nst==itdroc .and. .not.halftime) then
       call wrd_main(lundia    ,error     ,ndro      ,itdroc    ,runtxt    , &
                   & trifil    ,dtsec     ,gdp       )
       if (error) goto 9999
       !
       ! Update timestep to write drogue file
       ! ITSTRT <= ITDROF <= ITDROC <= ITDROL <= ITFINISH
       !
       if (itdroc+itdroi <= itdrol) then
          itdroc = itdroc + itdroi
       endif
    endif
    !
    if (halftime) goto 9999
    !
    ! Perform analysis and write to Fourier file
    !
    if (nofou > 0) then
       ifou = 1
       do 
          if (ifou > nofou) exit
          !
          ! Perform fourier analysis, every timestep as long as NST value
          ! lies in requested time interval FTMSTR and FTMSTO
          !
          if (foutyp(ifou) == 's') then
             !
             ! Scalar type Fourier component
             !
             ! Get Fourier component pointer
             !
             call getfpt(nmax    ,mmax     ,kmax    ,nofou    ,ifou      ,gdp  )
             call fouana(mmax    ,nmaxus    ,nofou     ,ifou    ,i(kcs)    , &
                       & i(kfs)  ,i(kfu)    ,i(kfv)    ,nst     ,r(ifoupt(ifou) + iofset(ifou))   , &
                       & r(umean),r(vmean)  ,d(dps)    ,gdp     )
             ifou = ifou + 1
          elseif (foutyp(ifou) == 'v') then
             !
             ! Vector type Fourier component: do everything for both vector components
             !
             ! Get Fourier pointer for first component
             !
             call getfpt(nmax    ,mmax     ,kmax    ,nofou    ,ifou      ,gdp  )
             !
             ! Get Fourier pointer for second component
             !
             call getfpt(nmax    ,mmax     ,kmax    ,nofou    ,ifou + 1  ,gdp  )
             if (founam(ifou)(:2) == 'u1') then
                ifoupt(ifou  ) = velu
                ifoupt(ifou+1) = velv
             !
             ! For bedstress define real stresses in WRKB1 and WRKB2 using TAUBPU(V),
             ! TAUBSU(V), U(V)1 for layer KMAX and RHOW
             ! IFOUPT = TAUBPU resp. TAUBPV and will be WRKB1 and WRKB2
             ! For adjusted velocities use work array WRKB3 (U1) and
             ! WRKB4 (V1)
             !
             elseif (founam(ifou)(:6) == 'taubpu') then
                call calbed(rhow      ,mmax      ,nmaxus    ,kmax      , &
                          & r(wrkb1)  ,r(taubpu) ,r(taubsu) ,r(wrkb3)  ,gdp       )
                ifoupt(ifou  ) = wrkb1
                call calbed(rhow      ,mmax      ,nmaxus    ,kmax      , &
                          & r(wrkb2)  ,r(taubpv) ,r(taubsv) ,r(wrkb4)  ,gdp       )
                ifoupt(ifou+1) = wrkb2
             else
             endif
             !
             ! Average the vector components from u and v points to the centre of the cell (zeta-point)
             ! Discharges are scaled to unit discharges by dividing by the cell width in uv2zeta.f90
             !
             if (founam(ifou)(:3) == 'qxk') then
                divByCellWidth = .true.
             else
                divByCellWidth = .false.
             endif
             call uv2zeta(mmax               ,nmaxus              ,i(kcs)           ,divByCellWidth     , &
                        & r(ifoupt(ifou)     + iofset(ifou))      ,r(ifoupt(ifou+1) + iofset(ifou+1))   , &
                        & foucomp(:,:,ifou)  ,foucomp(:,:,ifou+1) ,r(guu)           ,r(gvv)             , &
                        & gdp                )
             !
             ! Rotate the vector components from (ksi-eta)-coordinates to (x,y)-coordinates
             !
             call ksieta2xy(mmax     ,nmaxus              ,i(kcs)               , &
                          & r(alfas) ,foucomp(:,:,ifou)   ,foucomp(:,:,ifou+1)  ,gdp  )
             !
             ! Perform fourier analysis for first vector component
             !
             call fouana(mmax     ,nmaxus    ,nofou   ,ifou     ,i(kcs)   , &
                       & i(kfs)   ,i(kfu)    ,i(kfv)  ,nst      ,foucomp(:,:,ifou)   , &
                       & r(umean) ,r(vmean)  ,d(dps)  ,gdp      )
             !
             ! Perform fourier analysis for second vector component
             !
             call fouana(mmax     ,nmaxus    ,nofou   ,ifou+1   ,i(kcs)   , &
                       & i(kfs)   ,i(kfu)    ,i(kfv)  ,nst      ,foucomp(:,:,ifou+1) , &
                       & r(umean) ,r(vmean)  ,d(dps)  ,gdp      )
             !
             ! Determine the maximum or minimum of the magnitude of the vector
             ! Done for velocity (u,v), discharge (qxk,qyk) and bed shear stress (taubpu,taubpv)
             !
             call fouvecmax(mmax    ,nmaxus     ,i(kcs)    ,nofou      ,ifou   , &
                          & nst     ,gdp       )
             !
             ! Skip second component of vector (already done in fou_vector)
             !
             ifou = ifou + 2
          else 
             !
             ! Incorrect Fourier type found
             !
             call prterr(lundia    ,'P004'    ,'Incorrect Fourier type found.'  )
             call d3stop(1         ,gdp       )
          endif
       enddo
       !
       ! Write results of fourier analysis to data file
       ! only once when all fourier periods are complete
       !
       if (nst==fouwrt .or. ntstep==1) then
          call wrfou(nmax      ,mmax      ,nmaxus    ,kmax      ,lmax       , &
                   & nofou     ,runid     ,trifil    ,dtsec     ,versio    ,ch(namcon) , &
                   & i(kcs)    ,r(xz)     ,r(yz)     ,r(alfas)  ,r(xcor)    , &
                   & r(ycor)   ,i(kfu)    ,i(kfv)    ,itdate    ,gdp        )
          if (error) goto 9999
       endif
    endif
    !
    ! Write restart file
    ! Add interval ITRSTI to ITRSTC until ITFINISH (write also for ITFINISH)
    !
    if (nst == itrstc) then
       call wrirst(lundia    ,runid     ,itrstc    ,nmaxus    ,mmax      , &
                 & nmax      ,kmax      ,lstsci    ,ltur      ,r(s1)     , &
                 & r(u1)     ,r(v1)     ,r(r1)     ,r(rtur1)  ,r(umnldf) , &
                 & r(vmnldf) ,gdp       )
       write (lundia, '(a,f15.4,a)') '*** Restart file written at ', real(nst,sp)  &
                                   & *dtsec/60., ' minutes after ITDATE'
       itrstc = min(itrstc + itrsti, itfinish)
    endif
 9999 continue
end subroutine postpr
