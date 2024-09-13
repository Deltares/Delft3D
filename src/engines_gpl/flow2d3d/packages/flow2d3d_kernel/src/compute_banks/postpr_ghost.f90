subroutine postpr_ghost(nst  , s1 , u1 , v1 , qxk, qyk, Umean, Vmean, hu, hv, dpu, dpv, dps, &
                       & kmax, nlb, nub, mlb, mub, gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                
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
!  $Id: postpr_ghost.f90 6133 2016-05-13 14:53:11Z platzek $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_banks/postpr_ghost.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: 
!              - Call POSTPR without having to define all arguments in the
!                main routine. This routine is called in TRISOL for half
!                time steps.
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use flow2d3d_timers
    use globaldata
    use sync_flowcouple
    use dfparall
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    include 'fsm.i'
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !

    integer                             , pointer :: lundia
    integer                             , pointer :: lunprt
    real(fp)                            , pointer :: rhow
    integer                             , pointer :: ktemp
    real(fp)                            , pointer :: grdang
    character(19)                       , pointer :: prsmap
    character(21)                       , pointer :: selmap
    character(23)                       , pointer :: prshis
    character(23)                       , pointer :: selhis
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
    integer                             , pointer :: itmapc  ! Current time counter for the map file 
    integer                             , pointer :: itrstc  ! Current time counter for the restart file. Start writing after first interval is passed. Last time will always be written to file for ITRSTI > 0 
    integer                             , pointer :: npmap   ! Current array counter for printing map data 
    integer                             , pointer :: ntcur   ! Total number of timesteps on comm. file (to write to) 
    real(fp)                            , pointer :: dtsec   ! DT in seconds 
!   variables copied from postpr.f90
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
    integer                              , pointer :: nmaxus
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
    real(fp)                             , pointer :: bed
    real(fp)                             , pointer :: tmor
    integer                              , pointer :: itmor
    type (moroutputtype)                 , pointer :: moroutput
    logical                              , pointer :: multi
    logical                              , pointer :: first
    integer                              , pointer :: nuprpg
    integer                              , pointer :: nuprln
    character(131)   , dimension(:)      , pointer :: header
    logical                              , pointer :: wind
    logical                              , pointer :: culvert
    logical                              , pointer :: dredge
    logical                              , pointer :: drogue
    logical                              , pointer :: wave
    logical                              , pointer :: waveol
    logical                              , pointer :: sedim
    logical                              , pointer :: coupleact
    logical                              , pointer :: couplemod
    logical                              , pointer :: zmodel
    logical                              , pointer :: roller
    logical                              , pointer :: xbeach
    integer(pntrsize)                    , pointer :: alfas
    integer(pntrsize)                    , pointer :: areau
    integer(pntrsize)                    , pointer :: areav
    integer(pntrsize)                    , pointer :: atr
    integer(pntrsize)                    , pointer :: c
    integer(pntrsize)                    , pointer :: cfurou
    integer(pntrsize)                    , pointer :: cfvrou
    integer(pntrsize)                    , pointer :: cvalu0
    integer(pntrsize)                    , pointer :: cvalv0
    integer(pntrsize)                    , pointer :: ctr
    integer(pntrsize)                    , pointer :: dicuv
    integer(pntrsize)                    , pointer :: dicww
    integer(pntrsize)                    , pointer :: dis
    integer(pntrsize)                    , pointer :: disch
    integer(pntrsize)                    , pointer :: discum
    integer(pntrsize)                    , pointer :: dp
    integer(pntrsize)                    , pointer :: dpsed
    integer(pntrsize)                    , pointer :: dtr
    integer(pntrsize)                    , pointer :: enstro
    integer(pntrsize)                    , pointer :: entr
    integer(pntrsize)                    , pointer :: eroll1
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
    integer(pntrsize)                    , pointer :: mndro
    integer(pntrsize)                    , pointer :: patm
    integer(pntrsize)                    , pointer :: precip
    integer(pntrsize)                    , pointer :: qu
    integer(pntrsize)                    , pointer :: qv
    integer(pntrsize)                    , pointer :: qxkr
    integer(pntrsize)                    , pointer :: qxkw
    integer(pntrsize)                    , pointer :: qykr
    integer(pntrsize)                    , pointer :: qykw
    integer(pntrsize)                    , pointer :: r1
    integer(pntrsize)                    , pointer :: rbuff
    integer(pntrsize)                    , pointer :: rho
    integer(pntrsize)                    , pointer :: rich
    integer(pntrsize)                    , pointer :: rint
    integer(pntrsize)                    , pointer :: rlabda
    integer(pntrsize)                    , pointer :: rsed
    integer(pntrsize)                    , pointer :: rtur1
    integer(pntrsize)                    , pointer :: sbtr
    integer(pntrsize)                    , pointer :: sbtrc
    integer(pntrsize)                    , pointer :: sbuu
    integer(pntrsize)                    , pointer :: sbvv
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
    integer(pntrsize)                    , pointer :: umnldf
    integer(pntrsize)                    , pointer :: uorb
    integer(pntrsize)                    , pointer :: v0
    integer(pntrsize)                    , pointer :: vicuv
    integer(pntrsize)                    , pointer :: vicww
    integer(pntrsize)                    , pointer :: vmnldf
    integer(pntrsize)                    , pointer :: voldis
    integer(pntrsize)                    , pointer :: volum1
    integer(pntrsize)                    , pointer :: vortic
    integer(pntrsize)                    , pointer :: w1
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
    integer(pntrsize)                    , pointer :: zkfs
    integer(pntrsize)                    , pointer :: zqxk
    integer(pntrsize)                    , pointer :: zqyk
    integer(pntrsize)                    , pointer :: zrca
    integer(pntrsize)                    , pointer :: zrho
    integer(pntrsize)                    , pointer :: zrich
    integer(pntrsize)                    , pointer :: zrsdeq
    integer(pntrsize)                    , pointer :: zsbu
    integer(pntrsize)                    , pointer :: zsbv
    integer(pntrsize)                    , pointer :: zssu
    integer(pntrsize)                    , pointer :: zssv
    integer(pntrsize)                    , pointer :: ztauet
    integer(pntrsize)                    , pointer :: ztauks
    integer(pntrsize)                    , pointer :: ztur
    integer(pntrsize)                    , pointer :: zvicww
    integer(pntrsize)                    , pointer :: zvort
    integer(pntrsize)                    , pointer :: zwl
    integer(pntrsize)                    , pointer :: zws
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
    integer(pntrsize)                    , pointer :: evap
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
!
!
! Call variables
!
    integer                             :: nst           ! Current time step counter 
    integer                             :: nlb
    integer                             :: nub
    integer                             :: mlb
    integer                             :: mub
    integer                             :: kmax
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in) :: s1 
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(in) :: u1
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                           , intent(in) :: v1
    real(prec), dimension(nlb:nub,mlb:mub)                              , intent(in) :: dps
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in) :: dpu 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in) :: dpv 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in) :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in) :: hv 
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(in) :: qxk  
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(in) :: qyk 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in) :: Umean 
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in) :: Vmean 
!
! Local variables
!
    logical                                       :: error         ! Flag=TRUE if an error is encountered 
!   Local variables copied from postpr.f90
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
    integer                  :: msteps
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
    logical                  :: halftime
!
!! executable statements  -------------------------------------------------------
!
    lundia              => gdp%gdinout%lundia
    lunprt              => gdp%gdinout%lunprt
    rhow                => gdp%gdphysco%rhow
    ktemp               => gdp%gdtricom%ktemp
    grdang              => gdp%gdtricom%grdang
    prsmap              => gdp%gdtricom%prsmap
    selmap              => gdp%gdtricom%selmap
    prshis              => gdp%gdtricom%prshis
    selhis              => gdp%gdtricom%selhis
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
    itmapc              => gdp%gdtricom%itmapc
    itrstc              => gdp%gdtricom%itrstc
    npmap               => gdp%gdtricom%npmap
    ntcur               => gdp%gdtricom%ntcur
    dtsec               => gdp%gdtricom%dtsec
!    executable statements copied from postpr.f90
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
    nmaxus              => gdp%d%nmaxus
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
    bed                 => gdp%gdmorpar%bed
    tmor                => gdp%gdmorpar%tmor
    itmor               => gdp%gdmorpar%itmor
    moroutput           => gdp%gdmorpar%moroutput
    multi               => gdp%gdmorpar%multi
    nuprpg              => gdp%gdpostpr%nuprpg
    nuprln              => gdp%gdpostpr%nuprln
    header              => gdp%gdpostpr%header
    wind                => gdp%gdprocs%wind
    culvert             => gdp%gdprocs%culvert
    dredge              => gdp%gdprocs%dredge
    drogue              => gdp%gdprocs%drogue
    wave                => gdp%gdprocs%wave
    waveol              => gdp%gdprocs%waveol
    sedim               => gdp%gdprocs%sedim
    coupleact           => gdp%gdprocs%coupleact
    couplemod           => gdp%gdprocs%couplemod
    zmodel              => gdp%gdprocs%zmodel
    roller              => gdp%gdprocs%roller
    xbeach              => gdp%gdprocs%xbeach
    alfas               => gdp%gdr_i_ch%alfas
    areau               => gdp%gdr_i_ch%areau
    areav               => gdp%gdr_i_ch%areav
    atr                 => gdp%gdr_i_ch%atr
    c                   => gdp%gdr_i_ch%c
    cfurou              => gdp%gdr_i_ch%cfurou
    cfvrou              => gdp%gdr_i_ch%cfvrou
    cvalu0              => gdp%gdr_i_ch%cvalu0
    cvalv0              => gdp%gdr_i_ch%cvalv0
    ctr                 => gdp%gdr_i_ch%ctr
    dicuv               => gdp%gdr_i_ch%dicuv
    dicww               => gdp%gdr_i_ch%dicww
    dis                 => gdp%gdr_i_ch%dis
    disch               => gdp%gdr_i_ch%disch
    discum              => gdp%gdr_i_ch%discum
    dp                  => gdp%gdr_i_ch%dp
    dpsed               => gdp%gdr_i_ch%dpsed
    dtr                 => gdp%gdr_i_ch%dtr
    enstro              => gdp%gdr_i_ch%enstro
    entr                => gdp%gdr_i_ch%entr
    eroll1              => gdp%gdr_i_ch%eroll1
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
    qu                  => gdp%gdr_i_ch%qu
    qv                  => gdp%gdr_i_ch%qv
    qxkr                => gdp%gdr_i_ch%qxkr
    qxkw                => gdp%gdr_i_ch%qxkw
    qykr                => gdp%gdr_i_ch%qykr
    qykw                => gdp%gdr_i_ch%qykw
    patm                => gdp%gdr_i_ch%patm
    precip              => gdp%gdr_i_ch%precip
    r1                  => gdp%gdr_i_ch%r1
    rbuff               => gdp%gdr_i_ch%rbuff
    rho                 => gdp%gdr_i_ch%rho
    rich                => gdp%gdr_i_ch%rich
    rint                => gdp%gdr_i_ch%rint
    rlabda              => gdp%gdr_i_ch%rlabda
    rsed                => gdp%gdr_i_ch%rsed
    rtur1               => gdp%gdr_i_ch%rtur1
    sbtr                => gdp%gdr_i_ch%sbtr
    sbtrc               => gdp%gdr_i_ch%sbtrc
    sbuu                => gdp%gdr_i_ch%sbuu
    sbvv                => gdp%gdr_i_ch%sbvv
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
    umnldf              => gdp%gdr_i_ch%umnldf
    uorb                => gdp%gdr_i_ch%uorb
    v0                  => gdp%gdr_i_ch%v0
    evap                => gdp%gdr_i_ch%evap
    vicuv               => gdp%gdr_i_ch%vicuv
    vicww               => gdp%gdr_i_ch%vicww
    vmnldf              => gdp%gdr_i_ch%vmnldf
    voldis              => gdp%gdr_i_ch%voldis
    volum1              => gdp%gdr_i_ch%volum1
    vortic              => gdp%gdr_i_ch%vortic
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
    zkfs                => gdp%gdr_i_ch%zkfs
    zqxk                => gdp%gdr_i_ch%zqxk
    zqyk                => gdp%gdr_i_ch%zqyk
    zrca                => gdp%gdr_i_ch%zrca
    zrho                => gdp%gdr_i_ch%zrho
    zrich               => gdp%gdr_i_ch%zrich
    zrsdeq              => gdp%gdr_i_ch%zrsdeq
    zsbu                => gdp%gdr_i_ch%zsbu
    zsbv                => gdp%gdr_i_ch%zsbv
    zssu                => gdp%gdr_i_ch%zssu
    zssv                => gdp%gdr_i_ch%zssv
    ztauet              => gdp%gdr_i_ch%ztauet
    ztauks              => gdp%gdr_i_ch%ztauks
    ztur                => gdp%gdr_i_ch%ztur
    zvicww              => gdp%gdr_i_ch%zvicww
    zvort               => gdp%gdr_i_ch%zvort
    zwl                 => gdp%gdr_i_ch%zwl
    zws                 => gdp%gdr_i_ch%zws
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
    !
    ! Call postpr
    !
    error = .false.
    halftime = .true.
    !
    if (itmapi > 0) then
       if (nst == itmapc) then

          call psemnefis
          call timer_start(timer_postpr, gdp)
!          call wrtmap(lundia       ,error     ,trifil    ,selmap    ,itmapc      , &    
!                     & rhow        ,mmax      , &                                       
!                     & kmax        ,nmaxus    ,lstsci    ,ltur      , &                 
!                     & nsrc        ,zmodel    ,i(kcs)    ,i(kfs)    ,i(kfu)      , &    
!                     & i(kfv)      ,i(kfumin) ,i(kfvmin) ,i(kfumax) ,i(kfvmax)   , &    
!                     & i(kfsmin)   ,i(kfsmax) ,i(mnksrc) ,i(ibuff)  ,s1          , &    
!                     & dps         ,r(dzs1)   ,r(thick)  , &                            
!                     & u1          ,v1        ,r(w1)     ,r(wphy)   ,r(r1)       , &    
!                     & r(rtur1)    ,r(taubpu) ,r(taubpv) ,r(taubsu) ,r(taubsv)   , &    
!                     & r(vicww)    ,r(dicww)  ,r(rich)   ,r(rho)    ,r(p1)       , &    
!                     & r(vortic)   ,r(enstro) ,r(umnldf) ,r(vmnldf) ,r(vicuv)    , &    
!                     & r(taubmx)   ,r(windu)  ,r(windv)  ,velt      ,r(cvalu0)   , &    
!                     & r(cvalv0)   ,r(cfurou) ,r(cfvrou) ,rouflo    ,r(patm)     , &    
!                     & r(z0ucur)   ,r(z0vcur) ,r(z0urou) ,r(z0vrou) ,ktemp       , &    
!                     & r(precip)   ,r(evap)   ,qxk       ,qyk       ,dpu         , &    
!                     & dpv         ,hu        ,hv        ,r(guu)    ,r(gvv)    , &
!                     & gdp       )                  
          call timer_stop(timer_postpr, gdp)
          call vsemnefis
          !
          if (.not.halftime .and. nst==itmapc .and. itmapc+itmapi<=itmapl) then
             itmapc = itmapc + itmapi
          endif
       endif
    endif
    !
    !if (error) return
  end subroutine postpr_ghost
