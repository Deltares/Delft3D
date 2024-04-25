subroutine heatu(ktemp     ,anglat    ,sferic    ,timhr     ,keva      , &
               & ltem      ,lstsci    ,icx       ,icy       , &
               & nmmax     ,kmax      ,kfs       ,kfsmx0    ,kfsmax    , &
               & kfsmin    ,kspu      ,kspv      ,dzs0      ,dzs1      , &
               & sour      ,sink      ,r0        ,evap      ,dps       , &
               & s0        ,s1        ,thick     ,w10mag    ,patm      , &
               & xcor      ,ycor      ,gsqs      ,xz        ,yz        , &
               & anglon    ,gdp       )
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
!    Function: Computes heat exchange through water surface
!              Evaporation calculated or obtained from input file (time series)
!              No heat exchange through the free surface for
!              floating structure (compare with subroutine FILTERSTRUCTURES)
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use meteo
    use precision
    use mathconsts
    use physicalconsts, only: CtoKelvin
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
    real(fp)                , pointer :: eps
    real(sp)                , pointer :: smiss
    integer                 , pointer :: itdate
    integer                 , pointer :: ntstep
    real(fp)                , pointer :: tzone
    type (flwoutputtype)    , pointer :: flwoutput
    real(fp)                , pointer :: cfrcon
    real(fp)                , pointer :: cp
    real(fp)                , pointer :: sarea
    real(fp)                , pointer :: fclou
    real(fp) , dimension(:) , pointer :: secchi
    real(fp)                , pointer :: timjan
    real(fp)                , pointer :: stanton
    real(fp)                , pointer :: dalton
    real(fp)                , pointer :: mulsd
    real(fp)                , pointer :: betasd
    real(fp)                , pointer :: albedo
    real(fp)                , pointer :: qtotmx
    real(fp)                , pointer :: lambda
    real(fp)                , pointer :: rhum
    real(fp)                , pointer :: tdryb
    real(fp)                , pointer :: qsun
    real(fp)                , pointer :: qradin
    real(fp)                , pointer :: tback
    real(fp)                , pointer :: tair
    real(fp)                , pointer :: cfclou
    real(fp)                , pointer :: vapres
    real(fp) , dimension(:) , pointer :: qeva_out
    real(fp) , dimension(:) , pointer :: qco_out
    real(fp) , dimension(:) , pointer :: qbl_out
    real(fp) , dimension(:) , pointer :: qin_out
    real(fp) , dimension(:) , pointer :: qnet_out
    real(fp) , dimension(:) , pointer :: hlc_out
    real(fp) , dimension(:) , pointer :: hfree_out
    real(fp) , dimension(:) , pointer :: efree_out
    real(fp) , dimension(:) , pointer :: qmis_out
    integer                 , pointer :: ivapop
    real(fp) , dimension(:) , pointer :: rhumarr
    real(fp) , dimension(:) , pointer :: tairarr
    real(fp) , dimension(:) , pointer :: clouarr
    real(fp) , dimension(:) , pointer :: swrfarr
    logical  , dimension(:) , pointer :: flbcktemp
    logical                 , pointer :: rhum_file
    logical                 , pointer :: tair_file
    logical                 , pointer :: clou_file
    logical                 , pointer :: swrf_file
    logical                 , pointer :: scc_file
    logical                 , pointer :: free_convec
    logical                 , pointer :: solrad_read
    integer                 , pointer :: lundia
    real(fp)                , pointer :: dryflc
    real(fp)                , pointer :: rhow
    real(fp)                , pointer :: rhoa
    real(fp)                , pointer :: ag
    real(fp)                , pointer :: sboltz
    logical                 , pointer :: wind
    logical                 , pointer :: temp
    logical                 , pointer :: wave
    logical                 , pointer :: struct
    logical                 , pointer :: zmodel
!
! Global variables
!
    integer                                                     , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                                        !!  then computation proceeds in the X-dir.
                                                                                        !!  If icx=1 then computation proceeds in the Y-dir.
    integer                                                     , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                     , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                     , intent(in)  :: keva   !  Description and declaration in tricom.igs
    integer                                                     , intent(in)  :: ktemp  !  Description and declaration in tricom.igs
    integer                                                     , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                     , intent(in)  :: ltem   !  Description and declaration in dimens.igs
    integer                                                     , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                , intent(in)  :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                , intent(in)  :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                , intent(in)  :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)        , intent(in)  :: kspu   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)        , intent(in)  :: kspv   !  Description and declaration in esm_alloc_int.f90
    logical                                                     , intent(in)  :: sferic !  Description and declaration in tricom.igs
    real(fp)                                                    , intent(in)  :: anglat !!  - Angle of latitude of the model centre
                                                                                        !!    (used to determine the coef. for the coriolis force)
                                                                                        !!  - In spherical coordinates this parameter equals the angle of latitude
                                                                                        !!    for the origin (water level point) after INIPHY anglat = 0.
    real(fp)                                                    , intent(in)  :: anglon !!  - Angle of longitude of the model centre
    real(fp)                                                    , intent(in)  :: timhr  !!  Current timestep (in hours), TIMNOW * DTSEC / 3600.
    real(prec) , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)                             :: evap   !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: patm   !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: w10mag !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: ycor   !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: xz     !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: yz     !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: dzs0   !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: dzs1   !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci) , intent(in)  :: r0     !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)               :: sink   !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)               :: sour   !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(kmax)                                , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer       :: istat
    integer       :: k
    integer       :: k0
    integer       :: k1
    integer       :: k2
    integer       :: kstep
    integer       :: l
    integer       :: m
    integer       :: msgcount
    integer       :: n
    integer       :: ndm
    integer       :: nm
    integer       :: nmd
    real(fp)      :: b1      ! Transmission coefficient 
    real(fp)      :: bowrat
    real(fp)      :: cccoef  ! Coefficient for cloud cover 
    real(fp)      :: corr
    real(fp)      :: d       ! Declination angle at given time (radians)
    real(fp)      :: decln   ! Maximum declination angle of the earth
    real(fp)      :: delvap  ! Vapour pressure difference (see also Rob Uittenbogaard's 1DV model
    real(fp)      :: eal     ! Vapour pressure in air remote for given humidity
    real(fp)      :: easp
    real(fp)      :: efree   ! Free convection of latent heat
    real(fp)      :: em      ! Emissivity  0.985
    real(fp)      :: esvp
    real(fp)      :: ew      ! Saturation pressure of water vapour near water surface
    real(fp)      :: ewl     ! Saturation pressure of water vapour in air remote
    real(fp)      :: extinc  ! Extinction coefficient light absorbed
    real(fp)      :: extish  ! Extinction coefficient light absorbed in shallow part
    real(fp)      :: extide  ! Extinction coefficient light absorbed in deep part 
    real(fp)      :: explo   ! Exponential value bottom of layer
    real(fp)      :: expup   ! Exponential value top of layer
    real(fp)      :: ffclou
    real(fp)      :: fheat
    real(fp)      :: flux
    real(fp)      :: fwind
    real(fp)      :: gred    ! Reduced gravity
    real(fp)      :: h0new
    real(fp)      :: h0old
    real(fp)      :: hcp     ! Specific heat capacity 1004. [j/kg/K] 
    real(fp)      :: hfree   ! Free convection of sensible heat
    real(fp)      :: hlc
    real(fp)      :: htrsh
    real(fp)      :: pr2
    real(fp)      :: prair
    real(fp)      :: presa   ! Actual atmospheric pressure at Water- level points (mbar)
    real(fp)      :: pvap    ! Vapor pressure
    real(fp)      :: qa      ! Specific humidity of air at sst
    real(fp)      :: qal     ! Specific humidity of air remote
    real(fp)      :: qan     ! Heat flux atmosheric long wave radiation
    real(fp)      :: qbl     ! Heat flux back radiation sea surface
    real(fp)      :: qco     ! Heat flux convection
    real(fp)      :: qeva    ! Heat flux evaporation
    real(fp)      :: qin     ! Heat flux solar radiation
    real(fp)      :: qink
    real(fp)      :: ql      ! Total heat loss 
    real(fp)      :: qle
    real(fp)      :: qltemp
    real(fp)      :: qs      ! Solar radiation at the sea surface [W/m2] 
    real(fp)      :: qsn     ! Heat flux solar (short wave) radiation 
    real(fp)      :: qtot    ! Total heat flux 
    real(fp)      :: qtotk
    real(fp)      :: qw      ! Specific humidity of air at air temperature
    real(fp)      :: ratio   ! coefficient in exponential relation	
    real(fp)      :: rcpa
    real(fp)      :: rdry
    real(fp)      :: rhoa0
    real(fp)      :: rhoa10
    real(fp)      :: rlon    ! Longitude positive E. For SFERIC RLON=XCOR (NM) else RLON is not used
    real(fp)      :: rlat    ! Latitude positive N. For SFERIC RLAT=YCOR (NM) else RLAT=ANGLAT
    real(fp)      :: rvap
    real(fp)      :: sc      ! Solar constant  1368.0 [W/m2]
    real(fp)      :: snh     ! Sine of altitude angle of the sun
    real(fp)      :: sq_ea
    real(fp)      :: sq_eal  ! Square root of vapour pressure in air remote for given humidity
    real(fp)      :: time    ! time in minutes
    real(fp)      :: tkcube
    real(fp)      :: tkelvi
    real(fp)      :: tkelvn
    real(fp)      :: tl      ! Latent heat [j/kg]
    real(fp)      :: tm0     ! GMT time in hours after midnight January first (TIMJAN + TIMHR)
    real(fp)      :: tm      ! Actual time in hours after midnight January first (TIMJAN + TIMHR + TIMEZONE)
    real(fp)      :: w0      ! Angular frequency of one year period (/hours)
    real(fp)      :: w1      ! Angular frequency of one day period (/hours)
    real(fp)      :: xnuair
    real(fp)      :: zbottom
    real(fp)      :: zdown
    real(fp)      :: ztop
    real(fp), parameter :: fCtoKelvin = real(CtoKelvin, fp)
    logical       :: success
    character(100):: errmsg
!
!! executable statements -------------------------------------------------------
!
    eps         => gdp%gdconst%eps
    smiss       => gdp%gdconst%smiss
    itdate      => gdp%gdexttim%itdate
    ntstep      => gdp%gdinttim%ntstep
    tzone       => gdp%gdexttim%tzone
    flwoutput   => gdp%gdflwpar%flwoutput
    cfrcon      => gdp%gdheat%cfrcon
    cp          => gdp%gdheat%cp
    sarea       => gdp%gdheat%sarea
    fclou       => gdp%gdheat%fclou
    secchi      => gdp%gdheat%secchi
    timjan      => gdp%gdheat%timjan
    stanton     => gdp%gdheat%stanton
    dalton      => gdp%gdheat%dalton
    mulsd       => gdp%gdheat%mulsd
    betasd      => gdp%gdheat%betasd
    albedo      => gdp%gdheat%albedo
    qtotmx      => gdp%gdheat%qtotmx
    lambda      => gdp%gdheat%lambda
    rhum        => gdp%gdheat%rhum
    tdryb       => gdp%gdheat%tdryb
    qsun        => gdp%gdheat%qsun
    qradin      => gdp%gdheat%qradin
    tback       => gdp%gdheat%tback
    tair        => gdp%gdheat%tair
    cfclou      => gdp%gdheat%cfclou
    vapres      => gdp%gdheat%vapres
    qeva_out    => gdp%gdheat%qeva_out
    qco_out     => gdp%gdheat%qco_out
    qbl_out     => gdp%gdheat%qbl_out
    qin_out     => gdp%gdheat%qin_out
    qnet_out    => gdp%gdheat%qnet_out
    hlc_out     => gdp%gdheat%hlc_out
    hfree_out   => gdp%gdheat%hfree_out
    efree_out   => gdp%gdheat%efree_out
    qmis_out    => gdp%gdheat%qmis_out
    ivapop      => gdp%gdheat%ivapop
    rhumarr     => gdp%gdheat%rhumarr
    tairarr     => gdp%gdheat%tairarr
    clouarr     => gdp%gdheat%clouarr
    swrfarr     => gdp%gdheat%swrfarr
    rhum_file   => gdp%gdheat%rhum_file
    tair_file   => gdp%gdheat%tair_file
    clou_file   => gdp%gdheat%clou_file
    swrf_file   => gdp%gdheat%swrf_file
    scc_file    => gdp%gdheat%scc_file
    free_convec => gdp%gdheat%free_convec
    solrad_read => gdp%gdheat%solrad_read
    lundia      => gdp%gdinout%lundia
    dryflc      => gdp%gdnumeco%dryflc
    rhow        => gdp%gdphysco%rhow
    rhoa        => gdp%gdphysco%rhoa
    ag          => gdp%gdphysco%ag
    sboltz      => gdp%gdphysco%sboltz
    wind        => gdp%gdprocs%wind
    temp        => gdp%gdprocs%temp
    wave        => gdp%gdprocs%wave
    struct      => gdp%gdprocs%struct
    zmodel      => gdp%gdprocs%zmodel
    flbcktemp   => gdp%gdheat%flbcktemp
    !
    msgcount = 0
    htrsh    = 0.5_fp * dryflc
    !
    if (rhum_file .or. tair_file .or. clou_file .or. swrf_file .or. scc_file) then
       !
       ! update meteo input (if necessary)
       !
       time    = timhr * 60.0_fp
       success = meteoupdate(gdp%runid, itdate, tzone, time)
       call checkmeteoresult(success, gdp)
       if (rhum_file) then
          success = getmeteoval(gdp%runid, 'relhum', time, gdp%gdparall%mfg, gdp%gdparall%nfg, &
                              & gdp%d%nlb, gdp%d%nub, gdp%d%mlb, gdp%d%mub, rhumarr , 0)
          call checkmeteoresult(success, gdp)
       endif
       if (tair_file) then
          success = getmeteoval(gdp%runid, 'airtemp', time, gdp%gdparall%mfg, gdp%gdparall%nfg,&
                              & gdp%d%nlb, gdp%d%nub, gdp%d%mlb, gdp%d%mub, tairarr , 0 )
          call checkmeteoresult(success, gdp)
       endif
       if (clou_file) then
          success = getmeteoval(gdp%runid, 'cloud', time, gdp%gdparall%mfg, gdp%gdparall%nfg,&
                              & gdp%d%nlb, gdp%d%nub, gdp%d%mlb, gdp%d%mub, clouarr , 0 )
          call checkmeteoresult(success, gdp)
       endif
       if (swrf_file) then
          success = getmeteoval(gdp%runid, 'swrf', time, gdp%gdparall%mfg, gdp%gdparall%nfg,&
                              & gdp%d%nlb, gdp%d%nub, gdp%d%mlb, gdp%d%mub, swrfarr , 0 )
          call checkmeteoresult(success, gdp)
       endif
       if (scc_file) then
          success = getmeteoval(gdp%runid, 'Secchi_depth', time, gdp%gdparall%mfg, gdp%gdparall%nfg,&          !timhr*60.0_fp?
                              & gdp%d%nlb, gdp%d%nub, gdp%d%mlb, gdp%d%mub, secchi , 0 )
          call checkmeteoresult(success, gdp)
       endif
    endif
    !
    !
    ! WIND COEFFICIENT, FOLLOWING H.E. SWEERS
    ! 'A MONOGRAM TO ESTIMATE THE HEAT-EXCHANGE COEFFICIENT AT
    !  THE AIR-WATER INTERFACE AS A FUNCTION OF WINDSPEED AND
    !  TEMPERATURE', JOURNAL OF HYDROLOGY, 30, 1976.
    !
    ! fwind=(5.0e6/sarea)**0.05*(3.5+2.05*w10mag(nm))
    !
    ! Wind at 10 M is space varying, hence second part of FWIND will be
    ! calculated in loop 100
    !
    if (sarea > 1.0_fp) then
       fwind = (5.0e6_fp/sarea)**0.05_fp
    else
       fwind = 1.0_fp
    endif
    !
    !
do l=1,lstsci
   if ( l==ltem .or. flbcktemp(l) ) then   
    !
    !
    ! ---------------------------------------------------------------
    ! KTEMP=1
    !
    ! ABSOLUTE TEMPERATURE MODEL
    ! No heat exchange when nm is a floating structure:
    ! (kspu(nm,0)=kspu(nmd,0)=kspv(nm,0)=kspv(ndm,0)=2)
    ! ---------------------------------------------------------------
    !
    if (ktemp == 1) then
       do nm = 1, nmmax
          nmd = nm - icx
          ndm = nm - icy
          if (kfs(nm)>0 .and. (  kspu(nm,0)/=2 .or. kspu(nmd,0)/=2 .or. &
                                  & kspv(nm, 0) /= 2 .or. kspv(ndm, 0) /= 2     ) ) then
             !
             if (zmodel) then
                k0 = kfsmx0(nm)
             else
                k0 = 1
             endif
             !
             if (rhum_file) rhum = rhumarr(nm)
             if (tair_file) tair = tairarr(nm)
             !
             ! fwind=(5.0e6/sarea)**0.05*(3.5+2.05*w10mag(nm))
             !
             ! EVAPORATION in kg/(m**2 s)
             !
             ! Latent heat tl
             !
             tl = 2.5e6_fp - 2.3e3_fp*r0(nm,k0,l)
             !
             easp     = (rhum/100.0_fp) * 23.38_fp * exp( 18.1_fp - 5303.3_fp/(tdryb      +fCtoKelvin) )
             esvp     =                   23.38_fp * exp( 18.1_fp - 5303.3_fp/(r0(nm,k0,l)+fCtoKelvin) )
             !
             ! No negative evaporation
             ! Assumption: Negative evaporation is caused by
             ! "poor" modelling/parameter choice and not by the actual physical process
             ! of water condensation out ot the air into the water
             !
             delvap = max(0.0_fp , esvp-easp)
             !
             ! Previous formulation, with EVAP computed in caleva.f90:
             ! qeva = evap(nm) * tl * rhoa 
             !
             select case (keva)
             case (0,1)
                !
                ! No evap from fileva, evap and qeva calculated internally
                !
                qeva     = fwind * (3.5_fp + 2.05_fp*w10mag(nm)) * delvap
                evap(nm) = qeva / tl
             case (2)
                !
                ! evap from fileva, qeva based on read evap
                !
                qeva = evap(nm) * tl
             case (3)
                !
                ! evap from fileva, qeva is calculated internally
                !
                qeva = fwind * (3.5_fp + 2.05_fp*w10mag(nm)) * delvap
             case default
                ! nothing
             end select
             !
             ! HEAT FLUX CONVECTION
             !
             qco = 0.61_fp * (r0(nm, k0, l) - tdryb) * fwind * (3.5_fp + 2.05_fp*w10mag(nm))
             !
             ! BACK RADIATION
             !
             qbl = 303.0_fp + 5.2_fp*r0(nm, k0, l)
             !
             ! TOTAL HEAT LOSS
             !
             ql = qbl + qeva + qco
             !
             ! SOLAR RADIATION FOLLOWING LEE
             ! 'EVALUATION OF SOLAR BEAM IRRADIATION AS A CLIMATE
             !  PARAMETER OF MOUNTAIN WATER SHEDS', HYDROLOGY PAPERS,
             !  COLORADO STATE UNIVERSITY, FORT COLLINS, COLORADO 1963.
             !
             qsn = 0.94_fp * qsun * (1.0_fp - 0.65_fp*fclou**2)
             !
             ! ATMOSPHERIC RADIATION
             !
             qan = (218.0_fp + 6.3_fp*tdryb) * (1.0_fp + 0.17_fp*fclou**2)
             qin = qsn + qan
             !
             qtot = (qin - ql) / (rhow*cp)
             if (zmodel) then
                sour(nm, k0, l) = sour(nm, k0, l) + qtot*gsqs(nm)
             else
                h0old              = max( htrsh, s0(nm)+real(dps(nm),fp) ) * thick(k0)
                sour(nm, k0, l) = sour(nm, k0, l) + qtot/h0old
             endif
             !
             ! Filling of output arrays
             !
             if (flwoutput%temperature) then
                qeva_out(nm) = -qeva
                qco_out(nm)  = -qco
                qbl_out(nm)  = -qbl
                qin_out(nm)  =  qin
                qnet_out(nm) =  qin - ql
                if (keva == 3) then
                   !
                   ! qeva           : calculated (and used)
                   ! evap*tl        : derived from input
                   ! qeva - evap*tl : mismatch between calculated and derived heat flux
                   !
                   qmis_out(nm) = qeva - evap(nm)*tl
                endif
             endif
          else  ! dry points
             if (flwoutput%temperature) then
                qeva_out(nm)  = 0.0_fp
                qco_out(nm)   = 0.0_fp
                qbl_out(nm)   = 0.0_fp
                qin_out(nm)   = 0.0_fp
                qnet_out(nm)  = 0.0_fp
                if (keva == 3) then
                   qmis_out(nm) = 0.0_fp
                endif
             endif
          endif
       enddo
    !
    ! ---------------------------------------------------------------
    ! KTEMP=2
    !
    ! ABSOLUTE TEMPERATURE MODEL
    !
    ! No heat exchange when nm is a floating structure:
    ! (kspu(nm,0)=kspu(nmd,0)=kspv(nm,0)=kspv(ndm,0)=2)
    ! ---------------------------------------------------------------
    !
    elseif (ktemp == 2) then
       !
       do nm = 1, nmmax
          nmd = nm - icx
          ndm = nm - icy
          if (kfs(nm)>0 .and. (  kspu(nm,0)/=2 .or. kspu(nmd,0)/=2 .or. &
                                  & kspv(nm, 0) /= 2 .or. kspv(ndm, 0) /= 2     ) ) then            
             !
             if (zmodel) then
                k0 = kfsmx0(nm)
             else
                k0 = 1
             endif
             !
             if (rhum_file) rhum = rhumarr(nm)
             if (tair_file) tair = tairarr(nm)
             !
             ! fwind=(5.0e6/sarea)**0.05*(3.5+2.05*w10mag(nm))
             !
             ! EVAPORATION in kg/(m**2 s) 
             !
             ! Latent heat tl
             !
             tl = 2.5e6_fp - 2.3e3_fp*r0(nm,k0,l)
             !
             easp = (rhum/100.0_fp) * 23.38_fp * exp( 18.1_fp - 5303.3_fp/(tdryb      +fCtoKelvin) )
             esvp =                   23.38_fp * exp( 18.1_fp - 5303.3_fp/(r0(nm,k0,l)+fCtoKelvin) )
             !
             ! No negative evaporation
             ! Assumption: Negative evaporation is caused by
             ! "poor" modelling/parameter choice and not by the actual physical process
             ! of water condensation out of the air into the water
             !
             delvap = max(0.0_fp , esvp-easp)
             !
             ! Previous formulation, with EVAP computed in caleva.f90:
             ! qeva = evap(nm) * tl * rhoa
             !
             select case (keva)
             case (0,1)
                !
                ! No evap from fileva, evap and qeva calculated internally
                !
                qeva     = fwind * (3.5_fp + 2.05_fp*w10mag(nm)) * delvap
                evap(nm) = qeva / tl
             case (2)
                !
                ! evap from fileva, qeva based on read evap
                !
                qeva = evap(nm) * tl
             case (3)
                !
                ! evap from fileva, qeva is calculated internally
                !
                qeva = fwind * (3.5_fp + 2.05_fp*w10mag(nm)) * delvap
             case default
                ! nothing
             end select
             !
             ! HEAT FLUX CONVECTION
             !
             qco = 0.61_fp * ( r0(nm, k0, l) - tdryb ) * fwind * (3.5_fp + 2.05_fp*w10mag(nm))
             !
             ! BACK RADIATION
             !
             qbl = 303.0_fp + 5.2_fp*r0(nm, k0, l)
             !
             ! TOTAL HEAT LOSS
             !
             ql = qbl + qeva + qco
             !
             ! TOTAL RADIATION (Short + long wave)
             !
             qin = qradin
             !
             qtot = (qin - ql) / (rhow*cp)
             if (zmodel) then
                sour(nm, k0, l) = sour(nm, k0, l) + qtot*gsqs(nm)
             else
                h0old              = max(htrsh, s0(nm)+real(dps(nm),fp) ) * thick(k0)
                sour(nm, k0, l) = sour(nm, k0, l) + qtot/h0old
             endif
             !
             ! Filling of output arrays
             !
             if (flwoutput%temperature) then
                qeva_out(nm) = -qeva
                qco_out(nm)  = -qco
                qbl_out(nm)  = -qbl
                qin_out(nm)  =  qin
                qnet_out(nm) =  qin - ql
                if (keva == 3) then
                   !
                   ! qeva           : calculated (and used)
                   ! evap*tl        : derived from input
                   ! qeva - evap*tl : mismatch between calculated and derived heat flux
                   !
                   qmis_out(nm) = qeva - evap(nm)*tl
                endif
             endif
          else  ! dry points
             if (flwoutput%temperature) then
                qeva_out(nm)  = 0.0_fp
                qco_out(nm)   = 0.0_fp
                qbl_out(nm)   = 0.0_fp
                qin_out(nm)   = 0.0_fp
                qnet_out(nm)  = 0.0_fp
                if (keva == 3) then
                   qmis_out(nm) = 0.0_fp
                endif
             endif
          endif
       enddo
    !
    ! ---------------------------------------------------------------
    ! KTEMP=3  QTOT=QLE+QS
    !
    ! EXCESS TEMPERATURE MODEL
    ! No heat exchange when nm is a floating structure:
    ! (kspu(nm,0)=kspu(nmd,0)=kspv(nm,0)=kspv(ndm,0)=2)
    ! ---------------------------------------------------------------
    !
    elseif (ktemp==3) then
       !
       do nm = 1, nmmax
          nmd = nm - icx
          ndm = nm - icy
          if (kfs(nm)>0 .and. (  kspu(nm,0)/=2 .or. kspu(nmd,0)/=2 .or. &
                                & kspv(nm, 0) /= 2 .or. kspv(ndm, 0) /= 2     ) ) then
             !
             if (zmodel) then
                k0 = kfsmx0(nm)
             else
                k0 = 1
             endif
             !
             hlc = 4.48_fp + 0.049_fp * r0(nm, k0, l) + fwind * ( 3.5_fp + 2.05_fp*w10mag(nm) ) &
                 & * ( 1.12_fp + 0.018_fp*r0(nm, k0, l) + 0.00158_fp * r0(nm, k0, l)**2 )
             if (lambda > 0.0_fp ) then
                hlc = lambda
             endif
             !
             if (zmodel) then
                h0new = max(htrsh, dzs1(nm, kfsmax(nm)))
             else
                h0new = max(htrsh, s1(nm) + real(dps(nm),fp))*thick(1)
             endif
             !
             ! limiting value of qle by updating hlc
             !
             if (qtotmx > 0.0_fp) then
                qltemp = -hlc*(r0(nm, k0, l) - tback)
                if (abs(qltemp) > qtotmx) then
                   if (abs(r0(nm, k0, l) - tback) > eps) then
                      hlc   = qtotmx / abs(r0(nm, k0, l) - tback)
                   endif
                endif
             endif
             !
             if (r0(nm, k0, l) <= tback) then
                qle = -hlc*(r0(nm, k0, l) - tback)
                flux = qle/(rhow*cp)
             else
                qle = hlc*tback
                flux = qle/(rhow*cp)
                if (zmodel) then
                   sink(nm, k0, l) = sink(nm, k0, l) + hlc*gsqs(nm)/(rhow*cp)
                else
                   sink(nm, k0, l) = sink(nm, k0, l) + hlc/(rhow*cp*h0new)
                endif
             endif
             !
             ! NOTE: different definitions for SOUR in SIGMA and ZMODEL
             ! (consistent with use in DIFU and Z_DIFU)
             !
             if (zmodel) then
                sour(nm, k0, l) = sour(nm, k0, l) + flux*gsqs(nm)
             else
                h0old = max(htrsh, s0(nm)+real(dps(nm),fp) ) * thick(k0)
                sour(nm, k0, l) = sour(nm, k0, l) + flux/h0old
             endif
             !
             ! Filling of output arrays
             !
             if (flwoutput%temperature) then
                hlc_out(nm)  = hlc 
                qnet_out(nm) = -hlc*(r0(nm, k0, l) - tback)
             endif
          else  ! dry points
             if (flwoutput%temperature) then
                hlc_out(nm)  = 0.0_fp
                qnet_out(nm) = 0.0_fp
             endif
          endif
       enddo
    !
    ! ---------------------------------------------------------------
    ! KTEMP=4 Murakami Heat model
    !
    ! ABSOLUTE TEMPERATURE MODEL: - RHUM in %
    !                             - EASP & ESVP in Mbar
    !                             - PRECIP & EVAPOR in KG/(M2 S)
    !                             - CMTOMT = CM to Meters
    !
    ! S_SIGM = 0.9 * SBOLTZ replaced by EM * SBOLTZ in eq.
    ! to obtain the same eq. as Proctor module
    !
    ! No heat exchange when nm is a floating structure:
    ! (kspu(nm,0)=kspu(nmd,0)=kspv(nm,0)=kspv(ndm,0)=2)
    ! ---------------------------------------------------------------
    !
    elseif (ktemp==4) then
       em = 0.9_fp
       do nm = 1, nmmax
          nmd = nm - icx
          ndm = nm - icy
          if (kfs(nm)>0 .and. (  kspu(nm,0)/=2 .or. kspu(nmd,0)/=2 .or. &
                                & kspv(nm, 0) /= 2 .or. kspv(ndm, 0) /= 2     ) ) then
             !
             if (zmodel) then
                k0 = kfsmx0(nm)
             else
                k0 = 1
             endif
             !
             if (rhum_file) rhum = rhumarr(nm)
             if (tair_file) tair = tairarr(nm)
             !
             ! EVAPORATION HEAT FLUX: QEVA = RHOW * L * (0.12e-6*W10MAG(NM)) * (ESVP - EASP)
             !
             ! All units are in SI
             !           The latent heat of moisted air *RHOA =
             !               latent heat of water       *RHOW
             !
             ! VAPRES is defined in INCTEM and INITEM
             !
             ! Latent heat tl
             !
             tl = 2.5e6_fp - 2.3e3_fp*r0(nm,k0,l)
             !
             if (ivapop == 0) then
                easp = (rhum/100.0_fp) * 23.38_fp * exp(18.1_fp - 5303.3_fp/(tair        + fCtoKelvin))
             else
                easp = vapres
             endif
             esvp =                      23.38_fp * exp(18.1_fp - 5303.3_fp/(r0(nm,k0,l) + fCtoKelvin))
             !
             ! No negative evaporation
             ! Assumption: Negative evaporation is caused by
             ! "poor" modelling/parameter choice and not by the actual physical process
             ! of water condensation out of the air into the water
             !
             delvap = max(0.0_fp , esvp-easp)
             !
             ! Formulation for EVAP moved from CALEVA to HEATU
             !
             select case (keva)
             case (0,1)
                !
                ! No evap from fileva, evap and qeva calculated internally
                !
                evap(nm) = 1.2e-9_fp * w10mag(nm) * delvap * rhow
                qeva     = evap(nm) * tl
             case (2)
                !
                ! evap from fileva, qeva based on read evap
                !
                qeva = evap(nm) * tl
             case (3)
                !
                ! evap from fileva, qeva is calculated internally
                !
                qeva = 1.2e-9_fp * w10mag(nm) * delvap * rhow * tl
             case default
                ! nothing
             end select
             !
             ! HEAT FLUX CONVECTION: Bowens ratio * QEVA
             !                       Bowens ratio = 0.66*(r0-tair)/(esvp-easp)
             !
             bowrat = 0.66_fp * (r0(nm, k0, l) - tair) / (esvp - easp)
             if (delvap > eps) then
                bowrat = 0.66_fp * (r0(nm, k0, l) - tair) / delvap
             else
                bowrat = 0.66_fp * (r0(nm, k0, l) - tair) / eps
             endif
             qco    = bowrat * qeva
             !
             ! LONG WAVE INCOMING AND BACK RADIATION 
             ! (see for comparison Eq. B.7 from Deltares internal note; QBL= QAN-QBR)
             ! All units are in SI
             !
             tkelvi = tair + fCtoKelvin
             tkcube = tkelvi * tkelvi * tkelvi
             if (easp > eps) then
                sq_ea = sqrt(easp)
             else
                sq_ea = 0.0_fp
             endif
             ffclou = (1.0_fp - 0.65_fp*fclou*fclou)
             qbl    = em*sboltz*(0.39_fp - 0.058_fp*sq_ea)*tkcube*tkelvi*ffclou +        &
                    & 4.0_fp*em*sboltz*tkcube*(r0(nm, k0, l) - tair)
             !
             ! TOTAL HEAT LOSS
             !
             ql = qbl + qeva + qco
             !
             ! NET (SHORT WAVE) SOLAR INSOLATION IS SPECIFIED BY USER;
             ! So, except for reflection which is equal to 9%, we do not have to
             ! compute QSN separately
             !
             qsn    = qsun * (1.0_fp-albedo)
             !
             h0old   = max(htrsh, s0(nm) + real(dps(nm),fp))
             h0new   = max(htrsh, s1(nm) + real(dps(nm),fp))
             ztop    = 0.0_fp
             zbottom = -h0old
             if (zmodel) then
                k1    = kfsmx0(nm) - 1
                k2    = kfsmin(nm)
                kstep = -1
                zdown = -dzs0(nm, kfsmx0(nm))
             else
                k1    = 2
                k2    = kmax
                kstep = 1
                zdown = -thick(1) * h0old
             endif
             !
             extinc = 1.7_fp/secchi(nm)
             corr  = 1.0_fp/( (1.0_fp - exp(extinc*zbottom))/extinc )
             qink  = corr * qsn * ( 1.0_fp - exp(extinc*zdown) ) / extinc
             qtotk = (qink - ql) / (rhow*cp)
             !
             ! Reduction of total heat flux at shallow areas
             ! This is needed to avoid extreme high temperatures. The idea is that when it is very shallow, 
             ! some heat is absorped by the ground below it.
             ! Restriction on qsn instead of qtotk seems appropriate, but does not work fine; that still caused unwanted heat fluctuations.
             !
             if (h0old<secchi(nm) .and. qtotk>0.0_fp) then
                qtotk = qtotk * (1.0_fp - exp(extinc*zdown))
             endif    
             if (zmodel) then
                if (qtotk > 0.0_fp) then
                   sour(nm, k0, l) = sour(nm, k0, l) + qtotk*gsqs(nm)
                elseif (r0(nm, k0, l) > 0.0_fp) then
                   sink(nm, k0, l) = sink(nm, k0, l) - qtotk*gsqs(nm)/r0(nm, k0, l)
                else
                   msgcount = msgcount + 1
                endif
             else
                if (qtotk > 0.0_fp) then
                   sour(nm, k0, l) = sour(nm, k0, l) + qtotk/(thick(k0)*h0old)
                elseif ( r0(nm,k0,l) > 0.0_fp ) then
                   sink(nm, k0, l) = sink(nm, k0, l) - qtotk/(thick(k0)*h0new*r0(nm, k0, l))
                else
                   msgcount = msgcount + 1
                endif
             endif
             do k = k1, k2, kstep
                ztop = zdown
                if (zmodel) then
                   zdown = zdown - dzs0(nm, k)
                else
                   zdown = zdown - thick(k)*h0old
                endif
                qink  = corr * qsn * (exp(extinc*ztop)-exp(extinc*zdown)) / extinc
                qtotk = qink / (rhow*cp)
                if (zmodel) then
                   sour(nm, k, l) = sour(nm, k, l) + qtotk*gsqs(nm)
                else
                   sour(nm, k, l) = sour(nm, k, l) + qtotk/(thick(k)*h0old)
                endif
             enddo
             !
             ! Filling of output arrays
             !
             if (flwoutput%temperature) then
                qeva_out(nm) = -qeva
                qco_out(nm)  = -qco
                qbl_out(nm)  = -qbl
                qin_out(nm)  =  qsn
                qnet_out(nm) =  qsn - ql
                if (keva == 3) then
                   !
                   ! qeva           : calculated (and used)
                   ! evap*tl        : derived from input
                   ! qeva - evap*tl : mismatch between calculated and derived heat flux
                   !
                   qmis_out(nm) = qeva - evap(nm)*tl
                endif
             endif
          else  ! dry points
             if (flwoutput%temperature) then
                qeva_out(nm)  = 0.0_fp
                qco_out(nm)   = 0.0_fp
                qbl_out(nm)   = 0.0_fp
                qin_out(nm)   = 0.0_fp
                qnet_out(nm)  = 0.0_fp
                if (keva == 3) then
                   qmis_out(nm) = 0.0_fp
                endif
             endif
          endif
       enddo
    !
    ! ---------------------------------------------------------------
    ! KTEMP=5
    !
    ! HEAT FLUX THROUGH FREE SURFACE (PROCTOR)
    ! ---------------------------------------------------------------
    !
    elseif (ktemp==5) then
       !
       ! initialize local parameters
       !
       rlon = anglon
       rlat = anglat
       !
       cccoef = 0.4_fp
       tm0    = timjan + timhr
       !
       decln  = 23.5_fp * degrad
       w0     = 2.0_fp * pi / (365.24_fp*24.0_fp)
       w1     = 2.0_fp * pi / 24.0_fp
       sc     = 1368.0_fp
       !
       ! take into account the effect of atmospheric absorption
       !       defant 1961 suggests i(z)=i0*EXP(-t*a/SIN(h1))
       !       where  t             turbidity factor (unspecified)
       !              a             0.128 to 0.054 times -LN(SIN(h1))
       !              b1=EXP(-t*a)  transmission coefficient between .6 and .7
       !       comparison with Bunker & Goldsmith averages gives b1=0.76 or
       !       a=0.274
       !
       b1  = 0.76_fp
       !
       em  = 0.985_fp
       hcp = 1004.0_fp
       !
       ! initial parameters already defined for FLOW
       !
       !       RHOA = 1.25   (INPUT)
       !       CP   = 3986.  (initialized IN INITDD)
       !       RHOW = 1000.  (INPUT)
       !       PATM is in N/m2 and PRESA should be in milibar (dived by 100.)
       !
       ! No heat exchange when nm is a floating structure:
       ! (kspu(nm,0)=kspu(nmd,0)=kspv(nm,0)=kspv(ndm,0)=2)
       !
       do nm = 1, nmmax
          nmd = nm - icx
          ndm = nm - icy
          if (  kfs(nm)>0 .and. (  kspu(nm,0)/=2 .or. kspu(nmd,0)/=2 .or. &
                                    & kspv(nm, 0) /= 2 .or. kspv(ndm, 0) /= 2     ) ) then
             !
             if (zmodel) then
                k0 = kfsmx0(nm)
             else
                k0 = 1
             endif
             !
             if (rhum_file) then
                rhum = rhumarr(nm)
             endif
             if (tair_file) then
                tair = tairarr(nm)
             endif
             !
             ! Cloudiness in file is specified in percentages
             !
             if (clou_file) then
                cfclou = clouarr(nm) / 100.0_fp
             endif
             !
             presa = patm(nm) / 100.0_fp
             !
             if (solrad_read) then
                !
                ! Measured solar radiation qradin specified in .tem file
                !
                qsn = qradin * (1-albedo)
                !
             elseif (swrf_file) then
                !
                ! Spatially varying solar radiation from file
                !
                qsn = swrfarr(nm)
                !             
             else
                !
                ! Calculate solar radiation from cloud coverage specified in file
                !
                if (sferic) then
                   !
                   ! Time zone determined with longitude of grid cell
                   ! Using latitude of grid cell for declination
                   !
                   rlon = xcor(nm)
                   rlat = ycor(nm)
                   !
                endif
                !
                tm = tm0 + 24.0_fp*rlon/360.0_fp - tzone
                !
                ! Calculate sine of the angle of the sun above the horizon: SNH
                ! d is the declination angle 
                ! June 21st is the 171st day after TM=0
                !
                d   = decln * cos(w0*tm - 2.950_fp)
                snh = -cos(rlat*degrad) * cos(d) * cos(w1*tm) + sin(rlat*degrad) * sin(d)
                if (snh > 1.0_fp) then
                   snh = 1.0_fp
                endif
                if (snh < 0.0_fp) then
                   snh = 0.0_fp
                endif
                !
                ! incident solar radiation
                !
                qs = sc * snh * b1
                !
                ! receipt of solar radiation qs by water, restricted by presence of
                ! clouds and reflection of water surface (albedo)
                !
                qsn = qs * (1.0_fp - cfclou*cccoef - 0.38_fp*cfclou*cfclou) * (1.0_fp-albedo)
                !
             endif
             !
             ! Latent heat tl
             !
             tl = 2.5e6_fp - 2.3e3_fp*r0(nm,k0,l)
             !
             ! Calculate heat loss at the sea surface
             ! CFCLOU = fraction => multiply by 10-4 removed
             !
             ! Saturation pressure of water vapour in air remote (ewl) and
             ! near water surface (ew)
             !
             ew  = 10.0_fp**(  ( 0.7859_fp + 0.03477_fp*r0(nm,k0,l) ) &
                 & / ( 1.0_fp + 0.00412_fp*r0(nm,k0,l) )  )
             ewl = 10.0_fp**( (0.7859_fp + 0.03477_fp*tair) / (1.0_fp + 0.00412_fp*tair) )
             !
             ! Vapour pressure in air remote (eal) for given humidity
             ! rhum is in percentages; divide by 100 for fraction
             !
             eal = (rhum/100.0_fp) * ewl
             if (eal > eps) then
                sq_eal = sqrt(eal)
             else
                sq_eal = 0.0_fp
             endif
             !
             ! Specific humidity (qal) of air remote and
             ! saturated air (qw) near water surface; eq.(A.9)+(A.10):
             !
             qw     = (0.62_fp*ew ) / (presa - 0.38_fp*ew )
             qal    = (0.62_fp*eal) / (presa - 0.38_fp*eal)
             !
             ! No negative evaporation
             ! Assumption: Negative evaporation is caused by
             ! "poor" modelling/parameter choice and not by the actual physical process
             ! of water condensation out of the air into the water
             !
             delvap = max(0.0_fp , qw-qal)
             !
             ! Heat loss of water by evaporation through forced convection,
             ! Evaporation calculation moved from caleva to heatu
             !
             select case (keva)
             case (0,1)
                !
                ! No evap from fileva, evap and qeva calculated internally
                !
                evap(nm) = dalton * rhoa * w10mag(nm) * delvap
                qeva     = evap(nm) * tl
             case (2)
                !
                ! evap from fileva, qeva based on read evap
                !
                qeva = evap(nm) * tl
             case (3)
                !
                ! evap from fileva, qeva is calculated internally
                !
                qeva = dalton * rhoa * w10mag(nm) * delvap * tl
             case default
                ! nothing
             end select
             !
             ! Heat loss of water by forced convection of sensible heat
             !
             qco = stanton * rhoa * hcp * w10mag(nm) * (r0(nm,k0,l)-tair)
             !
             if (free_convec) then
                !
                ! Contribution by free convection
                !
                prair  = 0.7_fp
                pr2    = prair**2
                xnuair = 16.0e-6_fp
                !
                ! To compute rho air
                !
                rdry   = 287.05_fp
                rvap   = 461.495_fp
                tkelvn = fCtoKelvin
                !
                ! For sensible heat
                ! Reduced gravity GRED
                !
                pvap   = 100.0_fp * ew
                rhoa0  = ((presa*100.0_fp-pvap)/rdry + pvap/rvap) / ( r0(nm,k0,l)+tkelvn )
                pvap   = 100.0_fp * eal
                rhoa10 = ((presa*100.0_fp-pvap)/rdry + pvap/rvap) / (tair+tkelvn)
                !
                ! gred = ag*(rhoa10-rhoa0) / ((rhoa0+rhoa10)/2)
                !
                gred   = 2.0_fp * ag * (rhoa10-rhoa0) / (rhoa0+rhoa10)
                fheat  = 0.0_fp
                if (gred > 0.0_fp) then
                   fheat = gred * xnuair / pr2
                   fheat = cfrcon * fheat**0.33333333_fp
                endif
                !
                ! Add to heat loss by forced convection
                !
                rcpa  = hcp * (rhoa0+rhoa10) / 2.0_fp
                hfree = rcpa * fheat * (r0(nm,k0,l)-tair)
                qco   = qco + hfree
                !
                ! Latent heat by free convection
                !
                select case (keva)
                case (0,1)
                   !
                   ! No evap from fileva
                   ! mass flux due to free convection added to evap
                   ! heat flux due to free convection added to qeva
                   !
                   efree    = fheat * delvap * tl * (rhoa0+rhoa10)/2.0_fp
                   evap(nm) = evap(nm) + efree/tl
                   qeva     = qeva + efree
                case (2)
                   !
                   ! evap from fileva, qeva based on read evap
                   ! efree part can not be backwards recalculated
                   !
                   efree    = 0.0_fp
                case (3)
                   !
                   ! evap from fileva, qeva is calculated internally
                   ! heat flux due to free convection added to qeva
                   !
                   efree    = fheat * delvap * tl * (rhoa0+rhoa10)/2.0_fp
                   qeva     = qeva + efree
                case default
                   ! nothing
                end select
             endif
             !
             ! heat loss by effective infrared back radiation hl, restricted by
             ! presence of clouds and water vapour in air
             !
             qbl = em * sboltz * ( (r0(nm,k0,l) + fCtoKelvin)**4.0_fp )                 &
                 & * (0.39_fp - 0.05_fp*sq_eal) * (1.0_fp - 0.6_fp*cfclou*cfclou)
             !
             qbl = max(0.0_fp, qbl)
             !
             ! net heat flux [W/m^2] into water, solar radiation excluded
             !
             ql      = qbl + qco + qeva
             h0old   = max(htrsh, s0(nm) + real(dps(nm),fp))
             h0new   = max(htrsh, s1(nm) + real(dps(nm),fp))
             ztop    = 0.0_fp
             zbottom = -h0old
             !
             ! For thin layers of water: no heat flux calculations
             ! to avoid large fluxes in small bodies of water
             !
             if (h0old > htrsh) then
                if (zmodel) then
                   k1    = kfsmx0(nm) - 1
                   k2    = kfsmin(nm)
                   kstep = -1
                   zdown = -max(htrsh,dzs0(nm, kfsmx0(nm)))
                else
                   k1    = 2
                   k2    = kmax
                   kstep = 1
                   zdown = -thick(1)*h0old
                endif
                !
                extinc = 1.7_fp/secchi(nm)
                corr  = 1.0_fp / ( (1.0_fp - exp(extinc*zbottom)) / extinc )
                qink  = corr * qsn * (1.0_fp - exp(extinc*zdown)) / extinc
                qtotk = (qink-ql) / (rhow*cp)
                !
                ! Reduction of solar radiation at shallow areas
                !
                if (h0old<secchi(nm) .and. qtotk>0.0_fp) then
                   qtotk = qtotk * (1.0_fp - exp(extinc*zdown))
                endif    
                !
                if (zmodel) then
                   if (qtotk > 0.0_fp) then
                      sour(nm, k0, l) = sour(nm, k0, l) + qtotk*gsqs(nm)
                   elseif (r0(nm, k0, l) > 0.01_fp) then
                      sink(nm, k0, l) = sink(nm, k0, l) - qtotk*gsqs(nm)/r0(nm, k0, l)
                   elseif (r0(nm, k0, l) > 0.0_fp .and. r0(nm, k0, l) < 0.01_fp) then
                      !
                      ! No addition to sink when the water temperature is lower than 0.01 degree.
                      !
                   else
                      msgcount = msgcount + 1
                   endif
                else
                   if (qtotk > 0.0_fp) then
                      sour(nm, k0, l) = sour(nm, k0, l) + qtotk/(thick(k0)*h0old)
                   elseif (r0(nm, k0, l) > 0.01_fp) then
                      sink(nm, k0, l) = sink(nm, k0, l) - qtotk/(thick(k0)*h0new*r0(nm, k0, l))
                   elseif (r0(nm, k0, l) > 0.0_fp .and. r0(nm, k0, l) < 0.01_fp) then
                      !
                      ! No addition to sink when the water temperature is lower than 0.01 degree.
                      !                   
                   else
                      msgcount = msgcount + 1
                   endif
                endif
                do k = k1, k2, kstep
                   ztop = zdown
                   if (zmodel) then
                      zdown = zdown - dzs0(nm, k)
                   else
                      zdown = zdown - thick(k)*h0old
                   endif
                   qink  = corr * qsn * (exp(extinc*ztop) - exp(extinc*zdown)) / extinc
                   qtotk = qink / (rhow*cp)
                   if (zmodel) then
                      sour(nm, k, l) = sour(nm, k, l) + qtotk*gsqs(nm)
                   else
                      sour(nm, k, l) = sour(nm, k, l) + qtotk/(thick(k)*h0old)
                   endif
                enddo
             else
                !
                ! Thin layer of water: no heat fluxes
                !
                qsn   = 0.0_fp
                qeva  = 0.0_fp
                qco   = 0.0_fp
                qbl   = 0.0_fp
                ql    = 0.0_fp
                if (free_convec) then
                   hfree = 0.0_fp
                   efree = 0.0_fp
                endif
             endif
             !
             ! Filling of output arrays
             !
             if (flwoutput%temperature) then
                qeva_out(nm) = -qeva
                qco_out(nm)  = -qco
                qbl_out(nm)  = -qbl
                qin_out(nm)  =  qsn
                qnet_out(nm) =  qsn - ql
                if (free_convec) then
                   hfree_out(nm) = -hfree
                   efree_out(nm) = -efree
                endif
                if (keva == 3) then
                   !
                   ! qeva           : calculated (and used)
                   ! evap*tl        : derived from input
                   ! qeva - evap*tl : mismatch between calculated and derived heat flux
                   !
                   qmis_out(nm) = qeva - evap(nm)*tl
                endif
             endif
          else  ! dry points
             if (flwoutput%temperature) then
                qeva_out(nm) = 0.0_fp
                qco_out(nm)  = 0.0_fp
                qbl_out(nm)  = 0.0_fp
                qin_out(nm)  = 0.0_fp
                qnet_out(nm) = 0.0_fp
                if (free_convec) then
                   hfree_out(nm) = 0.0_fp
                   efree_out(nm) = 0.0_fp
                endif
                if (keva == 3) then
                   qmis_out(nm) = 0.0_fp
                endif
             endif
          endif
       enddo
    else
    endif
    !
    if (msgcount > 0) then
        write (errmsg,'(a,2(i0,a))') &
            & 'Timestep ', ntstep, ': No heat flux to air; water temperature is 0 degrees in ', msgcount, ' points.'
       call prterr(lundia, 'U190', trim(errmsg))
    endif
    !
   endif
enddo
!
end subroutine heatu
