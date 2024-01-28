subroutine u_ppr(lundia    ,lunprt    ,error     ,versio    ,prsmap    , &
               & prshis    ,selmap    ,selhis    ,runid     ,rhow      , &
               & grdang    ,dtsec     ,nst       ,iphisc    , &
               & npmap     ,ithisc    ,itmapc    ,itdroc    ,itrstc    , &
               & ftstat    ,ftcros    ,gdp       )
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
!    Function: - Routine similar to POSTPR that may be used by
!                the user to define their own output
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
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
    include 'tri-dyn.igd'
    integer(pntrsize)      , pointer :: alfas
    integer(pntrsize)      , pointer :: atr
    integer(pntrsize)      , pointer :: c
    integer(pntrsize)      , pointer :: ctr
    integer(pntrsize)      , pointer :: dicuv
    integer(pntrsize)      , pointer :: dicww
    integer(pntrsize)      , pointer :: dp
    integer(pntrsize)      , pointer :: dps
    integer(pntrsize)      , pointer :: dpsed
    integer(pntrsize)      , pointer :: dtr
    integer(pntrsize)      , pointer :: enstro
    integer(pntrsize)      , pointer :: entr
    integer(pntrsize)      , pointer :: evap
    integer(pntrsize)      , pointer :: fltr
    integer(pntrsize)      , pointer :: gro
    integer(pntrsize)      , pointer :: guu
    integer(pntrsize)      , pointer :: guv
    integer(pntrsize)      , pointer :: gvu
    integer(pntrsize)      , pointer :: gvv
    integer(pntrsize)      , pointer :: hrms
    integer(pntrsize)      , pointer :: hu
    integer(pntrsize)      , pointer :: hv
    integer(pntrsize)      , pointer :: patm
    integer(pntrsize)      , pointer :: precip
    integer(pntrsize)      , pointer :: qu
    integer(pntrsize)      , pointer :: qxk
    integer(pntrsize)      , pointer :: qyk
    integer(pntrsize)      , pointer :: r1
    integer(pntrsize)      , pointer :: rho
    integer(pntrsize)      , pointer :: rich
    integer(pntrsize)      , pointer :: rlabda
    integer(pntrsize)      , pointer :: rsed
    integer(pntrsize)      , pointer :: rtur1
    integer(pntrsize)      , pointer :: s1
    integer(pntrsize)      , pointer :: sbtr
    integer(pntrsize)      , pointer :: sbtrc
    integer(pntrsize)      , pointer :: sbuu
    integer(pntrsize)      , pointer :: sbvv
    integer(pntrsize)      , pointer :: seddif
    integer(pntrsize)      , pointer :: sstr
    integer(pntrsize)      , pointer :: sstrc
    integer(pntrsize)      , pointer :: taubpu
    integer(pntrsize)      , pointer :: taubpv
    integer(pntrsize)      , pointer :: taubsu
    integer(pntrsize)      , pointer :: taubsv
    integer(pntrsize)      , pointer :: teta
    integer(pntrsize)      , pointer :: thick
    integer(pntrsize)      , pointer :: tp
    integer(pntrsize)      , pointer :: u1
    integer(pntrsize)      , pointer :: umnldf
    integer(pntrsize)      , pointer :: uorb
    integer(pntrsize)      , pointer :: v1
    integer(pntrsize)      , pointer :: vicuv
    integer(pntrsize)      , pointer :: vicww
    integer(pntrsize)      , pointer :: vmnldf
    integer(pntrsize)      , pointer :: vortic
    integer(pntrsize)      , pointer :: w1
    integer(pntrsize)      , pointer :: windcd
    integer(pntrsize)      , pointer :: windu
    integer(pntrsize)      , pointer :: windv
    integer(pntrsize)      , pointer :: wphy
    integer(pntrsize)      , pointer :: ws
    integer(pntrsize)      , pointer :: xydro
    integer(pntrsize)      , pointer :: xz
    integer(pntrsize)      , pointer :: yz
    integer(pntrsize)      , pointer :: zalfas
    integer(pntrsize)      , pointer :: zbdsed
    integer(pntrsize)      , pointer :: zcuru
    integer(pntrsize)      , pointer :: zcurv
    integer(pntrsize)      , pointer :: zcurw
    integer(pntrsize)      , pointer :: zdicww
    integer(pntrsize)      , pointer :: zdps
    integer(pntrsize)      , pointer :: zdpsed
    integer(pntrsize)      , pointer :: zenst
    integer(pntrsize)      , pointer :: zfixfac
    integer(pntrsize)      , pointer :: zfrac
    integer(pntrsize)      , pointer :: zhidexp
    integer(pntrsize)      , pointer :: zkfs
    integer(pntrsize)      , pointer :: zmudfrac
    integer(pntrsize)      , pointer :: zqxk
    integer(pntrsize)      , pointer :: zqyk
    integer(pntrsize)      , pointer :: zrca
    integer(pntrsize)      , pointer :: zrho
    integer(pntrsize)      , pointer :: zrich
    integer(pntrsize)      , pointer :: zrsdeq
    integer(pntrsize)      , pointer :: zsandfrac
    integer(pntrsize)      , pointer :: zsbu
    integer(pntrsize)      , pointer :: zsbv
    integer(pntrsize)      , pointer :: zseddif
    integer(pntrsize)      , pointer :: zsinkse
    integer(pntrsize)      , pointer :: zsourse
    integer(pntrsize)      , pointer :: zssu
    integer(pntrsize)      , pointer :: zssv
    integer(pntrsize)      , pointer :: ztaub
    integer(pntrsize)      , pointer :: ztauet
    integer(pntrsize)      , pointer :: ztauks
    integer(pntrsize)      , pointer :: ztur
    integer(pntrsize)      , pointer :: zvicww
    integer(pntrsize)      , pointer :: zvort
    integer(pntrsize)      , pointer :: zwl
    integer(pntrsize)      , pointer :: zws
    integer(pntrsize)      , pointer :: zwndsp
    integer(pntrsize)      , pointer :: zwnddr
    integer(pntrsize)      , pointer :: zairp
    integer(pntrsize)      , pointer :: zprecp
    integer(pntrsize)      , pointer :: zevap
    integer(pntrsize)      , pointer :: zwndcd
    integer(pntrsize)      , pointer :: rl
    integer(pntrsize)      , pointer :: p1
    integer(pntrsize)      , pointer :: hydprs
    integer(pntrsize)      , pointer :: kcs
    integer(pntrsize)      , pointer :: kfs
    integer(pntrsize)      , pointer :: kfu
    integer(pntrsize)      , pointer :: kfv
    integer(pntrsize)      , pointer :: mnksrc
    integer(pntrsize)      , pointer :: kfumin
    integer(pntrsize)      , pointer :: kfvmin
    integer(pntrsize)      , pointer :: kfsmin
    integer(pntrsize)      , pointer :: kfumax
    integer(pntrsize)      , pointer :: kfvmax
    integer(pntrsize)      , pointer :: kfsmax
    integer(pntrsize)      , pointer :: kfuz1
    integer(pntrsize)      , pointer :: kfvz1
    integer(pntrsize)      , pointer :: namdro
    integer(pntrsize)     , pointer :: wrka1
    integer(pntrsize)     , pointer :: wrka2
    integer(pntrsize)     , pointer :: wrka3
    integer(pntrsize)     , pointer :: wrka4
    integer(pntrsize)     , pointer :: wrka5
    integer(pntrsize)     , pointer :: wrkb3
    integer(pntrsize)     , pointer :: wrkb4
    integer               , pointer :: nmax
    integer               , pointer :: mmax
    integer               , pointer :: nmaxus
    integer               , pointer :: kmax
    integer               , pointer :: lmax
    integer               , pointer :: lmaxd
    integer               , pointer :: lsts
    integer               , pointer :: lstsc
    integer               , pointer :: lstsci
    integer               , pointer :: lsal
    integer               , pointer :: lsed
    integer               , pointer :: lsedtot
    integer               , pointer :: ltem
    integer               , pointer :: ltur
    integer               , pointer :: kc
    integer               , pointer :: nsrc
    integer               , pointer :: nostat
    integer               , pointer :: ntruv
    integer               , pointer :: ntru
    integer               , pointer :: ndro
    real(fp)              , pointer :: timsec
    real(fp)              , pointer :: timmin
    integer               , pointer :: itstrt
    integer               , pointer :: itfinish
    integer               , pointer :: julday
    character*256         , pointer :: filus1
    character*256         , pointer :: filus2
    character*256         , pointer :: filus3
    logical               , pointer :: first
    integer               , pointer :: lunus1
    integer               , pointer :: lunus2
    integer               , pointer :: lunus3
    logical               , pointer :: drogue
    logical               , pointer :: wave
    logical               , pointer :: wind
    logical               , pointer :: zmodel
!
! Global variables
!
    integer             :: iphisc  !!  Current time counter for printing
                                   !!  history data
    integer             :: itdroc  !!  Current time counter for the dro-
                                   !!  gue data file
    integer             :: ithisc  !!  Current time counter for the his-
                                   !!  tory data file
    integer, intent(in) :: itmapc  !!  Current time counter for the MAP
    integer             :: itrstc  !!  Current time counter for the re-
                                   !!  start file. Start writing after
                                   !!  first interval is passed. Last time
                                   !!  will always be written to file for
                                   !!  ITRSTI > 0
    integer, intent(in) :: lundia  !  Description and declaration in inout.igs
    integer             :: lunprt  !  Description and declaration in inout.igs
    integer             :: npmap
    integer, intent(in) :: nst     !!  Current time step counter
    logical             :: error   !!  Flag=TRUE if an error is encountered
    logical, intent(in) :: ftcros  !!  Flag set when TCROSS is invoked
    logical, intent(in) :: ftstat  !!  Flag set when TSTAT  is invoked
    real(fp)            :: dtsec   !!  Integration time step [in seconds]
    real(fp)            :: grdang  !  Description and declaration in tricom.igs
    real(fp)            :: rhow    !  Description and declaration in esm_alloc_real.f90
    character(*)        :: runid   !!  Run identification code for the cur-
                                   !!  rent simulation (used to determine
                                   !!  the names of the in- /output files
                                   !!  used by the system)
    character(*)        :: selmap  !  Description and declaration in tricom.igs
    character(19)       :: prsmap  !  Description and declaration in tricom.igs
    character(23)       :: prshis  !  Description and declaration in tricom.igs
    character(23)       :: selhis  !  Description and declaration in tricom.igs
    character(5)        :: versio  !!  Version nr. of the current package
!
! Local variables
!
    integer                       :: icross
    integer                       :: idate  ! Current simulation date YYYMMDD 
    integer                       :: idro   ! Drogue number selected for output 
    integer                       :: ilay   ! Layer number selected for map output 
    integer                       :: istat  ! Station number selected 
    integer                       :: itime  ! Current simulation time MMHHSS 
    integer                       :: notim  ! No. of time step to be written to the file 
    integer, external             :: newlun
    character(131), dimension(10) :: header ! File header 
!
!! executable statements -------------------------------------------------------
!
    wrka1       => gdp%gdaddress%wrka1
    wrka2       => gdp%gdaddress%wrka2
    wrka3       => gdp%gdaddress%wrka3
    wrka4       => gdp%gdaddress%wrka4
    wrka5       => gdp%gdaddress%wrka5
    wrkb3       => gdp%gdaddress%wrkb3
    wrkb4       => gdp%gdaddress%wrkb4
    filus1      => gdp%gddatusr%filus1
    filus2      => gdp%gddatusr%filus2
    filus3      => gdp%gddatusr%filus3
    nmax        => gdp%d%nmax
    mmax        => gdp%d%mmax
    nmaxus      => gdp%d%nmaxus
    kmax        => gdp%d%kmax
    lmax        => gdp%d%lmax
    lmaxd       => gdp%d%lmaxd
    lsts        => gdp%d%lsts
    lstsc       => gdp%d%lstsc
    lstsci      => gdp%d%lstsci
    lsal        => gdp%d%lsal
    lsed        => gdp%d%lsed
    lsedtot     => gdp%d%lsedtot
    ltem        => gdp%d%ltem
    ltur        => gdp%d%ltur
    kc          => gdp%d%kc
    nsrc        => gdp%d%nsrc
    nostat      => gdp%d%nostat
    ntruv       => gdp%d%ntruv
    ntru        => gdp%d%ntru
    ndro        => gdp%d%ndro
    timsec      => gdp%gdinttim%timsec
    timmin      => gdp%gdinttim%timmin
    itstrt      => gdp%gdinttim%itstrt
    itfinish    => gdp%gdinttim%itfinish
    julday      => gdp%gdinttim%julday
    drogue      => gdp%gdprocs%drogue
    wave        => gdp%gdprocs%wave
    wind        => gdp%gdprocs%wind
    zmodel      => gdp%gdprocs%zmodel
    alfas       => gdp%gdr_i_ch%alfas
    atr         => gdp%gdr_i_ch%atr
    c           => gdp%gdr_i_ch%c
    ctr         => gdp%gdr_i_ch%ctr
    dicuv       => gdp%gdr_i_ch%dicuv
    dicww       => gdp%gdr_i_ch%dicww
    dp          => gdp%gdr_i_ch%dp
    dps         => gdp%gdr_i_ch%dps
    dpsed       => gdp%gdr_i_ch%dpsed
    dtr         => gdp%gdr_i_ch%dtr
    enstro      => gdp%gdr_i_ch%enstro
    entr        => gdp%gdr_i_ch%entr
    evap        => gdp%gdr_i_ch%evap
    fltr        => gdp%gdr_i_ch%fltr
    gro         => gdp%gdr_i_ch%gro
    guu         => gdp%gdr_i_ch%guu
    guv         => gdp%gdr_i_ch%guv
    gvu         => gdp%gdr_i_ch%gvu
    gvv         => gdp%gdr_i_ch%gvv
    hrms        => gdp%gdr_i_ch%hrms
    hu          => gdp%gdr_i_ch%hu
    hv          => gdp%gdr_i_ch%hv
    patm        => gdp%gdr_i_ch%patm
    precip      => gdp%gdr_i_ch%precip
    qu          => gdp%gdr_i_ch%qu
    qxk         => gdp%gdr_i_ch%qxk
    qyk         => gdp%gdr_i_ch%qyk
    r1          => gdp%gdr_i_ch%r1
    rho         => gdp%gdr_i_ch%rho
    rich        => gdp%gdr_i_ch%rich
    rlabda      => gdp%gdr_i_ch%rlabda
    rsed        => gdp%gdr_i_ch%rsed
    rtur1       => gdp%gdr_i_ch%rtur1
    s1          => gdp%gdr_i_ch%s1
    sbtr        => gdp%gdr_i_ch%sbtr
    sbtrc       => gdp%gdr_i_ch%sbtrc
    sbuu        => gdp%gdr_i_ch%sbuu
    sbvv        => gdp%gdr_i_ch%sbvv
    seddif      => gdp%gdr_i_ch%seddif
    sstr        => gdp%gdr_i_ch%sstr
    sstrc       => gdp%gdr_i_ch%sstrc
    taubpu      => gdp%gdr_i_ch%taubpu
    taubpv      => gdp%gdr_i_ch%taubpv
    taubsu      => gdp%gdr_i_ch%taubsu
    taubsv      => gdp%gdr_i_ch%taubsv
    teta        => gdp%gdr_i_ch%teta
    thick       => gdp%gdr_i_ch%thick
    tp          => gdp%gdr_i_ch%tp
    u1          => gdp%gdr_i_ch%u1
    umnldf      => gdp%gdr_i_ch%umnldf
    uorb        => gdp%gdr_i_ch%uorb
    v1          => gdp%gdr_i_ch%v1
    vicuv       => gdp%gdr_i_ch%vicuv
    vicww       => gdp%gdr_i_ch%vicww
    vmnldf      => gdp%gdr_i_ch%vmnldf
    vortic      => gdp%gdr_i_ch%vortic
    w1          => gdp%gdr_i_ch%w1
    windcd      => gdp%gdr_i_ch%windcd
    windu       => gdp%gdr_i_ch%windu
    windv       => gdp%gdr_i_ch%windv
    wphy        => gdp%gdr_i_ch%wphy
    ws          => gdp%gdr_i_ch%ws
    xydro       => gdp%gdr_i_ch%xydro
    xz          => gdp%gdr_i_ch%xz
    yz          => gdp%gdr_i_ch%yz
    zalfas      => gdp%gdr_i_ch%zalfas
    zbdsed      => gdp%gdr_i_ch%zbdsed
    zcuru       => gdp%gdr_i_ch%zcuru
    zcurv       => gdp%gdr_i_ch%zcurv
    zcurw       => gdp%gdr_i_ch%zcurw
    zdicww      => gdp%gdr_i_ch%zdicww
    zdps        => gdp%gdr_i_ch%zdps
    zdpsed      => gdp%gdr_i_ch%zdpsed
    zenst       => gdp%gdr_i_ch%zenst
    zfixfac     => gdp%gdr_i_ch%zfixfac
    zfrac       => gdp%gdr_i_ch%zfrac
    zhidexp     => gdp%gdr_i_ch%zhidexp
    zkfs        => gdp%gdr_i_ch%zkfs
    zmudfrac    => gdp%gdr_i_ch%zmudfrac
    zqxk        => gdp%gdr_i_ch%zqxk
    zqyk        => gdp%gdr_i_ch%zqyk
    zrca        => gdp%gdr_i_ch%zrca
    zrho        => gdp%gdr_i_ch%zrho
    zrich       => gdp%gdr_i_ch%zrich
    zrsdeq      => gdp%gdr_i_ch%zrsdeq
    zsandfrac   => gdp%gdr_i_ch%zsandfrac
    zsbu        => gdp%gdr_i_ch%zsbu
    zsbv        => gdp%gdr_i_ch%zsbv
    zseddif     => gdp%gdr_i_ch%zseddif
    zsinkse     => gdp%gdr_i_ch%zsinkse
    zsourse     => gdp%gdr_i_ch%zsourse
    zssu        => gdp%gdr_i_ch%zssu
    zssv        => gdp%gdr_i_ch%zssv
    ztaub       => gdp%gdr_i_ch%ztaub
    ztauet      => gdp%gdr_i_ch%ztauet
    ztauks      => gdp%gdr_i_ch%ztauks
    ztur        => gdp%gdr_i_ch%ztur
    zvicww      => gdp%gdr_i_ch%zvicww
    zvort       => gdp%gdr_i_ch%zvort
    zwl         => gdp%gdr_i_ch%zwl
    zws         => gdp%gdr_i_ch%zws
    zwndsp      => gdp%gdr_i_ch%zwndsp
    zwnddr      => gdp%gdr_i_ch%zwnddr
    zairp       => gdp%gdr_i_ch%zairp
    zprecp      => gdp%gdr_i_ch%zprecp
    zevap       => gdp%gdr_i_ch%zevap
    zwndcd      => gdp%gdr_i_ch%zwndcd
    rl          => gdp%gdr_i_ch%rl
    p1          => gdp%gdr_i_ch%p1
    hydprs      => gdp%gdr_i_ch%hydprs
    kcs         => gdp%gdr_i_ch%kcs
    kfs         => gdp%gdr_i_ch%kfs
    kfu         => gdp%gdr_i_ch%kfu
    kfv         => gdp%gdr_i_ch%kfv
    mnksrc      => gdp%gdr_i_ch%mnksrc
    kfumin      => gdp%gdr_i_ch%kfumin
    kfvmin      => gdp%gdr_i_ch%kfvmin
    kfsmin      => gdp%gdr_i_ch%kfsmin
    kfumax      => gdp%gdr_i_ch%kfumax
    kfvmax      => gdp%gdr_i_ch%kfvmax
    kfsmax      => gdp%gdr_i_ch%kfsmax
    kfuz1       => gdp%gdr_i_ch%kfuz1
    kfvz1       => gdp%gdr_i_ch%kfvz1
    namdro      => gdp%gdr_i_ch%namdro
    first       => gdp%gdu_ppr%first
    lunus1      => gdp%gdu_ppr%lunus1
    lunus2      => gdp%gdu_ppr%lunus2
    lunus3      => gdp%gdu_ppr%lunus3
    !
    lunus1 = 0
    lunus2 = 0
    lunus3 = 0
    !
    ! Open new unit number for the first time
    ! One may define as many file units as possible. In this example
    ! 3 formatted files are defined
    !
    if (first) then
       if (filus1 /= ' ') then
          open (newunit=lunus1, file = filus1, form = 'formatted')
          write (lundia, *) ' Writing user HIS file: ', filus1
       endif
       if (filus2 /= ' ') then
          open (newunit=lunus2, file = filus2, form = 'formatted')
          write (lundia, *) ' Writing user DRO file: ', filus2
       endif
       if (filus1 /= ' ') then
          open (newunit=lunus3, file = filus3, form = 'formatted')
          write (lundia, *) ' Writing user MAP file: ', filus3, itmapc
       endif
    endif
    if (filus1==' ' .and. filus2==' ' .and. filus3==' ') goto 9999
    !
    ! Calculate date and time YYYYMMDD HHMMSS
    !
    call timdat(julday    ,timsec    ,idate     ,itime     )
    !
    !-If not yet computed (not required) in POSTPR then compute now the
    ! quantities that has been defined along cross-sections:
    ! CTR, ATR, FLTR, DTR (see meaning in 'r-i-ch.inc' file)
    !-Compute flow- & concentration fluxes in defined cross-sections
    ! DO NOT CHANGE THE STRUCTURE OF THE ROUTINE BELOW
    !
    if (.not.ftcros) then
       call tcross(dtsec     ,prshis    ,selhis    ,ntruv     ,ntru      , &
                 & lstsci    ,nmaxus    ,nmax      ,mmax      ,kmax      , &
                 & i(kfu)    ,i(kfv)    ,r(ctr)    ,r(fltr)   , &
                 & r(atr)    ,r(dtr)    ,r(guu)    ,r(gvv)    ,r(guv)    , &
                 & r(gvu)    ,r(thick)  ,r(r1)     ,r(qxk)    ,r(qyk)    , &
                 & r(hu)     ,r(hv)     ,r(dicuv)  ,lsed      ,lsedtot   , &
                 & r(sbtr)   ,r(sstr)   ,r(sbtrc)  ,r(sstrc)  ,r(sbuu)   , &
                 & r(sbvv)   ,gdp       )
    endif
    !
    ! If not yet computed (not required) in POSTPR then compute now the
    ! quantities that are defined at stations (at zeta points):
    ! ZWL, ZCURU, ZCURV, ZCURW, ALFAS, GRO, ZTUR, ZTAUKS, ZTAUET,ZVICWW,
    ! ZDICWW, ZRHO, ZQXK, ZQYK (see meaning in 'r-i-ch.inc' file).
    ! DO NOT CHANGE THE STRUCTURE OF THE ROUTINE BELOW
    !
    if (.not.ftstat) then
       call tstat(prshis    ,selhis    ,rhow      ,zmodel    ,nostat    , &
                & nmax      ,mmax      ,kmax      ,lmax      ,lstsci    , &
                & ltur      ,lsal      ,ltem      ,i(kfs)    ,i(kfu)    , &
                & i(kfv)    ,i(kcs)    ,i(kfuz1)  ,i(kfvz1)  ,i(kfumin) , &
                & i(kfumax) ,i(kfvmin) ,i(kfvmax) ,i(kfsmin) ,i(kfsmax) , &
                & i(zkfs)   ,r(s1)     ,r(wrkb3)  ,r(wrkb4)  ,r(r1)     , &
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
    endif
    !
    ! HIS data (USE LUNUS1)
    ! In this example, history data is written after each time step
    ! for station number 5
    !
    if (nostat>=5 .and. filus1/=' ') then
       notim = itfinish - itstrt + 1
       istat = 5
       write (lundia, *) '@@@@@ Writing user HIS file at tstep: ', nst
       !
       ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       ! @                                              @
       ! @ PROGRAM YOUR OWN HISTORY OUTPUT ROUTINE HERE @
       ! @ worked out example: U_WHIS (see TRM FLOW)    @
       ! @                                              @
       ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       !
       call u_whis(lunus1    ,header    ,runid     ,itime     ,idate     , &
                 & first     ,kmax      ,lmaxd     ,lstsci    ,ltur      , &
                 & lsal      ,ltem      ,lsed      ,istat     ,icross    , &
                 & ntru      ,ntruv     ,nostat    ,notim     ,zmodel    , &
                 & timmin    ,grdang    ,r(zalfas) ,r(zwl)    ,r(zcuru)  , &
                 & r(zcurv)  ,r(zcurw)  ,r(ztur)   ,r(zqxk)   ,r(zqyk)   , &
                 & r(ztauks) ,r(ztauet) ,r(zvicww) ,r(zdicww) ,r(zrich)  , &
                 & r(zrho)   ,r(zbdsed) ,r(zrsdeq) ,r(zdpsed) ,r(zdps)   , &
                 & r(zws)    ,r(gro)    ,r(hydprs) ,r(atr)    ,r(ctr)    , &
                 & r(dtr)    ,r(fltr)   ,gdp       )
    endif
    !
    ! DRO data (USE LUNUS2)
    ! In this example, DROGUE data for drogue nr. 1 is written after
    ! each time step
    !
    if (ndro>=1 .and. filus2/=' ') then
       notim = itfinish - itstrt + 1
       idro = 1
       write (lundia, *) '@@@@@ Writing user DRO file at tstep: ', nst
       !
       ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       ! @                                              @
       ! @  PROGRAM YOUR OWN DROGUE OUTPUT ROUTINE HERE @
       ! @  worked out example: U_WDRO (see TRM FLOW)   @
       ! @                                              @
       ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       !
       call u_wdro(lunus2    ,first     ,header    ,runid     ,itime     , &
                 & idate     ,timmin    ,notim     ,idro      ,ndro      , &
                 & r(xydro)  ,ch(namdro))
    endif
    !
    ! MAP data (USE LUNUS3)
    ! In this example, map data (vertical eddy viscosity at layer=ILAY,
    ! water elevation + height at waterlvel points) is written at times
    ! identical to map times (MAKE SURE YOU HAVE MAP FILE !)
    !
    if (nst==itmapc .and. kmax>3 .and. filus3/=' ') then
       ilay = 3
       write (lundia, *) '@@@@@ Writing user MAP file at tstep: ', nst
       !
       ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       ! @                                              @
       ! @   PROGRAM YOUR OWN MAP OUTPUT ROUTINE HERE   @
       ! @   worked out example: U_WMAP (see TRM FLOW)  @
       ! @                                              @
       ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       !
       call u_wmap(lunus3    ,header    ,runid     ,itime     ,idate     , &
                 & timmin    ,mmax      ,nmax      ,kmax      ,nmaxus    , &
                 & nsrc      ,ltur      ,lmaxd     ,lstsci    ,lsal      , &
                 & ltem      ,ilay      ,zmodel    ,i(kfu)    ,i(kfv)    , &
                 & i(kcs)    ,i(kfs)    ,i(kfumin) ,i(kfvmin) ,i(kfumax) , &
                 & i(kfvmax) ,i(kfsmin) ,i(kfsmax) ,i(mnksrc) ,r(xz)     , &
                 & r(yz)     ,d(dps)    ,r(rho)    ,r(s1)     ,r(u1)     , &
                 & r(v1)     ,r(w1)     ,r(wphy)   ,r(r1)     ,r(p1)     , &
                 & r(rtur1)  ,r(taubpu) ,r(taubpv) ,r(taubsu) ,r(taubsv) , &
                 & r(vicww)  ,r(dicww)  ,r(vicuv)  ,r(rich)   ,r(umnldf) , &
                 & r(vmnldf) )
    endif
    !
    ! set first to false
    !
    if (first) first = .false.
    !
    ! Close all opened USER files at the end of simulation
    ! in this example all 3 files are closed
    !
 9999 continue
    if (nst == itfinish) then
       if (lunus1 /= 0) close (lunus1)
       if (lunus2 /= 0) close (lunus2)
       if (lunus3 /= 0) close (lunus3)
    endif
end subroutine u_ppr
