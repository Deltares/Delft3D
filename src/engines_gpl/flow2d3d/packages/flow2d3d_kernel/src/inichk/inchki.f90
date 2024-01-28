subroutine inchki(lundia    ,error     ,runid     ,sferic    ,filrgf    , &
                & dx        ,dy        ,anglat    ,anglon    ,grdang    , &
                & tgfcmp    ,riglid    ,keva      , &
                & temeqs    ,saleqs    ,ktemp     ,fclou     , &
                & sarea     ,roumet    ,rouflo    ,restid    , &
                & lturi     ,gdp       )
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
!              defines initial arrays
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
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
    include 'tri-dyn.igd'
    integer(pntrsize)      , pointer :: alfas
    integer(pntrsize)      , pointer :: c
    integer(pntrsize)      , pointer :: cfurou
    integer(pntrsize)      , pointer :: cfvrou
    integer(pntrsize)      , pointer :: dicuv
    integer(pntrsize)      , pointer :: dp
    integer(pntrsize)      , pointer :: fcorio
    integer(pntrsize)      , pointer :: gsqd
    integer(pntrsize)      , pointer :: gsqs
    integer(pntrsize)      , pointer :: guu
    integer(pntrsize)      , pointer :: guv
    integer(pntrsize)      , pointer :: gvu
    integer(pntrsize)      , pointer :: gvv
    integer(pntrsize)      , pointer :: hu
    integer(pntrsize)      , pointer :: pship
    integer(pntrsize)      , pointer :: r1
    integer(pntrsize)      , pointer :: rho
    integer(pntrsize)      , pointer :: rob
    integer(pntrsize)      , pointer :: rtu2d0
    integer(pntrsize)      , pointer :: rtur1
    integer(pntrsize)      , pointer :: s1
    integer(pntrsize)      , pointer :: sig
    integer(pntrsize)      , pointer :: sigdif
    integer(pntrsize)      , pointer :: sigmol
    integer(pntrsize)      , pointer :: thick
    integer(pntrsize)      , pointer :: vicuv
    integer(pntrsize)      , pointer :: ws
    integer(pntrsize)      , pointer :: x2y
    integer(pntrsize)      , pointer :: x3
    integer(pntrsize)      , pointer :: xcor
    integer(pntrsize)      , pointer :: xy2
    integer(pntrsize)      , pointer :: xz
    integer(pntrsize)      , pointer :: y3
    integer(pntrsize)      , pointer :: ycor
    integer(pntrsize)      , pointer :: yz
    integer(pntrsize)      , pointer :: res
    integer(pntrsize)      , pointer :: xj
    integer(pntrsize)      , pointer :: guz
    integer(pntrsize)      , pointer :: gvz
    integer(pntrsize)      , pointer :: gud
    integer(pntrsize)      , pointer :: gvd
    integer(pntrsize)      , pointer :: gsqiu
    integer(pntrsize)      , pointer :: gsqiv
    integer(pntrsize)      , pointer :: irocol
    integer(pntrsize)      , pointer :: kcs
    integer(pntrsize)      , pointer :: kcu
    integer(pntrsize)      , pointer :: kcv
    integer(pntrsize)      , pointer :: kfs
    integer(pntrsize)      , pointer :: kspu
    integer(pntrsize)      , pointer :: kspv
    integer(pntrsize)      , pointer :: mnbnd
    integer(pntrsize)      , pointer :: nob
    integer(pntrsize)      , pointer :: typbnd
    integer                , pointer :: nmax
    integer                , pointer :: mmax
    integer                , pointer :: ddbound
    integer                , pointer :: nmaxus
    integer                , pointer :: kmax
    integer                , pointer :: nmaxd
    integer                , pointer :: mmaxd
    integer                , pointer :: jstart
    integer                , pointer :: nmmaxj
    integer                , pointer :: nmmax
    integer                , pointer :: lmax
    integer                , pointer :: lsts
    integer                , pointer :: lstsc
    integer                , pointer :: lstsci
    integer                , pointer :: lsecfl
    integer                , pointer :: lsec
    integer                , pointer :: ltur
    integer                , pointer :: kmxt
    integer                , pointer :: nlcest
    integer                , pointer :: noroco
    integer                , pointer :: norow
    integer                , pointer :: nocol
    integer                , pointer :: nto
    integer                , pointer :: kc
    integer                , pointer :: nopest
    integer                , pointer :: nrob
    integer                , pointer :: nostat
    integer                , pointer :: ntruv
    integer                , pointer :: ntru
    integer                , pointer :: iter2
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: rhoa
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: z0
    real(fp)               , pointer :: vonkar
    real(fp)               , pointer :: vicmol
    real(fp)               , pointer :: vicoww
    real(fp)               , pointer :: dicoww
    real(fp)               , pointer :: sboltz
    real(fp), dimension(:) , pointer :: wstcof
    integer                , pointer :: iro
    logical                , pointer :: wind
    logical                , pointer :: salin
    logical                , pointer :: temp
    logical                , pointer :: wave
    logical                , pointer :: iweflg
    logical                , pointer :: struct
    logical                , pointer :: lftrto
    logical                , pointer :: veg3d
    logical                , pointer :: fldry
    logical                , pointer :: fltd
    logical                , pointer :: solrad_read
    logical                , pointer :: swrf_file
    character(256)         , pointer :: flbdfh
    real(fp), dimension(:) , pointer :: duneheight
!
! Global variables
!
    integer       :: keva   !  Description and declaration in tricom.igs
    integer       :: ktemp  !  Description and declaration in tricom.igs
    integer       :: lturi  !  Description and declaration in tricom.igs
    integer       :: lundia !  Description and declaration in inout.igs
    logical       :: error  !!  Flag=TRUE if an error is encountered
    logical       :: sferic !  Description and declaration in tricom.igs
    real(fp)      :: anglat !!  - Angle of latitude of the model
                            !!    centre (used to determine the coef.
                            !!    for the coriolis force)
                            !!  - In spherical coordinates this para-
                            !!    meter equals the angle of latitude
                            !!    for the origin (water level point)
                            !!    after INIPHY anglat = 0.
    real(fp)      :: anglon !!  - Angle of longitude of the model
                            !!    centre (used to determine solar radiation)
    real(fp)      :: dx     !!  Uniform grid-distance in the x-dir.
                            !!  in meters
                            !!  if sferic = T dx := dphi
                            !!  in (decimal) degrees
    real(fp)      :: dy     !!  Uniform grid-distance in the y-dir.
                            !!  in meters
                            !!  if sferic = T dy := dtheta
                            !!  in (decimals) degrees
    real(fp)      :: fclou  !  Description and declaration in heat.igs
    real(fp)      :: grdang !  Description and declaration in tricom.igs
    real(fp)      :: riglid !!  Rigid lid factor to reduce horizontal
                            !!  wet area (incompressible)
    real(fp)      :: saleqs !  Description and declaration in tricom.igs
    real(fp)      :: sarea  !  Description and declaration in heat.igs
    real(fp)      :: temeqs !  Description and declaration in tricom.igs
    character(*)  :: filrgf !!  File name for the curvi-linear grid
                            !!  file (telmcrgf.xxx)
                            !!  !! file will be read formatted !!
    character(*)  :: restid !!  Run identification of the restart
                            !!  file. If RESTID = non-blank then
                            !!  current simulation will use this
                            !!  file to for setting the initial
                            !!  conditions
    character(*)  :: runid  !!  Run identification code for the cur-
                            !!  rent simulation (used to determine
                            !!  the names of the in- /output files
                            !!  used by the system)
    character(1)  :: roumet !!  Bed stress formulation specified:
                            !!   C : Chezy    W : White Colebrook
                            !!   M : Manning  Z : roughness par.
    character(36) :: tgfcmp !  Description and declaration in tricom.igs
    character(4)  :: rouflo !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer :: icx
    integer :: icy
    integer(pntrsize) :: iofset  ! Offset to get begin pointer of array at point (1,1) instead of (1,-1)
    integer :: mmaxddb
    integer :: nmaxddb
    integer :: nmtot   ! Total number of array elements NMAX * (MMAX + 4)
!
!! executable statements -------------------------------------------------------
!
    nmax        => gdp%d%nmax
    mmax        => gdp%d%mmax
    ddbound     => gdp%d%ddbound
    nmaxus      => gdp%d%nmaxus
    kmax        => gdp%d%kmax
    nmaxd       => gdp%d%nmaxd
    mmaxd       => gdp%d%mmaxd
    jstart      => gdp%d%jstart
    nmmaxj      => gdp%d%nmmaxj
    nmmax       => gdp%d%nmmax
    lmax        => gdp%d%lmax
    lsts        => gdp%d%lsts
    lstsc       => gdp%d%lstsc
    lstsci      => gdp%d%lstsci
    lsecfl      => gdp%d%lsecfl
    lsec        => gdp%d%lsec
    ltur        => gdp%d%ltur
    kmxt        => gdp%d%kmxt
    nlcest      => gdp%d%nlcest
    noroco      => gdp%d%noroco
    norow       => gdp%d%norow
    nocol       => gdp%d%nocol
    nto         => gdp%d%nto
    kc          => gdp%d%kc
    nopest      => gdp%d%nopest
    nrob        => gdp%d%nrob
    nostat      => gdp%d%nostat
    ntruv       => gdp%d%ntruv
    ntru        => gdp%d%ntru
    iter2       => gdp%gdnumeco%iter2
    rhow        => gdp%gdphysco%rhow
    rhoa        => gdp%gdphysco%rhoa
    ag          => gdp%gdphysco%ag
    z0          => gdp%gdphysco%z0
    vonkar      => gdp%gdphysco%vonkar
    vicmol      => gdp%gdphysco%vicmol
    vicoww      => gdp%gdphysco%vicoww
    dicoww      => gdp%gdphysco%dicoww
    sboltz      => gdp%gdphysco%sboltz
    wstcof      => gdp%gdphysco%wstcof
    iro         => gdp%gdphysco%iro
    wind        => gdp%gdprocs%wind
    salin       => gdp%gdprocs%salin
    temp        => gdp%gdprocs%temp
    wave        => gdp%gdprocs%wave
    iweflg      => gdp%gdprocs%iweflg
    struct      => gdp%gdprocs%struct
    lftrto      => gdp%gdprocs%lftrto
    veg3d       => gdp%gdprocs%veg3d
    alfas       => gdp%gdr_i_ch%alfas
    c           => gdp%gdr_i_ch%c
    cfurou      => gdp%gdr_i_ch%cfurou
    cfvrou      => gdp%gdr_i_ch%cfvrou
    dicuv       => gdp%gdr_i_ch%dicuv
    dp          => gdp%gdr_i_ch%dp
    fcorio      => gdp%gdr_i_ch%fcorio
    gsqd        => gdp%gdr_i_ch%gsqd
    gsqs        => gdp%gdr_i_ch%gsqs
    guu         => gdp%gdr_i_ch%guu
    guv         => gdp%gdr_i_ch%guv
    gvu         => gdp%gdr_i_ch%gvu
    gvv         => gdp%gdr_i_ch%gvv
    hu          => gdp%gdr_i_ch%hu
    pship       => gdp%gdr_i_ch%pship
    r1          => gdp%gdr_i_ch%r1
    rho         => gdp%gdr_i_ch%rho
    rob         => gdp%gdr_i_ch%rob
    rtu2d0      => gdp%gdr_i_ch%rtu2d0
    rtur1       => gdp%gdr_i_ch%rtur1
    s1          => gdp%gdr_i_ch%s1
    sig         => gdp%gdr_i_ch%sig
    sigdif      => gdp%gdr_i_ch%sigdif
    sigmol      => gdp%gdr_i_ch%sigmol
    thick       => gdp%gdr_i_ch%thick
    vicuv       => gdp%gdr_i_ch%vicuv
    ws          => gdp%gdr_i_ch%ws
    x2y         => gdp%gdr_i_ch%x2y
    x3          => gdp%gdr_i_ch%x3
    xcor        => gdp%gdr_i_ch%xcor
    xy2         => gdp%gdr_i_ch%xy2
    xz          => gdp%gdr_i_ch%xz
    y3          => gdp%gdr_i_ch%y3
    ycor        => gdp%gdr_i_ch%ycor
    yz          => gdp%gdr_i_ch%yz
    res         => gdp%gdr_i_ch%res
    xj          => gdp%gdr_i_ch%xj
    guz         => gdp%gdr_i_ch%guz
    gvz         => gdp%gdr_i_ch%gvz
    gud         => gdp%gdr_i_ch%gud
    gvd         => gdp%gdr_i_ch%gvd
    gsqiu       => gdp%gdr_i_ch%gsqiu
    gsqiv       => gdp%gdr_i_ch%gsqiv
    irocol      => gdp%gdr_i_ch%irocol
    kcs         => gdp%gdr_i_ch%kcs
    kcu         => gdp%gdr_i_ch%kcu
    kcv         => gdp%gdr_i_ch%kcv
    kfs         => gdp%gdr_i_ch%kfs
    kspu        => gdp%gdr_i_ch%kspu
    kspv        => gdp%gdr_i_ch%kspv
    mnbnd       => gdp%gdr_i_ch%mnbnd
    nob         => gdp%gdr_i_ch%nob
    typbnd      => gdp%gdr_i_ch%typbnd
    fldry       => gdp%gdtmpfil%fldry
    fltd        => gdp%gdtmpfil%fltd
    flbdfh      => gdp%gdbedformpar%flbdfh
    duneheight  => gdp%gdbedformpar%duneheight
    solrad_read => gdp%gdheat%solrad_read
    swrf_file   => gdp%gdheat%swrf_file
    !
    icx     = 0
    icy     = 0
    nmaxddb = nmax + 2*gdp%d%ddbound
    mmaxddb = mmax + 2*gdp%d%ddbound
    iofset  = 2*nmaxddb
    nmtot   = nmaxddb*(mmaxddb + 4)
    !
    ! Initialize global parameters
    !
    nrob  = 0
    iter2 = 2
    !
    ! INIGRD: initialises ICOM
    !         KCU   (NMAX  ,-1:MMAX+2) here ICOM  (MMAX  ,NMAX     )
    !         KCV   (NMAX  ,-1:MMAX+2) here IPX   (*     )
    !         KCS   (NMAX  ,-1:MMAX+2) here IPY   (*     )
    !
    call inigrd(lundia    ,error     ,runid     ,nmax      ,mmax      , &
              & nmaxus    ,i(kcu)    ,i(kcv)    ,i(kcs)    ,gdp       )
    !
    if (error) goto 9999
    !
    ! re-initialize arrays KCV & KCS used as hulpspace in INIGRD
    ! Total length of array NMAX * (MMAX + 4)
    !
    call nuliar(i(kcv)    ,nmtot     )
    call nuliar(i(kcs)    ,nmtot     )
    !
    ! INIBND: initialize NOB array and calculate NROB (for NTO > 0)
    !         NOB's dimensions are (8,NOPEST). In later calls the
    !         dimensions  will be (8,NROB  ). Because the variable
    !         dimension is the last, it is ok
    !
    if (nto > 0) then
       call inibnd(lundia    ,error     ,nto       ,nopest    ,nrob      , &
                 & i(mnbnd)  ,i(nob)    ,ch(typbnd),gdp       )
       if (error) goto 9999
    endif
    !
    ! CHKBND: build IROCOL table opening code
    !
    call chkbnd(lundia    ,error     ,nmax      ,mmax      ,nrob      , &
              & nlcest    ,noroco    ,norow     ,nocol     ,i(nob)    , &
              & i(irocol) ,i(kcu)    ,i(kcv)    ,i(mnbnd)  ,gdp       )
    if (error) goto 9999
    !
    ! re-initialize KCU & KCV used as help array in INIBND & CHKBND
    !
    call nuliar(i(kcu)    ,nmtot     )
    call nuliar(i(kcv)    ,nmtot     )
    !
    ! CHKKC : check dry points and thin dams and initilize KCU, KCV
    !         and KCS arrays
    !
    call chkkc(lundia    ,error     ,runid     ,fldry     ,fltd      , &
             & noroco    ,norow     ,mmax      ,nmax      ,i(irocol) , &
             & i(kcu)    ,i(kcv)    ,i(kcs)    ,gdp       )
    if (error) goto 9999
    !
    ! check important geometry parameters and
    ! redefine THICK array and initialize SIG
    !
    call chkgeo(lundia    ,error     , &
              & kmax      ,r(thick)  ,r(sig)    ,gdp       )
    if (error) goto 9999
    !
    ! INISFE: is removed
    !
    !
    ! INIGEO: read and initilize geometry
    ! In order to address the arrays in routine INIGEO with (1,1), we
    ! have to add IOFSET to all array pointers.
    ! This is because INIGEO will be used by other programs as well
    !
    call inigeo(lundia    ,error     ,filrgf    ,sferic    ,            &
              & dx        ,dy        ,mmax      ,nmax      ,nmaxus    , &
              & r(alfas + iofset)    ,r(fcorio + iofset)   ,r(xcor + iofset)     ,r(ycor + iofset)     ,r(xz + iofset)       , &
              & r(yz + iofset)       ,r(guu + iofset)      ,r(gvv + iofset)      ,r(guv + iofset)      ,r(gvu + iofset)      , &
              & r(gsqs + iofset)     ,r(gsqd + iofset)     ,r(guz + iofset)      ,r(gvz + iofset)      ,r(gud + iofset)      , &
              & r(gvd + iofset)      ,r(gsqiu + iofset)    ,r(gsqiv + iofset)    ,i(kcu + iofset)      ,i(kcv + iofset)      , &
              & i(kcs + iofset)      ,r(guu)               ,r(gvv)               ,gdp       )
    if (error) goto 9999
    !
    call griddims_admin( i(kcs), r(xz), r(yz), r(xcor), r(ycor),  gdp )
    !
    ! DFUPDGEO: exchange geometrical information as computed in routine inigeo with neighbours in case of parallel runs
    !
    call dfupdgeo( r(alfas)    ,r(fcorio)   ,r(xcor)     ,r(ycor)     ,r(xz)       , &
                 & r(yz)       ,r(guu)      ,r(gvv)      ,r(guv)      ,r(gvu)      , &
                 & r(gsqs)     ,r(gsqd)     ,r(guz)      ,r(gvz)      ,r(gud)      , &
                 & r(gvd)      ,r(gsqiu)    ,r(gsqiv)    ,gdp         )
    ! INIPHY: set initial values for IRO, Z0, VONKAR, VICOM & SBOLTZ
    !         and re-define ANGLAT and GRDANG for SFERIC = .true.
    !
    call iniphy(iro       ,z0        ,vonkar    ,vicmol    ,sboltz    , &
              & sferic    ,anglat    ,grdang    ,temeqs    ,gdp       )
    !
    ! CHKPHY: checks all physical coefficients
    !
    call chkphy(lundia     ,error     ,salin     ,temp      ,wind      , &
              & nmax       ,mmax      ,kmax      ,lmax      ,ktemp     , &
              & temeqs     ,saleqs    ,fclou     ,sarea     ,wstcof    , &
              & rhow       ,rhoa      ,i(kcu)    ,i(kcv)    ,i(kcs)    , &
              & r(cfurou)  ,r(cfvrou) ,r(vicuv)  ,r(dicuv)  ,anglon    , &
              & solrad_read,swrf_file ,sferic    ,gdp       )
    if (error) goto 9999
    !
    ! CHKTRT: checks Trachytopes if defined
    !
    if (lftrto) then
       call chktrt(lundia    ,error     ,nmax      ,mmax      ,nmaxus    , &
                 & i(kcu)    ,i(kcv)    ,gdp)
    endif
    if (error) goto 9999
    !
    ! CHKNUM: checks all numerical parameters
    !
    call chknum(lundia    ,error     ,roumet    , &
              & rouflo    ,gdp       )
    if (error) goto 9999
    !
    ! CHKSIT: check stations and cross sections
    !
    call chksit(lundia    ,error     ,nostat    ,ntruv     ,ntru      , &
              & nmax      ,mmax      ,nmaxus    ,i(kcu)    ,i(kcv)    , &
              & i(kcs)    ,gdp       )
    if (error) goto 9999
    !
    ! CALPSH: Define KSPU/V definition for floating structure
    !         Substract floating structure depth PSHIP from initial
    !         water-level S1 and re-define pressure from floating
    !         structure depth (in N/m2)
    !
    call calpsh(restid    ,error     ,nmax      ,mmax      ,nmaxus    , &
              & i(kcs)    ,r(pship)  ,r(s1)     ,gdp       )
    if (error) goto 9999
    !
    ! CHKSTR: check structures
    !
    call chkstr(lundia    ,nmax      ,mmax      ,nmaxus    , &
              & kmax      ,i(kcu)    ,i(kcv)    ,i(kspu)   ,i(kspv)   , &
              & gdp       )
    !
    ! INIRGL: re-set horizontal wet area due to value of RIGLID
    !
    call inirgl(riglid    ,nmax      ,mmax      ,nmaxus    ,kmax      , &
              & i(kcs)    ,i(kspu)   ,i(kspv)   ,r(gsqs)   ,gdp       )
    !
    ! CHKIC : check initial values for initial condition
    !         only if LMAX .gt. 0
    !
    if (lmax > 0) then
       call chkic(lundia    ,error     ,mmax      ,nmax      ,kmax      , &
                & nmaxus    ,lstsci    ,ltur      ,lturi     ,lsecfl    , &
                & r(r1)     ,i(kfs)    ,r(rtur1)  ,gdp       )
       if (error) goto 9999
    endif
    !
    ! INITGF: set tide generating forces arrays if TGFCMP <> ' '
    !
    if (tgfcmp /= ' ') then
       call initgf(tgfcmp    ,gdp       )
    endif
    !
    ! TKECOF: set initial turbulence model coefficients
    !
    call tkecof(lturi     ,vonkar    ,r(sigdif) ,r(sigmol) , &
              & r(rtur1)  ,r(rtu2d0) ,gdp       )
    !
    ! IWE
    !
    if (iweflg) then
       !
       ! CHKIWE: check selected point for diagnostic and
       !         re-define VICOWW and DICOWW (set 0.)
       !
       call chkiwe(lundia    ,error     ,nmax      ,mmax      ,nmaxus    , &
                 & vicoww    ,dicoww    ,i(kcs)    ,gdp       )
       if (error) goto 9999
       !
       ! IWECOF: set initial internal wave coeffiecients
       !
       call iwecof(kmxt      ,gdp       )
    endif
    !
    ! XYDER : calculate coefficients for streakline
    ! subroutine parameter(4) = ICX := NMAX
    ! subroutine parameter(5) = ICY := 1
    !
    if (lsec > 0) then
       icx = nmaxddb
       icy = 1
       call xyder(jstart    ,nmmaxj    ,nmmax     ,icx       ,icy       , &
                & r(xcor)   ,r(ycor)   ,i(kcs)    ,r(x3)     ,r(x2y)    , &
                & r(xy2)    ,r(y3)     ,gdp       )
    endif
    !
    ! Read and initialize (Rigid) 3D Vegetation Model input if veg3d = .true.
    !
    if (veg3d) then
       call rdveg3d(mmax      ,nmax      ,nmaxus    , &
                  & r(xz)     ,r(yz)     ,gdp       )
    endif
    !
    ! Allocate arrays for 2D advection solver
    !
    call allocadv2d(norow, nocol, gdp)
    !
    ! For bedform:
    ! Call mirror routine to fill external points on the boundary with the internal value in x-direction
    ! This call was originally located in rdbedformpar, but at that time, kcs was not defined yet.
    !
    if (flbdfh /= ' ') then
       call mirror_bnd(1       ,gdp%d%nub-gdp%d%nlb+1,gdp%d%nmmax     , &
                     & i(kcs)  ,duneheight           ,gdp%d%nmlb      ,gdp%d%nmub      )
    endif
    !
    ! Allocate and initialize arrays for output of heatfluxes
    !
    call init_out_heatfluxes(lundia, ktemp, keva, nostat, gdp)
    !
 9999 continue
end subroutine inchki
