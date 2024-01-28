subroutine wrm_main(lundia    ,error     ,selmap    ,grdang    ,dtsec     , &
                  & itmapc    ,runtxt    ,trifil    ,wrifou    ,gdp       )
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
!    Function: Main routine for writing the FLOW HIS file.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall, only: inode, master, parll, dfint, nproc
    use datagroups
    use netcdf
    use dffunctionals, only: dffind_duplicate
    use flow2d3d_version_module
    !
    use globaldata
    !
    implicit none
    !
    include 'fsm.i'
    include 'tri-dyn.igd'
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                              , pointer :: itmapl
    integer                              , pointer :: nmax
    integer                              , pointer :: nmaxus
    integer                              , pointer :: mmax
    integer                              , pointer :: kmax
    integer                              , pointer :: lmax
    integer                              , pointer :: lstsci
    integer                              , pointer :: lsed
    integer                              , pointer :: lsedtot
    integer                              , pointer :: ltur
    integer                              , pointer :: noroco
    integer                              , pointer :: norow
    integer                              , pointer :: nsrc
    integer                              , pointer :: nostat
    integer                              , pointer :: ntruv
    logical                              , pointer :: lfsdu
    logical                              , pointer :: zmodel
    integer(pntrsize)                    , pointer :: ibuff
    integer(pntrsize)                    , pointer :: irocol
    integer(pntrsize)                    , pointer :: kcs
    integer(pntrsize)                    , pointer :: kcu
    integer(pntrsize)                    , pointer :: kcv
    integer(pntrsize)                    , pointer :: kfs
    integer(pntrsize)                    , pointer :: kfu
    integer(pntrsize)                    , pointer :: kfv
    integer(pntrsize)                    , pointer :: kfumin
    integer(pntrsize)                    , pointer :: kfvmin
    integer(pntrsize)                    , pointer :: kfsmin
    integer(pntrsize)                    , pointer :: kfumax
    integer(pntrsize)                    , pointer :: kfvmax
    integer(pntrsize)                    , pointer :: kfsmax
    integer(pntrsize)                    , pointer :: mnksrc
    integer(pntrsize)                    , pointer :: alfas
    integer(pntrsize)                    , pointer :: cfurou
    integer(pntrsize)                    , pointer :: cfvrou
    integer(pntrsize)                    , pointer :: cvalu0
    integer(pntrsize)                    , pointer :: cvalv0
    integer(pntrsize)                    , pointer :: dicww
    integer(pntrsize)                    , pointer :: dp
    integer(pntrsize)                    , pointer :: dps
    integer(pntrsize)                    , pointer :: dpu
    integer(pntrsize)                    , pointer :: dpv
    integer(pntrsize)                    , pointer :: dzs1
    integer(pntrsize)                    , pointer :: enstro
    integer(pntrsize)                    , pointer :: eroll1
    integer(pntrsize)                    , pointer :: evap
    integer(pntrsize)                    , pointer :: ewave1
    integer(pntrsize)                    , pointer :: fxw
    integer(pntrsize)                    , pointer :: fyw
    integer(pntrsize)                    , pointer :: gsqs
    integer(pntrsize)                    , pointer :: guu
    integer(pntrsize)                    , pointer :: gvv
    integer(pntrsize)                    , pointer :: hrms
    integer(pntrsize)                    , pointer :: p1
    integer(pntrsize)                    , pointer :: patm
    integer(pntrsize)                    , pointer :: precip
    integer(pntrsize)                    , pointer :: qxkr
    integer(pntrsize)                    , pointer :: qxkw
    integer(pntrsize)                    , pointer :: qykr
    integer(pntrsize)                    , pointer :: qykw
    integer(pntrsize)                    , pointer :: r1
    integer(pntrsize)                    , pointer :: rbuff
    integer(pntrsize)                    , pointer :: rho
    integer(pntrsize)                    , pointer :: rich
    integer(pntrsize)                    , pointer :: rlabda
    integer(pntrsize)                    , pointer :: rtur1
    integer(pntrsize)                    , pointer :: s1
    integer(pntrsize)                    , pointer :: sig
    integer(pntrsize)                    , pointer :: taubmx
    integer(pntrsize)                    , pointer :: taubpu
    integer(pntrsize)                    , pointer :: taubpv
    integer(pntrsize)                    , pointer :: taubsu
    integer(pntrsize)                    , pointer :: taubsv
    integer(pntrsize)                    , pointer :: teta
    integer(pntrsize)                    , pointer :: thick
    integer(pntrsize)                    , pointer :: tp
    integer(pntrsize)                    , pointer :: u1
    integer(pntrsize)                    , pointer :: umnldf
    integer(pntrsize)                    , pointer :: uorb
    integer(pntrsize)                    , pointer :: v1
    integer(pntrsize)                    , pointer :: vicuv
    integer(pntrsize)                    , pointer :: vicww
    integer(pntrsize)                    , pointer :: vmnldf
    integer(pntrsize)                    , pointer :: vortic
    integer(pntrsize)                    , pointer :: w1
    integer(pntrsize)                    , pointer :: windcd
    integer(pntrsize)                    , pointer :: windu
    integer(pntrsize)                    , pointer :: windv
    integer(pntrsize)                    , pointer :: wphy
    integer(pntrsize)                    , pointer :: wrkb3
    integer(pntrsize)                    , pointer :: wrkb4
    integer(pntrsize)                    , pointer :: wsu
    integer(pntrsize)                    , pointer :: wsv
    integer(pntrsize)                    , pointer :: xcor
    integer(pntrsize)                    , pointer :: xz
    integer(pntrsize)                    , pointer :: ycor
    integer(pntrsize)                    , pointer :: yz
    integer(pntrsize)                    , pointer :: z0ucur
    integer(pntrsize)                    , pointer :: z0vcur
    integer(pntrsize)                    , pointer :: z0urou
    integer(pntrsize)                    , pointer :: z0vrou
    integer(pntrsize)                    , pointer :: namcon
    integer(pntrsize)                    , pointer :: namsrc
    integer                              , pointer :: itdate
    real(fp)                             , pointer :: dt
    real(fp)                             , pointer :: tunit
    real(fp)                             , pointer :: tzone
    logical                              , pointer :: dredge
    logical                              , pointer :: lfbedfrm
    logical                              , pointer :: roller
    logical                              , pointer :: sferic
    logical                              , pointer :: xbeach
    character(20) , dimension(:)         , pointer :: namsed
    character(8)                         , pointer :: dpsopt
    character(4)                         , pointer :: rouflo
    integer       , dimension(:)         , pointer :: order_sta
    integer       , dimension(:)         , pointer :: order_tra
    !
    type (datagroup)                     , pointer :: group
    logical                              , pointer :: first
    real(fp)                             , pointer :: rhow
    integer                              , pointer :: ktemp
    !
    integer                              , pointer :: mfg
    integer                              , pointer :: mlg
    integer                              , pointer :: nfg
    integer                              , pointer :: nlg
    integer                              , pointer :: nmaxgl
    integer                              , pointer :: mmaxgl
    integer       , dimension(:,:)       , pointer :: iarrc
    integer       , dimension(:)         , pointer :: mf
    integer       , dimension(:)         , pointer :: ml
    integer       , dimension(:)         , pointer :: nf
    integer       , dimension(:)         , pointer :: nl
    !
    logical                              , pointer :: mergemap
!
! Global variables
!
    integer                                      :: itmapc !!  Current time counter for the map data file
    integer                                      :: lundia !  Description and declaration in inout.igs
    logical                                      :: error  !!  Flag=TRUE if an error is encountered
    real(fp)                                     :: dtsec  !!  Integration time step [in seconds]
    real(fp)                                     :: grdang !  Description and declaration in tricom.igs
    character(23)                                :: selmap !  Description and declaration in tricom.igs
    character(30) , dimension(10)                :: runtxt !!  Textual description of model input
    character(*)                   , intent(in)  :: trifil !  File name for FLOW NEFIS output files (tri"h/m"-"casl""labl".dat/def)
    logical                                      :: wrifou
!
! Local variables
!
    character(10)                                     :: velt          ! Velocity type 'Eulerian' or 'GLM'
    integer                                           :: nostatgl      ! global number of stations (i.e. original number excluding duplicate stations located in the halo regions)
    integer                                           :: nostatto      ! total number of stations (including "duplicate" stations located in halo regions)
    integer                                           :: ntruvgl       ! global number of tracks (i.e. original number excluding duplicate stations located in the halo regions)
    integer                                           :: ntruvto       ! total number of tracks (including "duplicate" stations located in halo regions)
    integer                                           :: irequest
    integer                                           :: ierror
    integer                                           :: ifile
    integer                                           :: filetype
    integer                                           :: fds
    integer                                , external :: open_datdef
    integer                                , external :: clsnef
    character(256)                                    :: filename
    integer(pntrsize)                                 :: velu           ! U velocity array (FSM r-index)
    integer(pntrsize)                                 :: velv           ! V velocity array (FSM r-index)
    character(256)                                    :: full_version
    character(8)                                      :: cdate
    character(10)                                     :: ctime
    character(5)                                      :: czone
    character(1024)                                   :: error_string
    character(16)                                     :: simdat        ! Simulation date representing the flow condition at this date
    character(20)                                     :: rundat        ! Execution date of the simulation
    character(6)                                      :: ftype         ! String containing to which output file version group should be written
    !
    integer                                           :: ii
    integer                                           :: ip
    integer                                           :: istat
    integer                                           :: n
    integer                                           :: m
    character(4)                                      :: part_nr
    integer                                           :: bck_inode
    integer                                           :: bck_nproc
    logical                                           :: bck_parll
    type(dfparalltype)                  , pointer     :: bck_gdparall
    integer      , dimension(:,:)       , allocatable :: ipartition
!
!! executable statements -------------------------------------------------------
!
    wrkb3               => gdp%gdaddress%wrkb3
    wrkb4               => gdp%gdaddress%wrkb4
    itmapl              => gdp%gdinttim%itmapl
    nmax                => gdp%d%nmax
    nmaxus              => gdp%d%nmaxus
    mmax                => gdp%d%mmax
    kmax                => gdp%d%kmax
    lmax                => gdp%d%lmax
    lstsci              => gdp%d%lstsci
    lsed                => gdp%d%lsed
    lsedtot             => gdp%d%lsedtot
    ltur                => gdp%d%ltur
    noroco              => gdp%d%noroco
    norow               => gdp%d%norow
    nsrc                => gdp%d%nsrc
    nostat              => gdp%d%nostat
    ntruv               => gdp%d%ntruv
    zmodel              => gdp%gdprocs%zmodel
    ibuff               => gdp%gdr_i_ch%ibuff
    irocol              => gdp%gdr_i_ch%irocol
    kcs                 => gdp%gdr_i_ch%kcs
    kcu                 => gdp%gdr_i_ch%kcu
    kcv                 => gdp%gdr_i_ch%kcv
    kfs                 => gdp%gdr_i_ch%kfs
    kfu                 => gdp%gdr_i_ch%kfu
    kfv                 => gdp%gdr_i_ch%kfv
    kfumin              => gdp%gdr_i_ch%kfumin
    kfvmin              => gdp%gdr_i_ch%kfvmin
    kfsmin              => gdp%gdr_i_ch%kfsmin
    kfumax              => gdp%gdr_i_ch%kfumax
    kfvmax              => gdp%gdr_i_ch%kfvmax
    kfsmax              => gdp%gdr_i_ch%kfsmax
    mnksrc              => gdp%gdr_i_ch%mnksrc
    alfas               => gdp%gdr_i_ch%alfas
    cfurou              => gdp%gdr_i_ch%cfurou
    cfvrou              => gdp%gdr_i_ch%cfvrou
    cvalu0              => gdp%gdr_i_ch%cvalu0
    cvalv0              => gdp%gdr_i_ch%cvalv0
    dicww               => gdp%gdr_i_ch%dicww
    dp                  => gdp%gdr_i_ch%dp
    dps                 => gdp%gdr_i_ch%dps
    dpu                 => gdp%gdr_i_ch%dpu
    dpv                 => gdp%gdr_i_ch%dpv
    dzs1                => gdp%gdr_i_ch%dzs1
    enstro              => gdp%gdr_i_ch%enstro
    eroll1              => gdp%gdr_i_ch%eroll1
    evap                => gdp%gdr_i_ch%evap
    ewave1              => gdp%gdr_i_ch%ewave1
    fxw                 => gdp%gdr_i_ch%fxw
    fyw                 => gdp%gdr_i_ch%fyw
    gsqs                => gdp%gdr_i_ch%gsqs
    guu                 => gdp%gdr_i_ch%guu
    gvv                 => gdp%gdr_i_ch%gvv
    hrms                => gdp%gdr_i_ch%hrms
    p1                  => gdp%gdr_i_ch%p1
    patm                => gdp%gdr_i_ch%patm
    precip              => gdp%gdr_i_ch%precip
    qxkr                => gdp%gdr_i_ch%qxkr
    qxkw                => gdp%gdr_i_ch%qxkw
    qykr                => gdp%gdr_i_ch%qykr
    qykw                => gdp%gdr_i_ch%qykw
    r1                  => gdp%gdr_i_ch%r1
    rbuff               => gdp%gdr_i_ch%rbuff
    rho                 => gdp%gdr_i_ch%rho
    rich                => gdp%gdr_i_ch%rich
    rlabda              => gdp%gdr_i_ch%rlabda
    rtur1               => gdp%gdr_i_ch%rtur1
    s1                  => gdp%gdr_i_ch%s1
    sig                 => gdp%gdr_i_ch%sig
    taubmx              => gdp%gdr_i_ch%taubmx
    taubpu              => gdp%gdr_i_ch%taubpu
    taubpv              => gdp%gdr_i_ch%taubpv
    taubsu              => gdp%gdr_i_ch%taubsu
    taubsv              => gdp%gdr_i_ch%taubsv
    teta                => gdp%gdr_i_ch%teta
    thick               => gdp%gdr_i_ch%thick
    tp                  => gdp%gdr_i_ch%tp
    u1                  => gdp%gdr_i_ch%u1
    umnldf              => gdp%gdr_i_ch%umnldf
    uorb                => gdp%gdr_i_ch%uorb
    v1                  => gdp%gdr_i_ch%v1
    vicuv               => gdp%gdr_i_ch%vicuv
    vicww               => gdp%gdr_i_ch%vicww
    vmnldf              => gdp%gdr_i_ch%vmnldf
    vortic              => gdp%gdr_i_ch%vortic
    w1                  => gdp%gdr_i_ch%w1
    windcd              => gdp%gdr_i_ch%windcd
    windu               => gdp%gdr_i_ch%windu
    windv               => gdp%gdr_i_ch%windv
    wphy                => gdp%gdr_i_ch%wphy
    wsu                 => gdp%gdr_i_ch%wsu
    wsv                 => gdp%gdr_i_ch%wsv
    xcor                => gdp%gdr_i_ch%xcor
    xz                  => gdp%gdr_i_ch%xz
    ycor                => gdp%gdr_i_ch%ycor
    yz                  => gdp%gdr_i_ch%yz
    z0ucur              => gdp%gdr_i_ch%z0ucur
    z0vcur              => gdp%gdr_i_ch%z0vcur
    z0urou              => gdp%gdr_i_ch%z0urou
    z0vrou              => gdp%gdr_i_ch%z0vrou
    namcon              => gdp%gdr_i_ch%namcon
    namsrc              => gdp%gdr_i_ch%namsrc
    itdate              => gdp%gdexttim%itdate
    dt                  => gdp%gdexttim%dt
    tunit               => gdp%gdexttim%tunit
    tzone               => gdp%gdexttim%tzone
    dredge              => gdp%gdprocs%dredge
    lfbedfrm            => gdp%gdbedformpar%lfbedfrm
    roller              => gdp%gdprocs%roller
    sferic              => gdp%gdtricom%sferic
    xbeach              => gdp%gdprocs%xbeach
    namsed              => gdp%gdsedpar%namsed
    dpsopt              => gdp%gdnumeco%dpsopt
    rouflo              => gdp%gdtricom%rouflo
    rhow                => gdp%gdphysco%rhow
    ktemp               => gdp%gdtricom%ktemp
    mergemap            => gdp%gdpostpr%mergemap
    lfsdu               => gdp%gdprocs%lfsdu
    !
    if (wrifou) then
       ifile = FILOUT_FOU
       filename = trifil(1:3) // 'f' // trifil(5:)
    else
       ifile = FILOUT_MAP
       filename = trifil(1:3) // 'm' // trifil(5:)
    endif
    !
    call getdatagroup(gdp, ifile, 'map-const', group)
    first               => group%first
    !
    if (first) then
       mfg                 => gdp%gdparall%mfg
       nfg                 => gdp%gdparall%nfg
       mf                  => gdp%gdparall%mf
       ml                  => gdp%gdparall%ml
       nf                  => gdp%gdparall%nf
       nl                  => gdp%gdparall%nl
       !
       allocate(ipartition(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), stat=istat)
       if (parll) then
          do ip = 0, nproc-1
             do n = max(gdp%d%nlb,nf(ip)-nfg+1), min(nl(ip)-nfg+1,gdp%d%nub)
                do m = max(gdp%d%mlb,mf(ip)-mfg+1), min(ml(ip)-mfg+1,gdp%d%mub)
                   ipartition(n, m) = ip
                enddo
             enddo
          enddo
       else
          ipartition = 0
       endif
    endif
    !
    if (mergemap) then
        part_nr = '';
    else
        bck_inode = inode
        bck_nproc = nproc
        bck_parll = parll
        bck_gdparall => gdp%gdparall
        !
        write(part_nr,'(A,I3.3)') '-',inode
        !
        inode = 1
        nproc = 1
        parll = .false.
        !
        if (first) then
           gdp%iopartit%mfg = 1
           gdp%iopartit%mlg = mmax
           gdp%iopartit%nfg = 1
           gdp%iopartit%nlg = nmaxus
           gdp%iopartit%mmaxgl = mmax
           gdp%iopartit%nmaxgl = nmaxus
           allocate(gdp%iopartit%nf(1))
           allocate(gdp%iopartit%nl(1))
           allocate(gdp%iopartit%mf(1))
           allocate(gdp%iopartit%ml(1))
           allocate(gdp%iopartit%iarrc(4,1))
           allocate(gdp%iopartit%order_tra(1:ntruv))
           do ii = 1,ntruv
               gdp%iopartit%order_tra(ii) = ii
           enddo
           allocate(gdp%iopartit%order_sta(1:nostat))
           do ii = 1,nostat
               gdp%iopartit%order_sta(ii) = ii
           enddo
        endif
        gdp%gdparall => gdp%iopartit
    endif
    !
    mfg                 => gdp%gdparall%mfg
    mlg                 => gdp%gdparall%mlg
    nfg                 => gdp%gdparall%nfg
    nlg                 => gdp%gdparall%nlg
    mf                  => gdp%gdparall%mf
    ml                  => gdp%gdparall%ml
    nf                  => gdp%gdparall%nf
    nl                  => gdp%gdparall%nl
    iarrc               => gdp%gdparall%iarrc
    mmaxgl              => gdp%gdparall%mmaxgl
    nmaxgl              => gdp%gdparall%nmaxgl
    order_tra           => gdp%gdparall%order_tra
    order_sta           => gdp%gdparall%order_sta
    !
    call dattim(rundat    )
    simdat(1:16)  = 'yyyymmdd  hhmmss'
    simdat(1:4)   = rundat(1:4)
    simdat(5:6)   = rundat(6:7)
    simdat(7:8)   = rundat(9:10)
    simdat(11:12) = rundat(12:13)
    simdat(13:14) = rundat(15:16)
    simdat(15:16) = rundat(18:19)
    !
    call getfullversionstring_flow2d3d(full_version)
    call date_and_time(cdate, ctime, czone)
    !
    filetype = getfiletype(gdp, ifile)
    filename = trim(filename) // trim(part_nr) 
    if (filetype == FTYPE_NETCDF) filename = trim(filename)//'.nc'
    !
    if (gdp%gdflwpar%flwoutput%veuler) then
       velu = wrkb3
       velv = wrkb4
       velt = 'Eulerian'
    else
       velu = u1
       velv = v1
       velt = 'GLM'
    endif
    !
    if (parll) then
       !
       ! Recalculates the effective number of stations, filtering out duplicates affected to more
       ! than one partition (i.e. located in halos)
       !
       call dfsync(gdp)
       call dffind_duplicate(lundia, nostat, nostatto, nostatgl, order_sta, gdp)
       !
       ! Recalculates the effective global number of cross sections
       !
       call dfsync(gdp)
       call dffind_duplicate(lundia, ntruv, ntruvto, ntruvgl, order_tra, gdp)
       !
       ! When order_tra points to tra_orgline, both partition-related and re-ordering-related stuff are taken care of
       !
       order_tra => gdp%gdstations%tra_orgline
    elseif (mergemap) then ! sequential run
       nostatto = nostat
       nostatgl = nostat
       ntruvto  = ntruv
       ntruvgl  = ntruv
       !
       order_sta => gdp%gdstations%sta_orgline
       order_tra => gdp%gdstations%tra_orgline
    endif
    !
    ierror = 0
    do irequest = REQUESTTYPE_DEFINE, REQUESTTYPE_WRITE
       !
       ! request REQUESTTYPE_DEFINE: define all groups, dimensions, and elements
       !         REQUESTTYPE_WRITE : write the data
       !
       if (irequest == REQUESTTYPE_DEFINE) then
          if (.not.first) cycle
          if (inode /= master) cycle
          call delnef(filename,gdp       )
       endif
       !
       ! create or open the file
       !
       if (inode /= master) then
          ! only the master needs to open the file
       elseif (first .and. irequest == REQUESTTYPE_WRITE) then
          ! the file has already been opened in step 1
       elseif (filetype == FTYPE_NEFIS) then
          if (first .and. irequest == REQUESTTYPE_DEFINE) then              
             write(lundia,*) 'Creating new '//trim(filename)//'.dat'
             write(lundia,*) 'Creating new '//trim(filename)//'.def'
          endif
          ierror = open_datdef(filename ,fds      , .false.)
          if (ierror /= 0) then
             write(error_string,'(2a)') 'While trying to open dat/def-file ',trim(filename)
             call prtnefiserr(trim(error_string), gdp)
          endif
       elseif (filetype == FTYPE_NETCDF) then
          if (first .and. irequest == REQUESTTYPE_DEFINE) then              
             write(lundia,*) 'Creating new '//trim(filename)
             ierror = nf90_create(filename, gdp%gdpostpr%nc_mode, fds); call nc_check_err(lundia, ierror, "creating file", filename)
             !
             ! global attributes
             !
             ierror = nf90_put_att(fds, nf90_global,  'Conventions', 'CF-1.6 SGRID-0.3'); call nc_check_err(lundia, ierror, "put_att global Conventions", filename)
             ierror = nf90_put_att(fds, nf90_global,  'institution', trim('Deltares')); call nc_check_err(lundia, ierror, "put_att global institution", filename)
             ierror = nf90_put_att(fds, nf90_global,  'references', trim('www.deltares.nl')); call nc_check_err(lundia, ierror, "put_att global references", filename)
             ierror = nf90_put_att(fds, nf90_global,  'source', trim(full_version)); call nc_check_err(lundia, ierror, "put_att global source", filename)
             ierror = nf90_put_att(fds, nf90_global,  'history', &
                    'This file is created on '//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)// &
                    ', '//trim('Delft3D')); call nc_check_err(lundia, ierror, "put_att global history", filename)
          else
             ierror = nf90_open(filename, NF90_WRITE, fds); call nc_check_err(lundia, ierror, "opening file", filename)
          endif
       endif
       if (ierror/=0) goto 9999
       !
       ! time independent data
       !
       if (first) then
          call wrimap(lundia    ,error     ,filename  ,selmap    ,simdat    , &
                    & itdate    ,tzone     ,tunit     ,dt        ,mmax      , &
                    & kmax      ,lmax      ,lstsci    ,ltur      ,nmaxus    , &
                    & noroco    ,norow     ,nostat    ,nsrc      ,ntruv     , &
                    & grdang    ,dpsopt    ,sferic    ,lsed      ,lsedtot   , &
                    & zmodel    ,ch(namsrc),ch(namcon),namsed    , &
                    & i(kcu)    ,i(kcv)    ,i(kcs)    ,i(irocol) , &
                    & r(xcor)   ,r(ycor)   ,r(xz)     ,r(yz)     ,r(alfas)  , &
                    & r(dp)     ,r(thick)  ,r(sig)    ,r(sig)    , &
                    & d(dps)    ,r(dpu)    ,r(dpv)    ,r(gsqs)   ,wrifou    , &
                    & irequest  ,fds       ,iarrc     ,mf        ,ml        , &
                    & nf        ,nl        ,nostatto  ,nostatgl  ,order_sta , &
                    & ntruvto   ,ntruvgl   ,order_tra ,ipartition,gdp       )
          if (error) goto 9999
          !
          if (dredge .and. gdp%gdflwpar%flwoutput%dredge_map) then
             call wrimapdad(lundia    ,error     ,filename  ,irequest  , &
                          & fds       ,iarrc     ,mf        ,ml        , &
                          & nf        ,nl        ,gdp       )
             if (error) goto 9999
          endif
       endif
       !
       ! data per time step
       !
       if (wrifou) then
          ! no data per time step
       elseif (irequest==REQUESTTYPE_DEFINE .or. .not. first) then
          call wrtmap(lundia    ,error     ,filename  ,selmap    ,itmapc    , &
                    & rhow      ,mmax      ,itdate    ,dtsec     , &
                    & kmax      ,nmaxus    ,lstsci    ,ltur      , &
                    & nsrc      ,zmodel    ,i(kcs)    ,i(kfs)    ,i(kfu)    , &
                    & i(kfv)    ,i(kfumin) ,i(kfvmin) ,i(kfumax) ,i(kfvmax) , &
                    & i(kfsmin) ,i(kfsmax) ,i(mnksrc) ,r(s1)     , &
                    & d(dps)    ,r(dzs1)   ,r(thick)  ,r(windcd)  , &
                    & r(velu)   ,r(velv)   ,r(w1)     ,r(wphy)   ,r(r1)     , &
                    & r(rtur1)  ,r(taubpu) ,r(taubpv) ,r(taubsu) ,r(taubsv) , &
                    & r(vicww)  ,r(dicww)  ,r(rich)   ,r(rho)    ,r(p1)     , &
                    & r(vortic) ,r(enstro) ,r(umnldf) ,r(vmnldf) ,r(vicuv)  , &
                    & r(taubmx) ,r(windu)  ,r(windv)  ,velt      ,r(cvalu0) , &
                    & r(cvalv0) ,r(cfurou) ,r(cfvrou) ,rouflo    ,r(patm)   , &
                    & r(z0ucur) ,r(z0vcur) ,r(z0urou) ,r(z0vrou) ,ktemp     , &
                    & r(precip) ,r(evap)   ,irequest  ,fds       ,iarrc     , &
                    & mf        ,ml        ,nf        ,nl        ,gdp       )
          if (error) goto 9999
          !
          if (lsedtot>0 .or. lfbedfrm .or. lfsdu) then
             call wrsedmgrp(lundia    ,error     ,filename  ,itmapc    ,mmax      , &
                          & kmax      ,nmaxus    ,lsed      ,lsedtot   , &
                          & i(kfsmin) ,i(kfsmax) ,irequest  ,fds       ,iarrc     , &
                          & mf        ,ml        ,nf        ,nl        ,gdp       )
             if (error) goto 9999
          endif
          !
          if (lsedtot > 0) then
             if (itmapc==itmapl .or. gdp%gdmorpar%moroutput%cumavg .or. irequest==REQUESTTYPE_DEFINE) then
                call wrsedmavg(lundia    ,error     ,filename  ,itmapc    ,mmax      , &
                             & nmaxus    ,lsed      ,lsedtot   ,irequest  ,fds       , &
                             & iarrc     ,mf        ,ml        ,nf        ,nl        , &
                             & gdp       )
                if (error) goto 9999
             endif
          endif
          !
          if (gdp%gdflwpar%flwoutput%waveqnt .or. roller .or. xbeach) then
             call wrrolm(lundia    ,error     ,filename  ,itmapc    ,nmax      , &
                       & mmax      ,nmaxus    ,r(ewave1) ,r(eroll1) ,r(qxkr)   , &
                       & r(qykr)   ,r(qxkw)   ,r(qykw)   ,r(fxw)    ,r(fyw)    , &
                       & r(wsu)    ,r(wsv)    ,r(guu)    ,r(gvv)    ,r(rbuff)  , &
                       & r(hrms)   ,r(tp)     ,r(teta)   ,r(rlabda) ,r(uorb)   , &
                       & irequest  ,fds       ,iarrc     ,mf        ,ml        , &
                       & nf        ,nl        ,roller    ,xbeach    ,gdp       )
             if (error) goto 9999
          endif
       endif
       !
       if (irequest == REQUESTTYPE_DEFINE) then
          !
          ! upon first request: define all groups, dimensions, and elements on file
          !
          call defnewgrp(fds, ifile, gdp, filename)
          !
          if (filetype == FTYPE_NETCDF) then
             !
             ierror = nf90_enddef(fds); call nc_check_err(lundia, ierror, "enddef", filename)
             if (ierror/=0) goto 9999
             !
          endif
       endif
    enddo
    !
9999 continue
    if (ierror/= 0) error = .true.
    if (inode == master) then
       if (filetype == FTYPE_NETCDF) then
          ierror = nf90_sync(fds); call nc_check_err(lundia, ierror, "sync file", filename)
          ierror = nf90_close(fds); call nc_check_err(lundia, ierror, "closing file", filename)
       elseif (filetype == FTYPE_NEFIS) then
          ierror = clsnef(fds)
          !
          ! wridoc needs to be called AFTER defnewgrp because this routine will
          ! add the map-version not just to the file, but also to the group
          ! administration in memory and calling defnewgrp afterwards would thus
          ! duplicate definition of the map-version group.
          !
          if (first .and. .not.wrifou) then
             ftype = 'map'
             call wridoc(error     ,trifil    ,ftype     ,simdat    ,runtxt    , &
                       & .false.   ,part_nr   ,gdp       )
             !
             if (.not. parll .and. .not. error) then
                call wrplot(filename  ,lundia    ,error     ,mmax      ,nmax      , &
                          & nmaxus    ,i(kcs)    ,i(ibuff)  ,r(xz)     ,r(yz)     , &
                          & sferic    ,gdp       )
             endif
          endif
       endif
       if (ierror/= 0) error = .true.
    endif
    first = .false.
    !
    if (allocated(ipartition)) deallocate(ipartition, stat=istat)
    if (.not.mergemap) then
        inode = bck_inode
        parll = bck_parll
        nproc = bck_nproc
        gdp%gdparall => bck_gdparall
    endif
end subroutine wrm_main
