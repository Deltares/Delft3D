subroutine wrh_main(lundia    ,error     ,selhis    ,grdang    ,dtsec     , &
                  & ithisc    ,runtxt    ,trifil    ,gdp       )
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
    use dfparall, only: inode, master, parll
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
    integer(pntrsize)                    , pointer :: wrka1 ! zhs
    integer(pntrsize)                    , pointer :: wrka2 ! ztp
    integer(pntrsize)                    , pointer :: wrka3 ! zdir
    integer(pntrsize)                    , pointer :: wrka4 ! zrlabd
    integer(pntrsize)                    , pointer :: wrka5 ! zuwb
    integer                              , pointer :: kmax
    integer                              , pointer :: lmax
    integer                              , pointer :: lstsci
    integer                              , pointer :: lsal
    integer                              , pointer :: lsed
    integer                              , pointer :: lsedtot
    integer                              , pointer :: ltem
    integer                              , pointer :: ltur
    integer                              , pointer :: mmax
    integer                              , pointer :: nmax
    integer                              , pointer :: nsluv
    integer                              , pointer :: nsrc
    integer                              , pointer :: nostat
    integer                              , pointer :: ntruv
    integer                              , pointer :: itstrt
    integer                              , pointer :: ithisi
    logical                              , pointer :: lfsdu
    logical                              , pointer :: wind
    logical                              , pointer :: culvert
    logical                              , pointer :: dredge
    logical                              , pointer :: wave
    logical                              , pointer :: zmodel
    integer(pntrsize)                    , pointer :: kcs
    integer(pntrsize)                    , pointer :: alfas
    integer(pntrsize)                    , pointer :: atr
    integer(pntrsize)                    , pointer :: cbuv
    integer(pntrsize)                    , pointer :: ctr
    integer(pntrsize)                    , pointer :: disch
    integer(pntrsize)                    , pointer :: dps
    integer(pntrsize)                    , pointer :: dtr
    integer(pntrsize)                    , pointer :: fltr
    integer(pntrsize)                    , pointer :: gro
    integer(pntrsize)                    , pointer :: rint
    integer(pntrsize)                    , pointer :: s1
    integer(pntrsize)                    , pointer :: sbtr
    integer(pntrsize)                    , pointer :: sbtrc
    integer(pntrsize)                    , pointer :: sig
    integer(pntrsize)                    , pointer :: sstr
    integer(pntrsize)                    , pointer :: sstrc
    integer(pntrsize)                    , pointer :: thick
    integer(pntrsize)                    , pointer :: voldis
    integer(pntrsize)                    , pointer :: xcor
    integer(pntrsize)                    , pointer :: xz
    integer(pntrsize)                    , pointer :: ycor
    integer(pntrsize)                    , pointer :: yz
    integer(pntrsize)                    , pointer :: zbdsed
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
    integer(pntrsize)                    , pointer :: hydprs
    integer(pntrsize)                    , pointer :: mnksrc
    integer(pntrsize)                    , pointer :: nambar
    integer(pntrsize)                    , pointer :: namcon
    integer(pntrsize)                    , pointer :: namsrc
    integer                              , pointer :: itdate
    real(fp)                             , pointer :: dt
    real(fp)                             , pointer :: tunit
    real(fp)                             , pointer :: tzone
    logical                              , pointer :: sferic
    character(20) , dimension(:)         , pointer :: namsed
    real(fp)                             , pointer :: zbot
    integer       , dimension(:)         , pointer :: order_sta
    integer       , dimension(:)         , pointer :: order_tra
    !
    type (datagroup)                     , pointer :: group
    logical                              , pointer :: first
!
! Global variables
!
    integer                                                                           :: ithisc !!  Current time counter for the history data file
    integer                                                                           :: lundia !  Description and declaration in inout.igs
    logical                                                                           :: error  !!  Flag=TRUE if an error is encountered
    real(fp)                                                                          :: dtsec  !!  Integration time step [in seconds]
    real(fp)                                                                          :: grdang !  Description and declaration in tricom.igs
    character(23)                                                                     :: selhis !  Description and declaration in tricom.igs
    character(30) , dimension(10)                                                     :: runtxt !!  Textual description of model input
    character(*)                                                        , intent(in)  :: trifil !  File name for FLOW NEFIS output
                                                                                                !  files (tri"h/m"-"casl""labl".dat/def)
!
! Local variables
!
    character(10)                                     :: velt          ! Velocity type 'Eulerian' or 'GLM'
    !
    integer                                           :: nostatgl      ! global number of stations (i.e. original number excluding duplicate stations located in the halo regions)
    integer                                           :: nostatto      ! total number of stations (including "duplicate" stations located in halo regions)
    integer                                           :: ntruvgl       ! global number of tracks (i.e. original number excluding duplicate stations located in the halo regions)
    integer                                           :: ntruvto       ! total number of tracks (including "duplicate" stations located in halo regions)
    !
    integer                                           :: irequest
    integer                                           :: ierror
    integer                                           :: filetype
    integer                                           :: fds
    integer                                , external :: open_datdef
    integer                                , external :: clsnef
    character(256)                                    :: filename
    !
    character(256)                                    :: full_version
    character(8)                                      :: cdate
    character(10)                                     :: ctime
    character(5)                                      :: czone
    character(1024)                                   :: error_string
    !
    character(16) :: simdat  ! Simulation date representing the flow condition at this date
    character(20) :: rundat  ! Execution date of the simulation
    character(6)  :: ftype   ! String containing to which output file version group should be written

!
!! executable statements -------------------------------------------------------
!
    wrka1               => gdp%gdaddress%wrka1
    wrka2               => gdp%gdaddress%wrka2
    wrka3               => gdp%gdaddress%wrka3
    wrka4               => gdp%gdaddress%wrka4
    wrka5               => gdp%gdaddress%wrka5
    kmax                => gdp%d%kmax
    lmax                => gdp%d%lmax
    lstsci              => gdp%d%lstsci
    lsal                => gdp%d%lsal
    lsed                => gdp%d%lsed
    lsedtot             => gdp%d%lsedtot
    ltem                => gdp%d%ltem
    ltur                => gdp%d%ltur
    mmax                => gdp%d%mmax
    nmax                => gdp%d%nmax
    nsluv               => gdp%d%nsluv
    nsrc                => gdp%d%nsrc
    nostat              => gdp%d%nostat
    ntruv               => gdp%d%ntruv
    itstrt              => gdp%gdinttim%itstrt
    ithisi              => gdp%gdinttim%ithisi
    wind                => gdp%gdprocs%wind
    culvert             => gdp%gdprocs%culvert
    dredge              => gdp%gdprocs%dredge
    wave                => gdp%gdprocs%wave
    zmodel              => gdp%gdprocs%zmodel
    kcs                 => gdp%gdr_i_ch%kcs
    alfas               => gdp%gdr_i_ch%alfas
    atr                 => gdp%gdr_i_ch%atr
    cbuv                => gdp%gdr_i_ch%cbuv
    ctr                 => gdp%gdr_i_ch%ctr
    disch               => gdp%gdr_i_ch%disch
    dps                 => gdp%gdr_i_ch%dps
    dtr                 => gdp%gdr_i_ch%dtr
    fltr                => gdp%gdr_i_ch%fltr
    gro                 => gdp%gdr_i_ch%gro
    rint                => gdp%gdr_i_ch%rint
    s1                  => gdp%gdr_i_ch%s1
    sbtr                => gdp%gdr_i_ch%sbtr
    sbtrc               => gdp%gdr_i_ch%sbtrc
    sig                 => gdp%gdr_i_ch%sig
    sstr                => gdp%gdr_i_ch%sstr
    sstrc               => gdp%gdr_i_ch%sstrc
    thick               => gdp%gdr_i_ch%thick
    voldis              => gdp%gdr_i_ch%voldis
    xcor                => gdp%gdr_i_ch%xcor
    xz                  => gdp%gdr_i_ch%xz
    ycor                => gdp%gdr_i_ch%ycor
    yz                  => gdp%gdr_i_ch%yz
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
    hydprs              => gdp%gdr_i_ch%hydprs
    mnksrc              => gdp%gdr_i_ch%mnksrc
    nambar              => gdp%gdr_i_ch%nambar
    namcon              => gdp%gdr_i_ch%namcon
    namsrc              => gdp%gdr_i_ch%namsrc
    itdate              => gdp%gdexttim%itdate
    dt                  => gdp%gdexttim%dt
    tunit               => gdp%gdexttim%tunit
    tzone               => gdp%gdexttim%tzone
    sferic              => gdp%gdtricom%sferic
    namsed              => gdp%gdsedpar%namsed
    zbot                => gdp%gdzmodel%zbot
    order_sta           => gdp%gdparall%order_sta
    order_tra           => gdp%gdparall%order_tra
    lfsdu               => gdp%gdprocs%lfsdu
    !
    call getdatagroup(gdp, FILOUT_HIS, 'his-const', group)
    first               => group%first
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
    filename = trifil(1:3) // 'h' // trifil(5:)
    filetype = getfiletype(gdp, FILOUT_HIS)
    if (filetype == FTYPE_NETCDF) filename = trim(filename)//'.nc'
    !
    if (gdp%gdflwpar%flwoutput%veuler) then
       velt = 'Eulerian'
    else
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
       ! When order_tra points to tra_orgline, both partition-related and re-ordering-related stuff is taken care of
       !
       order_tra => gdp%gdstations%tra_orgline
    else
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
             ierror = nf90_put_att(fds, nf90_global,  'Conventions', 'CF-1.6'); call nc_check_err(lundia, ierror, "put_att global Conventions", filename)
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
          call wrihis(lundia    ,error     ,filename  ,selhis    ,simdat    , &
                    & itdate    ,tzone     ,tunit     ,dt        ,nostat    , &
                    & ntruv     ,nmax      ,mmax      ,kmax      ,lmax      , &
                    & lstsci    ,ltur      ,grdang    ,sferic    ,lsed      , &
                    & lsedtot   ,zmodel    ,ch(namcon),namsed    ,r(xz)     , &
                    & r(yz)     ,r(alfas)  ,d(dps)    ,r(thick)  ,r(sig)    , &
                    & r(sig)    ,irequest  ,fds       ,nostatto  ,nostatgl  , &
                    & order_sta ,ntruvto   ,ntruvgl   ,order_tra ,r(xcor)   , &
                    & r(ycor)   ,i(kcs)    ,nsluv     ,ch(nambar),gdp       )
          if (error) goto 9999
          !
          if (nsrc>0 .and. gdp%gdflwpar%flwoutput%hisdis) then
             call wrihisdis(lundia    ,error     ,filename  ,itdate    ,tunit     , &
                          & dt        ,nsrc      ,ch(namsrc),irequest  ,fds       , &
                          & gdp       )
             if (error) goto 9999
          endif
          !
          call wrihisbal(filename  ,lundia    ,error     ,irequest  ,fds       , &
                       & gdp       )
          !
          if (dredge) then
             call wrihisdad(lundia    ,error     ,filename  , &
                          & itdate    ,tunit     ,dt        ,lsedtot   , &
                          & irequest  ,fds       ,gdp       )
             if (error) goto 9999
          endif
       endif
       !
       ! data per time step
       !
       if (irequest==REQUESTTYPE_DEFINE .or. .not. first) then
          call wrthis(lundia    ,error     ,filename  ,selhis    ,ithisc    , &
                    & itstrt    ,ithisi    ,zmodel    ,nostat    ,ntruv     , &
                    & kmax      ,lmax      ,lstsci    ,lsal      ,ltem      , &
                    & ltur      ,i(zkfs)   ,r(zwl)    ,r(zcuru)  ,r(zcurv)  , &
                    & r(zcurw)  ,r(zqxk)   ,r(zqyk)   ,r(ztauks) ,r(ztauet) , &
                    & r(zvicww) ,r(zdicww) ,r(zrich)  ,r(zrho)   ,r(gro)    , &
                    & r(ztur)   ,r(zvort)  ,r(zenst)  ,r(hydprs) ,r(fltr)   , &
                    & r(ctr)    ,r(atr)    ,r(dtr)    ,velt      ,r(zdps)   , &
                    & r(zwndsp) ,r(zwnddr) ,r(zairp)  ,wind      ,sferic    , &
                    & r(zprecp) ,r(zevap)  ,itdate    ,dtsec     ,irequest  , &
                    & fds       ,nostatto  ,nostatgl  ,order_sta ,ntruvto   , &
                    & ntruvgl   ,order_tra ,nsluv     ,r(cbuv)   ,r(zwndcd) ,gdp       )
          if (error) goto 9999
          !
          if (nsrc>0 .and. gdp%gdflwpar%flwoutput%hisdis) then
             call wrthisdis(lundia    ,error     ,filename  ,ithisc    , &
                          & zmodel    ,kmax      ,lstsci    ,nsrc      , &
                          & i(mnksrc) ,r(disch)  ,d(dps)    ,r(rint)   , &
                          & r(s1)     ,r(sig)    ,r(sig)    ,r(voldis) , &
                          & r(xcor)   ,r(ycor)   ,sferic    ,irequest  , &
                          & fds       ,gdp      )
             if (error) goto 9999
          endif          
          !
          call wrthisbal(ithisc    ,filename  ,lundia    ,error     ,irequest  , &
                       & fds       ,gdp       )
          !
          if (lsedtot>0 .or. lfsdu) then
             call wrsedh(lundia    ,error     ,filename  ,ithisc    ,ntruv     , &
                       & nostat    ,kmax      ,lsed      ,lsedtot   ,zmodel    , &
                       & r(zws)    ,r(zrsdeq) ,r(zbdsed) ,r(zdpsed) ,r(zdps)   , &
                       & r(zsbu)   ,r(zsbv)   ,r(zssu)   ,r(zssv)   ,r(sbtr)   , &
                       & r(sstr)   ,r(sbtrc)  ,r(sstrc)  ,r(zrca)   ,r(zsourse), &
                       & r(zsinkse),r(zfrac)  ,r(zmudfrac),r(zsandfrac),r(zfixfac), &
                       & r(ztaub)  ,r(zhidexp),r(zseddif), &
                       & irequest  ,fds       ,nostatto  ,nostatgl  ,order_sta , &
                       & ntruvto   ,ntruvgl   ,order_tra ,gdp       )
             if (error) goto 9999
          endif
          !
          if (dredge) then
             call wrthisdad(lundia    ,error     ,filename  ,ithisc    , &
                          & lsedtot   ,irequest  ,fds       ,gdp       )
             if (error) goto 9999
          endif
          !
          if (wave) then
             call wrwavh(lundia    ,error     ,filename  ,ithisc    , &
                       & nostat    ,r(wrka1)  ,r(wrka2)  , &
                       & r(wrka3)  ,r(wrka4)  ,r(wrka5)  ,irequest  , &
                       & fds       ,nostatto  ,nostatgl  ,order_sta , &
                       & gdp       )
             if (error) goto 9999
          endif
       endif
       !
       if (irequest == REQUESTTYPE_DEFINE) then
          !
          ! upon first request: define all groups, dimensions, and elements on file
          !
          call defnewgrp(fds, FILOUT_HIS, gdp, filename)
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
          ! add the his-version not just to the file, but also to the group
          ! administration in memory and calling defnewgrp afterwards would thus
          ! duplicate definition of the his-version group.
          !
          if (first) then
             ftype = 'his'
             call wridoc(error     ,trifil    ,ftype     ,simdat    ,runtxt    , &
                       & .false.   ,''        ,gdp       )
          endif
       endif
       if (ierror/= 0) error = .true.
    endif
    first = .false.
end subroutine wrh_main
