subroutine inippr(lundia    ,error     ,trifil    ,comfil    , &
                & selhis    ,selmap    ,tscale    ,commrd    , &
                & itlen     ,itcur     ,itimc     , &
                & it01      ,it02      ,sferic    ,grdang    , &
                & rouflo    ,nfltyp    , &
                & runtxt    ,gdp       )
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
!    Function: Writes initial output to output files
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use datagroups, only: getfiletype
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
    integer                          , pointer :: nmax
    integer                          , pointer :: mmax
    integer                          , pointer :: nmaxus
    integer                          , pointer :: kmax
    integer                          , pointer :: lmax
    integer                          , pointer :: lsts
    integer                          , pointer :: lstsc
    integer                          , pointer :: lstsci
    integer                          , pointer :: lsed
    integer                          , pointer :: lsedtot
    integer                          , pointer :: ltur
    integer                          , pointer :: noroco
    integer                          , pointer :: norow
    integer                          , pointer :: nocol
    integer                          , pointer :: nto
    integer                          , pointer :: kc
    integer                          , pointer :: nrob
    integer                          , pointer :: nsrc
    integer                          , pointer :: nostat
    integer                          , pointer :: ntruv
    integer                          , pointer :: ntru
    integer                          , pointer :: nofou
    integer                          , pointer :: ndro
    integer                          , pointer :: itdate
    real(fp)                         , pointer :: dt
    real(fp)                         , pointer :: tunit
    real(fp)                         , pointer :: tzone
    integer    , dimension(:)        , pointer :: ipmap
    integer                          , pointer :: iphisi
    integer                          , pointer :: itmapi
    integer                          , pointer :: ithisi
    integer                          , pointer :: itcomi
    integer                          , pointer :: itdrof
    integer                          , pointer :: itdroi
    integer                          , pointer :: iro
    logical                          , pointer :: wind
    logical                          , pointer :: culvert
    logical                          , pointer :: dredge
    logical                          , pointer :: drogue
    logical                          , pointer :: zmodel
    integer(pntrsize)                , pointer :: alfas
    integer(pntrsize)                , pointer :: c
    integer(pntrsize)                , pointer :: cfurou
    integer(pntrsize)                , pointer :: cfvrou
    integer(pntrsize)                , pointer :: dis
    integer(pntrsize)                , pointer :: dpd
    integer(pntrsize)                , pointer :: dps
    integer(pntrsize)                , pointer :: dpu
    integer(pntrsize)                , pointer :: dpv
    integer(pntrsize)                , pointer :: gro
    integer(pntrsize)                , pointer :: gsqd
    integer(pntrsize)                , pointer :: gsqs
    integer(pntrsize)                , pointer :: guu
    integer(pntrsize)                , pointer :: guv
    integer(pntrsize)                , pointer :: gvu
    integer(pntrsize)                , pointer :: gvv
    integer(pntrsize)                , pointer :: rbuff
    integer(pntrsize)                , pointer :: rob
    integer(pntrsize)                , pointer :: sig
    integer(pntrsize)                , pointer :: thick
    integer(pntrsize)                , pointer :: tp
    integer(pntrsize)                , pointer :: windu
    integer(pntrsize)                , pointer :: windv
    integer(pntrsize)                , pointer :: xcor
    integer(pntrsize)                , pointer :: xydro
    integer(pntrsize)                , pointer :: xyzsrc
    integer(pntrsize)                , pointer :: xz
    integer(pntrsize)                , pointer :: ycor
    integer(pntrsize)                , pointer :: yz
    integer(pntrsize)                , pointer :: ibuff
    integer(pntrsize)                , pointer :: irocol
    integer(pntrsize)                , pointer :: kcs
    integer(pntrsize)                , pointer :: kcu
    integer(pntrsize)                , pointer :: kcv
    integer(pntrsize)                , pointer :: mnbnd
    integer(pntrsize)                , pointer :: mnksrc
    integer(pntrsize)                , pointer :: nob
    integer(pntrsize)                , pointer :: namcon
    integer(pntrsize)                , pointer :: namdro
    integer(pntrsize)                , pointer :: namsrc
    real(fp)                         , pointer :: zbot
    character(20) , dimension(:)     , pointer :: namsed
    character(8)                     , pointer :: dpsopt
!
! Global variables
!
    integer                                    :: it01   !  Description and declaration in esm_alloc_int.f90
    integer                                    :: it02   !  Description and declaration in esm_alloc_int.f90
    integer                                    :: itcur  !!  Current time counter for the com-
                                                         !!  munication file, where starting
                                                         !!  point depend on CYCLIC
    integer                                    :: itimc  !!  Current time step counter for 2D
                                                         !!  system
    integer                                    :: itlen  !  Description and declaration in esm_alloc_int.f90
    integer                                    :: lundia !  Description and declaration in inout.igs
    integer                                    :: nfltyp !  Description and declaration in esm_alloc_int.f90
    logical                                    :: error  !!  Flag=TRUE if an error is encountered
    logical                                    :: commrd
    logical                                    :: sferic !  Description and declaration in tricom.igs
    real(fp)                                   :: grdang !  Description and declaration in tricom.igs
    real(fp)                                   :: tscale !  Description and declaration in esm_alloc_real.f90
    character(*)                               :: comfil !!  Name for communication file
                                                         !!  com-<case><label>
    character(*)                               :: selmap !  Description and declaration in tricom.igs
    character(*)                               :: trifil !!  File name for FLOW NEFIS output
                                                         !!  files (tri"h/m"-"casl""labl".dat/def)
    character(23)                              :: selhis !  Description and declaration in tricom.igs
    character(30) , dimension(10)              :: runtxt !!  Textual description of model input
    character(4)                               :: rouflo !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    logical                                    :: wrifou  ! true: wrimap is going to write the initial part of the fourier nefis file
    character(16)                              :: simdat  ! Simulation date representing the flow condition at this date
    character(20)                              :: rundat  ! Execution date of the simulation
    character(6)                               :: ftype   ! String containing to which output file version group should be written
    integer                                    :: ithisc  ! Dummy variable for wrh_main call (actual value needed in POSTPR)
    integer                                    :: itmapc  ! Dummy variable for wrm_main call (actual value needed in POSTPR)
    integer                                    :: itdroc  ! Dummy variable for wrd_main call (actual value needed in POSTPR)
    real(fp)                                   :: dtsec   ! Dummy variable for wrh_main/wrm_main call (actual value needed in POSTPR)
!
!! executable statements -------------------------------------------------------
!
    nmax        => gdp%d%nmax
    mmax        => gdp%d%mmax
    nmaxus      => gdp%d%nmaxus
    kmax        => gdp%d%kmax
    lmax        => gdp%d%lmax
    lsts        => gdp%d%lsts
    lstsc       => gdp%d%lstsc
    lstsci      => gdp%d%lstsci
    lsed        => gdp%d%lsed
    lsedtot     => gdp%d%lsedtot
    ltur        => gdp%d%ltur
    noroco      => gdp%d%noroco
    norow       => gdp%d%norow
    nocol       => gdp%d%nocol
    nto         => gdp%d%nto
    kc          => gdp%d%kc
    nrob        => gdp%d%nrob
    nsrc        => gdp%d%nsrc
    nostat      => gdp%d%nostat
    ntruv       => gdp%d%ntruv
    ntru        => gdp%d%ntru
    nofou       => gdp%d%nofou
    ndro        => gdp%d%ndro
    itdate      => gdp%gdexttim%itdate
    dt          => gdp%gdexttim%dt
    tunit       => gdp%gdexttim%tunit
    tzone       => gdp%gdexttim%tzone
    ipmap       => gdp%gdinttim%ipmap
    iphisi      => gdp%gdinttim%iphisi
    itmapi      => gdp%gdinttim%itmapi
    ithisi      => gdp%gdinttim%ithisi
    itcomi      => gdp%gdinttim%itcomi
    itdrof      => gdp%gdinttim%itdrof
    itdroi      => gdp%gdinttim%itdroi
    iro         => gdp%gdphysco%iro
    wind        => gdp%gdprocs%wind
    culvert     => gdp%gdprocs%culvert
    dredge      => gdp%gdprocs%dredge
    drogue      => gdp%gdprocs%drogue
    zmodel      => gdp%gdprocs%zmodel
    alfas       => gdp%gdr_i_ch%alfas
    c           => gdp%gdr_i_ch%c
    cfurou      => gdp%gdr_i_ch%cfurou
    cfvrou      => gdp%gdr_i_ch%cfvrou
    dis         => gdp%gdr_i_ch%dis
    dpd         => gdp%gdr_i_ch%dpd
    dps         => gdp%gdr_i_ch%dps
    dpu         => gdp%gdr_i_ch%dpu
    dpv         => gdp%gdr_i_ch%dpv
    gro         => gdp%gdr_i_ch%gro
    gsqd        => gdp%gdr_i_ch%gsqd
    gsqs        => gdp%gdr_i_ch%gsqs
    guu         => gdp%gdr_i_ch%guu
    guv         => gdp%gdr_i_ch%guv
    gvu         => gdp%gdr_i_ch%gvu
    gvv         => gdp%gdr_i_ch%gvv
    rbuff       => gdp%gdr_i_ch%rbuff
    rob         => gdp%gdr_i_ch%rob
    sig         => gdp%gdr_i_ch%sig
    thick       => gdp%gdr_i_ch%thick
    tp          => gdp%gdr_i_ch%tp
    windu       => gdp%gdr_i_ch%windu
    windv       => gdp%gdr_i_ch%windv
    xcor        => gdp%gdr_i_ch%xcor
    xydro       => gdp%gdr_i_ch%xydro
    xyzsrc      => gdp%gdr_i_ch%xyzsrc
    xz          => gdp%gdr_i_ch%xz
    ycor        => gdp%gdr_i_ch%ycor
    yz          => gdp%gdr_i_ch%yz
    ibuff       => gdp%gdr_i_ch%ibuff
    irocol      => gdp%gdr_i_ch%irocol
    kcs         => gdp%gdr_i_ch%kcs
    kcu         => gdp%gdr_i_ch%kcu
    kcv         => gdp%gdr_i_ch%kcv
    mnbnd       => gdp%gdr_i_ch%mnbnd
    mnksrc      => gdp%gdr_i_ch%mnksrc
    nob         => gdp%gdr_i_ch%nob
    namcon      => gdp%gdr_i_ch%namcon
    namdro      => gdp%gdr_i_ch%namdro
    namsrc      => gdp%gdr_i_ch%namsrc
    zbot        => gdp%gdzmodel%zbot
    namsed      => gdp%gdsedpar%namsed
    dpsopt      => gdp%gdnumeco%dpsopt
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
    ! Write the time independent data to the communication file
    !
    if (itcomi>0) then
       call wrcomi(comfil    ,lundia    ,error     ,zmodel    ,mmax      , &
                 & nmax      ,kmax      ,nmaxus    ,nsrc      ,norow     , &
                 & nocol     ,noroco    ,nto       ,nrob      ,zbot      , &
                 & dt        ,nfltyp    ,tscale                          , &
                 & itlen     ,it01      ,it02      ,tzone                , &
                 & rouflo    ,r(xcor)   ,r(ycor)   ,r(guu)               , &
                 & r(gvv)    ,r(guv)    ,r(gvu)    ,r(gsqs)   ,r(gsqd)   , &
                 & r(alfas)  ,r(thick)  ,r(sig)    ,ch(namsrc),i(mnksrc) , &
                 & r(xyzsrc) ,i(irocol) ,i(mnbnd)  ,i(nob)    ,i(kcu)    , &
                 & i(kcv)    ,i(kcs)    ,r(dpd)    ,d(dps)    ,r(cfurou) , &
                 & r(cfvrou) ,i(ibuff)  ,r(rbuff)  ,r(rbuff)  ,sferic    , &
                 & gdp       )
       if (error) goto 9999
       call wrplot(comfil    ,lundia    ,error     ,mmax      ,nmax      , &
                 & nmaxus    ,i(kcs)    ,i(ibuff)  ,r(xz)     ,r(yz)     , &
                 & sferic    ,gdp       )
       if (error) goto 9999
       ftype = 'com'
       call wridoc(error     ,comfil    ,ftype     ,simdat    ,runtxt    , &
                 & commrd    ,''        ,gdp       )
       if (error) goto 9999
       !
       ! Write group WIND
       ! Wind must (also) be written in inippr, because it may be used by
       ! Delft3D-WAVE, before postpr is executed.
       !
       if (wind) then
          call wrcomwind(error     ,comfil    ,itcur     ,itimc     , &
                       & mmax      ,nmaxus    , &
                       & r(windu)  ,r(windv)  ,gdp       )
          if (error) goto 9999
       endif
    endif
    !
    ! FLOW output files
    !
    !
    ! Definition, declaration and writing of group 2 to the HIS DAT
    ! and HIS DEF files following the NEFIS description
    !
    if (iphisi > 0 .or. ipmap(1) > -1) then
       !
       ! Writing output to ascii file tri-prt.<runid> as is requested through
       ! the use of keyword PRHIS in the MD-file
       !
       call prihis(gdp)
       ftype = 'ascii'
       call wridoc(error     ,trifil    ,ftype     ,simdat    ,runtxt    , &
                 & commrd    ,''        ,gdp       )
       if (error) goto 9999
    endif
    !
    ithisc = 0
    itmapc = 0
    itdroc = 0
    !
    if (ithisi > 0) then
       call wrh_main(lundia    ,error     ,selhis    ,grdang    ,dtsec     , &
                   & ithisc    ,runtxt    ,trifil    ,gdp       )
       if (error) goto 9999
    endif
    if (itmapi > 0) then
       wrifou = .false.
       call wrm_main(lundia    ,error     ,selmap    ,grdang    ,dtsec     , &
                   & itmapc    ,runtxt    ,trifil    ,wrifou    ,gdp       )
       if (error) goto 9999
    endif
    if (drogue) then
       call wrd_main(lundia    ,error     ,ndro      ,itdroc    ,runtxt    , &
                   & trifil    ,dtsec     ,gdp       )
       if (error) goto 9999
    endif
    if (nofou>0 .and. (getfiletype(gdp,FILOUT_FOU) == FTYPE_NETCDF)) then
       wrifou = .true.
       call wrm_main(lundia    ,error     ,selmap    ,grdang    ,dtsec     , &
                   & itmapc    ,runtxt    ,trifil    ,wrifou    ,gdp       )
       if (error) goto 9999
    endif
 9999 continue
end subroutine inippr
