subroutine dredge(nmmax  ,lsedtot,nst    , &
                & cdryb  ,dps    ,dbodsd ,kfsed  , &
                & s1     ,timhr  ,morhr  ,agsqs  , gsqs ,gdp    )
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
!  $Id: dredge.f90 6033 2016-04-19 08:23:40Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/dredge.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use flow_tables
    use bedcomposition_module
    use m_alloc
    use dfparall, only: parll, inode, nproc
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    type (handletype)              , pointer :: tseriesfile
    real(fp)      , dimension(:,:) , pointer :: link_percentage
    real(fp)      , dimension(:,:) , pointer :: link_sum
    real(fp)      , dimension(:)   , pointer :: dzdred
    real(fp)      , dimension(:)   , pointer :: refplane
    real(fp)      , dimension(:,:) , pointer :: voldred
    real(fp)      , dimension(:)   , pointer :: totvoldred
    real(fp)      , dimension(:)   , pointer :: globalareadred
    real(fp)      , dimension(:,:) , pointer :: voldump
    real(fp)      , dimension(:,:) , pointer :: percsupl
    real(fp)      , dimension(:)   , pointer :: totvoldump
    real(fp)      , dimension(:)   , pointer :: localareadump
    real(fp)      , dimension(:)   , pointer :: globalareadump
    real(fp)      , dimension(:)   , pointer :: globaldumpcap
    real(fp)      , dimension(:)   , pointer :: duneheight
    integer                        , pointer :: dredge_domainnr
    integer                        , pointer :: dredge_ndomains
    integer                        , pointer :: nadred
    integer                        , pointer :: nadump
    integer                        , pointer :: nasupl
    integer                        , pointer :: nalink
    integer                        , pointer :: ntimaccum
    integer       , dimension(:,:) , pointer :: link_def
    integer       , dimension(:)   , pointer :: ndredged
    integer       , dimension(:)   , pointer :: nploughed
    logical                        , pointer :: tsmortime
    logical                        , pointer :: firstdredge
    type (dredtype), dimension(:)  , pointer :: dredge_prop
    type (dumptype), dimension(:)  , pointer :: dump_prop
    real(fp)                       , pointer :: morfac
    real(fp)                       , pointer :: thresh
    real(fp)                       , pointer :: bed
    real(fp)                       , pointer :: tmor
    integer                        , pointer :: itmor
    logical                        , pointer :: cmpupd
    real(fp)                       , pointer :: hdt
    integer                        , pointer :: numdomains
    integer                        , pointer :: lundia
    integer                        , pointer :: julday
    integer, pointer :: cutcell
!
! Global variables
!
    integer                                              , intent(in)  :: lsedtot
    integer                                              , intent(in)  :: nmmax   !  Description and declaration in dimens.igs
    integer                                              , intent(in)  :: nst
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kfsed
    real(fp)                                             , intent(in)  :: morhr
    real(fp)                                             , intent(in)  :: timhr
    real(fp)  , dimension(lsedtot)                       , intent(in)  :: cdryb   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(lsedtot, gdp%d%nmlb:gdp%d%nmub)              :: dbodsd  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: s1      !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                       :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                         :: agsqs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                         :: gsqs

!
! Local variables
!
    integer                         :: i
    integer                         :: i2
    integer                         :: ia
    integer                         :: ib
    integer                         :: ib2
    integer                         :: id
    integer                         :: il
    integer                         :: imax
    integer                         :: imaxdunes
    integer                         :: imin
    integer                         :: imindunes
    integer                         :: inm
    integer                         :: irock
    integer                         :: istat
    integer                         :: j
    integer                         :: jnm
    integer                         :: lsed
    integer                         :: nm
    integer                         :: np
    integer                         :: in_ndomains
    integer                         :: globalnpnt
    integer                         :: localoffset
    integer ,dimension(4)           :: paract
    real(fp)                        :: areatim
    real(fp)                        :: availvolume ! volume available for dredging
    real(fp)                        :: avg_alphadune
    real(fp)                        :: avg_depth
    real(fp)                        :: avg_trigdepth
    real(fp)                        :: clr
    real(fp)                        :: ddp
    real(fp)                        :: div2h
    real(fp)                        :: dmax
    real(fp)                        :: dpadd
    real(fp)                        :: dredge_area
    real(fp)                        :: dump_area
    real(fp)                        :: dz
    real(fp)                        :: dzdump
    real(fp)                        :: dzl        ! depth change due to one sediment fraction
    real(fp)                        :: extravolume
    real(fp)                        :: factor
    real(fp)                        :: fracdumped
    real(fp)                        :: fracoutlet
    real(fp)                        :: lin_dz
    real(fp)                        :: ltimhr
    real(fp)                        :: maxdumpvol ! (maximum) volume to be dumped in current time step
    real(fp)                        :: maxvol     ! maximum volume to be dredged in current time step
    real(fp)                        :: plough_fac ! fraction of dune height that remains after ploughing
    real(fp)                        :: qua_dz
    real(fp)                        :: requiredvolume
    real(fp)                        :: voltim     ! local volume variable, various meanings
    real(fp), dimension(1)          :: values
    real(fp)                        :: voldredged
    real(fp)                        :: voldumped
    real(fp)                        :: voltot
    real(fp)                        :: z_dredge
    real(fp)                        :: z_dump
    real(fp)                        :: zmin
    real(fp)                        :: zmax
    real(fp), dimension(:), pointer :: numpoints
    real(fp), dimension(:), pointer :: dz_dredge
    real(fp), dimension(:), pointer :: area
    real(fp), dimension(:), pointer :: hdune
    real(fp), dimension(:), pointer :: reflevel
    real(fp), dimension(:), pointer :: dunetoplevel
    real(fp), dimension(:), pointer :: triggerlevel
    real(fp), dimension(:), pointer :: bedlevel
    real(fp), dimension(:), pointer :: dz_dump
    real(fp), dimension(:), pointer :: troughlevel
    real(fp), dimension(:), pointer :: sedimentdepth
    real(fp), dimension(:), pointer :: dz_dummy
    logical                         :: dredged
    logical                         :: error
    logical                         :: local_cap
    logical                         :: ploughed
    logical , dimension(:), pointer :: triggered
    character(80)                   :: msgstr
    type(dredtype),         pointer :: pdredge
    type(dumptype),         pointer :: pdump
!
!! executable statements -------------------------------------------------------
!
    cutcell => gdp%gdimbound%cutcell
    duneheight          => gdp%gdbedformpar%duneheight
    tseriesfile         => gdp%gddredge%tseriesfile
    link_percentage     => gdp%gddredge%link_percentage
    link_sum            => gdp%gddredge%link_sum
    dzdred              => gdp%gddredge%dzdred
    refplane            => gdp%gddredge%refplane
    voldred             => gdp%gddredge%voldred
    totvoldred          => gdp%gddredge%totvoldred
    globalareadred      => gdp%gddredge%globalareadred
    voldump             => gdp%gddredge%voldump
    percsupl            => gdp%gddredge%percsupl
    totvoldump          => gdp%gddredge%totvoldump
    localareadump       => gdp%gddredge%localareadump
    globalareadump      => gdp%gddredge%globalareadump
    globaldumpcap       => gdp%gddredge%globaldumpcap
    dredge_domainnr     => gdp%gddredge%dredge_domainnr
    dredge_ndomains     => gdp%gddredge%dredge_ndomains
    nadred              => gdp%gddredge%nadred
    nadump              => gdp%gddredge%nadump
    nasupl              => gdp%gddredge%nasupl
    nalink              => gdp%gddredge%nalink
    ntimaccum           => gdp%gddredge%ntimaccum
    link_def            => gdp%gddredge%link_def
    tsmortime           => gdp%gddredge%tsmortime
    firstdredge         => gdp%gddredge%firstdredge
    dredge_prop         => gdp%gddredge%dredge_prop
    dump_prop           => gdp%gddredge%dump_prop
    ndredged            => gdp%gddredge%ndredged
    nploughed           => gdp%gddredge%nploughed
    morfac              => gdp%gdmorpar%morfac
    thresh              => gdp%gdmorpar%thresh
    bed                 => gdp%gdmorpar%bed
    tmor                => gdp%gdmorpar%tmor
    itmor               => gdp%gdmorpar%itmor
    cmpupd              => gdp%gdmorpar%cmpupd
    hdt                 => gdp%gdnumeco%hdt
    numdomains          => gdp%gdprognm%numdomains
    lundia              => gdp%gdinout%lundia
    julday              => gdp%gdinttim%julday
    !
    if (firstdredge) then
       globalareadump = localareadump
       if (numdomains > 1 .or. parll) then
          !
          ! Start communication with other domains
          ! Determine number of domains that use dredging
          ! Determine "rank" of current domain (use 1-based number instead of
          ! the 0-based number returned by C routine)
          !
          if (parll) then
              dredge_domainnr = inode
              dredge_ndomains = nproc
          else
              call start_dd_dredgecommunication (dredge_domainnr, dredge_ndomains)
              dredge_domainnr = dredge_domainnr+1
          endif
          !
          ! For all dredge and dump areas count the global number of points
          ! Due to the way dredgecommunicate is implemented we need to
          ! communicate via floating point array.
          !
          allocate(numpoints(dredge_ndomains), stat = istat)
          !
          ! For each dredge area count the global number of points
          !
          do ia = 1, nadred+nasupl
             pdredge => dredge_prop(ia)
             !
             numpoints = 0.0_fp
             numpoints(dredge_domainnr) = real(pdredge%npnt,fp)
             !
             call dredgecommunicate(numpoints, dredge_ndomains, error, msgstr)
             if (error) goto 999
             !
             in_ndomains = 0
             globalnpnt = 0
             localoffset = 0
             do id = 1,  dredge_ndomains
                np = nint(numpoints(id))
                if (np>0) then
                   in_ndomains = in_ndomains + 1
                   globalnpnt = globalnpnt + np
                   if (id<dredge_domainnr) localoffset = localoffset + np
                endif
             enddo
             if (in_ndomains <= 1) then
                pdredge%in1domain = .true.
             else
                pdredge%npnt = globalnpnt
                !
                ! Reallocate and shift
                !
                istat = 0
                call reallocP(pdredge%area         ,globalnpnt,fill=0.0_fp,shift=localoffset,stat=istat)
                call dredgecommunicate(pdredge%area, pdredge%npnt, error, msgstr)
                if (error) goto 999
                !
                call reallocP(pdredge%hdune        ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdredge%dz_dredge    ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdredge%reflevel     ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdredge%dunetoplevel ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdredge%triggerlevel ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdredge%bedlevel     ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdredge%troughlevel  ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdredge%sedimentdepth,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdredge%sortvar      ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdredge%inm          ,globalnpnt       ,shift=localoffset,stat=istat)
                ! nm(i)=0 for points outside this domain is used in this subroutine
                call reallocP(pdredge%nm           ,globalnpnt,fill=0,shift=localoffset,stat=istat)
                call reallocP(pdredge%triggered    ,globalnpnt       ,shift=localoffset,stat=istat)
                !
                if (istat/=0) then
                   call prterr(lundia, 'U021', 'Dredge: memory realloc error')
                   call d3stop(1, gdp)
                endif
                !
                globalareadred(ia) = 0.0_fp
                do i = 1,globalnpnt
                   pdredge%inm(i) = i
                   globalareadred(ia) = globalareadred(ia) + pdredge%area(i)
                enddo
             endif
          enddo
          !
          ! For each dump area count the global number of points
          !
          do ib = 1, nadump
             pdump => dump_prop(ib)
             !
             numpoints = 0.0_fp
             numpoints(dredge_domainnr) = real(pdump%npnt,fp)
             !
             call dredgecommunicate(numpoints, dredge_ndomains, error, msgstr)
             if (error) goto 999
             !
             in_ndomains = 0
             globalnpnt = 0
             localoffset = 0
             do id = 1,  dredge_ndomains
                np = nint(numpoints(id))
                if (np>0) then
                   in_ndomains = in_ndomains + 1
                   globalnpnt = globalnpnt + np
                   if (id<dredge_domainnr) localoffset = localoffset + np
                endif
             enddo
             if (in_ndomains <= 1) then
                pdump%in1domain = .true.
             else
                pdump%npnt = globalnpnt
                !
                ! Reallocate and shift
                !
                istat = 0
                call reallocP(pdump%area    ,globalnpnt,fill=0.0_fp,shift=localoffset,stat=istat)
                call dredgecommunicate(pdump%area, pdump%npnt, error, msgstr)
                if (error) goto 999
                !
                call reallocP(pdump%hdune   ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdump%reflevel,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdump%bedlevel,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdump%dz_dump ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdump%sortvar ,globalnpnt       ,shift=localoffset,stat=istat)
                call reallocP(pdump%inm     ,globalnpnt       ,shift=localoffset,stat=istat)
                ! nm(i)=0 for points outside this domain is used in this subroutine
                call reallocP(pdump%nm      ,globalnpnt,fill=0,shift=localoffset,stat=istat)
                !
                if (istat/=0) then
                   call prterr(lundia, 'U021', 'Dredge: memory realloc error')
                   call d3stop(1, gdp)
                endif
                !
                do i = 1,globalnpnt
                   pdump%inm(i) = i
                enddo
             endif
          enddo
          !
          deallocate(numpoints, stat = istat)
          !
          ! Communicate dump areas with other domains
          !
          call dredgecommunicate(globalareadump, nadump, error, msgstr)
          if (error) goto 999
       else
          !
          ! Only one domain, so no exchange needed for any dredge or dump area
          !
          dredge_domainnr = 1
          do ia = 1, nadred+nasupl
             dredge_prop(ia)%in1domain = .true.
          enddo
          do ib = 1, nadump
             dump_prop(ib)%in1domain = .true.
          enddo
       endif
       !
       firstdredge = .false.
    endif
    !
    ntimaccum = ntimaccum+1
    !
    ! DREDGING areas include SANDMINING areas.
    !
    ! Verify for each dredge and nourishment area whether dredging
    ! respectively nourishment should occur at the current time step.
    !
    if (tsmortime) then
       ltimhr = morhr
    else
       ltimhr = timhr
    endif
    do ia = 1,nadred+nasupl
       pdredge => dredge_prop(ia)
       !
       ! The default setting for dredging/nourishment is false
       ! unless there is no interval at all, then it is true.
       !
       if (pdredge%paractive(1) == -999) then
          pdredge%active = .true.
       else
          paract = pdredge%paractive
          call flw_gettabledata(tseriesfile, paract(1) , paract(2), &
                   & paract(3), paract(4)  , values    , ltimhr   , &
                   & julday   , gdp        )
          pdredge%active = values(1)>0.0_fp
       endif
    enddo
    !
    ! For each dump area determine the maximum dump capacity
    !
    do ib = 1, nadump
       pdump => dump_prop(ib)
       area => pdump%area
       reflevel => pdump%reflevel
       !
       ! Set the reference level and compute dump capacity and area.
       !
       voltim = 0.0_fp
       do i = 1,pdump%npnt
          nm = pdump%nm(i)
          if (nm==0) then
             reflevel(i) = 0.0_fp
             cycle
          endif
          !
          select case (pdump%depthdef)
          case (DEPTHDEF_REFPLANE)
             reflevel(i) = refplane(nm)
          case (DEPTHDEF_WATERLVL)
             reflevel(i) = s1(nm)
          case (DEPTHDEF_MAXREFWL)
             reflevel(i) = max(s1(nm),refplane(nm))
          case (DEPTHDEF_MINREFWL)
             reflevel(i) = min(s1(nm),refplane(nm))
          end select
          if (kfsed(nm)==1 .or. pdredge%dredgewhendry) then
             voltim = voltim + max( (reflevel(i) - pdump%mindumpdepth) + real(dps(nm),fp), 0.0_fp)*area(i)
          endif
       enddo
       !
       ! If capacity limited use dump capacity to distribute sediment over the
       ! domains, otherwise use the area.
       !
       if (pdump%dumpcapaflag) then
          globaldumpcap(ib) = voltim
       else
          globaldumpcap(ib) = 0.0_fp
       endif
    enddo
    !
    if (numdomains > 1 .or. parll) then
       !
       ! Communicate dump capacity with other domains
       !
       call dredgecommunicate(globaldumpcap, nadump, error, msgstr)
       if (error) goto 999
    endif
    !
    ! For each dredging area carry out the dredging.
    !
    do ia = 1,nadred+nasupl
       pdredge => dredge_prop(ia)
       voldred(ia,:) = 0.0_fp
       !
       ! If not in a dredging interval then go to next dredge/nourishment area
       !
       if (.not. pdredge%active) cycle
       if (pdredge%npnt==0 .and. pdredge%itype/=DREDGETYPE_NOURISHMENT) cycle
       !
       ! Maximum dredging volume depends on morphological time step.
       ! Although during the initial period morfac is arbitrary,
       ! it should effectively be set to 0.
       !
       if ((comparereal(morfac,0.0_fp) == 0) .or. nst < itmor) then
          !
          ! Rate limited dredging will be zero during simulation phases
          ! with morfac=0. User may have allowed for (unlimited)
          ! instaneous dredging during such periods.
          !
          if (pdredge%if_morfac_0) then
             maxvol = -999.0_fp
          else
             maxvol = 0.0_fp
          endif
       elseif (comparereal(pdredge%maxvolrate  ,-999.0_fp) /= 0) then
          !
          ! Rate limited dredging.
          !
          maxvol = pdredge%maxvolrate*hdt*morfac
       else
          !
          ! Dredging speed unconstrained.
          !
          maxvol = -999.0_fp
       endif
       !
       if (pdredge%dumplimited) then
          maxdumpvol = 0.0_fp
          do il = 1, nalink
             if (link_def(il,1) == ia) then
                maxdumpvol = maxdumpvol + globaldumpcap(link_def(il,2))
             endif
          enddo
          !
          if (comparereal(maxvol,-999.0_fp) == 0) then
             maxvol = maxdumpvol
          else
             maxvol = min(maxvol, maxdumpvol)
          endif
       endif
       !
       if (pdredge%itype == DREDGETYPE_NOURISHMENT) then
          if (dredge_domainnr /= 1) cycle
          !
          if (comparereal(maxvol, -999.0_fp) == 0) then
             maxvol = pdredge%totalvolsupl
          endif
          if (comparereal(pdredge%totalvolsupl, -999.0_fp) /= 0) then
             maxvol = min(pdredge%totalvolsupl,maxvol)
             pdredge%totalvolsupl = pdredge%totalvolsupl-maxvol
          endif
          do lsed = 1, lsedtot
             voldred(ia,lsed) = 0.01_fp*percsupl(pdredge%idx_type,lsed)*maxvol
          enddo
          cycle
       endif
       !
       ! Dredging down to certain depth or level, or dredging at specified rate.
       !
       hdune => pdredge%hdune
       hdune = 0.0_fp
       reflevel => pdredge%reflevel
       reflevel = 0.0_fp
       bedlevel => pdredge%bedlevel
       bedlevel = 0.0_fp
       sedimentdepth => pdredge%sedimentdepth
       sedimentdepth = 0.0_fp
       !
       do i = 1, pdredge%npnt
          nm = pdredge%nm(i)
          if (nm==0) cycle
          !
          bedlevel(i) = -real(dps(nm),fp)
          !
          if (pdredge%use_dunes) hdune(i) = duneheight(nm)
          !
          select case (pdredge%depthdef)
          case (DEPTHDEF_REFPLANE)
             reflevel(i) = refplane(nm)
          case (DEPTHDEF_WATERLVL)
             reflevel(i) = s1(nm)
          case (DEPTHDEF_MAXREFWL)
             reflevel(i) = max(s1(nm),refplane(nm))
          case (DEPTHDEF_MINREFWL)
             reflevel(i) = min(s1(nm),refplane(nm))
          end select
          !
          ! The sediment depth is stored always as a separate thickness instead
          ! of the bottom of the sediment column as a "rock level" for reasons
          ! of accuracy.
          !
          if (kfsed(nm)==1 .or. pdredge%dredgewhendry) then
             if (pdredge%obey_cmp) then
                call getsedthick(gdp%gdmorlyr, nm, sedimentdepth(i))
             else
                sedimentdepth(i) = 1.0E11_fp
             endif
          else
             sedimentdepth(i) = 0.0_fp
          endif
       enddo
       !
       if (.not.pdredge%in1domain) then
          !
          ! communicate dredge data among domains
          !
          call dredgecommunicate(reflevel, pdredge%npnt, error, msgstr)
          if (error) goto 999
          call dredgecommunicate(bedlevel, pdredge%npnt, error, msgstr)
          if (error) goto 999
          call dredgecommunicate(hdune, pdredge%npnt, error, msgstr)
          if (error) goto 999
          call dredgecommunicate(sedimentdepth, pdredge%npnt, error, msgstr)
          if (error) goto 999
       endif
       !
       availvolume = 0.0_fp
       area => pdredge%area
       dz_dredge => pdredge%dz_dredge
       dunetoplevel => pdredge%dunetoplevel
       triggerlevel => pdredge%triggerlevel
       troughlevel => pdredge%troughlevel
       triggered => pdredge%triggered
       triggered = .false.
       do i = 1, pdredge%npnt
          !
          ! Derive the other characteristic levels from the bed level.
          !
          dunetoplevel(i) = bedlevel(i) + hdune(i)/2
          triggerlevel(i) = bedlevel(i) + pdredge%alpha_dh*hdune(i)
          troughlevel(i) = bedlevel(i) - hdune(i)/2
       enddo
       !
       ddp = pdredge%dredge_depth
       clr = pdredge%clearance
       if (pdredge%stilldredging) then
          !
          ! If dredging was not completed last time, lower threshold depth
          ! now by clearance level. NOTE: This implementation works only in
          ! combination with trigger_all.
          !
          ddp = ddp + clr
          clr = 0.0_fp
          pdredge%stilldredging = .false.
       endif
       !
       dredged = .false.
       ploughed = .false.
       !
       !-----------------------------------------------------------------------
       ! Trigger dredging and ploughing
       !
       plough_fac = 1.0_fp - pdredge%plough_effic
       select case (pdredge%triggertype)
       case (DREDGETRIG_POINTBYPOINT,DREDGETRIG_ALLBYONE)
          !
          ! In case of one point triggers all: check whether the bed level at
          ! any one point is above the critical level for triggering dredging.
          ! Allow only points with sediment to trigger dredging.
          !
          if (pdredge%triggertype==DREDGETRIG_ALLBYONE) then
             do i = 1, pdredge%npnt
                !
                ! The check on the bed levels will be whether
                ! triggerlevel = z_bed (+duneheight) > z_dredge = z_ref - dredgedepth
                !
                z_dredge = reflevel(i) - ddp
                if (z_dredge<triggerlevel(i) .and. sedimentdepth(i)>0.0_fp) then
                   !
                   ! If dredging is triggered, then dredge all points
                   ! above the critical level minus clearance depth.
                   !
                   ddp = ddp + clr
                   clr = 0.0_fp
                   exit
                endif
             enddo
          endif
          !
          ! Determine how much we would dredge at every location if the dredge
          ! rate is not limited by a maximum dredge rate and compute the
          ! resulting total volume.
          !
          do i = 1, pdredge%npnt
             !
             ! Trigger dredging based on depth without clearance
             ! (unless clearance has been added above due to
             ! trigger all or continuation of previous time step).
             !
             z_dredge = reflevel(i) - ddp
             if (triggerlevel(i)>z_dredge .and. sedimentdepth(i)>0.0_fp) then
                !
                if (plough_fac<1.0_fp) then
                   if (bedlevel(i)+plough_fac*pdredge%alpha_dh*hdune(i) < z_dredge-clr) then
                      !
                      ! if just ploughing the dunes is sufficient to satisfy
                      ! the critical depth plus clearance, then just plough
                      ! the dunes.
                      !
                      ploughed = .true.
                      hdune(i) = hdune(i)*plough_fac
                      dz_dredge(i) = 0.0_fp
                      cycle
                   endif
                endif
                !
                ! If dredging is triggered, lower dredging level by
                ! clearance.
                !
                triggered(i) = .true.
                z_dredge = z_dredge - clr
                if (z_dredge<=troughlevel(i)) then
                   !
                   ! Don't dredge more than is available unless
                   ! indicated otherwise.
                   !
                   dz_dredge(i) = min(bedlevel(i)-z_dredge, sedimentdepth(i))
                else
                   !
                   ! dune range:
                   ! dredgeable volume = 1/2 * dz * [(dz/H_dune) * L_dune]
                   ! effective height  = volume / L_dune = dz^2/(2*H_dune)
                   !
                   dz_dredge(i) = (dunetoplevel(i) - z_dredge)**2/(2*hdune(i))
                endif
                !
                ! Don't dredge negative amounts of sediment.
                !
                dz_dredge(i) = max(dz_dredge(i),0.0_fp)
                availvolume = availvolume + dz_dredge(i)*area(i)
             else 
                dz_dredge(i) = 0.0_fp
             endif
             !
          enddo
          requiredvolume = availvolume
       case (DREDGETRIG_ALLBYAVG)
          !
          ! In case of average triggers all: check whether the average bed
          ! level is above the critical level for triggering dredging.
          !
          avg_depth     = 0.0_fp
          avg_trigdepth = 0.0_fp
          dredge_area   = 0.0_fp
          do i = 1, pdredge%npnt
             avg_depth     = avg_depth     + (reflevel(i)-bedlevel(i))*area(i)
             avg_trigdepth = avg_trigdepth + (reflevel(i)-triggerlevel(i))*area(i)
             dredge_area   = dredge_area   + area(i)
             !
             ! maximum depth to dredge is the amount of sediment available
             ! all points with sediment are triggered
             !
             dz_dredge(i) = sedimentdepth(i)
             availvolume = availvolume + dz_dredge(i)*area(i)
             if (sedimentdepth(i)>0) then
                triggered(i) = .true.
             endif
          enddo
          avg_depth     = avg_depth/dredge_area
          avg_trigdepth = avg_trigdepth/dredge_area
          !
          if (avg_trigdepth<ddp) then
             !
             ! If dredging is triggered, lower dredging level by
             ! clearance.
             !
             avg_alphadune = avg_trigdepth - avg_depth
             if (avg_depth-plough_fac*avg_alphadune < ddp-clr) then
                !
                ! if just ploughing the dunes is sufficient to satisfy
                ! the critical depth plus clearance, then just plough
                ! the dunes.
                !
                ploughed = .true.
                do i = 1, pdredge%npnt
                   hdune(i) = hdune(i)*plough_fac
                enddo
                requiredvolume = 0.0_fp
             else
                requiredvolume = (ddp - avg_trigdepth + clr)*dredge_area
             endif
          else 
             requiredvolume = 0.0_fp
          endif
       end select
       !
       if (ploughed) then
           nploughed(ia) = nploughed(ia)+1
       endif
       if (requiredvolume > 0.0_fp .and. (maxvol < 0.0_fp .or. maxvol > 0.0_fp)) then
           ndredged(ia) = ndredged(ia)+1
       else
           cycle
       endif
       !
       !-----------------------------------------------------------------------
       ! Perform dredging
       !
       if (comparereal(maxvol,0.0_fp) == 0) then
          !
          ! No dredging capacity, reset all dredging amounts to zero.
          !
          dz_dredge = 0.0_fp
       elseif ((maxvol > 0.0_fp .and. requiredvolume > maxvol) .or. &
             & (requiredvolume > 0.0_fp .and. requiredvolume < availvolume)) then
          !
          ! a) we need to dredge more than we can dredge per time step, or
          ! b) dredging has been triggered by an average level and we still
          !    have to figure out where to dredge.
          !
          ! In case a) limit the amount of dredging to what we can dredge and
          ! set a flag to indicate to continue dredging at the next time step
          ! The latter is only relevant in case of dredging and not in case of
          ! sandmining.
          !
          if (maxvol > 0.0_fp .and. requiredvolume > maxvol) then
             requiredvolume = maxvol
             pdredge%stilldredging = pdredge%itype==DREDGETYPE_DREDGING
          endif
          !
          ! Reduce total dredging volume and dredging amounts
          ! per point at current time step
          !
          select case (dredge_prop(ia)%dredgedistr)
          case (DREDGEDISTR_UNIFORM)
             !
             ! dredge sediment uniformly:
             !  * sort nm points based on increasing dz_dredge:
             !    thinnest sediment layer will become 1,
             !    thickest sediment layer will become pdredge%npnt
             !
             call sortindices(pdredge%inm,pdredge%npnt, &
                & dz_dredge, 1, pdredge%npnt,.true.)
             !
             !  * increase thickness gradually
             !
             dredge_area = globalareadred(ia)
             do i = 1, pdredge%npnt
                inm = pdredge%inm(i)
                !
                ! check whether dredge thickness can be found
                ! that matches the required dredging volume.
                !
                dmax = dz_dredge(inm)
                extravolume = dredge_area * dmax
                if (extravolume<requiredvolume) then
                   requiredvolume = requiredvolume - dmax*area(inm)
                   dredge_area = dredge_area - area(inm)
                else
                   dmax = requiredvolume / dredge_area
                   do j = i,pdredge%npnt
                      jnm = pdredge%inm(j)
                      dz_dredge(jnm) = dmax
                   enddo
                   exit
                endif
             enddo
          case (DREDGEDISTR_HIGHEST,DREDGEDISTR_SHALLOWEST) 
             !
             ! dredge slices off the top of the bed
             !  * sort nm points based on decreasing dunetoplevel
             !    (for simulations without dune effect, this is equal
             !    to the bed level):
             !    the highest point (max dunetoplevel) will become 1,
             !    the deepest point (min dunetoplevel) will become npnt.
             !
             ! Make sure that points that are not triggered for dredging
             ! do not actively participate in the dredging.
             !
             if (dredge_prop(ia)%dredgedistr==DREDGEDISTR_SHALLOWEST) then
                do i=1,pdredge%npnt
                   bedlevel(i)     = bedlevel(i)     - reflevel(i)
                   dunetoplevel(i) = dunetoplevel(i) - reflevel(i)
                   triggerlevel(i) = triggerlevel(i) - reflevel(i)
                   troughlevel(i)  = troughlevel(i)  - reflevel(i)
                enddo
             endif
             !
             do i=1,pdredge%npnt
                if (.not.triggered(i)) dunetoplevel(i) = -1.0E11_fp
             enddo
             !
             call sortindices(pdredge%inm,pdredge%npnt, &  
                & dunetoplevel, 1, pdredge%npnt, .false.)
             !
             !  * determine the approximate height by checking the top
             !    levels
             !
             imin = 1              ! volume above = 0.0
             imax = pdredge%npnt+1 ! dummy point: volume = availvolume>maxvol
             do while (imax>imin+1)
                i = (imin+imax)/2
                inm = pdredge%inm(i)
                !
                ! check whether there is sufficient sediment above the top
                ! of the sediment column indicated by index i.
                !
                z_dredge = dunetoplevel(inm)
                voltim = 0.0_fp
                do j = 1, i
                   jnm = pdredge%inm(j)
                   !
                   if (z_dredge<=troughlevel(jnm)) then
                      dz = min(bedlevel(jnm)-z_dredge, sedimentdepth(jnm))
                   else
                      dz = (dunetoplevel(jnm) - z_dredge)**2/(2*hdune(jnm))
                   endif
                   !
                   voltim = voltim + max(dz,0.0_fp)*area(jnm)
                enddo
                if (voltim>requiredvolume) then
                   imax = i
                else
                   imin = i
                endif
             enddo
             !
             zmax = dunetoplevel(pdredge%inm(imin))
             if (imin<pdredge%npnt) then
                zmin = dunetoplevel(pdredge%inm(imin+1))
             else
                zmin = -1.0E11_fp
             endif
             imaxdunes = imin
             do i = imin+1,pdredge%npnt
                inm = pdredge%inm(i)
                dz_dredge(inm) = 0.0_fp
             enddo
             !
             ! dredge level is known to lie between zmin and zmax
             ! points imaxdunes+1:npnt have dunetoplevel below/equal zmin
             ! points 1:imaxdunes have dunetoplevel above/equal zmax
             !
             ! now determine imindunes such that the points
             ! imindunes:imaxdunes have troughlevel below/equal zmin
             ! and dunetoplevel above/equal zmax
             !
             if (pdredge%use_dunes) then
                !
                !  * sort the first imaxdunes points based on decreasing troughlevel:
                !    the highest point (max troughlevel) will become 1,
                !    the deepest point (min troughlevel) will become imaxdunes.
                !
                call sortindices(pdredge%inm,imaxdunes, &  
                   & troughlevel, 1, pdredge%npnt, .false.)
                !
                !  * determine the approximate height by checking the trough
                !    levels
                !
                ! default imindunes = 1 in case all points 1:imaxdunes have
                !
                imindunes = 1
                do i = imaxdunes,1,-1
                   inm = pdredge%inm(i)
                   z_dredge = troughlevel(inm)
                   if (z_dredge>=zmax) then
                      !
                      ! troughlevel above zmax. Thus imindunes has been found.
                      !
                      imindunes = i+1
                      exit
                   elseif (z_dredge>zmin) then
                      !
                      ! troughlevel below zmax and above zmin. Use this level
                      ! to narrow the zmin-zmax range.
                      !
                      voltim = 0.0_fp
                      do j = 1, imaxdunes
                         jnm = pdredge%inm(j)
                         !
                         if (z_dredge<=troughlevel(jnm)) then
                            dz = min(bedlevel(jnm)-z_dredge, sedimentdepth(jnm))
                         else
                            dz = (dunetoplevel(jnm) - z_dredge)**2/(2*hdune(jnm))
                         endif
                         !
                         voltim = voltim + max(dz,0.0_fp)*area(jnm)
                      enddo
                      if (voltim>requiredvolume) then
                         !
                         ! zmin level can be raised.
                         !
                         zmin = z_dredge
                      else
                         !
                         ! after update the troughlevel is above zmax, so,
                         ! imindunes has been found.
                         !
                         zmax = z_dredge
                         imindunes = i+1
                         exit
                      endif
                      !
                   endif
                enddo
             else
                imindunes = imaxdunes+1
             endif
             !
             ! dredge level is known to lie between zmin and zmax
             ! points imaxdunes+1:npnt have dunetoplevel below/equal zmin
             ! points 1:imaxdunes have dunetoplevel above/equal zmax
             ! points imindunes:imaxdunes have troughlevel below zmin
             ! points 1:imindunes-1 have troughlevel above zmax
             !
             ! now determine irock such that the points
             ! 1:irock have bedlevel-sedimentdepth above/equal zmax
             !
             !  * sort the first imindunes-1 points based on decreasing bedlevel-sedimentdepth:
             !    the highest point (max bedlevel-sedimentdepth) will become 1,
             !    the deepest point (min bedlevel-sedimentdepth) will become imindunes-1.
             !
             pdredge%sortvar = bedlevel-sedimentdepth
             call sortindices(pdredge%inm,imindunes-1, &  
                & pdredge%sortvar, 1, pdredge%npnt, .false.)
             !
             !  * determine the approximate height by checking the trough
             !    levels
             !
             ! default imindunes = 1 in case all points 1:imaxdunes have
             !
             irock = 0
             do i = imindunes-1,1,-1
                inm = pdredge%inm(i)
                z_dredge = bedlevel(inm)-sedimentdepth(inm)
                if (z_dredge>=zmax) then
                   !
                   ! bedlevel-sedimentdepth above zmax. Thus irock has been found.
                   !
                   irock = i
                   exit
                elseif (z_dredge>zmin) then
                   !
                   ! bedlevel-sedimentdepth below zmax and above zmin. Use this level
                   ! to narrow the zmin-zmax range.
                   !
                   voltim = 0.0_fp
                   do j = 1, imaxdunes
                      jnm = pdredge%inm(j)
                      !
                      if (z_dredge<=troughlevel(jnm)) then
                         dz = min(bedlevel(jnm)-z_dredge, sedimentdepth(jnm))
                      else
                         dz = (dunetoplevel(jnm) - z_dredge)**2/(2*hdune(jnm))
                      endif
                      !
                      voltim = voltim + max(dz,0.0_fp)*area(jnm)
                   enddo
                   if (voltim>requiredvolume) then
                      !
                      ! zmin level can be raised.
                      !
                      zmin = z_dredge
                   else
                      !
                      ! after update the bedlevel-sedimentdepth is above zmax, so,
                      ! irock has been found.
                      !
                      zmax = z_dredge
                      irock = i
                      exit
                   endif
                   !
                endif
             enddo
             !
             ! dredge level is known to lie between zmin and zmax
             ! points imaxdunes+1:npnt have dunetoplevel below/equal zmin
             ! points 1:imaxdunes have dunetoplevel above/equal zmax
             ! points imindunes:imaxdunes have troughlevel below zmin
             ! points 1:imindunes-1 have troughlevel above zmax
             ! points 1:irock have bedlevel-sedimentdepth above/equal zmax
             !
             !  * determine the exact height of the critical dredge depth
             !    Critical dredge depth lies between zmin and zmax.
             !
             ! points 1:irock can be dredged completely.
             !
             voltim = 0.0_fp
             lin_dz = 0.0_fp
             qua_dz = 0.0_fp
             do i = 1,irock
                inm = pdredge%inm(i)
                !
                voltim = voltim + dz_dredge(inm)*area(inm)
                !
                if (pdredge%use_dunes) then
                   hdune(inm) = 0.0_fp
                endif
             enddo
             !
             ! at points irock+1:imindunes-1 the dunes are dredged
             ! completely and possibly a bit more.
             !
             do i = irock+1,imindunes-1
                inm = pdredge%inm(i)
                !
                dz_dredge(inm) = bedlevel(inm)-zmax
                !
                voltim = voltim + dz_dredge(inm)*area(inm)
                lin_dz = lin_dz + area(inm)
             enddo
             !
             ! at points imindunes:imaxdunes only part of the dunes
             ! will be dredged.
             !
             do i = imindunes,imaxdunes
                inm = pdredge%inm(i)
                !
                dz = dunetoplevel(inm) - zmax
                div2h = 1.0_fp /(2*hdune(inm))
                !
                dz_dredge(inm) = dz**2*div2h
                !
                voltim = voltim + dz_dredge(inm)*area(inm)
                lin_dz = lin_dz + dz*div2h*area(inm)
                qua_dz = qua_dz + area(inm)*div2h
             enddo
             !
             ! solve equation requiredvolume = voltim + dz*lin_dz + dz**2*qua_dz
             !
             if (comparereal(qua_dz,0.0_fp) == 0) then
                !
                ! a = 0
                ! b = lin_dz
                ! c = voltim-requiredvolume
                !
                ! dz = -c/b
                !
                dz = (requiredvolume-voltim)/lin_dz
             else
                !
                ! a = qua_dz
                ! b = lin_dz
                ! c = voltim-requiredvolume
                !
                ! dz = [-b +/- sqrt(b**2-4*a*c)]/(2*a)
                ! dz = -b/(2*a) + sqrt{[b/(2*a)]**2-c/a}
                !
                lin_dz = -lin_dz/(2*qua_dz)
                dz = lin_dz + sqrt(lin_dz**2+(requiredvolume-voltim)/qua_dz)
             endif
             !
             z_dredge = max(zmax-dz,zmin)
             !
             do i = irock+1,imindunes-1
                inm = pdredge%inm(i)
                !
                dz_dredge(inm) = bedlevel(inm)-z_dredge
                !
                if (pdredge%use_dunes) then
                   hdune(inm) = 0.0_fp
                endif
             enddo
             !
             do i = imindunes,imaxdunes
                inm = pdredge%inm(i)
                !
                dz = dunetoplevel(inm) - z_dredge
                div2h = 1.0_fp /(2*hdune(inm))
                !
                dz_dredge(inm) = dz**2*div2h
                !
                if (pdredge%use_dunes) then
                   hdune(inm) = 0.0_fp
                endif
             enddo
          case (DREDGEDISTR_PROPORTIONAL)
             !
             ! dredge sediment proportionally to amount of sediment available
             ! for dredging
             !
             factor = requiredvolume / availvolume
             do i = 1, pdredge%npnt
                dz_dredge(i) = dz_dredge(i) * factor
             enddo
          case (DREDGEDISTR_HIGHFIRST,DREDGEDISTR_SHALLOWFIRST)
             !
             ! dredge highest points first
             !  * sort nm points based on decreasing dunetoplevel (for
             !    simulations without dune effect, this is equal to the
             !    bed level):
             !    the highest point (max dunetoplevel) will become 1,
             !    the deepest point (min dunetoplevel) will become npnt.
             !
             ! Make sure that points that are not triggered for dredging
             ! do not actively participate in the dredging.
             !
             if (dredge_prop(ia)%dredgedistr==DREDGEDISTR_SHALLOWFIRST) then
                do i=1,pdredge%npnt
                   bedlevel(i)     = bedlevel(i)     - reflevel(i)
                   dunetoplevel(i) = dunetoplevel(i) - reflevel(i)
                   triggerlevel(i) = triggerlevel(i) - reflevel(i)
                   troughlevel(i)  = troughlevel(i)  - reflevel(i)
                enddo
             endif
             !
             do i=1,pdredge%npnt
                if (.not.triggered(i)) dunetoplevel(i) = -1.0E11_fp
             enddo
             !
             call sortindices(pdredge%inm,pdredge%npnt, &  
                & dunetoplevel, 1, pdredge%npnt, .false.)
             !
             !  dredge until you obtain the required volume
             !
             voltim = 0.0_fp
             do i = 1,pdredge%npnt
                inm = pdredge%inm(i)
                !
                if (voltim+dz_dredge(inm)*area(inm)<requiredvolume) then
                   voltim = voltim + dz_dredge(inm)*area(inm)
                   hdune(inm) = 0.0_fp
                else
                   dz_dredge(inm) = (requiredvolume-voltim)/area(inm)
                   hdune(inm) = 0.0_fp
                   !
                   do j=i+1,pdredge%npnt
                      jnm = pdredge%inm(j)
                      !
                      dz_dredge(jnm) = 0.0_fp
                      hdune(jnm) = 0.0_fp
                   enddo
                   exit
                endif
             enddo
          case (DREDGEDISTR_LOWFIRST,DREDGEDISTR_DEEPFIRST)
             !
             ! dredge lowest points first
             !  * sort nm points based on increasing dunetoplevel (for
             !    simulations without dune effect, this is equal to the
             !    bed level):
             !    the deepest point (min dunetoplevel) will become 1,
             !    the highest point (max dunetoplevel) will become npnt.
             !
             ! Make sure that points that are not triggered for dredging
             ! do not actively participate in the dredging.
             !
             if (dredge_prop(ia)%dredgedistr==DREDGEDISTR_DEEPFIRST) then
                do i=1,pdredge%npnt
                   bedlevel(i)     = bedlevel(i)     - reflevel(i)
                   dunetoplevel(i) = dunetoplevel(i) - reflevel(i)
                   triggerlevel(i) = triggerlevel(i) - reflevel(i)
                   troughlevel(i)  = troughlevel(i)  - reflevel(i)
                enddo
             endif
             !
             do i=1,pdredge%npnt
                if (.not.triggered(i)) dunetoplevel(i) = 1.0E11_fp
             enddo
             !
             call sortindices(pdredge%inm,pdredge%npnt, &  
                & dunetoplevel, 1, pdredge%npnt, .true.)
             !
             !  dredge until you obtain the required volume
             !
             voltim = 0.0_fp
             do i = 1,pdredge%npnt
                inm = pdredge%inm(i)
                !
                if (voltim+dz_dredge(inm)*area(inm)<requiredvolume) then
                   voltim = voltim + dz_dredge(inm)*area(inm)
                   hdune(inm) = 0.0_fp
                else
                   dz_dredge(inm) = (requiredvolume-voltim)/area(inm)
                   hdune(inm) = 0.0_fp
                   !
                   do j=i+1,pdredge%npnt
                      jnm = pdredge%inm(j)
                      !
                      dz_dredge(jnm) = 0.0_fp
                      hdune(jnm) = 0.0_fp
                   enddo
                   exit
                endif
             enddo
          end select
       else
          !
          ! Dredging not limited by maximum volume, so we will dredge the
          ! dz_dredge amount already computed.
          !
          if (pdredge%use_dunes) then 
             do i = 1, pdredge%npnt
                nm = pdredge%nm(i)
                !
                if (dz_dredge(i)>0.0_fp) then
                   hdune(i) = 0.0_fp
                endif
             enddo
          endif
       endif
       !
       do i = 1,pdredge%npnt
          nm = pdredge%nm(i)
          if (nm==0) cycle
          !
          dzdred(nm) = dz_dredge(i)
          if (pdredge%use_dunes) duneheight(nm) = hdune(i)
       enddo
       !
       ! Update sediment administration for sandmining/dredging only
       ! dbodsd is filled (kg/m^2 sediment removed in a cell)
       !
       if (cmpupd) then
          if (gettoplyr(gdp%gdmorlyr, dzdred, dbodsd, gdp%messages) /= 0) then
             call writemessages(gdp%messages, lundia)
             call d3stop(1, gdp)
          endif
       else
          dbodsd = 0.0_fp
       endif
       !
       ! Use dbodsd to calculate voldred, and update dps
       !
       do i = 1, pdredge%npnt
          nm = pdredge%nm(i)
          if (nm==0) cycle
          !
          dz = 0.0_fp
          do lsed = 1, lsedtot
             dzl               = dbodsd(lsed, nm) / cdryb(lsed)
             voldred(ia,lsed)  = voldred(ia,lsed) + dzl * area(i)
             dz                = dz + dzl
          enddo
          if (pdredge%obey_cmp) then
             dps(nm)               = dps(nm) + dz
          else
             dps(nm)               = dps(nm) + dz_dredge(i)
             voldred(ia,lsedtot+1) = voldred(ia,lsedtot+1) + (dz_dredge(i)-dz) * area(i)
          endif
          dzdred(nm)        = 0.0_fp
       enddo
    enddo
    !
    if (numdomains > 1 .or. parll) then
       !
       ! Communicate dredged volumes with other domains
       !
       call dredgecommunicate(voldred, (nadred+nasupl)*(lsedtot+1), error, msgstr)
       if (error) goto 999
    endif
    !
    !--------------------------------------------------------------------------
    ! Distribute sediments over dump areas
    !
    voldump(1:nadump,1:lsedtot) = 0.0_fp
    do ia = 1, nadred+nasupl
       pdredge => dredge_prop(ia)
       !
       ! Add dredged volumes to the total dredged volumes (cumulative!)
       !
       voldredged = 0.0_fp
       do lsed = 1, lsedtot
          voldredged = voldredged + voldred(ia,lsed)
       enddo
       totvoldred(ia) = totvoldred(ia) + voldredged + voldred(ia,lsedtot+1)
       if (voldredged<=0.0_fp) cycle
       !
       select case (pdredge%dumpdistr)
       case (DR2DUDISTR_PERCENTAGE)
          !
          ! Distribute based on user-specified percentages
          !
          do i = 1,nalink
             if (link_def(i,1) /= ia) cycle
             ib = link_def(i,2)
             i2 = pdredge%outletlink
             if (i2>0) then
                ib2 = link_def(i2,2)
             else
                ib2 = 0
             endif
             !
             voldumped = 0.0_fp
             do lsed = 1,lsedtot
                voltim = 0.01_fp*link_percentage(i,lsed)*voldred(ia,lsed)
                voldumped = voldumped + voltim
             enddo
             !
             if (voldumped>globaldumpcap(ib) .and. dump_prop(ib)%dumpcapaflag) then
                fracdumped = globaldumpcap(ib)/voldumped
                globaldumpcap(ib) = 0.0_fp
             else
                fracdumped = 1.0_fp
                if (dump_prop(ib)%dumpcapaflag) then
                   globaldumpcap(ib) = globaldumpcap(ib)-voldumped
                endif
             endif
             fracoutlet = 1.0_fp - fracdumped
             !
             do lsed = 1,lsedtot
                voltim = 0.01_fp*link_percentage(i,lsed)*voldred(ia,lsed)
                link_sum(i, lsed) = link_sum(i, lsed) + voltim*fracdumped
                voldump(ib, lsed) = voldump(ib, lsed) + voltim*fracdumped
                !
                if (ib2>0) then
                   link_sum(i2, lsed) = link_sum(i2, lsed) + voltim*fracoutlet
                   voldump(ib2, lsed) = voldump(ib2, lsed) + voltim*fracoutlet
                endif
             enddo
          enddo
       case (DR2DUDISTR_SEQUENTIAL)
          !
          ! Distribute according user-specified order up to maximum
          ! capacity
          !
          voldumped = 0.0_fp
          do i = 1,nalink
             if (link_def(i,1) /= ia) cycle
             ib = link_def(i,2)
             maxvol = voldredged-voldumped
             if (dump_prop(ib)%dumpcapaflag) then
                maxvol = min(maxvol,globaldumpcap(ib))
             endif
             !
             do lsed = 1,lsedtot
                voltim = maxvol*(voldred(ia,lsed)/voldredged)
                link_sum(i, lsed) = link_sum(i, lsed) + voltim
                voldump(ib, lsed) = voldump(ib, lsed) + voltim
             enddo
             if (dump_prop(ib)%dumpcapaflag) then
                globaldumpcap(ib) = globaldumpcap(ib)-maxvol
             endif
             !
             voldumped = voldumped + maxvol
             if (comparereal(voldredged,voldumped) == 0) exit
          enddo
          !
          ! Maximum capacity reached; any sediment remaining?
          !
          if (voldredged>voldumped) then
             maxvol = voldredged - voldumped
             i = pdredge%outletlink
             ib = link_def(i,2)
             do lsed = 1,lsedtot
                voltim = maxvol*(voldred(ia,lsed)/voldredged)
                link_sum(i, lsed) = link_sum(i, lsed) + voltim
                voldump(ib, lsed) = voldump(ib, lsed) + voltim
             enddo
          endif
       case (DR2DUDISTR_PROPORTIONAL)
          maxvol = 0.0_fp
          do i = 1,nalink
             if (link_def(i,1) /= ia) cycle
             if (i==pdredge%outletlink) cycle
             ib = link_def(i,2)
             maxvol = maxvol + globaldumpcap(ib)
          enddo
          !
          ! Distribute proportionally based on dumping capacity
          ! Don't dump more than capacity available
          !
          voldumped = min(voldredged,maxvol)
          if (voldumped>0.0_fp) then
             do i = 1,nalink
                if (link_def(i,1) /= ia) cycle
                if (i==pdredge%outletlink) cycle
                ib = link_def(i,2)
                !
                voltot = (globaldumpcap(ib)/maxvol)*voldumped
                do lsed = 1,lsedtot
                   voltim = voltot*(voldred(ia,lsed)/voldredged)
                   link_sum(i, lsed) = link_sum(i, lsed) + voltim
                   voldump(ib, lsed) = voldump(ib, lsed) + voltim
                enddo
                globaldumpcap(ib) = globaldumpcap(ib)-voltot
             enddo
          endif
          !
          ! Maximum capacity reached; any sediment remaining?
          !
          if (voldredged>voldumped) then
             maxvol = voldredged - voldumped
             i = pdredge%outletlink
             ib = link_def(i,2)
             do lsed = 1,lsedtot
                voltim = maxvol*(voldred(ia,lsed)/voldredged)
                link_sum(i, lsed) = link_sum(i, lsed) + voltim
                voldump(ib, lsed) = voldump(ib, lsed) + voltim
             enddo
          endif
       end select
    enddo
    !
    !--------------------------------------------------------------------------
    ! And finally: Dumping
    !
    if (cutcell==2) then ! areas of active cell can vary with bank erosion, recompute them (not ok for parellel/domain decomposition case since    globalareadump (=localareadump ) is simply overwriten
       do ib = 1, nadump
          pdump => dump_prop(ib)
          area => pdump%area
          globalareadump(ib) = 0._fp !I overwrite it
          do i = 1, pdump%npnt
             nm = pdump%nm(i)
             if (nm==0) cycle
             area(i) = gsqs(nm)*agsqs(nm)
             globalareadump(ib) = globalareadump(ib) + area(i)
          enddo           
       enddo
    endif
!
    dbodsd(1:lsedtot, 1:nmmax) = 0.0_fp
    do ib = 1, nadump
       pdump => dump_prop(ib)
       !
       ! Add dumped volumes to the total dumped volumes (cumulative!)
       !
       voldumped = 0.0_fp
       do lsed = 1, lsedtot
          voldumped = voldumped + voldump(ib, lsed)
       enddo
       totvoldump(ib) = totvoldump(ib) + voldumped
       !
       ! Skip dump areas where nothing has to be dumped
       !
       if (comparereal(voldumped,0.0_fp) == 0) cycle
       !
       bedlevel => pdump%bedlevel
       bedlevel = 0.0_fp
       hdune => pdump%hdune
       hdune = 0.0_fp
       dz_dump => pdump%dz_dump
       dz_dump = 0.0_fp
       reflevel => pdump%reflevel
       local_cap = .false.
       do i = 1, pdump%npnt
          nm = pdump%nm(i)
          if (nm==0) cycle
          !
          bedlevel(i) = -real(dps(nm),fp)
          if (pdump%use_dunes) hdune(i) = duneheight(nm)
          !
          if (kfsed(nm)==1 .or. pdump%dumpwhendry) then
             if (pdump%dumpcapaflag) then
                dz_dump(i) = max( (reflevel(i) - pdump%mindumpdepth) - bedlevel(i), 0.0_fp)
                local_cap = local_cap .or. dz_dump(i)>0.0_fp
             else
                dz_dump(i) = 1.0E11_fp
                local_cap = .true.
             endif
          else
             dz_dump(i) = 0.0_fp
          endif
       enddo
       !
       area => pdump%area
       if (.not.pdump%in1domain) then
          !
          ! communicate dredge data among domains
          !
          call dredgecommunicate(reflevel, pdump%npnt, error, msgstr)
          if (error) goto 999
          call dredgecommunicate(bedlevel, pdump%npnt, error, msgstr)
          if (error) goto 999
          call dredgecommunicate(dz_dump, pdump%npnt, error, msgstr)
          if (error) goto 999
       endif
       !
       ! Go through dumping procedure only if some dump capacity is available
       ! locally
       !
       if (.not.local_cap) cycle
       !
       select case (pdump%dumpdistr)
       case (DUMPDISTR_UNIFORM)
          !
          ! dump sediment uniformly:
          !  * sort nm points based on increasing dump capacity dz_dump:
          !    least capacity will become 1, maximum capacity wil become
          !    pdump%npnt.
          !
          call sortindices(pdump%inm,pdump%npnt, &
             & dz_dump, 1, pdump%npnt,.true.)
          !
          ! loop over points and increase dzdump gradually
          !
          requiredvolume = voldumped
          dump_area = globalareadump(ib)
          do i = 1, pdump%npnt
             inm = pdump%inm(i)
             !
             extravolume = dz_dump(inm)*dump_area
             if (extravolume<requiredvolume) then
                !
                ! if insufficient capacity at current point, fill it up
                ! and continue with next point
                !
                dump_area = dump_area - area(inm)
                requiredvolume = requiredvolume - dz_dump(inm)*area(inm)
             else
                dzdump = dz_dump(inm)*requiredvolume/extravolume
                !
                ! if sufficient capacity, fill all remaining points and
                ! exit loop
                !
                do j = i, pdump%npnt
                   jnm = pdump%inm(j)
                   !
                   dz_dump(jnm) = dzdump
                enddo
                exit
             endif
          enddo
       case (DUMPDISTR_LOWEST,DUMPDISTR_DEEPEST)
          !
          ! lowest or deepest locations first:
          !  * sort nm points based on increasing bedlevel:
          !    deepest point (min bedlevel) will become 1,
          !    shallowest point (max bedlevel) will become pdump%npnt.
          !
          if (pdump%dumpdistr == DUMPDISTR_DEEPEST) then
             do i=1,pdump%npnt
                bedlevel(i) = bedlevel(i) - reflevel(i)
             enddo
          endif
          !
          call sortindices(pdump%inm,pdump%npnt, &
             & bedlevel, 1, pdump%npnt,.true.)
          !
          !  * search bed level below which sufficient dumping capacity is
          !    available
          !
          requiredvolume = voldumped
          do i = 2,pdump%npnt
             inm = pdump%inm(i)
             z_dump = bedlevel(inm)
             !
             voltim = 0.0_fp
             do j = 1, i-1
                jnm = pdump%inm(j)
                !
                voltim = voltim + max(min(dz_dump(jnm),z_dump-bedlevel(jnm)),0.0_fp)*area(jnm)
             enddo
             !
             if (voltim>=requiredvolume) exit
          enddo
          imax = i-1
          if (imax==pdump%npnt) then
             zmax = 1.0E11_fp
          else
             zmax = z_dump
          endif
          zmin = bedlevel(pdump%inm(imax))
          !
          ! dump level is known to lie between zmin and zmax
          ! points imax+1:npnt have bedlevel above zmax
          ! points 1:imax have bedlevel below zmin
          !
          !  * sort the first imax points based on increasing level of bed
          !    level plus maximum dump thickness
          !
          pdump%sortvar = bedlevel+dz_dump
          call sortindices(pdump%inm,imax, &
             & pdump%sortvar, 1, pdump%npnt,.true.)
          !
          do i = 1,imax
             inm = pdump%inm(i)
             z_dump = pdump%sortvar(inm)
             !
             if (z_dump<zmin) cycle
             if (z_dump>zmax) exit
             !
             voltim = 0.0_fp
             do j = 1, imax
                jnm = pdump%inm(j)
                !
                voltim = voltim + max(min(dz_dump(jnm),z_dump-bedlevel(jnm)),0.0_fp)*area(jnm)
             enddo
             !
             if (voltim>=requiredvolume) then
                zmax = z_dump
                exit
             else
                zmin = z_dump
             endif
          enddo
          imin = i
          !
          ! dump level is known to lie between zmin and zmax
          ! points imax+1:npnt have bedlevel above zmax
          ! points 1:imax have bedlevel below zmin
          ! points 1:imin-1 have capacity limit below zmin
          ! points imin:imax have capacity limit above zmax
          !
          !  * determine exact height of dump level which lies between
          !    zmin and zmax
          !
          voltim = 0.0_fp
          areatim = 0.0_fp
          z_dump = zmin
          do i = 1,imax
             inm = pdump%inm(i)
             !
             voltim = voltim + max(min(dz_dump(inm),z_dump-bedlevel(inm)),0.0_fp)*area(inm)
             if (i>=imin) areatim = areatim + area(inm)
          enddo
          dz = (requiredvolume - voltim)/areatim
          z_dump = zmin + dz
          !
          do i = 1, pdump%npnt
             inm = pdump%inm(i)
             !
             ! determine the thickness of the local deposit
             ! determine the associated volume
             !
             dz_dump(inm) = max(min(dz_dump(inm),z_dump-bedlevel(inm)),0.0_fp)
          enddo
       case (DUMPDISTR_PROPORTIONAL)
          !
          ! proportional to maximum dump depth
          ! determine ratio of dumped volume and capacity
          !
          maxvol = 0.0_fp
          do i = 1, pdump%npnt
             maxvol = maxvol + dz_dump(i)*area(i)
          enddo
          factor = voldumped / maxvol
          do i = 1, pdump%npnt
             dz_dump(i) = dz_dump(i)*factor
          enddo
       end select
       !
       ! Now dump the sediments locally
       !
       do i = 1, pdump%npnt
          nm = pdump%nm(i)
          if (nm==0) cycle
          !
          dz = dz_dump(i)
          do lsed = 1, lsedtot
             dpadd            = dz * (voldump(ib, lsed) / voldumped)
             dbodsd(lsed, nm) = dbodsd(lsed, nm) + dpadd * cdryb(lsed)
          enddo
          !
          dps(nm) = dps(nm) - real(dz_dump(i), prec)
          if (pdump%use_dunes) duneheight(nm) = hdune(i)
       enddo
    enddo
    !
    ! Update sediment administration for dumping only
    ! dbodsd is filled (kg/m^2 sediment added to a cell)
    !
    if (cmpupd) then
       allocate(dz_dummy(gdp%d%nmlb:gdp%d%nmub), stat=istat)
       if (updmorlyr(gdp%gdmorlyr, dbodsd, dz_dummy, gdp%messages) /= 0) then
          call writemessages(gdp%messages, lundia)
          call d3stop(1, gdp)
       else
          call writemessages(gdp%messages, lundia)
       endif
       deallocate(dz_dummy, stat=istat)
    endif
    return

999 call prterr(lundia, 'U021', msgstr)
    call d3stop(1, gdp)
end subroutine dredge


subroutine dredgecommunicate(a, n, error, msgstr)
    use precision
    use dfparall, only: parll, dfloat, dfsum
    implicit none
    !
    integer               , intent(in)    :: n      ! length of real array
    real(fp), dimension(n), intent(inout) :: a      ! real array to be accumulated
    logical               , intent(out)   :: error  ! error flag
    character(*)          , intent(out)   :: msgstr ! string to pass message
    !
    if (parll) then
        call dfreduce ( a, n, dfloat, dfsum, error, msgstr )
    else
        error = .false.
        msgstr = ' '
        call dd_dredgecommunicate(a,n)
    endif
end subroutine dredgecommunicate
