subroutine rddredge(xcor      ,ycor      ,xz        ,yz        ,gsqs      , &
                  & mmax      ,nmax      ,nmaxus    ,nmmax     ,lsedtot   , &
                  & kcs       ,gdp       )
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
!  $Id: rddredge.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/input/rddredge.f90 $
!!--description-----------------------------------------------------------------
!
! Reads Dredge and Dump input file.
! Allocates related arrays.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use properties
    use flow_tables
    use polygon_module
    use m_alloc
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    type (handletype)                , pointer :: tseriesfile
    real(fp)      , dimension(:,:)   , pointer :: link_percentage
    real(fp)      , dimension(:)     , pointer :: link_distance
    real(fp)      , dimension(:,:)   , pointer :: link_sum
    real(fp)      , dimension(:)     , pointer :: dzdred
    real(fp)      , dimension(:)     , pointer :: refplane
    real(fp)      , dimension(:,:)   , pointer :: voldred
    real(fp)      , dimension(:)     , pointer :: voldune
    real(fp)      , dimension(:)     , pointer :: totvoldred
    real(fp)      , dimension(:)     , pointer :: globalareadred
    real(fp)      , dimension(:,:)   , pointer :: voldump
    real(fp)      , dimension(:,:)   , pointer :: percsupl
    real(fp)      , dimension(:)     , pointer :: totvoldump
    real(fp)      , dimension(:)     , pointer :: localareadump
    real(fp)      , dimension(:)     , pointer :: globalareadump
    real(fp)      , dimension(:)     , pointer :: globaldumpcap
    integer                          , pointer :: nadred
    integer                          , pointer :: nadump
    integer                          , pointer :: nasupl
    integer                          , pointer :: nalink
    integer       , dimension(:,:)   , pointer :: link_def
    integer       , dimension(:)     , pointer :: ndredged
    integer       , dimension(:)     , pointer :: nploughed
    logical                          , pointer :: tsmortime
    character(256)                   , pointer :: dredgefile
    character( 80), dimension(:)     , pointer :: dredge_areas
    character( 80), dimension(:)     , pointer :: dump_areas
    type (dredtype), dimension(:)    , pointer :: dredge_prop
    type (dumptype), dimension(:)    , pointer :: dump_prop
    integer                          , pointer :: lundia
    character(20) , dimension(:)     , pointer :: namsed
    real(fp)                         , pointer :: dt
    integer                          , pointer :: julday
    logical                          , pointer :: lfbedfrm
    logical                          , pointer :: cmpupd
!
! Global variables
!
    integer                                     , intent(in)  :: nmax    !  Description and declaration in esm_alloc_int.f90
    integer                                     , intent(in)  :: nmaxus  !  Description and declaration in esm_alloc_int.f90
    integer                                     , intent(in)  :: nmmax   !  Description and declaration in esm_alloc_int.f90
    integer                                     , intent(in)  :: mmax    !  Description and declaration in esm_alloc_int.f90
    integer                                     , intent(in)  :: lsedtot !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: kcs     !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: gsqs    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: xcor    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: ycor    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: xz      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: yz      !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                                 :: cntdred
    integer                                 :: cntdump
    integer                                 :: cntssrc
    integer                                 :: cntsupl
    integer                                 :: cntlink
    integer                                 :: cntsedidx
    integer             , dimension(4)      :: def_active
    integer                                 :: def_chkloc
    integer                                 :: def_depthdef
    integer                                 :: def_dredgedistr
    integer                                 :: def_dumpdistr
    integer                                 :: def_dr2dudistr
    integer                                 :: def_drtrigger
    integer                                 :: i
    integer                                 :: ia
    integer                                 :: ic
    integer                                 :: inm
    integer                                 :: icdr
    integer                                 :: icdu
    integer                                 :: ilink             ! first link for dredge location
    integer                                 :: istat
    integer                                 :: iter
    integer                                 :: j
    integer                                 :: j2
    integer                                 :: lsed
    integer                                 :: nm
    integer                                 :: nmaxddb
    integer                                 :: nmcor
    integer                                 :: noutletlinks
    integer                                 :: npnt
    integer, allocatable, dimension(:)      :: imask
    integer, allocatable, dimension(:)      :: ipdr
    integer, pointer    , dimension(:)      :: ipdu
    integer, allocatable, dimension(:)      :: npdr
    integer, pointer    , dimension(:)      :: npdu
    integer             , dimension(4)      :: tmp_active
    integer                                 :: totnpdr           ! total number of points in dredge polygons
    integer                                 :: totnpdu           ! total number of points in dump   polygons
    real(fp)                                :: def_clearance
    real(fp)                                :: sumperc
    real(fp), allocatable, dimension(:)     :: xdr
    real(fp), allocatable, dimension(:)     :: xdu
    real(fp)             , dimension(2)     :: x1
    real(fp), allocatable, dimension(:)     :: ydr
    real(fp), allocatable, dimension(:)     :: ydu
    real(fp)             , dimension(2)     :: y1
    real(fp)                                :: loctemp
    real(fp)                                :: sedperc
    real(fp)                                :: def_dredge_depth
    real(fp)                                :: def_maxvolrate
    real(fp)                                :: def_mindumpdepth
    real(fp)                                :: def_alpha_dh
    real(fp)                                :: def_plough_effic
    real(fp)                                :: rmissval
    real(sp)                                :: versionnr
    real(sp)                                :: versionnrinput
    logical                                 :: ex
    logical                                 :: error
    logical, external                       :: stringsequalinsens
    logical                                 :: success
    logical                                 :: unique
    logical                                 :: def_dredgewhendry
    logical                                 :: def_dumpwhendry
    logical                                 :: def_if_morfac_0
    logical                                 :: def_obey_cmp
    logical                                 :: def_use_dunes
    logical                                 :: lastdumparea
    logical                                 :: sfound
    logical                                 :: triggerall
    character(11)                           :: fmttmp            ! Format file ('formatted  ') 
    character(80)                           :: name
    character(80)                           :: parname
    character(80)                           :: dredgetype
    character(20)                           :: sedname
    character(1024)                         :: message
    character(20)                           :: areatp
    character(256)                          :: filename
    character(256)                          :: polygonfile
    character(256)                          :: refplanefile
    character(80)                           :: stringval
    type(tree_data), pointer                :: dad_ptr
    type(tree_data), pointer                :: dredge_area_ptr
    type(tree_data), pointer                :: dump_area_ptr
    type(tree_data), pointer                :: link_ptr
    type(tree_data), pointer                :: node_ptr
    type(tree_data), pointer                :: pol_ptr
    type(dredtype) , pointer                :: pdredge
    type(dumptype) , pointer                :: pdump
!
!! executable statements -------------------------------------------------------
!
    tseriesfile       => gdp%gddredge%tseriesfile
    nadred            => gdp%gddredge%nadred
    nadump            => gdp%gddredge%nadump
    nasupl            => gdp%gddredge%nasupl
    nalink            => gdp%gddredge%nalink
    tsmortime         => gdp%gddredge%tsmortime
    dredgefile        => gdp%gddredge%dredgefile
    lundia            => gdp%gdinout%lundia
    namsed            => gdp%gdsedpar%namsed
    dt                => gdp%gdexttim%dt
    julday            => gdp%gdinttim%julday
    lfbedfrm          => gdp%gdbedformpar%lfbedfrm
    cmpupd            => gdp%gdmorpar%cmpupd
    !
    if (.not.cmpupd) then
       call prterr(lundia, 'P004', 'Dredging and dumping not supported when bed composition updating is disabled')
       call d3stop(1, gdp)
    endif
    !
    rmissval       = -9.99E9_fp
    nmaxddb        = nmax + 2*gdp%d%ddbound
    !
    versionnr      = 1.02_sp
    loctemp        = 0.0_fp
    call tree_create_node( gdp%input_tree, 'Dredge and Dump', dad_ptr )
    call tree_put_data( dad_ptr, transfer(trim(dredgefile),node_value), 'STRING' )
    !
    ! Put dad-file in input tree
    !
    call prop_file('ini', trim(dredgefile), dad_ptr, istat)
    if (istat /= 0) then
       select case (istat)
       case(1)
          call prterr(lundia, 'G004', dredgefile)
       case(3)
          call prterr(lundia, 'G006', dredgefile)
       case default
          call prterr(lundia, 'G007', dredgefile)
       endselect
       call d3stop(1, gdp)
    endif
    !
    ! Put polygon file in input tree
    !
    write (lundia,*)
    write (lundia,'(a)') '*** Start of Dredging & dumping input'
    write (lundia,'(a)') 'General'
    !
    filename = ''
    call prop_get_string(dad_ptr, 'General', 'PolygonFile', filename)
    nullify(pol_ptr)
    if (filename /= ' ') then
       call tree_get_node_by_name( dad_ptr, 'General', node_ptr )
       if (associated(node_ptr)) then
          call tree_get_node_by_name( node_ptr, 'PolygonFile', pol_ptr )
       endif
    else
       call tree_get_node_by_name( dad_ptr, 'DredgeFileInformation', node_ptr )
       if (associated(node_ptr)) then
          call tree_get_node_by_name( node_ptr, 'PolygonFile', pol_ptr )
       endif
    endif
    if (associated(pol_ptr)) then
       call tree_get_data_string(pol_ptr, polygonfile, success)
       call prop_file('tekal', polygonfile, pol_ptr, istat)
       if (istat /= 0) then
          select case (istat)
          case(1)
             call prterr(lundia, 'G004', polygonfile)
          case(3)
             call prterr(lundia, 'G006', polygonfile)
          case default
             call prterr(lundia, 'G007', polygonfile)
          endselect
          call d3stop(1, gdp)
       endif
     else
       call prterr(lundia, 'P004', 'Unable to find keyword "Polygon File" in dad-file')
       call d3stop(1, gdp)
    endif
    !
    ! Check version number of dredge input file
    !
    versionnrinput = 0.00_sp
    call prop_get_real(dad_ptr, 'DredgeFileInformation', 'FileVersion', versionnrinput)
    !
    if (comparereal(versionnrinput, versionnr) == -1) then
       write (message,'(a,f6.2,a)') 'Dredge input file must have version number ',versionnr, ' or higher'
       call prterr(lundia, 'U021', trim(message))
       call d3stop(1, gdp)
    endif
    !
    def_depthdef      = DEPTHDEF_REFPLANE
    def_dredgedistr   = DREDGEDISTR_PROPORTIONAL
    def_dumpdistr     = DUMPDISTR_UNIFORM
    def_dr2dudistr    = DR2DUDISTR_PERCENTAGE
    def_chkloc        = CHKLOC_CENTRE
    def_clearance     = 0.0_fp
    def_dredgewhendry = .false.
    def_dumpwhendry   = .false.
    def_if_morfac_0   = .true.
    def_obey_cmp      = .true.
    def_drtrigger     = DREDGETRIG_POINTBYPOINT
    def_dredge_depth  = 1.0e10_fp
    def_maxvolrate    = -999.0_fp
    def_mindumpdepth  = -999.0_fp
    def_use_dunes     = .false.
    def_alpha_dh      = 0.5_fp
    def_plough_effic  = 0.0_fp
    !
    ! Allocate array for refplane
    !
    allocate (gdp%gddredge%refplane (gdp%d%nmlb:gdp%d%nmub), stat = istat)
    if (istat /= 0) then
       call prterr(lundia, 'U021', 'RdDredge: memory alloc error')
       call d3stop(1, gdp)
    endif
    !
    ! update local pointers
    !
    refplane          => gdp%gddredge%refplane
    !
    refplane    = 0.0_fp
    refplane(1) = rmissval
    !
    filename    = ' '
    call prop_get_string(dad_ptr, 'General', 'TimeSeries', filename)
    !
    ! Intel 7.0 crashes on an inquire statement when file = ' '
    !
    if (filename == ' ') filename = 'dummyname'
    inquire (file = trim(filename), exist = ex)
    if (ex) then
       call flw_readtable(tseriesfile, trim(filename), julday,gdp)
    elseif (filename /= 'dummyname') then
       call prterr(lundia, 'U021', 'Missing time series file "'//trim(filename)//'"')
       call d3stop(1, gdp)
    endif
    !
    call prop_get(dad_ptr, 'General', 'DepthDef', def_depthdef)
    if (def_depthdef < 1 .or. def_depthdef > DEPTHDEF_MAX) then
       call prterr(lundia, 'U021', 'Invalid default depth definition')
       call d3stop(1, gdp)
    endif
    call prop_get_logical(dad_ptr, 'General', 'TS_MorTimeScale', tsmortime)
    call prop_get(dad_ptr, 'General', 'DredgeDepth', def_dredge_depth)
    call prop_get(dad_ptr, 'General', 'Clearance'  , def_clearance)
    call prop_get(dad_ptr, 'General', 'MaxVolRate' , def_maxvolrate)
    call prop_get_integer(dad_ptr, 'General', 'InPolygon', def_chkloc)
    if (def_chkloc < 1 .or. def_chkloc > 3) then
       call prterr(lundia, 'U021', 'Invalid default for in polygon check')
       call d3stop(1, gdp)
    endif
    call prop_get_integer(dad_ptr, 'General', 'DredgeDistr', def_dredgedistr)
    if (def_dredgedistr < 1 .or. def_dredgedistr > DREDGEDISTR_MAX) then
       call prterr(lundia, 'U021', 'Invalid default dredge distribution')
       call d3stop(1, gdp)
    endif
    call prop_get_logical(dad_ptr, 'General', 'UseDunes'        , def_use_dunes)
    if (def_use_dunes .and. .not. lfbedfrm) then
       call prterr(lundia, 'U021', 'UseDunes: Dunes can only be used when modelled.')
       call d3stop(1, gdp)
    endif
    call prop_get(dad_ptr, 'General', 'AlphaDuneHeight', def_alpha_dh)
    call prop_get(dad_ptr, 'General', 'PloughEfficiency', def_plough_effic)
    call prop_get_integer(dad_ptr, 'General', 'DumpDistr', def_dumpdistr)
    if (def_dumpdistr < 1 .or. def_dumpdistr > DUMPDISTR_MAX) then
       call prterr(lundia, 'U021', 'Invalid default dump distribution')
       call d3stop(1, gdp)
    endif
    call prop_get_integer(dad_ptr, 'General', 'DistrOverDump', def_dr2dudistr)
    if (def_dr2dudistr < 1 .or. def_dr2dudistr > DR2DUDISTR_MAX) then
       call prterr(lundia, 'U021', 'Invalid default distribution over dump areas')
       call d3stop(1, gdp)
    endif
    !
    def_active    = 0
    def_active(1) = -999
    if (validtable(tseriesfile)) then
       call flw_gettable(tseriesfile, 'General'    , 'active', def_active(1), &
                       & def_active(2), def_active(3), 0, gdp)
       if (def_active(3) > 1) then
          write(message,'(i3,3a)') def_active(3), &
                                 & ' active parameters specified in file "', &
                                 & trim(getfilename(tseriesfile)), '" instead of 1.'
          call prterr(lundia, 'U021', message)
          call d3stop(1, gdp)
       elseif (def_active(3) == 1) then
          call flw_checktable(tseriesfile   , def_active(1) , &
                            & def_active(2) , def_active(3) , &
                            & CHKTAB_LOGICAL, gdp           )
          def_active(4) = 1
       endif
    endif
    !
    call prop_get        (dad_ptr, 'General', 'MinimumDumpDepth', def_mindumpdepth)
    call prop_get_logical(dad_ptr, 'General', 'DredgeWhenDry'   , def_dredgewhendry)
    call prop_get_logical(dad_ptr, 'General', 'DumpWhenDry'     , def_dumpwhendry)
    call prop_get_logical(dad_ptr, 'General', 'ObeyCmp'           , def_obey_cmp)
    triggerall = .false.
    call prop_get_logical(dad_ptr, 'General', 'TriggerAll'        , triggerall)
    if (triggerall) then
       def_drtrigger = DREDGETRIG_ALLBYONE
    else
       def_drtrigger = DREDGETRIG_POINTBYPOINT
    endif
    call prop_get        (dad_ptr, 'General', 'DredgeTrigger'     , def_drtrigger)
    call prop_get_logical(dad_ptr, 'General', 'DredgeWhileMorfac0', def_if_morfac_0)
    !
    sfound = .false.
    ex     = .false.
    if ( associated(dad_ptr%child_nodes) ) then
       do i = 1, size(dad_ptr%child_nodes)
          link_ptr => dad_ptr%child_nodes(i)%node_ptr
          if (tree_get_name( link_ptr ) /= 'domain') cycle
          !
          stringval = ''
          call prop_get_string(link_ptr, '*', 'Name', stringval)
          if (stringval /= gdp%runid) cycle
          !
          ! First assume that 'RefPlane' contains a filename
          ! If the file does not exist, assume that 'RefPlane' contains
          ! a uniform value (real)
          !
          refplanefile = ''
          call prop_get_string(link_ptr, '*', 'RefPlane', refplanefile)
          !
          ! Intel 7.0 crashes on an inquire statement when file = ' '
          !
          if (refplanefile == ' ') refplanefile = 'dummyname'
          inquire (file = refplanefile, exist = ex)
          if (ex) then
             sfound = .true.
          else
             refplanefile = ' '
             call prop_get(link_ptr, '*', 'RefPlane', refplane(1))
             sfound = comparereal(refplane(1),rmissval) /= 0
          endif
       enddo
    endif
    if (.not. sfound) then
       !
       ! No domain specific RefPlane input found, try General block.
       ! Check if 'RefPlane' contains a filename.
       !
       refplanefile = ''
       call prop_get_string(dad_ptr, 'General', 'RefPlane', refplanefile)
       !
       ! Intel 7.0 crashes on an inquire statement when file = ' '
       !
       if (refplanefile == ' ') refplanefile = 'dummyname'
       inquire (file = refplanefile, exist = ex)
    endif
    if (ex) then
       !
       ! Space varying data has been specified
       !
       write(lundia,'(2a)') '  Reference plane             : ', trim(refplanefile)
       !
       fmttmp = 'formatted'
       error  = .false.
       call depfil(lundia    ,error     ,refplanefile,fmttmp  , &
                 & refplane  ,1         ,1         ,gdp%griddim)
       if (error) call d3stop(1, gdp)
    else
       if (.not. sfound) then
          !
          ! No domain specific RefPlane input found, and no filename in the General block.
          ! Now consider the case that the General block contains a uniform value.
          !
          refplanefile = ' '
          call prop_get(dad_ptr, 'General', 'RefPlane', refplane(1))
       endif
       if (comparereal(refplane(1),rmissval) == 0) then
          !
          ! RefPlane keyword not found (neither in domain specific block, nor in General block)
          ! Let's use a default RefPlane = 0
          !
          refplane(1) = 0.0_fp
       endif
       do nm = 2, nmmax
          refplane(nm) = refplane(1)
       enddo
       write(lundia,'(a,es12.3e3)') '  Reference plane             : ', refplane(1)
    endif
    !
    ! Read dimensions from input file
    ! Add dredgid's and dumpid's to the polygons in the input_tree
    ! They are used during reading
    !
    nadred  = 0
    nadump  = 0
    nasupl  = 0
    nalink  = 0
    totnpdr = 0
    totnpdu = 0
    !
    if ( associated(dad_ptr%child_nodes) ) then
       !
       ! Unfortunately, almost the complete input tree must be scanned, just
       ! to get the dimensions.
       !
       do i = 1, size(dad_ptr%child_nodes)
          link_ptr => dad_ptr%child_nodes(i)%node_ptr
          dredgetype = tree_get_name( link_ptr )
          !
          ! Distinguish the cases:
          ! - 'dredge'
          ! - 'sandmining'
          !
          select case ( dredgetype )
          case ('dredge', 'sandmining')
             !
             ! Dredge or sandmining area specification found - name must be unique
             ! nadred incremented by register_polygon call
             !
             areatp  = 'dredge'
             unique  = .true.
             call prop_get_string(link_ptr, '*', 'name', name)
             call register_polygon(name   , pol_ptr, nadred, totnpdr, &
                                 & areatp , unique, gdp    )
             if ( associated(link_ptr%child_nodes) ) then
                areatp = 'dump'
                unique = .false.
                do j = 1, size(link_ptr%child_nodes)
                   !
                   ! Does link_ptr contain one or more children with name 'Dump'?
                   ! nadump incremented by register_polygon call
                   !
                   node_ptr => link_ptr%child_nodes(j)%node_ptr
                   parname = tree_get_name( node_ptr )
                   if (parname == 'dump') then
                      call prop_get_string(node_ptr, '*', 'dump', name)
                      call register_polygon(name   , pol_ptr, nadump, totnpdu, &
                                          & areatp , unique, gdp    )
                      nalink = nalink + 1
                   endif
                enddo
             endif
          case ('nourishment')
             !
             ! Nourishment specification found
             !
             nasupl = nasupl + 1        
             if ( associated(link_ptr%child_nodes) ) then
                areatp = 'dump'
                unique = .false.
                do j = 1, size(link_ptr%child_nodes)
                   !
                   ! Does link_ptr contain one or more children with name 'Dump'?
                   !
                   node_ptr => link_ptr%child_nodes(j)%node_ptr
                   parname = tree_get_name( node_ptr )
                   if (parname == 'dump') then
                      call prop_get_string(link_ptr, '*', 'dump', name)
                      call register_polygon(name   , pol_ptr, nadump, totnpdu, &
                                          & areatp , unique, gdp    )
                      nalink = nalink + 1
                   endif
                enddo
             endif
          case default
             !
             ! Ignore anything else
             !
          end select
       enddo
    endif
    !
    ! Allocate arrays used during computation
    !
                  allocate (gdp%gddredge%link_def        (nalink               ,2        ), stat = istat)
    if (istat==0) allocate (gdp%gddredge%ndredged        (nadred+nasupl                  ), stat = istat)
    if (istat==0) allocate (gdp%gddredge%nploughed       (nadred+nasupl                  ), stat = istat)
    !
    if (istat==0) allocate (gdp%gddredge%link_percentage (nalink               ,lsedtot  ), stat = istat)
    if (istat==0) allocate (gdp%gddredge%link_distance   (nalink                         ), stat = istat)
    if (istat==0) allocate (gdp%gddredge%link_sum        (nalink               ,lsedtot  ), stat = istat)
    if (istat==0) allocate (gdp%gddredge%dzdred          (gdp%d%nmlb:gdp%d%nmub          ), stat = istat)
    if (istat==0) allocate (gdp%gddredge%voldred         (nadred+nasupl        ,lsedtot+1), stat = istat)
    if (istat==0) allocate (gdp%gddredge%totvoldred      (nadred+nasupl                  ), stat = istat)
    if (istat==0) allocate (gdp%gddredge%globalareadred  (nadred+nasupl                  ), stat = istat)
    if (istat==0) allocate (gdp%gddredge%voldune         (gdp%d%nmlb:gdp%d%nmub          ), stat = istat)
    if (istat==0) allocate (gdp%gddredge%percsupl        (nasupl               ,lsedtot  ), stat = istat)
    if (istat==0) allocate (gdp%gddredge%totvoldump      (nadump                         ), stat = istat)
    if (istat==0) allocate (gdp%gddredge%localareadump   (nadump                         ), stat = istat)
    if (istat==0) allocate (gdp%gddredge%globalareadump  (nadump                         ), stat = istat)
    if (istat==0) allocate (gdp%gddredge%globaldumpcap   (nadump                         ), stat = istat)
    if (istat==0) allocate (gdp%gddredge%voldump         (nadump               ,lsedtot  ), stat = istat)
    !
    if (istat==0) allocate (gdp%gddredge%dredge_areas    (nadred+nasupl                  ), stat = istat)
    if (istat==0) allocate (gdp%gddredge%dump_areas      (nadump                         ), stat = istat)
    !
    if (istat==0) allocate (gdp%gddredge%dredge_prop     (nadred+nasupl                  ), stat = istat)
    if (istat==0) allocate (gdp%gddredge%dump_prop       (nadump                         ), stat = istat)
    if (istat/=0) then
       call prterr(lundia, 'U021', 'RdDredge: memory alloc error')
       call d3stop(1, gdp)
    endif
    !
    ! update local pointers
    !
    link_def          => gdp%gddredge%link_def
    ndredged          => gdp%gddredge%ndredged
    nploughed         => gdp%gddredge%nploughed
    !
    link_percentage   => gdp%gddredge%link_percentage
    link_distance     => gdp%gddredge%link_distance
    link_sum          => gdp%gddredge%link_sum
    dzdred            => gdp%gddredge%dzdred
    voldred           => gdp%gddredge%voldred
    totvoldred        => gdp%gddredge%totvoldred
    globalareadred    => gdp%gddredge%globalareadred
    voldune           => gdp%gddredge%voldune
    percsupl          => gdp%gddredge%percsupl
    totvoldump        => gdp%gddredge%totvoldump
    localareadump     => gdp%gddredge%localareadump
    globalareadump    => gdp%gddredge%globalareadump
    globaldumpcap     => gdp%gddredge%globaldumpcap
    voldump           => gdp%gddredge%voldump
    !
    dredge_areas      => gdp%gddredge%dredge_areas
    dump_areas        => gdp%gddredge%dump_areas
    !
    dredge_prop       => gdp%gddredge%dredge_prop
    dump_prop         => gdp%gddredge%dump_prop
    !
    ! Allocate arrays used for detecting dredge, dump points
    !
                  allocate (imask(gdp%d%nmlb:gdp%d%nmub), stat = istat)
    !
    if (istat==0) allocate (ipdr(nadred), stat = istat)
    if (istat==0) allocate (ipdu(nadump), stat = istat)
    if (istat==0) allocate (npdr(nadred), stat = istat)
    if (istat==0) allocate (npdu(nadump), stat = istat)
    !
    if (istat==0) allocate (xdr(totnpdr), stat = istat)
    if (istat==0) allocate (xdu(totnpdu), stat = istat)
    if (istat==0) allocate (ydr(totnpdr), stat = istat)
    if (istat==0) allocate (ydu(totnpdu), stat = istat)
    if (istat/=0) then
       call prterr(lundia, 'U021', 'RdDredge: memory alloc error')
       call d3stop(1, gdp)
    endif
    !
    ! necessary initializations
    !
    link_def        = 0
    ndredged        = 0
    nploughed       = 0
    !
    link_percentage = 0.0_fp
    link_distance   = 0.0_fp
    link_sum        = 0.0_fp
    dzdred          = 0.0_fp
    voldred         = 0.0_fp
    totvoldred      = 0.0_fp
    globalareadred  = 0.0_fp
    voldune         = 0.0_fp
    percsupl        = 0.0_fp
    totvoldump      = 0.0_fp
    localareadump   = 0.0_fp
    globalareadump  = 0.0_fp
    globaldumpcap   = 0.0_fp
    voldump         = 0.0_fp
    !
    dredge_areas    = ' '
    dump_areas      = ' '
    !
    ipdr            = -999
    ipdu            = -999
    npdr            = -999
    npdu            = -999
    !
    write (lundia,'(a,i0)') '  Number of dredging areas    : ', nadred
    write (lundia,'(a,i0)') '  Number of dump areas        : ', nadump
    write (lundia,'(a,i0)') '  Number of nourishment areas : ', nasupl
    !
    ! Finally the input can be read
    !
    cntdred = 0
    cntdump = 0
    cntsupl = 0
    cntssrc = 0
    cntlink = 0
    icdr    = 1
    icdu    = 1
    !
    do iter = 1, 2
       if ( associated(dad_ptr%child_nodes) ) then
          do i = 1, size(dad_ptr%child_nodes)
             link_ptr => dad_ptr%child_nodes(i)%node_ptr
             dredgetype = tree_get_name( link_ptr )
             !
             ! Distinguish the cases:
             ! - 'dredge'
             ! - 'sandmining'
             ! to obtain the associated parameter values
             !
             ! sandmining cases should be processed first because in case of
             ! overlapping sandmining and dredging areas, continuous sandmining
             ! activities should prevail over conditional dredging.
             !
             if ((dredgetype == 'dredge') .or. (dredgetype == 'nourishment')) then
                if (iter == 1) cycle
             else
                if (iter /= 1) cycle
             endif
             !
             select case ( dredgetype )
             case ('dredge', 'sandmining')
                !
                ! Dredge or sandmining area specification found
                !
                cntdred = cntdred + 1
                cntssrc = cntssrc + 1
                pdredge => dredge_prop(cntssrc)
                !
                ! Initialize
                !
                pdredge%idx_type      = cntdred
                pdredge%ichkloc       = def_chkloc
                pdredge%paractive     = def_active
                pdredge%depthdef      = def_depthdef
                pdredge%dredge_depth  = def_dredge_depth
                pdredge%maxvolrate    = def_maxvolrate
                pdredge%clearance     = def_clearance
                pdredge%stilldredging = .false.
                pdredge%dredgewhendry = def_dredgewhendry
                pdredge%dumplimited   = .false.
                pdredge%in1domain     = .false.
                pdredge%if_morfac_0   = def_if_morfac_0
                pdredge%obey_cmp      = def_obey_cmp
                pdredge%triggertype   = def_drtrigger
                pdredge%dredgedistr   = def_dredgedistr
                pdredge%dumpdistr     = def_dr2dudistr
                pdredge%totalvolsupl  = 0.0_fp
                pdredge%outletlink    = 0
                pdredge%npnt          = 0
                pdredge%use_dunes     = def_use_dunes
                pdredge%alpha_dh      = def_alpha_dh
                pdredge%plough_effic  = def_plough_effic
                nullify(pdredge%nm)
                nullify(pdredge%inm)
                nullify(pdredge%area)
                nullify(pdredge%hdune)
                nullify(pdredge%dz_dredge)
                nullify(pdredge%reflevel)
                nullify(pdredge%dunetoplevel)
                nullify(pdredge%triggerlevel)
                nullify(pdredge%bedlevel)
                nullify(pdredge%troughlevel)
                nullify(pdredge%sedimentdepth)
                nullify(pdredge%sortvar)
                nullify(pdredge%triggered)
                !
                ! Set dredge area name
                !
                dredge_areas(cntssrc) = ''
                call prop_get_string(link_ptr, '*', 'Name', dredge_areas(cntssrc))
                pdredge%name = dredge_areas(cntssrc)

                write(lundia, '(a,i0)')      'Dredge definition number      : ', cntdred
                write(lundia, '(a)')         '  Dredge area                 : ' // trim(dredge_areas(cntssrc))
                !
                ! Read dredging parameters
                !
                tmp_active = 0
                if (validtable(tseriesfile)) then
                   call flw_gettable(tseriesfile  , pdredge%name , 'active'     , &
                                   & tmp_active(1), tmp_active(2), tmp_active(3), 0, gdp)
                   if (tmp_active(3) == 0) then
                      tmp_active = def_active
                   elseif (tmp_active(3) > 1) then
                      write(message,'(i3,3a)') tmp_active(3), &
                                             & ' active parameters specified in file "', &
                                             & trim(getfilename(tseriesfile)), '" instead of 1.'
                      call prterr(lundia, 'U021', message)
                      call d3stop(1, gdp)
                   else
                      call flw_checktable(tseriesfile   , tmp_active(1), &
                                        & tmp_active(2) , tmp_active(3), &
                                        & CHKTAB_LOGICAL, gdp          )
                      tmp_active(4) = 1
                   endif
                   pdredge%paractive = tmp_active
                endif
                !
                call prop_get(link_ptr, '*', 'DredgeDepth', pdredge%dredge_depth)
                call prop_get(link_ptr, '*', 'Volume'     , pdredge%maxvolrate)
                call prop_get(link_ptr, '*', 'MaxVolRate' , pdredge%maxvolrate)
                !
                if (comparereal(pdredge%dredge_depth,1.0e10_fp) == 0 .and. &
                  & comparereal(pdredge%maxvolrate  ,-999.0_fp) == 0) then
                   if (parname == 'sandmining') then
                      call prterr(lundia, 'U021', &
                                & 'Unable to read sand mining area "'// &
                                & trim(dredge_areas(cntssrc))//'"')
                   else
                      call prterr(lundia, 'U021', &
                                & 'Unable to read dredge depth of area "'// &
                                & trim(dredge_areas(cntssrc))//'"')
                   endif
                   call d3stop(1, gdp)
                elseif (comparereal(pdredge%dredge_depth,1.0e10_fp) == 0) then
                   !
                   ! Fixed rate dredging, previously known as sandmining
                   !
                   pdredge%itype       = DREDGETYPE_SANDMINING
                   pdredge%dredgedistr = 1
                   write(lundia,'(a,es12.3e3,a)') '  Dredging rate               : ', pdredge%maxvolrate,' m^3/y (including pores)'
                else
                   !
                   ! Dredging to specified depth (with optional maximum rate)
                   !
                   pdredge%itype = DREDGETYPE_DREDGING
                   write(lundia,'(a,es12.3e3,a)') '  Dredge depth                : ', pdredge%dredge_depth,' m'
                   if (comparereal(pdredge%maxvolrate  ,-999.0_fp) /= 0) then
                      write(lundia,'(a,es12.3e3,a)') '  Maximum dredging rate       : ', pdredge%maxvolrate,' m^3/y (including pores)'
                   endif
                endif

                if (.not. (comparereal(pdredge%maxvolrate,-999.0_fp) == 0)) then
                    pdredge%maxvolrate = pdredge%maxvolrate/yearsec_hp
                endif
!                write(lundia,'(a,f8.3)') 'maxvolrate ',pdredge%maxvolrate
                                !
                call prop_get(link_ptr, '*', 'DepthDef', pdredge%depthdef)
                if (pdredge%depthdef < 1 .or. pdredge%depthdef > DEPTHDEF_MAX) then
                   call prterr(lundia, 'U021', &
                             & 'Invalid depth definition for dredge area "'// &
                             & trim(dredge_areas(cntssrc))//'"')
                   call d3stop(1, gdp)
                endif
                call prop_get_integer(link_ptr, '*', 'InPolygon', pdredge%ichkloc)
                if (pdredge%ichkloc < 1 .or. pdredge%ichkloc > 3) then
                   call prterr(lundia, 'U021', &
                             & 'Invalid in polygon check for dredge area "'// &
                             & trim(dredge_areas(cntssrc))//'"')
                   call d3stop(1, gdp)
                endif
                call prop_get_integer(link_ptr, '*', 'DredgeDistr', pdredge%dredgedistr)
                if (pdredge%dredgedistr < 1 .or. pdredge%dredgedistr > DREDGEDISTR_MAX) then
                   call prterr(lundia, 'U021', &
                             & 'Invalid dredge distribution for dredge area "'// &
                             & trim(dredge_areas(cntssrc))//'"')
                   call d3stop(1, gdp)
                endif
                if (pdredge%paractive(1) /= -999) then
                   write(lundia,'(a)')   '  Dredging active             : during intervals'
                else
                   write(lundia,'(a)')   '  Dredging active             : always'
                endif
                !
                select case (pdredge%dredgedistr)
                case (DREDGEDISTR_UNIFORM)
                   stringval = 'uniform'
                case (DREDGEDISTR_HIGHEST)
                   stringval = 'highest'
                case (DREDGEDISTR_PROPORTIONAL)
                   stringval = 'proportional'
                case (DREDGEDISTR_HIGHFIRST)
                   stringval = 'high first'
                case (DREDGEDISTR_LOWFIRST)
                   stringval = 'low first'
                case (DREDGEDISTR_SHALLOWEST)
                   stringval = 'shallowest'
                case (DREDGEDISTR_SHALLOWFIRST)
                   stringval = 'shallow first'
                case (DREDGEDISTR_DEEPFIRST)
                   stringval = 'deep first'
                end select
                write(lundia,'(a,i0,a)') '  Dredge distribution         : ', pdredge%dredgedistr,' ('//trim(stringval)//')'
                !
                select case (pdredge%depthdef)
                case (DEPTHDEF_REFPLANE)
                   stringval = 'reference plane'
                case (DEPTHDEF_WATERLVL)
                   stringval = 'water level'
                case (DEPTHDEF_MAXREFWL)
                   stringval = 'maximum(reference plane,water level)'
                case (DEPTHDEF_MINREFWL)
                   stringval = 'minimum(reference plane,water level)'
                end select
                write(lundia,'(a,i0,a)')     '  Depth definition            : ',pdredge%depthdef,' (relative to '//trim(stringval)//')'
                !
                call prop_get(link_ptr, '*', 'Clearance'    , pdredge%clearance)
                call prop_get(link_ptr, '*', 'DredgeWhenDry', pdredge%dredgewhendry)
                call prop_get(link_ptr, '*', 'DumpLimited'  , pdredge%dumplimited)
                if (pdredge%maxvolrate < 0.0_fp) then
                   !
                   ! DredgeWhileMorfac0 can only be .true. if dredging is instantaneous.
                   !
                   call prop_get(link_ptr, '*', 'DredgeWhileMorfac0', pdredge%if_morfac_0)
                else
                   pdredge%if_morfac_0 = .false.
                endif
                call prop_get(link_ptr, '*', 'ObeyCmp'    , pdredge%obey_cmp)
                triggerall = .false.
                call prop_get(link_ptr, '*', 'TriggerAll' , triggerall)
                if (triggerall) then
                   ! TriggerAll = #YES# was explicitly specified
                   pdredge%triggertype = DREDGETRIG_ALLBYONE
                else
                   ! triggerall may be false because it was specified or just because we set the default to false.
                   ! we need to distinguish, so let's change the default setting
                   triggerall = .true.
                   call prop_get(link_ptr, '*', 'TriggerAll' , triggerall)
                   if (.not.triggerall) then
                      ! now we know that TriggerAll = #NO# was explicitly specified
                      pdredge%triggertype = DREDGETRIG_POINTBYPOINT
                   endif
                endif
                call prop_get(link_ptr, '*', 'DredgeTrigger', pdredge%triggertype)
                call prop_get(link_ptr, '*', 'UseDunes'     , pdredge%use_dunes)
                if (pdredge%use_dunes .and. .not. lfbedfrm) then
                   call prterr(lundia, 'U021', 'UseDunes: Dunes can only be used when modelled.')
                   call d3stop(1, gdp)
                endif
                if (pdredge%use_dunes) then
                   call prop_get(link_ptr, '*', 'AlphaDuneHeight', pdredge%alpha_dh)
                   if ((pdredge%alpha_dh > 0.5_fp) .or. (pdredge%alpha_dh < 0.0_fp)) then
                      call prterr(lundia, 'U021', 'AlphaDuneHeight should be a real number between 0.0 and 0.5')
                      call d3stop(1, gdp)
                   endif
                   call prop_get(link_ptr, '*', 'PloughEfficiency', pdredge%plough_effic)
                   if ((pdredge%plough_effic > 1.0_fp) .or. (pdredge%plough_effic < 0.0_fp)) then
                      call prterr(lundia, 'U021', 'PloughEfficiency should be a real number between 0.0 and 1.0')
                      call d3stop(1, gdp)
                   endif
                endif
                !
                ! Read the coordinates of the corresponding polygon
                !
                call tree_get_node_by_name(pol_ptr, dredge_areas(cntssrc), dredge_area_ptr )
                areatp = 'dredge'
                call read_polygon_data(dredge_area_ptr, icdr, ipdr(cntdred), npdr(cntdred), &
                                     & xdr, ydr, areatp, cntdred, gdp)
                !
                ! Each dredge area may distribute to several dump areas
                ! The sum of all distribution percentages must be 100.0 for each dredge area
                ! The dumpid's, added while obtaining the dimensions, are used to uniquely
                ! identify each dump area (they may occur more than once).
                !
                ilink     = 0
                cntsedidx = 0
                if ( associated(link_ptr%child_nodes) ) then
                   do j = 1, size(link_ptr%child_nodes)
                      !
                      ! Does link_ptr contain one or more children with name 'Dump'?
                      !
                      node_ptr => link_ptr%child_nodes(j)%node_ptr
                      parname = tree_get_name( node_ptr )
                      if (parname == 'dump') then
                         cntlink = cntlink + 1
                         if (ilink == 0) ilink = cntlink
                         !
                         ! Get dump name
                         !
                         call tree_get_data_string(node_ptr, parname, success)
                         !
                         ! Get corresponding polygon
                         !
                         call tree_get_node_by_name( pol_ptr, parname, dump_area_ptr )
                         !
                         ! Get the polygon's dumpid
                         !
                         cntdump = 0
                         call prop_get_integer(dump_area_ptr, '*', 'dumpid', cntdump)
                         if (cntdump < 1) then
                            call prterr(lundia, 'U021','Invalid dump ID: '//trim(parname))
                            call d3stop(1, gdp)
                         endif
                         link_def(cntlink,1) = cntssrc
                         link_def(cntlink,2) = cntdump
                         !
                         if (dump_areas(cntdump) == parname) then
                            write(lundia,'(3a)') 'Dump area ', trim(dump_areas(cntdump)), ' has already been read.'
                            !
                            ! Do not read polygon points again
                            !
                            cycle
                         endif
                         !
                         dump_areas(cntdump) = parname
                         !
                         ! Read the coordinates of the corresponding polygon
                         !
                         call tree_get_node_by_name(pol_ptr, dump_areas(cntdump), dump_area_ptr )
                         areatp = 'dump'
                         call read_polygon_data(dump_area_ptr, icdu, ipdu(cntdump), npdu(cntdump), &
                                              & xdu, ydu, areatp, cntdump, gdp)
                      elseif (parname == 'percentage') then
                         if (ilink == 0) then
                            call prterr(lundia, 'U021', 'Unexpected percentage encountered')
                            call d3stop(1, gdp)
                         endif
                         sedperc = 0.0_fp
                         call prop_get(node_ptr, '*', 'Percentage', sedperc)
                         if (cntsedidx == 0) then
                            do lsed = 1, lsedtot
                               link_percentage(cntlink,lsed) = sedperc
                            enddo
                         else
                            link_percentage(cntlink,cntsedidx) = sedperc
                            cntsedidx = 0
                         endif
                      elseif ( parname == 'sediment') then
                         sedname = ''
                         call prop_get(node_ptr, '*', 'Sediment' , sedname)
                         sfound = .false.
                         do j2 = 1, lsedtot
                            if ( stringsequalinsens(namsed(j2), sedname) ) then
                               cntsedidx = j2
                               sfound    = .true.
                            endif
                         enddo
                         if (.not. sfound) then
                            call prterr(lundia, 'U021', 'Unknown sediment fraction "'//trim(sedname)//'"')
                            call d3stop(1, gdp)
                         endif
                      endif
                   enddo
                endif
            case ('nourishment')
                !
                ! Nourishment specification found
                !
                cntssrc = cntssrc + 1
                cntsupl = cntsupl + 1
                pdredge => dredge_prop(cntssrc)
                !
                cntsedidx = 0
                dredge_areas(cntssrc) = ''
                call prop_get_string(link_ptr, '*', 'Name', dredge_areas(cntssrc))
                if (dredge_areas(cntssrc) == ' ') then
                   write(stringval,'(a,i0)') 'nourishment ', cntsupl
                   dredge_areas(cntssrc) = stringval
                endif
                pdredge%name = dredge_areas(cntssrc)
                !
                ! Initialize
                !
                pdredge%idx_type      = cntsupl
                pdredge%paractive     = def_active
                pdredge%dredge_depth  = -999.0_fp
                pdredge%clearance     = -999.0_fp
                pdredge%stilldredging = .false.
                pdredge%dredgewhendry = .false.
                pdredge%dumplimited   = .false.
                pdredge%in1domain     = .false.
                pdredge%if_morfac_0   = .false.
                pdredge%obey_cmp      = .true.
                pdredge%triggertype   = DREDGETRIG_POINTBYPOINT
                pdredge%depthdef      = DEPTHDEF_REFPLANE
                pdredge%dredgedistr   = 0
                pdredge%dumpdistr     = def_dr2dudistr
                pdredge%outletlink    = 0
                pdredge%npnt          = 0
                pdredge%ichkloc       = 0
                nullify(pdredge%nm)
                nullify(pdredge%inm)
                nullify(pdredge%area)
                nullify(pdredge%hdune)
                nullify(pdredge%dz_dredge)
                nullify(pdredge%reflevel)
                nullify(pdredge%dunetoplevel)
                nullify(pdredge%triggerlevel)
                nullify(pdredge%bedlevel)
                nullify(pdredge%troughlevel)
                nullify(pdredge%sedimentdepth)
                nullify(pdredge%sortvar)
                nullify(pdredge%triggered)
                pdredge%maxvolrate   = def_maxvolrate
                pdredge%totalvolsupl = -999.0_fp
                !
                tmp_active = 0
                if (validtable(tseriesfile)) then
                   call flw_gettable(tseriesfile  , pdredge%name , 'active'     , &
                                   & tmp_active(1), tmp_active(2), tmp_active(3), 0, gdp)
                   if (tmp_active(3) == 0) then
                      tmp_active = def_active
                   elseif (tmp_active(3) > 1) then
                      write(message,'(i3,3a)') tmp_active(3), &
                                             & ' active parameters specified in file "', &
                                             & trim(getfilename(tseriesfile)), '" instead of 1.'
                      call prterr(lundia, 'U021', message)
                      call d3stop(1, gdp)
                   else
                      call flw_checktable(tseriesfile   , tmp_active(1), &
                                        & tmp_active(2) , tmp_active(3), &
                                        & CHKTAB_LOGICAL, gdp          )
                      tmp_active(4) = 1
                   endif
                   pdredge%paractive = tmp_active
                endif
                !
                write(lundia, '(a,i0)')        'Nourishment definition number : ', cntsupl
                write(lundia, '(2a)')          '  Name                        : ', dredge_areas(cntssrc)
                pdredge => dredge_prop(cntssrc)
                call prop_get(link_ptr, '*', 'Volume' , pdredge%totalvolsupl)
                write(lundia,'(a,es12.3e3)')   '  Total nourishment volume    : ', pdredge%totalvolsupl
                call prop_get(link_ptr, '*', 'MaxVolRate' , pdredge%maxvolrate)
                write(lundia,'(a,es12.3e3,a)')   '  Nourishment rate            : ',pdredge%maxvolrate,' m^3/y (including pores)'
                if (pdredge%paractive(1) /= -999) then
                   write(lundia,'(a)')   '  Nourishment active          : during intervals'
                else
                   write(lundia,'(a)')   '  Nourishment active          : always'
                endif
                !
                pdredge%itype = DREDGETYPE_NOURISHMENT
                if (.not. (comparereal(pdredge%maxvolrate  ,-999.0_fp) == 0)) then
                    pdredge%maxvolrate = pdredge%maxvolrate/yearsec_hp
                endif
                !
                ilink = 0
                if ( associated(link_ptr%child_nodes) ) then
                   do j = 1, size(link_ptr%child_nodes)
                      node_ptr => link_ptr%child_nodes(j)%node_ptr
                      parname = tree_get_name( node_ptr )
                      if ( parname == 'sediment') then
                         sedname = ''
                         call prop_get(node_ptr, '*', 'Sediment', sedname)
                         sfound = .false.
                         do j2 = 1, lsedtot
                            if ( stringsequalinsens(namsed(j2), sedname) ) then
                               cntsedidx = j2
                               sfound    = .true.
                            endif
                         enddo
                         if (.not. sfound) then
                            call prterr(lundia, 'U021', 'Unknown sediment fraction "'//trim(sedname)//'"')
                            call d3stop(1, gdp)
                         endif
                      elseif (parname == 'sedpercentage') then 
                         sedperc = 0.0_fp
                         call prop_get(node_ptr,'*','SedPercentage', sedperc)
                         if (cntsedidx == 0) then
                            call prterr(lundia, 'U021', 'SedPercentage without preceding Sediment keyword')
                            call d3stop(1, gdp)
                         else
                            j2 = cntsedidx
                            write(lundia,'(3a,f8.3)') '  Percentage of ', trim(namsed(j2)), ': ', sedperc
                            percsupl(cntsupl,j2) = sedperc
                            cntsedidx            = 0
                         endif
                      elseif ( parname == 'dump') then
                         cntlink = cntlink + 1
                         if (ilink == 0) ilink = cntlink
                         !
                         ! Get dump name
                         !
                         call tree_get_data_string(node_ptr, parname, success)
                         !
                         ! Get corresponding polygon
                         !
                         call tree_get_node_by_name( pol_ptr, parname, dump_area_ptr )
                         !
                         ! Get the polygon's dumpid
                         !
                         cntdump = 0
                         call prop_get_integer(dump_area_ptr, '*', 'dumpid', cntdump)
                         if (cntdump < 1) then
                            call prterr(lundia, 'U021', 'Invalid dump ID')
                            call d3stop(1, gdp)    
                         endif
                         link_def(cntlink,1) = cntssrc
                         link_def(cntlink,2) = cntdump
                         !
                         if (dump_areas(cntdump) == parname) then
                            write(lundia,'(3a)') 'Dump area ', trim(dump_areas(cntdump)), ' has already been read.'
                            !
                            ! Do not read polygon points again
                            !
                            cycle
                         endif
                         !
                         dump_areas(cntdump) = parname
                         !
                         ! Read the coordinates of the corresponding polygon
                         !
                         call tree_get_node_by_name(pol_ptr, dump_areas(cntdump), dump_area_ptr )
                         areatp = 'dump'
                         call read_polygon_data(dump_area_ptr, icdu, ipdu(cntdump), npdu(cntdump), &
                                              & xdu, ydu, areatp, cntdump, gdp)
                      elseif ( parname == 'percentage') then
                         if (ilink == 0) then
                            call prterr(lundia, 'U021', 'Unexpected percentage encountered')
                            call d3stop(1, gdp)
                         endif
                         call prop_get(node_ptr, '*', 'Percentage', link_percentage(cntlink,1))
                         do lsed = 2, lsedtot
                            link_percentage(cntlink,lsed) = link_percentage(cntlink,1)
                         enddo
                      endif 
                   enddo
                endif
                !
                write(lundia,'(a)') '  Sediment composition'
                if (lsedtot > 1) then
                   sumperc = 0.0_fp
                   do lsed = 1, lsedtot
                      if (percsupl(cntsupl,lsed) > 0.0_fp) then
                         write(lundia,'(a,f6.2,2a)') '    ', percsupl(cntsupl,lsed), &
                                                   & '% of sed. fraction  : ', trim(namsed(lsed))
                      endif
                      sumperc = sumperc + percsupl(cntsupl,lsed)
                   enddo
                   if (comparereal(100.0_fp,sumperc) /= 0) then
                      write(message,'(3a)') 'Sum of sediment fractions is not 100.0 for nourishment "', dredge_areas(cntssrc),'"'
                      call prterr(lundia, 'U021', message)
                      call d3stop(1, gdp)
                   endif
                else
                   percsupl(cntsupl,1) = 100.0_fp
                endif
             case default
                !
                ! Ignore any other child (like 'dump')
                !
             end select
             !
             ! Now verify the distribution of sediment from the dredging area,
             ! sandmining area, or nourishment to the dumping areas.
             !
             select case ( dredgetype )
             case ('dredge', 'sandmining', 'nourishment')
                call prop_get(link_ptr, '*', 'DumpDistr'    , pdredge%dumpdistr) ! old keyword still supported
                call prop_get(link_ptr, '*', 'DistrOverDump', pdredge%dumpdistr)
                if (pdredge%dumpdistr<1 .or. pdredge%dumpdistr>DR2DUDISTR_MAX) then
                   call prterr(lundia, 'U021', &
                             & 'Invalid dump distribution for '//trim(dredgetype)//' area "'// &
                             & trim(dredge_areas(cntssrc))//'"')
                   call d3stop(1, gdp)
                endif
                !
                if (ilink == 0) ilink = cntlink + 1
                sumperc = 0.0_fp
                do lsed = 1, lsedtot
                   do j = ilink, cntlink
                      sumperc = sumperc + link_percentage(j,lsed)
                   enddo
                enddo
                if (comparereal(sumperc,0.0_fp) == 0) then
                   if (ilink<=cntlink .and. pdredge%dumpdistr == DR2DUDISTR_PERCENTAGE) &
                     & pdredge%dumpdistr = DR2DUDISTR_SEQUENTIAL
                else
                   if (pdredge%dumpdistr /= DR2DUDISTR_PERCENTAGE) then
                      call prterr(lundia, 'U021', &
                                & 'Specified percentages conflict with specified dump'// &
                                & ' distribution for '//trim(dredgetype)//' area "'// &
                                & trim(dredge_areas(cntssrc))//'"')
                      call d3stop(1, gdp)
                   endif
                endif
                !
                select case (pdredge%dumpdistr)
                case (DR2DUDISTR_PERCENTAGE)
                   !
                   ! Verify that percentages sum to 100%
                   !
                   stringval = 'percentage'
                   write(lundia,'(a,i0,a)') '  Dump distribution           : ', pdredge%dumpdistr,' ('//trim(stringval)//')'
                   do lsed = 1, lsedtot
                      if (lsedtot > 1) then
                         write(lundia,'(a,a)') '  Sediment fraction           : ', trim(namsed(lsed))
                      endif
                      sumperc = 0.0_fp
                      do j = ilink, cntlink
                         if (link_percentage(j,lsed) > 0.0_fp) then
                            write(lundia,'(a,f6.2,a,a)') '    Dump ',link_percentage(j,lsed), &
                                                       & '% at           : ', trim(dump_areas(link_def(j,2)))
                         endif
                         sumperc = sumperc + link_percentage(j,lsed)
                      enddo
                      if (comparereal(100.0_fp,sumperc) /= 0 .and. pdredge%itype /= DREDGETYPE_SANDMINING) then
                         write(message,'(3a)') 'Sum of dump % of '//trim(dredgetype)//' area "', &
                                             & trim(dredge_areas(cntssrc)),'" is not equal to 100.0 '
                         call prterr(lundia, 'U021', message)
                         call d3stop(1, gdp)
                      endif
                   enddo
                case (DR2DUDISTR_SEQUENTIAL, DR2DUDISTR_PROPORTIONAL)
                   !
                   ! Verify that no percentages have been specified
                   !
                   if (pdredge%dumpdistr == DR2DUDISTR_SEQUENTIAL) then
                      stringval = 'sequential'
                   else !if (pdredge%dumpdistr == DR2DUDISTR_PROPORTIONAL) then
                      stringval = 'proportional'
                   endif
                   write(lundia,'(a,i0,a)') '  Dump distribution           : ', pdredge%dumpdistr,' ('//trim(stringval)//')'
                   !
                   do j = ilink, cntlink
                      write(lundia,'(2a)') '    Dump at                   : ', trim(dump_areas(link_def(j,2)))
                   enddo
                   !
                   ! To be checked futher down: unique relation and capacity limitation
                   !
                end select
             end select
          enddo
       endif
    enddo
    !
    do i = 1, nadump
       pdump => dump_prop(i)
       !
       ! Initialize
       !
       pdump%depthdef     = def_depthdef
       pdump%mindumpdepth = def_mindumpdepth
       pdump%dumpcapaflag = comparereal(pdump%mindumpdepth,-999.0_fp) /= 0
       pdump%dumpdistr    = def_dumpdistr
       pdump%dumpwhendry  = def_dumpwhendry
       pdump%in1domain    = .false.
       pdump%ichkloc      = def_chkloc
       pdump%use_dunes    = def_use_dunes
       pdump%npnt         = 0
       nullify(pdump%nm)
       nullify(pdump%inm)
       nullify(pdump%reflevel)
       nullify(pdump%area)
       nullify(pdump%hdune)
       nullify(pdump%bedlevel)
       nullify(pdump%dz_dump)
       nullify(pdump%sortvar)
    enddo
    !
    if ( associated(dad_ptr%child_nodes) ) then
       do i = 1, size(dad_ptr%child_nodes)
          link_ptr => dad_ptr%child_nodes(i)%node_ptr
          dredgetype = tree_get_name( link_ptr )
          !
          ! Distinguish the cases:
          ! - 'dump'
          ! to obtain the associated parameter values
          !
          select case ( dredgetype )
          case ('dump')
             !
             ! Dump area specification found
             !
             ! Get dredge area name
             !
             parname = ''
             call prop_get_string(link_ptr, '*', 'Name', parname)
             do ia = 1, nadump
                if (parname == dump_areas(ia)) exit
             enddo
             !
             if (ia > nadump) then
                call prterr(lundia, 'U190', &
                          & 'Skipping data block for unknown dump area "'// &
                          & trim(parname)//'"')
                cycle
             endif
             pdump => dump_prop(ia)
             !
             ! Read dumping parameters
             !
             call prop_get(link_ptr, '*', 'DepthDef', pdump%depthdef)
             if (pdump%depthdef<1 .or. pdump%depthdef>DEPTHDEF_MAX) then
                call prterr(lundia, 'U021', &
                          & 'Invalid depth definition for dump area "'// &
                          & trim(dump_areas(ia))//'"')
                call d3stop(1, gdp)
             endif
             call prop_get_integer(link_ptr, '*', 'DumpDistr', pdump%dumpdistr)
             if (pdump%dumpdistr<1 .or. pdump%dumpdistr>DUMPDISTR_MAX) then
                call prterr(lundia, 'U021', &
                          & 'Invalid dump distribution for dump area "'// &
                          & trim(dump_areas(ia))//'"')
                call d3stop(1, gdp)
             endif
             call prop_get_integer(link_ptr, '*', 'InPolygon', pdump%ichkloc)
             if (pdump%ichkloc<1 .or. pdump%ichkloc>3) then
                call prterr(lundia, 'U021', &
                          & 'Invalid in polygon check for dump area "'// &
                          & trim(dump_areas(ia))//'"')
                call d3stop(1, gdp)
             endif
             !
             call prop_get_logical(link_ptr, '*', 'DumpWhenDry'  , pdump%dumpwhendry)
             call prop_get(link_ptr, '*', 'MinimumDumpDepth', pdump%mindumpdepth)
             pdump%dumpcapaflag = comparereal(pdump%mindumpdepth,-999.0_fp) /= 0
             !
             call prop_get(link_ptr, '*', 'UseDunes'     , pdump%use_dunes)
             if (pdump%use_dunes .and. .not. lfbedfrm) then
                call prterr(lundia, 'U021', 'UseDunes: Dunes can only be used when modelled.')
                call d3stop(1, gdp)
             endif
          case default
             !
             ! Ignore any other child (like 'dredge')
             !
          end select
       enddo
       !
       do i = 1, nadump
          pdump => dump_prop(i)
          write(lundia, '(a,i0)')      'Dump definition number        : ', i
          write(lundia, '(a)')         '  Dump area                   : ' // trim(dump_areas(i))
          pdump%name = dump_areas(i)
          select case (pdump%dumpdistr)
          case (DUMPDISTR_UNIFORM)
             stringval = 'uniform'
          case (DUMPDISTR_LOWEST)
             stringval = 'lowest'
          case (DUMPDISTR_DEEPEST)
             stringval = 'deepest'
          case (DUMPDISTR_PROPORTIONAL)
             stringval = 'proportional'
          end select
          write(lundia,'(a,i0,a)')     '  Dump distribution           : ',pdump%dumpdistr,' ('//trim(stringval)//')'
          if (pdump%dumpcapaflag) then
             write(lundia,'(a,es12.3e3)') '  MinimumDumpDepth           : ',pdump%mindumpdepth
          endif
          !
          select case (pdump%depthdef)
          case (DEPTHDEF_REFPLANE)
             stringval = 'reference plane'
          case (DEPTHDEF_WATERLVL)
             stringval = 'water level'
          case (DEPTHDEF_MAXREFWL)
             stringval = 'maximum(reference plane,water level)'
          case (DEPTHDEF_MINREFWL)
             stringval = 'minimum(reference plane,water level)'
          end select
          write(lundia,'(a,i0,a)')     '  Depth definition            : ',pdump%depthdef,' (relative to '//trim(stringval)//')'
       enddo
    endif
    !
    do i = 1, nadred+nasupl
       pdredge => dredge_prop(i)
       !
       if (.not. pdredge%dumplimited) cycle
       !
       ! This is a dredging area for which the dredging rate is limited by the
       ! dumping capacity. Now all links and dump areas are known, we can check
       ! whether the dump areas are uniquely associated with this dredging area.
       !
       do j = 1, nalink
          if (link_def(j,1) /= i) cycle
          !
          ic     = link_def(j,2)
          sfound = .false.
          do j2 = 1, nalink
             if (link_def(j2,2) /= ic .or. j2 == j) cycle
             sfound = .true.
          enddo
          !
          if (sfound) then
             write(message,'(5a)') 'Dump area "', trim(dump_areas(ic)), &
                                 & '" not uniquely associated with sediment source "', &
                                 & trim(dredge_areas(i)), '".'
             call prterr(lundia, 'U021', message)
             call d3stop(1, gdp)
          endif
       enddo
    enddo
    !
    noutletlinks = 0
    do i = 1, nadred+nasupl
       pdredge => dredge_prop(i)
       !
       select case (pdredge%dumpdistr)
       case (DR2DUDISTR_PERCENTAGE)
          !
          ! Add an outlet if one of the dump areas can be full.
          !
          do j = 1, nalink
             if (link_def(j,1) /= i) cycle
             !
             ic = link_def(j,2)
             if (dump_prop(ic)%dumpcapaflag) then
                noutletlinks       = noutletlinks + 1
                pdredge%outletlink = nalink + noutletlinks
                exit
             endif
          enddo
          !
          ! or if total percentage is less than 100%
          !
          if (pdredge%outletlink==0) then
             do lsed = 1, lsedtot
                sumperc = 0.0_fp
                do j = 1, nalink
                   if (link_def(j,1) /= i) cycle
                   sumperc = sumperc + link_percentage(j,lsed)
                enddo
                if (comparereal(100.0_fp,sumperc) /= 0 .and. pdredge%outletlink==0) then
                   noutletlinks       = noutletlinks + 1
                   pdredge%outletlink = nalink + noutletlinks
                   exit
                endif
             enddo
          endif
       case (DR2DUDISTR_SEQUENTIAL)
          !
          ! Add an outlet if all dump areas can be full.
          !
          lastdumparea = .true.
          do j = nalink, 1, -1
             if (link_def(j,1) /= i) cycle
             !
             ic = link_def(j,2)
             if (lastdumparea) then
                !
                ! if the last dump area is unlimited, we don't need an outlet
                !
                if (dump_prop(ic)%dumpcapaflag) then
                   noutletlinks       = noutletlinks + 1
                   pdredge%outletlink = nalink + noutletlinks
                endif
                lastdumparea = .false.
             else
                !
                ! if an earlier dump area is unlimited, give an error
                !
                if (.not.dump_prop(ic)%dumpcapaflag) then
                   write(message,'(5a)') 'Dump area "', trim(dump_areas(ic)), &
                                       & '" is not last in sequence of dump areas for "', &
                                       & trim(dredge_areas(i)), '" but has unlimited dumping capacity.'
                   call prterr(lundia, 'U021', message)
                   call d3stop(1, gdp)
                endif
             endif
          enddo
       case (DR2DUDISTR_PROPORTIONAL)
          !
          ! All dump areas should be limited, verify this and add one outletlink.
          !
          do j = nalink, 1, -1
             if (link_def(j,1) /= i) cycle
             !
             ic = link_def(j,2)
             if (.not. dump_prop(ic)%dumpcapaflag) then
                write(message,'(5a)') 'Dump area "', trim(dump_areas(ic)), &
                                    & '" in list of dump areas for "', trim(dredge_areas(i)), &
                                    & '" has unlimited dumping capacity.'
                call prterr(lundia, 'U021', message)
                call d3stop(1, gdp)
             endif
          enddo
          noutletlinks       = noutletlinks + 1
          pdredge%outletlink = nalink + noutletlinks
       end select
    enddo
    !
    do i = 1, nadump
       pdump => dump_prop(i)
       !
       if (pdump%dumpdistr == DUMPDISTR_PROPORTIONAL .and. .not.pdump%dumpcapaflag) then
          write(message,'(3a)') 'Dump distribution proportional for area "', &
                               & trim(dump_areas(i)), '" requires specification of MinimumDumpDepth.'
          call prterr(lundia, 'U021', message)
          call d3stop(1, gdp)
       endif
    enddo
    !
    write (lundia,'(a)') '*** End of Dredging & dumping input'
    write (lundia, *)
    !
    if (noutletlinks > 0) then
       !
       istat = 0
       if (istat == 0) call reallocP(gdp%gddredge%link_percentage, (/nalink+noutletlinks,lsedtot/), fill = 100.0_fp, stat = istat)
       if (istat == 0) call reallocP(gdp%gddredge%link_distance, nalink+noutletlinks, fill = 0.0_fp, stat = istat)
       if (istat == 0) call reallocP(gdp%gddredge%link_sum, (/nalink+noutletlinks,lsedtot/), fill = 0.0_fp, stat = istat)
       if (istat == 0) call reallocP(gdp%gddredge%voldump, (/nadump+1,lsedtot/), fill = 0.0_fp, stat = istat)
       if (istat == 0) call reallocP(gdp%gddredge%totvoldump, nadump+1, fill = 0.0_fp, stat = istat)
       if (istat == 0) call reallocP(gdp%gddredge%localareadump, nadump+1, fill = 0.0_fp, stat = istat)
       if (istat == 0) call reallocP(gdp%gddredge%globalareadump, nadump+1, fill = 0.0_fp, stat = istat)
       if (istat == 0) call reallocP(gdp%gddredge%globaldumpcap, nadump+1, fill = 0.0_fp, stat = istat)
       if (istat == 0) call reallocP(gdp%gddredge%link_def, (/nalink+noutletlinks,2/), fill = 0, stat = istat)
       if (istat == 0) call reallocP(gdp%gddredge%dump_areas, nadump+1, fill = ' ', stat = istat)
       if (istat == 0) then
          allocate(gdp%gddredge%dump_prop(nadump+1), stat=istat)
          if (istat == 0) then
             gdp%gddredge%dump_prop(1:nadump) = dump_prop(1:nadump)
             deallocate(dump_prop, stat=istat)
          endif
       endif
       !
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'RdDredge: memory realloc error')
          call d3stop(1, gdp)
       endif
       !
       if (istat == 0) call reallocP(ipdu, nadump+1, stat = istat, fill=0)
       if (istat == 0) call reallocP(npdu, nadump+1, stat = istat, fill=0)
       !
       ! update local pointers
       !
       link_percentage   => gdp%gddredge%link_percentage
       link_distance     => gdp%gddredge%link_distance
       link_sum          => gdp%gddredge%link_sum
       voldump           => gdp%gddredge%voldump
       totvoldump        => gdp%gddredge%totvoldump
       localareadump     => gdp%gddredge%localareadump
       globalareadump    => gdp%gddredge%globalareadump
       globaldumpcap     => gdp%gddredge%globaldumpcap
       link_def          => gdp%gddredge%link_def
       dump_areas        => gdp%gddredge%dump_areas
       dredge_prop       => gdp%gddredge%dredge_prop
       dump_prop         => gdp%gddredge%dump_prop
       !
       nalink = nalink + noutletlinks
       nadump = nadump + 1
       !
       do i = 1, nadred+nasupl
          pdredge => dredge_prop(i)
          !
          if (pdredge%outletlink>0) then
             link_def(pdredge%outletlink,1) = i
             link_def(pdredge%outletlink,2) = nadump
          endif
       enddo
       !
       dump_areas(nadump) = 'REMOVED FROM MODEL'
       ! dump_prop
       pdump => dump_prop(nadump)
       !
       ! Initialize
       !
       pdump%name         = dump_areas(nadump)
       pdump%mindumpdepth = -999.0_fp
       pdump%dumpcapaflag = .false.
       pdump%dumpdistr    = DUMPDISTR_UNIFORM
       pdump%dumpwhendry  = .false.
       ! pdump%npnt         = 0 will be set by loop over dump polygons below
       pdump%use_dunes    = .false.
       pdump%depthdef     = DEPTHDEF_REFPLANE
       pdump%ichkloc      = CHKLOC_CENTRE
       nullify(pdump%nm)
       nullify(pdump%inm)
       nullify(pdump%reflevel)
       nullify(pdump%bedlevel)
       nullify(pdump%dz_dump)
       nullify(pdump%sortvar)
       !
    endif
    !
    ! assign points to dredging and dumping areas,
    ! compute areas of grid cells and total areas dumping locations
    !
    do i = 1, nadred+nasupl
       if (dredge_prop(i)%itype == DREDGETYPE_NOURISHMENT) cycle
       cntdred        = dredge_prop(i)%idx_type
       !
       imask(:)       = 0
       npnt           = 0
       ia             = ipdr(cntdred)
       do nm = 1, nmmax
          if (gsqs(nm) > 0.0_fp .and. kcs(nm)==1) then
             select case (dredge_prop(i)%ichkloc)
             case (CHKLOC_ALLCORNER)
                nmcor = nm
                call ipon(xdr(ia), ydr(ia), npdr(cntdred), xcor(nmcor), ycor(nmcor), istat, gdp)
                nmcor = nm - 1
                if (istat >= 0) call ipon(xdr(ia), ydr(ia), npdr(cntdred), xcor(nmcor), ycor(nmcor), istat, gdp)
                nmcor = nm - nmaxddb
                if (istat >= 0) call ipon(xdr(ia), ydr(ia), npdr(cntdred), xcor(nmcor), ycor(nmcor), istat, gdp)
                nmcor = nm - nmaxddb-1
                if (istat >= 0) call ipon(xdr(ia), ydr(ia), npdr(cntdred), xcor(nmcor), ycor(nmcor), istat, gdp)
             case (CHKLOC_CENTRE)
                call ipon(xdr(ia), ydr(ia), npdr(cntdred), xz(nm), yz(nm), istat, gdp)
             case (CHKLOC_ANYCORNER)
                nmcor = nm
                call ipon(xdr(ia), ydr(ia), npdr(cntdred), xcor(nmcor), ycor(nmcor), istat, gdp)
                nmcor = nm - 1
                if (istat < 0) call ipon(xdr(ia), ydr(ia), npdr(cntdred), xcor(nmcor), ycor(nmcor), istat, gdp)
                nmcor = nm - nmaxddb
                if (istat < 0) call ipon(xdr(ia), ydr(ia), npdr(cntdred), xcor(nmcor), ycor(nmcor), istat, gdp)
                nmcor = nm - nmaxddb - 1
                if (istat < 0) call ipon(xdr(ia), ydr(ia), npdr(cntdred), xcor(nmcor), ycor(nmcor), istat, gdp)
             end select
             if (istat >= 0) then
                imask(nm) = 1
                npnt      = npnt + 1
             endif
          endif
       enddo
       !
       dredge_prop(i)%npnt = npnt
                       allocate (dredge_prop(i)%nm(npnt)           , stat = istat)
       if (istat == 0) allocate (dredge_prop(i)%inm(npnt)          , stat = istat)
       if (istat == 0) allocate (dredge_prop(i)%area(npnt)         , stat = istat)
       if (istat == 0) allocate (dredge_prop(i)%hdune(npnt)        , stat = istat)
       if (istat == 0) allocate (dredge_prop(i)%dz_dredge(npnt)    , stat = istat)
       if (istat == 0) allocate (dredge_prop(i)%reflevel(npnt)     , stat = istat)
       if (istat == 0) allocate (dredge_prop(i)%dunetoplevel(npnt) , stat = istat)
       if (istat == 0) allocate (dredge_prop(i)%triggerlevel(npnt) , stat = istat)
       if (istat == 0) allocate (dredge_prop(i)%bedlevel(npnt)     , stat = istat)
       if (istat == 0) allocate (dredge_prop(i)%troughlevel(npnt)  , stat = istat)
       if (istat == 0) allocate (dredge_prop(i)%sedimentdepth(npnt), stat = istat)
       if (istat == 0) allocate (dredge_prop(i)%sortvar(npnt)      , stat = istat)
       if (istat == 0) allocate (dredge_prop(i)%triggered(npnt)    , stat = istat)
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'RdDredge: memory alloc error')
          call d3stop(1, gdp)
       endif
       !
       npnt = 0
       do nm = 1, nmmax
          if (imask(nm) > 0) then
             npnt                      = npnt + 1
             dredge_prop(i)%nm(npnt)   = nm
             dredge_prop(i)%area(npnt) = gsqs(nm)
             globalareadred(i)         = globalareadred(i) + gsqs(nm)
          endif
       enddo
       do ic = 1,npnt
          dredge_prop(i)%inm(ic) = ic
       enddo
       dredge_prop(i)%hdune         = -1.0E11_fp
       dredge_prop(i)%dz_dredge     = -1.0E11_fp
       dredge_prop(i)%reflevel      = -1.0E11_fp
       dredge_prop(i)%dunetoplevel  = -1.0E11_fp
       dredge_prop(i)%triggerlevel  = -1.0E11_fp
       dredge_prop(i)%bedlevel      = -1.0E11_fp
       dredge_prop(i)%troughlevel   = -1.0E11_fp
       dredge_prop(i)%sedimentdepth = -1.0E11_fp
       dredge_prop(i)%sortvar       = -1.0E11_fp
       dredge_prop(i)%triggered     = .false.
       !
       ! Calculate average x,y coordinate of dredge location
       ! for distance between dredge and dump locations
       !
       if (npnt > 0) then
          loctemp = 0.0_fp
          do ic = 1, npnt
            inm     = dredge_prop(i)%nm(ic)
            loctemp = loctemp + xz(inm)    
          enddo
          dredge_prop(i)%dredgeloc(1) = loctemp/npnt
          loctemp                     = 0.0_fp
          do ic = 1, npnt
            inm     = dredge_prop(i)%nm(ic)
            loctemp = loctemp + yz(inm)    
          enddo
          dredge_prop(i)%dredgeloc(2) = loctemp / npnt
       else 
          dredge_prop(i)%dredgeloc(1) = -999.0_fp
          dredge_prop(i)%dredgeloc(2) = -999.0_fp
       endif
    enddo
    !
    do i = 1, nadump
       imask(:)       = 0
       npnt           = 0
       if (npdu(i) /= 0) then
          ia = ipdu(i)
          do nm = 1, nmmax
             if (gsqs(nm) > 0.0_fp .and. kcs(nm)==1) then
                select case (dump_prop(i)%ichkloc)
                case (CHKLOC_ALLCORNER)
                   nmcor = nm
                   call ipon(xdu(ia), ydu(ia), npdu(i), xcor(nmcor), ycor(nmcor), istat, gdp)
                   nmcor = nm - 1
                   if (istat >= 0) call ipon(xdu(ia), ydu(ia), npdu(i), xcor(nmcor), ycor(nmcor), istat, gdp)
                   nmcor = nm - nmaxddb
                   if (istat >= 0) call ipon(xdu(ia), ydu(ia), npdu(i), xcor(nmcor), ycor(nmcor), istat, gdp)
                   nmcor = nm - nmaxddb - 1
                   if (istat >= 0) call ipon(xdu(ia), ydu(ia), npdu(i), xcor(nmcor), ycor(nmcor), istat, gdp)
                case (CHKLOC_CENTRE)
                   call ipon(xdu(ia), ydu(ia), npdu(i), xz(nm), yz(nm), istat, gdp)
                case (CHKLOC_ANYCORNER)
                   nmcor = nm
                   call ipon(xdu(ia), ydu(ia), npdu(i), xcor(nmcor), ycor(nmcor), istat, gdp)
                   nmcor = nm - 1
                   if (istat < 0) call ipon(xdu(ia), ydu(ia), npdu(i), xcor(nmcor), ycor(nmcor), istat, gdp)
                   nmcor = nm - nmaxddb
                   if (istat < 0) call ipon(xdu(ia), ydu(ia), npdu(i), xcor(nmcor), ycor(nmcor), istat, gdp)
                   nmcor = nm - nmaxddb - 1
                   if (istat < 0) call ipon(xdu(ia), ydu(ia), npdu(i), xcor(nmcor), ycor(nmcor), istat, gdp)
                end select
                if (istat >= 0) then
                   imask(nm) = 1
                   npnt      = npnt + 1
                endif
             endif
          enddo
       endif
       !
       dump_prop(i)%npnt = npnt
                       allocate (dump_prop(i)%nm(npnt)      , stat = istat)
       if (istat == 0) allocate (dump_prop(i)%inm(npnt)     , stat = istat)
       if (istat == 0) allocate (dump_prop(i)%reflevel(npnt), stat = istat)
       if (istat == 0) allocate (dump_prop(i)%area(npnt)    , stat = istat)
       if (istat == 0) allocate (dump_prop(i)%hdune(npnt)   , stat = istat)
       if (istat == 0) allocate (dump_prop(i)%bedlevel(npnt), stat = istat)
       if (istat == 0) allocate (dump_prop(i)%dz_dump(npnt) , stat = istat)
       if (istat == 0) allocate (dump_prop(i)%sortvar(npnt) , stat = istat)
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'RdDredge: memory alloc error')
          call d3stop(1, gdp)
       endif
       do ic = 1, npnt
          dump_prop(i)%inm(ic) = ic
       enddo
       dump_prop(i)%hdune    = 1.0E11_fp
       dump_prop(i)%reflevel = -1.0E11_fp
       dump_prop(i)%bedlevel = -1.0E11_fp
       dump_prop(i)%dz_dump  = -1.0E11_fp
       dump_prop(i)%sortvar  = -1.0E11_fp
       !
       npnt = 0
       do nm = 1, nmmax
          if (imask(nm) > 0) then
             npnt                    = npnt + 1
             localareadump(i)        = localareadump(i) + gsqs(nm)
             dump_prop(i)%nm(npnt)   = nm
             dump_prop(i)%area(npnt) = gsqs(nm)
          endif
       enddo
       !
       ! Calculate average x,y coordinate of dump location
       ! for distance between dredge and dump locations
       !
       if (npnt > 0) then
          loctemp = 0.0_fp
          do ic = 1, npnt
             inm     = dump_prop(i)%nm(ic)
             loctemp = loctemp + xz(inm)    
          enddo
          dump_prop(i)%dumploc(1) = loctemp / npnt
          loctemp = 0.0_fp
          do ic = 1, npnt
             inm     = dump_prop(i)%nm(ic)
             loctemp = loctemp + yz(inm)    
          enddo
          dump_prop(i)%dumploc(2) = loctemp / npnt
       else 
          dump_prop(i)%dumploc(1) = -999.0_fp
          dump_prop(i)%dumploc(2) = -999.0_fp
       endif
    enddo
    !
    do ic = 1, nalink
       x1 = dredge_prop(link_def(ic,1))%dredgeloc
       y1 =   dump_prop(link_def(ic,2))%dumploc
       if  (      comparereal(  dump_prop(link_def(ic,2))%dumploc(1)  , -999.0_fp)              == 0   &
           & .or. comparereal(  dump_prop(link_def(ic,2))%dumploc(2)  , -999.0_fp)              == 0   &
           & .or. comparereal(dredge_prop(link_def(ic,1))%dredgeloc(1), -999.0_fp)              == 0   &
           & .or. comparereal(dredge_prop(link_def(ic,1))%dredgeloc(2), -999.0_fp)              == 0   &
             .or. dredge_prop(link_def(ic,1))%itype                  == DREDGETYPE_NOURISHMENT      ) then
            link_distance(ic) = 0.0_fp
        else    
            link_distance(ic) = sqrt(abs((y1(1)-x1(1))**2.0_fp)+abs((y1(2)-x1(2))**2.0_fp ))
        endif
    enddo
    !
    ! Deallocate arrays used for detecting dredge, dump points
    !
    deallocate (ipdr , stat = istat)
    deallocate (ipdu , stat = istat)
    deallocate (npdr , stat = istat)
    deallocate (npdu , stat = istat)
    !
    deallocate (xdr  , stat = istat)
    deallocate (xdu  , stat = istat)
    deallocate (ydr  , stat = istat)
    deallocate (ydu  , stat = istat)
    !
    deallocate (imask, stat = istat)
    !
end subroutine rddredge
