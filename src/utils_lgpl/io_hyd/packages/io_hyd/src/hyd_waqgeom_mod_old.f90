module wqhyd_version_module


implicit none

    character(*),  public, parameter :: wqhyd_major        = '1'
    character(*),  public, parameter :: wqhyd_minor        = '1'
    character(*),  public, parameter :: wqhyd_revision     = '00'
    character(*),  public, parameter :: wqhyd_build_number = '000000'

    character(*),  public, parameter :: wqhyd_company      = 'Deltares'
    character(*),  public, parameter :: wqhyd_company_url  = 'http://www.deltares.nl'
    character(*),  public, parameter :: wqhyd_program      = 'wqhyd'
    character(*),  public, parameter :: wqhyd_basename     = 'wqhyd'

    character(*),  public, parameter :: wqhyd_version      = wqhyd_major//'.'//wqhyd_minor//'.'//wqhyd_revision//'.'//wqhyd_build_number
    character(*),  public, parameter :: wqhyd_version_full = 'Deltares, '//wqhyd_program//' Version '//wqhyd_version
    character(*),  public, parameter :: wqhyd_version_id   = '@_(_#_)'//wqhyd_version_full ! Blocked recognision by what!

contains

    subroutine wqhyd_version_init()

        write(*,*) wqhyd_version_id

    end subroutine

    subroutine get_wqhyd_versionstring(stringout)
        character(len=*), intent(out) :: stringout
        stringout = wqhyd_version
    end subroutine get_wqhyd_versionstring

    subroutine get_full_versionstring_wqhyd_full(stringout)
        character(len=*), intent(out) :: stringout
        stringout = wqhyd_version_full
    end subroutine get_full_versionstring_wqhyd_full

end module wqhyd_version_module


module wqm_profiles
 ! profile related :
 type tprof                                          !< this is a profile type
   integer                        :: ityp            !< 1 = circle, 2=rectan1dlumped, 3=rectan2d, 9=yzlumped, 10=yzconveyance
   integer                        :: frctp           !< friction type chezy manning etc
   double precision               :: frccf           !< friction coefficient
   double precision               :: width           !< max width
   double precision               :: height          !< max height
   real, allocatable              :: xx(:), yy(:)    !< y z point coordinates
   real, allocatable              :: y (:), z (:)    !< y and z arrays
 end type tprof

 integer                          :: nprofdefs       !< nr of unique  profile definitions
 type(tprof), allocatable         :: profiles1D(:)   !< these are the profiles

 integer                          :: nproflocs       !< nr of profile locations, always <= nprofdefs
 integer                          :: maxproflocnr       !< highest referred profnr in profloc.xyz
 integer                          :: minproflocnr       !< lowest  referred profnr in profloc.xyz
 double precision, allocatable    :: xpr(:), ypr(:), zpr(:) !< profile locations, x,y,z
 integer,          allocatable    :: npr(:)                 !< at these locations, reference to profdefs
end module wqm_profiles

 !> in m_waqgeom: nd and ln apply to waterlevel nodes and links
 !! in m_netw    : nod and lin apply to 'grid' or 'net' nodes and links
 module wqm_waqgeom
 use wqm_profiles
 use coordinate_reference_system

 implicit none

! projection
 type(t_crs)                       :: crs            !< Container for information about coordinate reference system

! net nodes
 integer                           :: numk           !< number of net nodes
 double precision, allocatable     :: xk (:)         !< net node x (m)
 double precision, allocatable     :: yk (:)         !< net node y (m)
 double precision, allocatable     :: zk (:)         !< net node z (m)
 integer                           :: numl           !< number of nodes links
 integer, allocatable              :: kn(:,:)        !< node links
 integer                           :: nv             !< max nr of nodes describing an element
 integer                           :: nump           !< number of elements
 integer, allocatable              :: netcellnod(:,:)!< element nodes

 ! node (s) related : dim=ndx
 type tnode                                          !< node administration
   integer                         :: lnx            !< max nr of links attached to this node
   integer, allocatable            :: ln (:)         !< linknrs attached to this node, >0: to this flownode, <0: from this flownode

   integer, allocatable            :: nod(:)         !< Mapping to net nodes
   double precision, allocatable   :: x  (:)         !< for now, this is only for quick/aligned plotting, the corners of a cell
   double precision, allocatable   :: y  (:)         !< for now, this is only for quick/aligned plotting, the corners of a cell
 end type tnode

 double precision                  :: bamin          !< minimum 2D cell area
 double precision                  :: bamin1D        !< minimum cell area 1d nodes
 double precision                  :: dxmin          !< minimum link length (m)

 double precision                  :: wu1DUNI           !< uniform 1D profile width
 double precision                  :: hh1DUNI           !< uniform 1D profile height


 integer                           :: ndx2D          !< nr of 2d FLOW CELLS = NUMP
 integer                           :: ndxi           !< nr of internal flowcells  (internal = 2D + 1D )
 integer                           :: ndx            !< nr of flow nodes (internal + boundary)
 type (tnode),     allocatable     :: nd (:)         !< (ndx) node administration
 integer,          allocatable     :: kcs(:)         !< node code permanent
 integer,          allocatable, target     :: kfs(:)         !< node code flooding
 double precision, allocatable     :: ba (:)         !< bottom area (m2), if < 0 use table in node type
 double precision, allocatable     :: bai(:)         !< inv bottom area (m2), if < 0 use table in node type
 double precision, allocatable, target :: bl (:)         !< bottom level (m) (positive upward)
 double precision, allocatable     :: xz (:)         !< waterlevel point / cell centre x (m)
 double precision, allocatable     :: yz (:)         !< waterlevel point / cell centre y (m)
 double precision, allocatable     :: aif(:)         !< cell based skewness ai factor sqrt(1+(dz/dy)**2) = abottom/asurface
                                                     !< so that cfu=g(Au/conveyance)**2 = g*aif*(Au/convflat)**2
                                                     !< convflat is flat-bottom conveyance


 ! link (u) related : dim = lnx
 integer                           :: lnx1D          !< nr of 1D flow links (so first 1D, next 2D, next boundaries)
 integer                           :: lnxi           !< nr of flow links (internal, 1D+2D    )
 integer                           :: lnx            !< nr of flow links (internal + boundary). First we have 1D links, next 2D links, next boundary links
 integer,          allocatable     :: ln    (:,:)    !< link (2,*) node   administration, 1=nd1,  2=nd2   linker en rechter celnr
 integer,          allocatable     :: lncn  (:,:)    !< link (2,*) corner administration, 1=nod1, 2=nod2  linker en rechter netnr
 integer,          allocatable     :: lnk   (:,:)    !< link (2,*) 3D cel administration, 1=ndk1, 2=ndk2  linker en rechter kcelnr
 integer,          allocatable     :: kcu   (:)      !< link code, 1=1D link, 2=2D link, -1= bc 1D, -2=bc 2D, 3=2D parall wall, 4=1D2Dlink, 5=Pump
 integer,          allocatable, target     :: iadv  (:)      !< type of advection for this link
 double precision, allocatable     :: teta  (:)      !< link teta (m)
 integer,          allocatable     :: klnup (:,:)    !< link upwind cell pointer if q> 0 use (1:3,L), else (4:6,L)
 double precision, allocatable     :: dx    (:)      !< link length (m)
 double precision, allocatable     :: dxi   (:)      !< inverse dx
 double precision, allocatable     :: wu    (:)      !< link initial width (m), if < 0 pointer to convtab
 double precision, allocatable     :: wui   (:)      !< inverse link initial width (m), if < 0 pointer to convtab
 real,             allocatable     :: prof1D (:,:)   !< dim = (3,lnx1D) 1= 1D prof width, 2=1D profile height, 3=proftyp, or: if 1,2< 0, pointers to prof 1,2, then 3=alfa1
 double precision, allocatable     :: bob   (:,:)    !< left and right binnenkant onderkant buis HEIGHT values (m) (positive upward)
 integer,          allocatable     :: ibot  (:)      !< local ibedlevtype for setting min or max network depths (temporary, result goes to bobs)
 double precision, allocatable     :: acl   (  :)    !< left dx fraction, alfacl
 double precision, allocatable     :: acn   (:,:)    !< 2,L left and rigth wu fraction
 double precision, allocatable     :: xu    (:)      !< velocity point x (m)
 double precision, allocatable     :: yu    (:)      !< velocity point y (m)
 double precision, allocatable     :: blu   (:)      !< velocity point bottom level positive up (m)
 double precision, allocatable     :: csu   (:)      !< cosine comp of u0, u1
 double precision, allocatable     :: snu   (:)      !< sine   comp of u0, u1
 double precision, allocatable     :: wcl   (:,:)    !< link weights (2,lnx) for center scalar , 1,L for k1, 2,L for k2
!double precision, allocatable     :: wcxyl (:,:)    !< link weights (4,lnx) for center velocities 1=1-x, 2=1-y, 3=2-x, 4=2y
!double precision, allocatable     :: wcnxyl(:,:)    !< link weights (4,lnx) for corner velocities 1=3-x, 2=3-y, 3=4-x, 4=4y
 double precision, allocatable     :: wcx1(:)        !< link weights (lnx) for corner velocities k3
 double precision, allocatable     :: wcy1(:)        !< link weights (lnx) for corner velocities k3
 double precision, allocatable     :: wcx2(:)        !< link weights (lnx) for corner velocities k4
 double precision, allocatable     :: wcy2(:)        !< link weights (lnx) for corner velocities k4
 double precision, allocatable     :: wcnx3(:)       !< link weights (lnx) for corner velocities k3
 double precision, allocatable     :: wcny3(:)       !< link weights (lnx) for corner velocities k3
 double precision, allocatable     :: wcnx4(:)       !< link weights (lnx) for corner velocities k4
 double precision, allocatable     :: wcny4(:)       !< link weights (lnx) for corner velocities k4


 double precision, allocatable     :: slnup (:,:)    !< link upwind cell weight, if q> 0 use (1:3,L), else (4:6,L)
 double precision, allocatable     :: econsfacL(:)   !< link upwind cell weight, if q> 0 use (1:3,L), else (4:6,L)
 integer                           :: jaecons        !< use this array or not 1,0


 integer,          allocatable     :: ln2lne(:)      !< temporary array link to edge adressing
 integer,          allocatable     :: lne2ln(:)      !< temporary array edge to link adressing


 ! cell corner related, the links attached to a cell corner
 type tcorn                                          !< corner administration
   integer                         :: lnx            !< max nr of links attached to this corner
   integer, allocatable            :: ln (:)         !< linknrs attached to this corner

 end type tcorn                                      !< corner administration

 type(tcorn)     , allocatable     :: cn  (:)        !< cell cornerpoints, (in counting order of nod)
 double precision, allocatable     :: ucnx(:)        !< cell corner velocity, global x-dir (m/s)
 double precision, allocatable     :: ucny(:)        !< cell corner velocity, global y-dir (m/s) (in m_waqgeom...)
 double precision, allocatable, target   :: vort(:)        !< vorticity at netnodes


 ! fixed wall related, may be expanded to closed internal walls later for now, dim=(7,*)
integer                            :: mxwalls            !< max nr of walls
double precision, allocatable      :: walls(:,:)     !< 1,* : inside waterlevel point (node)
                                                     !! 2,* : first  cornerpoint
                                                     !! 3,* : second cornerpoint
                                                     !! 4,* : flow link 1 attached to first  cornerpoint
                                                     !! 5,* : flow link 2 attached to second cornerpoint
                                                     !! 6,* : stress contribution to link 1
                                                     !! 7,* : stress contribution to link 1
integer                            :: nwcnx          !< max nr of cornerpoints to which walls are attached
integer,          allocatable      :: nwalcn(:,:)    !< pointer to those walls, 1 = left wall, 2 =right wall

! closed wall corner (netnode) related
integer                           :: nrcnw          !< nr of cn points attached to 2 closed walls
integer         , allocatable     ::  kcnw (:)      !< closed wall point nr k, reference to net nodes
real            , allocatable     :: cscnw (:)      !< closed wall alignment cos (1:nrcnw)
real            , allocatable     :: sncnw (:)      !< closed wall alignment sin (1:nrcnw)



 ! branch related :
 type tbranch                                        !< this is a branch type
   integer                        :: num_cells_u_dir             !< with num_cells_u_dir links and num_cells_u_dir + 1 nodes in it
   integer, allocatable           :: ln (:)         !< successive flow linknrs
 end type tbranch

 integer                          :: mxflowbr       !< max nr of flow branches
 type(tbranch), allocatable       :: flowbr(:)      !< this is a list of flow branches


 integer, allocatable             :: Lbnd1D(:)      !< for prof1D, boundary links refer to regular attached 1D links

 ! 1D endnode related
 integer                          :: mx1Dend        !< nr of 1D endnodes
 integer,          allocatable    :: n1Dend(:)      !< node nrs of 1D endnodes


! netnode/flownode  related, dim = mxban
 double precision, allocatable     :: banf  (:)     !< horizontal netnode/flownode area (m2)
 double precision, allocatable     :: ban  (:)      !< horizontal netnode          area (m2)
 integer         , allocatable     :: nban  (:,:)   !< base area pointers to banf, 1,* = netnode number, 2,* = flow node number
 integer                           :: mxban         !< max dim of ban


 ! useful parameters :
 double precision                 :: rrtol            !< relative cellsize factor in search tolerance ()
 double precision, allocatable    :: xyen(:,:)        !< temp boundary opposite point (end of EdgeNormal) (replaces ebtol tolerance)
 integer                          :: jasnelucxy       !< method for cell center velocities
                                                       !! 1 = slow node oriented, 2 = link csu etc, 3= fast linkweight
 integer                          :: jarenumber       !< renumberFlowNodes
 integer                          :: jaFlowNetChanged !< To enforce various net(link)-related init routines after renumbering


! JRE Stuff related to setting up wave directional grid
 integer                                     :: ntheta          !< Number of wave direction bins
 double precision                            :: thetamax        !< upper limit wave directional sector
 double precision                            :: thetamin        !< lower limit wave directional sector
 double precision                            :: thetanaut       !< nautical convention or not
 double precision                            :: dtheta          !< directional resolution
 double precision                            :: theta0          !< mean theta-grid direction
 double precision, allocatable               :: thetabin(:)           !< bin-means of theta-grid

 end module wqm_waqgeom



 module wqm_sferic
 implicit none
 integer                           :: jsferic = 0       ! xy pair is in : 0=cart, 1=sferic coordinates
 integer                           :: jsfertek= 0       ! drawn in 0=cart, 1=stereografisch
 integer                           :: jasferdistance = 0 ! 1 = local dx and dy based upon greatcircledistances , 0=original form
 integer                           :: jglobe  = 0       ! if (jsferic==1) do we need extra tests for 360-0 transgression
 double precision                  :: pi                ! pi
 double precision                  :: twopi             ! 2pi
 double precision                  :: dg2rd             ! degrees to radians
 double precision                  :: rd2dg             ! and vice versa
 double precision                  :: ra = 6378137d0    ! earth radius (m)
 double precision                  :: omega             ! earth angular velocity (rad/s)
 double precision                  :: fcorio            ! 2omegasinfi
 double precision                  :: anglat = 0d0      ! 26.0     ! dubai 52.5     ! angle of latitude
 double precision                  :: anglon = 0d0      ! 26.0     ! dubai 52.5     ! angle of latitude
 double precision                  :: dy2dg             ! from dy in m to lat in degrees
 double precision                  :: csphi             ! cosphi of latest requested

 double precision, parameter       :: dtol_pole = 1d-4   ! pole tolerance in degrees
 end module wqm_sferic



module wqm_partitioninfo

   integer, dimension(:), allocatable      :: idomain            !< cell-based domain number, dim(nump1d2d or ndx)
   integer, dimension(:), allocatable      :: iglobal            !< global cell numbers
   integer                                 :: my_rank = 0        !< domainnumber

end module wqm_partitioninfo

! TODO: FB: #define NC_CHECK if(ierr .ne. 0 ) call mess(LEVEL_ERROR, nf90_strerror(ierr))

!> Reads and writes unstructured net/flow data in netCDF format.
module wq_unstruc_netcdf

!

use precision
use netcdf
use MessageHandling !unstruc_messages
use wqhyd_version_module
use coordinate_reference_system

implicit none

!! Error codes
integer, parameter :: UG_NOERR                 = NF90_NOERR
integer, parameter :: UG_SOMEERR               = 10 !< Some unspecified error.
integer, parameter :: UG_INVALID_MESHNAME      = 11
integer, parameter :: UG_INVALID_MESHDIMENSION = 12
integer, parameter :: UG_INVALID_DATALOCATION  = 13
integer, parameter :: UG_INVALID_CRS           = 30 !< Invalid/missing coordinate reference system (using default)
integer, parameter :: UG_NOTIMPLEMENTED        = 99

integer, parameter :: maxMessageLen = 1024
character(len=maxMessageLen) :: ug_messagestr !< Placeholder string for storing diagnostic messages. /see{ug_get_message}

integer            :: nerr_
logical            :: err_firsttime_
character(len=255) :: err_firstline_

!> All NetCDF files should be opened through unc_open or unc_create,
!! such that all opened files are maintained and can be properly closed
!! upon exit of the program by unc_closeall.
integer, parameter :: maxopenfiles = 1000
character(len=255) :: open_files_(maxopenfiles)    !< Names of open NetCDF files.
integer            :: open_datasets_(maxopenfiles) !< Dataset IDs of open NetCDF files.
integer            :: nopen_files_ = 0             !< Nr. of NetCDF files currently open.
character(len=80)  :: version_full

private :: nerr_, err_firsttime_, err_firstline_, &
           prepare_error, check_error, &
           open_files_, open_datasets_, nopen_files_

contains


!> Puts global attributes in NetCDF data set.
!! This includes: institution, Conventions, etc.
subroutine unc_addglobalatts(ncid, version_full)
    !use unstruc_model, only : md_ident
    integer, intent(in) :: ncid
    character(len=*), intent(in) ::  version_full

    character(len=8)  :: cdate
    character(len=10) :: ctime
    character(len=5)  :: czone
    integer :: ierr, jaInDefine
    ierr = nf90_noerr
    jaInDefine = 0

    ierr = nf90_redef(ncid)
    if (ierr == nf90_eindefine) jaInDefine = 1 ! Was still in define mode.
    if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
        call mess(LEVEL_ERROR, 'Could not put global attributes in NetCDF #', ncid)
        return
    end if

    ierr = nf90_put_att(ncid, nf90_global,  'institution', trim(wqhyd_company))
    ierr = nf90_put_att(ncid, nf90_global,  'references', trim(wqhyd_company_url))
    ierr = nf90_put_att(ncid, nf90_global,  'source', &
            trim(version_full)//                    &
            ', model ')!''//trim(md_ident)//'''')

    call date_and_time(cdate, ctime, czone)
    ierr = nf90_put_att(ncid, nf90_global,  'history', &
        'Created on '//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)// &
        ', '//trim(wqhyd_program))

    ierr = nf90_put_att(ncid, nf90_global,  'Conventions', 'CF-1.5:Deltares-0.1')

    ! Leave the dataset in the same mode as we got it.
    if (jaInDefine == 1) then
        ierr = nf90_redef(ncid)
    end if
end subroutine unc_addglobalatts


!> Opens a NetCDF file for reading.
!! The file is maintained in the open-file-list.
function unc_open(filename, cmode, ncid)
    character(len=*), intent(in ) :: filename
    integer,          intent(in ) :: cmode
    integer,          intent(out) :: ncid
    integer                       :: unc_open
    unc_open = nf90_open(trim(filename), cmode, ncid)
    if (unc_open == nf90_noerr) then
        nopen_files_ = nopen_files_ + 1
        open_files_(nopen_files_)    = filename
        open_datasets_(nopen_files_) = ncid
        write (msgbuf, '(a,a,a,i10,a)') 'Opened ''', trim(filename), ''' as #', ncid, '.'
        call msg_flush()
    else
        call mess(LEVEL_ERROR, 'could not open '//trim(filename))
        call msg_flush()
!        call qnerror('Failed to open: '//trim(filename), ' ', ' ')
    end if
end function unc_open


!> Creates or opens a NetCDF file for writing.
!! The file is maintained in the open-file-list.
function unc_create(filename, version_full, cmode, ncid)
    character(len=*), intent(in ) :: filename
    character(len=*), intent(in) ::  version_full
    integer,          intent(in ) :: cmode
    integer,          intent(out) :: ncid
    integer                       :: unc_create

    unc_create = nf90_create(filename, cmode, ncid)
    if (unc_create == nf90_noerr) then
        nopen_files_ = nopen_files_ + 1
        open_files_(nopen_files_)    = filename
        open_datasets_(nopen_files_) = ncid
        write (msgbuf, '(a,a,a,i10,a)') 'Opened NetCDF file ''', trim(filename), ''' as #', ncid, '.'
        call msg_flush()
    end if

    call unc_addglobalatts(ncid, version_full)

end function unc_create


!> Closes a NetCDF file.
!! The file is removed from the open-file-list
integer function unc_close(ncid)
    integer, intent(in) :: ncid
    integer             :: i, j
    logical             :: jafound

    unc_close = 0

    jafound = .false.
    ! Search dataset ID
    do i=nopen_files_,1,-1
        if (open_datasets_(i) == ncid) then
            jafound = .true.
            exit
        end if
    end do
    ! If found, shift all entries behind it one to the left.
    if (jafound) then
        unc_close = nf90_close(ncid)
        write (msgbuf, '(a,a,a)') 'Closed NetCDF file ''', trim(open_files_(nopen_files_)), '.'
        call msg_flush()
        do j=nopen_files_-1,-1,i
            open_files_(j)    = open_files_(j+1)
            open_datasets_(j) = open_datasets_(j+1)
        end do
        open_files_(nopen_files_)    = ' '
        open_datasets_(nopen_files_) = 0
        nopen_files_ = nopen_files_ - 1
    else
        write (msgbuf, '(a,i3,a)') 'Tried to close NetCDF id ', ncid, ', not found.'
        call msg_flush()
    end if
end function unc_close


!> Closes all NetCDF files that are still open.
subroutine unc_closeall()
    integer :: i, istat
    do i = nopen_files_,1,-1
        istat = unc_close(open_datasets_(i))
    end do
end subroutine unc_closeall


!> Adds coordinate attributes according to CF conventions, based on jsferic.
!! Non-standard attributes (such as long_name) should be set elsewhere.
function unc_addcoordatts(ncid, id_varx, id_vary, jsferic)
    integer, intent(in) :: ncid     !< NetCDF dataset id
    integer, intent(in) :: id_varx  !< NetCDF horizontal variable id
    integer, intent(in) :: id_vary  !< NetCDF vertical variable id
    integer, intent(in) :: jsferic  !< Sferical coords or not (1/0)
    integer             :: unc_addcoordatts !< Result status of NetCDF primitives

    integer :: ierr

    if (jsferic == 0) then
        ierr = nf90_put_att(ncid, id_varx, 'units',         'm')
        ierr = nf90_put_att(ncid, id_vary, 'units',         'm')
        ierr = nf90_put_att(ncid, id_varx, 'standard_name', 'projection_x_coordinate')
        ierr = nf90_put_att(ncid, id_vary, 'standard_name', 'projection_y_coordinate')
        ierr = nf90_put_att(ncid, id_varx, 'long_name'    , 'x')
        ierr = nf90_put_att(ncid, id_vary, 'long_name'    , 'y')
    else
        ierr = nf90_put_att(ncid, id_varx, 'units',         'degrees_east')
        ierr = nf90_put_att(ncid, id_vary, 'units',         'degrees_north')
        ierr = nf90_put_att(ncid, id_varx, 'standard_name', 'longitude')
        ierr = nf90_put_att(ncid, id_vary, 'standard_name', 'latitude')
        ierr = nf90_put_att(ncid, id_varx, 'long_name'    , 'longitude')
        ierr = nf90_put_att(ncid, id_vary, 'long_name'    , 'latitude')
    end if
    unc_addcoordatts = ierr
end function unc_addcoordatts


!> Add longitude and latitude coordinates to a NetCDF dataset.
!!
!! Lon/lat coordinates are required by CF-standards, even if the coordinates
!! used are projected Cartesian. Two new coordinate variables are added
!! to the NetCDF id (e.g. a .nc file), but only if jsferic==0.
!! The names for the new variables are based on varbasename and a postfix.
function unc_add_lonlat_vars(ncid, varnameprefix, varnamepostfix, id_dims, id_varlon, id_varlat, jsferic) result(ierr)
    integer,               intent(in)  :: ncid           !< NetCDF dataset id
    character(len=*),      intent(in)  :: varnameprefix  !< Base text string for new lon lat variable names.
    character(len=*),      intent(in)  :: varnamepostfix !< Text string to be appended for new lon lat variable names.
    integer, dimension(:), intent(in)  :: id_dims        !< Array with NetCDF dimension ids for the coordinate variables.
    integer,               intent(out) :: id_varlon      !< NetCDF horizontal variable id
    integer,               intent(out) :: id_varlat      !< NetCDF vertical variable id
    integer,               intent(in)  :: jsferic        !< Spherical coords or not (1/0). If 1, nothing happens.
    integer                            :: ierr           !< Result status of NetCDF primitives

    ierr = 0

    ! If current system is already spherical, lon/lat should already be present.
    if (jsferic == 1) then
        return
    end if

    ! Define lon and lat variables
    ierr = nf90_def_var(ncid, trim(varnameprefix)//'_lon'//trim(varnamepostfix), nf90_double, id_dims, id_varlon)
    call check_error(ierr, 'Add longitude variable for '//trim(varnameprefix))
    ierr = nf90_def_var(ncid, trim(varnameprefix)//'_lat'//trim(varnamepostfix), nf90_double, id_dims, id_varlat)
    call check_error(ierr, 'Add latitude variable for '//trim(varnameprefix))

    ! Add standard spherical coordinate attributes
    ierr = unc_addcoordatts(ncid, id_varlon, id_varlat, 1)

!    ierr = unc_add_coordmapping(ncid, jsferic)
    call check_error(ierr, 'Add grid_mapping variable for '//trim(varnameprefix)//'_lon'//trim(varnamepostfix)//'/_lat'//trim(varnamepostfix))

    ierr = unc_add_gridmapping_att(ncid, (/ id_varlon, id_varlat /), 1)
    call check_error(ierr, 'Add grid_mapping attributes to '//trim(varnameprefix)//'_lon'//trim(varnamepostfix)//'/_lat'//trim(varnamepostfix))

end function unc_add_lonlat_vars


!> Adds coordinate mapping attributes according to CF conventions, based on jsferic.
!! Attributes are put in a scalar integer variable.
function unc_add_coordmapping(ncid, crs) result(ierr)
   integer,      intent(in) :: ncid  !< NetCDF dataset id
   type(t_crs),  intent(in) :: crs   !< Coordinate reference system that was used for the coordinate mapping.
   integer                  :: ierr  !< Result status (UG_NOERR==NF90_NOERR) if successful.

   integer :: id_crs
   integer :: epsg
   integer :: ierr_missing
   character(len=11) :: epsgstring
   character(len=30) :: varname  !< Name of the created grid mapping variable.

   ierr = UG_NOERR
   ierr_missing = UG_NOERR ! Store whether crs was missing (and default was used)

   epsgstring = ' '

   varname = ' '
   if (len_trim(crs%varname) > 0) then
      varname = crs%varname
   else if (crs%epsg_code == 4326) then
      ierr_missing = UG_INVALID_CRS
      varname = 'wgs84'
   else
      ierr_missing = UG_INVALID_CRS
      varname = 'projected_coordinate_system'
   end if

   ierr = nf90_inq_varid(ncid, trim(varname), id_crs)
   if (ierr == nf90_noerr) then
      ! A variable with that name already exists. Return without error.
      ierr = UG_NOERR
      goto 888
   end if

   ierr = nf90_def_var(ncid, trim(varname), nf90_int, id_crs)

   if (allocated(crs%attset)) then
      ierr = ug_put_var_attset(ncid, id_crs, crs%attset)
   elseif (crs%epsg_code == 4326) then
      ierr_missing = UG_INVALID_CRS
      epsg      = 4326
      epsgstring = 'EPSG:4326'
      ierr = nf90_put_att(ncid, id_crs, 'name',                       'WGS84'             ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'epsg',                       epsg                ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'grid_mapping_name',          'latitude_longitude') ! CF
      ierr = nf90_put_att(ncid, id_crs, 'longitude_of_prime_meridian', 0d0                ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'semi_major_axis',             6378137d0          ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'semi_minor_axis',             6356752.314245d0   ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'inverse_flattening',          298.257223563d0    ) ! CF
!      ierr = nf90_put_att(ncid, id_crs, 'proj4_params',                ' '                ) ! ADAGUC
      ierr = nf90_put_att(ncid, id_crs, 'epsg_code',                   trim(epsgstring)   ) ! ADAGUC
!      ierr = nf90_put_att(ncid, id_crs, 'projection_name',             ' '                ) ! ADAGUC
!      ierr = nf90_put_att(ncid, id_crs, 'wkt',                         ' '                ) ! WKT
!      ierr = nf90_put_att(ncid, id_crs, 'comment',                     ' '                )
      ierr = nf90_put_att(ncid, id_crs, 'value',                       'value is equal to EPSG code')
   else
      ierr_missing = UG_INVALID_CRS
      epsg      = crs%epsg_code
      epsgstring = 'EPSG:28992'
      ierr = nf90_put_att(ncid, id_crs, 'name',                        'Unknown projected' ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'epsg',                        epsg                ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'grid_mapping_name',           'Unknown projected' ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'longitude_of_prime_meridian', 0d0                 ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'semi_major_axis',             6378137d0           ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'semi_minor_axis',             6356752.314245d0    ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'inverse_flattening',          298.257223563d0     ) ! CF
!      ierr = nf90_put_att(ncid, id_crs, 'proj4_params',                ' '                 ) ! ADAGUC
      ierr = nf90_put_att(ncid, id_crs, 'EPSG_code',                   trim(epsgstring)    ) ! ADAGUC
!      ierr = nf90_put_att(ncid, id_crs, 'projection_name',             ' '                 ) ! ADAGUC
!      ierr = nf90_put_att(ncid, id_crs, 'wkt',                         ' '                 ) ! WKT
!      ierr = nf90_put_att(ncid, id_crs, 'comment',                     ' '                 )
      ierr = nf90_put_att(ncid, id_crs, 'value',                       'value is equal to EPSG code')
   end if

   if (ierr_missing /= UG_NOERR) then
      ierr = ierr_missing
      ug_messagestr = 'Missing coordinate reference system. Now using default: '//trim(varname)//' ('//trim(epsgstring)//').'
      ! But continue...
   end if

   ! Check for any remaining native NetCDF errors
   if (ierr /= nf90_noerr) then
      goto 888
   end if

   ierr = UG_NOERR
   return ! Return with success

   ! TODO: AvD: actual epsg value is not put in variable value yet (redef stuff)

888 continue

end function unc_add_coordmapping


!> Add the grid mapping attribute to one or more NetCDF variables.
!!
!! The specified gridmappingname should be an existing variable in the NetCDF dataset.
function unc_add_gridmapping_att(ncid, id_vars, jsferic) result(ierr)
    integer,               intent(in)  :: ncid        !< NetCDF dataset id
    integer, dimension(:), intent(in)  :: id_vars     !< Array of NetCDF variable ids
    integer,               intent(in)  :: jsferic     !< Spherical coords or not (1/0)
    integer                            :: ierr        !< Result status of NetCDF primitives

    integer :: i, n, ierr_
    character(len=30)  :: gridmappingvar              !< Name of grid mapping variable

    gridmappingvar = ' '
    if (jsferic == 0) then
        gridmappingvar = 'projected_coordinate_system'
    else
        gridmappingvar = 'wgs84'
    end if

    ierr = nf90_noerr
    n    = size(id_vars)

    do i=1,n
        ierr_ = nf90_put_att(ncid, id_vars(i), 'grid_mapping', trim(gridmappingvar))
        if (ierr_ /= nf90_noerr) then
            ierr = ierr_
        end if
    end do

end function unc_add_gridmapping_att


!> Writes the unstructured waq geometry to a netCDF file.
!! If file exists, it will be overwritten.
subroutine unc_write_waqgeom(filename, version_full)
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: version_full

    integer :: igeomfile, ierr

    ierr = unc_create(filename, version_full, 0, igeomfile)
    if (ierr /= nf90_noerr) then
        call mess(LEVEL_ERROR, 'Could not create flow geometry file '''//trim(filename)//'''.')
        call check_error(ierr)
        return
    end if

    call unc_write_waqgeom_filepointer(igeomfile)

    ierr = unc_close(igeomfile)
end subroutine unc_write_waqgeom


!> Writes the unstructured waq geometry to an already opened netCDF dataset.
subroutine unc_write_waqgeom_filepointer(igeomfile)
    use wqm_waqgeom
    use wqm_sferic
    use netcdf
    use wqm_partitioninfo

    integer, intent(in) :: igeomfile

    integer, allocatable :: kn3(:), ibndlink(:)

    integer, save :: id_netnodedim, id_netlinkdim, id_netlinkptsdim, &
               id_netelemmaxnodedim, id_netelemdim, id_netelemnode, &             !< Dimensions
               id_netnodex, id_netnodey, id_netnodez, &                           !< Node variables
               id_netlink, id_netlinktype, &                                      !< Link variables
               id_crsvar

    integer, save :: ierr, &
        id_laydim, &
        id_flowelemdim, id_flowelemmaxnodedim, id_flowelemcontourptsdim, &
        id_flowlinkdim, id_flowlinkptsdim, id_erolaydim, &
        id_flowelemxcc, id_flowelemycc, &
        id_flowelemloncc, id_flowelemlatcc, &
        id_flowelemcontourx, id_flowelemcontoury, &
        id_flowelemcontourlon, id_flowelemcontourlat, &
        id_flowelembl, &
        id_flowlink, id_flowlinktype, &
        id_flowlinkxu, id_flowlinkyu, &
        id_flowlinklonu, id_flowlinklatu, &
        id_flowelemdomain, id_flowlinkdomain, &
        id_flowiglobal


    integer :: i, l, numContPts, numNodes
    integer :: jaInDefine = 0
    integer :: jaghost, idmn, iglev

    integer, dimension(:), allocatable :: kn1write
    integer, dimension(:), allocatable :: kn2write

    if (ndxi <= 0) then
        call mess(LEVEL_WARN, 'No flow cells in model, will not write flow geometry.')
        return
    end if


    ! Determine max nr of vertices and contour points
    numNodes   = 0
    numContPts = 0
    do i=1,ndxi
        numNodes   = max(numNodes,   size(nd(i)%nod))
        numContPts = max(numContPts, size(nd(i)%x))
    end do

    ! Put dataset in define mode (possibly again) to add dimensions and variables.
    ierr = nf90_redef(igeomfile)
    if (ierr == nf90_eindefine) jaInDefine = 1 ! Was still in define mode.
    if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
        call mess(LEVEL_ERROR, 'Could not put header in flow geometry file.')
        call check_error(ierr)
        return
    end if

    ierr = nf90_def_dim(igeomfile, 'NetElemNode',         numk,       id_netnodedim)
    ierr = nf90_def_dim(igeomfile, 'nNetLink'   ,         numl,       id_netlinkdim)
    ierr = nf90_def_dim(igeomfile, 'nNetLinkPts',         2,          id_netlinkptsdim)
    ierr = nf90_def_dim(igeomfile, 'nNetElemMaxNode',     nv,         id_netelemmaxnodedim)
    ierr = nf90_def_dim(igeomfile, 'nNetElem',            nump,       id_netelemdim)

    ierr = nf90_def_dim(igeomfile, 'nFlowElem',           ndxi,       id_flowelemdim)

    if (numNodes > 0) then
        ierr = nf90_def_dim(igeomfile, 'nFlowElemMaxNode',    numNodes,   id_flowelemmaxnodedim)
    end if

    ierr = nf90_def_dim(igeomfile, 'nFlowElemContourPts', numContPts, id_flowelemcontourptsdim)

    if (lnx > 0) then
        ierr = nf90_def_dim(igeomfile, 'nFlowLink',           lnx ,       id_flowlinkdim)
        ierr = nf90_def_dim(igeomfile, 'nFlowLinkPts',        2,          id_flowlinkptsdim)
    end if

    ! Node data
    ierr = nf90_def_var(igeomfile, 'NetNode_x', nf90_double, id_netnodedim, id_netnodex)
    ierr = nf90_def_var(igeomfile, 'NetNode_y', nf90_double, id_netnodedim, id_netnodey)
    ierr = nf90_def_var(igeomfile, 'NetNode_z', nf90_double, id_netnodedim, id_netnodez)
    ierr = nf90_put_att(igeomfile, id_netnodex, 'units',         'm')
    ierr = nf90_put_att(igeomfile, id_netnodey, 'units',         'm')
    ierr = nf90_put_att(igeomfile, id_netnodez, 'units',         'm')
    ierr = nf90_put_att(igeomfile, id_netnodex, 'standard_name', 'projection_x_coordinate')
    ierr = nf90_put_att(igeomfile, id_netnodey, 'standard_name', 'projection_y_coordinate')
    ierr = nf90_put_att(igeomfile, id_netnodez, 'standard_name', 'projection_z_coordinate')
    ierr = nf90_put_att(igeomfile, id_netnodex, 'long_name'    , 'x-coordinate of net nodes')
    ierr = nf90_put_att(igeomfile, id_netnodey, 'long_name'    , 'y-coordinate of net nodes')
    ierr = nf90_put_att(igeomfile, id_netnodey, 'long_name'    , 'z-coordinate of net nodes')

    ! Link data
    ierr = nf90_def_var(igeomfile, 'NetLink', nf90_int, (/ id_netlinkptsdim, id_netlinkdim /) , id_netlink)
    ierr = nf90_put_att(igeomfile, id_netlink, 'standard_name', 'netlink')
    ierr = nf90_put_att(igeomfile, id_netlink, 'long_name',     'link between two netnodes')
    ierr = nf90_put_att(igeomfile, id_netlink, 'start_index', 1)

    ! Countour data
    ierr = nf90_def_var(igeomfile, 'NetElemNode', nf90_int, (/ id_netelemmaxnodedim, id_netelemdim /), id_netelemnode)
    ierr = nf90_put_att(igeomfile, id_netelemnode, 'long_name', 'mapping from net cell to net nodes (counterclockwise)')

    ! Flow cells
    ierr = nf90_def_var(igeomfile, 'FlowElem_xcc', nf90_double, id_flowelemdim, id_flowelemxcc)
    ierr = nf90_def_var(igeomfile, 'FlowElem_ycc', nf90_double, id_flowelemdim, id_flowelemycc)
    ierr = unc_addcoordatts(igeomfile, id_flowelemxcc, id_flowelemycc, jsferic)
    ierr = nf90_put_att(igeomfile, id_flowelemxcc, 'long_name'    , 'Flow element circumcenter x')
    ierr = nf90_put_att(igeomfile, id_flowelemycc, 'long_name'    , 'Flow element circumcenter y')
    ierr = nf90_put_att(igeomfile, id_flowelemxcc, 'bounds'       , 'FlowElemContour_x')
    ierr = nf90_put_att(igeomfile, id_flowelemycc, 'bounds'       , 'FlowElemContour_y')

    ! Netcell contours (plot help)
    ! Todo: generalize x/y's to 2/3-D coords everywhere else [Avd]
    ierr = nf90_def_var(igeomfile, 'FlowElemContour_x', nf90_double, (/ id_flowelemcontourptsdim, id_flowelemdim /), id_flowelemcontourx)
    ierr = nf90_def_var(igeomfile, 'FlowElemContour_y', nf90_double, (/ id_flowelemcontourptsdim, id_flowelemdim /), id_flowelemcontoury)
    ierr = unc_addcoordatts(igeomfile, id_flowelemcontourx, id_flowelemcontoury, jsferic)
    ierr = nf90_put_att(igeomfile, id_flowelemcontourx, 'long_name',     'List of x-points forming flow element')
    ierr = nf90_put_att(igeomfile, id_flowelemcontoury, 'long_name',     'List of y-points forming flow element')

    ! Flow elems bottom levels
    ierr = nf90_def_var(igeomfile, 'FlowElem_bl', nf90_double, id_flowelemdim, id_flowelembl)
    ierr = nf90_put_att(igeomfile, id_flowelembl, 'units',         'm')
    ierr = nf90_put_att(igeomfile, id_flowelembl, 'positive',      'up')
    ierr = nf90_put_att(igeomfile, id_flowelembl, 'standard_name', 'sea_floor_depth')
    ierr = nf90_put_att(igeomfile, id_flowelembl, 'long_name',     'Bottom level at flow element''s circumcenter.')

    if (lnx > 0) then
        ierr = nf90_def_var(igeomfile, 'FlowLink',     nf90_int, (/ id_flowlinkptsdim, id_flowlinkdim /) ,   id_flowlink)
        ierr = nf90_put_att(igeomfile, id_flowlink    , 'long_name'    , 'link/interface between two flow elements')

        ierr = nf90_def_var(igeomfile, 'FlowLinkType', nf90_int, (/ id_flowlinkdim /) ,   id_flowlinktype)
        ierr = nf90_put_att(igeomfile, id_flowlinktype, 'long_name'    ,   'type of flowlink')
        ierr = nf90_put_att(igeomfile, id_flowlinktype, 'valid_range'  ,   (/ 1, 2 /))
        ierr = nf90_put_att(igeomfile, id_flowlinktype, 'flag_values'  ,   (/ 1, 2 /))
        ierr = nf90_put_att(igeomfile, id_flowlinktype, 'flag_meanings', 'link_between_1D_flow_elements link_between_2D_flow_elements')

        ierr = nf90_def_var(igeomfile, 'FlowLink_xu',     nf90_double, (/ id_flowlinkdim /) ,   id_flowlinkxu)
        ierr = nf90_def_var(igeomfile, 'FlowLink_yu',     nf90_double, (/ id_flowlinkdim /) ,   id_flowlinkyu)
        ierr = unc_addcoordatts(igeomfile, id_flowlinkxu, id_flowlinkyu, jsferic)
        ierr = nf90_put_att(igeomfile, id_flowlinkxu, 'long_name'    , 'Center coordinate of net link (velocity point).')
        ierr = nf90_put_att(igeomfile, id_flowlinkyu, 'long_name'    , 'Center coordinate of net link (velocity point).')
    end if

    ! Coordinate/grid mapping
    ierr = unc_add_coordmapping(igeomfile, crs)

    ! Add mandatory lon/lat coords too (only if jsferic==0)
    ! BJ: following two lines commented out since QuickPlot will select longitude and latitude based on preference; however, these arrays don't actually contain data yet!
    !ierr = unc_add_lonlat_vars(igeomfile, 'FlowElem',        'cc', (/ id_flowelemdim /),                           id_flowelemloncc,      id_flowelemlatcc,      jsferic)
    !ierr = unc_add_lonlat_vars(igeomfile, 'FlowElemContour', ''  , (/ id_flowelemcontourptsdim, id_flowelemdim /), id_flowelemcontourlon, id_flowelemcontourlat, jsferic)

    ! Add grid_mapping reference to all original coordinate and data variables
    ierr = unc_add_gridmapping_att(igeomfile, &
        (/ id_flowelemxcc, id_flowelemycc, id_flowelemcontourx, id_flowelemcontoury, &
           id_flowelembl /), jsferic)

!    if (lnx > 0) then
!        ierr = unc_add_lonlat_vars(igeomfile, 'FlowLink',        'u' , (/ id_flowlinkdim /),                           id_flowlinklonu,       id_flowlinklatu,       jsferic)
!
!        ! Add grid_mapping reference to all original coordinate and data variables
!        ierr = unc_add_gridmapping_att(igeomfile, &
!            (/ id_flowlinkxu, id_flowlinkyu /), jsferic)
!    end if


!   domain numbers
!    if ( jampi.eq.1 ) then
!       ierr = nf90_def_var(igeomfile, 'FlowElemDomain', nf90_short, id_flowelemdim, id_flowelemdomain)
!       ierr = nf90_put_att(igeomfile, id_flowelemdomain, 'long_name'    ,   'Domain number of flow element')
!       ierr = nf90_def_var(igeomfile, 'FlowLinkDomain', nf90_short, id_flowlinkdim, id_flowlinkdomain)
!       ierr = nf90_put_att(igeomfile, id_flowlinkdomain, 'long_name'    ,   'Domain number of flow link')
!       ierr = nf90_def_var(igeomfile, 'FlowIGlobal', nf90_short, id_flowelemdim, id_flowiglobal)
!       ierr = nf90_put_att(igeomfile, id_flowiglobal, 'long_name'    ,   'Global cell numbering')
!    end if

    ierr = nf90_enddef(igeomfile)
    ! End of writing time-independent flow net data.
    !call readyy('Writing waq geometry data',.05d0)

    ! -- Start data writing (time-independent data) ------------
    ! Net nodes
    ierr = nf90_put_var(igeomfile, id_netnodex, xk(1:numk))
    ierr = nf90_put_var(igeomfile, id_netnodey, yk(1:numk))
    ierr = nf90_put_var(igeomfile, id_netnodez, zk(1:numk))

    ! Net links
    ierr = nf90_put_var(igeomfile, id_netlink, kn(1:2,1:numl))

    ! Net cell contours
    ierr = nf90_put_var(igeomfile, id_netelemnode, netcellnod(1:nv,1:nump))

    ! Flow cell cc coordinates (only 1D + internal 2D)
    ierr = nf90_put_var(igeomfile, id_flowelemxcc, xz(1:ndxi))
    ierr = nf90_put_var(igeomfile, id_flowelemycc, yz(1:ndxi))
    !call readyy('Writing waq geometry data',.15d0)

    ! Flow cell contours
    do i=1,ndxi
        numContPts = size(nd(i)%x)
        ierr = nf90_put_var(igeomfile, id_flowelemcontourx, nd(i)%x, (/ 1, i /), (/ numContPts, 1 /) )
        ierr = nf90_put_var(igeomfile, id_flowelemcontoury, nd(i)%y, (/ 1, i /), (/ numContPts, 1 /) )
    enddo
    !call readyy('Writing flow geometry data',.45d0)

    ! flowcells bottom levels
    ierr = nf90_put_var(igeomfile, id_flowelembl, bl(1:ndxi))
    !call readyy('Writing flow geometry data',.55d0)

    ! Flow links
    ierr = nf90_put_var(igeomfile, id_flowlink,   ln(:,1:lnx))
    do i=1,lnx1D
        ierr = nf90_put_var(igeomfile, id_flowlinktype, (/ 1 /), start = (/ i /))
    end do
    do i=lnx1D+1,lnx
        ierr = nf90_put_var(igeomfile, id_flowlinktype, (/ 2 /), start = (/ i /))
    end do
    !call readyy('Writing flow geometry data',.90d0)

    if (lnx > 0) then
        ! Flow links velocity points
        ierr = nf90_put_var(igeomfile, id_flowlinkxu, xu(1:lnx))
        ierr = nf90_put_var(igeomfile, id_flowlinkyu, yu(1:lnx))
    end if

!!   domain numbers
!    if ( jampi.eq.1 ) then
!!      flow cell domain numbers
!       ierr = nf90_put_var(igeomfile, id_flowelemdomain, idomain(1:ndxi) )
!!      flow link domain numbers
!       do i=1,Lnx
!!         determine if flow link is a ghost link and get domain number and ghost level of link
!          call link_ghostdata(i, my_rank, jaghost, idmn, iglev)
!          ierr = nf90_put_var(igeomfile, id_flowlinkdomain, (/ idmn /), start=(/ i /) )   ! corresponds with partition_get_ghosts
!       end do
!       ierr = nf90_put_var(igeomfile, id_flowiglobal, iglobal(1:ndxi))
!    end if

    !call readyy('Writing flow geometry data',1d0)

    ! Leave the dataset in the same mode as we got it.
    if (jaInDefine == 1) then
        ierr = nf90_redef(igeomfile)
    end if

    !call readyy('Writing flow geometry data',-1d0)
end subroutine unc_write_waqgeom_filepointer


! -- PRIVATE ROUTINES ---------------------------
!> Resets current error status and sets informative message for subsequent
!! errors. Generally called at start of any routine that wants to use
!! routine check_error. The informative message is only shown/used when
!! later check_error's indeed detect an error.
subroutine prepare_error(firstline)
    character(len=*), intent(in) :: firstline !< Informative message for screen/log.

    err_firstline_ = firstline
    err_firsttime_ = .true.
    nerr_          = 0
end subroutine prepare_error

subroutine check_error(ierr, info)
    integer, intent(in)        :: ierr
    character(len=*), intent(in), optional :: info

    character(len=255)         :: infostring

    if (ierr /= nf90_noerr) then
        nerr_ = nerr_ + 1

        ! Optional informative message (appended to NetCDF error string)
        if (present(info)) then
            infostring = '('//trim(info)//')'
        else
            infostring = ' '
        endif

        ! First error line
        if (err_firsttime_) then
            call mess(LEVEL_WARN, err_firstline_)
            err_firsttime_ = .false.
        endif

        ! Actual error line
        call mess(LEVEL_WARN, 'NetCDF error: ', nf90_strerror(ierr), trim(infostring))
    endif
end subroutine check_error


function ug_get_var_attset(ncid, varid, attset) result(ierr)
   integer,                         intent(in)  :: ncid      !< NetCDF dataset id
   integer,                         intent(in)  :: varid     !< NetCDF variable id
   type(ug_nc_attribute), allocatable, intent(out) :: attset(:) !< Resulting attribute set.
   integer                                      :: ierr      !< Result status (UG_NOERR==NF90_NOERR) if successful.

   character(len=64) :: attname
   character(len=1024) :: tmpstr
   integer :: i, j, natts, atttype, attlen, nlen

   ierr = UG_NOERR

   ierr = nf90_inquire_variable(ncid, varid, natts = natts)
   if (ierr /= nf90_noerr) then
      goto 888
   end if

   if (allocated(attset)) deallocate(attset)
   allocate(attset(natts), stat=ierr)

   do i = 1,natts
      ierr = nf90_inq_attname(ncid, varid, i, attname)    ! get attribute name
      ierr = nf90_inquire_attribute(ncid, varid, trim(attname), xtype = atttype, len=attlen) ! get other attribute information

      select case(atttype)
      case(NF90_CHAR)
         tmpstr = ''
         ierr = nf90_get_att(ncid, varid, attname, tmpstr)

         allocate(attset(i)%strvalue(attlen))

         nlen = min(len(tmpstr), attlen)
         do j=1,nlen
            attset(i)%strvalue(j) = tmpstr(j:j)
         end do
      case(NF90_INT)
         allocate(attset(i)%intvalue(attlen))
         ierr = nf90_get_att(ncid, varid, attname, attset(i)%intvalue)
      case(NF90_FLOAT)
         allocate(attset(i)%fltvalue(attlen))
         ierr = nf90_get_att(ncid, varid, attname, attset(i)%fltvalue)
      case(NF90_DOUBLE)
         allocate(attset(i)%dblvalue(attlen))
         ierr = nf90_get_att(ncid, varid, attname, attset(i)%dblvalue)
      case default
         ! NF90_BYTE
         ! NF90_SHORT
         ug_messagestr = 'ug_get_var_attset: error for attribute '''//trim(attname)//'''. Data types byte/short not implemented.'
         ierr = UG_NOTIMPLEMENTED
         goto 888
      end select
      attset(i)%attname = attname
      attset(i)%xtype   = atttype
      attset(i)%len     = attlen
   end do

   return ! Return with success

888 continue

end function ug_get_var_attset


!> Puts a set of NetCDF-attributes onto a given variable.
!!
!! This function is non-UGRID-specific: only used to write grid mapping variables.
!! @see ug_get_var_attset
function ug_put_var_attset(ncid, varid, attset) result(ierr)
   integer,             intent(in)  :: ncid      !< NetCDF dataset id
   integer,             intent(in)  :: varid     !< NetCDF variable id
   type(ug_nc_attribute),  intent(in)  :: attset(:) !< Attribute set to be put into the variable.
   integer                          :: ierr      !< Result status (UG_NOERR==NF90_NOERR) if successful.

   character(len=1024) :: tmpstr
   integer :: i, j, natts, nlen

   ierr = UG_NOERR

   natts = size(attset)

   do i = 1,natts
      select case(attset(i)%xtype)
      case(NF90_CHAR)
         tmpstr = ' '
         nlen = min(len(tmpstr), attset(i)%len)
         do j=1,nlen
            tmpstr(j:j) = attset(i)%strvalue(j)
         end do

         ierr = nf90_put_att(ncid, varid, attset(i)%attname, tmpstr)
      case(NF90_INT)
         ierr = nf90_put_att(ncid, varid, attset(i)%attname, attset(i)%intvalue(1:attset(i)%len))
      case(NF90_FLOAT)
         ierr = nf90_put_att(ncid, varid, attset(i)%attname, attset(i)%fltvalue(1:attset(i)%len))
      case(NF90_DOUBLE)
         ierr = nf90_put_att(ncid, varid, attset(i)%attname, attset(i)%dblvalue(1:attset(i)%len))
      case default
         ! NF90_BYTE
         ! NF90_SHORT
         ug_messagestr = 'ug_put_var_attset: error for attribute '''//trim(attset(i)%attname)//'''. Data types byte/short not implemented.'
         ierr = UG_NOTIMPLEMENTED
      end select
   end do

end function ug_put_var_attset

end module wq_unstruc_netcdf

module wq_ugrid

use netcdf
use coordinate_reference_system

implicit none

! TODO: AvD: GL2: add 'full_grid_output' support, to write 1. face_edge_connectivity; 2. edge_face_connectivity; and possibly more.
! TODO: AvD: GL2: add cell_methods to edge/face data (:mean)
! TODO: AvD: GL2: add integer variable Mesh2_edge_bc with :flag_meanings = "none closed dirichlet"; :flag_values = 0, 1, 2 ;
! TODO: AvD: GL2: move grid_mapping attribute to all data variables, not coordinate variables.

!! Conventions
character(len=6), parameter :: UG_CONV_CF   = 'CF-1.6'      !< Version of CF conventions currently adopted.
character(len=9), parameter :: UG_CONV_UGRID = 'UGRID-0.9'  !< Version of UGRID conventions currently adopted.

!! Meta data
! TODO: AvD: Make data below settable by callers (other companies/packages may want to use this API too).
character(len=*), parameter :: META_INSTITUTION = 'Deltares'
character(len=*), parameter :: META_SOURCE      = 'Delft3D-Flexible Mesh'
character(len=*), parameter :: META_REFERENCES  = 'http://www.deltares.nl'

!! Error codes
integer, parameter :: UG_NOERR                 = NF90_NOERR
integer, parameter :: UG_SOMEERR               = 10 !< Some unspecified error.
integer, parameter :: UG_INVALID_MESHNAME      = 11
integer, parameter :: UG_INVALID_MESHDIMENSION = 12
integer, parameter :: UG_INVALID_DATALOCATION  = 13
integer, parameter :: UG_INVALID_CRS           = 30 !< Invalid/missing coordinate reference system (using default)
integer, parameter :: UG_NOTIMPLEMENTED        = 99

!! Location types
integer, parameter :: UG_LOC_NONE = 0 !< Mesh data location: nowhere at all (include only required mesh locations)
integer, parameter :: UG_LOC_NODE = 1 !< Mesh data location: mesh node (corner)
integer, parameter :: UG_LOC_EDGE = 2 !< Mesh data location: mesh edge
integer, parameter :: UG_LOC_FACE = 4 !< Mesh data location: mesh face
integer, parameter :: UG_LOC_VOL  = 8 !< Mesh data location: mesh volume
integer, parameter :: UG_LOC_ALL2D = UG_LOC_NODE + UG_LOC_EDGE + UG_LOC_FACE !< All three possible 2D locations.

!! Basics
integer, parameter :: dp=kind(1.0d00)
integer, parameter :: maxMessageLen = 1024
character(len=maxMessageLen) :: ug_messagestr !< Placeholder string for storing diagnostic messages. /see{ug_get_message}

type t_ug_file
   character(len=256)   :: fileName
   integer                          :: numMesh
   integer, allocatable             :: ids_meshtopo(:)
   character(len=256), allocatable :: meshNames(:)
end type t_ug_file

!> Structure for storing all variable ids for an unstructured mesh.
type t_ug_meshids
   !
   ! Dimensions:
   !
   integer :: id_nodedim         = -1 !< Dimension ID for nodes.
   integer :: id_edgedim         = -1 !< Dimension ID for edges.
   integer :: id_facedim         = -1 !< Dimension ID for faces.
   integer :: id_maxfacenodesdim = -1 !< Dimension ID for max nr of nodes per face.

   !
   ! Coordinate variables
   !
   integer :: id_nodex           = -1 !< Coordinate variable ID for node x-coordinate.
   integer :: id_nodey           = -1 !< Coordinate variable ID for node y-coordinate.
   integer :: id_nodez           = -1 !< Data       variable ID for node z-coordinate.
   integer :: id_edgex           = -1 !< Coordinate variable ID for edge x-coordinate.
   integer :: id_edgey           = -1 !< Coordinate variable ID for edge y-coordinate.
   integer :: id_edgexbnd        = -1 !<            variable ID for edge boundaries' x-coordinate.
   integer :: id_edgeybnd        = -1 !<            variable ID for edge boundaries' y-coordinate.
   integer :: id_facex           = -1 !< Coordinate variable ID for face x-coordinate.
   integer :: id_facey           = -1 !< Coordinate variable ID for face y-coordinate.
   integer :: id_facexbnd        = -1 !<            variable ID for face boundaries' x-coordinate.
   integer :: id_faceybnd        = -1 !<            variable ID for face boundaries' y-coordinate.

   !
   ! Topology variables
   !
   integer :: id_meshtopo        = -1 !< Top-level variable ID for mesh topology, collects all related variable names via attributes.
   integer :: id_edgenodes       = -1 !< Variable ID for edge-to-node mapping table.
   integer :: id_facenodes       = -1 !< Variable ID for face-to-node mapping table.

end type t_ug_meshids

   contains

!> Returns the latest message string from this module.
!!
!! Use this when a previous function call has returned a nonzero error status.
!! Call this function only once for each returned error: message buffer will be cleared on each call.
integer function ug_get_message(str) result(ierr)
   character(len=*), intent(out) :: str !< String variable in which the message will be stored.

   ierr = UG_NOERR
   str = trim(ug_messagestr)

   ! Directly clear the message buffer, to prevent false messages for future errors.
   ug_messagestr = ' '

end function ug_get_message


!> Puts global attributes in an open NetCDF data set.
!! This includes: institution, Conventions, etc.
function ug_addglobalatts(ncid, program_version) result(ierr)
   !use unstruc_model, only : md_ident
   integer, intent(in) :: ncid  !< Already opened NetCDF id to put global attributes into.
   character(len=*), intent(in) :: program_version !< Program name and version which uses this ugrid format.
   integer          :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.

   character(len=8)  :: cdate
   character(len=10) :: ctime
   character(len=5)  :: czone
   integer :: wasInDefine

   ierr = UG_NOERR
   wasInDefine = 0

   ierr = nf90_redef(ncid)
   if (ierr == nf90_eindefine) wasInDefine = 1 ! Was still in define mode.
   if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
      ! TODO: AvD: call mess(LEVEL_ERROR, 'Could not put global attributes in NetCDF #', ierr)
      return
   end if

   ierr = nf90_put_att(ncid, nf90_global,  'institution', trim(META_INSTITUTION))
   ierr = nf90_put_att(ncid, nf90_global,  'references',  trim(META_REFERENCES))
   ierr = nf90_put_att(ncid, nf90_global,  'source',      trim(META_SOURCE)//' '//trim(program_version)//'. Model: ') !//trim(md_ident))

   call date_and_time(cdate, ctime, czone)
   ierr = nf90_put_att(ncid, nf90_global,  'history', &
      'Created on '//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)// &
      ', '//trim(META_SOURCE))

   ierr = nf90_put_att(ncid, nf90_global,  'Conventions', trim(UG_CONV_CF)//' '//trim(UG_CONV_UGRID))

   ! Leave the dataset in the same mode as we got it.
   if (wasInDefine == 0) then
      ierr = nf90_enddef(ncid)
   end if
end function ug_addglobalatts


!> Gets all NetCDF-attributes for a given variable.
!!
!! This function is non-UGRID-specific: only used to read grid mapping variables.
!! @see ug_put_var_attset
function ug_get_var_attset(ncid, varid, attset) result(ierr)
   integer,                         intent(in)  :: ncid      !< NetCDF dataset id
   integer,                         intent(in)  :: varid     !< NetCDF variable id
   type(ug_nc_attribute), allocatable, intent(out) :: attset(:) !< Resulting attribute set.
   integer                                      :: ierr      !< Result status (UG_NOERR==NF90_NOERR) if successful.

   character(len=64) :: attname
   character(len=1024) :: tmpstr
   integer :: i, j, natts, atttype, attlen, nlen

   ierr = UG_NOERR

   ierr = nf90_inquire_variable(ncid, varid, natts = natts)
   if (ierr /= nf90_noerr) then
      goto 888
   end if

   if (allocated(attset)) deallocate(attset)
   allocate(attset(natts), stat=ierr)

   do i = 1,natts
      ierr = nf90_inq_attname(ncid, varid, i, attname)    ! get attribute name
      ierr = nf90_inquire_attribute(ncid, varid, trim(attname), xtype = atttype, len=attlen) ! get other attribute information

      select case(atttype)
      case(NF90_CHAR)
         tmpstr = ''
         ierr = nf90_get_att(ncid, varid, attname, tmpstr)

         allocate(attset(i)%strvalue(attlen))

         nlen = min(len(tmpstr), attlen)
         do j=1,nlen
            attset(i)%strvalue(j) = tmpstr(j:j)
         end do
      case(NF90_INT)
         allocate(attset(i)%intvalue(attlen))
         ierr = nf90_get_att(ncid, varid, attname, attset(i)%intvalue)
      case(NF90_FLOAT)
         allocate(attset(i)%fltvalue(attlen))
         ierr = nf90_get_att(ncid, varid, attname, attset(i)%fltvalue)
      case(NF90_DOUBLE)
         allocate(attset(i)%dblvalue(attlen))
         ierr = nf90_get_att(ncid, varid, attname, attset(i)%dblvalue)
      case default
         ! NF90_BYTE
         ! NF90_SHORT
         ug_messagestr = 'ug_get_var_attset: error for attribute '''//trim(attname)//'''. Data types byte/short not implemented.'
         ierr = UG_NOTIMPLEMENTED
         goto 888
      end select
      attset(i)%attname = attname
      attset(i)%xtype   = atttype
      attset(i)%len     = attlen
   end do

   return ! Return with success

888 continue

end function ug_get_var_attset


!> Puts a set of NetCDF-attributes onto a given variable.
!!
!! This function is non-UGRID-specific: only used to write grid mapping variables.
!! @see ug_get_var_attset
function ug_put_var_attset(ncid, varid, attset) result(ierr)
   integer,             intent(in)  :: ncid      !< NetCDF dataset id
   integer,             intent(in)  :: varid     !< NetCDF variable id
   type(ug_nc_attribute),  intent(in)  :: attset(:) !< Attribute set to be put into the variable.
   integer                          :: ierr      !< Result status (UG_NOERR==NF90_NOERR) if successful.

   character(len=1024) :: tmpstr
   integer :: i, j, natts, nlen

   ierr = UG_NOERR

   natts = size(attset)

   do i = 1,natts
      select case(attset(i)%xtype)
      case(NF90_CHAR)
         tmpstr = ' '
         nlen = min(len(tmpstr), attset(i)%len)
         do j=1,nlen
            tmpstr(j:j) = attset(i)%strvalue(j)
         end do

         ierr = nf90_put_att(ncid, varid, attset(i)%attname, tmpstr)
      case(NF90_INT)
         ierr = nf90_put_att(ncid, varid, attset(i)%attname, attset(i)%intvalue(1:attset(i)%len))
      case(NF90_FLOAT)
         ierr = nf90_put_att(ncid, varid, attset(i)%attname, attset(i)%fltvalue(1:attset(i)%len))
      case(NF90_DOUBLE)
         ierr = nf90_put_att(ncid, varid, attset(i)%attname, attset(i)%dblvalue(1:attset(i)%len))
      case default
         ! NF90_BYTE
         ! NF90_SHORT
         ug_messagestr = 'ug_put_var_attset: error for attribute '''//trim(attset(i)%attname)//'''. Data types byte/short not implemented.'
         ierr = UG_NOTIMPLEMENTED
      end select
   end do

end function ug_put_var_attset


! -- COORDINATES ------------
!> Adds coordinate attributes according to CF conventions, based on given coordinate projection type.
!! Non-standard attributes (such as long_name) should be set elsewhere.
function ug_addcoordatts(ncid, id_varx, id_vary, crs) result(ierr)
   integer,      intent(in) :: ncid     !< NetCDF dataset id
   integer,      intent(in) :: id_varx  !< NetCDF 'x' variable id
   integer,      intent(in) :: id_vary  !< NetCDF 'y' variable id
   type(t_crs),  intent(in) :: crs      !< Coordinate reference system for the x/y-coordinates variables.
   integer                  :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.


   ierr = UG_NOERR

   if (crs%epsg_code == 4326) then
      ierr = nf90_put_att(ncid, id_varx, 'units',       'degrees_east')
      ierr = nf90_put_att(ncid, id_vary, 'units',       'degrees_north')
      ierr = nf90_put_att(ncid, id_varx, 'standard_name', 'longitude')
      ierr = nf90_put_att(ncid, id_vary, 'standard_name', 'latitude')
      ierr = nf90_put_att(ncid, id_varx, 'long_name'   , 'longitude')
      ierr = nf90_put_att(ncid, id_vary, 'long_name'   , 'latitude')
   else
      ierr = nf90_put_att(ncid, id_varx, 'units',       'm')
      ierr = nf90_put_att(ncid, id_vary, 'units',       'm')
      ierr = nf90_put_att(ncid, id_varx, 'standard_name', 'projection_x_coordinate')
      ierr = nf90_put_att(ncid, id_vary, 'standard_name', 'projection_y_coordinate')
      ierr = nf90_put_att(ncid, id_varx, 'long_name'   , 'x')
      ierr = nf90_put_att(ncid, id_vary, 'long_name'   , 'y')
   end if
end function ug_addcoordatts


!> Adds coordinate mapping attributes according to CF conventions, based on jsferic.
!! Attributes are put in a scalar integer variable.
function ug_add_coordmapping(ncid, crs) result(ierr)
   integer,      intent(in) :: ncid  !< NetCDF dataset id
   type(t_crs),  intent(in) :: crs   !< Coordinate reference system that was used for the coordinate mapping.
   integer                  :: ierr  !< Result status (UG_NOERR==NF90_NOERR) if successful.

   integer :: id_crs
   integer :: epsg
   integer :: ierr_missing
   character(len=11) :: epsgstring
   character(len=30) :: varname  !< Name of the created grid mapping variable.

   ierr = UG_NOERR
   ierr_missing = UG_NOERR ! Store whether crs was missing (and default was used)

   epsgstring = ' '

   ! TODO: AvD: Name and params are now hardcoded globally based on a single jsferic=0/1 flag.
   ! generalize this!

   varname = ' '
   if (len_trim(crs%varname) > 0) then
      varname = crs%varname
   else if (crs%epsg_code == 4326) then
      ierr_missing = UG_INVALID_CRS
      varname = 'wgs84'
   else
      ierr_missing = UG_INVALID_CRS
      varname = 'projected_coordinate_system'
   end if

   ierr = nf90_inq_varid(ncid, trim(varname), id_crs)
   if (ierr == nf90_noerr) then
      ! A variable with that name already exists. Return without error.
      ierr = UG_NOERR
      goto 888
   end if

   ierr = nf90_def_var(ncid, trim(varname), nf90_int, id_crs)

   if (allocated(crs%attset)) then
      ierr = ug_put_var_attset(ncid, id_crs, crs%attset)
   elseif (crs%epsg_code == 4326) then
      ierr_missing = UG_INVALID_CRS
      epsg      = 4326
      epsgstring = 'EPSG:4326'
      ierr = nf90_put_att(ncid, id_crs, 'name',                       'WGS84'             ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'epsg',                       epsg                ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'grid_mapping_name',          'latitude_longitude') ! CF
      ierr = nf90_put_att(ncid, id_crs, 'longitude_of_prime_meridian', 0d0                ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'semi_major_axis',             6378137d0                               ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'semi_minor_axis',             6356752.314245d0   ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'inverse_flattening',          298.257223563d0    ) ! CF
!      ierr = nf90_put_att(ncid, id_crs, 'proj4_params',                ' '                ) ! ADAGUC
      ierr = nf90_put_att(ncid, id_crs, 'epsg_code',                   trim(epsgstring)   ) ! ADAGUC
!      ierr = nf90_put_att(ncid, id_crs, 'projection_name',             ' '                ) ! ADAGUC
!      ierr = nf90_put_att(ncid, id_crs, 'wkt',                         ' '                ) ! WKT
!      ierr = nf90_put_att(ncid, id_crs, 'comment',                     ' '                )
      ierr = nf90_put_att(ncid, id_crs, 'value',                       'value is equal to EPSG code')
   else
      ierr_missing = UG_INVALID_CRS
      epsg      = 28992
      epsgstring = 'EPSG:28992'
      ierr = nf90_put_att(ncid, id_crs, 'name',                        'Unknown projected' ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'epsg',                        epsg                ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'grid_mapping_name',           'Unknown projected' ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'longitude_of_prime_meridian', 0d0                 ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'semi_major_axis',             6378137d0                              ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'semi_minor_axis',             6356752.314245d0    ) ! CF
      ierr = nf90_put_att(ncid, id_crs, 'inverse_flattening',          298.257223563d0     ) ! CF
!      ierr = nf90_put_att(ncid, id_crs, 'proj4_params',                ' '                 ) ! ADAGUC
      ierr = nf90_put_att(ncid, id_crs, 'epsg_code',                   trim(epsgstring)    ) ! ADAGUC
!      ierr = nf90_put_att(ncid, id_crs, 'projection_name',             ' '                 ) ! ADAGUC
!      ierr = nf90_put_att(ncid, id_crs, 'wkt',                         ' '                 ) ! WKT
!      ierr = nf90_put_att(ncid, id_crs, 'comment',                     ' '                 )
      ierr = nf90_put_att(ncid, id_crs, 'value',                       'value is equal to EPSG code')
   end if

   if (ierr_missing /= UG_NOERR) then
      ierr = ierr_missing
      ug_messagestr = 'Missing coordinate reference system. Now using default: '//trim(varname)//' ('//trim(epsgstring)//').'
      ! But continue...
   end if

   ! Check for any remaining native NetCDF errors
   if (ierr /= nf90_noerr) then
      goto 888
   end if

   ierr = UG_NOERR
   return ! Return with success

   ! TODO: AvD: actual epsg value is not put in variable value yet (redef stuff)

888 continue

end function ug_add_coordmapping


!> Add the grid mapping attribute to one or more NetCDF variables.
function ug_put_gridmapping_att(ncid, id_vars, crs) result(ierr)
   integer,               intent(in) :: ncid     !< NetCDF dataset id
   integer, dimension(:), intent(in) :: id_vars  !< Array of NetCDF variable ids
   type(t_crs),           intent(in) :: crs      !< Projection type that was used for the coordinate mapping.
   integer                           :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.

   integer :: i, n
   character(len=30)  :: gridmappingvar           !< Name of grid mapping variable

   ierr = UG_SOMEERR

   gridmappingvar = ' '
   if (.true.) then
      gridmappingvar = crs%varname
   else if (crs%epsg_code == 4326) then
      gridmappingvar = 'wgs84'
   else
      gridmappingvar = 'projected_coordinate_system'
   end if

   ierr = UG_NOERR
   n   = size(id_vars)

   do i=1,n
      ierr = nf90_put_att(ncid, id_vars(i), 'grid_mapping', trim(gridmappingvar))
      if (ierr /= nf90_noerr) then
         goto 888
      end if
   end do

   ierr = UG_NOERR
   return ! Return with success

888 continue

end function ug_put_gridmapping_att



!> Checks whether a specific mesh data location is inside a location specification code.
!! Mesh data may be specified on nodes (corners), edges and faces, encoded as a sum of location codes.
!! Used to decide which optional mesh topology data should be written to file, and which not.
!! \see UG_LOC_NODE, UG_LOC_EDGE, UG_LOC_FACE
function ug_checklocation(dataLocsCode, locType) result(is_used)
   integer, intent(in) :: dataLocsCode  !< Integer code describing on which topological locations data is/will be used.
   integer, intent(in) :: locType       !< Integer location code to test on (one of UG_LOC_NODE, UG_LOC_EDGE, UG_LOC_FACE, UG_LOC_VOL).
   logical             :: is_used       !< Returns whether specified locType is contained in dataLocsCode.

   ! Perform logical AND to determine whether locType is inside dataLocs 'set'.
   is_used = iand(dataLocsCode, locType) == locType
end function ug_checklocation


!> Write mesh topoplogy
function ug_write_meshtopology(ncid, meshids, meshName, dim, dataLocsCode) result(ierr)
   integer,          intent(in) :: ncid         !< NetCDF dataset id
   type(t_ug_meshids), intent(inout) :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
   character(len=*), intent(in) :: meshName     !< Name for the mesh variable, also used as prefix for all related entities.
   integer,          intent(in) :: dim          !< Dimensionality of the mesh (1/2/3)
   integer,          intent(in) :: dataLocsCode !< Specifies at which mesh locations data may be specified.
   integer                      :: ierr         !< Result status (UG_NOERR==NF90_NOERR) if successful.

   character(len=len_trim(meshName)) :: prefix
   character(len=256) :: buffer

   ierr = UG_SOMEERR

   prefix = trim(meshName)

   if (len_trim(meshName) == 0) then
      ierr = UG_INVALID_MESHNAME
      goto 888
   end if

   if (dim <=0 .or. dim > 3) then
      ierr = UG_INVALID_MESHDIMENSION
      goto 888
   end if

   ! TODO: AvD: check for conflicts between dataLocsCode and dim (e.g. FACE data in a 1D model)

   ! Define the mesh topology variable
   ierr = nf90_def_var(ncid, prefix, nf90_int, meshids%id_meshtopo)

   ! Attributes for all dimensions:
   ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'cf_role',       'mesh_topology')
   write(buffer, '(a,i0,a)') 'Topology data of ', dim, 'D network'
   ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'long_name',      trim(buffer))
   ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'dimension',      dim)
   ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'node_coordinates', prefix//'_node_x '//prefix//'_node_y')

   ! 1D: required, 2D: optionally required if data there
   if (dim == 1 .or. ug_checklocation(dataLocsCode, UG_LOC_EDGE)) then
      ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'edge_node_connectivity', prefix//'_edge_nodes')
   end if

   ! Optionally required if data there:
   if (ug_checklocation(dataLocsCode, UG_LOC_EDGE)) then
      ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'edge_coordinates',      prefix//'_edge_x '//prefix//'_edge_y')
   end if

   ! 2D: required, 3D: optionally required if data there:
   if (dim == 2 .or. ug_checklocation(dataLocsCode, UG_LOC_FACE)) then
      ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'face_node_connectivity', prefix//'_face_nodes')
   end if

   ! Optionally required if data there:
   if (ug_checklocation(dataLocsCode, UG_LOC_FACE)) then
      ierr = nf90_put_att(ncid, meshids%id_meshtopo, 'face_coordinates', prefix//'_face_x '//prefix//'_face_y')
   end if
   ! TODO: AvD: UNST-476:
   ! Consider adding 1. face_edge_connectivity, 2. edge_face_connectivity.

   if (dim >= 3) then
      ierr = UG_NOTIMPLEMENTED
      goto 888
   end if

   ! Check for any remaining native NetCDF errors
   if (ierr /= nf90_noerr) then
      goto 888
   end if

   ierr = UG_NOERR
   return ! Return with success

888 continue


end function ug_write_meshtopology




!> Defines a new variable in an existing dataset.
!! Does not write the actual data yet.
function ug_def_var(ncid, meshids, id_var, id_dims, itype, iloc, mesh_name, var_name, standard_name, long_name, unit, cell_method, ifill, dfill) result(ierr)
   integer,                 intent(in)    :: ncid          !< NetCDF dataset id
   type(t_ug_meshids),      intent(in)    :: meshids       !< Set of NetCDF-ids for all mesh geometry arrays.
   integer,                 intent(out)   :: id_var        !< Created NetCDF variable id.
   integer, dimension(:),   intent(in)    :: id_dims       !< NetCDF dimension ids for this variable. Example: (/ id_edgedim /) for scalar data on edges, or (/ 2, id_facedim /) for vector data on faces.
   integer,                 intent(in)    :: itype         !< The variable type expressed in one of the basic nf90_* types, e.g., nf90_double.
   integer,                 intent(in)    :: iloc          !< Specifies at which unique mesh location data will be specified.
   character(len=*),        intent(in)    :: mesh_name     !< Name for the mesh variable, also used as prefix for all related entities.
   character(len=*),        intent(in)    :: var_name      !< Name for the new data variable.
   character(len=*),        intent(in)    :: standard_name !< Standard name (CF-compliant) for 'standard_name' attribute in this variable.
   character(len=*),        intent(in)    :: long_name     !< Long name for 'long_name' attribute in this variable (use empty string if not wanted).
   character(len=*),        intent(in)    :: unit          !< Unit of this variable (CF-compliant) (use empty string for dimensionless quantities).
   character(len=*),        intent(in)    :: cell_method   !< Cell method for the spatial dimension (i.e., for edge/face/volume), value should be one of 'point', 'mean', etc. (See CF) (empty string if not relevant).
   integer, optional,       intent(in)    :: ifill         !< (Optional) Integer fill value.
   double precision, optional, intent(in) :: dfill         !< (Optional) Double precision fill value.
   integer                                :: ierr          !< Result status (UG_NOERR==NF90_NOERR) if successful.

   character(len=len_trim(mesh_name)) :: prefix

   ierr = UG_SOMEERR

   prefix = trim(mesh_name)

   ierr = nf90_def_var(ncid, prefix//'_'//trim(var_name), itype, id_dims, id_var)
   ierr = nf90_put_att(ncid, id_var, 'mesh',   trim(mesh_name))
   select case (iloc)
   case (UG_LOC_NODE)
      ierr = nf90_put_att(ncid, id_var, 'location',    'node')
      ierr = nf90_put_att(ncid, id_var, 'coordinates', prefix//'_node_x '//prefix//'_node_y')
   case (UG_LOC_EDGE)
      ierr = nf90_put_att(ncid, id_var, 'location',    'edge')
      ierr = nf90_put_att(ncid, id_var, 'coordinates', prefix//'_edge_x '//prefix//'_edge_y')
      if (len_trim(cell_method) > 0) then
         ierr = nf90_put_att(ncid, id_var, 'cell_methods', 'n'//prefix//'_edge: '//trim(cell_method))
      end if
   case (UG_LOC_FACE)
      ierr = nf90_put_att(ncid, id_var, 'location',    'face')
      ierr = nf90_put_att(ncid, id_var, 'coordinates', prefix//'_face_x '//prefix//'_face_y')
      if (len_trim(cell_method) > 0) then
         ierr = nf90_put_att(ncid, id_var, 'cell_methods', 'n'//prefix//'_face: '//trim(cell_method))
      end if
   case (UG_LOC_VOL)
      ierr = UG_NOTIMPLEMENTED
      goto 888
   case default
      ierr = UG_INVALID_DATALOCATION
      goto 888
   end select

   ierr = nf90_put_att(ncid, id_var, 'standard_name', trim(standard_name))
   ierr = nf90_put_att(ncid, id_var, 'long_name'    , trim(long_name))
   ierr = nf90_put_att(ncid, id_var, 'units'        , trim(unit))

   if (itype == nf90_int .and. present(ifill)) then
      ierr = nf90_put_att(ncid, id_var, '_FillValue'   , ifill)
   end if
   if (itype == nf90_double .and. present(dfill)) then
      ierr = nf90_put_att(ncid, id_var, '_FillValue'   , dfill)
   end if

   ! Check for any remaining native NetCDF errors
   if (ierr /= nf90_noerr) then
      goto 888
   end if

   ierr = UG_NOERR
   return ! Return with success

888 continue


end function ug_def_var

!> Writes a complete mesh geometry to an open NetCDF data set based on separate arrays with all mesh data..
!! The mesh geometry is the required starting point for all variables/data defined ON that mesh.
!! This function requires all mesh arrays as input, for the derived type-based function, see ug_write_mesh_struct.
function ug_write_mesh_arrays(ncid, meshids, meshName, dim, dataLocs, numNode, numEdge, numFace, &
                              edge_nodes, face_nodes, xn, yn, xe, ye, xf, yf, &
                              crs, imiss, dmiss) result(ierr)
   integer,          intent(in) :: ncid     !< NetCDF dataset id, should be already open and ready for writing.
   type(t_ug_meshids), intent(inout) :: meshids !< Set of NetCDF-ids for all mesh geometry arrays.
   character(len=*), intent(in) :: meshName !< Name for the mesh variable, also used as prefix for all related entities.
   integer,          intent(in) :: dim      !< Dimensionality of the mesh (1/2/3)
   integer,          intent(in) :: dataLocs !< Integer code describing on which topological locations data is/will be used.
   integer,          intent(in) :: numNode  !< Number of nodes in the mesh.
   integer,          intent(in) :: numEdge  !< Number of edges in the mesh.
   integer,          intent(in) :: numFace  !< Number of faces in the mesh.
   integer,          intent(in) :: edge_nodes(:,:) !< Edge-to-node mapping array.
   integer,          intent(in) :: face_nodes(:,:) !< Face-to-node mapping array.
   real(kind=dp),    intent(in) :: xn(:), yn(:) !< x,y-coordinates of the mesh nodes.
   real(kind=dp),    intent(in) :: xe(:), ye(:) !< representative x,y-coordinates of the mesh edges.
   real(kind=dp),    intent(in) :: xf(:), yf(:) !< representative x,y-coordinates of the mesh faces.
   type(t_crs),      intent(in) :: crs      !< Coordinate reference system for input coordinates
   integer,          intent(in) :: imiss    !< Fill value used for integer values (e.g. in edge/face_nodes arrays).
   real(kind=dp),    intent(in) :: dmiss    !< Fill value used for double precision values (e.g. in face_x_bnd variable).
   integer                      :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.

   integer :: id_twodim

   real(kind=dp), allocatable :: edgexbnd(:,:), edgeybnd(:,:), facexbnd(:,:), faceybnd(:,:)
   integer :: maxnv, k, n
   character(len=len_trim(meshName)) :: prefix
   integer :: wasInDefine

   ierr = UG_SOMEERR
   wasInDefine = 0

   ierr = nf90_redef(ncid)
   if (ierr == nf90_eindefine) wasInDefine = 1 ! Was still in define mode.

   prefix=trim(meshName)

   ierr = ug_write_meshtopology(ncid, meshids, meshName, dim, dataLocs)
   if (ierr /= UG_NOERR) then
      goto 888
   end if


   ! Dimensions
   ierr = nf90_def_dim(ncid, 'n'//prefix//'_node',        numNode,   meshids%id_nodedim)
   ierr = nf90_def_dim(ncid, 'n'//prefix//'_edge',        numEdge,   meshids%id_edgedim)
   ierr = nf90_def_dim(ncid, 'Two',                         2,       id_twodim)! TODO: AvD: duplicates!
   if (dim == 2 .or. ug_checklocation(dataLocs, UG_LOC_FACE)) then
      maxnv = size(face_nodes, 1)
      ierr = nf90_def_dim(ncid, 'n'//prefix//'_face',        numFace,   meshids%id_facedim)
      ierr = nf90_def_dim(ncid, 'num_rows'//prefix//'_face_nodes', maxnv,   meshids%id_maxfacenodesdim)
   end if

   ierr = ug_add_coordmapping(ncid, crs)

   ! Nodes
   ierr = nf90_def_var(ncid, prefix//'_node_x', nf90_double, meshids%id_nodedim, meshids%id_nodex)
   ierr = nf90_def_var(ncid, prefix//'_node_y', nf90_double, meshids%id_nodedim, meshids%id_nodey)
   ierr = ug_addcoordatts(ncid, meshids%id_nodex, meshids%id_nodey, crs)
   ierr = nf90_put_att(ncid, meshids%id_nodex, 'long_name',    'x-coordinate of mesh nodes')
   ierr = nf90_put_att(ncid, meshids%id_nodey, 'long_name',    'y-coordinate of mesh nodes')


   ! Add mandatory lon/lat coords too (only if jsferic==0)
   ! TODO: AvD ierr = ug_add_lonlat_vars(inetfile, 'NetNode', '', (/ id_netnodedim /), id_netnodelon, id_netnodelat, jsferic)

   ierr = ug_def_var(ncid, meshids, meshids%id_nodez, (/ meshids%id_nodedim /), nf90_double, UG_LOC_NODE, &
                     meshName, 'node_z', 'altitude', 'z-coordinate of mesh nodes', 'm', '', dfill=dmiss)
!   ierr = nf90_put_att(ncid, meshids%id_nodez, 'positive',       'up') ! Not allowed for non-coordinate variables.

   ierr = ug_put_gridmapping_att(ncid, (/ meshids%id_nodez /), crs) ! Not: meshids%id_nodex, meshids%id_nodey,



   ! Edges
   if (dim == 1 .or. ug_checklocation(dataLocs, UG_LOC_EDGE)) then
      ierr = nf90_def_var(ncid, prefix//'_edge_nodes', nf90_int, (/ id_twodim, meshids%id_edgedim /) , meshids%id_edgenodes)
      ierr = nf90_put_att(ncid, meshids%id_edgenodes, 'cf_role',   'edge_node_connectivity')
      ierr = nf90_put_att(ncid, meshids%id_edgenodes, 'long_name',  'Maps every edge to the two nodes that it connects.')
      ierr = nf90_put_att(ncid, meshids%id_edgenodes, 'start_index',  1)
      ierr = nf90_put_att(ncid, meshids%id_edgenodes, '_FillValue',  imiss)
   end if
   if (ug_checklocation(dataLocs, UG_LOC_EDGE)) then
      ierr = nf90_def_var(ncid, prefix//'_edge_x', nf90_double, meshids%id_edgedim, meshids%id_edgex)
      ierr = nf90_def_var(ncid, prefix//'_edge_y', nf90_double, meshids%id_edgedim, meshids%id_edgey)
      ierr = ug_addcoordatts(ncid, meshids%id_edgex, meshids%id_edgey, crs)
      ierr = nf90_put_att(ncid, meshids%id_edgex, 'long_name',    'Characteristic longitude of mesh edge (e.g. midpoint of the edge).')
      ierr = nf90_put_att(ncid, meshids%id_edgey, 'long_name',    'Characteristic latitude of mesh edge (e.g. midpoint of the edge).')

      ierr = nf90_put_att(ncid, meshids%id_edgex, 'bounds',    prefix//'_edge_x_bnd')
      ierr = nf90_put_att(ncid, meshids%id_edgey, 'bounds',    prefix//'_edge_y_bnd')

      ierr = nf90_def_var(ncid, prefix//'_edge_x_bnd', nf90_double, (/ id_twodim, meshids%id_edgedim /), meshids%id_edgexbnd)
      ierr = nf90_def_var(ncid, prefix//'_edge_y_bnd', nf90_double, (/ id_twodim, meshids%id_edgedim /), meshids%id_edgeybnd)
      ierr = ug_addcoordatts(ncid, meshids%id_edgexbnd, meshids%id_edgeybnd, crs)
      ierr = nf90_put_att(ncid, meshids%id_edgexbnd, 'long_name',    'Horizontal coordinate bounds of 2D mesh edge (i.e. end point coordinates)."')
      ierr = nf90_put_att(ncid, meshids%id_edgeybnd, 'long_name',    'Vertical coordinate bounds of 2D mesh edge (i.e. end point coordinates)."')

   end if
   !ierr = ug_def_var(ncid, meshids, meshName, prefix//'_u1', nf90_double, UG_LOC_EDGE, 'mean', (/ id_nodedim /), id_nodez)
   !ierr = nf90_put_att(ncid, id_nodez, 'units',          'm')
   !ierr = nf90_put_att(ncid, id_nodez, 'standard_name',  'altitude')
   !ierr = nf90_put_att(ncid, id_nodez, 'long_name',      'z-coordinate of mesh nodes')

   !ierr = nf90_def_var(inetfile, 'NetLinkType', nf90_int, id_netlinkdim, id_netlinktype)
   !ierr = nf90_put_att(inetfile, id_netlinktype, 'long_name',    'type of netlink')
   !ierr = nf90_put_att(inetfile, id_netlinktype, 'valid_range',   (/ 0, 2 /))
   !ierr = nf90_put_att(inetfile, id_netlinktype, 'flag_values',   (/ 0, 1, 2 /))
   !ierr = nf90_put_att(inetfile, id_netlinktype, 'flag_meanings', 'closed_link_between_2D_nodes link_between_1D_nodes link_between_2D_nodes')

   ! Faces
   if (dim == 2 .or. ug_checklocation(dataLocs, UG_LOC_FACE)) then
      ierr = nf90_def_var(ncid, prefix//'_face_nodes', nf90_int, (/ meshids%id_maxfacenodesdim, meshids%id_facedim /) , meshids%id_facenodes)
      ierr = nf90_put_att(ncid, meshids%id_facenodes, 'cf_role',   'face_node_connectivity')
      ierr = nf90_put_att(ncid, meshids%id_facenodes, 'long_name',  'Maps every face to it''s corner nodes (anticlockwise).')
      ierr = nf90_put_att(ncid, meshids%id_facenodes, 'start_index',  1)
      ierr = nf90_put_att(ncid, meshids%id_facenodes, '_FillValue',  imiss)
   end if
   if (ug_checklocation(dataLocs, UG_LOC_FACE)) then
      ierr = nf90_def_var(ncid, prefix//'_face_x', nf90_double, meshids%id_facedim, meshids%id_facex)
      ierr = nf90_def_var(ncid, prefix//'_face_y', nf90_double, meshids%id_facedim, meshids%id_facey)
      ierr = ug_addcoordatts(ncid, meshids%id_facex, meshids%id_facey, crs)
      ierr = nf90_put_att(ncid, meshids%id_facex, 'long_name',    'Characteristic horizontal coordinate of mesh face.')
      ierr = nf90_put_att(ncid, meshids%id_facey, 'long_name',    'Characteristic vertical coordinate of mesh face.')
      ierr = nf90_put_att(ncid, meshids%id_facex, 'bounds',    prefix//'_face_x_bnd')
      ierr = nf90_put_att(ncid, meshids%id_facey, 'bounds',    prefix//'_face_y_bnd')

      ierr = nf90_def_var(ncid, prefix//'_face_x_bnd', nf90_double, (/ meshids%id_maxfacenodesdim, meshids%id_facedim /), meshids%id_facexbnd)
      ierr = nf90_def_var(ncid, prefix//'_face_y_bnd', nf90_double, (/ meshids%id_maxfacenodesdim, meshids%id_facedim /), meshids%id_faceybnd)
      ierr = ug_addcoordatts(ncid, meshids%id_facexbnd, meshids%id_faceybnd, crs)
      ierr = nf90_put_att(ncid, meshids%id_facexbnd, 'long_name',    'Horizontal coordinate bounds of 2D mesh face (i.e. corner coordinates)."')
      ierr = nf90_put_att(ncid, meshids%id_faceybnd, 'long_name',    'Vertical coordinate bounds of 2D mesh face (i.e. corner coordinates)."')
      ierr = nf90_put_att(ncid, meshids%id_facexbnd, '_FillValue',  dmiss)
      ierr = nf90_put_att(ncid, meshids%id_faceybnd, '_FillValue',  dmiss)

   end if
! TODO: AvD: add the following (resolution may be difficult)
!>       :geospatial_lat_min = 52.9590188916822 ;
!>       :geospatial_lat_max = 53.8746171549558 ;
!>       :geospatial_lat_units = "degrees_north" ;
!>       :geospatial_lat_resolution = "on average     370.50 meters" ;
!>       :geospatial_lon_min = 6.37848435307356 ;
!>       :geospatial_lon_max = 7.68944972163126 ;
!>       :geospatial_lon_units = "degrees_east" ;
!>       :geospatial_lon_resolution = "on average     370.50 meters" ;


   ierr = nf90_enddef(ncid)
! -- end of header --

   ! Write the actual data
   ! Nodes:
   ierr = nf90_put_var(ncid, meshids%id_nodex,    xn(1:numNode))
   ierr = nf90_put_var(ncid, meshids%id_nodey,    yn(1:numNode))
!   ierr = nf90_put_var(ncid, meshids%id_nodez,    zn(1:numNode))

   ! Edges:
   if (dim == 1 .or. ug_checklocation(dataLocs, UG_LOC_EDGE)) then
      ierr = nf90_put_var(ncid, meshids%id_edgenodes, edge_nodes, count=(/ 2, numEdge /))
   end if

   if (ug_checklocation(dataLocs, UG_LOC_EDGE)) then
      ierr = nf90_put_var(ncid, meshids%id_edgex,    xe(1:numEdge))
      ierr = nf90_put_var(ncid, meshids%id_edgey,    ye(1:numEdge))

      ! end point coordinates:
      allocate(edgexbnd(2, numEdge), edgeybnd(2, numEdge))
      edgexbnd = dmiss
      edgeybnd = dmiss
      do n=1,numEdge
         edgexbnd(1:2, n) = xn(edge_nodes(1:2, n))
         edgeybnd(1:2, n) = yn(edge_nodes(1:2, n))
      end do
      ierr = nf90_put_var(ncid, meshids%id_edgexbnd, edgexbnd)
      ierr = nf90_put_var(ncid, meshids%id_edgeybnd, edgeybnd)
      deallocate(edgexbnd, edgeybnd)

   end if

   ! Faces:
   if (dim == 2 .or. ug_checklocation(dataLocs, UG_LOC_FACE)) then
      ! Write mesh faces (2D cells)
      ierr = nf90_put_var(ncid, meshids%id_facenodes, face_nodes)

      ! corner point coordinates:
      allocate(facexbnd(maxnv, numFace), faceybnd(maxnv, numFace))
      facexbnd = dmiss
      faceybnd = dmiss

      do n=1,numFace
         do k=1,maxnv
            if (face_nodes(k, n) == imiss) then
               exit ! This face has less corners than maxnv, we're done for this one.
            end if

            facexbnd(k, n) = xn(face_nodes(k, n))
            faceybnd(k, n) = yn(face_nodes(k, n))
         end do
      end do
      ierr = nf90_put_var(ncid, meshids%id_facexbnd, facexbnd)
      ierr = nf90_put_var(ncid, meshids%id_faceybnd, faceybnd)
      deallocate(facexbnd, faceybnd)
   end if

   if (ug_checklocation(dataLocs, UG_LOC_FACE)) then
      ierr = nf90_put_var(ncid, meshids%id_facex,    xf(1:numFace))
      ierr = nf90_put_var(ncid, meshids%id_facey,    yf(1:numFace))
   end if


   ! Check for any remaining native NetCDF errors
   if (ierr /= nf90_noerr) then
      goto 888
   end if

   ! Leave the dataset in the same mode as we got it.
   if (wasInDefine == 1) then
      ierr = nf90_redef(ncid)
   end if

   ierr = UG_NOERR
   return ! Return with success

888 continue


end function ug_write_mesh_arrays


!> Gets the number of mesh topologies in an open dataset.
!! use this to determine on how many different meshes, data is defined in the dataset.
!!
!! \see
function ug_get_meshcount(ncid, numMesh) result(ierr)
   integer,        intent(in)  :: ncid     !< NetCDF dataset id
   integer,        intent(out) :: numMesh  !< Number of mesh topologies in the dataset (>= 0).
   integer                     :: ierr     !< Result status (UG_NOERR==NF90_NOERR) if successful.

   integer :: numVar, i
   character(len=13) :: buffer

   ierr = nf90_inquire(ncid, nVariables = numVar)

   numMesh = 0
   do i=1,numVar
      buffer = ' '
      ierr = nf90_get_att(ncid, i, 'cf_role', buffer)

      if (ierr /= nf90_noerr) then
         cycle
      end if


      if (trim(buffer) == 'mesh_topology') then
         numMesh = numMesh + 1
      end if
   end do

end function ug_get_meshcount

end module wq_ugrid
