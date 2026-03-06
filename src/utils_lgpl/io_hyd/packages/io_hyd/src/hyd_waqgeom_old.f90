module hyd_waqgeom_old

! 

use precision
use netcdf
use wq_unstruc_netcdf
use MessageHandling !unstruc_messages
use wqhyd_version_module
use wqm_waqgeom
use wqm_sferic
use wqm_partitioninfo

implicit none

integer            :: nerr_
logical            :: err_firsttime_
character(len=255) :: err_firstline_

!> All NetCDF files should be opened through unc_open or unc_create,
!! such that all opened files are maintained and can be properly closed
!! upon exit of the program by unc_closeall.
!integer, parameter,private :: maxopenfiles = 1000
character(len=255) :: open_files_(maxopenfiles)    !< Names of open NetCDF files.
integer            :: open_datasets_(maxopenfiles) !< Dataset IDs of open NetCDF files.
integer            :: nopen_files_ = 0             !< Nr. of NetCDF files currently open.

private :: nerr_, err_firsttime_, err_firstline_, &
           prepare_error, check_error, &
           open_files_, open_datasets_, nopen_files_

integer :: numContPts, numNodes

contains

subroutine write_waqgeom(hyd, version_full)
      use m_hydmod
      type(t_hydrodynamics)               :: hyd                    !< description of the hydrodynamics

       character(len=256)            :: filename          !  Name of NetCDF file
       character(len=*), intent(in)  :: version_full

       integer                       :: igeomfile         !  NetCDF file handle
       integer                       :: ierr              !  error indication
       integer                       :: i                 !  loop counter
       integer                       :: ip1               !  segment pointer
       integer                       :: ip2               !  segment pointer
       integer, allocatable          :: lntmp (:,:)       !  temporary array for link (2,*) node administration
       real*8, allocatable           :: xutmp(:)          ! xu
       real*8, allocatable           :: yutmp(:)          ! yu
       integer                       :: nobndl            !  number of boundary links per layer

       ! copy relevant dimensions

       numcontpts = hyd%numcontpts

       ! deallocate m_waqgeom

       if ( allocated(xk) ) deallocate (xk)
       if ( allocated(yk) ) deallocate (yk)
       if ( allocated(zk) ) deallocate (zk)

       if ( allocated(kn) ) deallocate (kn)
       if ( allocated(netcellnod) ) deallocate (netcellnod)

       if ( allocated(xz) ) deallocate(xz)
       if ( allocated(yz) ) deallocate(yz)
       if ( allocated(nd) ) then
          do i=1,ndxi
             if ( allocated(nd(i)%ln )) deallocate(nd(i)%ln )
             if ( allocated(nd(i)%nod)) deallocate(nd(i)%nod)
             if ( allocated(nd(i)%x  )) deallocate(nd(i)%x  )
             if ( allocated(nd(i)%y  )) deallocate(nd(i)%y  )
          enddo
          deallocate(nd)
       endif
       if (allocated(bl)           ) deallocate(bl)
       if (allocated(ln)           ) deallocate(ln)
       if (allocated(xu)           ) deallocate(xu)
       if (allocated(yu)           ) deallocate(yu)
       if (allocated(lntmp)        ) deallocate(lntmp)
       if (allocated(xutmp)        ) deallocate(xutmp)
       if (allocated(yutmp)        ) deallocate(yutmp)

       numk         = hyd%numk
       numl         = hyd%numl
       nv           = hyd%nv
       nump         = hyd%nump

       ndxi         = hyd%nosegl
       numNodes     = hyd%numcontpts
       numContPts   = hyd%numcontpts
       nobndl       = hyd%nobndl

       ! allocate m_waqgeom
       allocate(xk(numk))
       allocate(yk(numk))
       allocate(zk(numk))
       allocate(kn(2,numl))
       allocate(netcellnod(nv,nump), stat = ierr)

       allocate(xz(ndxi))
       allocate(yz(ndxi))
       allocate(nd(ndxi))
       do i=1,ndxi
          allocate(nd(i)%ln (numContPts))
          allocate(nd(i)%nod(numContPts))
          allocate(nd(i)%x  (numContPts))
          allocate(nd(i)%y  (numContPts))
       enddo
       allocate(bl(ndxi))
       allocate(lntmp(2,hyd%num_exchanges_u_dir))
       allocate(xutmp(hyd%num_exchanges_u_dir))
       allocate(yutmp(hyd%num_exchanges_u_dir))

       ! copy the values
       do i=1,numk
          xk(i)      = hyd%xk(i)
          yk(i)      = hyd%yk(i)
          zk(i)      = hyd%zk(i)
       enddo
       do i=1,numl
          kn(1,i)    = hyd%kn(1,i)
          kn(2,i)    = hyd%kn(2,i)
       enddo
       do i=1,nump
          netcellnod(:,i) = hyd%netcellnod(:,i)
       enddo
       do i=1,ndxi
         xz(i) = hyd%xdepth(1,i)
         yz(i) = hyd%ydepth(1,i)
         nd(i)%x = hyd%flowelemcontourx(:,i)
         nd(i)%y = hyd%flowelemcontoury(:,i)
         bl(i) = hyd%depth(i)
       enddo
       lnx = 0
       do i=1,hyd%num_exchanges_u_dir
          ip1 = hyd%ipoint(1,i)
          ip2 = hyd%ipoint(2,i)
          if(ip1.ge.-nobndl .and. ip1.le.ndxi .and. &
             ip2.ge.-nobndl .and. ip2.le.ndxi) then
             lnx = lnx + 1
             xutmp(lnx) = hyd%xu(i)
             yutmp(lnx) = hyd%yu(i)
             if ( ip1 .lt. 0 ) then
                ip1 = hyd%nosegl-ip1
             endif
             if ( ip2 .lt. 0 ) then
                ip2 = hyd%nosegl-ip2
             endif
             lntmp(1,lnx) = ip1
             lntmp(2,lnx) = ip2
          endif
       enddo

       hyd%lnx = lnx
       allocate(ln(2,lnx))
       allocate(xu(lnx))
       allocate(yu(lnx))
       do i=1,lnx
             ln(1,i) = lntmp(1,i)
             ln(2,i) = lntmp(2,i)
             xu(i) = xutmp(i)
             yu(i) = yutmp(i)
       enddo
       crs = hyd%crs

       filename = hyd%file_geo%name
       call unc_write_waqgeom(filename, version_full)
       
end subroutine write_waqgeom

subroutine read_waqgeom(hyd)
      use m_hydmod
      type(t_hydrodynamics)               :: hyd                    !< description of the hydrodynamics

       character(len=256)            :: filename          !  Name of NetCDF file
       integer                       :: igeomfile         !  NetCDF file handle
       integer                       :: ierr              !  error indication
       integer                       :: i                 !  loop counter

       filename = hyd%file_geo%name

       ierr = unc_open(filename, nf90_nowrite, igeomfile)
       call check_error(ierr, 'file '''//trim(filename)//'''')
       if (nerr_ > 0) return

       call unc_read_waqgeom_filepointer(igeomfile)

       ! copy relevant dimensions
       hyd%numk       = numk
       hyd%numl       = numl
       hyd%nv         = nv
       hyd%nump       = nump
       hyd%numcontpts = numcontpts

       ! allocate the arrays
       allocate(hyd%xk(hyd%numk))
       allocate(hyd%yk(hyd%numk))
       allocate(hyd%zk(hyd%numk))
       allocate(hyd%kn(2,hyd%numl))
       allocate(hyd%netcellnod(hyd%nv,hyd%nump))

       allocate(hyd%xdepth(hyd%num_rows,hyd%num_columns))
       allocate(hyd%ydepth(hyd%num_rows,hyd%num_columns))
       allocate(hyd%depth(hyd%num_columns))

       allocate(hyd%idomain(hyd%num_columns*hyd%num_layers))
       allocate(hyd%iglobal(hyd%num_columns*hyd%num_layers))
       allocate(hyd%ilocal_link(hyd%num_exchanges_u_dir))
       allocate(hyd%iglobal_link(hyd%num_exchanges_u_dir))
       allocate(hyd%flowelemcontourx(hyd%numcontpts,hyd%num_columns))
       allocate(hyd%flowelemcontoury(hyd%numcontpts,hyd%num_columns))
       allocate(hyd%xu(hyd%num_exchanges_u_dir))
       allocate(hyd%yu(hyd%num_exchanges_u_dir))

       ! copy the values
       do i=1,numk
          hyd%xk(i)      = xk(i)
          hyd%yk(i)      = yk(i)
          hyd%zk(i)      = zk(i)
       enddo
       do i=1,numl
          hyd%kn(1,i)      = kn(1,i)
          hyd%kn(2,i)      = kn(2,i)
       enddo
       do i=1,nump
          hyd%netcellnod(:,i) = netcellnod(:,i)
       enddo
       do i=1,hyd%num_columns
          hyd%xdepth(1,i)  = xz(i)
          hyd%ydepth(1,i)  = yz(i)
          hyd%idomain(i)   = idomain(i)
          hyd%flowelemcontourx(:,i) = nd(i)%x
          hyd%flowelemcontoury(:,i) = nd(i)%y
          hyd%depth(i)     = bl(i)
       enddo
       hyd%lnx1d        = lnx1d
       hyd%lnx          = lnx
       do i=1,lnx
          hyd%xu(i)  = xu(i)
          hyd%yu(i)  = yu(i)
       enddo
       do i=1,hyd%num_columns
          hyd%iglobal(i) = iglobal(i)
       enddo
       hyd%crs = crs

end subroutine read_waqgeom

!> Read the unstructured waq geometry to an already opened netCDF dataset.
subroutine unc_read_waqgeom_filepointer(igeomfile)
    use wqm_waqgeom
    use wqm_sferic
    use netcdf
    use wqm_partitioninfo
!    use m_flow, only: kmx
    integer, intent(in) :: igeomfile

    integer, allocatable :: kn3(:), ibndlink(:)

    integer, save :: id_netnodedim, id_netlinkdim, &
               id_netelemmaxnodedim, id_netelemdim, id_netelemnode, &  !< Dimensions
               id_netnodex, id_netnodey, id_netnodez, &                !< Node variables
               id_netlink, id_netlinktype, &                           !< Link variables
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
        id_flowelemglobalnr

    integer :: i, idomain_from, idomain_to
    integer :: jaInDefine = 0
    integer :: jaghost, idmn, iglev
    integer :: nFlowLinkPts
    character(len=256)   :: jvbtest_filename
    integer              :: ln_type(1)

    ! Default element names to support the old-style waqgeom files
    ! If we have a proper UGRID waqgeom file, use the names from the attributes
    integer, parameter :: nNetNode = 1
    integer, parameter :: nNetLink = 2
    integer, parameter :: nNetElemMaxNode = 3
    integer, parameter :: nNetElem = 4
    integer, parameter :: nFlowElem = 5
    integer, parameter :: nFlowElemMaxNode = 6
    integer, parameter :: nFlowElemContourPts = 7
    integer, parameter :: nFlowLink = 8
    integer, parameter :: nFlowLinkPts_name = 9
    integer, parameter :: projected_coordinate_system = 10
    integer, parameter :: NetNode_x = 11
    integer, parameter :: NetNode_y = 12
    integer, parameter :: NetNode_z = 13
    integer, parameter :: NetLink = 14
    integer, parameter :: NetElemNode = 15
    integer, parameter :: FlowElem_xcc = 16
    integer, parameter :: FlowElem_ycc = 17
    integer, parameter :: FlowElem_zcc = 18
    integer, parameter :: FlowElemContour_x = 19
    integer, parameter :: FlowElemContour_y = 20
    integer, parameter :: FlowElemContour_z = 21
    integer, parameter :: FlowElem_bl = 22
    integer, parameter :: FlowLink = 23
    integer, parameter :: FlowLinkType = 24
    integer, parameter :: FlowLink_xu = 25
    integer, parameter :: FlowLink_yu = 26
    integer, parameter :: FlowElemDomain = 27
    integer, parameter :: FlowElemGlobalNr = 28

    integer, parameter :: number_names = 28
    character(len=40), dimension(number_names), save :: default_element_name = &
        [character(len=40) :: &
         'nNetNode',          'nNetLink',          'nNetElemMaxNode',     'nNetElem',      &   !  1 -  4: Names of the dimensions
         'nFlowElem',         'nFlowElemMaxNode',  'nFlowElemContourPts',                  &   !  5 -  7: Names of the dimensions
         'nFlowLink',         'nFlowLinkPts',                                              &   !  8 -  9: Names of the dimensions
         'projected_coordinate_system',                                                    &   ! 10 - 10: Coordinate system (fixed name)
         'NetNode_x',         'NetNode_y',         'NetNode_z',                            &   ! 11 - 13: Coordinates of the nodes
         'NetLink',           'NetElemNode',                                               &   ! 14 - 15: Links and elements made up of nodes
         'FlowElem_xcc',      'FlowElem_ycc',      'FlowElem_zcc',                         &   ! 16 - 18: Centre coordinates of flow elements
         'FlowElemContour_x', 'FlowElemContour_y', 'FlowElemContour_z',                    &   ! 19 - 21: Contour coordinates of flow elements
         'FlowElem_bl',                                                                    &   ! 22 - 22: Flow elements, bottom levels
         'FlowLink',          'FlowLinkType'     , 'FlowLink_xu',       'FlowLink_yu',     &   ! 23 - 26: Flow link information
         'FlowElemDomain',    'FlowElemGlobalNr']                                              ! 27 - 28: Domain administration
    character(len=40), dimension(number_names), save :: element_name

    ! deallocate m_waqgeom

    if ( allocated(xk) ) deallocate (xk)
    if ( allocated(yk) ) deallocate (yk)
    if ( allocated(zk) ) deallocate (zk)

    if ( allocated(kn) ) deallocate (kn)
    if ( allocated(netcellnod) ) deallocate (netcellnod)

    if ( allocated(xz) ) deallocate(xz)
    if ( allocated(yz) ) deallocate(yz)
    if ( allocated(nd) ) then
       do i=1,ndxi
          if ( allocated(nd(i)%ln )) deallocate(nd(i)%ln )
          if ( allocated(nd(i)%nod)) deallocate(nd(i)%nod)
          if ( allocated(nd(i)%x  )) deallocate(nd(i)%x  )
          if ( allocated(nd(i)%y  )) deallocate(nd(i)%y  )
       enddo
       deallocate(nd)
    endif
    if (allocated(bl)           ) deallocate(bl)
    if (allocated(ln)           ) deallocate(ln)
    if (allocated(xu)           ) deallocate(xu)
    if (allocated(yu)           ) deallocate(yu)
    if (allocated(idomain)      ) deallocate(idomain)
    if (allocated(iglobal)      ) deallocate(iglobal)

    ! determine the element names
    call determine_elements( igeomfile, element_name, default_element_name )

    ! read dimensions

    ! Get nr of nodes and edges
    ierr = nf90_inq_dimid(igeomfile, element_name(nNetNode), id_netnodedim)
    call check_error(ierr, 'nNetNode')
    ierr = nf90_inq_dimid(igeomfile, element_name(nNetLink), id_netlinkdim)
    call check_error(ierr, 'nNetLink')
    ierr = nf90_inq_dimid(igeomfile, element_name(nNetElemMaxNode), id_netelemmaxnodedim)
    call check_error(ierr, 'nNetElemMaxNode')
    ierr = nf90_inq_dimid(igeomfile, element_name(nNetElem), id_netelemdim)
    call check_error(ierr, 'nNetElem'          )
    if (nerr_ > 0) return

    ierr = nf90_inquire_dimension(igeomfile, id_netnodedim, len=numk)
    call check_error(ierr, 'node count')
    ierr = nf90_inquire_dimension(igeomfile, id_netlinkdim, len=numl)
    call check_error(ierr, 'link count')
    ierr = nf90_inquire_dimension(igeomfile, id_netelemmaxnodedim, len = nv)
    call check_error(ierr, 'elem max node count')
    ierr = nf90_inquire_dimension(igeomfile, id_netelemdim, len=nump)
    call check_error(ierr, 'Elem count')
    if (nerr_ > 0) return

    ierr = nf90_inq_varid(igeomfile, element_name(projected_coordinate_system), id_crsvar)
    if (ierr /= nf90_noerr) then
       ierr = nf90_inq_varid(igeomfile, 'wgs84', id_crsvar)
    end if
    if (ierr == nf90_noerr) then
        ierr = nf90_inquire_variable(igeomfile, id_crsvar, name = crs%varname)
        ierr = nf90_get_att(igeomfile, id_crsvar, 'epsg', crs%epsg_code)
        if (ierr /= nf90_noerr) then 
           ierr = nf90_get_att(igeomfile, id_crsvar, 'EPSG', crs%epsg_code)
        end if
        ierr = ug_get_var_attset(igeomfile, id_crsvar, crs%attset)
    end if

    ierr = nf90_inq_varid(igeomfile, element_name(NetNode_x), id_netnodex)
    call check_error(ierr, 'x coordinates')
    ierr = nf90_inq_varid(igeomfile, element_name(NetNode_y), id_netnodey)
    call check_error(ierr, 'y coordinates')
    ierr = nf90_inq_varid(igeomfile, element_name(NetNode_z), id_netnodez)
    call check_error(ierr, 'z coordinates')
    ierr = nf90_inq_varid(igeomfile, element_name(NetLink)    , id_netlink    )
    call check_error(ierr, 'netlinks')
    ierr = nf90_inq_varid(igeomfile, element_name(NetElemNode), id_netelemnode)
    call check_error(ierr, 'net elem nodes')
    if (nerr_ > 0) return

    allocate (xk(numk), yk(numk), zk(numk))
    ierr = nf90_get_var(igeomfile, id_netnodex, xk(1:numk))
    call check_error(ierr, 'x values')
    ierr = nf90_get_var(igeomfile, id_netnodey, yk(1:numk))
    call check_error(ierr, 'y values')
    ierr = nf90_get_var(igeomfile, id_netnodez, zk(1:numk))
    call check_error(ierr, 'z values')

    allocate (kn(2,numl))
    ierr = nf90_get_var(igeomfile, id_netlink, kn) ! (1:2,1:numl), count = (/ 2, numl /)) ! , map=(/ 1, 3 /)
    call check_error(ierr, 'node links')

    allocate (netcellnod(nv,nump))
    ierr = nf90_get_var(igeomfile, id_netelemnode, netcellnod)
    call check_error(ierr, 'cell elem.')

    ierr = nf90_inq_dimid(igeomfile, element_name(nFlowElem)          , id_flowelemdim)
    call check_error(ierr, 'nFlowElem'          )
    ierr = nf90_inq_dimid(igeomfile, element_name(nFlowElemMaxNode)   , id_flowelemmaxnodedim)
    call check_error(ierr, 'nFlowElemMaxNode'   )
    ierr = nf90_inq_dimid(igeomfile, element_name(nFlowElemContourPts), id_flowelemcontourptsdim)
    call check_error(ierr, 'nFlowElemContourPts')
    ierr = nf90_inq_dimid(igeomfile, element_name(nFlowLink)          , id_flowlinkdim)
    call check_error(ierr, 'nFlowLink'          )
    ierr = nf90_inq_dimid(igeomfile, element_name(nFlowLinkPts_name)  , id_flowlinkptsdim)
    call check_error(ierr, 'nFlowLinkPts'       )

    ierr = nf90_inquire_dimension(igeomfile, id_flowelemdim           , len=ndxi)
    call check_error(ierr, 'nFlowElem'          )
    ierr = nf90_inquire_dimension(igeomfile, id_flowelemmaxnodedim    , len=numNodes)
    call check_error(ierr, 'nFlowElemMaxNode'   )
    ierr = nf90_inquire_dimension(igeomfile, id_flowelemcontourptsdim , len=numContPts)
    call check_error(ierr, 'nFlowElemContourPts')
    ierr = nf90_inquire_dimension(igeomfile, id_flowlinkdim           , len=lnx)
    call check_error(ierr, 'nFlowLink'          )
    ierr = nf90_inquire_dimension(igeomfile, id_flowlinkptsdim        , len=nFlowLinkPts)
    call check_error(ierr, 'nFlowLinkPts'       )
    if (nerr_ > 0) return

    ! allocate m_waqgeom

    allocate(xz(ndxi))
    allocate(yz(ndxi))
    allocate(nd(ndxi))
    do i=1,ndxi
       allocate(nd(i)%ln (numContPts))
       allocate(nd(i)%nod(numContPts))
       allocate(nd(i)%x  (numContPts))
       allocate(nd(i)%y  (numContPts))
    enddo
    allocate(bl(ndxi))
    allocate(ln(2,lnx))
    allocate(xu(lnx))
    allocate(yu(lnx))
    allocate(idomain(ndxi))
    allocate(iglobal(ndxi))

    ! Flow cells
    ierr = nf90_inq_varid(igeomfile, element_name(FlowElem_xcc), id_flowelemxcc)
    call check_error(ierr, 'FlowElem_xcc')
    ierr = nf90_inq_varid(igeomfile, element_name(FlowElem_ycc), id_flowelemycc)
    call check_error(ierr, 'FlowElem_ycc')
    ierr = nf90_get_var(igeomfile, id_flowelemxcc, xz)
    call check_error(ierr, 'FlowElem_xcc')
    ierr = nf90_get_var(igeomfile, id_flowelemycc, yz)
    call check_error(ierr, 'FlowElem_ycc')

    ierr = nf90_inq_varid(igeomfile, element_name(FlowElemContour_x), id_flowelemcontourx)
    ierr = nf90_inq_varid(igeomfile, element_name(FlowElemContour_y), id_flowelemcontoury)
    ! Flow cell contours
    do i=1,ndxi
        ierr = nf90_get_var(igeomfile, id_flowelemcontourx, nd(i)%x, (/ 1, i /), (/ numContPts, 1 /) )
        ierr = nf90_get_var(igeomfile, id_flowelemcontoury, nd(i)%y, (/ 1, i /), (/ numContPts, 1 /) )
    enddo

    ! Flow elems bottom levels
    ierr = nf90_inq_varid(igeomfile, element_name(FlowElem_bl), id_flowelembl)
    ierr = nf90_get_var(igeomfile, id_flowelembl, bl(1:ndxi))

    if (lnx > 0) then
        ierr = nf90_inq_varid(igeomfile, element_name(FlowLink),     id_flowlink)
        ierr = nf90_inq_varid(igeomfile, element_name(FlowLinkType), id_flowlinktype)
        ierr = nf90_inq_varid(igeomfile, element_name(FlowLink_xu),  id_flowlinkxu)
        ierr = nf90_inq_varid(igeomfile, element_name(FlowLink_yu),  id_flowlinkyu)
        ierr = nf90_get_var(igeomfile, id_flowlink    ,ln(:,1:lnx))
        ierr = nf90_get_var(igeomfile, id_flowlinkxu, xu(1:lnx))
        ierr = nf90_get_var(igeomfile, id_flowlinkyu, yu(1:lnx))
        lnx1D = 0
        do i=1,lnx
            ierr = nf90_get_var(igeomfile, id_flowlinktype,ln_type, start=(/ 1, i /), count = (/ 1, 1 /) )
            if ( ln_type(1) .eq. 1 ) lnx1D = i
        end do
    end if


!   domain numbers
    ierr = nf90_inq_varid(igeomfile, element_name(FlowElemDomain),   id_flowelemdomain)
    ierr = nf90_inq_varid(igeomfile, element_name(FlowElemGlobalNr), id_flowelemglobalnr)
    ierr = nf90_get_var(igeomfile, id_flowelemdomain,   idomain )
    ierr = nf90_get_var(igeomfile, id_flowelemglobalnr, iglobal )

contains
!> Determine which element names to use for reading the waqgeom file
!! Support two different conventions
subroutine determine_elements( igeomfile, element_name, default_element_name )
    integer, intent(in)                         :: igeomfile                !< Handle to the waqgeom file
    character(len=*), intent(out), dimension(:) :: element_name             !< Element names to be used
    character(len=*), intent(in), dimension(:)  :: default_element_name     !< Default names (old style)

    integer                        :: nvars, varid, i, k, xtype, length, attnum, natts
    integer                        :: ierror
    character(len=nf90_max_name)   :: varname, attribute
    character(len=4*nf90_max_name) :: attribute_value

    ! Copy the default names first

    element_name = default_element_name

    ! Look for the "mesh" element - TODO: delwaq_role

    varid     = -1
    attribute = 'cf_role'
    ierror = nf90_inquire( igeomfile, nVariables = nvars )
    if ( ierror /= nf90_noerr ) then
        return
    endif

    do i = 1,nvars
        ierror = nf90_inquire_variable( igeomfile, i, name=varname )

        ierror = nf90_inquire_attribute( igeomfile, i, attribute, xtype, length, attnum )
        if ( ierror /= nf90_noerr .and. ierror /= nf90_enotatt ) then
            call check_error( ierror, 'Retrieving attributes - NetCDF variable ' //trim(varname) )
            return
        elseif ( ierror == nf90_noerr ) then
            attribute_value = ' ' ! This appears to be necessary
            ierror = nf90_get_att( igeomfile, i, attribute, attribute_value )
            k = index( attribute_value, char(0) )
            if ( k > 0 ) attribute_value(k:) = ' '
            if ( attribute_value == 'mesh_topology' ) then
                varid = i
                ierror = nf90_inquire_variable( igeomfile, varid, name=varname, nAtts = natts )
                exit
            endif
        endif
    enddo

    ! If there is no such variable, then we will use the default names instead

    if ( varid == -1 ) then
        return
    endif

    ! The names we need are stored in the attributes, so get their values
    ! Be careful: only character-type attributes

    do i = 1,natts
        ierror = nf90_inq_attname( igeomfile, varid, i, attribute )
        ierror = nf90_inquire_attribute( igeomfile, varid, attribute, xtype, length, attnum )
        if ( xtype /= nf90_char ) then
            cycle
        endif
        if ( ierror == nf90_noerr ) then
            attribute_value = ' ' ! This appears to be necessary
            ierror = nf90_get_att( igeomfile, varid, attribute, attribute_value )
        endif
        if ( ierror /= nf90_noerr ) then
            call check_error( ierror, 'Retrieving attributes - NetCDF variable ' // trim(varname) )
            return
        endif

        k = index( attribute_value, char(0) )
        if ( k > 0 ) attribute_value(k:) = ' '

        select case ( attribute )
            case( 'face_dimension' )
                read( attribute_value, * ) element_name(nNetElem)             ! Default: nNetElem

            case( 'node_dimension' )
                read( attribute_value, * ) element_name(nNetNode)             ! Default: nNetNode

            case( 'edge_dimension' )
                read( attribute_value, * ) element_name(nNetLink)             ! Default: nNetLink

            case( 'face_coordinates' )
                ! Not used ...
                !read( attribute_value, * ) element_name(..), element_name(..) ! Default: none, not used

            case( 'max_face_nodes_dimension' )
                read( attribute_value, * ) element_name(nNetElemMaxNode)      ! Default: nNetElemMaxNode

            case( 'edge_coordinates' )
                read( attribute_value, * ) element_name(FlowLink_xu), element_name(FlowLink_yu) ! Default: FlowLink_xu, FlowLink_yu

            case( 'node_coordinates' )
                read( attribute_value, * ) element_name(NetNode_x), element_name(NetNode_y) ! Default: NetNode_x, NetNode_y
                element_name(NetNode_z) = element_name(NetNode_x)                ! Construct the likely name
                element_name(NetNode_z)(len_trim(element_name(NetNode_z)):) = 'z'

            case( 'face_node_connectivity' )
                read( attribute_value, * ) element_name(NetElemNode)          ! Default: NetElemNode

            case( 'edge_node_connectivity' )
                read( attribute_value, * ) element_name(NetLink)              ! Default: NetLink

            case( 'edge_face_connectivity' )
                read( attribute_value, * ) element_name(FlowLink)             ! Default: FlowLink

            case( 'long_name', 'cf_role', 'topology_dimension' )
                ! Ignored - known, but not used here

            case default
                ! Ignored - unknown, could be extra stuff
        end select
    enddo

end subroutine determine_elements

end subroutine unc_read_waqgeom_filepointer

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
    integer,          intent(in)           :: ierr
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

!> Write a new waqgeom file, in UGRID-format this time
subroutine write_waqgeom_ugrid( filename, hyd, version_full)
    use m_hydmod
    use wq_ugrid
    character(len=*)  :: filename
    type(t_hydrodynamics)       :: hyd
    character(len=*)  :: version_full

    call wrwaqgeom( filename, version_full, sferic = .false., epsg = hyd%crs%epsg_code, nr_nodes = hyd%numk, &
             xk = hyd%xk, yk = hyd%yk, zk = hyd%zk, max_vertex = hyd%nv, nr_elems = hyd%nump, &
             netelem = hyd%netcellnod, nr_edges = hyd%numl, netlink = kn, &
             nr_flowlinks = hyd%lnx, flowlink = ln, xu = hyd%xu, yu = hyd%yu )

end subroutine write_waqgeom_ugrid

subroutine wrwaqgeom (filename, version_full, sferic, epsg, nr_nodes, xk, yk, zk, max_vertex, nr_elems, netelem, nr_edges, netlink, &
                      nr_flowlinks, flowlink, xu, yu)
      !
      !===============================================================================
      ! Write the waqgeom netcdf file
      !===============================================================================
      !

      use m_alloc
      use netcdf
      use wq_ugrid
      use io_netcdf
      use io_ugrid
      use m_write_waqgeom

      implicit none

      character(len=*) :: filename                          ! sobek_waqgeom.nc
      character(len=*) :: version_full                      ! SOBEK Version
      logical :: sferic                                     ! No Sferic here, .false.
      integer :: epsg                                       ! Projection type, 0
      integer :: nr_nodes                                   ! Total number of polypoints
      real(hp) :: xk(nr_nodes), yk(nr_nodes), zk(nr_nodes)  ! Poly Points
      integer :: max_vertex                                 ! Maximaal aantal polypoints in polygon (here all 6)
      integer :: nr_elems                                   ! Number of waq segments (isegtotal)
      integer :: netelem(max_vertex, nr_elems)              ! idxPolygons(6, isegtotal)
      integer :: nr_edges                                   ! Number of all line pieces to be drawn.
      integer :: netlink(2, nr_edges)                       ! From ploypoint to polypoint
      integer :: nr_flowlinks
      integer :: flowlink(2, nr_flowlinks)
      real(hp) :: xu(nr_flowlinks), yu(nr_flowlinks)

!
!           Local variables
!
      integer :: ierr
      integer :: igeomfile
      integer :: id_netelemdim  ! netcdf id for mesh element dimension
      integer :: id_netnodedim ! netcdf id for node dimension
      integer :: id_netelemmaxnodedim ! netcdf id maximum number of vertices for an element (== 4 in Delft3D-FLOW)
      integer :: id_netnodex ! netcdf id for x-coordinate of node
      integer :: id_netnodey ! netcdf id for y-coordinate of node
      integer :: id_netnodez ! netcdf id for z-coordinate of node
      integer :: id_netlink, id_netelem, id_netlinkdim, id_netlinkptsdim
      integer :: id_flowlink, id_flowlinkdim, id_flowlinkptsdim, id_flowlinktype, id_flowlinkxu, id_flowlinkyu
      integer :: id_cfdim, id_cfmesh
      integer :: id_facexcrd, id_faceycrd
      real(hp), dimension(:), allocatable :: xcrd, ycrd, zcrd

      type(t_ug_meta)     :: meta                      ! netcdf meta data
      integer             :: conv_type                 ! netcdf convention type
      real(8)             :: conv_version              ! netcdf convension version
      type(t_crs)         :: crs                       ! projection code
      type(t_ug_meshgeom) :: waqgeom                   ! geometry data
      integer , dimension(:), pointer :: edge_type     ! edge type variable to be written to the NetCDF file.

      character(len=20)  :: rundat
      character(len=20)  :: datetime
      integer            :: iyea, imon, iday, ihou, imin, isec
      integer            :: i, j, k, nk
      logical            :: success

      !
      !! executable statements -------------------------------------------------------

      meta%institution = "Deltares"
      meta%source = version_full
      meta%references = "http://www.deltares.nl"
      conv_type = IONC_CONV_UGRID
      conv_version = 1.0

      ierr = 0
      !

      ierr = t_ug_meshgeom_destructor(waqgeom)
      waqgeom%meshname = 'mesh2d'
      waqgeom%dim = 2
      waqgeom%numnode = nr_nodes
      waqgeom%numedge = nr_edges
      waqgeom%numface = nr_elems
      waqgeom%maxnumfacenodes = max_vertex
      waqgeom%start_index = 1
      waqgeom%epsg = epsg
      crs%epsg_code = epsg

      call reallocP(waqgeom%edge_nodes, (/2, nr_edges/))
      do i = 1, nr_edges
         waqgeom%edge_nodes(1,i) = netlink(1,i)
         waqgeom%edge_nodes(2,i) = netlink(2,i)
      end do

      call reallocP(waqgeom%face_nodes, (/max_vertex, nr_elems/))
      do i = 1, max_vertex
         do j = 1, nr_elems
            waqgeom%face_nodes(i, j) = netelem(i, j)
         end do
      end do

      call reallocP(waqgeom%edge_faces, (/2, nr_edges/))
      do i = 1, nr_edges
         waqgeom%edge_faces(1,i) = -999 ! TODO
         waqgeom%edge_faces(2,i) = -999 ! TODO
      end do

      call reallocP(waqgeom%face_edges, (/max_vertex, nr_elems/))
      do i = 1, max_vertex
         do j = 1, nr_elems
            waqgeom%face_edges(i, j) = -999 ! TODO
         end do
      end do

      call reallocP(waqgeom%face_links, (/max_vertex, nr_elems/))
      do i = 1, max_vertex
         do j = 1, nr_elems
            waqgeom%face_links(i, j) = -999 ! TODO
         end do
      end do

      call reallocP(waqgeom%nodex, nr_nodes)
      call reallocP(waqgeom%nodey, nr_nodes)
      call reallocP(waqgeom%nodez, nr_nodes)
      do i = 1, nr_nodes
         waqgeom%nodex(i) = xk(i)
         waqgeom%nodey(i) = yk(i)
         waqgeom%nodez(i) = zk(i)
      end do

      call reallocP(waqgeom%edgex, nr_flowlinks)
      call reallocP(waqgeom%edgey, nr_flowlinks)
      call reallocP(waqgeom%edgez, nr_flowlinks)
      do i = 1, nr_flowlinks
         waqgeom%edgex(i) = xu(i)
         waqgeom%edgey(i) = yu(i)
         waqgeom%edgez(i) = -999.0_hp ! TODO
      end do

      allocate( xcrd(nr_elems), ycrd(nr_elems), zcrd(nr_elems))
      do i = 1,nr_elems
          xcrd(i) = 0.0_hp
          ycrd(i) = 0.0_hp
          zcrd(i) = 0.0_hp
          nk = 0
          do j = 1,max_vertex
              k  = netelem(j,i)
              if ( k > 0 ) then
                  nk      = nk + 1
                  xcrd(i) = xcrd(i) + xk(k)
                  ycrd(i) = ycrd(i) + yk(k)
                  zcrd(i) = zcrd(i) + zk(k)
              endif
          enddo
          if ( nk > 0 ) then
              xcrd(i) = xcrd(i) / nk
              ycrd(i) = ycrd(i) / nk
              zcrd(i) = zcrd(i) / nk
          else
              xcrd(i) = -999.0_hp
              ycrd(i) = -999.0_hp
              zcrd(i) = -999.0_hp
          endif
      enddo
      call reallocP(waqgeom%facex, nr_elems)
      call reallocP(waqgeom%facey, nr_elems)
      call reallocP(waqgeom%facez, nr_elems)
      do i = 1, nr_elems
         waqgeom%facex(i) = xcrd(i)
         waqgeom%facey(i) = ycrd(i)
         waqgeom%facez(i) = zcrd(i)
      end do
      deallocate( xcrd, ycrd )
      
      call reallocP(edge_type, nr_edges, keepExisting=.false., fill=0)
!      do i = 1, nr_edges
!         edge_type(i) = 0 ! TODO
!      end do
      
      success = write_waqgeom_file(filename, meta, crs, waqgeom, edge_type, conv_type, conv_version)
      
end subroutine wrwaqgeom

end module hyd_waqgeom_old

