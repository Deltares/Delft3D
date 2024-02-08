!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

! 
! 

module dfm_merge
use netcdf
use netcdf_utils
use dfm_params
implicit none

! TODO: re-use the definitions below from original source: unstruc_netcdf
! The following location codes generalize for 1D/2D/3D models.
integer, parameter :: UNC_LOC_CN = 1  !< Data location: corner point.
integer, parameter :: UNC_LOC_S  = 2  !< Data location: pressure point.
integer, parameter :: UNC_LOC_SN = 102  !< TEMP: Data location: netelem (not always identical to flowelem). (UNST-1256)
integer, parameter :: UNC_LOC_U  = 3  !< Data location: horizontal velocity point.
integer, parameter :: UNC_LOC_L  = 13  !< Data location: net link. TODO: AvD: phase out
integer, parameter :: UNC_LOC_W  = 6  !< Data location: vertical velocity point.
integer, parameter :: UNC_LOC_SBND=7  !< Data location: boundary waterlevel points

contains

!> Merges multiple D-Flow FM map files into a single file.
!! Typically this routine should be used on output of a parallel run,
!! i.e., on files <modelname>_000x_map.nc
function dfm_merge_mapfiles(infiles, nfiles, outfile, force) result(ierr)
   use m_alloc
   use string_module
   use io_netcdf
   use m_missing,only: dmiss, imiss
   use io_ugrid, only: mdim_face, mdim_node, mdim_edge, mdim_maxfacenodes, mdim_layer, mdim_interface, mdim_two, &
                       t_ug_network, ntdim_1dnodes, ntdim_1dgeopoints, ntdim_1dedges, ntdim_two, &
                       ntid_start, ntid_end, &
                       ug_clone_network_definition, ug_clone_network_data, &
                       ug_is_link_topology, &
                       t_ug_contacts, ug_clone_contact_definition, cdim_ncontacts
   implicit none

   character(len=MAXNAMELEN), intent(inout) :: infiles(:) !< Input files names, will be sorted if not sorted already.
   integer,                   intent(in)    :: nfiles     !< Number of input files.
   character(len=MAXNAMELEN), intent(inout) :: outfile    !< Output file name. When empty, the name is derived from the input file names.
   logical,                   intent(in)    :: force      !< Whether to disallow interactive user prompting (for file overwrite)
   integer                                  :: ierr       !< Result status (0 = success)

   integer, parameter :: int8 = 1     ! also local storage compact in 1 byte
   integer, parameter :: mapclass_time_buffer_size =   1
   integer, parameter :: netnodemaxface = 12
   integer, parameter :: ikx0 = 1

   integer, allocatable :: id_facedim(:,:), id_edgedim(:,:), id_laydim(:,:), id_wdim(:,:), id_nodedim(:,:), id_sedtotdim(:,:), id_sedsusdim(:,:), id_nlyrdim(:,:), &
                                   id_netedgedim(:,:), id_netfacedim(:,:), id_netfacemaxnodesdim(:,:), id_bnddim(:,:) !< dim and var ids, maintained for all input files + 1 output file.
   integer, dimension(nfiles+1) :: ncids, id_time, id_timestep, id_mappingVar
   integer, allocatable :: id_timedim(:,:)
   double precision :: convversion
   integer :: jaugrid, iconvtype, formatCode, new_ndx
   integer, dimension(nfiles) :: jaugridi, ioncids
   logical :: isNetCDF4
   integer, allocatable :: dimids(:,:,:) !<(nDims, maxTopodim,nfiles+1) Used for storing any remaining vectormax dimension IDs
   integer, allocatable, target :: ndx(:,:), lnx(:,:), ndxg(:,:), lnxg(:,:), kmx(:,:), numk(:,:), numl(:,:), nump(:,:), numkg(:,:), numlg(:,:), netfacemaxnodes(:,:), ndxbnd(:,:), nt(:) !< counters, maintained for all input files + 1 output file.
   integer, dimension(:), pointer :: item_counts !< Generalized count pointer, will point to ndx, lnx, numl, or numk during var data reading + writing.
   integer:: noutfile !< array index/position of output file ids, by default the last, i.e., nfiles + 1.
   integer, allocatable, target  :: face_domain(:,:), facebnd_domain(:,:), edge_domain(:,:), node_domain(:,:), netedge_domain(:,:) !< Global face/edge/node numbers and their domain number.
   integer,              pointer :: item_domain(:)
   integer, allocatable :: ln(:,:) !< Flow links
   integer, allocatable :: edgenodes(:,:,:) !< Net links
   integer, allocatable :: netedgefaces(:,:)  !< ElemLinks
   integer, allocatable :: netfaceedges(:,:)  !< NetElemLinks
   integer, allocatable :: netfacenodes(:,:) !< Net cell - to - net node mapping
   integer, allocatable :: netfacenodesl(:,:) !< Net cell - to - net node mapping for local usage (per cell)
   ! A summary of global numbering of nodes/edges/faces in map merge can be found in the description of UNST-3929.
   integer, allocatable :: face_c2g(:,:), node_c2g(:,:), netedge_c2g(:,:), edge_c2g(:,:) !< Concatenated index - to - global index mapping.
   integer, allocatable :: node_g2c(:), edge_g2c(:), face_g2c(:), face_c2cc(:), netedge_g2c(:) !< Global index - to - concatenated index mapping.
   integer, allocatable :: node_faces(:,:) ! faces that surrounds the node
   integer, allocatable :: nfaceedges(:)   ! total number of edges that surround a face
   double precision, allocatable :: node_x(:), node_y(:), edge_x(:), edge_y(:) !< coordinates
   double precision              :: xx, yy
   integer :: imm, ikk, ic, iii, k1c, g1, g2, nfaces, iedge, iface, tmpvarDim, jafound
   integer :: ifacefile, ifacein, ifaceout, ifacec
   integer :: inodefile, netedgecount2
   integer :: id_nodex, id_nodey, id_edgex, id_edgey
   integer :: fill_value_int
   double precision :: fill_value_double
   integer :: ja1DCNVar = 0
!netface_g2c(:)
   integer :: id_flownodedomain , id_flownodeglobnr, id_edgefaces, id_netfacenodes, id_edgenodes, id_netedgefaces, id_netfaceedges
   integer :: ierri
   integer :: maxlen, nlen, plen, mlen, ii, id, iv, it, itm, ip, ik, is, ie, ntsel, ndims, nvardims, vartype
   integer :: netfacemaxnodesg, numkt, lnxt, ndxt, numlt
   integer, allocatable :: ndxc(:), lnxc(:), numkc(:), numlc(:),ndx_bndc(:) ! number of ndx, lnx,... of all input files on a certain mesh
   integer :: ndxMax, lnxMax, numkMax, numlMax, ndx_bndMax
   integer, allocatable :: nfaceglob(:), nedgeglob(:), nnodeglob(:), nbndglob(:), nnetedgeglob(:)
   integer, allocatable :: nfacecount(:), nedgecount(:), nnodecount(:), nnetedgecount(:), nbndcount(:)
   integer :: nfaceglob0,  ifaceglob
   integer :: nedgeglob0,  iedgeglob
   integer :: nnodeglob0, inodeglob
   integer :: netedgecount, inetedgeglob
!   integer :: nnetfaceglob, nnetfaceglob0, nnetfacecount, numpg
   integer :: nnetedgeglob0
   integer :: nitemglob, nitemglob0, nitemcount
   integer :: maxitems ! the largest value of all topological position counts, including ndx, lnx...
   integer :: nkmxglob
   integer :: id_network ! ID of 'network1d' in one input file
   integer :: varid
   type(t_ug_network) :: netids_input, netids_output
   type(t_ug_contacts) :: cids_input, cids_output
   integer :: idom, n1, n2, n3, k1, k2
   integer :: tmpdimids(NF90_MAX_VAR_DIMS)

   integer, allocatable, target  :: itmpvar1D(:)   !< array buffer for a single global variable slice, size: (maxitems)
   integer, allocatable, target  :: itmpvar1D_tmp(:)
   integer, allocatable, target  :: itmpvar2D(:,:) !< array buffer for a single global variable slice, size: e.g. (kmx, maxitems)
   integer, allocatable, target  :: itmpvar2D_tmp(:,:)
   integer, allocatable, target  :: itmpvar2D_tmpmax(:,:)
   integer,              pointer :: itmpvarptr(:,:,:)
   ! for class map: store in 1 byte:
   integer(kind=int8),allocatable,target  :: btmpvar1D(:,:) !< array buffer for a single global variable slice, size is the largest of all ndx position counts.
   integer(kind=int8),allocatable,target  :: btmpvar1D_tmp(:,:)
   integer(kind=int8),            pointer :: btmpvarptr(:,:,:,:)
   ! for others: double precision:
   double precision, allocatable, target  :: tmpvar1D(:) !< array buffer for a single global variable slice, size: (maxitems)
   double precision, allocatable, target  :: tmpvar1D_tmp(:)
   double precision, allocatable, target  :: tmpvar2D(:,:) !< array buffer for a single global variable slice, size: e.g. (kmx, maxitems)
   double precision, allocatable, target  :: tmpvar2D_tmp(:,:)
   double precision, allocatable, target  :: tmpvar2D_tmpmax(:,:)
   double precision, allocatable, target  :: tmpvar3D(:,:,:) !< array buffer for a single global variable slice, size: e.g. (kxdim, kmx, maxitems)
   double precision,              pointer :: tmpvarptr(:,:,:)
   character, allocatable :: ctmpvar2D(:,:) !< Character arrays
   character, allocatable :: ctmpvar2D_tmp(:,:)
   character(len=4) :: fmtstr
   character(len=4096) :: tmpstr1, tmpstr2
   integer,                      allocatable :: varids(:,:,:)        !< Variable IDs for the selected variables in the input files. Support having different variables in differnt input files.
   integer,                      allocatable :: varids_out(:,:)      !< Variable IDs for the selected variables in the output file. Will typically not be equal to the var IDs in the input files, as we may not be copying *all* vars from input files.
   character(len=NF90_MAX_NAME), allocatable :: var_names(:,:)       !< Names of the selected variables.
   integer,                      allocatable :: var_types(:,:)       !< Data types of the selected variables.
   integer,                      allocatable :: var_dimids(:,:,:)    !< Dimension ids for selected variables, should be filled later, starting at the end.
   integer,                      allocatable :: var_timdimpos(:,:)   !< Position in var_dimids(1:4,iv) of time dimension (-1 if not timedep)
   integer,                      allocatable :: var_spacedimpos(:,:) !< Position in var_dimids(1:4,iv) of space dimension (-1 if not timedep)
   integer,                      allocatable :: var_laydimpos(:,:)   !< Position in var_dimids(1:4,iv) of layer dimension (-1 if not timedep)
   integer,                      allocatable :: var_kxdimpos(:,:,:)    !< Position in var_dimids(1:4,iv) of vectormax dimension (-1 if not timedep)
   integer,                      allocatable :: var_seddimpos(:,:)   !< Position in var_dimids(1:4,iv) of sediment dimension (-1 if not timedep)
   integer,                      allocatable :: var_ndims(:,:)       !< Actual number of dimensions.
   integer,                      allocatable :: var_loctype(:,:)     !< Spatial location type for each var (face/node/etc.)
   integer,                      allocatable :: var_wdimpos(:,:)     !< Position in var_dimids(1:4,iv) of layer interface dimension (-1 if not timedep)
   integer,                      allocatable :: file_ndims(:)      !< Nr. dimensions in every input file
   integer,                      allocatable :: dimids_uses(:,:)     !< Nr. vectormax-like dimensions that are used
   character(len=NF90_MAX_NAME), allocatable :: mesh_names(:,:)    !< Mesh names in every input file.
   integer,                      allocatable :: nMesh(:)           !< Nr. meshes in every input file
   integer,                      allocatable :: ncontacts(:)       !< Nr. of mesh contacts in every input file.
   integer,                      allocatable :: numkx(:,:)         !< Nr. of kx dimensions in all variables

   integer :: ivarcandidate, ifirstdim, ilastdim, ikx
   integer, parameter :: MAX_VAR_DIMS = 4 !< Max nr of dimensions for a single var. Support: (vectormax, layers, space, time).
   integer, dimension(MAX_VAR_DIMS) :: start_idx   !< Start index array for calling nf90_get_var(..., start=...)
   integer, dimension(MAX_VAR_DIMS) :: count_read  !< Data size array for calling nf90_get_var(..., count=...)
   integer, dimension(MAX_VAR_DIMS) :: count_write !< Data size array for calling nf90_put_var(..., count=...)
   integer :: nofill, ifill_value !< For inquiring fill values

   integer :: id_npartdim !< Dimension ID for the partitions counter
   integer :: id_part_face_start, id_part_edge_start, id_part_node_start, id_part_facebnd_start !< Var IDs for storing start index for each partition in the merged global arrays.
   integer :: id_part_face_count, id_part_edge_count, id_part_node_count, id_part_facebnd_count !< Var IDs for storing count of unique items for each partition in the merged global arrays
   integer :: Lrst_m
   integer :: isBndLink = 0, id_infile
   integer :: jamerge_cntv = 1 ! merge topology connectivity variables
   integer :: jaread_sep = 0   ! read the variable seperately

   character(len=NF90_MAX_NAME) :: varname, dimname
   character(len=NF90_MAX_NAME), allocatable :: timedimname(:,:)            ! time
   character(len=NF90_MAX_NAME), allocatable :: facedimname(:,:)            ! UGRID face / FM flow node
   character(len=NF90_MAX_NAME), allocatable :: nodedimname(:,:)            ! UGRID node / FM net node
   character(len=NF90_MAX_NAME), allocatable :: netedgedimname(:,:)         ! UGRID edge / FM net link
   character(len=NF90_MAX_NAME), allocatable :: netfacemaxnodesdimname(:,:) ! UGRID max face-nodes / FM max net cell nods
   character(len=NF90_MAX_NAME), allocatable :: edgedimname(:,:)            ! (no UGRID) / FM flow link
   character(len=NF90_MAX_NAME), allocatable :: netfacedimname(:,:)         ! (no UGRID) / FM net cell
   character(len=NF90_MAX_NAME), allocatable :: laydimname(:,:)             ! layer (mids)
   character(len=NF90_MAX_NAME), allocatable :: wdimname(:,:)               ! layer interfaces
   character(len=NF90_MAX_NAME), allocatable :: meshname(:,:)               ! mesh name
   integer,                      allocatable :: itimsel(:)
   double precision,             allocatable :: times(:)
   double precision,             allocatable :: timestep(:)
   logical :: isfound, needshift, exist
   integer :: size_btmp
   character(len=1) :: answer
   character*8  :: cdate
   character*10 :: ctime
   character*5  :: czone
   integer      :: nMaxMeshes, nMeshOld, ifileScan, ivScan, jaTopLevel, maxnvars, nvarsScan
   integer, allocatable :: ifile(:), nvars(:,:), max_nvars(:), varids_itopo(:)
   integer :: isOnMesh

   integer, allocatable :: links1d2dnodes(:,:,:), links1d2dtype(:,:), numl1d2d(:,:), nlink1d2dcount(:), numl1d2d_icontact(:)
   integer :: maxNcontacts, nLinks1d2d, numl1d2dMax, icontact
   character(len=NF90_MAX_NAME) :: contactname
   integer, allocatable, target  :: link1d2d_domain(:,:)
   integer, allocatable :: link1d2d_c2g(:,:), links1d2dnodes_g(:,:,:), nlink1d2dglob(:)
   integer :: numl1d2dt
   integer :: nMaxvars_contact
   integer, allocatable :: icfile(:), nvars_contact(:,:), varids_contact(:,:,:), id_contactvars(:,:), nMaxvars_contact_icontact(:)
   integer, allocatable :: nvarsel(:)
   integer :: jareadglob
   integer :: imesh, maxTopodim, minTopodim, topodimTmp, itopo
   integer, allocatable :: topodim(:,:)   ! For each topology index the topology dimension
   integer, allocatable :: mesh2topo(:,:) ! A mapping from meshid to topology index
   integer, allocatable :: topo2mesh(:,:) ! A mapping from topology index to meshid
   character(len=:), allocatable :: att_value
   integer :: ilink1d2dglob, idimtime, idimlink1d2d, idimstring, idim
   integer :: id_dimTwo

   if (nfiles <= 1) then
      write (*,'(a)') 'Error: mapmerge: At least two input files required.'
      ierr = 12
      goto 888
   else
      if (verbose_mode) then
         write (*,'(a,i0,a)') 'Info: mapmerge: Starting merge of ', nfiles, ' files...'
      end if
   end if

   noutfile = nfiles+1
   ncids      = -1
   id_time    = -1
   id_timedim = -1
   id_timestep= -1
   id_dimTwo  = -1
   nMeshOld   = 1 ! For old format input file, the number of meshes is 1

   !! 0a. Open input files
   call dfm_order_by_partition(infiles, nfiles)

   ierr = nf90_noerr
   isNetCDF4 = .false.
   do ii=1,nfiles
      ierri = ionc_open(infiles(ii), NF90_NOWRITE, ioncids(ii), iconvtype, convversion)
      if (ierri /= nf90_noerr) then
         write (*,'(a)') 'Error: mapmerge: could not open file `'//trim(infiles(ii))//'''.'
         ierr = ierri
         ncids(ii) = -1
      else
         if (iconvtype == IONC_CONV_UGRID .and. convversion >= 1.0) then
            jaugridi(ii) = 1
         else
            jaugridi(ii) = 0
         endif
      end if
      ierri = ionc_get_ncid(ioncids(ii), ncids(ii))
      ierri = nf90_inquire(ncids(ii), formatNum=formatCode)
      isNetCDF4 = (isNetCDF4 .or. formatCode == nf90_format_netcdf4 .or. formatCode == nf90_format_netcdf4_classic)
   end do
   if (any(ncids(1:nfiles) == -1)) then
      goto 888
   end if

   ! check if all the map files are the same format
   jaugrid = jaugridi(1)
   do ii=2,nfiles
      if (jaugridi(ii) .ne. jaugrid) then
         write (*,'(a)') 'Error: mapmerge: map files are not the same format.'
         ierr = 1
         ncids(ii) = -1
         exit
      endif
   enddo
   if (ierr /= nf90_noerr .and. .not. verbose_mode) then
      goto 888
   else
      if (jaugrid == 1) then
         write (*,'(a,i0,a)') 'Info: mapmerge: all input files are of UGRID format.'
      else
         write (*,'(a,i0,a)') 'Info: mapmerge: all input files are of old format.'
      end if
   end if

   do ii = 1, nfiles
      ierr = ionc_get_ncid(ioncids(ii), ncids(ii))
      if (ierr /= nf90_noerr) then
         write (*,'(a)') 'Error: mapmerge: could not get ncID for file: `'//trim(infiles(ii))//'''.'
         if (.not. verbose_mode) goto 888
      end if
   enddo



   !! 0b. Open output file
   if (len_trim(outfile) == 0) then
      nlen = len_trim(infiles(1))
      n3 = index(infiles(1)(1:nlen), '.', .true.) - 1  ! pos of '.nc'
      if (n3 < 0) then
         n3 = nlen
      end if
      Lrst_m = index(infiles(1), '_rst.nc')
      if (Lrst_m > 0) then ! If they are _rst files
         n1 = index(infiles(1)(1:Lrst_m), '_0000_', .true.)
         outfile = infiles(1)(1:n1) //'merged_'// infiles(1)(n1+6:Lrst_m)//'rst.nc'
         jamerge_cntv = 0
         write (*,'(a)') 'Info: mapmerge: for *_rst.nc files, topology connectivity variables (except for "FlowLink") are not merged.'
      else
      n2  = index(infiles(1)(1:n3), '_', .true.) - 1  ! pos of '_map'
      n1  = index(infiles(1)(1:n2), '_', .true.) - 1  ! pos of '_0000'
      if (n1 >= 0) then      ! mdident_0000_typ.nc --> mdident_merged_typ.nc
         outfile = infiles(1)(1:n1) // '_merged_'//infiles(1)(n2+2:n3)//'.nc'
      else if (n2 >= 0) then ! mdident_typ.nc      --> mdident_merged_typ.nc
         outfile = infiles(1)(1:n2) // '_merged_'//infiles(1)(n2+2:n3)//'.nc'
      else                   ! mdident.nc          --> mdident_merged_map.nc
         outfile = infiles(1)(1:n3) // '_merged_map.nc'
      end if
      endif
   end if
   ! Check for possible overwrite of outfile
   inquire(file=trim(outfile), exist=exist)
   if (exist .and. .not. force) then
      write (*, '(a)', advance='no') 'mapmerge: overwrite `'//trim(outfile)//'''? (Y/N) '
      answer = ''
      read (*,'(a)') answer
      if (.not. strcmpi(answer, 'Y')) then
         goto 888
      end if
   end if

   if (isNetCDF4) then
      ierr = nf90_create(outfile, NF90_HDF5, ncids(noutfile))
   else
      ierr = nf90_create(outfile, ior(NF90_CLOBBER, NF90_64BIT_OFFSET), ncids(noutfile))
   endif
   if (ierr /= nf90_noerr) then
      write (*,'(a)') 'Error: mapmerge: could not open file for writing: `'//trim(outfile)//'''.'
      if (.not. verbose_mode) goto 888
   end if


   maxlen = 16
   if (nfiles > 0) then
      maxlen = min(maxlen, maxval(len_trim(infiles(1:nfiles))))
   end if

   !! 0c. Prepare arrays and variables for the mesh number, mesh topology dimension.
   call realloc(nMesh, nfiles, keepExisting = .false., fill = 0)
   if (jaugrid == 1) then ! UGRID format
      ! Compute nMesh and maximal mesh number.
      nMaxMeshes = 0
      do ii = 1, nfiles
         ierr = ionc_get_mesh_count(ioncids(ii), nMesh(ii))
         if (nMesh(ii) > nMaxMeshes) then
            nMaxMeshes = nMesh(ii)
         end if
      end do
      ! Gets mesh topology from each input file, 
      ! fill in the mapping array mesh2topo that is mapping from meshid to topology index
      ! and compute maximal and minimal topology dimensions.
      call realloc(topodim,   (/nMaxMeshes,nfiles/), keepExisting = .false., fill = -1)
      call realloc(mesh2topo, (/nMaxMeshes,nfiles/), keepExisting = .false., fill = -1)
      maxTopodim = 0
      minTopodim = 999
      do ii = 1, nfiles
         do imesh = 1, nMesh(ii)
            ierr = ionc_get_topology_dimension(ioncids(ii), imesh, topodimTmp)
            topodim(imesh,ii) = topodimTmp
            mesh2topo(imesh,ii) = topodimTmp ! Here, topology dimension equals to topology index
            if (topodimTmp > maxTopodim) then
               maxTopodim = topodimTmp
            end if
            if (topodimTmp < minTopodim) then
               minTopodim = topodimTmp
            end if
         end do
      end do
      ! Fill in mapping array topo2mesh that is mapping from topology index to meshid
      call realloc(topo2mesh, (/maxTopodim,nfiles/), keepExisting = .false., fill = -1)
      do ii = 1, nfiles
         do imesh = 1, nMesh(ii)
            topo2mesh(topodim(imesh,ii),ii) = imesh ! Here, topology dimension equals to topology index
         end do
      end do
      ! For the situation,e.g. both file 1 and file 2 have only mesh2d, adjust the variables and arrays,
      ! because in this way, we do not need to allocate unnecessary dimension for the working arrays such as ndx, id_edgedim.
      ! In this situation, topology dimension is 2 in topodim, but the topology index is 1 in mesh2topo and topo2mesh.
      if (nMaxMeshes == 1 .and. maxTopodim == 2 .and. minTopodim == maxTopodim) then
         minTopodim = 1
         maxTopodim = 1
         mesh2topo  = 1
         call realloc(topo2mesh, (/maxTopodim,nfiles/), keepExisting = .false., fill = 1)
      end if
   else ! old format
      nMaxMeshes = nMeshOld
      nMesh(1:nfiles) = nMaxMeshes
      maxTopodim = nMeshOld
      minTopodim = nMeshOld
   end if

   !! 0d. Allocate arrays based on maxTopodim
   call realloc(id_timedim,            (/maxTopodim,nfiles+1/), keepExisting = .false., fill = -1)
   call realloc(id_facedim,            (/maxTopodim,nfiles+1/), keepExisting = .false., fill = -1)
   call realloc(id_edgedim,            (/maxTopodim,nfiles+1/), keepExisting = .false., fill = -1)
   call realloc(id_laydim,             (/maxTopodim,nfiles+1/), keepExisting = .false., fill = -1)
   call realloc(id_wdim,               (/maxTopodim,nfiles+1/), keepExisting = .false., fill = -1)
   call realloc(id_nodedim,            (/maxTopodim,nfiles+1/), keepExisting = .false., fill = -1)
   call realloc(id_sedtotdim,          (/maxTopodim,nfiles+1/), keepExisting = .false., fill = -1)
   call realloc(id_sedsusdim,          (/maxTopodim,nfiles+1/), keepExisting = .false., fill = -1)
   call realloc(id_nlyrdim,            (/maxTopodim,nfiles+1/), keepExisting = .false., fill = -1)
   call realloc(id_netedgedim,         (/maxTopodim,nfiles+1/), keepExisting = .false., fill = -1)
   call realloc(id_netfacedim,         (/maxTopodim,nfiles+1/), keepExisting = .false., fill = -1)
   call realloc(id_netfacemaxnodesdim, (/maxTopodim,nfiles+1/), keepExisting = .false., fill = -1)
   call realloc(id_bnddim,             (/maxTopodim,nfiles+1/), keepExisting = .false., fill = -1)

   call realloc(nt,              nfiles+1,                keepExisting = .false., fill = 0)
   call realloc(ndx,             (/maxTopodim,nfiles+1/), keepExisting = .false., fill = 0)
   call realloc(lnx,             (/maxTopodim,nfiles+1/), keepExisting = .false., fill = 0)
   call realloc(ndxg,            (/maxTopodim,nfiles+1/), keepExisting = .false., fill = 0)
   call realloc(lnxg,            (/maxTopodim,nfiles+1/), keepExisting = .false., fill = 0)
   call realloc(kmx,             (/maxTopodim,nfiles+1/), keepExisting = .false., fill = 0)
   call realloc(numk,            (/maxTopodim,nfiles+1/), keepExisting = .false., fill = 0)
   call realloc(numl,            (/maxTopodim,nfiles+1/), keepExisting = .false., fill = 0)
   call realloc(nump,            (/maxTopodim,nfiles+1/), keepExisting = .false., fill = 0)
   call realloc(numkg,           (/maxTopodim,nfiles+1/), keepExisting = .false., fill = 0)
   call realloc(numlg,           (/maxTopodim,nfiles+1/), keepExisting = .false., fill = 0)
   call realloc(netfacemaxnodes, (/maxTopodim,nfiles+1/), keepExisting = .false., fill = 0)
   call realloc(ndxbnd,          (/maxTopodim,nfiles+1/), keepExisting = .false., fill = 0)

   call realloc(timedimname,            (/maxTopodim,nfiles/), keepExisting = .false., fill = '')
   call realloc(facedimname,            (/maxTopodim,nfiles/), keepExisting = .false., fill = '')
   call realloc(nodedimname,            (/maxTopodim,nfiles/), keepExisting = .false., fill = '')
   call realloc(netedgedimname,         (/maxTopodim,nfiles/), keepExisting = .false., fill = '')
   call realloc(netfacemaxnodesdimname, (/maxTopodim,nfiles/), keepExisting = .false., fill = '')
   call realloc(edgedimname,            (/maxTopodim,nfiles/), keepExisting = .false., fill = '')
   call realloc(netfacedimname,         (/maxTopodim,nfiles/), keepExisting = .false., fill = '')
   call realloc(laydimname,             (/maxTopodim,nfiles/), keepExisting = .false., fill = '')
   call realloc(wdimname,               (/maxTopodim,nfiles/), keepExisting = .false., fill = '')
   call realloc(mesh_names,             (/maxTopodim,nfiles/), keepExisting = .false., fill = '')
   call realloc(meshname,               (/maxTopodim,nfiles/), keepExisting = .false., fill = '')

   !! 0e. Find the input file 'ifile', which has the most variables. This file will be the reference file where later we scan variables.
   ! For old format input file, ifile(maxTopodim) is the index of file that has the most variables.
   ! For Ugrid format, ifile(im) is index of file that have the most variables on mesh topology im.
   allocate(file_ndims(nfiles))
   call realloc(ifile,     maxTopodim, keepExisting = .false., fill = 1)
   call realloc(nvars,     (/maxTopodim, nfiles/), keepExisting = .false., fill = 0)
   call realloc(max_nvars, maxTopodim, keepExisting = .false., fill = 0)

   if (jaugrid == 0) then ! old format
      do ii = 1, nfiles
         ierr = nf90_inquire(ncids(ii), nVariables = nvars(maxTopodim,ii) )
         if ( ierr /= nf90_noerr ) then
             write (*,'(a)') 'Error: mapmerge: no variables found in file `'//trim(infiles(ii))//'''.'
             if (.not. verbose_mode) goto 888
         else
             ierr = nf90_inquire(ncids(ii), nDimensions = file_ndims(ii) )
             if ( ierr /= nf90_noerr ) then
                 write (*,'(a)') 'Error: mapmerge: no dimension found in file `'//trim(infiles(ii))//'''.'
                 if (.not. verbose_mode) goto 888
             endif
         endif
         if (nvars(maxTopodim,ii) > max_nvars(maxTopodim)) then
            max_nvars(maxTopodim) = nvars(maxTopodim,ii)
            ifile(maxTopodim) = ii   ! Set 'ifile' the one that has the most variables.
         endif
      end do
      nDims = file_ndims(ifile(maxTopodim)) ! nDims is equal to Nr. dimensions in ifile
   else ! UGrid
      do ii = 1, nfiles
         ierr = nf90_inquire(ncids(ii), nDimensions = file_ndims(ii))
         do imesh = 1, nMesh(ii)
            itopo = mesh2topo(imesh,ii)
            ierr = ionc_get_var_total_count(ioncids(ii), imesh, 0, 0, nvars(itopo,ii)) ! Get total number of variables on mesh imesh of file ii
            if (nvars(itopo,ii) > max_nvars(itopo)) then
               max_nvars(itopo) = nvars(itopo,ii)
               ifile(itopo) = ii
            end if
         end do
      enddo
      nDims = maxval(file_ndims)
   end if

   !! 1a. Scan for flownode (face), flow link (edge) and time dimensions in input files
   call realloc(dimids,     (/nDims,maxTopodim,nfiles+1/), keepExisting = .false., fill = -999)
   call realloc(dimids_uses,(/nDims,maxTopodim/),          keepExisting = .false. ,fill = 0)
   id_network  = -1
   do ii=1,nfiles
      if (ncids(ii) <= 0) then
         if (verbose_mode) then
            write (*,'(a)') 'Warning: mapmerge: Skipping scan of file `'//trim(infiles(ii))//'''.'
         end if
         cycle
      end if

      do itopo=minTopodim,maxTopodim ! Loop on each mesh topology to detect dimensions
         if (jaugrid == 1) then
            imesh = topo2mesh(itopo,ii) ! meshid
            if (imesh <= 0) then ! File ii does not have mesh topology itopo
               cycle
            end if
            ierr = ionc_get_mesh_name(ioncids(ii), imesh, mesh_names(itopo,ii))

            topodimTmp = topodim(imesh,ii) ! Topodim of imesh in file ii
            if (topodimTmp == 2 .or. topodimTmp == 1) then
               meshname(itopo,ii) = mesh_names(itopo,ii)

               if (topodimTmp == 1 .and. ii == ifile(itopo)) then
                  ! Check whether the mesh1d is defined on a network1d coordinate space or not:
                  ierr = ionc_get_network_id_from_mesh_id_ugrid(ioncids(ifile(itopo)), imesh, id_network)
                  if (ierr == ionc_noerr .and. id_network > 0) then
                     write (*,'(a)') 'Info: mapmerge: detected 1D network definition, this will be copied from '''//trim(infiles(ifile(itopo)))//''' to the merged file '''//trim(outfile)//'''.'

                     ierr = ionc_get_1d_netids(ioncids(ifile(itopo)), id_network, netids_input)
                     if (ierr /= nf90_noerr) then
                         write (*,'(a)') 'Error: mapmerge: cannot read dimemsion/variable ids of the 1D network data.'
                         goto 888
                     end if

                     ! Mark dimids_uses to -1 for the 3 dimensions that are used for network data
                     ! This means: do not copy them here in dfm_merge (instead done by io_netcdf), but also do not give a warning about them.
                     dimids_uses(netids_input%dimids(ntdim_1dnodes),itopo)     = -1
                     dimids_uses(netids_input%dimids(ntdim_1dgeopoints),itopo) = -1
                     dimids_uses(netids_input%dimids(ntdim_1dedges),itopo)     = -1
                  end if
               end if

               ! find the mesh topology variables
               ! face -netelem
               if (topodimTmp /= 1) then
                  ierr = ionc_get_dimid(ioncids(ii), imesh, mdim_face, id)
                  ierr = nf90_inquire_dimension(ncids(ii), id, name = dimname, len = nlen)
                  id_facedim(itopo,ii) = id
                  facedimname(itopo,ii)= dimname
                  ndx(itopo,ii)        = nlen
                  nump(itopo,ii)       = nlen
               end if
               ! net node
               ierr = ionc_get_dimid(ioncids(ii), imesh, mdim_node, id)
               ierr = nf90_inquire_dimension(ncids(ii), id, name = dimname, len = nlen)
               id_nodedim(itopo,ii) = id
               nodedimname(itopo,ii)= dimname
               numk(itopo,ii)       = nlen
               if (topodimTmp == 1) then
                  ndx(itopo,ii) = nlen ! In 1D map file, the net nodes are flow nodes
               end if
               ! edge
               ierr = ionc_get_dimid(ioncids(ii), imesh, mdim_edge, id)
               ierr = nf90_inquire_dimension(ncids(ii), id, name = dimname, len = nlen)
               id_netedgedim(itopo,ii) = id
               netedgedimname(itopo,ii)= dimname ! note: ugrid has no flow link, netlink only (==edge)
               numl(itopo,ii)          = nlen
               if (topodimTmp == 1) then
                  lnx(itopo,ii) = nlen ! In 1D map file, the edges are flow links
               end if
               ! max face node
               if (topodimTmp /= 1) then
                  ierr = ionc_get_dimid(ioncids(ii), imesh, mdim_maxfacenodes, id)
                  ierr = nf90_inquire_dimension(ncids(ii), id, name = dimname, len = nlen)
                  if (id > 0) then
                     dimids(id,itopo,ii) = id
                  end if
                  id_netfacemaxnodesdim(itopo,ii) = id
                  netfacemaxnodesdimname(itopo,ii)= dimname
                  netfacemaxnodes(itopo,ii)       = nlen
               end if

               ! Dimensions for 3D models
               if (topodimTmp /= 1) then ! mesh1d does not support layer yet
                  ierr = ionc_get_dimid(ioncids(ii), imesh, mdim_layer, id)
                  ierr = nf90_inquire_dimension(ncids(ii), id, name = dimname, len = nlen)
                  if (ierr == nf90_noerr) then
                     id_laydim(itopo,ii) = id
                     laydimname(itopo,ii)= dimname
                     kmx(itopo,ii)       = nlen
                  else
                     ierr = nf90_noerr
                  end if
                  ierr = ionc_get_dimid(ioncids(ii), imesh, mdim_interface, id)
                  ierr = nf90_inquire_dimension(ncids(ii), id, name = dimname, len = nlen)
                  if (ierr == nf90_noerr) then
                     id_wdim(itopo,ii) = id
                     wdimname(itopo,ii)= dimname
                  else
                     ierr = nf90_noerr
                  end if
               end if

               ! other dimensions
               do id = 1, file_ndims(ii)
                  if (dimids_uses(id, itopo) == - 1) then
                     cycle ! -1 means "silently ignore": do not scan these dimensions (e.g., the 3 dimensions that are used for network data)
                  end if
                  ierr = nf90_inquire_dimension(ncids(ii), id, name = dimname, len = nlen)
                  if (ierr /= nf90_noerr) then
                     write (*,'(a,i0,a)') 'Error: mapmerge: unable to read dimension information from file `'//trim(infiles(ii))//''' for #', id,'.'
                     if (.not. verbose_mode) goto 888
                  end if
                  if (trim(dimname) == 'time') then ! detect time dimension only once in each file
                     !! Time dimension
                     id_timedim(itopo,ii) = id
                     timedimname(itopo,ii)= dimname
                     nt(ii)         = nlen
                     cycle
                  end if
                  ! check if it is a dimension for sediment variables.
                  ! The sediment related dimensions cannot be dinstinguished on mesh 1d or 2d.
                  if (strcmpi(dimname, 'nSedTot')) then
                     id_sedtotdim(itopo,ii) = id
                     dimids(id,itopo,ii) = id
                     cycle
                  else if (strcmpi(dimname, 'nSedSus')) then
                     id_sedsusdim(itopo,ii) = id
                     dimids(id,itopo,ii) = id
                     cycle
                  else if (strcmpi(dimname, 'nBedLayers')) then
                     id_nlyrdim(itopo,ii) = id
                     dimids(id,itopo,ii) = id
                     cycle
                  end if
                  ! Check if this dimension is related to mesh imesh
                  isOnMesh = ionc_check_dim_on_a_mesh(ioncids(ii), imesh, topodimTmp, id)
                  if (isOnMesh == 0 .and. index(dimname, trim(meshname(itopo,ii))) == 0) then
                     ! Dimension "nmesh1d_FlowElemContourPts" cannot be checked by ionc_check_dim_on_a_mesh, so add another check on dimname
                     cycle
                  end if
                  if (id /= id_facedim(itopo,ii) .and. id /= id_nodedim(itopo,ii) .and. id /= id_netedgedim(itopo,ii) .and. id /= id_netfacemaxnodesdim(itopo,ii) .and. id /= id_laydim(itopo,ii) .and. id /= id_wdim(itopo,ii)) then
                     ! No special dimension, so probably just some vectormax-type dimension that
                     ! we may need later for some variables, so store it.
                     dimids(id,itopo,ii) = id ! Only stored to filter on non-missing values in def_dim loop later
                  endif
               enddo
            end if
         else ! old format
            do id=1,file_ndims(ii)
               ierr = nf90_inquire_dimension(ncids(ii), id, name = dimname, len = nlen) ! NetCDF-F90 allows us to assume that the dim IDs are 1:ndims
               if (ierr /= nf90_noerr) then
                  write (*,'(a,i0,a)') 'Error: mapmerge: unable to read dimension information from file `'//trim(infiles(ii))//''' for #', id,'.'
                  if (.not. verbose_mode) goto 888
               end if

               if (trim(dimname) == 'nFlowElem') then
               !! Flow nodes (face) dimension
                  id_facedim(itopo,ii) = id
                  facedimname(itopo,ii)= dimname
                  ndx(itopo,ii)        = nlen

               else if (trim(dimname) == 'nFlowLink') then
               !! Flow links (edge) dimension
                  id_edgedim(itopo,ii) = id
                  edgedimname(itopo,ii)= dimname
                  lnx(itopo,ii)        = nlen

               else if (trim(dimname) == 'laydim') then ! TODO: AvD: also wdim?
                  id_laydim(itopo,ii) = id
                  laydimname(itopo,ii)= dimname
                  kmx(itopo,ii)       = nlen
               else if (trim(dimname) == 'wdim') then
                  id_wdim(itopo,ii) = id
                  wdimname(itopo,ii)= dimname

            !! Now some Net* related dimensions (in addition to Flow*).

               else if (trim(dimname) == 'nNetElem') then
               !! Net cells (again face) dimension
                  id_netfacedim(itopo,ii) = id
                  netfacedimname(itopo,ii)= dimname
                  nump(itopo,ii)          = nlen

               else if (trim(dimname) == 'nNetElemMaxNode') then ! TODO: AvD: now we detect nNetElemMaxNode, but should be not change to nFlowElemMaxNode, now that facedim is the overall counter and netfacedim is hardly used anymore?
                  dimids(id,itopo,ii) = id ! Store this now, because later it is just a vectormax dim, so should be available in dim filter
                  id_netfacemaxnodesdim(itopo,ii) = id
                  netfacemaxnodesdimname(itopo,ii)= dimname
                  netfacemaxnodes(itopo,ii)       = nlen

               else if (trim(dimname) == 'nNetNode') then
               !! Net nodes (node) dimension
                  id_nodedim(itopo,ii) = id
                  nodedimname(itopo,ii)= dimname
                  numk(itopo,ii)       = nlen

               else if (trim(dimname) == 'nNetLink') then
               !! Net links (again edge) dimension
                  id_netedgedim(itopo,ii) = id
                  netedgedimname(itopo,ii)= dimname
                  numl(itopo,ii)          = nlen

               else if (trim(dimname) == 'time') then
               !! Time dimension
                  id_timedim(itopo,ii) = id
                  timedimname(itopo,ii)= dimname
                  nt(ii)         = nlen
               else if (trim(dimname) == 'nFlowElemBnd') then
               !! Flow nodes (face) boundary points dimension
                  id_bnddim(itopo,ii) = id
                  ndxbnd(itopo,ii)   = nlen
                  ! TODO: dimname needed?
                  if (verbose_mode) then
                     write (*,'(a)') 'Info: mapmerge: find dimension of boundary waterlevel points in file `'//trim(infiles(ii))//'''.'
                  endif
               else
                  ! No special dimension, so probably just some vectormax-type dimension that
                  ! we may need later for some variables, so store it.
                  dimids(id,itopo,ii) = id ! Only stored to filter on non-missing values in def_dim loop later

                  ! check if it is a dimension for sediment variables
                  if (strcmpi(dimname, 'nSedTot')) then
                     id_sedtotdim(itopo,ii) = id
                  else if (strcmpi(dimname, 'nSedSus')) then
                     id_sedsusdim(itopo,ii) = id
                  else if (strcmpi(dimname, 'nBedLayers')) then
                     id_nlyrdim(itopo,ii) = id
                  end if
               end if
            end do ! id
         end if ! ugrid or not
      end do ! itopo
   end do ! ii

   !! Prepare for the big loop on mesh topology, i.e. allocating relative arrays.
   maxnvars = maxval(max_nvars)
   allocate(varids(nfiles, maxnvars, maxTopodim));
   allocate(varids_out(maxnvars, maxTopodim))
   allocate(var_names(maxnvars, maxTopodim))
   allocate(var_types(maxnvars, maxTopodim))
   allocate(var_dimids(MAX_VAR_DIMS,maxnvars, maxTopodim))
   allocate(var_timdimpos(maxnvars, maxTopodim));   var_timdimpos   = -1
   allocate(var_spacedimpos(maxnvars, maxTopodim)); var_spacedimpos = -1
   allocate(var_laydimpos(maxnvars, maxTopodim));   var_laydimpos   = -1
   allocate(var_kxdimpos(4, maxnvars, maxTopodim)); var_kxdimpos    = -1
   allocate(var_wdimpos(maxnvars, maxTopodim));     var_wdimpos     = -1
   allocate(var_seddimpos(maxnvars, maxTopodim));   var_seddimpos   = -1
   allocate(var_ndims(maxnvars, maxTopodim));       var_ndims       =  0
   allocate(var_loctype(maxnvars, maxTopodim));     var_loctype     =  0
   allocate(numkx(maxnvars, maxTopodim));           numkx           =  0

   ! Allocate arrays for merging
   allocate(nfaceglob(maxTopodim));    nfaceglob = 0 !< total number of flow nodes (faces) without duplicates, on each mesh
   allocate(nedgeglob(maxTopodim));    nedgeglob = 0 !< total number of flow links (edges) without duplicates, on each mesh
   allocate(nnodeglob(maxTopodim));    nnodeglob = 0 !< total number of net nodes (nodes) without duplicates, on each mesh
   allocate(nnetedgeglob(maxTopodim)); nnetedgeglob = 0 !< total number of net links (edges) without duplicates, on each mesh
   allocate(nbndglob(maxTopodim));     nbndglob = 0 !< total number of boundary waterlevel points without duplicates, on each mesh

   call realloc(ndxc,     maxTopodim, keepExisting=.false., fill=0)
   call realloc(lnxc,     maxTopodim, keepExisting=.false., fill=0)
   call realloc(numkc,    maxTopodim, keepExisting=.false., fill=0)
   call realloc(numlc,    maxTopodim, keepExisting=.false., fill=0)
   call realloc(ndx_bndc, maxTopodim, keepExisting=.false., fill=0)

   do itopo = minTopodim, maxTopodim
      ndxc(itopo)     = sum(ndx(itopo,1:nfiles))
      lnxc(itopo)     = sum(lnx(itopo,1:nfiles))
      numkc(itopo)    = sum(numk(itopo,1:nfiles))
      numlc(itopo)    = sum(numl(itopo,1:nfiles))
      ndx_bndc(itopo) = sum(ndxbnd(itopo,1:nfiles))
   end do
   ndxMax = maxval(ndxc)
   ndxt   = sum(ndxc) ! total number of faces on all meshes
   call realloc(face_domain, (/ndxMax,maxTopodim/), keepExisting=.false., fill=-1)
   call realloc(face_c2g,    (/ndxMax,maxTopodim/), keepExisting=.false., fill=-1)
   call realloc(face_g2c,    ndxt,                  keepExisting=.false., fill=-1)
   call realloc(face_c2cc,   ndxt,                  keepExisting=.false., fill=-1)

   lnxMax = maxval(lnxc)
   lnxt   = sum(lnxc) ! total number of links on all meshes
   call realloc(edge_domain, (/lnxMax,maxTopodim/), keepExisting=.false., fill=-1)
   call realloc(edge_c2g,    (/lnxMax,maxTopodim/), keepExisting=.false., fill=-1)
   call realloc(edge_g2c,    lnxt,                  keepExisting=.false., fill=-1)

   numkMax = maxval(numkc)
   numkt   = sum(numkc) ! total number of nodes on all meshes
   call realloc(node_domain, (/numkMax,maxTopodim/), keepExisting=.false., fill=huge(1))
   call realloc(node_c2g,    (/numkMax,maxTopodim/), keepExisting=.false., fill=-1)
   call realloc(node_g2c,    numkt,                  keepExisting=.false., fill=-1)

   numlMax = maxval(numlc)
   numlt   = sum(numlc) ! total number of net edges on all meshes
   call realloc(netedge_domain, (/numlMax,maxTopodim/),   keepExisting=.false., fill=-1)
   call realloc(netedge_c2g,    (/numlMax,maxTopodim/),   keepExisting=.false., fill=-1)
   call realloc(edgenodes,      (/2,numlMax,maxTopodim/), keepExisting=.false., fill=-1)
   call realloc(netedge_g2c,    numlt,                    keepExisting=.false., fill=-1)

   ndx_bndMax = maxval(ndx_bndc)
   call realloc(facebnd_domain, (/ndx_bndMax,maxTopodim/), keepExisting=.false., fill =-1)

   call realloc(nfacecount,  maxTopodim, keepExisting=.false., fill = 0) !< running total of ndx(ii) while iterating the files on a cetrain mesh im
   call realloc(nedgecount,  maxTopodim, keepExisting=.false., fill = 0) !< running total of lnx(ii) while iterating the files on a cetrain mesh im
   call realloc(nnodecount,  maxTopodim, keepExisting=.false., fill = 0) !< running total of numk(ii) while iterating the files on a cetrain mesh im
   call realloc(nnetedgecount,  maxTopodim, keepExisting=.false., fill = 0) !< running total of numl(ii) while iterating the  on a cetrain mesh im
   call realloc(nbndcount,  maxTopodim, keepExisting=.false., fill = 0) !< total number of boundary waterlevel points on a cetrain mesh im
   
   call realloc(nvarsel, maxTopodim, keepExisting=.false.,fill=0)
   
   !! 2. Start the loops on each mesh topology, to define dimensions and variables. One loop includes:
   !! scaning variables (2a.), 
   !! defining top level attributes(only once) (2b.), including time dimensions and variables,
   !! constructing merged flow geometry (3a.),
   !! defining dimensions (3b), and
   !! defining variables (4.).
   jareadglob = 0
   jaTopLevel = 0
   do itopo=minTopodim,maxTopodim ! Scan on different mesh topology
      ifileScan = ifile(itopo) ! will scan variables in ifileScan in the following

      !! 2a. Scan for variables in the file which has the most dimension (and variables).
      if (verbose_mode) then
         if (jaugrid == 0) then
            write (*,'(a)') 'Info: mapmerge: Scan for variables in file `'//trim(infiles(ifileScan))//'''.'
         else
            write (*,'(a)') 'Info: mapmerge: Scan for variables of `'//trim(meshname(itopo,ifileScan))//''' in file `'//trim(infiles(ifileScan))//'''.'
         end if
      endif
      if ( ierr /= nf90_noerr) then
         if (jaugrid == 0) then
            write (*,'(a)') 'Error: mapmerge: no variables found in file `'//trim(infiles(ifileScan))//'''.'
         else
            write (*,'(a)') 'Error: mapmerge: no variables of `'//trim(meshname(itopo,ifileScan))//''' found in file `'//trim(infiles(ifileScan))//'''.'
         end if
         if (.not. verbose_mode) goto 888
      endif

      if (verbose_mode) then
         write (*,'(a)') '## Selecting variables to include in merge:'
      end if

      nvarsScan = nvars(itopo,ifileScan)
      if (jaugrid == 1) then
         call realloc(varids_itopo, nvarsScan, keepExisting = .false., fill = -1)
         ! Get the variable ids on the current mesh topology
         ierr = ionc_get_var_total_count(ioncids(ifileScan), topo2mesh(itopo,ifileScan), 0, 1, nvarsScan, varids=varids_itopo)
      end if

      do iv = 1,nvarsScan
         if (jaugrid == 1) then
            ivScan = varids_itopo(iv)
         else
            ivScan = iv
         end if
         ierr = nf90_inquire_variable(ncids(ifileScan), ivScan, name=varname, xtype=vartype, ndims=nvardims, dimids=tmpdimids)
         if (nvardims == 1) then
            if (tmpdimids(1) == id_timedim(itopo,ifileScan) .and. trim(varname) == 'time') then
               id_time(ifileScan) = ivScan ! TODO: AvD: do this for all ii files, not only for 'ifileScan'
               if (verbose_mode) then
                  write (*,'(a)') 'Found time variable in file `'//trim(infiles(ifileScan))//''': `'//trim(varname)//'''.'
               end if
      
               cycle ! This was time, continue searching for remaining data variables.
            elseif (tmpdimids(1) == id_timedim(itopo,ifileScan) .and. trim(varname) == 'timestep') then
               id_timestep(ifileScan) = ivScan ! TODO: AvD: do this for all ii files, not only for 'ifileScan'
               cycle
            end if
         end if
!      
!         ! It was not time, see if this is a geometry or data variable on flow nodes/links/net nodes:
         ivarcandidate = nvarsel(itopo)+1
         isfound = .true.
         ilastdim = MAX_VAR_DIMS
         ifirstdim = ilastdim+1 ! dummy start: no dims detected yet.
         do id=nvardims,1,-1
            if (tmpdimids(id) == id_timedim(itopo,ifileScan)) then
               ifirstdim = ifirstdim-1
               var_timdimpos(ivarcandidate,itopo) = ifirstdim
            else if (tmpdimids(id) == id_facedim(itopo,ifileScan)) then
               tmpdimids(id) = id_facedim(itopo,ifileScan) ! replace netfacedim by (flow)facedim
               ifirstdim = ifirstdim-1
               var_loctype(ivarcandidate,itopo) = UNC_LOC_S
               var_spacedimpos(ivarcandidate,itopo) = ifirstdim
            else if (tmpdimids(id) == id_netfacedim(itopo,ifileScan)) then
               tmpdimids(id) = id_netfacedim(itopo,ifileScan)
               ifirstdim = ifirstdim-1
               var_loctype(ivarcandidate,itopo) = UNC_LOC_SN       ! UNST-1256: original UNST_LOC_S caused NetElemNode variable in merged file to be on nFlowElem dimension, whereas input is on nNetElem dimension.
               var_spacedimpos(ivarcandidate,itopo) = ifirstdim
            else if (tmpdimids(id) == id_edgedim(itopo,ifileScan)) then
               ifirstdim = ifirstdim-1
               var_loctype(ivarcandidate,itopo) = UNC_LOC_U
               var_spacedimpos(ivarcandidate,itopo) = ifirstdim
            else if (tmpdimids(id) == id_nodedim(itopo,ifileScan)) then
               ifirstdim = ifirstdim-1
               var_loctype(ivarcandidate,itopo) = UNC_LOC_CN
               var_spacedimpos(ivarcandidate,itopo) = ifirstdim
            else if (tmpdimids(id) == id_netedgedim(itopo,ifileScan)) then
               ifirstdim = ifirstdim-1
               var_loctype(ivarcandidate,itopo) = UNC_LOC_L
               var_spacedimpos(ivarcandidate,itopo) = ifirstdim
            else if (tmpdimids(id) == id_laydim(itopo,ifileScan)) then
               ! TODO: do we really not need to set the S3D/W3D here?
               ifirstdim = ifirstdim-1
             !  var_loctype(ivarcandidate) = UNC_LOC_S3D
               var_laydimpos(ivarcandidate,itopo) = ifirstdim
            else if (tmpdimids(id) == id_wdim(itopo,ifileScan)) then
               ifirstdim = ifirstdim-1
               !var_loctype(ivarcandidate) = UNC_LOC_W
               var_wdimpos(ivarcandidate,itopo) = ifirstdim
            else if (tmpdimids(id) == id_bnddim(itopo,ifileScan)) then
               tmpdimids(id) = id_bnddim(itopo,ifileScan)
               ifirstdim = ifirstdim-1
               var_loctype(ivarcandidate,itopo) = UNC_LOC_SBND
               var_spacedimpos(ivarcandidate,itopo) = ifirstdim
            else
               if (var_kxdimpos(numkx(ivarcandidate,itopo)+1,ivarcandidate,itopo) == -1) then
                  ifirstdim = ifirstdim-1
                  var_kxdimpos(numkx(ivarcandidate,itopo)+1,ivarcandidate,itopo) = ifirstdim
                  numkx(ivarcandidate,itopo) = numkx(ivarcandidate,itopo)+1 ! Add extra detected kxdim for this variable
                  ! count how many times this dimension is used
                  id_infile = tmpdimids(id)
                  dimids_uses(id_infile,itopo) = dimids_uses(id_infile,itopo) + 1
                  if (tmpdimids(id) == id_sedtotdim(itopo,ifileScan) .or. tmpdimids(id) == id_sedsusdim(itopo,ifileScan)) then
                     var_seddimpos(ivarcandidate,itopo) = ifirstdim
                  end if
               else
                  if (verbose_mode) then
                     write (*,'(a)')           'Error: mapmerge: detected more than one vectormax dimension for `'//trim(varname)//''':'
                     ! write (*,'(a,i0,a,i0,a)') '       current: ', id, ', other: ', var_kxdimpos(ivarcandidate,itopo), '. Skipping this variable.'
                  end if
                  isfound = .false.
                  exit ! Stop scanning any remaining dimensions for this var.
               end if
            end if
            var_dimids(ifirstdim,ivarcandidate,itopo) = tmpdimids(id) ! for debugging only: temp store the dimids from file #1
         end do ! id

         ! We can only merge a variable across multiple domains *if* it is defined on space locations (face/edge/node)
         ! *or* if it is a special time-related variable. All others impossible to merge.
         if (var_spacedimpos(ivarcandidate,itopo) <= 0 .and. var_timdimpos(ivarcandidate,itopo) <= 0 .and. nvardims .ne. 0) then
            if (nvardims == 1 .and. (var_laydimpos(ivarcandidate,itopo) > 0 .or. var_wdimpos(ivarcandidate,itopo) > 0) .and. verbose_mode) then
               write (*,'(a)')'Info: mapmerge: Variable `'//trim(varname)//''' will be copied from one of the input files to the merged file.'
            else
               if (verbose_mode) then
                  write (*,'(a)') 'Error: mapmerge: detected that variable `'//trim(varname)//''': is not defined on space' &
                  //'locations, and is not a special time-related variable. Skipping this variable.'
               end if
               ! Make decrement -1 to all dimensions of the skipped variable, i.e. they are used one time less
               ! dimids_uses(id_infile,itopo) needs to be decremented -1 here for the kx dim
               do id=nvardims,1,-1
                  id_infile = tmpdimids(id)
                  dimids_uses(id_infile,itopo) = dimids_uses(id_infile,itopo) - 1
               enddo
               var_timdimpos(ivarcandidate,itopo)   = -1
               var_spacedimpos(ivarcandidate,itopo) = -1
               var_kxdimpos(:,ivarcandidate,itopo)  = -1
               var_laydimpos(ivarcandidate,itopo)   = -1
               var_wdimpos(ivarcandidate,itopo)     = -1
               var_seddimpos(ivarcandidate,itopo)   = -1
               isfound = .false.
            end if
         end if

         if (isfound) then
            nvarsel(itopo) = nvarsel(itopo) + 1 ! === ivarcandidate
            ! NOTE: Use variable ID from file #1 and assume that all input files contain the same variables, and as such the same consecutive variable IDs.
            varids(ifileScan, nvarsel(itopo),itopo) = ivScan
            var_names(nvarsel(itopo),itopo) = varname
            var_types(nvarsel(itopo),itopo) = vartype
            ! NOTE: all dimension positions were already stored, actual dimension ids not,
            ! because those are only needed for output file, will be done later.
            var_ndims(nvarsel(itopo),itopo) = ilastdim-ifirstdim+1
            if (verbose_mode) then
               tmpstr1 = ''
               nlen=0
               do id=ifirstdim,ilastdim
                  ierr = nf90_inquire_dimension(ncids(ifileScan), var_dimids(id,nvarsel(itopo),itopo), name=tmpstr2)
                  mlen = len_trim(tmpstr2)
                  tmpstr1 = tmpstr1(1:nlen)//', '//tmpstr2(1:mlen)
                  nlen = nlen + 2 + mlen
               end do
               write (*,'(a,i3,a,a)') ' * ', nvarsel(itopo), ': ', trim(varname)//'('//tmpstr1(3:nlen)//')'
            end if
            ! Set IDs of current variable to other input files, if those files does not have this variable, then set ID to -1
            do ii = 1, nfiles
               if (ii .ne. ifileScan) then
                  ierr = nf90_inq_varid(ncids(ii), varname, varids(ii, nvarsel(itopo),itopo))
                  if (ierr .ne. nf90_noerr) then
                     if (verbose_mode) then
                        write (*,'(a)') 'Info: mapmerge: file `'//trim(infiles(ii))//'''does not have variable `'//trim(varname)//'''.'
                     endif
                     ierr = 0
                     varids(ii,nvarsel(itopo),itopo) = -1
                  endif
               endif
            enddo
         end if
      end do ! iv

      if (verbose_mode) then
         if (nvarsel(itopo) == 0) then
            if (jaugrid == 0) then
               write (*,'(a)') 'Error: mapmerge: no variables found in file `'//infiles(ifileScan)//'''.'
            else
               write (*,'(a)') 'Error: mapmerge: no variables of `'//trim(meshname(itopo,ifileScan))//''' found in file `'//infiles(ifileScan)//'''.'
            end if
            if (.not. verbose_mode) goto 888
         end if
      end if

      !! 2b. Write top level attributes to file as a copy from input file (do this step only once).
      if (jaTopLevel == 0) then
         ierr = ncu_copy_atts(ncids(ifileScan), ncids(noutfile), nf90_global, nf90_global)

         ! We're making history here
         tmpstr1 = ''
         ierr = nf90_get_att(ncids(ifileScan), nf90_global, 'history', tmpstr1)
         if (ierr /= nf90_noerr) then
            mlen = 0
         else
            mlen = min(len(tmpstr1), len_trim(tmpstr1)+1)
            tmpstr1(mlen:mlen) = char(10) ! Prepare for our extra history line below by adding newline now already.
         end if
         
         call get_command (tmpstr2, nlen, ierr)
         if (ierr < 0) then ! command did not fit into string, abbreviate it.
            tmpstr2(len(tmpstr2)-2:len(tmpstr2)) = '...'
         end if

         call date_and_time(cdate, ctime, czone)
         ierr = nf90_put_att(ncids(noutfile), nf90_global, 'history', &
            tmpstr1(1:mlen)// &
            cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)//': '// &
            tmpstr2(1:nlen))

         ! Don't set 'date_created', hopefully it was in the original input files, and then we copy it and leave it at that source value.
         ierr = nf90_put_att(ncids(noutfile), nf90_global,  'date_modified', cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5))

         !! Define time dim&var with attributes in outputfile as a copy from input file (do this step only once).
         ierr = nf90_inq_varid(ncids(ifileScan), 'time', id_time(ifileScan))
         ierr = nf90_def_dim(ncids(noutfile), trim(timedimname(itopo,ifileScan)), nf90_unlimited, id_timedim(itopo,noutfile))
         if (maxTopodim > 1) then
            id_timedim(1:maxTopodim,noutfile) = id_timedim(itopo,noutfile)
         end if
         ierr = nf90_def_var(ncids(noutfile), 'time', nf90_double,    (/ id_timedim(itopo,noutfile) /), id_time   (noutfile))
         ierr = ncu_copy_atts(ncids(ifileScan), ncids(noutfile), id_time(ifileScan), id_time(noutfile))
         if (isNetCDF4) then
            ierr = ncu_copy_chunking_deflate(ncids(ifileScan), ncids(noutfile), id_time(ifileScan), id_time(noutfile))
         endif
         
         ierr = nf90_inq_varid(ncids(ifileScan), 'timestep', id_timestep(ifileScan))
         ierr = nf90_def_var(ncids(noutfile), 'timestep', nf90_double,    (/ id_timedim(itopo,noutfile) /), id_timestep(noutfile))
         ierr = ncu_copy_atts(ncids(ifileScan), ncids(noutfile), id_timestep(ifileScan), id_timestep(noutfile))
         if (isNetCDF4) then
            ierr = ncu_copy_chunking_deflate(ncids(ifileScan), ncids(noutfile), id_timestep(ifileScan), id_timestep(noutfile))
            if (id_timestep(ifileScan) < 0) then
               ! avoid very large chuncksize on Linux for timestep
               ierr = nf90_def_var_chunking(ncids(noutfile), id_timestep(noutfile), nf90_chunked, [512])
               if (ierr /= 0) write(*,*) 'nf90_def_var_chunking failed for var timestep'
            endif
         endif
         
         ! Only for ugrid format, because this variable will be defined in step 4 for old format
         if (jaugrid == 1) then
            varname = 'projected_coordinate_system'
            ierr = nf90_inq_varid(ncids(ifileScan), varname, id_mappingVar(ifileScan))
            if (ierr /= nf90_noerr) then
               varname = 'wgs84'
               ierr = nf90_inq_varid(ncids(ifileScan), varname, id_mappingVar(ifileScan))
            end if
            if (ierr == nf90_noerr) then
               ierr = nf90_def_var(ncids(noutfile), varname, nf90_int, id_mappingVar(noutfile))
               ierr = ncu_copy_atts(ncids(ifileScan), ncids(noutfile), id_mappingVar(ifileScan), id_mappingVar(noutfile))
            end if
         end if

         jaTopLevel = 1
      end if

      !! 3. Construct merged flow geometry (using proper cellmasks, global numbers, etc.
      !! 3a. Count dimensions (removing partition overlap) for merged flow nodes (faces) and flow links (edges).
      !nnetfacecount = 0 !< running total of nump(ii) while iterating the files

      !! Allocate temporary arrays for a certain itopo
      call realloc(ln, (/ 2, lnxc(itopo) /), keepExisting=.false.)

      netfacemaxnodesg = maxval(netfacemaxnodes(itopo,1:nfiles))
      call realloc(netfacenodes, (/ netfacemaxnodesg, sum(nump(itopo,1:nfiles)) /), keepExisting=.false., fill=-1)
      call realloc(netfaceedges, (/ netfacemaxnodesg, sum(nump(itopo,1:nfiles)) /), keepExisting=.false., fill=-1)
      call realloc(nfaceedges, sum(nump(itopo,1:nfiles)), keepExisting=.false., fill=0)

      call realloc(node_x, numkc(itopo), keepExisting=.false.)
      call realloc(node_y, numkc(itopo), keepExisting=.false.)
      call realloc(node_faces, (/ netnodemaxface+1, numkc(itopo)/), keepExisting=.false., fill= -1)
      ! Prepare node_faces array: the last value for each nodes is a counter of how many faces are surrounding this node
      node_faces(netnodemaxface+1, 1: numkc(itopo)) = 0

      call realloc(netedgefaces, (/2, numlc(itopo)/), keepExisting=.false., fill=-1)
      call realloc(edge_x, numlc(itopo), keepExisting=.false.)
      call realloc(edge_y, numlc(itopo), keepExisting=.false.)

      if (verbose_mode) then
         write (*,'(a)') '## Scanning input files for dimensions...'

         write (fmtstr, '(a,i0)') 'a', maxlen
         write (*,'('//trim(fmtstr)//',a3,4(a13),a8)') 'File', &
            ' : ', '   flow nodes', ' | flow links', ' |  net nodes', ' |  net links', ' | times'
      end if
      do ii=1,nfiles
         if (jaugrid == 1) then
            imesh = topo2mesh(itopo,ii)
            if (imesh <= 0) then ! file ii does not have the mesh of topodim == itopo
               cycle
            end if
            topodimTmp = topodim(imesh,ii)
         else
            topodimTmp = 2 ! For old format, the topology of mesh is 2d
         end if
         !! 3a.0 read domain numbers and global numbers of flow nodes
         if (topodimTmp /= 1) then
            nfaceglob0 = nfaceglob(itopo)
            face_domain(nfacecount(itopo)+1:nfacecount(itopo)+ndx(itopo,ii),itopo) = ii-1 ! Just set default domain if FlowElemDomain not present.
         else
            nnodeglob0 = nnodeglob(itopo)
            node_domain(nnodecount(itopo)+1:nnodecount(itopo)+ndx(itopo,ii),itopo) = ii-1
         end if
         if (jaugrid == 0) then
            ierr = nf90_inq_varid(ncids(ii), 'FlowElemDomain', id_flownodedomain)
         else
            ierr = ionc_inq_varid(ioncids(ii), imesh, 'flowelem_domain', id_flownodedomain)
         endif
         if (ierr == nf90_noerr) then
            if (topodimTmp /= 1) then
               ierr = nf90_get_var(ncids(ii), id_flownodedomain, face_domain(nfacecount(itopo)+1:nfacecount(itopo)+ndx(itopo,ii),itopo), count=(/ ndx(itopo,ii) /))
            else
               ierr = nf90_get_var(ncids(ii), id_flownodedomain, node_domain(nnodecount(itopo)+1:nnodecount(itopo)+ndx(itopo,ii),itopo), count=(/ ndx(itopo,ii) /))
            end if
            if (ierr /= nf90_noerr) then
               if (jaugrid == 0) then
                  write (*,'(a)') 'Error: mapmerge: could not retrieve FlowElemDomain from `'//trim(infiles(ii))//'''. '
               else
                  write (*,'(a)') 'Error: mapmerge: could not retrieve `'//trim(mesh_names(itopo,ii))//'''_flowelem_domain from `'//infiles(ii)//'''.'
               end if
               if (.not. verbose_mode) goto 888
            end if
         else
            ! no problem if FlowElemDomain is missing: just include all elems (default ii-1 was set above).
         end if

         if (ierr == nf90_noerr) then
            if (jaugrid == 0) then
               ierr = nf90_inq_varid(ncids(ii), 'FlowElemGlobalNr', id_flownodeglobnr)
            else
               ierr = ionc_inq_varid(ioncids(ii), imesh, 'flowelem_globalnr', id_flownodeglobnr)
            end if
         end if
         if (ierr == nf90_noerr) then
            if (topodimTmp /= 1) then
               ierr = nf90_get_var(ncids(ii), id_flownodeglobnr, face_c2g(nfacecount(itopo)+1:nfacecount(itopo)+ndx(itopo,ii),itopo), count=(/ ndx(itopo,ii) /))
            else
               ierr = nf90_get_var(ncids(ii), id_flownodeglobnr, node_c2g(nnodecount(itopo)+1:nnodecount(itopo)+ndx(itopo,ii),itopo), count=(/ ndx(itopo,ii) /))
               jareadglob = 1
            end if
            if (ierr /= nf90_noerr) then
               if (jaugrid == 0) then
                  write (*,'(a)') 'Error: mapmerge: could not retrieve FlowElemGlobalNr from `'//trim(infiles(ii))//'''. '
               else
                  write (*,'(a)') 'Error: mapmerge: could not retrieve `'//trim(mesh_names(itopo,ii))//'''_flowelem_globalnr from `'//infiles(ii)//'''.'
               end if
               if (.not. verbose_mode) goto 888
            end if
         else
            ! no problem if FlowElemGlobalNr is missing: just include all elems: global nr is equal to 'concat-index'.
            if (topodimTmp /= 1) then
               do ip=1,ndx(itopo,ii)
                  face_c2g(nfacecount(itopo)+ip,itopo) = nfacecount(itopo)+ip
               end do
            else
               do ip=1,ndx(itopo,ii)
                  node_c2g(nnodecount(itopo)+ip,itopo) = nnodecount(itopo)+ip
               end do
            end if
         end if
!      
         !! 3a.1: handle flow nodes (faces in 2D, nodes in 1D)
         ! Count the actual unique flow nodes (to get rid of partition overlap)
         if (topodimTmp /= 1) then
            do ip=1,ndx(itopo,ii)
               if (face_domain(nfacecount(itopo)+ip, itopo) == ii-1) then
                  nfaceglob(itopo) = nfaceglob(itopo)+1
                  ifaceglob = face_c2g(nfacecount(itopo)+ip,itopo)
                  face_g2c(ifaceglob) = nfacecount(itopo) + ip
                  face_c2cc(nfacecount(itopo) + ip) = nfaceglob(itopo) ! "contiguous to concatinated", meaning: mapping from including ghost nodes to excluding ghost nodes.
               end if
            end do
            ndxg(itopo,ii)   = nfaceglob(itopo)-nfaceglob0
         else
            ! In 1D the node_c2g mapping follows directly from flowelem_globalnr
            ! In 2D the node_g2c mapping if determined later, once faces and edges have been treated.
            ! TODO: merge this loop with the 2D node loop later, because nnodeglob and node_g2c treatment is the same here and there.
            do ip=1,ndx(itopo,ii)
               if (node_domain(nnodecount(itopo)+ip,itopo) == ii-1) then
                  nnodeglob(itopo) = nnodeglob(itopo)+1
                  inodeglob = node_c2g(nnodecount(itopo) + ip,itopo)
                  node_g2c(inodeglob) = nnodecount(itopo) + ip
               end if
            end do
            ndxg(itopo,ii) = nnodeglob(itopo)-nnodeglob0
            numkg(itopo,ii)= ndxg(itopo,ii)
         end if
      
         !! 3a.2: handle flow links (edges)
         if (jaugrid==0) then ! Only old format files contain FlowLink varaible, UGRID format files do not contain it
            nedgeglob0 = nedgeglob(itopo)
            ierr = nf90_inq_varid(ncids(ii), 'FlowLink', id_edgefaces)
            if (ierr == nf90_noerr) then
               ierr = nf90_get_var(ncids(ii), id_edgefaces, ln(:,nedgecount(itopo)+1:nedgecount(itopo)+lnx(itopo,ii)), count=(/ 2,lnx(itopo,ii) /))
            else
               write (*,'(a)') 'Warning: mapmerge: could not retrieve FlowLink from `'//trim(infiles(ii))//'''. '
               if (.not. verbose_mode) goto 888
            end if
      
            ! Count the actual unique flow links (to get rid of partition overlap, and also of duplicate boundary links)
            do ip=1,lnx(itopo,ii)
               ! For each link, take 2nd point (such that boundary links will be uniquely
               ! owned by the domain who owns the internal flow node of that link.)
               n1 = ln(1,nedgecount(itopo)+ip) ! Could be a boundary point
               n2 = ln(2,nedgecount(itopo)+ip)
               if (n1 > ndx(itopo,ii)) then
                  idom = ii - 1 ! Boundary mirrornodes are always owned by the domain itself.
                  isBndLink = 1 ! Mark the boundary link
               else
                  idom = face_domain(nfacecount(itopo)+n1,itopo)
               end if
      
               idom = min(idom, face_domain(nfacecount(itopo)+n2,itopo))
      
               if (isBndLink == 1 .and. face_domain(nfacecount(itopo)+n2,itopo) .ne. ii-1) then
               ! If this boundary link connects an interior flownode which does not belong to the current subdomain
                  idom = - 999
               endif
               edge_domain(nedgecount(itopo)+ip,itopo) = idom
               if (idom == ii-1) then
                  nedgeglob(itopo) = nedgeglob(itopo)+1
                  edge_g2c(nedgeglob(itopo)) = nedgecount(itopo) + ip
                  edge_c2g(nedgecount(itopo)+ip,itopo) = nedgeglob(itopo)
               end if
               isBndLink = 0
            end do
            lnxg(itopo,ii)   = nedgeglob(itopo)-nedgeglob0
         end if
!      
         !! 3a.3: handle net nodes (nodes)
         ! Only for 2D/3D map files: face-node connectivity
         if (topodimTmp /= 1) then
            nnodeglob0 = nnodeglob(itopo)
            if (jaugrid==0) then
               ierr = nf90_inq_varid(ncids(ii), 'NetElemNode', id_netfacenodes)
            else
               ierr = ionc_inq_varid(ioncids(ii), imesh, 'face_nodes', id_netfacenodes)
            endif
            if (ierr == nf90_noerr) then
               ierr = ncu_inq_var_fill(ncids(ii), id_netfacenodes, nofill, ifill_value)
               call realloc (netfacenodesl, (/ netfacemaxnodes(itopo,ii), nump(itopo,ii) /), keepExisting = .false.)
               ierr = nf90_get_var(ncids(ii), id_netfacenodes, netfacenodesl(:,:), count=(/ netfacemaxnodes(itopo,ii), nump(itopo,ii) /))
               do ip=1,nump(itopo,ii)
                 netfacenodes(1:netfacemaxnodes(itopo,ii),nfacecount(itopo)+ip) = netfacenodesl(1:netfacemaxnodes(itopo,ii),ip)
                 ! generate node_faces: the faces that surround a node
                 do ik =1, netfacemaxnodes(itopo,ii) ! for its every node
                     k1 = netfacenodesl(ik,ip)
                     if (k1 .ne. -1 .and. k1 .ne. ifill_value) then
                         nfaces = node_faces(netnodemaxface+1, nnodecount(itopo)+k1) + 1
                         node_faces(netnodemaxface+1, nnodecount(itopo)+k1) = nfaces ! update the counter
                         node_faces(nfaces,nnodecount(itopo)+k1) = nfacecount(itopo)+ip
                     end if
                 end do
               end do
            else
               if (jaugrid==0) then
                  write (*,'(a)') 'Warning: mapmerge: could not retrieve NetElemNode from `'//trim(infiles(ii))//'''. '
               else
                  write (*,'(a)') 'Warning: mapmerge: could not retrieve `'//trim(mesh_names(itopo,ii))//'''_face_nodes from `'//trim(infiles(ii))//'''. '
               end if
               if (.not. verbose_mode) goto 888
            end if
         end if

         ! read coordinates of net nodes
         if (jaugrid==0) then
            ierr = nf90_inq_varid(ncids(ii), 'NetNode_x', id_nodex)
            ierr = nf90_inq_varid(ncids(ii), 'NetNode_y', id_nodey)
            ierr = nf90_get_var(ncids(ii), id_nodex, node_x(nnodecount(itopo)+1:nnodecount(itopo)+numk(itopo,ii)))
            ierr = nf90_get_var(ncids(ii), id_nodey, node_y(nnodecount(itopo)+1:nnodecount(itopo)+numk(itopo,ii)))
         else
            ierr = ionc_inq_varid(ioncids(ii), imesh, 'node_x', id_nodex)
            ierr = ionc_inq_varid(ioncids(ii), imesh, 'node_y', id_nodey)
            ierr = nf90_get_var(ncids(ii), id_nodex, node_x(nnodecount(itopo)+1:nnodecount(itopo)+numk(itopo,ii)))
            ierr = nf90_get_var(ncids(ii), id_nodey, node_y(nnodecount(itopo)+1:nnodecount(itopo)+numk(itopo,ii)))
         end if
         if (ierr /= nf90_noerr) then
            jamerge_cntv = 0
            if (jaugrid == 0) then
               write (*,'(a)') 'Warning: mapmerge: could not retrieve coordinates of net nodes from `'//trim(infiles(ii))//'''. '
            else
               write (*,'(a)') 'Warning: mapmerge: could not retrieve coordinates of net nodes of `'//trim(meshname(itopo,ii))//''' from `'//infiles(ii)//'''.'
            end if
         end if

         ! Identify the node domain based on the already processed net cells, only for 2D/3D.
         if (topodimTmp /= 1) then
            do ip=1,nump(itopo,ii)
               do ik=1,netfacemaxnodes(itopo,ii)
                  k1 = netfacenodes(ik, nfacecount(itopo)+ip)
                  if (k1 == -1 .or. k1 == ifill_value) then
                     exit
                  end if
                  ! Owner of the node will be the lowest domain number of any of the cells surrounding that node.
                  node_domain(nnodecount(itopo)+k1,itopo) = min(node_domain(nnodecount(itopo)+k1,itopo), face_domain(nfacecount(itopo)+ip,itopo))
               end do
            end do
            !numpg(ii)   = nnetfaceglob-nnetfaceglob0
      
            ! Count the actual unique nodes (to get rid of partition overlap)
            do ip=1,numk(itopo,ii)
               idom = node_domain(nnodecount(itopo)+ip,itopo)
      
               if (idom == ii-1) then ! Second check is to identify orphan nodes (in case no 1D netcells were available on file)
                  nnodeglob(itopo) = nnodeglob(itopo)+1 ! TODO: orphan nodes are not handled correctly yet for 1D channel strings (duplicates?)
                  node_g2c(nnodeglob(itopo)) = nnodecount(itopo) + ip
                  node_c2g(nnodecount(itopo) + ip,itopo) = nnodeglob(itopo)
               end if
            end do
            numkg(itopo,ii)   = nnodeglob(itopo)-nnodeglob0
         end if

         !! 3a.4: handle net edges (also edges)
         nnetedgeglob0 = nnetedgeglob(itopo)
         if (jaugrid==0) then
            ierr = nf90_inq_varid(ncids(ii), 'NetLink', id_edgenodes)
         else
            ierr = ionc_inq_varid(ioncids(ii), imesh, 'edge_nodes', id_edgenodes)
         endif

         if (ierr == nf90_noerr) then
            ierr = nf90_get_var(ncids(ii), id_edgenodes, edgenodes(:,nnetedgecount(itopo)+1:nnetedgecount(itopo)+numl(itopo,ii),itopo), count=(/ 2, numl(itopo,ii) /))
         else
            if (jaugrid==0) then
               write (*,'(a)') 'Warning: mapmerge: could not retrieve NetLink from `'//trim(infiles(ii))//'''. '
            else
               write (*,'(a)') 'Warning: mapmerge: could not retrieve `'//trim(mesh_names(itopo,ii))//'''_edge_nodes from `'//trim(infiles(ii))//'''. '
            end if
            if (.not. verbose_mode) goto 888
         end if

         if (topodimTmp /= 1) then
            if (jaugrid==0) then
               ierr = nf90_inq_varid(ncids(ii), 'ElemLink', id_netedgefaces)
            else
               ierr = ionc_inq_varid(ioncids(ii), imesh, 'edge_faces', id_netedgefaces)
            end if
            if (ierr == nf90_noerr) then
               ierr = nf90_get_var(ncids(ii), id_netedgefaces, netedgefaces(:,nnetedgecount(itopo)+1:nnetedgecount(itopo)+numl(itopo,ii)), count=(/ 2, numl(itopo,ii) /))
            else
               if (jaugrid==0) then
                  write (*,'(a)') 'Warning: mapmerge: could not retrieve ElemLink from `'//trim(infiles(ii))//'''. '
               else
                  write (*,'(a)') 'Warning: mapmerge: could not retrieve `'//trim(mesh_names(itopo,ii))//'''_edge_faces from `'//trim(infiles(ii))//'''. '
               end if
               if (.not. verbose_mode) goto 888
            end if
      
            if (jaugrid==0) then
               ierr = nf90_inq_varid(ncids(ii), 'NetElemLink', id_netfaceedges)
               if (ierr == nf90_noerr) then
                  ierr = nf90_get_var(ncids(ii), id_netfaceedges, netfaceedges(1:netfacemaxnodes(itopo,ii),nfacecount(itopo)+1:nfacecount(itopo)+nump(itopo,ii)), count=(/ netfacemaxnodes(itopo,ii), nump(itopo,ii) /))
                  if (ierr /= nf90_noerr) then
                     jamerge_cntv = 0
                     write (*,'(a)') 'Warning: mapmerge: could not retrieve NetElemLink from `'//trim(infiles(ii))//'''. '
                  end if
               else
                  write (*,'(a)') 'Warning: mapmerge: could not retrieve NetElemLink from `'//trim(infiles(ii))//'''. '
                  if (.not. verbose_mode) goto 888
               end if
            else ! UGRID map file does not contain _face_edges, build it here: netfaceedges
               do iedge = 1, numl(itopo,ii)
                  do ikk = 1, 2
                     ic = netedgefaces(ikk,iedge+nnetedgecount(itopo))
                     if (ic > 0) then
                        iface = ic + nfacecount(itopo)
                        nfaceedges(iface) = nfaceedges(iface) +1
                        netfaceedges(nfaceedges(iface), iface) = iedge
                     end if
                  end do
               end do
            end if
         end if

         if (jaugrid ==0) then
            ! read coordinates of NetLinks
            ierr = nf90_inq_varid(ncids(ii), 'NetLink_xu', id_edgex)
            ierr = nf90_inq_varid(ncids(ii), 'NetLink_yu', id_edgey)
            ierr = nf90_get_var(ncids(ii), id_edgex, edge_x(nnetedgecount(itopo)+1:nnetedgecount(itopo)+numl(itopo,ii)))
            ierr = nf90_get_var(ncids(ii), id_edgey, edge_y(nnetedgecount(itopo)+1:nnetedgecount(itopo)+numl(itopo,ii)))
         else
            ierr = ionc_inq_varid(ioncids(ii), imesh, 'edge_x', id_edgex)
            ierr = ionc_inq_varid(ioncids(ii), imesh, 'edge_y', id_edgey)
            ierr = nf90_get_var(ncids(ii), id_edgex, edge_x(nnetedgecount(itopo)+1:nnetedgecount(itopo)+numl(itopo,ii)))
            ierr = nf90_get_var(ncids(ii), id_edgey, edge_y(nnetedgecount(itopo)+1:nnetedgecount(itopo)+numl(itopo,ii)))
         end if
         if (ierr /= nf90_noerr) then
            jamerge_cntv = 0
            if (jaugrid == 0) then
               write (*,'(a)') 'Warning: mapmerge: could not retrieve coordinates of net edges from `'//trim(infiles(ii))//'''. '
            else
               write (*,'(a)') 'Warning: mapmerge: could not retrieve coordinates of net edges of `'//trim(mesh_names(itopo,ii))//''' from `'//trim(infiles(ii))//'''. '
            end if
         end if

         ! Identify the net link domain based on the already processed net nodes
         do ip=1,numl(itopo,ii)
            k1 = edgenodes(1,nnetedgecount(itopo)+ip,itopo)
            k2 = edgenodes(2,nnetedgecount(itopo)+ip,itopo)
            ! NOTE: AvD: by taking the MAX below, I believe this also guarantees that one and
            !            the same domain owns both the net link and the associated flow link.
            if (topodimTmp /= 1) then
               idom = max(node_domain(nnodecount(itopo)+k1,itopo), node_domain(nnodecount(itopo)+k2,itopo))
            else
               ! in 1D: net edge==flow link, take lowest flow node domain number, equivalent to 2D.
               idom = min(node_domain(nnodecount(itopo)+k1,itopo), node_domain(nnodecount(itopo)+k2,itopo))
            end if

            netedge_domain(nnetedgecount(itopo)+ip,itopo) = idom

            if (idom == ii-1) then
               nnetedgeglob(itopo) = nnetedgeglob(itopo)+1
               netedge_g2c(nnetedgeglob(itopo)) = nnetedgecount(itopo) + ip
               netedge_c2g(nnetedgecount(itopo) + ip,itopo) = nnetedgeglob(itopo)
            end if
         end do
         numlg(itopo,ii)   = nnetedgeglob(itopo)-nnetedgeglob0
         if (topodimTmp == 1) then
            lnxg(itopo,ii) = numlg(itopo,ii)
         end if

         !! 3a.5: handle boundary waterlevel points
         if (ndxbnd(itopo,ii) > 0) then
            facebnd_domain(nbndcount(itopo)+1:nbndcount(itopo)+ndxbnd(itopo,ii),itopo) = ii-1
            nbndglob(itopo) = nbndglob(itopo) + ndxbnd(itopo,ii)
         else if (verbose_mode) then
            if (jaugrid == 0) then
               write (*,'(a)') 'Info: mapmerge: no waterlevel boundary in `'//trim(infiles(ii))//'''. '
            else
               write (*,'(a)') 'Info: mapmerge: no waterlevel boundary of `'//trim(mesh_names(itopo,ii))//''' from `'//trim(infiles(ii))//'''. '
            end if
         endif

         ! Intentional: all of these need to be done at very last instant:
         nedgecount(itopo)    = nedgecount(itopo)    + lnx(itopo,ii)
         nfacecount(itopo)    = nfacecount(itopo)    + ndx(itopo,ii)
         nnodecount(itopo)    = nnodecount(itopo)    + numk(itopo,ii)
         nnetedgecount(itopo) = nnetedgecount(itopo) + numl(itopo,ii)
         !nnetfacecount = nnetfacecount + nump(ii)
         nbndcount(itopo)     = nbndcount(itopo)     + ndxbnd(itopo,ii)

         if (verbose_mode) then
            nlen = len_trim(infiles(ii))
            plen = 3*max(0,sign(1,nlen-maxlen-1))

            write (fmtstr, '(a,i0)') 'a', maxlen
            write (*,'('//trim(fmtstr)//',a3,i7,3(i13),i8)') repeat('.', plen)//infiles(ii)(max(1,nlen-maxlen+1)+plen:nlen)//repeat(' ', max(0,maxlen-nlen-plen)), &
               ' : ', ndx(itopo,ii), lnx(itopo,ii), numk(itopo,ii), numl(itopo,ii), nt(ii)
            write (*,'('//trim(fmtstr)//',a3,4(i13))')    repeat(' ', maxlen-7)//'Removed', &
               ' : ', (ndxg(itopo,ii)-ndx(itopo,ii)), (lnxg(itopo,ii)-lnx(itopo,ii)), (numkg(itopo,ii)-numk(itopo,ii)), (numlg(itopo,ii)-numl(itopo,ii))
         end if
      end do ! ii

      !! 3a.6: Fulfill arrays for connectivity variables.
      if (jaugrid == 1) then
         imesh = topo2mesh(itopo,ifileScan)
         topodimTmp = topodim(imesh, ifileScan)
      else
         topodimTmp = 2 ! For old format, the mesh topology is 2d
      end if
      if (jamerge_cntv == 1) then
         if (topodimTmp /= 1) then ! for 1D map file, node_c2g has been read from the input map files, so skip
            !! fulfill node_c2g, because in domain that is larger than 0000, there are nodes which are in this domain but
            !  their domain numbers are another domain.
            do ii = 2, nfiles
                nfacecount(itopo) = sum(nump(itopo,1:ii-1))
                nnodecount(itopo) = sum(numk(itopo,1:ii-1))
                do ik=1,numk(itopo,ii)
                    if (node_c2g(nnodecount(itopo)+ik,itopo)==-1) then
                        jafound = 0
                        do ikk = 1, node_faces(netnodemaxface+1, nnodecount(itopo)+ik)
                            ic = node_faces(ikk,nnodecount(itopo)+ik) ! index of one surrounding cell
                            if (ic .ne. -1) then
                               if (face_domain(ic,itopo) .ne. ii-1 .and. face_domain(ic,itopo) == node_domain(nnodecount(itopo)+ik,itopo)) then ! cell ic is not in current domain, and is in the same domain with node ik
                                   ifaceglob = face_c2g(ic,itopo)
                                   ifacec = face_g2c(ifaceglob) ! and this is now from the OTHER domain.
                                   ! in which domain (iii) does iface belong
                                   do iii = 1, nfiles
                                       if (ifacec > sum(nump(itopo,1:iii-1)) .and. ifacec <= sum(nump(itopo,1:iii))) then
                                           ifacefile = iii
                                           exit
                                       end if
                                   end do
                                   ! Loop on all the nodes of face ifacec, to find the node which has the same coordinates with node ik from file ii
                                   do imm=1,netfacemaxnodes(itopo,ifacefile)
                                       k1 = netfacenodes(imm, ifacec)
                                       if (k1 .ne. -1 .and. k1 .ne. ifill_value) then
                                           k1c = k1 + sum(numk(itopo,1:ifacefile-1)) ! concatinated index of nodes
                                           xx = node_x(k1c)
                                           yy = node_y(k1c)
                                           if (abs(node_x(ik+nnodecount(itopo))-xx)<1d-10 .and. abs(node_y(ik+nnodecount(itopo))-yy)<1d-10) then
                                               node_c2g(ik+nnodecount(itopo),itopo) = node_c2g(k1c,itopo)
                                               jafound = 1
                                               exit
                                           end if
                                       end if
                                   end do
                               end if
                            end if
                        end do
                        if (jafound == 0 .and. verbose_mode) then
                            write (*,'(a,i0,a)') 'Warning: mapmerge: node_c2g: could not find global number for node # ', ik ,' of file `'//trim(infiles(ii))//'''. '
                        end if
                    end if
                end do
            end do
         end if

         !! for 2D/3D, fulfill netedge_c2g, because in domain larger than 0000, there are netedges which are in this domain
         !  but their domain numbers are another domain.
         if (topodimTmp /= 1) then
            do ii = 2, nfiles
               netedgecount = sum(numl(itopo,1:ii-1))
               nfacecount(itopo) = sum(nump(itopo,1:ii-1))
               ierr = ncu_inq_var_fill(ncids(ii), id_netedgefaces, nofill, ifill_value)
               do ip = 1, numl(itopo,ii)
                   k1 = netedgefaces(1, netedgecount+ip)  ! flowelem that edge L connects
                   k2 = netedgefaces(2, netedgecount+ip)
                   if (k1.ne.0 .and. k2.ne. 0 .and. k1.ne.ifill_value .and. k2.ne.ifill_value) then       ! When edge L is not on the boundary
                       g1 = face_domain(nfacecount(itopo)+k1,itopo)    ! domain number of this flownode
                       g2 = face_domain(nfacecount(itopo)+k2,itopo)
                       if (g1 .ne. g2) then               ! if edge L lies on the boundary of two different domains
                           if (g1==ii-1 .and. g1>g2) then ! decide which flowelem is in the current domain, which is not
                               ifacein = k1
                               ifaceout= k2
                           else if (g2==ii-1 .and. g2>g1) then
                               ifacein = k2
                               ifaceout= k1
                           else
                               cycle
                           end if
      
                           ifaceglob = face_c2g(ifaceout+nfacecount(itopo),itopo) ! for the flownode which is in another domain with smaller domain number
                           ifacec = face_g2c(ifaceglob)                  ! and this is now from the OTHER domain.
                           ! in which domain (iii) does iface belong
                           do iii = 1, nfiles
                               if (ifacec>sum(nump(itopo,1:iii-1)) .and. ifacec < sum(nump(itopo,1:iii))) then
                                   ifacefile = iii
                                   exit
                               end if
                           end do
                           ! Loop on all netedges of face ifacec
                           do imm = 1, netfacemaxnodes(itopo,ifacefile)
                               k1 = netfaceedges(imm, ifacec)
                               if (k1 .ne. -1 .and. k1 .ne. ifill_value) then
                                  if (k1 .ne. -1) then
                                      k1c= k1 + sum(numl(itopo,1:ifacefile-1))! concatinated index of the edge
                                      xx = edge_x(k1c)
                                      yy = edge_y(k1c)
                                      if (abs(edge_x(ip+netedgecount)-xx)<1d-10 .and. abs(edge_y(ip+netedgecount)-yy)<1d-10) then
                                          netedge_c2g(ip+netedgecount,itopo) = netedge_c2g(k1c,itopo)
                                          exit
                                      end if
                                  end if
                               end if
                           end do
                       end if
                   end if
               end do
            end do
         else ! for 1D, fill in netedge_c2g for edges whose domain numbers are not in current domain
            do ii = 1, nfiles
               netedgecount = sum(numl(itopo,1:ii-1))
               nnodecount(itopo)   = sum(numk(itopo,1:ii-1))
               do ip = 1, numl(itopo,ii)
                  if (netedge_c2g(netedgecount+ip,itopo) < 0) then ! to be filled in
                     k1 = edgenodes(1, netedgecount+ip,itopo)      ! flownode that edge ip connects
                     k2 = edgenodes(2, netedgecount+ip,itopo)
                     g1 = node_domain(nnodecount(itopo)+k1,itopo)     ! domain number of the node
                     g2 = node_domain(nnodecount(itopo)+k2,itopo)
                     if (g1 == g2) then                      ! k1 and k2 are in the same domain
                        inodefile = g1 + 1                   ! Later search edge ip in inodefile
                     else                                    ! edge ip is on a partition boundary
                        if (g1 == ii-1) then
                           inodefile = g2 + 1
                        else if (g2 == ii-1) then
                           inodefile = g1 + 1
                        end if
                     end if

                     ! search edge ip in inodefile, based on edge coordinates
                     netedgecount2 = sum(numl(itopo,1:inodefile-1))
                     ! Loop on all netedges
                     do imm = 1, numl(itopo,inodefile)
                        xx = edge_x(netedgecount2+imm)
                        yy = edge_y(netedgecount2+imm)
                        if (abs(edge_x(ip+netedgecount)-xx)<1d-10 .and. abs(edge_y(ip+netedgecount)-yy)<1d-10) then
                           netedge_c2g(ip+netedgecount,itopo) = netedge_c2g(netedgecount2+imm,itopo)
                           exit
                        end if
                     end do
                  end if
               end do
            end do
         end if
      end if
      nkmxglob = kmx(itopo,1)
      do ii = 2,nfiles
         if (kmx(itopo,ii) .ne. nkmxglob) then
            write (*,'(a,i0,a,i0,a)') 'Error: mapmerge: Numbers of layers are different in files: ', kmx(itopo,ii), 'layers in file`'//trim(infiles(ii))//''' and', &
                                       nkmxglob, 'layers in other files.'
            if (.not. verbose_mode) goto 888
         endif
      enddo

      if (topodimTmp /= 1) then
         ndx(itopo,noutfile) = nfaceglob(itopo)
         lnx(itopo,noutfile) = nedgeglob(itopo)
      else
         ndx(itopo,noutfile) = nnodeglob(itopo)
         lnx(itopo,noutfile) = nnetedgeglob(itopo)
      end if
      numk(itopo,noutfile) = nnodeglob(itopo)
      numl(itopo,noutfile) = nnetedgeglob(itopo)
      kmx (itopo,noutfile) = nkmxglob
      ndxbnd(itopo,noutfile) = nbndglob(itopo)

      if (verbose_mode) then
         nlen = len_trim(outfile)
         plen = 3*max(0,sign(1,nlen-maxlen-1))
      
         write (fmtstr, '(a,i0)') 'a', maxlen
         write (*,'('//trim(fmtstr)//',a3,4(i13))')    repeat(' ', maxlen-7)//'Net sum', &
            ' : ', sum(ndxg(itopo,1:nfiles)), sum(lnxg(itopo,1:nfiles)), sum(numkg(itopo,1:nfiles)), sum(numlg(itopo,1:nfiles))
         write (*,'('//trim(fmtstr)//',a3,4(i13))')    repeat(' ', maxlen-11)//'Check count', &
            ' : ', ndx(itopo,noutfile), lnx(itopo,noutfile), numk(itopo,noutfile), numl(itopo,noutfile)
      end if

      !! 3b. dimensions for merged flow nodes (faces) and flow links (edges), same for net nodes + links.
      if (jaugrid==0) then
         ierr = nf90_def_dim(ncids(noutfile), trim(netfacedimname(itopo,ifileScan)), ndx(itopo,noutfile), id_netfacedim(itopo,noutfile)) ! UNST-1256: temp fix, pending proper UGRID support in UNST-1134.
         ierr = nf90_def_dim(ncids(noutfile), trim(facedimname(itopo,ifileScan)), ndx(itopo,noutfile), id_facedim(itopo,noutfile))
         ierr = nf90_def_dim(ncids(noutfile), trim(edgedimname(itopo,ifileScan)), lnx(itopo,noutfile), id_edgedim(itopo,noutfile))
         if (kmx(itopo,noutfile) > 0) then
            ierr = nf90_def_dim(ncids(noutfile), trim(laydimname(itopo,ifileScan)), kmx(itopo,noutfile), id_laydim(itopo,noutfile))
            ierr = nf90_def_dim(ncids(noutfile), trim(wdimname(itopo,ifileScan)), kmx(itopo,noutfile)+1, id_wdim(itopo,noutfile))
         end if
         ierr = nf90_def_dim(ncids(noutfile), trim(nodedimname(itopo,ifileScan)), numk(itopo,noutfile), id_nodedim(itopo,noutfile))
         ierr = nf90_def_dim(ncids(noutfile), trim(netedgedimname(itopo,ifileScan)), numl(itopo,noutfile), id_netedgedim(itopo,noutfile))
         ierr = nf90_def_dim(ncids(noutfile), 'nFlowElemBnd', ndxbnd(itopo,noutfile), id_bnddim(itopo,noutfile))! TODO: Add if ndxbnd > 0
      else
         if (topodimTmp /= 1) then
            ierr = nf90_def_dim(ncids(noutfile), trim(facedimname(itopo,ifileScan)), ndx(itopo,noutfile),  id_facedim(itopo,noutfile))
         end if
         ierr = nf90_def_dim(ncids(noutfile), trim(netedgedimname(itopo,ifileScan)), numl(itopo,noutfile), id_netedgedim(itopo,noutfile))
         ierr = nf90_def_dim(ncids(noutfile), trim(nodedimname(itopo,ifileScan)), numk(itopo,noutfile), id_nodedim(itopo,noutfile))
         if (kmx(itopo,noutfile) > 0) then
            ierr = nf90_def_dim(ncids(noutfile), trim(laydimname(itopo,ifileScan)), kmx(itopo,noutfile), id_laydim(itopo,noutfile))
            ierr = nf90_def_dim(ncids(noutfile), trim(wdimname(itopo,ifileScan)), kmx(itopo,noutfile)+1, id_wdim(itopo,noutfile))
         end if
      end if

      !! 4. Define all variables (grid + data), including any remaining dimensions
      !! 4a. Simply copy all remaining dimensions (probably vectormax-like) to output file.
      do id=1,ndims
         if (dimids(id,itopo,ifileScan) > 0) then ! For now, just copy the vectormax dimids (if any) from file #1 to output file. Assume same length in all files.
            ierr = nf90_inquire_dimension(ncids(ifileScan), dimids(id,itopo,ifileScan), name = dimname, len = nlen)
            ! if 'nSedTot', 'nSedSus' or 'nBedLayers' is present, copy to the merged file
            if (dimname == 'nSedTot' .or. dimname == 'nSedSus' .or. dimname == 'nBedLayers') then
                dimids_uses(id,itopo) = 1
            endif
            if (dimids_uses(id,itopo) == 0) then
               write (*,'(a)') 'Info: mapmerge: Dimension `'//trim(dimname)//''' is not merged because no merged variable uses it. '
               cycle
            endif
            ! When one file has only triangular mesh and one file has only rectangular mesh, then a variable, e.g. 'NetElemNode'
            ! has dimension 3 and 4, respectively. Then this variable in the target merged map should have dimension nlen=4,
            ! which is the maximum (UNST-1842).
            if (dimname == 'nNetElemMaxNode' .or. dimname == 'max_n'//trim(meshname(itopo,ifileScan))//'_face_nodes'  .or. dimname == trim(meshname(itopo,ifileScan))//'_nMax_face_nodes' .or. dimname=='nFlowElemContourPts') then
               nlen = maxval(netfacemaxnodes)
            end if

            if (jaugrid == 1 .and. maxTopodim == 2 .and. dimname == 'Two' .and. id_dimTwo > 0) then ! mesh1d and mesh2d share the same dimension "Two", and it has been defined in the output file
               dimids(id,itopo,noutfile) = id_dimTwo
               cycle
            end if

            if (ierr == nf90_noerr) then
               ierr = nf90_def_dim(ncids(noutfile), trim(dimname), nlen, dimids(id,itopo,noutfile))
            end if
            if (ierr /= nf90_noerr) then
               write (*,'(a,i0,a,i0)') 'Error: mapmerge: Could not copy dimension #', dimids(id,itopo,ifileScan), &
                                       ' from `'//trim(infiles(ifileScan))//''' into output file `'//trim(outfile)//'''. Error: ', ierr
               if (.not. verbose_mode) goto 888
            else if (dimname == 'Two') then
               id_dimTwo = dimids(id,itopo,noutfile)
            end if
         end if
      end do

      if (jaugrid == 1 .and. topodimTmp == 1 .and. id_network > 0) then ! If network data exist, then clone the definition of dimensions and variables from input file ifile.
         ierr = ug_clone_network_definition(ncids(ifileScan), ncids(noutfile), netids_input, netids_output)
         if (ierr /= nf90_noerr) then
             write (*,'(a)') 'Error: mapmerge: cannot define dimensions/variables of the 1D network data for the merged file.'
             goto 888
         end if
      end if

      !! 4b. Define all variables, based on previously detected dimension information.
      ! var_dimids = -1 ! NOTE: can't do this, as we still need the vectormax dimensions that are stored in here.
      do iv=1,nvarsel(itopo)
         !ierr = nf90_inquire_var(ncids(1), varids(iv))
!         ndims = 2
         !dimids(1) = id_facedim(noutfile)
         !dimids(2) = id_timedim(noutfile)

         ip = var_timdimpos(iv,itopo)
         if (ip /= -1) then
            var_dimids(ip,iv,itopo) = id_timedim(itopo,noutfile)
         end if
         ip = var_spacedimpos(iv,itopo)
         if (ip /= -1) then
            if (var_loctype(iv,itopo) == UNC_LOC_S) then
               var_dimids(ip,iv,itopo) = id_facedim(itopo,noutfile)
            else if (var_loctype(iv,itopo) == UNC_LOC_SN) then
               var_dimids(ip,iv,itopo) = id_netfacedim(itopo,noutfile)
            else if (var_loctype(iv,itopo) == UNC_LOC_U) then
               var_dimids(ip,iv,itopo) = id_edgedim(itopo,noutfile)
            else if (var_loctype(iv,itopo) == UNC_LOC_CN) then
               var_dimids(ip,iv,itopo) = id_nodedim(itopo,noutfile)
            else if (var_loctype(iv,itopo) == UNC_LOC_L) then
               var_dimids(ip,iv,itopo) = id_netedgedim(itopo,noutfile)
            else if (var_loctype(iv,itopo) == UNC_LOC_SBND) then
               var_dimids(ip,iv,itopo) = id_bnddim(itopo,noutfile)
            else
               write (*,'(a,i0,a)') 'Error: mapmerge: Unknown location type ', var_loctype(itopo,iv), ' for `'//trim(var_names(itopo,iv))//'''.'
            end if
         end if
         ip = var_laydimpos(iv,itopo)
         if (ip /= -1) then
            var_dimids(ip,iv,itopo) = id_laydim(itopo,noutfile)
         end if
         ip = var_wdimpos(iv,itopo)
         if (ip /= -1) then
            var_dimids(ip,iv,itopo) = id_wdim(itopo,noutfile)
         end if
         do ikx = 1, numkx(iv, itopo)
            ip = var_kxdimpos(ikx, iv,itopo)
            if (ip /= -1) then
               var_dimids(ip,iv,itopo) = dimids(var_dimids(ip,iv,itopo),itopo,noutfile) ! this is necessary because in outfile dim IDs will be in different order, even if *all* dim ids have been copied
               ! Dim ID for this var in outfile === based on *pos* in dimids(:) for *this* noutfile.
            end if
         end do

         ierr = nf90_def_var(ncids(noutfile), var_names(iv,itopo), var_types(iv,itopo), var_dimids(4-var_ndims(iv,itopo)+1:4,iv,itopo), varids_out(iv,itopo))

         if (ierr /= nf90_noerr) then
            write (*,'(a)') 'Error: mapmerge: could not create output variable `'//trim(var_names(iv,itopo))//'''. '
            write (*,'(a)')        'Details : '//trim(nf90_strerror(ierr))
            if (.not. verbose_mode) goto 888
            varids_out(iv,itopo) = -1
            cycle
         end if

         ierr = ncu_copy_atts(ncids(ifileScan), ncids(noutfile), varids(ifileScan, iv,itopo), varids_out(iv,itopo))
         if (isNetCDF4) then
            new_ndx = min(ndx(itopo,noutfile), 2000)  ! ! must be equal to mapclass_chunksize_ndx
            ierr = ncu_copy_chunking_deflate(ncids(ifileScan), ncids(noutfile), varids(ifileScan, iv,itopo), varids_out(iv,itopo), new_ndx)
         endif

         ! For Variable 'FlowLink', the flowelem might be outside the domain, and there might be no info. about such flowelem
         ! in mapfiles, so they are denoted by _FillValue.
         if (var_names(iv,itopo) .eq. 'FlowLink') then
            ierr = ncu_inq_var_fill(ncids(ifileScan), varids(ifileScan, iv,itopo), nofill, fill_value_int, fill_value_customed = imiss)
            ierr = nf90_put_att(ncids(noutfile), varids_out(iv,itopo), '_FillValue', fill_value_int)
         end if
      end do
   end do ! itopo

   ! When both 1D and 2D meshes are present, the global numbering of 1D nodes (read from input files) includes the 2D faces.
   ! In merged map file, we make the global numbering of 1D nodes only for 1D nodes, not including 2D faces.
   if (minTopodim == 1 .and. maxTopodim == 2 .and. jareadglob == 1) then
      node_c2g(:,1) = node_c2g(:,1) - ndx(2,noutfile)
   end if

   ! The partitions variables are used for restart. Since restart from a ugrid file is not supported yet, do not write these variables if it is ugrid.
   ! TODO: see UNST-4947 which aims to enable restart from ugrid file.
   if (jaugrid == 0) then
      ierr = nf90_put_att(ncids(noutfile), nf90_global, 'NumPartitionsInFile', nfiles)
      ierr = nf90_def_dim(ncids(noutfile), 'nPartitions', nfiles, id_npartdim)
      ierr = nf90_def_var(ncids(noutfile), 'partitions_face_start', nf90_int, (/ id_npartdim /), id_part_face_start)
      ierr = nf90_def_var(ncids(noutfile), 'partitions_edge_start', nf90_int, (/ id_npartdim /), id_part_edge_start)
      ierr = nf90_def_var(ncids(noutfile), 'partitions_node_start', nf90_int, (/ id_npartdim /), id_part_node_start)
      ierr = nf90_put_att(ncids(noutfile), id_part_face_start, 'long_name', 'start index in global data arrays for face data for all partititions')
      ierr = nf90_put_att(ncids(noutfile), id_part_edge_start, 'long_name', 'start index in global data arrays for edge data for all partititions')
      ierr = nf90_put_att(ncids(noutfile), id_part_node_start, 'long_name', 'start index in global data arrays for node data for all partititions')
      ! NOTE: AvD: intentionally not adding netedge here, as I want to phase it out.
      ierr = nf90_def_var(ncids(noutfile), 'partitions_face_count', nf90_int, (/ id_npartdim /), id_part_face_count)
      ierr = nf90_def_var(ncids(noutfile), 'partitions_edge_count', nf90_int, (/ id_npartdim /), id_part_edge_count)
      ierr = nf90_def_var(ncids(noutfile), 'partitions_node_count', nf90_int, (/ id_npartdim /), id_part_node_count)
      ierr = nf90_put_att(ncids(noutfile), id_part_face_count, 'long_name', 'per-partition count in global data arrays for face data')
      ierr = nf90_put_att(ncids(noutfile), id_part_edge_count, 'long_name', 'per-partition count in global data arrays for edge data')
      ierr = nf90_put_att(ncids(noutfile), id_part_node_count, 'long_name', 'per-partition count in global data arrays for node data')
      if (maxval(nbndglob)>0) then
         ierr = nf90_def_var(ncids(noutfile), 'partitions_facebnd_count', nf90_int, (/ id_npartdim /), id_part_facebnd_count)
         ierr = nf90_put_att(ncids(noutfile), id_part_facebnd_count, 'long_name', 'per-partition count in global data arrays for boundary points data')
         ierr = nf90_def_var(ncids(noutfile), 'partitions_facebnd_start', nf90_int, (/ id_npartdim /), id_part_facebnd_start)
         ierr = nf90_put_att(ncids(noutfile), id_part_facebnd_start, 'long_name', 'start index in global data arrays for boundary points for all partititions')
      endif
   end if
      !
   !! 5.Define dimensions and variables for 1D2D contact meshes.
   if (jaugrid == 1) then
      ! Determine max number of mesh contact topology variables (*not* yet the number of contact links), should typically be 0 or 1.
      maxNcontacts = 0
      call realloc(ncontacts, nfiles, keepExisting=.false., fill = 0)
      do ii = 1, nfiles
         ierr = ionc_get_contact_topo_count(ioncids(ii), ncontacts(ii))
         if (ncontacts(ii) > maxNcontacts) then
            maxNcontacts = ncontacts(ii)
         end if
      end do

      if (maxNcontacts > 0) then ! if 1d2d contact exists
         call realloc(nlink1d2dcount,    maxNcontacts,             keepExisting=.false., fill = 0)
         call realloc(numl1d2d_icontact, maxNcontacts,             keepExisting=.false., fill = 0)
         call realloc(numl1d2d,          (/maxNcontacts,nfiles+1/), keepExisting=.false., fill = 0)
         ! Get the number of 1d2d links of each connectivity variable in each input file
         do ii = 1, nfiles
            do icontact = 1, ncontacts(ii)
               ierr = ionc_get_contacts_count_ugrid(ioncids(ii), icontact, nLinks1d2d)
               numl1d2d(icontact,ii) = nLinks1d2d
               numl1d2d_icontact(icontact) = numl1d2d_icontact(icontact) + nLinks1d2d ! Total number of 1d2d links of a certain connectivity variable among all input files
            end do
         end do

         numl1d2dMax = maxval(numl1d2d_icontact)
         numl1d2dt   = sum(numl1d2d_icontact) ! Total number of all 1d2d links among all input files
         call realloc(links1d2dtype,    (/ numl1d2dMax, maxNcontacts /),    keepExisting=.false., fill = -1) ! The link type variable that is to be read from input files
         call realloc(links1d2dnodes,   (/ 2, numl1d2dMax, maxNcontacts /), keepExisting=.false., fill = -1) ! The connectivity variable that is to be read from input files
         call realloc(links1d2dnodes_g, (/ 2, numl1d2dt, maxNcontacts/),    keepExisting=.false., fill = -1) ! The connectivity variable that is to be written to merge file, the node/face numbering in this variable is global numbering
         call realloc(link1d2d_domain,  (/ numl1d2dMax, maxNcontacts/),     keepExisting=.false., fill = -1) ! Domain number of 1d2d links
         call realloc(nlink1d2dglob,    maxNcontacts,                       keepExisting=.false., fill = 0 ) ! total number of 1d2d links without duplicates, on each mesh
         call realloc(link1d2d_c2g,     (/ numl1d2dMax, maxNcontacts/),     keepExisting=.false., fill = 0 ) ! Mapping of 1d2d link numbering from contiguous to global numbering

         nnodecount(1) = 0
         nfacecount(2) = 0

         ! For a certain connectivity variable, read it from all input files
         ! NOTE: the following loop assumes that all input files have the same connectivity variables, 
         ! e.g.the same connectivity variable 'contact1' is at im==1 in all input files.
         ! It does not support if file(1) has connectivity variable "contact1" at im==1 and file(2) has
         ! 'contact2' at im==1. This is a TODO task.
         do icontact = 1, maxNcontacts
            do ii = 1, nfiles
               if (icontact > ncontacts(ii)) then
                  cycle
               end if
               ierr = ionc_get_mesh_contact_links(ioncids(ii), icontact, links1d2dnodes(:,nlink1d2dcount(icontact)+1:nlink1d2dcount(icontact)+numl1d2d(icontact,ii), icontact), links1d2dtype(:, icontact), 1)
               ierr = ionc_get_contact_name(ioncids(ii), icontact, contactname)
               if (ierr /= nf90_noerr) then
                  write (*,'(a)') 'Error: mapmerge: could not retrieve `'//trim(contactname)//''' from `'//infiles(ii)//'''.'
                  if (.not. verbose_mode) goto 888
               end if

               ! Determine domain number of 1d2d links, which equals to the lowest domain number of their two connected node/face.
               do ip = 1, numl1d2d(icontact,ii)
                  k1 = links1d2dnodes(1,nlink1d2dcount(icontact)+ip, icontact)
                  k2 = links1d2dnodes(2,nlink1d2dcount(icontact)+ip, icontact)
                  ! TODO: UNST-4999: generalize the code below. In UGRID ':contact' attribute, the 1D and 2D side are specified, does not always have to be 1d first.
                  ! k1 is a 1d node, k2 is a 2d cell (This can be seen from subroutine unc_write_flowgeom_filepointer_ugrid)
                  idom = min(node_domain(nnodecount(1)+k1,1), face_domain(nfacecount(2)+k2,2))
                  link1d2d_domain(nlink1d2dcount(icontact)+ip, icontact) = idom

                  if (idom == ii-1) then
                     nlink1d2dglob(icontact) = nlink1d2dglob(icontact)+1
                     link1d2d_c2g(nlink1d2dcount(icontact) + ip, icontact) = nlink1d2dglob(icontact)

                     links1d2dnodes_g(1,nlink1d2dglob(icontact), icontact) = node_c2g(nnodecount(1)+k1,1)
                     links1d2dnodes_g(2,nlink1d2dglob(icontact), icontact) = face_c2g(nfacecount(2)+k2,2)
                  end if
               end do

               nlink1d2dcount(icontact) = nlink1d2dcount(icontact) + numl1d2d(icontact,ii)
               nfacecount(2)      = nfacecount(2)      + ndx(2,ii)
               nnodecount(1)      = nnodecount(1)      + numk(1,ii)
            end do ! ii
            numl1d2d(icontact,noutfile) = nlink1d2dglob(icontact)
         end do ! icontact

         call realloc(icfile,                    maxNcontacts,            keepExisting=.false., fill = 0)
         call realloc(nvars_contact,             (/maxNcontacts, nfiles/), keepExisting=.false., fill = 0)
         call realloc(nMaxvars_contact_icontact, maxNcontacts,            keepExisting=.false., fill = 0)
         do icontact = 1, maxNcontacts
            do ii = 1, nfiles
               ierr = ionc_get_var_total_count(ioncids(ii), icontact, 1, 0, nvars_contact(icontact,ii)) ! Get total number of variables on the current mesh contact in file ii

               if (nvars_contact(icontact,ii)> 0) then
                  if (nvars_contact(icontact,ii) > nMaxvars_contact_icontact(icontact)) then
                     nMaxvars_contact_icontact(icontact) = nvars_contact(icontact,ii) ! the maximal number of variables of mesh contact icontact
                     icfile(icontact) = ii ! This is the reference file that contains the most variables on mesh contact icontact. It will be used for later scanning of variables.
                  end if
               end if
            end do
         end do

         nMaxvars_contact = maxval(nMaxvars_contact_icontact)
         call realloc(id_contactvars, (/maxNcontacts, nMaxvars_contact/), keepExisting=.false., fill =-1)
         call realloc(varids_contact, (/nMaxvars_contact,maxNcontacts,nfiles/), keepExisting=.false., fill =-1) ! Ids of contact variable in each input file
         do icontact = 1, maxNcontacts
            ifileScan = icfile(icontact)
            nvarsScan = nvars_contact(icontact, ifileScan)
            ierr = ionc_get_var_total_count(ioncids(ifileScan), icontact, 1, 1, nvarsScan, varids_contact(1:nvarsScan,icontact,ifileScan))
            ! Set IDs of these variables to other input files, if those files does not have this variable, then keep ID as -1
            do iv = 1, nvarsScan
               ierr = nf90_inquire_variable(ncids(ifileScan), varids_contact(iv,icontact,ifileScan), varname)
               do ii = 1, nfiles
                  if (ii /= ifileScan) then
                     ierr = nf90_inq_varid(ncids(ii), varname, varids_contact(iv,icontact,ii))
                     if (ierr /= nf90_noerr) then
                        if (verbose_mode) then
                           write (*,'(a)') 'Info: mapmerge: file `'//trim(infiles(ii))//'''does not have variable `'//trim(varname)//'''.'
                        endif
                        ierr = 0
                     end if
                  end if
               end do
            end do

            ! define dimensions and variables for mesh contact icontact in the merged file
            ierr = ionc_get_contactids(ioncids(ifileScan), icontact, cids_input)
            if (ierr /= nf90_noerr) then
                write (*,'(a)') 'Error: mapmerge: cannot read dimension/variable ids of the contact data.'
                goto 888
            end if
            ierr = ug_clone_contact_definition(ncids(ifileScan), ncids(noutfile), numl1d2d(icontact,noutfile), nvarsScan, varids_contact(1:nvarsScan,icontact,ifileScan), cids_input, cids_output, id_contactvars(icontact,1:nvarsScan))
         end do
      end if
   else
      continue ! non-UGRID 1D2D links not supported.
   end if

   ierr = nf90_enddef(ncids(noutfile))

   !! 6. Write new flow geom to output file
   !! 6a. Write all timedep fields from all files into output file
   ifileScan = ifile(minTopodim)
   if (nt(ifileScan) == 0) then! Time-independent data file
      ntsel = 1 ! This enables writing a single snapshot of time-independent data variables in the merging code later on.
   else
      ! For now: do all times.
      ntsel = nt(ifileScan)
      nt(noutfile) = ntsel
      allocate(itimsel(ntsel))
      itimsel = (/ (it, it=1,ntsel) /)
      allocate(times(ntsel), timestep(ntsel))
      do it=1,ntsel
         ierr = nf90_get_var(ncids(ifileScan), id_time(ifileScan), times(it), start = (/ itimsel(it) /)) ! count=1
         ierr = nf90_get_var(ncids(ifileScan), id_timestep(ifileScan), timestep(it), start = (/ itimsel(it) /)) ! count=1
      end do

      ierr = nf90_put_var(ncids(noutfile), id_time(noutfile), times, count = (/ ntsel /))
      ierr = nf90_put_var(ncids(noutfile), id_timestep(noutfile), timestep, count = (/ ntsel /))
   end if

   ! 6b. A loop on meshe topology to write the merged variables to the output file.
   do itopo = minTopodim, maxTopodim
      if (verbose_mode .and. jaugrid == 1) then
         write (*,'(a)') '## Writing merged variables of `'//trim(meshname(itopo,ifile(itopo)))//''' to output file...'
      end if
      if (jaugrid == 1) then
         imesh = topo2mesh(itopo,ifile(itopo))
         topodimTmp = topodim(imesh,ifile(itopo))
         if (topodimTmp == 1 .and. id_network > 0) then ! If network data exist, then clone them from input file ifile(itopo) to the merged file.
            ierr = ug_clone_network_data(ncids(ifile(itopo)), ncids(noutfile), netids_input, netids_output)
            if (ierr /= nf90_noerr) then
               write (*,'(a)') 'Error: mapmerge: cannot clone variable of the 1D network data from one input file to the merged file.'
               goto 888
            end if
         end if
      else
         topodimTmp = 2 ! For old format, mesh topology is 2d
      end if

      ! 1D tmp array: take largest of all topological position counts:
      maxitems = max(sum(nedgecount(1:maxTopodim)), sum(nfacecount(1:maxTopodim)), sum(nnodecount(1:maxTopodim)), sum(nnetedgecount(1:maxTopodim)), sum(nbndcount(1:maxTopodim)), kmx(itopo,noutfile)+1)
      call realloc( tmpvar1D, maxitems, keepExisting=.false.)
      call realloc(itmpvar1D, maxitems, keepExisting=.false.)
      call realloc(itmpvar1D_tmp,maxitems, keepExisting=.false.)
      size_btmp = max(ndx(itopo,size(ndx(itopo,:))), sum(ndx(itopo,1:size(ndx(itopo,:))-1)))
      call realloc(btmpvar1D, [size_btmp, mapclass_time_buffer_size], keepExisting=.false.)
      call realloc(btmpvar1D_tmp, [ndx(itopo,size(ndx(itopo,:))), mapclass_time_buffer_size], keepExisting=.false.)
      call realloc(tmpvar1D_tmp, maxitems, keepExisting=.false.)
      ! 2D/3D done in loop below
!
      do iv=1,nvarsel(itopo)
         if (verbose_mode) then
            write (tmpstr1, '(a,i0,a,i0,a)') 'Var #', iv, ' of ', nvarsel(itopo), ': '
         end if

         ! Skip merging the connectivity variables when coordinates of netnodes or netedges are not read from the files
         if (jamerge_cntv == 0 .and. (var_names(iv,itopo) .eq. 'NetLink' .or. var_names(iv,itopo) .eq. 'NetElemNode' .or. &
             var_names(iv,itopo) .eq. 'NetElemLink' .or. var_names(iv,itopo) .eq. 'ElemLink' .or. &
             var_names(iv,itopo) .eq. trim(meshname(itopo,ifile(itopo)))//'_edge_nodes' .or. var_names(iv,itopo) .eq. trim(meshname(itopo,ifile(itopo)))//'_face_nodes' .or. var_names(iv,itopo) .eq. trim(meshname(itopo,ifile(itopo)))//'_edge_faces')) then
             write (*,'(a)') 'Warning: mapmerge: Skipping merging topology variable: `'//trim(var_names(iv,itopo))//''', because coordinates of netnodes or netedges can not be read from the file.'
             cycle
         end if

         if (var_ndims(iv,itopo) == 0) then  ! For instance, 'Mesh2D'
            cycle
         else if (var_spacedimpos(iv,itopo) == -1 .and. var_timdimpos(iv,itopo) == -1 .and. any(var_kxdimpos(:,iv,itopo) /= -1)) then
            ! Some unknown non-space and non-time dimension: impossible to merge in a generic way. Skip it.
            write (*,'(a)') 'Warning: mapmerge: cannot merge vars with non-space/time dimensions: `'//trim(var_names(iv,itopo))//'''. Skipping.'
            cycle
            ! TODO: AvD: read + write timestep variable and similar here.
         end if

         is = MAX_VAR_DIMS-var_ndims(iv,itopo)+1
         ie = MAX_VAR_DIMS
         start_idx  (is:ie) = 1 ! For all relevant dimensions for this var: start indices to be set below
         count_read (is:ie) = 1 ! For all relevant dimensions for this var: read  counts to be set below
         count_write(is:ie) = 1 ! For all relevant dimensions for this var: write counts to be set below

         ! 6b.1 Optional kx/vectormax is the same for all files, for all times:
         do ikx = 1, numkx(iv, itopo)
            if (var_kxdimpos(ikx, iv,itopo) /= -1) then
               id = var_dimids(var_kxdimpos(ikx,iv,itopo), iv,itopo) ! Dim ID for this kx dim in outfile
               ierr = nf90_inquire_dimension(ncids(noutfile), id, name = dimname, len = nlen)
               if (ierr /= nf90_noerr) then
                  write (*,'(a,i0,a,a,a)') 'Error: mapmerge: Could not inquire vectormax dimension #', id, ' for variable `', trim(var_names(iv,itopo)), '''.'
                  cycle ! iv
               end if
               count_read (var_kxdimpos(ikx, iv,itopo)) = nlen
               count_write(var_kxdimpos(ikx, iv,itopo)) = nlen
            end if
         end do

         ! 6b.2 Optional kmx/layer dim is assumed to be the same for all files, for all times:
         if (var_laydimpos(iv,itopo) /= -1) then
            count_read (var_laydimpos(iv,itopo)) = kmx(itopo,noutfile)
            count_write(var_laydimpos(iv,itopo)) = kmx(itopo,noutfile)
            ! TODO: AvD: UNST-993: support w-position quantities: difference between kmx and kmx1 !!
            ! TODO: AvD: future work: what if kmx varies between input files?
         end if
         if (var_wdimpos(iv,itopo) /= -1) then
            count_read (var_wdimpos(iv,itopo)) = kmx(itopo,noutfile) +1
            count_write(var_wdimpos(iv,itopo)) = kmx(itopo,noutfile) +1
         end if

         ! 6b.3 Prepare for space dimension, by pointers to proper face/edge/node/netedge variables
         if ( var_spacedimpos(iv,itopo) /= -1) then
            select case (var_loctype(iv,itopo))
            case (UNC_LOC_S)
               item_counts => ndx(itopo,:)
               if (topodimTmp /= 1) then
                  item_domain => face_domain(:,itopo)
               else
                  item_domain => node_domain(:,itopo)
               end if
            case (UNC_LOC_SN)
               item_counts => ndx(itopo,:)
               if (topodimTmp /= 1) then
                  item_domain => face_domain(:,itopo)
               else
                  item_domain => node_domain(:,itopo)
               end if
            case (UNC_LOC_U)
               item_counts => lnx(itopo,:)
               item_domain => edge_domain(:,itopo)
            case (UNC_LOC_CN)
               item_counts => numk(itopo,:)
               item_domain => node_domain(:,itopo)
            case (UNC_LOC_L)
               item_counts => numl(itopo,:)
               item_domain => netedge_domain(:,itopo)
            case (UNC_LOC_SBND)
               item_counts => ndxbnd(itopo,:)
               item_domain => facebnd_domain(:,itopo)
            case default
               ! TODO: Handle non-space dependent vars
               if (var_ndims(iv,itopo) > 0) then
                  write (*,'(a)') 'Warning: mapmerge: cannot write space-independent vars: `'//trim(var_names(iv,itopo))//'''. Skipping.'
               else
                  if (verbose_mode) then
                     call progress(tmpstr1, 100) ! generate the progress bar.
                  end if
               end if

               cycle
            end select
            count_write(var_spacedimpos(iv,itopo)) = item_counts(noutfile)
         endif

         ! NOTE: AvD: below we assume that order is kx, kmx, ndx, nt, so not as generic anymore as the var_*dimpos analysis would allow.
         ! Allocate the proper memory space for nf90_get_var without risk of stack overflows in the netcdf lib
         if (var_types(iv,itopo) /= nf90_double .and. var_types(iv,itopo) /= nf90_int .and. var_types(iv,itopo) /= nf90_short .and. var_types(iv,itopo) /= nf90_byte .and. var_types(iv,itopo) /= nf90_char) then
            write (*,'(a,i0,a)') 'Error: mapmerge: encountered unsupported data type ', var_types(iv,itopo), ' for variable `'//trim(var_names(iv,itopo))//'''.'
            if (.not. verbose_mode) goto 888
         end if

         ! Read attribute _fillValue of this variable from the file
         if (var_types(iv,itopo) == nf90_double) then
            ierr = ncu_inq_var_fill(ncids(ifile(itopo)), varids(ifile(itopo),iv,itopo), nofill, fill_value_double, fill_value_customed = dmiss)
         else if (var_types(iv,itopo) == nf90_int .or. var_types(iv,itopo) == nf90_short) then
            ierr = ncu_inq_var_fill(ncids(ifile(itopo)), varids(ifile(itopo),iv,itopo), nofill, fill_value_int,    fill_value_customed = imiss)
         end if

         if (all(var_kxdimpos(:,iv,itopo) == -1) .and. var_laydimpos(iv,itopo) == -1 .and. var_wdimpos(iv,itopo) == -1 & ! 1D array with no layers and no vectormax (possibly time-dep)
              .or. (var_ndims(iv,itopo) == 1 .and. (var_laydimpos(iv,itopo) > 0 .or. var_wdimpos(iv,itopo) > 0))) then ! 1D array of vertical coordinates
            ! Already allocated at max(lnx, ndx, numk, numl), no risk of stack overflow
            if (var_types(iv,itopo) == nf90_double) then
               tmpvar1D = fill_value_double
               tmpvarptr(1:1,1:1,1:maxitems)  =>  tmpvar1D(:)
            else if (var_types(iv,itopo) == nf90_int .or. var_types(iv,itopo) == nf90_short) then
               itmpvar1D = fill_value_int
               itmpvarptr(1:1,1:1,1:maxitems) => itmpvar1D(:)
            else if (var_types(iv,itopo) == nf90_byte) then
               btmpvarptr(1:1,1:1,1:maxitems,1:mapclass_time_buffer_size) => btmpvar1D(:,1:mapclass_time_buffer_size)
            end if
            tmpvarDim = 1
         else if (any(var_kxdimpos(:,iv,itopo) /= -1)) then
            if (var_laydimpos(iv,itopo) /= -1 ) then   ! Both a vectormax AND a laydim
               call realloc(tmpvar3D, (/  count_read(var_kxdimpos(ikx0,iv,itopo)), count_read(var_laydimpos(iv,itopo)), maxitems /), keepExisting=.false.)
               ! use maxitems instead of items_count(noutfile) to try and have as few reallocs as possible.
               tmpvarptr => tmpvar3D
               tmpvarDim = 3
            else if (numkx(iv, itopo) > 1) then ! for 4 dimensional sediment variables which has more than 1 sediment dimension, e.g. msed
               call realloc(tmpvar3D, (/  count_read(var_kxdimpos(ikx0, iv,itopo)), count_read(var_kxdimpos(2,iv,itopo)), maxitems /), keepExisting=.false.)
               ! use maxitems instead of items_count(noutfile) to try and have as few reallocs as possible.
               tmpvarptr => tmpvar3D
               tmpvarDim = 3
            else                                ! Only a vectormax dim
               if (var_types(iv,itopo) == nf90_double) then
                  call realloc( tmpvar2D, (/  count_read(var_kxdimpos(ikx0,iv,itopo)), maxitems /), keepExisting=.false., fill=fill_value_double)
                  tmpvarptr(1:count_read(var_kxdimpos(ikx0,iv,itopo)),1:1,1:maxitems)  =>  tmpvar2D(:,:)
                  call realloc( tmpvar2D_tmp, (/  count_read(var_kxdimpos(ikx0,iv,itopo)), maxitems /), keepExisting=.false., fill=fill_value_double)
               else if (var_types(iv,itopo) == nf90_int .or. var_types(iv,itopo) == nf90_short) then
                  call realloc(itmpvar2D, (/  count_read(var_kxdimpos(ikx0,iv,itopo)), maxitems /), keepExisting=.false., fill=fill_value_int)
                  itmpvarptr(1:count_read(var_kxdimpos(ikx0,iv,itopo)),1:1,1:maxitems) => itmpvar2D(:,:)
                  call realloc(itmpvar2D_tmp, (/  count_read(var_kxdimpos(ikx0,iv,itopo)), maxitems /), keepExisting=.false., fill=fill_value_int)
               else if (var_types(iv,itopo) == nf90_char) then ! for variables such as mesh1d_node_id
                  call realloc(ctmpvar2D, (/ count_read(var_kxdimpos(ikx0,iv,itopo)), maxitems /), keepExisting=.false., fill='')
                  call realloc(ctmpvar2D_tmp, (/ count_read(var_kxdimpos(ikx0,iv,itopo)), maxitems /), keepExisting=.false., fill='')
               end if
               tmpvarDim = 2
            end if
         else if (var_laydimpos(iv,itopo) /= -1) then ! Only a laydim
            if (var_types(iv,itopo) == nf90_double) then
               call    realloc(tmpvar2D, (/  kmx(itopo,noutfile), maxitems /), keepExisting=.false., fill=fill_value_double)
               tmpvarptr(1:1,1:kmx(itopo,noutfile),1:maxitems)  =>  tmpvar2D(:,:)
               call    realloc(tmpvar2D_tmp, (/  kmx(itopo,noutfile), maxitems /), keepExisting=.false., fill=fill_value_double)
            else if (var_types(iv,itopo) == nf90_int .or. var_types(iv,itopo) == nf90_short) then
               call    realloc(itmpvar2D, (/  kmx(itopo,noutfile), maxitems /), keepExisting=.false., fill=fill_value_int)
               itmpvarptr(1:1,1:kmx(itopo,noutfile),1:maxitems) => itmpvar2D(:,:)
               call    realloc(itmpvar2D_tmp, (/  kmx(itopo,noutfile), maxitems /), keepExisting=.false., fill=fill_value_int)
            end if
            tmpvarDim = 2
         else if (var_wdimpos(iv,itopo) /= -1) then
            if (var_types(iv,itopo) == nf90_double) then
               call    realloc(tmpvar2D, (/  kmx(itopo,noutfile)+1, maxitems /), keepExisting=.false., fill=fill_value_double)
               tmpvarptr(1:1,1:kmx(itopo,noutfile)+1,1:maxitems)  =>  tmpvar2D(:,:)
               call    realloc(tmpvar2D_tmp, (/  kmx(itopo,noutfile)+1, maxitems /), keepExisting=.false., fill=fill_value_double)
            else if (var_types(iv,itopo) == nf90_int .or. var_types(iv,itopo) == nf90_short) then
               call    realloc(itmpvar2D, (/  kmx(itopo,noutfile)+1, maxitems /), keepExisting=.false., fill=fill_value_int)
               itmpvarptr(1:1,1:kmx(itopo,noutfile)+1,1:maxitems) => itmpvar2D(:,:)
               call    realloc(itmpvar2D_tmp, (/  kmx(itopo,noutfile)+1, maxitems /), keepExisting=.false., fill=fill_value_int)
            end if
            tmpvarDim = 2
         end if

         !! 1D array of vertical coordinates are COPIED from file "ifile(itopo)" to the merged file
         if (var_ndims(iv,itopo) == 1 .and. (var_laydimpos(iv,itopo) > 0 .or. var_wdimpos(iv,itopo) > 0)) then
            nlen = count_read(ie)
            if (var_types(iv,itopo) == nf90_double) then
               ierr = nf90_get_var(ncids(ifile(itopo)), varids(ifile(itopo),iv,itopo), tmpvar1D(1:nlen), count=count_read(is:ie))
               if (ierr /= nf90_noerr) then
                  write (*,'(a,i0,a)') 'Error: mapmerge: cannot read variable ', var_names(iv,itopo), ' from file `'//trim(infiles(ifile(itopo)))//'''.'
                  if (.not. verbose_mode) goto 888
               end if
         
               ierr = nf90_put_var(ncids(noutfile), varids_out(iv,itopo), tmpvar1D, count = count_write(is:ie), start = start_idx(is:ie))
               if (ierr /= nf90_noerr) then
                  write (*,'(a,i0,a)') 'Error: mapmerge: cannot write variable ', var_names(iv,itopo), ' to the merged file.'
                  if (.not. verbose_mode) goto 888
               end if
            else if (var_types(iv,itopo) == nf90_int .or. var_types(iv,itopo) == nf90_short) then
               ierr = nf90_get_var(ncids(ifile(itopo)), varids(ifile(itopo),iv,itopo), itmpvar1D(1:nlen), count=count_read(is:ie))
               if (ierr /= nf90_noerr) then
                  write (*,'(a,i0,a)') 'Error: mapmerge: cannot read variable ', var_names(iv,itopo), ' from file `'//trim(infiles(ifile(itopo)))//'''.'
                  if (.not. verbose_mode) goto 888
               end if
         
               ierr = nf90_put_var(ncids(noutfile), varids_out(iv,itopo), itmpvar1D, count = count_write(is:ie), start = start_idx(is:ie))
               if (ierr /= nf90_noerr) then
                  write (*,'(a,i0,a)') 'Error: mapmerge: cannot write variable ', var_names(iv,itopo), ' to the merged file.'
                  if (.not. verbose_mode) goto 888
               end if
            end if
         else ! the following is merging
            do it=1,ntsel
               itm = mod(it-1, mapclass_time_buffer_size)+1
               if (verbose_mode) then
                  call progress(tmpstr1, floor((it-1)*100.0/ntsel)) ! generate the progress bar.
               end if

               ! 6b.4 Time dimension: Which timestep to read from input files
               if (var_timdimpos(iv,itopo) /= -1) then
                  start_idx(var_timdimpos(iv,itopo)) = itimsel(it)
               end if

               nitemglob  = 0
               nitemcount = 0
               do ii=1,nfiles
                  nitemglob0 = nitemglob

                  if (var_spacedimpos(iv,itopo) /= -1) then
                     if (item_counts(ii) == 0) then
                        cycle
                     else
                        count_read(var_spacedimpos(iv,itopo)) = item_counts(ii) ! How many flow face/edges/nodes to read from file #ii
                     endif
                  end if

                  ! Do the actual reading

                  ! When one file has only triangular mesh and one file has only rectangular mesh, then a variable, e.g. 'NetElemNode'
                  ! in the target merged map has vectormax dimension nlen=4. To read such a variable from each file, the vectormax dimension
                  ! should be consistent in the current file. If this dimension is smaller than the maximal nlen, then a seperate array
                  ! "itmpvar2D_tmpmax" will be defined by the current vectormax dimension. We first read values into this new array and then
                  ! put them into array "itmpvar2D" (UNST-1842).
                  if (var_kxdimpos(ikx0,iv,itopo) /= -1 .and. (dimname == 'nNetElemMaxNode' .or. dimname == 'max_n'//trim(meshname(itopo,ifile(itopo)))//'_face_nodes' .or. dimname == trim(meshname(itopo,ifile(itopo)))//'_nMax_face_nodes' .or. dimname=='nFlowElemContourPts')) then
                     count_read(is) = netfacemaxnodes(itopo,ii)
                     if (netfacemaxnodes(itopo,ii) < nlen) then
                        jaread_sep = 1
                     end if
                  end if

                  if (all(var_kxdimpos(:,iv,itopo) == -1) .and. var_laydimpos(iv,itopo) == -1 .and. var_wdimpos(iv,itopo) == -1) then ! 1D array with no layers and no vectormax (possibly time-dep)
                     if (var_types(iv,itopo) == nf90_double) then
                        ierr = nf90_get_var(ncids(ii), varids(ii,iv,itopo), tmpvar1D(    nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                     else if (var_types(iv,itopo) == nf90_int .or. var_types(iv,itopo) == nf90_short) then
                        ierr = nf90_get_var(ncids(ii), varids(ii,iv,itopo), itmpvar1D(    nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                     else if (var_types(iv,itopo) == nf90_byte) then
                        ierr = nf90_get_var(ncids(ii), varids(ii,iv,itopo), btmpvar1D(    nitemglob0+1:,itm:itm), count=count_read(is:ie), start=start_idx(is:ie))
                     end if
                  else if (var_kxdimpos(ikx0,iv,itopo) /= -1 .neqv. var_laydimpos(iv,itopo) /= -1) then ! Either a vectormax OR a laydim
                     if (var_types(iv,itopo) == nf90_double) then
                        if (jaread_sep == 1) then
                           call realloc(tmpvar2D_tmpmax, (/  count_read(is), count_read(ie) /), keepExisting=.false., fill=fill_value_double)
                           ierr = nf90_get_var(ncids(ii), varids(ii,iv,itopo), tmpvar2D_tmpmax, count=count_read(is:ie), start=start_idx(is:ie))
                           tmpvar2D(:,nitemglob0+1:nitemglob0+count_read(ie)) = fill_value_double
                           tmpvar2D(1:netfacemaxnodes(itopo,ii),nitemglob0+1:nitemglob0+count_read(ie)) = tmpvar2D_tmpmax(1:count_read(is),1:count_read(ie))
                           jaread_sep = 0
                        else
                           if (numkx(iv, itopo) == 2) then
                              ! read sediment variable which has two sediment dimensions, e.g., msed
                              ierr = nf90_get_var(ncids(ii), varids(ii,iv,itopo),  tmpvar3D(:,:,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                           else if (var_seddimpos(iv,itopo) /= -1) then
                              ! Reading a sediment variable needs to specify the "map" argument in nf90_get_var, because its dimensions are in a different order than other vectormax variables
                              ierr = nf90_get_var(ncids(ii), varids(ii,iv,itopo),  tmpvar2D(  :,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie), map = (/ count_read (var_kxdimpos(ikx0,iv,itopo)), 1, count_read (var_kxdimpos(ikx0,iv,itopo))*item_counts(ii) /))
                           else
                              ierr = nf90_get_var(ncids(ii), varids(ii,iv,itopo),  tmpvar2D(  :,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                           end if
                        end if
                     else if (var_types(iv,itopo) == nf90_int .or. var_types(iv,itopo) == nf90_short) then
                        if (jaread_sep == 1) then
                           call realloc(itmpvar2D_tmpmax, (/ count_read(is), count_read(ie) /), keepExisting=.false., fill=fill_value_int)
                           ierr = nf90_get_var(ncids(ii), varids(ii,iv,itopo), itmpvar2D_tmpmax, count=count_read(is:ie), start=start_idx(is:ie))
                           itmpvar2D(:,nitemglob0+1:nitemglob0+count_read(ie)) = fill_value_int
                           itmpvar2D(1:netfacemaxnodes(itopo,ii),nitemglob0+1:nitemglob0+count_read(ie)) = itmpvar2D_tmpmax(1:count_read(is),1:count_read(ie))
                           jaread_sep = 0
                        else
                           ierr = nf90_get_var(ncids(ii), varids(ii,iv,itopo), itmpvar2D(  :,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                        end if
                     else if (var_types(iv,itopo) == nf90_char) then
                           ierr = nf90_get_var(ncids(ii), varids(ii,iv,itopo), ctmpvar2D(:,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                     end if
                  else if (var_kxdimpos(ikx0,iv,itopo) /= -1 .neqv. (var_laydimpos(iv,itopo) /= -1 .or. var_wdimpos(iv,itopo) /= -1)) then
                  ! Either a vectormax OR a wdim/laydim
                     if (var_types(iv,itopo) == nf90_double) then
                        ierr = nf90_get_var(ncids(ii), varids(ii,iv,itopo),  tmpvar2D(  :,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                     else if (var_types(iv,itopo) == nf90_int .or. var_types(iv,itopo) == nf90_short) then
                        ierr = nf90_get_var(ncids(ii), varids(ii,iv,itopo), itmpvar2D(  :,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                     end if
                  else ! Both a vectormax AND a laydim
                     if (var_types(iv,itopo) == nf90_double) then
                        ierr = nf90_get_var(ncids(ii), varids(ii,iv,itopo), tmpvar3D(:,:,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                     end if
                  end if
                  if (ierr /= nf90_noerr) then
                     write (*,'(a,i0,a)') 'Error: mapmerge: could not read `'//trim(var_names(iv,itopo))//''' from file `'//trim(infiles(ii))//''' (it=', itimsel(it), ').'
                     if (.not. verbose_mode) goto 888
                  end if

                  ! Now shift all items (in space) that really belong to *current* domain ii to the left,
                  ! such that global item (edge/node) nrs form one increasing range in tmpvar.
                  ! Faces related variables in the merged file are numbered by 'FlowElemGlobalNr'
                  ! Conectivity arrays are taken care seperately.
                  if (var_names(iv,itopo) .eq. 'NetLink' .or. var_names(iv,itopo) .eq. trim(meshname(itopo,ifile(itopo)))//'_edge_nodes') then
                     nnetedgecount(itopo) = sum(numl(itopo,1:ii-1))
                     nnodecount(itopo) = sum(numk(itopo,1:ii-1))
                     do ip=1,item_counts(ii)
                         if (item_domain(nnetedgecount(itopo)+ip) == ii-1) then ! only for the NetLink which belongs to the current domain
                             inetedgeglob = netedge_c2g(nnetedgecount(itopo)+ip,itopo)
                             if (inetedgeglob > 0) then
                                 nitemglob = nitemglob + 1               ! for later checking nitemglob
                                 itmpvar2D(:,nitemglob) = itmpvar2D(:,nitemglob0+ip)
                                 do ikk=1,2
                                     g1 = itmpvar2D(ikk, nitemglob)
                                     if (g1 > 0) then
                                         g1 = g1 +nnodecount(itopo)
                                         itmpvar2D(ikk,nitemglob) = node_c2g(g1,itopo) ! mapping the nodes to global nodes
                                     end if
                                 end do
                             end if
                         end if
                     end do
                  else if (var_names(iv,itopo) .eq. 'NetElemNode' .or. var_names(iv,itopo) .eq. trim(meshname(itopo,ifile(itopo)))//'_face_nodes') then
                     nfacecount(itopo) = sum(nump(itopo,1:ii-1))
                     nnodecount(itopo) = sum(numk(itopo,1:ii-1))
                     do ip=1, item_counts(ii)
                         if (item_domain(nfacecount(itopo)+ip) == ii-1) then
                             ifaceglob = face_c2g(nfacecount(itopo)+ip,itopo)
                             if (ifaceglob > 0) then
                                 nitemglob = nitemglob + 1
                                 itmpvar2D_tmp(:,nitemglob) = itmpvar2D(:,nitemglob0+ip)
                                 do ikk = 1, netfacemaxnodes(itopo,ii)
                                     g1 = itmpvar2D_tmp(ikk,nitemglob)
                                     if (g1 > 0) then
                                         g1 = g1 + nnodecount(itopo)
                                         itmpvar2D_tmp(ikk,nitemglob) = node_c2g(g1,itopo)
                                     end if
                                 end do
                             end if
                         end if
                     end do
                     if (ii==nfiles) then ! When finished, copy to itmpvar2D for writting
                        itmpvar2D(:,1:nitemglob) = itmpvar2D_tmp(:,1:nitemglob)
                     end if
                  else if (var_names(iv,itopo) .eq. 'NetElemLink') then
                     nfacecount(itopo) = sum(nump(itopo,1:ii-1))
                     nnetedgecount(itopo) = sum(numl(itopo,1:ii-1))
                     do ip =1, item_counts(ii)
                         if (item_domain(nfacecount(itopo)+ip) == ii-1) then
                             ifaceglob = face_c2g(nfacecount(itopo)+ip,itopo)
                             if (ifaceglob > 0) then
                                 nitemglob = nitemglob + 1
                                 itmpvar2D_tmp(:,nitemglob) = itmpvar2D(:,nitemglob0+ip)
                                 do ikk = 1, netfacemaxnodes(itopo,ii)
                                     g1 = itmpvar2D_tmp(ikk,nitemglob)
                                     if (g1 > 0) then
                                         g1 = g1 + nnetedgecount(itopo)
                                         itmpvar2D_tmp(ikk,nitemglob) = netedge_c2g(g1,itopo)
                                     end if
                                 end do
                             end if
                         end if
                     end do
                     if (ii==nfiles) then
                        itmpvar2D(:,1:nitemglob) = itmpvar2D_tmp(:,1:nitemglob)
                     end if
                  else if (var_names(iv,itopo) .eq. 'ElemLink' .or. var_names(iv,itopo) .eq. trim(meshname(itopo,ifile(itopo)))//'_edge_faces') then
                     nnetedgecount(itopo) = sum(numl(itopo,1:ii-1))
                     nfacecount(itopo) = sum(nump(itopo,1:ii-1))
                     do ip=1,item_counts(ii)
                         if (item_domain(nnetedgecount(itopo)+ip) == ii-1) then
                             inetedgeglob = netedge_c2g(nnetedgecount(itopo)+ip,itopo)
                             if (inetedgeglob > 0) then
                                 nitemglob = nitemglob + 1
                                 itmpvar2D(:,nitemglob) = itmpvar2D(:,nitemglob0+ip)
                                 do ikk=1,2
                                     g1 = itmpvar2D(ikk, nitemglob)
                                     if (g1 > 0) then
                                         g1 = g1 + nfacecount(itopo)
                                         if (face_c2cc(g1) < 0) then 
                                            ! This means that the face is not in the current domain when constructing face_c2cc array.
                                            ! Need to find the valid number for this face.
                                            ifaceglob = face_c2g(g1,itopo)
                                            ifacec    = face_g2c(ifaceglob)
                                            itmpvar2D(ikk,nitemglob) = face_c2cc(ifacec)
                                         else
                                            itmpvar2D(ikk,nitemglob) = face_c2cc(g1)
                                         end if
                                     else if (g1 == 0) then
                                         itmpvar2D(ikk,nitemglob) = 0
                                     end if
                                 end do
                             end if
                         end if
                     end do
                  else if (var_names(iv,itopo) .eq. 'FlowLink') then
                     nedgecount(itopo) = sum(lnx(itopo,1:ii-1))
                     nfacecount(itopo) = sum(nump(itopo,1:ii-1))
                     do ip=1,item_counts(ii)
                         if (item_domain(nedgecount(itopo)+ip) == ii-1) then
                             iedgeglob = edge_c2g(nedgecount(itopo)+ip,itopo)
                             if (iedgeglob > 0) then
                                 nitemglob = nitemglob + 1
                                 itmpvar2D_tmp(:,nitemglob) = itmpvar2D(:,nitemglob0+ip)
                                 do ikk=1,2
                                     g1 = itmpvar2D_tmp(ikk, nitemglob)
                                     if (g1 > 0 .and. g1 <= nump(itopo,ii)) then
                                         g1 = g1 + nfacecount(itopo)
                                         itmpvar2D_tmp(ikk,nitemglob) = face_c2g(g1,itopo)
                                     else if (g1 > nump(itopo,ii)) then
                                         itmpvar2D_tmp(ikk,nitemglob) = fill_value_int
                                     end if
                                 end do
                             end if
                         end if
                     end do
                     if (ii==nfiles) then
                         itmpvar2D(:,1:nitemglob) = itmpvar2D_tmp(:,1:nitemglob)
                     end if
                  !else if (var_loctype(iv) == UNC_LOC_S) then ! variables that locate on faces, temporaly disabled
                  !    if (var_types(iv) == nf90_int .or. var_types(iv) == nf90_short) then
                  !      nfacecount = sum(nump(1:ii-1))
                  !      do ip=1, item_counts(ii)
                  !         if (item_domain(nfacecount+ip) == ii-1) then
                  !            ifaceglob = face_c2g(nfacecount+ip)
                  !            if (ifaceglob > 0) then
                  !               nitemglob = nitemglob + 1
                  !               itmpvar1D_tmp(ifaceglob) = itmpvar1D(nitemglob0+ip)
                  !            end if
                  !         end if
                  !      end do
                  !      if (ii==nfiles) then
                  !          itmpvar1D(1:nitemglob) = itmpvar1D_tmp(1:nitemglob)
                  !      end if
                  !   else if (var_types(iv) == nf90_byte) then
                  !      nfacecount = sum(nump(1:ii-1))
                  !      do ip=1, item_counts(ii)
                  !         if (item_domain(nfacecount+ip) == ii-1) then
                  !            ifaceglob = face_c2g(nfacecount+ip)
                  !            if (ifaceglob > 0) then
                  !               nitemglob = nitemglob + 1
                  !               btmpvar1D_tmp(ifaceglob,itm:itm) = btmpvar1D(nitemglob0+ip,itm:itm)
                  !            end if
                  !         end if
                  !      end do
                  !      if (ii==nfiles) then
                  !          btmpvar1D(1:nitemglob,itm:itm) = btmpvar1D_tmp(1:nitemglob,itm:itm)
                  !      end if
                  !   else
                  !      nfacecount = sum(nump(1:ii-1))
                  !      if (tmpvarDim == 1) then
                  !         do ip=1, item_counts(ii)
                  !            if (item_domain(nfacecount+ip) == ii-1) then
                  !               ifaceglob = face_c2g(nfacecount+ip)
                  !               if (ifaceglob > 0) then
                  !                  nitemglob = nitemglob + 1
                  !                  tmpvar1D_tmp(ifaceglob) = tmpvar1D(nitemglob0+ip)
                  !               end if
                  !            end if
                  !         end do
                  !         if (ii==nfiles) then
                  !             tmpvar1D(1:nitemglob) = tmpvar1D_tmp(1:nitemglob)
                  !         end if
                  !      else if (tmpvarDim == 2) then
                  !         do ip=1, item_counts(ii)
                  !            if (item_domain(nfacecount+ip) == ii-1) then
                  !               ifaceglob = face_c2g(nfacecount+ip)
                  !               if (ifaceglob > 0) then
                  !                  nitemglob = nitemglob + 1
                  !                  tmpvar2D_tmp(:,ifaceglob)=tmpvar2D(:,nitemglob0+ip)
                  !               end if
                  !            end if
                  !         end do
                  !         if (ii==nfiles) then
                  !            tmpvar2D(:,1:nitemglob) = tmpvar2D_tmp(:,1:nitemglob)
                  !         end if
                  !      end if
                  !   end if
                  else if (jaugrid == 1 .and. topodim(imesh,ii) == 1 .and. var_loctype(iv,itopo) == UNC_LOC_CN) then
                     ! For 1D mesh, the node indices in the merged file are read directly from flowelem_globalnr into node_c2g.
                     ! So for variables that locate on nodes, we do not use the shift method, but use the global numbers in node_c2g.
                     ja1DCNVar = 1
                     nnodecount(itopo) = sum(numk(itopo,1:ii-1))
                     do ip=1,item_counts(ii)
                        if (item_domain(nnodecount(itopo)+ip) == ii-1) then ! only for the node which belongs to the current domain
                           inodeglob = node_c2g(nnodecount(itopo)+ip,itopo)
                           if (inodeglob > 0) then
                              if (var_types(iv,itopo) == nf90_double) then
                                 tmpvar1D_tmp(inodeglob) = tmpvar1D(nnodecount(itopo)+ip)
                              else if (var_types(iv,itopo) == nf90_int .or. var_types(iv,itopo) == nf90_short) then
                                 itmpvar1D_tmp(inodeglob) = itmpvar1D(nnodecount(itopo)+ip)
                              else if (var_types(iv,itopo) == nf90_byte) then
                                 btmpvar1D_tmp(inodeglob,itm:itm) = btmpvar1D(nnodecount(itopo)+ip,itm:itm)
                              else if (var_types(iv,itopo) == nf90_char) then
                                 ctmpvar2D_tmp(:,inodeglob) = ctmpvar2D(:,nnodecount(itopo)+ip)
                              end if
                           end if
                        end if
                     end do
                     nitemglob  = nnodecount(itopo) + item_counts(ii) ! This variable needs to be updated for reading variables from input map files.
                  else
                     needshift = .false. ! The get_var above started at the right place, so no shifting needed yet.
                     if (var_types(iv,itopo) == nf90_double) then ! TODO: AvD: try to remove this ugly code-copy for just different types
                        do ip=1,item_counts(ii)
                           if (item_domain(nitemcount+ip) == ii-1) then
                              nitemglob = nitemglob+1
                              if (needshift) then
                                 tmpvarptr(:,:,nitemglob) = tmpvarptr(:,:,nitemglob0+ip)
                              end if
                           else
                              needshift = .true. ! From now on, all points from this var/file need one or more shifts to the left.
                           end if
                        end do
                     else if (var_types(iv,itopo) == nf90_int .or. var_types(iv,itopo) == nf90_short) then
                        do ip=1,item_counts(ii)
                           if (item_domain(nitemcount+ip) == ii-1) then
                              nitemglob = nitemglob+1
                              if (needshift) then
                                 itmpvarptr(:,:,nitemglob) = itmpvarptr(:,:,nitemglob0+ip)
                              end if
                           else
                              needshift = .true. ! From now on, all points from this var/file need one or more shifts to the left.
                           end if
                        end do
                     else if (var_types(iv,itopo) == nf90_byte) then
                        do ip=1,item_counts(ii)
                           if (item_domain(nitemcount+ip) == ii-1) then
                              nitemglob = nitemglob+1
                              if (needshift) then
                                 btmpvarptr(:,:,nitemglob,itm:itm) = btmpvarptr(:,:,nitemglob0+ip,itm:itm)
                              end if
                           else
                              needshift = .true. ! From now on, all points from this var/file need one or more shifts to the left.
                           end if
                        end do
                     end if
                     nitemcount = nitemcount + item_counts(ii)
                  end if
               end do ! ii

               ! For 1D mesh variables, after merging among all files, copy tmp array to 1D array for later writing to the merged file.
               if (ja1DCNVar == 1) then
                  nitemglob = item_counts(noutfile) ! Update nitemglob, otherwise the check on nitemglob after the outer do-loop will show unexpected error message.
                  if (var_types(iv,itopo) == nf90_double) then
                     tmpvar1D(1:nitemglob) = tmpvar1D_tmp(1:nitemglob)
                  else if (var_types(iv,itopo) == nf90_int .or. var_types(iv,itopo) == nf90_short) then
                     itmpvar1D(1:nitemglob) = itmpvar1D_tmp(1:nitemglob)
                  else if (var_types(iv,itopo) == nf90_byte) then
                     btmpvar1D(1:nitemglob,itm:itm) = btmpvar1D_tmp(1:nitemglob,itm:itm)
                  else if (var_types(iv,itopo) == nf90_char) then
                     ctmpvar2D(:,1:nitemglob) = ctmpvar2D_tmp(:,1:nitemglob)
                  end if
               end if
               ja1DCNVar = 0

               if (item_counts(noutfile) /= nitemglob) then
                  write (*,'(a,i0,a,i0,a)') 'Error: mapmerge: accumulated ', nitemglob, ' items, but expected ', item_counts(noutfile), ', for `'//var_names(iv,itopo)//'''.'
                  if (.not. verbose_mode) goto 888
               end if
               !! tmpvar is now filled with 1 var, 1 time, across all domains, without overlap, so write it now:
               if (all(var_kxdimpos(:,iv,itopo) == -1) .and. var_laydimpos(iv,itopo) == -1 .and. var_wdimpos(iv,itopo) == -1 .and. var_seddimpos(iv,itopo) == -1) then ! 1D array with no layers and no vectormax (possibly time-dep)
                  if (var_types(iv,itopo) == nf90_double) then
                     ierr = nf90_put_var(ncids(noutfile), varids_out(iv,itopo), tmpvar1D, count = count_write(is:ie), start = start_idx(is:ie))
                  else if (var_types(iv,itopo) == nf90_int .or. var_types(iv,itopo) == nf90_short) then
                     ierr = nf90_put_var(ncids(noutfile), varids_out(iv,itopo), itmpvar1D, count = count_write(is:ie), start = start_idx(is:ie))
                  else if (var_types(iv,itopo) == nf90_byte) then
                     if (itm == mapclass_time_buffer_size) then
                        ierr = nf90_put_var(ncids(noutfile), varids_out(iv,itopo), btmpvar1D, count = [count_write(is), mapclass_time_buffer_size*count_write(ie)], start = [start_idx(is), start_idx(ie) + 1 - mapclass_time_buffer_size])
                     endif
                  end if
               else if (var_kxdimpos(ikx0,iv,itopo) /= -1 .neqv. var_laydimpos(iv,itopo) /= -1) then ! Either a vectormax OR a laydim
                  if (var_types(iv,itopo) == nf90_double) then
                     if (numkx(iv, itopo) == 2) then
                        ierr = nf90_put_var(ncids(noutfile), varids_out(iv,itopo), tmpvar3D, count = count_write(is:ie), start = start_idx(is:ie))
                     else if (var_seddimpos(iv,itopo) /= -1) then ! if it is a sediment variable
                        ierr = nf90_put_var(ncids(noutfile), varids_out(iv,itopo), tmpvar2D, count = count_write(is:ie), start = start_idx(is:ie), map = (/ count_write (var_kxdimpos(ikx0,iv,itopo)), 1, count_write (var_kxdimpos(ikx0,iv,itopo))*item_counts(ii) /))
                     else
                        ierr = nf90_put_var(ncids(noutfile), varids_out(iv,itopo), tmpvar2D, count = count_write(is:ie), start = start_idx(is:ie))
                     end if
                  else if (var_types(iv,itopo) == nf90_int .or. var_types(iv,itopo) == nf90_short) then
                     ierr = nf90_put_var(ncids(noutfile), varids_out(iv,itopo), itmpvar2D, count = count_write(is:ie), start = start_idx(is:ie))
                  else if (var_types(iv,itopo) == nf90_char) then
                     ierr = nf90_put_var(ncids(noutfile), varids_out(iv,itopo), ctmpvar2D, count = count_write(is:ie), start = start_idx(is:ie))
                  end if
               else if (var_kxdimpos(ikx0,iv,itopo) /= -1 .neqv. (var_laydimpos(iv,itopo) /= -1 .or. var_wdimpos(iv,itopo) /= -1)) then
                  ! Either a vectormax OR a wdim/laydim
                  if (var_types(iv,itopo) == nf90_double) then
                     ierr = nf90_put_var(ncids(noutfile), varids_out(iv,itopo), tmpvar2D, count = count_write(is:ie), start = start_idx(is:ie))
                  else if (var_types(iv,itopo) == nf90_int .or. var_types(iv,itopo) == nf90_short) then
                     ierr = nf90_put_var(ncids(noutfile), varids_out(iv,itopo), itmpvar2D, count = count_write(is:ie), start = start_idx(is:ie))
                  end if
               else ! Both a vectormax AND a laydim
                  if (var_types(iv,itopo) == nf90_double) then
                     ierr = nf90_put_var(ncids(noutfile), varids_out(iv,itopo), tmpvar3D, count = count_write(is:ie), start = start_idx(is:ie))
                  end if
               end if
               !if (kmx(noutfile) > 0) then
               !   ierr = nf90_put_var(ncids(noutfile), varids_out(iv), tmpvar, count = (/ kmx(noutfile), ndx(noutfile), 1 /), start = (/ 1, 1, it /))
               !else
               !   ierr = nf90_put_var(ncids(noutfile), varids_out(iv), tmpvar, count = (/ ndx(noutfile), 1 /), start = (/ 1, it /))
               !end if
               !if (ierr /= nf90_noerr) then
               !   write (*,'(a,i0,a)') 'Error: mapmerge: could not write merged variable `'//trim(var_names(iv))//''' into output file `'//trim(outfile)//''' (itime=', it, ').'
               !   if (.not. verbose_mode) goto 888
               !end if
               
               ! Check: if this was time-independent variable, first iteration-step was enough for reading+writing.
               if (var_timdimpos(iv,itopo) == -1) then
                  exit ! it
               end if

               end do ! it
         end if

         ! if writing with time buffer, write remaining time steps:
         if (var_types(iv,itopo) == nf90_byte) then
            if (itm /= mapclass_time_buffer_size) then
               ierr = nf90_put_var(ncids(noutfile), varids_out(iv,itopo), btmpvar1D(:,1:itm), count = [count_write(is), itm*count_write(ie)], start = [start_idx(is), start_idx(ie) + 1 - itm])
            endif
         endif

         if (verbose_mode) then
            call progress(tmpstr1, 100) ! generate the progress bar.
         end if

      end do ! iv
   end do ! itopo


   ! 7. Write some useful meta info on all merged domains into the output file.
   ! These variables are used for restart. Since restart from a ugrid file is not supported yet, do not write these variables if it is ugrid.
   ! TODO: see UNST-4947 which aims to enable restart from ugrid file.
   if (jaugrid == 0) then
      nfacecount = 0
      nedgecount = 0
      nnodecount = 0

      do ii=1,nfiles
         ierr = nf90_put_var(ncids(noutfile), id_part_face_start, nfacecount(maxTopodim)+1, start=(/ ii /))
         ierr = nf90_put_var(ncids(noutfile), id_part_edge_start, nedgecount(maxTopodim)+1, start=(/ ii /))
         ierr = nf90_put_var(ncids(noutfile), id_part_node_start, nnodecount(maxTopodim)+1, start=(/ ii /))
      
         nfacecount(maxTopodim) = nfacecount(maxTopodim) + ndxg (maxTopodim,ii)
         nedgecount(maxTopodim) = nedgecount(maxTopodim) + lnxg (maxTopodim,ii)
         nnodecount(maxTopodim) = nnodecount(maxTopodim) + numkg(maxTopodim,ii)
      end do
      ierr = nf90_put_var(ncids(noutfile), id_part_face_count, ndxg (maxTopodim,1:nfiles))
      ierr = nf90_put_var(ncids(noutfile), id_part_edge_count, lnxg (maxTopodim,1:nfiles))
      ierr = nf90_put_var(ncids(noutfile), id_part_node_count, numkg(maxTopodim,1:nfiles))
      
      if (nbndglob(maxTopodim)>0) then
         nbndcount  = 0
         do ii=1,nfiles
            ierr = nf90_put_var(ncids(noutfile), id_part_facebnd_start, nbndcount(maxTopodim)+1, start=(/ ii /))
            nbndcount(maxTopodim) = nbndcount(maxTopodim)  + ndxbnd(maxTopodim,ii)
         enddo
         ierr = nf90_put_var(ncids(noutfile), id_part_facebnd_count, ndxbnd(maxTopodim, 1:nfiles))
      endif
   end if

   !! 8. Write variables of 1d2d mesh contacts.
   if (jaugrid == 1 .and. maxNcontacts > 0) then
      maxitems = numl1d2dMax
      allocate(character(len=0) :: att_value)
      att_value = ''
      ! 8a. Loop on each variable of each mesh contact of the reference file ifileScan
      do icontact = 1, maxNcontacts
         write (*,'(a,i0,a)') 'Write variables for mesh contact ', icontact, ' to the output file.'
         ifileScan = icfile(icontact)
         nvarsScan = nvars_contact(icontact, ifileScan)
         do iv = 1, nvarsScan
            if (verbose_mode) then
               write (tmpstr1, '(a,i0,a,i0,a)') 'Var #', iv, ' of ', nvarsScan, ': '
            end if

            ! Note: Below we inquire the variable/dimension from the output file,
            ! because the needed information there is the same as in the reference file.
            id = id_contactvars(icontact,iv) ! Var id in the output file. 
            if (ug_is_link_topology(ncids(noutfile), id)) then ! It is the 1d2d link connectivity variable
               if (verbose_mode) then
                  call progress(tmpstr1, 1) ! generate the progress bar.
               end if
               ierr = nf90_put_var(ncids(noutfile), id, links1d2dnodes_g(1:2,1:numl1d2d(icontact,noutfile), icontact))
               if (verbose_mode) then
                  call progress(tmpstr1, 100) ! generate the progress bar.
               end if
            else ! Other 1d2d link variables
               ierr = nf90_inquire_variable(ncids(noutfile), id, name=varname, xtype=vartype, ndims=nvardims, dimids=tmpdimids)
               is = 1
               ie = nvardims
               start_idx  (is:ie) = 1 ! For all relevant dimensions for this var: start indices to be set below
               count_read (is:ie) = 1 ! For all relevant dimensions for this var: read  counts to be set below
               count_write(is:ie) = 1 ! For all relevant dimensions for this var: write counts to be set below

               ! Check dimensions of this variable
               idimtime     = -1
               idimlink1d2d = -1
               idimstring   = -1
               do idim = 1, nvardims
                  if (tmpdimids(idim) == id_timedim(1,noutfile)) then
                     idimtime = idim     ! This variable is time-dependent
                  else if (tmpdimids(idim) == cids_output%dimids(cdim_ncontacts)) then
                     idimlink1d2d = idim ! This variable has dimension of 1d2d links
                  else
                     idimstring = idim   ! This variable has string dimension, either strLengthIds or strLengthLongNames
                  end if
               end do
               if (idimlink1d2d < 0) then
                  write (*,'(a)') 'Warning: mapmerge: variable `'//trim(varname)//''' does not have a proper link1d2d dimension. Skipping.'
                  cycle
               end if

               ! Reallocate arrays
               if (vartype == nf90_double) then
                  call realloc(tmpvar1D,      maxitems, keepExisting=.false., fill = fill_value_double)
                  call realloc(tmpvar1D_tmp,  maxitems, keepExisting=.false., fill = fill_value_double)
               else if (vartype == nf90_int .or. vartype == nf90_short) then
                  call realloc(itmpvar1D,     maxitems, keepExisting=.false., fill = fill_value_int)
                  call realloc(itmpvar1D_tmp, maxitems, keepExisting=.false., fill = fill_value_int)
               else if (vartype == nf90_char) then
                  ierr = nf90_inquire_dimension(ncids(noutfile), tmpdimids(idimstring), name = dimname, len = nlen)
                  count_read(idimstring) = nlen
                  call realloc(ctmpvar2D,     (/ nlen, maxitems /), keepExisting=.false., fill='')
                  call realloc(ctmpvar2D_tmp, (/ nlen, maxitems /), keepExisting=.false., fill='')
               end if

               ! 8b. Do the merging on each time step
               do it=1,ntsel
                  if (verbose_mode) then
                     call progress(tmpstr1, ceiling((it-1)*100.0/ntsel)) ! generate the progress bar.
                  end if

                  ! 8b.1 Time dimension: Which timestep to read from input files
                  if (idimtime > 0) then ! This variable has time dimension
                     start_idx(idimtime) = itimsel(it)
                  end if

                  nitemglob  = 0
                  do ii=1,nfiles
                     ! 8b.2 Read from each file
                     nitemglob0 = nitemglob
                     count_read(idimlink1d2d) = numl1d2d(icontact,ii)

                     varid = varids_contact(iv, icontact,ii)
                     if (varid == -1) then ! This variable is not in file ii, skip file ii
                        cycle
                     end if

                     if (vartype == nf90_double) then
                        ierr = nf90_get_var(ncids(ii), varid, tmpvar1D(nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                     else if (vartype == nf90_int .or. vartype == nf90_short) then
                        ierr = nf90_get_var(ncids(ii), varid, itmpvar1D(nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                     else if (vartype == nf90_char) then
                        ierr = nf90_get_var(ncids(ii), varid, ctmpvar2D(:,nitemglob0+1:), count=count_read(is:ie), start=start_idx(is:ie))
                     end if

                     ! 8b.3 Merge
                     nlink1d2dcount(icontact) = sum(numl1d2d(icontact,1:ii-1))
                     do ip=1,numl1d2d(icontact,ii)
                        if (link1d2d_domain(nlink1d2dcount(icontact)+ip, icontact) == ii-1) then ! only for the 1d2dlink which belongs to the current domain
                           ilink1d2dglob = link1d2d_c2g(nlink1d2dcount(icontact)+ip, icontact)
                           if (ilink1d2dglob > 0) then
                              if (vartype == nf90_double) then
                                 tmpvar1D_tmp(ilink1d2dglob) = tmpvar1D(nlink1d2dcount(icontact)+ip)
                              else if (vartype == nf90_int .or. vartype == nf90_short) then
                                 itmpvar1D_tmp(ilink1d2dglob) = itmpvar1D(nlink1d2dcount(icontact)+ip)
                              else if (vartype == nf90_char) then
                                 ctmpvar2D_tmp(:,ilink1d2dglob) = ctmpvar2D(:,nlink1d2dcount(icontact)+ip)
                              end if
                           end if
                        end if
                     end do
                     nitemglob  = nlink1d2dcount(icontact) + numl1d2d(icontact,ii) ! This variable needs to be updated for reading variables from input map files.

                     if (ii == nfiles) then
                        nitemglob = numl1d2d(icontact,noutfile)
                        if (vartype == nf90_double) then
                           tmpvar1D(1:nitemglob) = tmpvar1D_tmp(1:nitemglob)
                        else if (vartype == nf90_int .or. vartype == nf90_short) then
                           itmpvar1D(1:nitemglob) = itmpvar1D_tmp(1:nitemglob)
                        else if (vartype == nf90_char) then
                           ctmpvar2D(:,1:nitemglob) = ctmpvar2D_tmp(:,1:nitemglob)
                        end if
                     end if
                  end do ! ii

                  ! 8b.4 Write to output file
                  count_write(idimlink1d2d) = numl1d2d(icontact,noutfile)
                  if (vartype == nf90_double) then
                     ierr = nf90_put_var(ncids(noutfile), id, tmpvar1D, count = count_write(is:ie), start = start_idx(is:ie))
                  else if (vartype == nf90_int .or. vartype == nf90_short) then
                     ierr = nf90_put_var(ncids(noutfile), id, itmpvar1D, count = count_write(is:ie), start = start_idx(is:ie))
                  else if (vartype == nf90_char) then
                     count_write(idimstring) = count_read(idimstring)
                     ierr = nf90_put_var(ncids(noutfile), id, ctmpvar2D, count = count_write(is:ie), start = start_idx(is:ie))
                  end if

                  ! Check: if it is a time-independent variable, first iteration-step was enough for reading+writing.
                  if (idimtime < 0) then
                     exit ! it
                  end if
               end do ! it

               if (verbose_mode) then
                  call progress(tmpstr1, 100) ! generate the progress bar.
               end if
            end if
         end do ! iv
      end do ! icontact
   end if

   ! Success:
   ierr = 0

   !! Cleanup:
888 continue
   do ii=1,nfiles
      ierr = nf90_close(ncids(ii))
   end do
   ierr = nf90_close(ncids(noutfile))

   if (allocated(varids))          deallocate(varids)
   if (allocated(varids_out))      deallocate(varids_out)
   if (allocated(var_names))       deallocate(var_names)
   if (allocated(var_types))       deallocate(var_types)
   if (allocated(var_dimids))      deallocate(var_dimids)
   if (allocated(var_timdimpos))   deallocate(var_timdimpos)
   if (allocated(var_spacedimpos)) deallocate(var_spacedimpos)
   if (allocated(var_laydimpos))   deallocate(var_laydimpos)
   if (allocated(var_kxdimpos))    deallocate(var_kxdimpos)
   if (allocated(var_ndims))       deallocate(var_ndims)
   if (allocated(var_loctype))     deallocate(var_loctype)
   if (allocated(itmpvar1D))       deallocate(itmpvar1D)
   if (allocated(itmpvar2D))       deallocate(itmpvar2D)
   if (allocated(itmpvar2D_tmp))   deallocate(itmpvar2D_tmp)
   if (allocated(itmpvar2D_tmpmax))deallocate(itmpvar2D_tmpmax)
   if (allocated(tmpvar1D))        deallocate(tmpvar1D)
   if (allocated(tmpvar1D_tmp))    deallocate(tmpvar1D_tmp)
   if (allocated(tmpvar2D))        deallocate(tmpvar2D)
   if (allocated(tmpvar2D_tmp))    deallocate(tmpvar2D_tmp)
   if (allocated(tmpvar2D_tmpmax)) deallocate(tmpvar2D_tmpmax)
   if (allocated(tmpvar3D))        deallocate(tmpvar3D)
   if (allocated(btmpvar1D))       deallocate(btmpvar1D)
   if (allocated(btmpvar1D_tmp))   deallocate(btmpvar1D_tmp)
   if (allocated(ctmpvar2D))       deallocate(ctmpvar2D)
   if (allocated(ctmpvar2D_tmp))   deallocate(ctmpvar2D_tmp)

end function dfm_merge_mapfiles

!> Orders a filename list by increasing partition number.
!! Sorting is done in place.
subroutine dfm_order_by_partition(files, nfiles)
   use m_alloc
   implicit none

   character(len=MAXNAMELEN), intent(inout) :: files(:)   !< List files names, will be replaced by the sorted list.
   integer,                   intent(in)    :: nfiles     !< Number of input files.

   integer,                   allocatable :: idom(:)
   character(len=MAXNAMELEN), allocatable :: filesorig(:)
   integer :: ii, ierr, nlen

   allocate(idom(nfiles))
   allocate(filesorig(nfiles))

   do ii=1,nfiles
      filesorig(ii) = files(ii)
      nlen = len_trim(files(ii))
      ! modelname_xxxx_map.nc'
      !           0  76  3  0
      if (files(ii)(max(1,nlen-6):nlen) == '_map.nc') then
         read (files(ii)(max(1,nlen-10):nlen-7), '(i4.4)', iostat=ierr) idom(ii)
      else
         ierr = 1
      end if
      if (ierr /= 0) then
         idom(ii) = ii-1
      end if
   end do

   do ii=1,nfiles
      if (idom(ii)+1 > nfiles) then
         write (*,'(a,i0,a,i0,a)') 'Error: mapmerge: found domain number ', idom(ii), ', which exceeds the number of files: ', nfiles, '.'
         exit ! We can't really recover from this.
      end if

      files(idom(ii)+1) = filesorig(ii)
   end do

   deallocate(idom, filesorig)

end subroutine dfm_order_by_partition


subroutine progress(prefix, j)
  implicit none
  character(len=*),intent(in)  :: prefix
  integer(kind=4),intent(in)   :: j
  integer(kind=4)              :: k
  character(len=27)            ::bar

  bar = "???% |                    |"
  write(unit=bar(1:3),fmt="(i3)") j
  do k=1, ceiling(j/5.0)
    bar(6+k:6+k)="*"
  enddo
  ! print the progress bar.
  if (j==100) then
     write(unit=6,fmt="(a1,a16,a27)") char(13), prefix, bar
  else
     write(unit=6,fmt="(a1,a16,a27)", advance='no') char(13), prefix, bar
  end if

  flush(6)
  return
end subroutine progress
end module dfm_merge

