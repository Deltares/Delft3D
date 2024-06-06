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
submodule (m_init_ext_forcings) initialisation

implicit none

contains
    
!> reads new external forcings file and makes required initialisations 
 module function init_external_forcings(external_force_file_name) result(res)
 use properties
 use tree_data_types
 use tree_structures
 use messageHandling
 use m_flowexternalforcings
 use m_flowgeom
 use timespace_data, only: weightfactors, poly_tim, uniform, spaceandtime, getmeteoerror
 use m_lateral, only : balat, qplat, lat_ids, n1latsg, n2latsg, ILATTP_1D, ILATTP_2D, ILATTP_ALL, kclat, numlatsg, nnlat, nlatnd
 use m_alloc
 use m_meteo, only: ec_addtimespacerelation
 use timespace
 use string_module, only: str_tolower, strcmpi
 use system_utils
 use unstruc_files, only: resolvePath
 use unstruc_model, only: ExtfileNewMajorVersion, ExtfileNewMinorVersion
 use m_missing
 use m_ec_parameters, only: provFile_uniform
 use m_partitioninfo, only: jampi, reduce_sum, is_ghost_node
 use m_lateral, only : apply_transport
 use m_flow, only: kmx

 character(len=*), intent(in) :: external_force_file_name   !< file name for new external forcing boundary blocks
 logical                      :: res
 
 type(tree_data), pointer     :: bnd_ptr             !< tree of extForceBnd-file's [boundary] blocks
 type(tree_data), pointer     :: node_ptr            !
 integer                      :: istat               !
 integer, parameter           :: INI_KEY_LEN   = 32  !
 integer, parameter           :: INI_VALUE_LEN = 256 !
 character(len=INI_KEY_LEN)   :: group_name          !
 character(len=INI_VALUE_LEN) :: property_name
 character(len=INI_VALUE_LEN) :: property_value
 character(len=INI_VALUE_LEN) :: quantity
 character(len=INI_VALUE_LEN) :: location_file       !
 character(len=INI_VALUE_LEN) :: forcing_file        !
 character(len=INI_VALUE_LEN) :: forcing_file_type   !
 character(len=INI_VALUE_LEN) :: target_mask_file    !
 integer                      :: i,j                 !
 integer                      :: num_items_in_file   !
 integer                      :: num_items_in_block
 logical                      :: return_value
 character(len=1)             :: oper                !
 character (len=300)          :: rec

 character(len=INI_VALUE_LEN) :: nodeid
 character(len=INI_VALUE_LEN) :: branchid

 character(len=INI_VALUE_LEN) :: locid
 character(len=INI_VALUE_LEN) :: itemtype
 character(len=INI_VALUE_LEN) :: fnam
 character(len=INI_VALUE_LEN) :: basedir
 double precision             :: chainage
 integer                      :: ierr
 integer                      :: ilattype, nlat
 integer                      :: k, n, k1, nini
 integer                      :: fmmethod
 integer, dimension(1)        :: targetindex
 integer                      :: ib, ibqh, ibt
 integer                      :: maxlatsg
 integer                      :: major, minor
 integer                      :: loc_spec_type
 integer                      :: numcoordinates
 double precision, allocatable :: xcoordinates(:), ycoordinates(:)
 character(len=:), allocatable :: file_name
 integer, allocatable          :: itpenzr(:), itpenur(:)

 file_name = trim(external_force_file_name)

 res = .true.

 call tree_create(file_name, bnd_ptr)
 call prop_file('ini', file_name, bnd_ptr, istat)

 ! check FileVersion
 major = 1
 minor = 0
 call prop_get_version_number(bnd_ptr, major = major, minor = minor, success = return_value)
 if ((major /= ExtfileNewMajorVersion .and. major /= 1) .or. minor > ExtfileNewMinorVersion) then
    write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported format of new external forcing file detected in '''//file_name//''': v', major, &
        minor, '. Current format: v',ExtfileNewMajorVersion,ExtfileNewMinorVersion,'. Ignoring this file.'
    call err_flush()
    res = .false.
    return
 end if

 call init_registered_items()

 call split_filename(file_name, basedir, fnam) ! Remember base dir of input file, to resolve all refenced files below w.r.t. that base dir.

 num_items_in_file = tree_num_nodes(bnd_ptr)

 ! Build temporary reverse lookup table that maps boundary block # in file -> boundary condition nr in openbndsect (separate for u and z).
 allocate(itpenzr(num_items_in_file)); itpenzr = 0
 allocate(itpenur(num_items_in_file)); itpenur = 0
 do ibt=1,nbndz
    ib = itpenz(ibt)
    if (ib > 0) then
       itpenzr(ib) = ibt
    end if
 end do
 do ibt=1,nbndu
    ib = itpenu(ibt)
    if (ib > 0) then
       itpenur(ib) = ibt
    end if
 end do

 ! Allocate lateral provider array now, just once, because otherwise realloc's in the loop would destroy target arrays in ecInstance.
 maxlatsg = tree_count_nodes_byname(bnd_ptr, 'lateral')
 if (maxlatsg > 0) then
    call realloc(balat, maxlatsg, keepExisting = .false., fill = 0d0)
    call realloc(qplat, (/max(1,kmx),maxlatsg/), keepExisting = .false., fill = 0d0)
    call realloc(lat_ids, maxlatsg, keepExisting = .false.)
    call realloc(n1latsg, maxlatsg, keepExisting = .false., fill = 0)
    call realloc(n2latsg, maxlatsg, keepExisting = .false., fill = 0)
 end if

 ib = 0
 ibqh = 0
 do i=1,num_items_in_file

    node_ptr => bnd_ptr%child_nodes(i)%node_ptr
    group_name = trim(tree_get_name(node_ptr))
    select case (str_tolower(group_name))
    case ('general')
       ! General block, was already read.

    case ('boundary')
       res = init_boundary_forcings()
       
    case ('lateral')
        res = init_lateral_forcings()

    case ('meteo')
       res = init_meteo_forcings()

    case default       ! Unrecognized item in a ext block
       ! res remains unchanged: Not an error (support commented/disabled blocks in ext file)
       write(msgbuf, '(5a)') 'Unrecognized block in file ''', file_name, ''': [', group_name, ']. Ignoring this block.'
       call warn_flush()
       
    end select
 end do

 if (allocated(itpenzr)) deallocate(itpenzr)
 if (allocated(itpenur)) deallocate(itpenur)
 if (numlatsg > 0) then
    do n = 1,numlatsg
       balat(n) = 0d0
       do k1=n1latsg(n),n2latsg(n)
          k = nnlat(k1)
          if (k > 0) then
             if (.not. is_ghost_node(k)) then
             balat(n) = balat(n) + ba(k)
             end if
          endif
       end do
    end do
    if (jampi > 0) then
       call reduce_sum(numlatsg, balat)
    end if
    if (allocated(kclat)) then
       deallocate(kclat)
    endif
 end if

 call tree_destroy(bnd_ptr)
 if (allocated(thrtt)) then
    call init_threttimes()
 endif
 
contains
 
!> reads boundary blocks from new external forcings file and makes required initialisations 
 function init_boundary_forcings() result (res)
 
 logical :: res
 
 type(tree_data), pointer     :: block_ptr           !
 
 res = .false.
 ! First check for required input:
       call prop_get_string(node_ptr, '', 'quantity', quantity, return_value)
       if (.not. return_value) then
          write(msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''quantity'' is missing.'
          call warn_flush()
          return
       end if
       ib = ib + 1

       call prop_get_string(node_ptr, '', 'nodeId', location_file, return_value)
       if (return_value) then
          filetype = node_id
          fmmethod  = spaceandtime
       else
          filetype = poly_tim
          fmmethod  = weightfactors
          call prop_get_string(node_ptr, '', 'locationfile', location_file, return_value)
       endif

       if (return_value) then
          call resolvePath(location_file, basedir)
       else
          write(msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''locationfile'' is missing.'
          call warn_flush()
          return
       end if

       call prop_get_string(node_ptr, '', 'forcingFile ', forcing_file , return_value)
       if (return_value) then
          call resolvePath(forcing_file, basedir)
       else
          write(msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''forcingFile'' is missing.'
          call warn_flush()
          return
       end if

       oper = '-'
       call prop_get_string(node_ptr, '', 'operand ', oper , return_value)

       num_items_in_block = 0
       if (associated(node_ptr%child_nodes)) then
           num_items_in_block = size(node_ptr%child_nodes)
       endif

       ! Now loop over all key-value pairs, to support reading *multiple* lines with forcingfile=...
       do j=1,num_items_in_block
          block_ptr => node_ptr%child_nodes(j)%node_ptr
          ! todo: read multiple quantities
          property_name = tree_get_name(block_ptr)
          call tree_get_data_string(block_ptr, property_value, return_value)
          if (return_value) then
             if (property_name == 'quantity') then
                quantity = property_value ! We already knew this
             else if (property_name == 'locationfile') then
                location_file = property_value ! We already knew this
                call resolvePath(location_file, basedir)
             else if (property_name == 'forcingfile') then
                forcing_file = property_value
                call resolvePath(forcing_file, basedir)
                if ( oper /= 'O' .and. oper /= '+' ) then
	               oper = 'O'
                   if (quantity_pli_combination_is_registered(quantity, location_file)) then
                      oper = '+'
                   endif
		        end if 
                call register_quantity_pli_combination(quantity, location_file)
                if (filetype == node_id .or. quantity == 'qhbnd') then
                   select case(quantity)
                   case ('waterlevelbnd')
                     targetIndex = itpenzr(ib)
                   case ('qhbnd')
                      ibqh = ibqh + 1
                      targetindex = (/ibqh/)
                      if (filetype/=node_id) then
                          location_file = qhpliname(ibqh)
                      end if
                   case ('dischargebnd')
                      targetIndex = itpenur(ib)
                   case default
                      targetindex = (/-1/)
                   end select

                   if (targetindex(1) <= 0) then
                      ! This boundary has been skipped in an earlier phase (findexternalboundarypoints),
                      ! so, also do *not* connect it as a spacetimerelation here.
                      return_value = .true. ! No failure: boundaries are allowed to remain disconnected.
                   else if (forcing_file == '-') then
                      return_value = addtimespacerelation_boundaries(quantity, location_file, filetype=node_id, method=fmmethod, &
                             operand=oper, targetindex=targetindex(1))
                   else
                      return_value = addtimespacerelation_boundaries(quantity, location_file, filetype=node_id, method=fmmethod, &
                             operand=oper, forcingfile = forcing_file, targetindex=targetindex(1))
                   endif
                else
                   if (forcing_file == '-') then
                      return_value = addtimespacerelation_boundaries(quantity, location_file, filetype=filetype, method=fmmethod, &
                             operand=oper)
                   else
                      return_value = addtimespacerelation_boundaries(quantity, location_file, filetype=filetype, method=fmmethod, &
                             operand=oper, forcingfile = forcing_file)
                   endif
                endif
                res = res .and. return_value ! Remember any previous errors.
                oper = '-'
             else if (property_name == 'operand') then
                continue
             else if (property_name == 'returntime' .or. property_name == 'return_time') then
                continue                   ! used elsewhere to set Thatcher-Harleman delay
             else if (property_name == 'openboundarytolerance') then
                continue                   ! used in findexternalboundarypoints/readlocationfiles... to set search distance. Not relevant here.
             else if (property_name == 'nodeid') then
                continue
             else if (property_name == 'bndwidth1d') then
                continue
             else if (property_name == 'bndbldepth') then
                continue
             else
                ! res remains unchanged: support ignored lines in ext file.
                write(msgbuf, '(9a)') 'Unrecognized line in file ''', file_name, ''' for block [', group_name, ']: ', trim(property_name), ' = ', trim(property_value), '. Ignoring this line.'
                call warn_flush()
                cycle
             endif
          endif
       enddo	      
       if (.not. return_value) then ! This addtimespace was not successful
          rec = getmeteoerror()
          if (len_trim(rec)>0) then
             call mess(LEVEL_WARN, trim(rec))
          endif
          call mess(LEVEL_WARN, 'initboundaryblockforcings: Error while initializing quantity '''//trim(quantity)//'''. Check preceding log lines for details.')
       end if
       
       res = .true.
       
 end function init_boundary_forcings
 
 
 !> reads lateral blocks from new external forcings file and makes required initialisations 
 function init_lateral_forcings() result(res)
 
 logical  :: res
 
 res = .false.
 ! [Lateral]
       ! Id = ...
       locid = ' '
       call prop_get(node_ptr, '', 'Id', locid, success)
       if (.not. success .or. len_trim(locid) == 0) then
          write(msgbuf, '(a,i0,a)') 'Required field ''Id'' missing in lateral (block #', i, ').'
          call warn_flush()
          return
       end if

       ! locationType = optional for lateral
       ! fileVersion >= 2: locationType = 1d | 2d | all
       ! fileVersion <= 1: Type         = 1d | 2d | 1d2d
       itemtype = ' '
       if (major >= 2) then
          call prop_get(node_ptr, '', 'locationType', itemtype, success)
       else
          call prop_get(node_ptr, '', 'Type',         itemtype, success)
       end if
       select case (str_tolower(trim(itemtype)))
       case ('1d')
          ilattype = ILATTP_1D
       case ('2d')
          ilattype = ILATTP_2D
       case ('1d2d', 'all')
          ilattype = ILATTP_ALL
       case default
          ilattype = ILATTP_ALL
       end select
       
       call reserve_sufficient_space(apply_transport, numlatsg+1, 0)
       call prop_get(node_ptr, '', 'applyTransport', apply_transport(numlatsg+1), success)

       ! [lateral]
       ! fileVersion >= 2: nodeId                  => location_specifier = LOCTP_NODEID
       !                   branchId+chainage       => location_specifier = LOCTP_BRANCH_CHAINAGE
       !                   numcoor+xcoors+ycoors   => location_specifier = LOCTP_XY_POLYGON
       ! fileVersion <= 1: LocationFile = test.pol => location_specifier = LOCTP_POLYGON_FILE
       loc_spec_type      = imiss
       nodeId             = ' '
       branchid           = ' '
       chainage           = dmiss
       numcoordinates     = imiss
       !
       if (major >= 2) then
          call prop_get_string(node_ptr, '', 'nodeId', nodeId, success)
          if (success) then
             loc_spec_type = LOCTP_NODEID
             ilattype = ILATTP_1D
          else
             call prop_get(node_ptr, '', 'branchId',         branchid, success)
             if (success) then
                call prop_get(node_ptr, '', 'chainage',         chainage, success)
             end if
             if (success) then
                if (len_trim(branchid)>0 .and. chainage /= dmiss .and. chainage >= 0.0d0) then
                   loc_spec_type = LOCTP_BRANCHID_CHAINAGE
                   ilattype = ILATTP_1D
                end if
             else
                call prop_get(node_ptr, '', 'numCoordinates',   numcoordinates, success)
                if (success .and. numcoordinates > 0) then
                   allocate(xcoordinates(numcoordinates), stat=ierr)
                   allocate(ycoordinates(numcoordinates), stat=ierr)
                   call prop_get_doubles(node_ptr, '', 'xCoordinates',     xcoordinates, numcoordinates, success)
                   call prop_get_doubles(node_ptr, '', 'yCoordinates',     ycoordinates, numcoordinates, success)
                   if (success) then
                      loc_spec_type = LOCTP_POLYGON_XY
                   end if
                end if
             end if
          end if
       else ! fileVersion <= 1
          loc_spec_type = LOCTP_POLYGON_FILE
          !
          location_file = ''
          call prop_get(node_ptr, '', 'LocationFile', location_file, success)
          if (.not. success .or. len_trim(location_file) == 0) then
             write(msgbuf, '(a,a,a)') 'Required field ''LocationFile'' missing in lateral ''', trim(locid), '''.'
             call warn_flush()
             return
          else
             call resolvePath(location_file, basedir)
          end if
       end if
       if (loc_spec_type == imiss) then
          write(msgbuf, '(a,a,a)') 'Unrecognized location specification in lateral ''', trim(locid), '''.'
          call warn_flush()
          return
       end if

       call ini_alloc_laterals()

       call prepare_lateral_mask(kclat, ilattype)

       numlatsg = numlatsg + 1
       call realloc(nnlat, max(2*ndxi, nlatnd+ndxi), keepExisting = .true., fill = 0)
       call selectelset_internal_nodes(xz, yz, kclat, ndxi, nnLat(nlatnd+1:), nlat, &
                                       loc_spec_type, location_file, numcoordinates, xcoordinates, ycoordinates, branchid, chainage, nodeId)

       n1latsg(numlatsg) = nlatnd + 1
       n2latsg(numlatsg) = nlatnd + nlat

       nlatnd = nlatnd + nlat

       if (allocated(xcoordinates)) deallocate(xcoordinates, stat=ierr)
       if (allocated(ycoordinates)) deallocate(ycoordinates, stat=ierr)

       ! [lateral]
       ! Flow = 1.23 | test.tim | REALTIME
       kx = 1
       rec = ' '
       call prop_get(node_ptr, '', 'discharge', rec, success)
       if (.not. success .and. major <= 1) then ! Old pre-2.00 keyword 'flow'
          call prop_get(node_ptr, '', 'flow', rec, success)
       end if
       if (len_trim(rec) > 0) then
          call resolvePath(rec, basedir)
       else
          write(msgbuf, '(a,a,a)') 'Required field ''discharge'' missing in lateral ''', trim(locid), '''.'
          call warn_flush()
          return
       end if

       qid = 'lateral_discharge' ! New quantity name in .bc files
       success = adduniformtimerelation_objects(qid, '', 'lateral', trim(locid), 'discharge', trim(rec), numlatsg, kx, qplat(1,:))
       if (success) then
          jaqin = 1
          lat_ids(numlatsg) = locid
       end if
       
       res = .true.
       
 end function init_lateral_forcings

 !> reads meteo blocks from new external forcings file and makes required initialisations 
 function init_meteo_forcings() result(res)
 
 logical :: res
 
 integer, allocatable :: k_mask(:)
 logical              :: invert_mask
 
 res = .false.
 ! First check for required input:
       call prop_get_string(node_ptr, '', 'quantity', quantity, return_value)
       if (.not. return_value) then
          write(msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''quantity'' is missing.'
          call warn_flush()
          return
       end if

       call prop_get_string(node_ptr, '', 'forcingFileType', forcing_file_type, return_value)
       if (.not. return_value) then
          write(msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''forcingFileType'' is missing.'
          call warn_flush()
          return
       end if

       call prop_get_string(node_ptr, '', 'forcingFile', forcing_file , return_value)
       if (.not. return_value) then
          write(msgbuf, '(5a)') 'Incomplete block in file ''', file_name, ''': [', group_name, ']. Field ''forcingFile'' is missing.'
          call warn_flush()
          return
       else
          call resolvePath(forcing_file, basedir)
       end if
       oper = 'O'
       call prop_get_string(node_ptr, '', 'operand', oper , return_value)

       target_mask_file = ''
       call prop_get_string(node_ptr, '', 'targetMaskFile', target_mask_file, return_value)
       invert_mask = .false.
       call prop_get_logical(node_ptr, '', 'targetMaskInvert', invert_mask , return_value)

       call realloc(kcsini, ndx, keepExisting=.false., fill = 0)
       if (len_trim(target_mask_file) > 0) then
          ! Mask flow nodes based on inside polygon(s), or outside.
          ! in: kcs, all flow nodes, out: kcsini: all masked flow nodes.
          call realloc(k_mask, ndx, keepExisting=.false., fill = 0)
          call selectelset_internal_nodes(xz, yz, kcs, ndx, k_mask, nini, LOCTP_POLYGON_FILE, target_mask_file)
          do n=1,nini
             kcsini(k_mask(n)) = 1
          end do

          if (invert_mask) then
             kcsini = ieor(kcsini, 1)
          end if

       else
          ! 100% masking: accept all flow nodes that were already active in kcs.
          where(kcs /= 0) kcsini = 1
       end if

       select case (quantity)
          case ('rainfall','rainfall_rate') ! case is zeer waarschijnlijk overbodig 
             if (.not. allocated(rain) ) then
                allocate ( rain(ndx) , stat=ierr)
                call aerr('rain(ndx)', ierr, ndx)
                rain = 0d0
             endif
             kx = 1
          case ('qext')
             ! Only time-independent sample file supported for now: sets Qext initially and this remains constant in time.
             if (jaQext == 0) then
                write(msgbuf, '(a)') 'quantity '''// trim(quantity) //' in file ''', file_name, ''': [', group_name, &
                    '] is missing QExt=1 in MDU. Ignoring this block.'
                call warn_flush()
                return
             end if
             if (strcmpi(forcing_file_type, 'sample')) then
                filetype = triangulation
                fmmethod = 5 ! triangulation
!                transformcoef(4) = 2 ! Nearest-neighbour
             else
                write(msgbuf, '(a)') 'Unknown forcingFileType '''// trim(forcing_file_type) //' in file ''', file_name, &
                    ''': [', group_name, '], quantity=', trim(quantity), '. Ignoring this block.'
                call warn_flush()
                return
             end if
             call prop_get(node_ptr, '', 'locationType', itemtype, success)
             select case (str_tolower(trim(itemtype)))
             case ('1d')
                ilattype = ILATTP_1D
             case ('2d')
                ilattype = ILATTP_2D
             case ('1d2d', 'all')
                ilattype = ILATTP_ALL
             case default
                ilattype = ILATTP_ALL
             end select


             call realloc(kcsini, ndx, keepExisting=.false., fill = 0)
             call prepare_lateral_mask(kcsini, ilattype)
             !kcsini(ndx2d+1:ndxi) = 1 ! Only 1D for now

             res = timespaceinitialfield(xz, yz, qext, ndx, forcing_file, filetype, fmmethod, oper, transformcoef, 2, kcsini) ! zie meteo module
             return ! This was a special case, don't continue with timespace processing below.
          case default
             write(msgbuf, '(a)') 'Unknown quantity '''// trim(quantity) //' in file ''', file_name, ''': [', group_name, &
                 ']. Ignoring this block.'
             call warn_flush()
             return
       end select
       select case (trim(str_tolower(forcing_file_type)))
       case ('bcascii')
          filetype = bcascii
          fmmethod = spaceandtime
          ! NOTE: Currently, we only support name=global meteo in.bc files, later maybe station time series as well.
          success = ec_addtimespacerelation(quantity, xz(1:ndx), yz(1:ndx), kcsini, kx,  'global', filetype=filetype, forcingfile=forcing_file, &
              method=fmmethod, operand=oper)
       case ('netcdf')
          filetype = ncgrid
          fmmethod = weightfactors
          success = ec_addtimespacerelation(quantity, xz(1:ndx), yz(1:ndx), kcsini, kx, forcing_file, filetype=filetype, method=fmmethod, operand=oper)
       case ('uniform')
          filetype = provFile_uniform
          fmmethod = spaceandtime
          success = ec_addtimespacerelation(quantity, xz(1:ndx), yz(1:ndx), kcsini, kx, forcing_file, filetype=filetype, method=fmmethod, operand=oper)
       case default
          write(msgbuf, '(a)') 'Unknown forcingFileType '''// trim(forcing_file_type) //' in file ''', file_name, &
              ''': [', group_name, ']. Ignoring this block.'
          call warn_flush()
          return
       end select
       if (success) then
          select case (quantity)
             case ('rainfall','rainfall_rate')
                jarain = 1
                jaqin = 1
             case ('windxy')
                jawind = 1
          end select
       endif
    res = .true.
    
 end function init_meteo_forcings
 
end function init_external_forcings

end submodule initialisation
