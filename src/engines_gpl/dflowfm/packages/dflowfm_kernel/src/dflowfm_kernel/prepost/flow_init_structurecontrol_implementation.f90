!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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
!> submodule that contains the implementation of flow_init_structurecontrol.
submodule(m_flow_init_structurecontrol) flow_init_structurecontrol_implementation
   use precision_basics, only: dp
   implicit none

contains
!> Initializes controllers that force structures.
   module function flow_init_structurecontrol() result(status)
      use dfm_error, only: DFM_NOERR
      use m_flowgeom, only: lnx, wu, ln, bob0
      use m_netw, only: numl
      use unstruc_channel_flow, only: initialize_compounds, initialize_structure_links, &
                                      update_lin2str_admin
      use m_structures ! Jan's channel_flow for Sobek's generalstructure (TODO)
      use m_GlobalParameters, only: ST_DAMBREAK, ST_PUMP, ST_WEIR, ST_GENERAL_ST, ST_ORIFICE, ST_GATE, &
                                    ST_UNI_WEIR, ST_CULVERT, ST_BRIDGE, ST_LONGCULVERT
      use timespace, only: LOCTP_BRANCHID_CHAINAGE, LOCTP_POLYLINE_XY, LOCTP_UNKNOWN, &
                           selectelset_internal_links, selectelset_internal_nodes
      use m_meteo
      use fm_external_forcings, only: adduniformtimerelation_objects
      use fm_external_forcings_data, only: npumpsg
      use unstruc_model, only: md_structurefile_dir
      use unstruc_files, only: resolvePath
      use string_module, only: strcmpi
      use m_longculverts, only: nlongculverts
      use m_partitioninfo, only: jampi
      use messagehandling, only: IDLEN
      use m_dambreak_breach, only: update_counters_for_dambreaks, update_dambreak_administration
      use m_update_counters_for_structures, only: update_counters_for_dambreak_or_pump
      use m_1d_structures, only: update_bedlevels_for_bridges

      logical :: status

      integer :: i, link, k, n
      integer :: num_dambreak_links, numgen
      integer, dimension(:), allocatable :: pumpidx, dambridx
      character(len=256) :: filename
      character(len=IDLEN) :: structure_id ! TODO: where to put IDLEN (now in MessageHandling)
      ! TODO: in readstruc* change incoming ids to len=*

      ! dambreak
      integer, dimension(:), allocatable :: lftopol
      integer :: i_status !< status of the function
      real(kind=dp), pointer :: tgtarr(:)
      integer :: loc_spec_type

      ! initialize exit status
      status = .true.
      i_status = DFM_NOERR
      jaoldstr = 0
      if (network%sts%count == 0) then
         status = flow_init_structurecontrol_old()
         return
      end if

      allocate (lftopol(numl))
      allocate (pumpidx(network%sts%Count))
      allocate (dambridx(network%sts%Count), source=-1)
      !
      ! Some structures may have already been read by flow1d's readStructures into network.
      !
      do i = 1, network%forcinglist%Count
         associate (pfrc => network%forcinglist%forcing(i))
            qid = trim(pfrc%quantity_id) ! e.g., qid = 'pump_capacity'
            filename = trim(pfrc%filename)
            if (.not. strcmpi(filename, 'REALTIME')) then
               call resolvePath(filename, md_structurefile_dir)
               call resolvePath(filename, md_structurefile_dir)
            end if
            ! Time-interpolated value will be placed in structure's appropriate member field, available in
            ! %targetptr, when calling ec_gettimespacevalue.
            call c_f_pointer(c_loc(pfrc%targetptr), tgtarr, [1])
            success = adduniformtimerelation_objects(qid, '', trim(pfrc%object_type), trim(pfrc%object_id), &
                                                     trim(pfrc%param_name), filename, 1, 1, tgtarr)
         end associate
      end do

      ! Find the flow link numbers for the structures.
      num_dambreak_links = 0
      do i = 1, network%sts%count
         associate (pstru => network%sts%struct(i))
            loc_spec_type = LOCTP_UNKNOWN
            if (pstru%ibran > 0) then
               loc_spec_type = LOCTP_BRANCHID_CHAINAGE
            else if (pstru%numCoordinates > 0) then
               loc_spec_type = LOCTP_POLYLINE_XY
            end if
            call selectelset_internal_links(lnx, kegen(1:numl), numgen, loc_spec_type, nump=pstru%numCoordinates, &
                                            xpin=pstru%xCoordinates, ypin=pstru%yCoordinates, &
                                            branchindex=pstru%ibran, chainage=pstru%chainage, sortLinks=1, lftopol=lftopol(num_dambreak_links + 1:numl))
            call reallocp(pstru%linknumbers, numgen)
            pstru%linknumbers = kegen(1:numgen)

            ! NOTE: kegen below does not apply to general structures. Just a placeholder for the link snapping of all structure types.
            select case (pstru%type)
            case (ST_DAMBREAK)
               num_dambreak_links = num_dambreak_links + numgen
               call update_counters_for_dambreaks(pstru%id, numgen, dambridx, i, kedb, kegen)
            case (ST_PUMP)
               call update_counters_for_dambreak_or_pump(pstru%id, numgen, npumpsg, L1pumpsg, L2pumpsg, pumpidx, i)
               kep(L1pumpsg(npumpsg):L2pumpsg(npumpsg)) = kegen(1:numgen)
               npump = l2pumpsg(npumpsg)
            end select

            if (numgen > 0) then
               i_status = initialize_structure_links(pstru, numgen, kegen(1:numgen), wu)
               call set_teta_of_1_to_neighbours(kegen(1:numgen))
            else
               call reallocP(pstru%linknumbers, 0)
               i_status = DFM_NOERR
               if (jampi == 0) then
                  ! TODO: change this if into a global reduction and check whether for each structure there is at least one partition handling it.
                  msgbuf = 'No intersecting flow links found for structure with id '''//trim(pstru%id)//'''.'
                  call msg_flush()
               end if
            end if
         end associate
      end do

      call update_lin2str_admin(network)

      call update_bedlevels_for_bridges(network%sts, bob0)

      if (network%cmps%Count > 0) then
         i_status = max(i_status, initialize_compounds(network%cmps, network%sts))
      end if
      npumpsg = network%sts%numPumps

      !
      ! pumps, including staged pumps
      !
      if (npumpsg > 0) then
         call realloc(pump_ids, npumpsg)
         call realloc(qpump, npumpsg, fill=0.0_dp)

         if (npump > 0) then
            call realloc(kpump, [3, npump], fill=0)
         end if

         do n = 1, npumpsg
            do k = L1pumpsg(n), L2pumpsg(n)
               kpump(3, k) = kep(k)
               link = abs(kpump(3, k))
               if (kpump(3, k) > 0) then
                  kpump(1, k) = ln(1, link)
                  kpump(2, k) = ln(2, link)
               else
                  kpump(1, k) = ln(2, link)
                  kpump(2, k) = ln(1, link)
               end if
            end do
         end do

         nPumpsWithLevels = 0
         call realloc(pumpsWithLevels, npumpsg)
         pumpsWithLevels = pumpidx
         call realloc(waterLevelsPumpLeft, npumpsg, fill=0.0_dp)
         call realloc(waterLevelsPumpRight, npumpsg, fill=0.0_dp)
         call realloc(pumpAveraging, [2, npumpsg], fill=0.0_dp)
         ! initialize
         do n = 1, npumpsg ! and now add it (poly_tim xys have just been prepared in separate loop)
            ! read the id first
            structure_id = ' '
            call prop_get(strs_ptr%child_nodes(pumpidx(n))%node_ptr, '', 'id', structure_id, success)
            pump_ids(n) = network%sts%struct(pumpidx(n))%id
         end do
      end if

      call update_dambreak_administration(dambridx, lftopol)

      status = i_status == DFM_NOERR

      ! Fill geometry arrays for structures
      ngategen = network%sts%numgates
      if (jahisweir > 0 .and. network%sts%numWeirs > 0) then
         call fill_geometry_arrays_structure(ST_WEIR, network%sts%numWeirs, nNodesWeir, nodeCountWeir, geomXWeir, geomYWeir)
      end if
      if (jahiscgen > 0 .and. network%sts%numGeneralStructures > 0) then
         call fill_geometry_arrays_structure(ST_GENERAL_ST, network%sts%numGeneralStructures, nNodesGenstru, nodeCountGenstru, geomXGenstru, geomYGenstru)
      end if
      if (jahisorif > 0 .and. network%sts%numOrifices > 0) then
         call fill_geometry_arrays_structure(ST_ORIFICE, network%sts%numOrifices, nNodesOrif, nodeCountOrif, geomXOrif, geomYOrif)
      end if
      if (jahisgate > 0 .and. network%sts%numgates > 0) then
         call fill_geometry_arrays_structure(ST_GATE, network%sts%numgates, nNodesgate, nodeCountgate, geomXgate, geomYgate)
      end if
      if (jahisuniweir > 0 .and. network%sts%numUniWeirs > 0) then
         call fill_geometry_arrays_structure(ST_UNI_WEIR, network%sts%numuniweirs, nNodesUniweir, nodeCountUniweir, geomXUniweir, geomYUniweir)
      end if
      if (jahisculv > 0 .and. network%sts%numculverts > 0) then
         call fill_geometry_arrays_structure(ST_CULVERT, network%sts%numculverts, nNodesCulv, nodeCountCulv, geomXCulv, geomYCulv)
      end if
      if (jahispump > 0 .and. network%sts%numPumps > 0) then
         call fill_geometry_arrays_structure(ST_PUMP, network%sts%numPumps, nNodesPump, nodeCountPump, geomXPump, geomYPump)
      end if
      if (jahisbridge > 0 .and. network%sts%numBridges > 0) then
         call fill_geometry_arrays_structure(ST_BRIDGE, network%sts%numBridges, nNodesBridge, nodeCountBridge, geomXBridge, geomYBridge)
      end if
      if (jahislongculv > 0 .and. nlongculverts > 0) then
         call fill_geometry_arrays_structure(ST_LONGCULVERT, nlongculverts, nNodesLongCulv, nodeCountLongCulv, geomXLongCulv, geomYLongCulv)
      end if

   end function flow_init_structurecontrol

!> Initializes controllers that force structures in case the file version of the structure file is equal to 1.
!! This function will become obsolete in the future.
   function flow_init_structurecontrol_old() result(status)
      use m_setfixedweirscheme3onlink, only: setfixedweirscheme3onlink
      use dfm_error, only: DFM_NOERR
      use m_hash_search, only: hashsearch
      use m_flowgeom, only: lnx, wu, ln, iadv, iadv_general_structure
      use m_netw, only: numl
      use fm_external_forcings, only: adduniformtimerelation_objects
      use unstruc_channel_flow, only: t_forcing, t_structure, initialize_structure_links, initialize_compounds, &
                                      addstructure, getstructype_from_string, update_lin2str_admin
      use m_structures ! Jan's channel_flow for Sobek's generalstructure (TODO)
      use m_strucs ! Herman's generalstructure
      use timespace, only: LOCTP_BRANCHID_CHAINAGE, LOCTP_POLYLINE_XY, LOCTP_UNKNOWN, &
                           selectelset_internal_links, LOCTP_POLYLINE_FILE, UNIFORM, SPACEANDTIME, FOURIER, JUSTUPDATE
      use m_meteo
      use m_readstructures, only: readpump
      use unstruc_model, only: md_structurefile_dir
      use unstruc_files, only: resolvePath
      use string_module, only: str_lower, strcmpi
      use m_longculverts, only: nlongculverts
      use m_partitioninfo, only: jampi
      use m_qnerror, only: qnerror
      use m_read_property, only: read_property
      use m_togeneral, only: togeneral
      use unstruc_messages, only: callback_msg
      use m_dambreak_breach, only: add_dambreak_signal, update_dambreak_administration_old

      logical :: status
      character(len=256) :: plifile
      integer :: i, L, Lf, kb, ierr, k, kbi, n, ifld
      integer :: nstr
      character(len=256) :: fnam, rec, key
      integer, allocatable :: pumpidx(:), gateidx(:), cdamidx(:), cgenidx(:), dambridx(:) ! temp
      real(kind=dp) :: tmpval
      integer :: istrtype, num_stages
      integer :: numg, numd, npum, ngs, numgen, ndambr
      type(tree_data), pointer :: str_ptr
      real(kind=dp), allocatable :: widths(:)
      real(kind=dp) :: widthtot
      real(kind=dp), dimension(1) :: xdum, ydum
      integer, dimension(1) :: kdum
      character(len=IdLen) :: strid ! TODO: where to put IdLen (now in MessageHandling)
      character(len=IdLen) :: strtype ! TODO: where to put IdLen (now in MessageHandling)
      ! TODO: in readstruc* change incoming ids to len=*
      character(len=idLen) :: branchid
      character(len=:), allocatable :: str_buf
      type(t_structure), pointer :: pstru
      type(t_forcing), pointer :: pfrc
      logical :: successloc
      logical :: is_double
      integer :: n_dambreak_links
      integer :: istrtmp
      real(kind=dp), allocatable :: hulp(:, :) ! hulp
      type(c_ptr) :: cptr

      integer, allocatable :: lftopol(:)
      integer :: istat
      real(kind=dp), pointer :: tgtarr(:)
      integer :: loc_spec_type

! initialize exit status
      status = .false.
!
! Some structures may have already been read by flow1d's readStructures into network.
!
      do i = 1, network%forcinglist%Count
         pfrc => network%forcinglist%forcing(i)

         qid = trim(pfrc%quantity_id) ! e.g., qid = 'pump_capacity'

         fnam = trim(pfrc%filename)
         if (.not. strcmpi(fnam, 'REALTIME')) then
            call resolvePath(fnam, md_structurefile_dir)
         end if

         ! Time-interpolated value will be placed in structure's appropriate member field, available in %targetptr, when calling ec_gettimespacevalue.
         cptr = c_loc(pfrc%targetptr)
         call c_f_pointer(cptr, tgtarr, [1])
         success = adduniformtimerelation_objects(qid, '', trim(pfrc%object_type), trim(pfrc%object_id), trim(pfrc%param_name), trim(fnam), 1, 1, tgtarr)

      end do
!
! Hereafter, conventional dflowfm structures.
!
      istat = 0
      ngs = 0 ! Local counter for all crossed flow liks by *all* general structures.
      nstr = tree_num_nodes(strs_ptr) ! TODO: minor issue: will count *all* children in structure file.
      if (nstr > 0) then
         jaoldstr = 0
      else
         jaoldstr = 1
         status = .true.
         return
      end if

      allocate (widths(numl))
      allocate (lftopol(numl))
      allocate (pumpidx(nstr))
      allocate (gateidx(nstr))
      allocate (cdamidx(nstr))
      allocate (cgenidx(nstr))
      allocate (dambridx(nstr), source=-1)
      if (allocated(dambreakPolygons)) then
         deallocate (dambreakPolygons)
      end if
      allocate (dambreakPolygons(nstr))

! UNST-3308: early counting of n_dambreak_links is needed here, because of lftopol array
      n_dambreak_links = 0

! NOTE: readStructures(network, md_structurefile) has already been called.
      do i = 1, network%sts%count
         pstru => network%sts%struct(i)

         loc_spec_type = LOCTP_UNKNOWN
         if (pstru%ibran > 0) then
            loc_spec_type = LOCTP_BRANCHID_CHAINAGE
         else if (pstru%numCoordinates > 0) then
            loc_spec_type = LOCTP_POLYLINE_XY
         end if

         ! NOTE: kegen below does not apply to general structures. Just a placeholder for the link snapping of all structure types.
         select case (pstru%type)
         case (ST_DAMBREAK)
            call selectelset_internal_links(lnx, kegen(1:numl), numgen, &
                                            loc_spec_type, nump=pstru%numCoordinates, xpin=pstru%xCoordinates, ypin=pstru%yCoordinates, &
                                            branchindex=pstru%ibran, chainage=pstru%chainage, &
                                            xps=dambreakPolygons(i)%xp, yps=dambreakPolygons(i)%yp, nps=dambreakPolygons(i)%np, &
                                            lftopol=lftopol(n_dambreak_links + 1:numl), sortLinks=1)
            n_dambreak_links = n_dambreak_links + numgen ! UNST-3308: early counting of n_dambreak_links is needed here, because of lftopol array
         case default
            call selectelset_internal_links(lnx, kegen(1:numl), numgen, &
                                            loc_spec_type, nump=pstru%numCoordinates, xpin=pstru%xCoordinates, ypin=pstru%yCoordinates, &
                                            branchindex=pstru%ibran, chainage=pstru%chainage, &
                                            sortLinks=1)
         end select

         if (numgen > 0) then
            istat = initialize_structure_links(pstru, numgen, kegen(1:numgen), wu)
            call set_teta_of_1_to_neighbours(kegen(1:numgen))
         else
            call reallocP(pstru%linknumbers, 0)
            istat = DFM_NOERR
            if (jampi == 0) then
               ! TODO: change this if into a global reduction and check whether for each structure there is at least one partition handling it.
               msgbuf = 'No intersecting flow links found for structure with id '''//trim(pstru%id)//'''.'
               call msg_flush()
            end if
         end if

      end do

      call update_lin2str_admin(network)

! UNST-3308: early counting of n_dambreak_links was needed here, because of lftopol array, but must be redone later below as well.
      n_dambreak_links = 0

      if (network%cmps%Count > 0) then
         istat = max(istat, initialize_compounds(network%cmps, network%sts))
      end if

      do i = 1, nstr
         plifile = ''
         qid = ''
         str_ptr => strs_ptr%child_nodes(i)%node_ptr

         success = .true.

         if (.not. strcmpi(tree_get_name(str_ptr), 'Structure')) then
            ! Only read [Structure] blocks, skip any other (e.g., [General]).
            cycle
         end if

         strtype = ' '
         call prop_get(str_ptr, '', 'type', strtype, success)
         if (.not. success .or. len_trim(strtype) == 0) then
            write (msgbuf, '(a,i0,a)') 'Required field ''type'' missing in structure #', i, '.'
            call warn_flush()
            cycle
         end if

         ! check if this structure concerns Flow1D type structure
         call prop_get(str_ptr, '', 'branchid', branchid, success)
         if (.not. success) call prop_get(str_ptr, '', 'numCoordinates', branchid, success)
         if (success) then
            if (trim(strtype) /= 'pump' .and. trim(strtype) /= 'dambreak') then
               cycle
            end if
         end if

         strid = ' '
         call prop_get(str_ptr, '', 'id', strid, success)
         if (.not. success .or. len_trim(strid) == 0) then
            write (msgbuf, '(a,i0,a)') 'Required field ''id'' missing in '//trim(strtype)//' #', i, '.'
            call warn_flush()
            cycle
         end if

         ! Test for old-style .pli file input, then read it here.
         ! If not, structure was already read in readStructures().
         call prop_get_alloc_string(str_ptr, '', 'polylinefile', str_buf, success)
         if (success) then
            loc_spec_type = LOCTP_POLYLINE_FILE
            plifile = str_buf
            call resolvePath(plifile, md_structurefile_dir)
         else
            istrtmp = hashsearch(network%sts%hashlist_structure, strid) ! Assumes unique names across all structure types.
            if (istrtmp == -1) then
               ! Not in sts, and also no polylinefile: error
               if (.not. strcmpi(strtype, 'compound') .and. .not. strcmpi(strtype, 'longCulvert')) then
                  write (msgbuf, '(a,a,a)') 'Required field ''polylinefile'' missing in '//trim(strtype)//' ''', trim(strid), '''.'
                  call warn_flush()
               else
                  success = .true. ! Compound processed elsewhere, success here.
               end if
               cycle
            end if

            pstru => network%sts%struct(istrtmp)

            loc_spec_type = LOCTP_UNKNOWN
            if (pstru%ibran > 0) then
               loc_spec_type = LOCTP_BRANCHID_CHAINAGE
            else if (pstru%numCoordinates > 0) then
               loc_spec_type = LOCTP_POLYLINE_XY
            end if

         end if

         select case (strtype)
         case ('gateloweredgelevel') ! Old-style controllable gateloweredgelevel
            !else if (qid == 'gateloweredgelevel' ) then

            call selectelset_internal_links(lnx, keg(ngate + 1:numl), numg, LOCTP_POLYLINE_FILE, plifile)
            success = .true.
            write (msgbuf, '(2a,i8,a)') trim(qid), trim(plifile), numg, ' nr of gateheight links'
            call msg_flush()

            ngatesg = ngatesg + 1
            gateidx(ngatesg) = i
            call realloc(L1gatesg, ngatesg)
            L1gatesg(ngatesg) = ngate + 1
            call realloc(L2gatesg, ngatesg)
            L2gatesg(ngatesg) = ngate + numg

            ngate = ngate + numg

         case ('damlevel') ! Old-style controllable damlevel
            ! else if (qid == 'damlevel' ) then

            call selectelset_internal_links(lnx, ked(ncdam + 1:numl), numd, LOCTP_POLYLINE_FILE, plifile)
            success = .true.
            write (msgbuf, '(2a,i8,a)') trim(qid), trim(plifile), numd, ' nr of dam level cells'
            call msg_flush()

            ncdamsg = ncdamsg + 1
            cdamidx(ncdamsg) = i
            call realloc(L1cdamsg, ncdamsg)
            L1cdamsg(ncdamsg) = ncdam + 1
            call realloc(L2cdamsg, ncdamsg)
            L2cdamsg(ncdamsg) = ncdam + numd

            ncdam = ncdam + numd

         case ('pump')
            if (loc_spec_type /= LOCTP_POLYLINE_FILE) then
               !use branchId, chainage
               npum = pstru%numlinks
               if (pstru%numlinks > 0) then
                  kep(npump + 1:npump + npum) = pstru%linknumbers(1:npum)
               end if
            else
               call selectelset_internal_links(lnx, kep(npump + 1:numl), npum, LOCTP_POLYLINE_FILE, plifile)
            end if

            !endif
            success = .true.
            write (msgbuf, '(2a,i8,a)') trim(qid), trim(plifile), npum, ' nr of pump links'
            call msg_flush()

            npumpsg = npumpsg + 1
            pumpidx(npumpsg) = i
            call realloc(L1pumpsg, npumpsg)
            L1pumpsg(npumpsg) = npump + 1
            call realloc(L2pumpsg, npumpsg)
            L2pumpsg(npumpsg) = npump + npum

            npump = npump + npum

         case ('dambreak')

            if (loc_spec_type /= LOCTP_POLYLINE_FILE) then
               ndambr = pstru%numlinks
               if (pstru%numlinks > 0) then
                  kedb(n_dambreak_links + 1:n_dambreak_links + ndambr) = pstru%linknumbers(1:ndambr)
               end if
            else
               call selectelset_internal_links(lnx, kedb(n_dambreak_links + 1:numl), ndambr, LOCTP_POLYLINE_FILE, plifile, &
                                               xps=dambreakPolygons(i)%xp, yps=dambreakPolygons(i)%yp, nps=dambreakPolygons(i)%np, &
                                               lftopol=lftopol(n_dambreak_links + 1:numl), sortLinks=1)
            end if

            success = .true.
            write (msgbuf, '(2a,i8,a)') trim(qid), trim(plifile), ndambr, ' nr of dambreak links'
            call msg_flush()

            call add_dambreak_signal(i, dambridx, n_dambreak_links, ndambr)

         case ('gate', 'weir', 'generalstructure') !< The various generalstructure-based structures
            if (loc_spec_type /= LOCTP_POLYLINE_FILE) then
               numgen = pstru%numlinks
               if (pstru%numlinks > 0) then
                  kegen(ncgen + 1:ncgen + numgen) = pstru%linknumbers(1:numgen)
               end if
            else
               call selectelset_internal_links(lnx, kegen(ncgen + 1:numl), numgen, LOCTP_POLYLINE_FILE, plifile, sortLinks=1)
            end if

            success = .true.
            write (msgbuf, '(a,1x,a,i8,a)') trim(qid), trim(plifile), numgen, ' nr of '//trim(strtype)//' cells'
            call msg_flush()

            ncgensg = ncgensg + 1
            cgenidx(ncgensg) = i
            call realloc(L1cgensg, ncgensg)
            L1cgensg(ncgensg) = ncgen + 1
            call realloc(L2cgensg, ncgensg)
            L2cgensg(ncgensg) = ncgen + numgen

            ncgen = ncgen + numgen
            ! For later usage split up the set of all generalstructures into weirs, gates or true general structures (in user input)
            select case (strtype)
            case ('weir')
               nweirgen = nweirgen + 1
            case ('gate')
               ngategen = ngategen + 1
            case ('generalstructure')
               ngenstru = ngenstru + 1
            case default
               call mess(LEVEL_ERROR, 'Programming error: unhandled structure type '''//trim(strtype)//''' under general structure block.')
            end select
         case default
            call mess(LEVEL_WARN, 'flow_init_structurecontrol: unknown structure type '''//trim(strtype)//'''.')
         end select
      end do

      xdum = 1.0_dp
      ydum = 1.0_dp
      kdum = 1

      if (ncgensg > 0) then ! All generalstructure, i.e., the weir/gate/generalstructure user input
         if (allocated(zcgen)) then
            deallocate (zcgen)
         end if
         if (allocated(kcgen)) then
            deallocate (kcgen)
         end if
         kx = 3 ! 1: crest/sill, 2: gateloweredge, 3: width (?)
         allocate (zcgen(ncgensg * kx), kcgen(4, ncgen), stat=ierr)
         call aerr('zcgen(ncgensg*kx), kcgen(4,ncgen)', ierr, ncgen * (2 * kx + 3))
         kcgen = 0.0_dp
         zcgen = huge(1.0_dp)

         if (allocated(cgen_ids)) then
            deallocate (cgen_ids)
         end if
         if (allocated(cgen_type)) then
            deallocate (cgen_type)
         end if
         if (allocated(cgen2str)) then
            deallocate (cgen2str)
         end if
         if (allocated(weir2cgen)) then
            deallocate (weir2cgen)
         end if
         if (allocated(gate2cgen)) then
            deallocate (gate2cgen)
         end if
         if (allocated(genstru2cgen)) then
            deallocate (genstru2cgen)
         end if
         allocate (cgen_ids(ncgensg), cgen_type(ncgensg), cgen2str(ncgensg))
         allocate (weir2cgen(nweirgen), gate2cgen(ngategen), genstru2cgen(ngenstru))
         if (allocated(gates)) then
            deallocate (gates)
         end if
         allocate (gates(ngategen))

         nweirgen = 0
         ngategen = 0
         ngenstru = 0

         if (allocated(fusav)) then
            deallocate (fusav)
         end if
         if (allocated(rusav)) then
            deallocate (rusav)
         end if
         if (allocated(ausav)) then
            deallocate (ausav)
         end if
         allocate (Fusav(3, ncgen), Rusav(3, ncgen), Ausav(3, ncgen), stat=ierr)
         Fusav = 0.0_dp
         Rusav = 0.0_dp
         ausav = 0.0_dp

         do n = 1, ncgensg

            do k = L1cgensg(n), L2cgensg(n)
               Lf = abs(kegen(k))
               kb = ln(1, Lf)
               kbi = ln(2, Lf)
               if (kegen(k) > 0) then
                  kcgen(1, k) = kb
                  kcgen(2, k) = kbi
               else
                  kcgen(1, k) = kbi
                  kcgen(2, k) = kb
               end if

               kcgen(3, k) = Lf
               kcgen(4, k) = n ! pointer to general structure signal nr n
               call setfixedweirscheme3onlink(Lf)
               iadv(Lf) = IADV_GENERAL_STRUCTURE ! iadv = general

            end do

         end do

         allocate (hulp(NUMGENERALKEYWRD, ncgensg))
         hulp(idx_upstream1width, 1:ncgensg) = 10.0_dp ! Upstream1Width
         hulp(idx_upstream1level, 1:ncgensg) = 0.0_dp ! Upstreamlevel
         hulp(idx_upstream2width, 1:ncgensg) = 10.0_dp ! Upstream2Width
         hulp(idx_upstream2level, 1:ncgensg) = 0.0_dp ! Upstream2Level
         hulp(idx_crestwidth, 1:ncgensg) = 10.0_dp ! CrestWidth
         hulp(idx_crestlevel, 1:ncgensg) = 0.0_dp ! CrestLevel
         hulp(idx_downstream1width, 1:ncgensg) = 10.0_dp ! Downstream1Width
         hulp(idx_dowsstream1level, 1:ncgensg) = 0.0_dp ! Downstream1Level
         hulp(idx_downstream2width, 1:ncgensg) = 10.0_dp ! Downstream2Width
         hulp(idx_downstream2level, 1:ncgensg) = 0.0_dp ! Downstream2Level
         hulp(idx_gateloweredgelevel, 1:ncgensg) = 0.0_dp ! GateLowerEdgeLevel
         hulp(idx_gateheightintervalcntrl, 1:ncgensg) = huge(1.0_dp) ! gateheightintervalcntrl=12
         hulp(idx_pos_freegateflowcoeff, 1:ncgensg) = 1.0_dp ! pos_freegateflowcoeff=1
         hulp(idx_pos_drowngateflowcoeff, 1:ncgensg) = 1.0_dp ! pos_drowngateflowcoeff=1
         hulp(idx_pos_freeweirflowcoeff, 1:ncgensg) = 1.0_dp ! pos_freeweirflowcoeff=1
         hulp(idx_pos_drownweirflowcoeff, 1:ncgensg) = 1.0_dp ! pos_drownweirflowcoeff=1.0
         hulp(idx_pos_contrcoeffreegate, 1:ncgensg) = 1.0_dp ! pos_contrcoeffreegate=0.6
         hulp(idx_neg_freegateflowcoeff, 1:ncgensg) = 1.0_dp ! neg_freegateflowcoeff=1
         hulp(idx_neg_drowngateflowcoeff, 1:ncgensg) = 1.0_dp ! neg_drowngateflowcoeff=1
         hulp(idx_neg_freeweirflowcoeff, 1:ncgensg) = 1.0_dp ! neg_freeweirflowcoeff=1
         hulp(idx_neg_drownweirflowcoeff, 1:ncgensg) = 1.0_dp ! neg_drownweirflowcoeff=1.0
         hulp(idx_neg_contrcoeffreegate, 1:ncgensg) = 1.0_dp ! neg_contrcoeffreegate=0.6
         hulp(idx_extraresistence, 1:ncgensg) = 0.0_dp ! extraresistance=0
         hulp(idx_dynstrucentent, 1:ncgensg) = 1.0_dp ! dynstructext=1.
         hulp(idx_gateheight, 1:ncgensg) = huge(1.0_dp) ! GateHeight
         hulp(idx_gateopeningwidth, 1:ncgensg) = 0.0_dp ! GateOpeningWidth

         if (allocated(generalstruc)) then
            deallocate (generalstruc)
         end if
         allocate (generalstruc(ncgensg))

         do n = 1, ncgensg

            str_ptr => strs_ptr%child_nodes(cgenidx(n))%node_ptr

            strtype = ' '
            call prop_get(str_ptr, '', 'type', strtype)

            strid = ' '
            call prop_get(str_ptr, '', 'id', strid, success)
            cgen_ids(n) = strid

            plifile = ' '
            call prop_get(str_ptr, '', 'polylinefile', plifile, successloc) ! TODO: Remove? This plifile is nowhere used below
            call resolvePath(plifile, md_structurefile_dir)

            ! Start with some general structure default params, and thereafter, make changes depending on actual strtype
            if (strtype /= 'generalstructure') then
               hulp(idx_upstream1width, n) = huge(1.0_dp) ! Upstream1Width
               hulp(idx_upstream1level, n) = -huge(1.0_dp) ! Upstream1Level
               hulp(idx_upstream2width, n) = huge(1.0_dp) ! Upstream2Width
               hulp(idx_upstream2level, n) = -huge(1.0_dp) ! Upstream2Level
               hulp(idx_crestwidth, n) = huge(1.0_dp) ! CrestWidth
               hulp(idx_crestlevel, n) = -huge(1.0_dp) ! CrestLevel
               hulp(idx_downstream1width, n) = huge(1.0_dp) ! Downstream1Width
               hulp(idx_dowsstream1level, n) = -huge(1.0_dp) ! Downstream1Level
               hulp(idx_downstream2width, n) = huge(1.0_dp) ! Downstream2Width
               hulp(idx_downstream2level, n) = -huge(1.0_dp) ! Downstream2Level
               hulp(idx_gateloweredgelevel, n) = huge(1.0_dp) ! GateLowerEdgeLevel
               hulp(idx_gateheightintervalcntrl, n) = huge(1.0_dp) ! gateheightintervalcntrl=12
               hulp(idx_pos_freegateflowcoeff, n) = 1.0_dp ! pos_freegateflowcoeff=1
               hulp(idx_pos_drowngateflowcoeff, n) = 1.0_dp ! pos_drowngateflowcoeff=1
               hulp(idx_pos_freeweirflowcoeff, n) = 1.0_dp ! pos_freeweirflowcoeff=1
               hulp(idx_pos_drownweirflowcoeff, n) = 1.0_dp ! pos_drownweirflowcoeff=1.0
               hulp(idx_pos_contrcoeffreegate, n) = 1.0_dp ! pos_contrcoeffreegate=0.6
               hulp(idx_neg_freegateflowcoeff, n) = 1.0_dp ! neg_freegateflowcoeff=1
               hulp(idx_neg_drowngateflowcoeff, n) = 1.0_dp ! neg_drowngateflowcoeff=1
               hulp(idx_neg_freeweirflowcoeff, n) = 1.0_dp ! neg_freeweirflowcoeff=1
               hulp(idx_neg_drownweirflowcoeff, n) = 1.0_dp ! neg_drownweirflowcoeff=1.0
               hulp(idx_neg_contrcoeffreegate, n) = 1.0_dp ! neg_contrcoeffreegate=0.6
               hulp(idx_extraresistence, n) = 0.0_dp ! extraresistance=0
               hulp(idx_dynstrucentent, n) = 1.0_dp ! dynstructext=1.
               hulp(idx_gateheight, n) = huge(1.0_dp) ! GateHeight
               hulp(idx_gateopeningwidth, n) = 0.0_dp ! GateOpeningWidth
            end if

            select case (strtype)
      !! WEIR !!
            case ('weir')
               rec = ' '
               key = 'CrestLevel'
               call read_property(strs_ptr%child_nodes(cgenidx(n))%node_ptr, trim(key), rec, tmpval, is_double, strid, successloc)
               if (.not. successloc) then
                  write (msgbuf, '(a,a,a)') 'Required field '//trim(key)//' missing in weir ''', trim(strid), '''.'
                  call warn_flush()
                  cycle
               end if
               if (is_double) then
                  ! Constant value for always, set it now already.
                  zcgen((n - 1) * kx + 1) = tmpval
                  hulp(idx_crestlevel, n) = tmpval
               else
                  if (trim(rec) == 'REALTIME') then
                     success = .true.
                     ! zcgen(1, 1+kx, ..) should be filled via DLL's API
                     write (msgbuf, '(a,a,a)') 'Control for weir ''', trim(strid), ''', CrestLevel set to REALTIME.'
                     call dbg_flush()
                  else
                     qid = 'generalstructure' ! TODO: werkt dit als je de losse quantities (crest/gateloweredge/width) dezelfde id geeft, maar wel netjes correct veschillende offset?
                     fnam = trim(rec)
                     call resolvePath(fnam, md_structurefile_dir)
                     ! Time-interpolated value will be placed in zcgen((n-1)*3+1) when calling ec_gettimespacevalue.
                     if (index(trim(fnam)//'|', '.tim|') > 0) then
                        success = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, uniform, spaceandtime, 'O', targetIndex=(n - 1) * kx + 1) ! Hook up 1 component at a time, even when target element set has kx=3
                     end if
                     if (index(trim(fnam)//'|', '.cmp|') > 0) then
                        success = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, fourier, justupdate, 'O', targetIndex=(n - 1) * kx + 1) ! Hook up 1 component at a time, even when target element set has kx=3
                     end if
                  end if
               end if

               tmpval = dmiss
               call prop_get(str_ptr, '', 'CrestWidth', rec, success)
               if (success) then
                  read (rec, *, iostat=ierr) tmpval
                  zcgen((n - 1) * kx + 3) = tmpval ! Constant value for always, set it now already.
               end if

               tmpval = dmiss
               call prop_get(str_ptr, '', 'lat_contr_coeff', tmpval)
               ! TODO: Herman/Jaco: this is not relevant anymore, using width (gate only)??
               if (tmpval /= dmiss) then
                  hulp(idx_pos_freegateflowcoeff, n) = tmpval
                  hulp(idx_pos_drowngateflowcoeff, n) = tmpval
                  hulp(idx_pos_freeweirflowcoeff, n) = tmpval
                  hulp(idx_pos_drownweirflowcoeff, n) = tmpval
                  hulp(idx_pos_contrcoeffreegate, n) = 1.0_dp
                  hulp(idx_neg_freegateflowcoeff, n) = tmpval
                  hulp(idx_neg_drowngateflowcoeff, n) = tmpval
                  hulp(idx_neg_freeweirflowcoeff, n) = tmpval
                  hulp(idx_neg_drownweirflowcoeff, n) = tmpval
                  hulp(idx_neg_contrcoeffreegate, n) = 1.0_dp
               end if
               nweirgen = nweirgen + 1
               weir2cgen(nweirgen) = n ! Mapping from 1:nweirgen to underlying generalstructure --> (1:ncgensg)
               cgen2str(n) = nweirgen ! Inverse mapping
               cgen_type(n) = ICGENTP_WEIR

      !! GATE !!
            case ('gate')
               rec = ' '
               key = 'CrestLevel'
               call read_property(strs_ptr%child_nodes(cgenidx(n))%node_ptr, trim(key), rec, tmpval, is_double, strid, successloc)
               if (.not. successloc) then
                  write (msgbuf, '(a)') 'Required field '//trim(key)//' missing in gate '//trim(strid)//'.'
                  call warn_flush()
                  cycle
               end if
               if (is_double) then
                  ! Constant value for always, set it now already.
                  zcgen((n - 1) * kx + 1) = tmpval
                  hulp(idx_crestlevel, n) = tmpval
               else
                  if (trim(rec) == 'REALTIME') then
                     success = .true.
                     ! zcgen(1, 1+kx, ..) should be filled via DLL's API
                     write (msgbuf, '(a,a,a)') 'Control for gate ''', trim(strid), ''', CrestLevel set to REALTIME.'
                     call dbg_flush()
                  else
                     qid = 'generalstructure'
                     fnam = trim(rec)
                     call resolvePath(fnam, md_structurefile_dir)
                     ! Time-interpolated value will be placed in zcgen((n-1)*3+1) when calling ec_gettimespacevalue.
                     if (index(trim(fnam)//'|', '.tim|') > 0) then
                        success = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, uniform, spaceandtime, 'O', targetIndex=(n - 1) * kx + 1) ! Hook up 1 component at a time, even when target element set has kx=3
                     end if
                     if (index(trim(fnam)//'|', '.cmp|') > 0) then
                        success = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, fourier, justupdate, 'O', targetIndex=(n - 1) * kx + 1) ! Hook up 1 component at a time, even when target element set has kx=3
                     end if
                  end if
               end if

               tmpval = dmiss
               call prop_get(str_ptr, '', 'CrestWidth', tmpval, success)
               if (.not. success .or. tmpval == dmiss) then
                  ! Not required, just default to all crossed flow links
                  tmpval = huge(1.0_dp)
               end if
               gates(ngategen + 1)%sill_width = tmpval

               tmpval = dmiss
               call prop_get(str_ptr, '', 'GateHeight', tmpval, success) ! Gate height (from lower edge level to top, i.e. NOT a level/position)
               if (.not. success .or. tmpval == dmiss) then
                  write (msgbuf, '(a,a,a)') 'Required field ''GateHeight'' missing in gate ''', trim(strid), '''.'
                  call warn_flush()
                  cycle
               end if
               gates(ngategen + 1)%door_height = tmpval
               hulp(idx_gateheight, n) = tmpval ! gatedoorheight.
               rec = ' '
               key = 'GateLowerEdgeLevel'
               call read_property(strs_ptr%child_nodes(cgenidx(n))%node_ptr, trim(key), rec, tmpval, is_double, strid, successloc)
               if (.not. successloc) then
                  write (msgbuf, '(a)') 'Required field '//trim(key)//' missing in gate '//trim(strid)//'.'
                  call warn_flush()
                  cycle
               end if
               if (is_double) then
                  ! Constant value for always, set it now already.
                  zcgen((n - 1) * kx + 2) = tmpval
                  hulp(idx_gateloweredgelevel, n) = tmpval
               else
                  if (trim(rec) == 'REALTIME') then
                     success = .true.
                     ! zcgen(2, 2+kx, ..) should be filled via DLL's API
                     write (msgbuf, '(a,a,a)') 'Control for gate ''', trim(strid), ''', GateLowerEdgeLevel set to REALTIME.'
                     call dbg_flush()
                  else
                     qid = 'generalstructure'
                     fnam = trim(rec)
                     call resolvePath(fnam, md_structurefile_dir)
                     ! Time-interpolated value will be placed in zcgen((n-1)*3+2) when calling ec_gettimespacevalue.
                     if (index(trim(fnam)//'|', '.tim|') > 0) then
                        success = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, uniform, spaceandtime, 'O', targetIndex=(n - 1) * kx + 2) ! Hook up 1 component at a time, even when target element set has kx=3
                     end if
                     if (index(trim(fnam)//'|', '.cmp|') > 0) then
                        success = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, fourier, justupdate, 'O', targetIndex=(n - 1) * kx + 2) ! Hook up 1 component at a time, even when target element set has kx=3
                     end if
                  end if
               end if

               ! Opening width between left and right doors. (If any. Otherwise set to 0 for a single gate door with under/overflow)
               rec = ' '
               key = 'GateOpeningWidth'
               call read_property(strs_ptr%child_nodes(cgenidx(n))%node_ptr, trim(key), rec, tmpval, is_double, strid, successloc)
               if (.not. successloc) then
                  write (msgbuf, '(a)') 'Optional field '//trim(key)//' not available for gate '//trim(strid)//'. Use default value.'
                  call msg_flush()
                  zcgen((n - 1) * kx + 3) = dmiss ! GateOpeningWidth is optional
                  success = .true.
               else
                  if (is_double) then
                     ! Constant value for always, set it now already.
                     zcgen((n - 1) * kx + 3) = tmpval
                     hulp(idx_gateopeningwidth, n) = tmpval
                  else
                     if (trim(rec) == 'REALTIME') then
                        success = .true.
                        ! zcgen(3, 3+kx, ..) should be filled via DLL's API
                        write (msgbuf, '(a,a,a)') 'Control for gate ''', trim(strid), ''', GateOpeningWidth set to REALTIME.'
                        call dbg_flush()
                     else
                        qid = 'generalstructure' ! todo: check met Hermans gatewidth, if any
                        fnam = trim(rec)
                        call resolvePath(fnam, md_structurefile_dir)
                        ! Time-interpolated value will be placed in zcgen((n-1)*3+3) when calling ec_gettimespacevalue.
                        if (index(trim(fnam)//'|', '.tim|') > 0) then
                           success = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, uniform, spaceandtime, 'O', targetIndex=(n - 1) * kx + 3) ! Hook up 1 component at a time, even when target element set has kx=3
                        end if
                        if (index(trim(fnam)//'|', '.cmp|') > 0) then
                           success = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, fourier, justupdate, 'O', targetIndex=(n - 1) * kx + 3) ! Hook up 1 component at a time, even when target element set has kx=3
                        end if
                     end if
                  end if
               end if

               rec = ' '
               call prop_get(str_ptr, '', 'GateOpeningHorizontalDirection', rec, success)
               if (.not. successloc) then
                  write (msgbuf, '(a,a,a)') 'Optional field ''GateOpeningHorizontalDirection'' not available for gate ''', trim(strid), '''. Use default value.'
                  call msg_flush()
               end if
               call str_lower(rec)
               select case (trim(rec))
               case ('from_left', 'fromleft')
                  istrtmp = IOPENDIR_FROMLEFT
               case ('from_right', 'fromright')
                  istrtmp = IOPENDIR_FROMRIGHT
               case ('symmetric')
                  istrtmp = IOPENDIR_SYMMETRIC
               case default
                  istrtmp = IOPENDIR_SYMMETRIC
               end select
               gates(ngategen + 1)%opening_direction = istrtmp

               ngategen = ngategen + 1
               gate2cgen(ngategen) = n ! Mapping from 1:ngategen to underlying generalstructure --> (1:ncgensg)
               cgen2str(n) = ngategen ! Inverse mapping
               cgen_type(n) = ICGENTP_GATE

      !! GENERALSTRUCTURE !!
            case ('generalstructure')
               do k = 1, NUMGENERALKEYWRD ! generalstructure keywords
                  tmpval = dmiss
                  key = generalkeywrd(k)
                  call read_property(strs_ptr%child_nodes(cgenidx(n))%node_ptr, trim(key), rec, tmpval, is_double, strid, successloc, is_required=.false.)
                  if (.not. successloc) then
                     ! All fields are optional.
                     cycle
                  end if
                  if (is_double) then
                     ! Constant value for always, set it now already.
                     hulp(k, n) = tmpval
                  else
                     if (trim(rec) == 'REALTIME') then
                        select case (trim(generalkeywrd(k)))
                        case ('CrestLevel', 'GateHeight', 'GateLowerEdgeLevel', 'GateOpeningWidth')
                           success = .true.
                           write (msgbuf, '(a,a,a)') 'Control for generalstructure ''', trim(strid), ''', '//trim(generalkeywrd(k))//' set to REALTIME.'
                           call dbg_flush()
                        case default
                           success = .false.
                           call mess(LEVEL_ERROR, 'Programming error: general structure via structures.ini file does not support REALTIME control for '//trim(generalkeywrd(k)))
                        end select
                     else
                        if (len_trim(plifile) > 0) then
                           ! 2D /pli file based structure
                           success = .false.
                        else
                           ! network%sts based structure, processed already before via forcinglist.
                           success = .true.
                        end if

                        select case (key)
                        case ('CrestLevel')
                           ifld = 1
                        case ('GateLowerEdgeLevel')
                           ifld = 2
                        case ('GateOpeningWidth')
                           ifld = 3
                        case default
                           success = .false.
                           call mess(LEVEL_ERROR, 'Programming error: general structure via structures.ini file does not yet support timeseries for '//trim(generalkeywrd(k)))
                           ifld = 0
                        end select
                        if (ifld > 0) then
                           ! Time-interpolated value will be placed in zcgen((n-1)*3+...) when calling ec_gettimespacevalue.
                           qid = 'generalstructure'
                           fnam = trim(rec)
                           call resolvePath(fnam, md_structurefile_dir)
                           if (index(trim(fnam)//'|', '.tim|') > 0) then
                              success = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, uniform, spaceandtime, 'O', targetIndex=(n - 1) * kx + ifld) ! Hook up 1 component at a time, even when target element set has kx=3
                           end if
                           if (index(trim(fnam)//'|', '.cmp|') > 0) then
                              success = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, fourier, justupdate, 'O', targetIndex=(n - 1) * kx + ifld) ! Hook up 1 component at a time, even when target element set has kx=3
                           end if
                        end if
                     end if
                  end if
                  if (.not. success) then
                     return
                  end if
               end do

               ! Set some zcgen values to their initial scalar values (for example, zcgen((n-1)*3+1) is quickly need for updating bobs.)
               zcgen((n - 1) * 3 + 1) = hulp(idx_crestlevel, n) ! CrestLevel
               zcgen((n - 1) * 3 + 2) = hulp(idx_gateloweredgelevel, n) ! GateLowerEdgeLevel
               zcgen((n - 1) * 3 + 3) = hulp(idx_gateopeningwidth, n) ! GateOpeningWidth

               ngenstru = ngenstru + 1
               genstru2cgen(ngenstru) = n ! Mapping from 1:ngenstru to underlying generalstructure --> (1:ncgensg)
               cgen2str(n) = ngenstru ! Inverse mapping
               cgen_type(n) = ICGENTP_GENSTRU
            end select

            widthtot = 0.0_dp
            do k = L1cgensg(n), L2cgensg(n)
               L = kegen(k)
               Lf = kcgen(3, k)
               widths(k - L1cgensg(n) + 1) = wu(Lf)
               widthtot = widthtot + wu(Lf)
            end do
            numgen = L2cgensg(n) - L1cgensg(n) + 1

            call togeneral(n, hulp(:, n), numgen, widths(1:numgen))

         end do

         deallocate (hulp)

      end if ! generalstructure: weir, gate, or true generalstructure

      if (ngate > 0) then ! Old-style controllable gateloweredgelevel

         if (allocated(kgate)) then
            deallocate (zgate, kgate)
         end if

         if (allocated(gate_ids)) then
            deallocate (gate_ids)
         end if
         allocate (gate_ids(ngatesg))
         allocate (zgate(ngatesg), kgate(3, ngate), stat=ierr)
         call aerr('zgate(ngatesg), kgate(3,ngate)', ierr, ngate * 5)
         kgate = 0.0_dp
         zgate = huge(1.0_dp)
         kx = 1

         do n = 1, ngatesg

            do k = L1gatesg(n), L2gatesg(n)
               Lf = abs(keg(k))
               kb = ln(1, Lf)
               kbi = ln(2, Lf)
               kgate(1, k) = kb
               kgate(2, k) = kbi
               kgate(3, k) = Lf

               call setfixedweirscheme3onlink(Lf)
            end do

         end do

         do n = 1, ngatesg ! and now add it (poly_tim xys have just been prepared in separate loop)
            str_ptr => strs_ptr%child_nodes(gateidx(n))%node_ptr

            strid = ' '
            call prop_get(str_ptr, '', 'id', strid, success)
            gate_ids(n) = strid

            plifile = ' '
            call prop_get(str_ptr, '', 'polylinefile', plifile, success) ! TODO: Remove? This plifile is nowhere used below
            call resolvePath(plifile, md_structurefile_dir)

            key = 'lower_edge_level'
            call read_property(strs_ptr%child_nodes(gateidx(n))%node_ptr, trim(key), rec, tmpval, is_double, strid, successloc)
            if (.not. successloc) then
               write (msgbuf, '(a)') 'Required field '//trim(key)//' missing in gate '//trim(strid)//'.'
               call warn_flush()
               cycle
            end if
            if (is_double) then
               ! Constant value for always, set it now already.
               zgate(n) = tmpval
            else
               if (trim(rec) == 'REALTIME') then
                  success = .true.
                  ! zgate should be filled via DLL's API
                  write (msgbuf, '(a,a,a)') 'Control for GateLoweredgelevel ''', trim(strid), ''' set to REALTIME.'
                  call dbg_flush()
               else
                  qid = 'gateloweredgelevel'
                  fnam = trim(rec)
                  call resolvePath(fnam, md_structurefile_dir)
                  if (index(trim(fnam)//'|', '.tim|') > 0) then
                     ! Time-interpolated value will be placed in zgate(n) when calling ec_gettimespacevalue.
                     success = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, fnam, uniform, spaceandtime, 'O', targetIndex=n)
                  end if
                  if (index(trim(fnam)//'|', '.cmp|') > 0) then
                     ! Evaluated harmonic signals value will be placed in zgate(n) when calling ec_gettimespacevalue.
                     success = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, fnam, fourier, justupdate, 'O', targetIndex=n)
                  end if
               end if
            end if
         end do

      end if ! Old style controllable gateloweredgelevel

      if (ncdamsg > 0) then ! Old-style controllable damlevel
         if (allocated(zcdam)) then
            deallocate (zcdam)
         end if
         if (allocated(kcdam)) then
            deallocate (kcdam)
         end if

         if (allocated(cdam_ids)) then
            deallocate (cdam_ids)
         end if
         allocate (cdam_ids(ncdamsg))
         allocate (zcdam(ncdamsg), kcdam(3, ncdam), stat=ierr)
         call aerr('zcdam(ncdamsg), kcdam(3,ncdam)', ierr, ncdam * 5)
         kcdam = 0.0_dp
         zcdam = huge(1.0_dp)
         kx = 1

         do n = 1, ncdamsg

            do k = L1cdamsg(n), L2cdamsg(n)
               Lf = abs(ked(k))
               kb = ln(1, Lf) ! TODO: HK: moeten we hier niet altijd de upstream kb pakken (af van sign(ked(k))?
               kbi = ln(2, Lf)
               kcdam(1, k) = kb
               kcdam(2, k) = kbi
               kcdam(3, k) = Lf

               call setfixedweirscheme3onlink(Lf)

            end do

         end do

         do n = 1, ncdamsg ! and now add it (poly_tim xys have just been prepared in separate loop)
            str_ptr => strs_ptr%child_nodes(cdamidx(n))%node_ptr

            strid = ' '
            call prop_get(str_ptr, '', 'id', strid, success)
            cdam_ids(n) = strid

            plifile = ' '
            call prop_get(str_ptr, '', 'polylinefile', plifile) ! TODO: Remove? This plifile is nowhere used below
            call resolvePath(plifile, md_structurefile_dir)

            rec = ' '
            key = 'crest_level'
            call read_property(strs_ptr%child_nodes(cdamidx(n))%node_ptr, trim(key), rec, tmpval, is_double, strid, successloc)
            if (.not. successloc) then
               write (msgbuf, '(a,a,a)') 'Field ''crest_level'' not available for damlevel ''', trim(strid), '''.'
               call msg_flush()
               ! consider all fields optional for now.
               cycle
            end if
            if (is_double) then
               ! Constant value for always, set it now already.
               zcdam(n) = tmpval
            else
               if (trim(rec) == 'REALTIME') then
                  success = .true.
                  ! zcdam should be filled via DLL's API
                  write (msgbuf, '(a,a,a)') 'Control for damlevel ''', trim(strid), ''' set to REALTIME.'
                  call dbg_flush()
               else
                  qid = 'damlevel'
                  fnam = trim(rec)
                  call resolvePath(fnam, md_structurefile_dir)
                  if (index(trim(fnam)//'|', '.tim|') > 0) then
                     ! Time-interpolated value will be placed in zcdam(n) when calling ec_gettimespacevalue.
                     success = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, fnam, uniform, spaceandtime, 'O', targetIndex=n)
                  end if
                  if (index(trim(fnam)//'|', '.cmp|') > 0) then
                     ! Evaluated harmonic signals value will be placed in zcdam(n) when calling ec_gettimespacevalue.
                     success = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, fnam, fourier, justupdate, 'O', targetIndex=n)
                  end if
               end if
            end if

         end do
      end if

!
! pumps, including staged pumps
!
      if (npumpsg > 0) then
         if (allocated(qpump)) then
            deallocate (qpump)
         end if
         if (allocated(pump_ids)) then
            deallocate (pump_ids)
         end if
         allocate (pump_ids(npumpsg))
         allocate (qpump(npumpsg), stat=ierr)
         call aerr('qpump(npumpsg)', ierr, npumpsg)
         qpump = 0.0_dp

         if (npump > 0) then
            if (allocated(kpump)) then
               deallocate (kpump)
            end if
            allocate (kpump(3, npump), stat=ierr)
            call aerr('kpump(3,npump)', ierr, npump * 3)
            kpump = 0
         end if

         kx = 1

         do n = 1, npumpsg

            do k = L1pumpsg(n), L2pumpsg(n)
               L = kep(k)
               Lf = abs(L)
               if (L > 0) then
                  kb = ln(1, Lf)
                  kbi = ln(2, Lf)
               else
                  kb = ln(2, Lf)
                  kbi = ln(1, Lf)
               end if
               kpump(1, k) = kb
               kpump(2, k) = kbi
               kpump(3, k) = L ! f
            end do
         end do

         nPumpsWithLevels = 0

         if (allocated(pumpsWithLevels)) then
            deallocate (pumpsWithLevels)
         end if
         allocate (pumpsWithLevels(npumpsg))
         pumpsWithLevels = -1
         if (allocated(waterLevelsPumpLeft)) then
            deallocate (waterLevelsPumpLeft)
         end if
         allocate (waterLevelsPumpLeft(npumpsg))
         waterLevelsPumpLeft = 0.0_dp
         if (allocated(waterLevelsPumpRight)) then
            deallocate (waterLevelsPumpRight)
         end if
         allocate (waterLevelsPumpRight(npumpsg))
         waterLevelsPumpRight = 0.0_dp
         if (allocated(pumpAveraging)) then
            deallocate (pumpAveraging)
         end if
         allocate (pumpAveraging(2, npumpsg))
         pumpAveraging = 0.0_dp
         ! initialize
         pumpsWithLevels = -1
         do n = 1, npumpsg ! and now add it (poly_tim xys have just been prepared in separate loop)

            str_ptr => strs_ptr%child_nodes(pumpidx(n))%node_ptr

            ! read the id first
            strid = ' '
            call prop_get(str_ptr, '', 'id', strid, success)
            pump_ids(n) = strid

            ! read the type
            strtype = ' '
            call prop_get(str_ptr, '', 'type', strtype, success)
            istrtype = getStructype_from_string(strtype)

            ! Do a try-read to determine whether this is a staged flow1d pump. If not, just continue (capacity is enough then).
            call prop_get(str_ptr, 'structure', 'numStages', num_stages, success)
            if (success .and. num_stages > 0) then
               ! flow1d_io library: add and read SOBEK pump
               ! just use the first link of the the structure (the network%sts%struct(istrtmp)%link_number  is not used in computations)
               if (L1pumpsg(n) <= L2pumpsg(n)) then
                  istrtmp = hashsearch(network%sts%hashlist_pump, strid)
                  if (istrtmp == -1) then
                     k = L1pumpsg(n)
                     istrtmp = addStructure(network%sts, kpump(1, k), kpump(2, k), abs(kpump(3, k)), -1, "", strid, istrtype)
                     call readPump(network%sts%struct(istrtmp)%pump, str_ptr, strid, network%forcinglist, success)
                  end if
               end if
            else
               success = .false.
            end if

            ! mapping for qpump array
            if (success) then
               nPumpsWithLevels = nPumpsWithLevels + 1
               pumpsWithLevels(n) = istrtmp
            end if

            if (.not. success) then ! Original pump code, with only a capacity.

               plifile = ' '
               call prop_get(str_ptr, '', 'polylinefile', plifile) ! TODO: Remove? This plifile is nowhere used below
               call resolvePath(plifile, md_structurefile_dir)

               rec = ' '
               key = 'capacity'
               call read_property(strs_ptr%child_nodes(pumpidx(n))%node_ptr, trim(key), rec, tmpval, is_double, strid, successloc)
               if (.not. successloc) then
                  write (msgbuf, '(a,i0,a)') 'Required field '//trim(key)//' missing for pump #', n, '.'
                  call warn_flush()
               end if
               if (is_double) then
                  ! Constant value for always, set it now already.
                  qpump(n) = tmpval
                  success = .true.
               else
                  if (trim(rec) == 'REALTIME') then
                     success = .true.
                     ! zgate should be filled via DLL's API
                     write (msgbuf, '(a,a,a)') 'Control for pump ''', trim(strid), ''' set to REALTIME.'
                     call dbg_flush()
                  else
                     qid = 'pump'
                     fnam = trim(rec)
                     call resolvePath(fnam, md_structurefile_dir)
                     if (index(trim(fnam)//'|', '.tim|') > 0) then
                        ! Time-interpolated value will be placed in qpump(n) when calling ec_gettimespacevalue.
                        success = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, fnam, uniform, spaceandtime, 'O', targetIndex=n)
                        if (.not. success) then
                           message = dumpECMessageStack(LEVEL_WARN, callback_msg)
                           call qnerror(message, ' for ', strid)
                        end if
                     end if
                     if (index(trim(fnam)//'|', '.cmp|') > 0) then
                        ! Evaluated harmonic signals value will be placed in qpump(n) when calling ec_gettimespacevalue.
                        success = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, fnam, fourier, justupdate, 'O', targetIndex=n)
                        if (.not. success) then
                           message = dumpECMessageStack(LEVEL_WARN, callback_msg)
                           call qnerror(message, ' for ', strid)
                        end if
                     end if
                  end if
               end if
            end if
         end do
      end if

!
! dambreak
!
      call update_dambreak_administration_old(dambridx, lftopol)

      if (istat == DFM_NOERR) then
         status = .true.
      else
         status = .false.
      end if

! Fill geometry arrays for structures
      if (jahisweir > 0 .and. network%sts%numWeirs > 0) then
         call fill_geometry_arrays_structure(ST_WEIR, network%sts%numWeirs, nNodesWeir, nodeCountWeir, geomXWeir, geomYWeir)
      end if
      if (jahiscgen > 0 .and. network%sts%numGeneralStructures > 0) then
         call fill_geometry_arrays_structure(ST_GENERAL_ST, network%sts%numGeneralStructures, nNodesGenstru, nodeCountGenstru, geomXGenstru, geomYGenstru)
      end if
      if (jahisorif > 0 .and. network%sts%numOrifices > 0) then
         call fill_geometry_arrays_structure(ST_ORIFICE, network%sts%numOrifices, nNodesOrif, nodeCountOrif, geomXOrif, geomYOrif)
      end if
      if (jahisorif > 0 .and. network%sts%numOrifices > 0) then
         call fill_geometry_arrays_structure(ST_ORIFICE, network%sts%numOrifices, nNodesOrif, nodeCountOrif, geomXOrif, geomYOrif)
      end if
      if (jahisuniweir > 0 .and. network%sts%numUniWeirs > 0) then
         call fill_geometry_arrays_structure(ST_UNI_WEIR, network%sts%numuniweirs, nNodesUniweir, nodeCountUniweir, geomXUniweir, geomYUniweir)
      end if
      if (jahisculv > 0 .and. network%sts%numculverts > 0) then
         call fill_geometry_arrays_structure(ST_CULVERT, network%sts%numculverts, nNodesCulv, nodeCountCulv, geomXCulv, geomYCulv)
      end if
      if (jahispump > 0 .and. network%sts%numPumps > 0) then
         call fill_geometry_arrays_structure(ST_PUMP, network%sts%numPumps, nNodesPump, nodeCountPump, geomXPump, geomYPump)
      end if
      if (jahisbridge > 0 .and. network%sts%numBridges > 0) then
         call fill_geometry_arrays_structure(ST_BRIDGE, network%sts%numBridges, nNodesBridge, nodeCountBridge, geomXBridge, geomYBridge)
      end if
      if (jahislongculv > 0 .and. nlongculverts > 0) then
         call fill_geometry_arrays_structure(ST_LONGCULVERT, nlongculverts, nNodesLongCulv, nodeCountLongCulv, geomXLongCulv, geomYLongCulv)
      end if

   end function flow_init_structurecontrol_old

   !> Set teta to 1 for all links that are connected to an upstream or downstream node of a general structure link.
   subroutine set_teta_of_1_to_neighbours(links)
      use m_flowgeom, only: nd, ln, teta

      integer, dimension(:), intent(in) :: links !< flow links on hydraulic structures.

      integer :: i, nn, n12, kk

      do i = 1, ubound(links, 1)
         do nn = 1, 2
            n12 = ln(nn, abs(links(i)))
            do kk = 1, nd(n12)%lnx
               teta(abs(nd(n12)%ln(kk))) = 1.0_dp
            end do
         end do
      end do

   end subroutine set_teta_of_1_to_neighbours

end submodule flow_init_structurecontrol_implementation
