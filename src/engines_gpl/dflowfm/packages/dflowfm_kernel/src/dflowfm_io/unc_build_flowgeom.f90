module unc_build_flowgeom
   implicit none(type, external)
contains

!> Fills the given arrays for all edges in the 2D mesh, ordered as follows: first internal flow links, then boundary flow links, then closed net links.
   subroutine get_2d_edge_data(edge_nodes, edge_faces, edge_type, xue, yue, edge_mapping_table, reverse_edge_mapping_table)
      use network_data
      use m_flowgeom
      use io_ugrid, only: UG_EDGETYPE_INTERNAL, UG_EDGETYPE_BND, UG_EDGETYPE_INTERNAL_CLOSED, UG_EDGETYPE_BND_CLOSED
      implicit none

      integer, intent(out) :: edge_nodes(:, :) !< Edge node connectivity array to be filled.
      integer, pointer :: edge_faces(:, :) !< Edge face connectivity array to be filled (uses -999 as fill value).
      integer, intent(out) :: edge_type(:) !< Edge type array to be filled.
      real(kind=dp), intent(out) :: xue(:) !< Edge x coordinate array to be filled.
      real(kind=dp), intent(out) :: yue(:) !< Edge y coordinate array to be filled.
      integer, optional, intent(out) :: edge_mapping_table(:) !< Mapping from original edges to ordered edges (first flow links, then closed edges). To be filled if present.
      integer, optional, intent(out) :: reverse_edge_mapping_table(:) !< Mapping from ordered edges (first flow links, then closed edges) to original edges. To be filled if present.

      integer :: is, i, L, Lf !< Counters.
      logical :: is_lne2ln_allocated, is_edge_faces_associated, is_edge_mapping_table_present, is_reverse_edge_mapping_table_present

      is_lne2ln_allocated = allocated(lne2ln)
      is_edge_faces_associated = associated(edge_faces)
      is_edge_mapping_table_present = present(edge_mapping_table)
      is_reverse_edge_mapping_table_present = present(reverse_edge_mapping_table)

      ! set LC mask to 0
      LC = 0

      ! Write all edges that are 2D internal flow links.
      i = 0
      ! Lf is flow link number.
      do Lf = lnx1d + 1, lnxi

         L = ln2lne(Lf)
         if (LC(L) /= 0) then
            cycle
         end if
         LC(L) = 1

         ! i is edge number.
         i = i + 1

         edge_nodes(1:2, i) = lncn(1:2, Lf)
         if (is_edge_faces_associated) then
            edge_faces(1:2, i) = ln(1:2, Lf)
         end if

         edge_type(i) = UG_EDGETYPE_INTERNAL
         xue(i) = xu(Lf)
         yue(i) = yu(Lf)

         if (is_edge_mapping_table_present) then
            edge_mapping_table(L - numl1d) = i
         end if
         if (is_reverse_edge_mapping_table_present) then
            reverse_edge_mapping_table(i) = L - numl1d
         end if
      end do

      ! Write all edges that are 2D boundary flow links.
      ! Lf is flow link number.
      do Lf = lnx1Db + 1, lnx

         L = ln2lne(Lf)
         if (LC(L) /= 0) then
            cycle
         end if
         LC(L) = 1

         ! i is edge number.
         i = i + 1

         edge_nodes(1:2, i) = lncn(1:2, Lf)
         if (is_edge_faces_associated) then
            ! NOTE: the internal face intentionally gets placed on index 1,
            ! even though the flow link has it on index 2 by definition.
            edge_faces(1, i) = ln(2, Lf)
            edge_faces(2, i) = -999
         end if

         edge_type(i) = UG_EDGETYPE_BND
         xue(i) = xu(Lf)
         yue(i) = yu(Lf)

         if (is_edge_mapping_table_present) then
            edge_mapping_table(L - numl1d) = i
         end if
         if (is_reverse_edge_mapping_table_present) then
            reverse_edge_mapping_table(i) = L - numl1d
         end if
      end do

      ! Write all remaining edges, which are closed.
      ! Loop over all 2D net links, which includes both 2D flow links and closed 2D net links.
      ! L is net link number
      if (is_lne2ln_allocated) then
         do L = NUML1D + 1, NUML

            ! Lf is flow link number.
            Lf = lne2ln(L)

            if (Lf <= 0) then ! If this net link does not have a flow link (i.e. closed net link).

               if (LC(L) /= 0) then
                  cycle
               end if
               LC(L) = 1

               ! i is edge number.
               i = i + 1
               edge_nodes(1:2, i) = KN(1:2, L)
               if (lnn(L) < 2) then
                  edge_type(i) = UG_EDGETYPE_BND_CLOSED
               else if (kn(3, L) == 0) then
                  edge_type(i) = UG_EDGETYPE_INTERNAL_CLOSED
               end if

               if (is_edge_faces_associated) then
                  do is = 1, 2
                     if (lne(is, L) > 0) then
                        edge_faces(is, i) = lne(is, L)
                     else
                        edge_faces(is, i) = -999
                     end if
                  end do
               end if

               ! Edge coordinate is in the middle of the net link.
               xue(i) = 0.5_dp * (xk(kn(1, L)) + xk(kn(2, L)))
               yue(i) = 0.5_dp * (yk(kn(1, L)) + yk(kn(2, L)))

               if (is_edge_mapping_table_present) then
                  edge_mapping_table(L - numl1d) = i
               end if
               if (is_reverse_edge_mapping_table_present) then
                  reverse_edge_mapping_table(i) = L - numl1d
               end if
            end if

         end do
      end if

      ! restore the mask
      LC = 0

   end subroutine get_2d_edge_data

     subroutine build_flowgeom_2d(flowgeom, jabndnd, cell_mask)
      use m_flowgeom, only: ndxi, ndx, ndx2d, ndx1db, nd, xz, yz, lnx1d, lnxi, lnx1db, lnx, t_fm_flowgeom
      use network_data, only: xk, yk, zk, kc, numk, numl, numl1d
      use m_missing, only: dmiss
      use m_alloc, only: realloc, reallocP
      use m_save_ugrid_state, only: mesh2dname
      use precision, only: dp
      implicit none

      type(t_fm_flowgeom), intent(inout) :: flowgeom !< Populated flow geometry object.
      integer, intent(in) :: jabndnd !< Include boundary cells (1) or not (0).
      logical, intent(in), optional :: cell_mask(:) !< Selection mask over ndx2d cells; if absent, all cells are included.

      integer :: numl2d, numNodes, numFace, numEdge, ndxndxi
      integer :: i, l, n, nn, nnSize, netNodeReMappedIndex
      logical :: use_mask

      integer, allocatable :: tmp_edge_nodes(:, :), tmp_edge_type(:)
      integer, pointer     :: tmp_edge_faces(:, :)
      real(kind=dp), allocatable :: tmp_xue(:), tmp_yue(:)
      logical, allocatable :: edge_included(:)
      integer, allocatable :: edge_compact(:) !< full-grid edge index -> output index (0 = excluded)
      integer, allocatable :: face_compact(:) !< full-grid face index -> output index (0 = excluded)

      use_mask = present(cell_mask)

      if (jabndnd == 1) then
         ndxndxi = ndx
      else
         ndxndxi = ndxi
      end if

      numl2d = numl - numl1d

      ! =========================================================
      ! Phase 1: Resolve output set — build index maps
      ! =========================================================
      ! face_map(i)    = full-grid face index for output face i
      ! face_compact(n)= output face index for full-grid face n  (0 = excluded)
      ! edge_map(i)    = full-grid edge index for output edge i
      ! edge_compact(l)= output edge index for full-grid edge l  (0 = excluded)
      !
      ! In the unmasked case both maps are the identity; Phase 2 is then identical.

      if (use_mask) then
         numFace = count(cell_mask)
      else
         numFace = ndx2d
      end if

      call realloc(flowgeom%face_map, numFace, keepExisting=.false., fill=0)
      allocate(face_compact(ndx2d))
      face_compact = 0

      if (use_mask) then
         n = 0
         do i = 1, ndx2d
            if (cell_mask(i)) then
               n = n + 1
               flowgeom%face_map(n) = i
               face_compact(i)      = n
            end if
         end do
      else
         do i = 1, numFace
            flowgeom%face_map(i) = i
            face_compact(i)      = i
         end do
      end if

      ! Retrieve full-grid edge data; needed to apply the face mask to edges.
      allocate(tmp_edge_nodes(2, numl2d), tmp_edge_faces(2, numl2d), tmp_edge_type(numl2d))
      allocate(tmp_xue(numl2d), tmp_yue(numl2d))
      tmp_edge_nodes = -999
      tmp_edge_faces = -999

      call get_2d_edge_data(tmp_edge_nodes, tmp_edge_faces, tmp_edge_type, tmp_xue, tmp_yue)

      ! Include an edge if at least one of its adjacent faces is in the output set.
      allocate(edge_included(numl2d))
      if (use_mask) then
         do l = 1, numl2d
            edge_included(l) = (tmp_edge_faces(1, l) > 0 .and. face_compact(tmp_edge_faces(1, l)) > 0) .or. &
                                (tmp_edge_faces(2, l) > 0 .and. face_compact(tmp_edge_faces(2, l)) > 0)
         end do
      else
         edge_included = .true.
      end if

      numEdge = count(edge_included)

      call realloc(flowgeom%edge_map, numEdge, keepExisting=.false., fill=0)
      allocate(edge_compact(numl2d))
      edge_compact = 0
      n = 0
      do l = 1, numl2d
         if (edge_included(l)) then
            n = n + 1
            flowgeom%edge_map(n) = l
            edge_compact(l)      = n
         end if
      end do

      ! =========================================================
      ! Phase 2: Build geometry — common path through the maps
      ! =========================================================

      call realloc(flowgeom%edge_type,           numEdge,        fill=-999,  keepExisting=.false.)
      call reallocP(flowgeom%mesh2d%edge_nodes, [2, numEdge],    fill=-999,  keepExisting=.false.)
      call reallocP(flowgeom%mesh2d%edge_faces, [2, numEdge],    fill=-999)
      call reallocP(flowgeom%mesh2d%edgex,       numEdge,        fill=dmiss, keepExisting=.false.)
      call reallocP(flowgeom%mesh2d%edgey,       numEdge,        fill=dmiss, keepExisting=.false.)
      call reallocP(flowgeom%mesh2d%nodex,       numk,           fill=dmiss, keepExisting=.false.)
      call reallocP(flowgeom%mesh2d%nodey,       numk,           fill=dmiss, keepExisting=.false.)
      call reallocP(flowgeom%mesh2d%nodez,       numk,           fill=dmiss, keepExisting=.false.)

      ! facex/facey: owned memory when masked (non-contiguous gather), pointer slice otherwise.
      if (use_mask) then
         allocate(flowgeom%mesh2d%facex(numFace))
         allocate(flowgeom%mesh2d%facey(numFace))
         do i = 1, numFace
            flowgeom%mesh2d%facex(i) = xz(flowgeom%face_map(i))
            flowgeom%mesh2d%facey(i) = yz(flowgeom%face_map(i))
         end do
      else
         flowgeom%mesh2d%facex => xz(1:ndx2d)
         flowgeom%mesh2d%facey => yz(1:ndx2d)
      end if

      numNodes = 0
      do i = 1, numFace
         numNodes = max(numNodes, size(nd(flowgeom%face_map(i))%nod))
      end do
      call reallocP(flowgeom%mesh2d%face_nodes, [numNodes, numFace], fill=-999)

      associate (edge_nodes => flowgeom%mesh2d%edge_nodes, &
                 edge_faces => flowgeom%mesh2d%edge_faces, &
                 face_nodes => flowgeom%mesh2d%face_nodes, &
                 edge_type  => flowgeom%edge_type,          &
                 xue => flowgeom%mesh2d%edgex, yue => flowgeom%mesh2d%edgey, &
                 x2dn => flowgeom%mesh2d%nodex, y2dn => flowgeom%mesh2d%nodey, z2dn => flowgeom%mesh2d%nodez)

         ! Gather edge data from the full grid into the output arrays.
         do i = 1, numEdge
            l = flowgeom%edge_map(i)
            edge_nodes(:, i) = tmp_edge_nodes(:, l)
            edge_faces(:, i) = tmp_edge_faces(:, l)
            edge_type(i)     = tmp_edge_type(l)
            xue(i)           = tmp_xue(l)
            yue(i)           = tmp_yue(l)
         end do

         ! Remap edge_faces from full-grid face indices to output face indices.
         ! Excluded adjacent faces (face_compact == 0) become -999.
         ! In the unmasked case this is skipped; full-grid indices == output indices already.
         if (use_mask) then
            do i = 1, numEdge
               if (edge_faces(1, i) > 0) edge_faces(1, i) = face_compact(edge_faces(1, i))
               if (edge_faces(2, i) > 0) edge_faces(2, i) = face_compact(edge_faces(2, i))
               if (edge_faces(1, i) == 0) edge_faces(1, i) = -999
               if (edge_faces(2, i) == 0) edge_faces(2, i) = -999
            end do
         end if

         ! Remap net nodes referenced by included edges to a compact output set.
         kc = 0
         netNodeReMappedIndex = 0
         do i = 1, numEdge
            nn = edge_nodes(1, i)
            if (nn > 0 .and. kc(nn) == 0) then
               netNodeReMappedIndex = netNodeReMappedIndex + 1
               x2dn(netNodeReMappedIndex) = xk(nn)
               y2dn(netNodeReMappedIndex) = yk(nn)
               z2dn(netNodeReMappedIndex) = zk(nn)
               kc(nn) = netNodeReMappedIndex
            end if
            nn = edge_nodes(2, i)
            if (nn > 0 .and. kc(nn) == 0) then
               netNodeReMappedIndex = netNodeReMappedIndex + 1
               x2dn(netNodeReMappedIndex) = xk(nn)
               y2dn(netNodeReMappedIndex) = yk(nn)
               z2dn(netNodeReMappedIndex) = zk(nn)
               kc(nn) = netNodeReMappedIndex
            end if
         end do

         do i = 1, numEdge
            if (edge_nodes(1, i) > 0) edge_nodes(1, i) = kc(edge_nodes(1, i))
            if (edge_nodes(2, i) > 0) edge_nodes(2, i) = kc(edge_nodes(2, i))
         end do

         do i = 1, numFace
            n = flowgeom%face_map(i)
            nnSize = size(nd(n)%nod)
            do l = 1, nnSize
               nn = nd(n)%nod(l)
               if (nn > 0) face_nodes(l, i) = kc(nn)
            end do
         end do

         call realloc(flowgeom%node_map, netNodeReMappedIndex, keepExisting=.false., fill=0)
         do nn = 1, numk
            if (kc(nn) > 0) flowgeom%node_map(kc(nn)) = nn
         end do

         flowgeom%ndx2d  = ndx2d;  flowgeom%ndxi   = ndxi
         flowgeom%ndx1db = ndx1db; flowgeom%ndx    = ndx
         flowgeom%lnx1d  = lnx1d;  flowgeom%lnxi   = lnxi
         flowgeom%lnx1db = lnx1db; flowgeom%lnx    = lnx

         flowgeom%mesh2d%meshName        = mesh2dname
         flowgeom%mesh2d%dim             = 2
         flowgeom%mesh2d%start_index     = 1
         flowgeom%mesh2d%numNode         = netNodeReMappedIndex
         flowgeom%mesh2d%numEdge         = numEdge
         flowgeom%mesh2d%numFace         = numFace
         flowgeom%mesh2d%maxNumFaceNodes = numNodes

      end associate

   end subroutine build_flowgeom_2d

!> Constructs the 1D mesh geometry object, decoupled from direct m_flowgeom/network_data usage at the call site.
!! Populates flowgeom%mesh1D (coordinates, connectivity) and the 1D-specific mapping arrays
!! (edgetoln, contactstoln, contacts, contacttype). The writer unc_write_1D_flowgeom_ugrid
!! then calls ug_write_mesh_arrays using these plus the branch-metadata from m_save_ugrid_state.
   subroutine build_flowgeom_1d(flowgeom, jabndnd)
      use m_flowgeom, only: ndxi, ndx, ndx2d, ndx1db, nd, xz, yz, &
                            lnx1d, lnxi, lnx1db, lnx, ln, kcu, xu, yu, ln2lne, t_fm_flowgeom
      use m_save_ugrid_state, only: mesh1dname, meshgeom1d
      use network_data, only: nodepermutation, Lperm
      use m_missing, only: dmiss
      use m_alloc, only: realloc, reallocP
      use precision, only: dp
      implicit none

      type(t_fm_flowgeom), intent(inout) :: flowgeom !< Populated 1D geometry object.
      integer, intent(in) :: jabndnd !< Include boundary nodes (1) or not (0).

      integer :: ndx1d, n1d_write, last_1d, n1dedges, n1d2dcontacts
      integer :: n, L, Li, k1, L1

      ! --- Counts ---
      ndx1d = ndxi - ndx2d
      if (jabndnd == 1) then
         n1d_write = ndx1db - ndx2d
         last_1d = ndx1db
      else
         n1d_write = ndx1d
         last_1d = ndxi
      end if

      n1dedges = 0
      n1d2dcontacts = 0
      do L = 1, lnx1d
         if (kcu(L) == 1) then
            n1dedges = n1dedges + 1
         else if (kcu(L) == 3 .or. kcu(L) == 4 .or. kcu(L) == 5 .or. kcu(L) == 7) then
            n1d2dcontacts = n1d2dcontacts + 1
         end if
      end do
      if (jabndnd == 1) then
         n1dedges = n1dedges + (lnx1db - lnxi)
      end if

      ! --- Allocate mesh1D geometry arrays ---
      call reallocP(flowgeom%mesh1D%nodex, n1d_write, fill=dmiss, keepExisting=.false.)
      call reallocP(flowgeom%mesh1D%nodey, n1d_write, fill=dmiss, keepExisting=.false.)
      call reallocP(flowgeom%mesh1D%edge_nodes, [2, n1dedges], fill=-999, keepExisting=.false.)
      call reallocP(flowgeom%mesh1D%edgex, n1dedges, fill=dmiss, keepExisting=.false.)
      call reallocP(flowgeom%mesh1D%edgey, n1dedges, fill=dmiss, keepExisting=.false.)
      call realloc(flowgeom%edgetoln, n1dedges, fill=0, keepExisting=.false.)
      call realloc(flowgeom%contactstoln, n1d2dcontacts, fill=0, keepExisting=.false.)
      call realloc(flowgeom%contacts, [2, n1d2dcontacts], fill=-999, keepExisting=.false.)
      call realloc(flowgeom%contacttype, n1d2dcontacts, fill=0, keepExisting=.false.)

      ! Branch topology remapping arrays (only when branch/network topology was read).
      if (associated(meshgeom1d%ngeopointx)) then
         call reallocP(flowgeom%mesh1D%nodebranchidx, n1d_write, keepExisting=.false.)
         call reallocP(flowgeom%mesh1D%nodeoffsets, n1d_write, keepExisting=.false.)
         call reallocP(flowgeom%mesh1D%edgebranchidx, n1dedges, keepExisting=.false.)
         call reallocP(flowgeom%mesh1D%edgeoffsets, n1dedges, keepExisting=.false.)
      end if

      ! --- Fill node coordinates (and branch topology remapping) ---
      do n = 1, n1d_write
         flowgeom%mesh1D%nodex(n) = xz(ndx2d + n)
         flowgeom%mesh1D%nodey(n) = yz(ndx2d + n)

         if (n <= ndx1d .and. associated(meshgeom1d%ngeopointx)) then
            k1 = nodePermutation(nd(ndx2d + n)%nod(1))
            if (size(meshgeom1d%nodeidx_inverse) > 0) then
               k1 = meshgeom1d%nodeidx_inverse(k1)
            end if
            flowgeom%mesh1D%nodebranchidx(n) = meshgeom1d%nodebranchidx(k1)
            flowgeom%mesh1D%nodeoffsets(n) = meshgeom1d%nodeoffsets(k1)
         end if
      end do

      ! --- Fill edges and contacts ---
      n1dedges = 0
      n1d2dcontacts = 0
      do Li = 1, lnx1d + (lnx1db - lnxi)
         if (Li <= lnx1d) then
            L = Li
         else if (n1d_write == ndx1d) then ! not writing boundary nodes: skip boundary links
            exit
         else
            L = lnxi + (Li - lnx1d)
         end if

         if (abs(kcu(L)) == 1) then
            n1dedges = n1dedges + 1
            flowgeom%mesh1D%edge_nodes(1:2, n1dedges) = ln(1:2, L) - ndx2d
            flowgeom%mesh1D%edgex(n1dedges) = xu(L)
            flowgeom%mesh1D%edgey(n1dedges) = yu(L)
            flowgeom%edgetoln(n1dedges) = L
            if (associated(meshgeom1d%ngeopointx)) then
               L1 = Lperm(ln2lne(L))
               if (L1 > size(meshgeom1d%edgebranchidx)) L1 = n1dedges
               flowgeom%mesh1D%edgebranchidx(n1dedges) = meshgeom1d%edgebranchidx(L1)
               flowgeom%mesh1D%edgeoffsets(n1dedges) = meshgeom1d%edgeoffsets(L1)
            end if

         else if (kcu(L) == 3 .or. kcu(L) == 4 .or. kcu(L) == 5 .or. kcu(L) == 7) then
            n1d2dcontacts = n1d2dcontacts + 1
            flowgeom%contactstoln(n1d2dcontacts) = L
            flowgeom%contacttype(n1d2dcontacts) = kcu(L)
            if (ln(1, L) > ndx2d) then ! first node is 1D
               flowgeom%contacts(1, n1d2dcontacts) = ln(1, L) - ndx2d
               flowgeom%contacts(2, n1d2dcontacts) = ln(2, L)
            else ! second node is 1D
               flowgeom%contacts(1, n1d2dcontacts) = ln(2, L) - ndx2d
               flowgeom%contacts(2, n1d2dcontacts) = ln(1, L)
            end if
         end if
      end do

      ! --- Populate t_ug_meshgeom scalars ---
      flowgeom%mesh1D%meshName = mesh1dname
      flowgeom%mesh1D%dim = 1
      flowgeom%mesh1D%start_index = 1
      flowgeom%mesh1D%numNode = n1d_write
      flowgeom%mesh1D%numEdge = n1dedges
      flowgeom%mesh1D%numFace = 0
      flowgeom%mesh1D%maxNumFaceNodes = 0

      ! --- Populate t_fm_flowgeom scalars ---
      flowgeom%n1d2dcontacts = n1d2dcontacts
      flowgeom%ndx2d = ndx2d
      flowgeom%ndxi = ndxi
      flowgeom%ndx1db = ndx1db
      flowgeom%ndx = ndx
      flowgeom%lnx1d = lnx1d
      flowgeom%lnxi = lnxi
      flowgeom%lnx1db = lnx1db
      flowgeom%lnx = lnx

   end subroutine build_flowgeom_1d

!> Builds the complete flow geometry object for both 1D and 2D meshes.
!! Delegates to build_flowgeom_2d and build_flowgeom_1d in order; the call site
!! owns the t_fm_flowgeom object and receives a fully populated result.
   function build_flowgeom(jabndnd) result(flowgeom)
      use m_flowgeom, only: t_fm_flowgeom
      implicit none

      type(t_fm_flowgeom) :: flowgeom !< Populated geometry object for both 1D and 2D meshes.
      integer, intent(in) :: jabndnd !< Include boundary nodes (1) or not (0).

      call build_flowgeom_2d(flowgeom, jabndnd)
      call build_flowgeom_1d(flowgeom, jabndnd)

   end function build_flowgeom

end module
