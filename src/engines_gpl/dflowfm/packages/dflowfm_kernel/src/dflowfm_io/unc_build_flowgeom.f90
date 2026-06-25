module m_unc_flowgeom
   use m_unstruc_netcdf_data, only: t_fm_flowgeom

   implicit none(type, external)

   type(t_fm_flowgeom), public :: flowgeom

contains

!> Returns the output index of a full-grid face in face_map, or -999 if not found.
   pure function find_face_output_index(face_map, face_2d) result(output_idx)
      integer, intent(in) :: face_map(:)
      integer, intent(in) :: face_2d
      integer :: output_idx, i

      output_idx = -999
      do i = 1, size(face_map)
         if (face_map(i) == face_2d) then
            output_idx = i
            return
         end if
      end do
   end function

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

   subroutine build_flowgeom_2d(flowgeom, cell_mask)
      use m_flowgeom, only: ndx2d, nd, xz, yz
      use network_data, only: xk, yk, zk, kc, numk, numl, numl1d
      use m_missing, only: dmiss
      use m_alloc, only: realloc, reallocP
      use m_save_ugrid_state, only: mesh2dname
      use io_ugrid, only: UG_EDGETYPE_INTERNAL, UG_EDGETYPE_BND, UG_EDGETYPE_INTERNAL_CLOSED, UG_EDGETYPE_BND_CLOSED
      use precision, only: dp
      implicit none

      type(t_fm_flowgeom), intent(inout) :: flowgeom !< Populated flow geometry object.
      logical, intent(in), optional :: cell_mask(:) !< Selection mask over ndx2d cells; if absent, all cells are included.

      integer :: numl2d, numNodes, numFace, numEdge
      integer :: i, l, n, nn, nnSize, netNodeReMappedIndex
      logical :: use_mask

      integer, allocatable :: tmp_edge_nodes(:, :), tmp_edge_type(:)
      integer, pointer :: tmp_edge_faces(:, :)
      real(kind=dp), allocatable :: tmp_xue(:), tmp_yue(:)
      logical, allocatable :: edge_included(:)
      integer, allocatable :: inverse_face_map(:) !< full-grid face index -> output index (0 = excluded)

      use_mask = present(cell_mask)

      numl2d = numl - numl1d
      if (use_mask) then
         numFace = count(cell_mask)
      else
         numFace = ndx2d
      end if

      ! Retrieve full-grid edge data; needed to apply the face mask to edges.
      allocate (tmp_edge_nodes(2, numl2d), tmp_edge_faces(2, numl2d), tmp_edge_type(numl2d))
      allocate (tmp_xue(numl2d), tmp_yue(numl2d))
      tmp_edge_nodes = -999
      tmp_edge_faces = -999

      call get_2d_edge_data(tmp_edge_nodes, tmp_edge_faces, tmp_edge_type, tmp_xue, tmp_yue)

      numEdge = numl2d
      call realloc(edge_included, numEdge, keepExisting=.false., fill=.true.)

      ! =========================================================
      ! Build mapping arrays in case of masked output.
      ! =========================================================
      ! face_map(i)    = full-grid face index for output face i
      ! inverse_face_map(n)= output face index for full-grid face n  (0 = excluded)
      ! edge_map(i)    = full-grid edge index for output edge i
      ! inverse_edge_map(l)= output edge index for full-grid edge l  (0 = excluded)
      !
      ! In the unmasked case both maps are the identity; Phase 2 is then identical.

      if (use_mask) then
         call realloc(flowgeom%face_map, numFace, keepExisting=.false., fill=0)
         call realloc(inverse_face_map, ndx2d, keepExisting=.false., fill=0)
         n = 0
         do i = 1, ndx2d
            if (cell_mask(i)) then
               n = n + 1
               flowgeom%face_map(n) = i
               inverse_face_map(i) = n
            end if
         end do

         ! Include an edge if at least one of its adjacent faces is in the output set.
         do l = 1, numl2d
            edge_included(l) = .false.
            if (tmp_edge_faces(1, l) > 0) then
               if (inverse_face_map(tmp_edge_faces(1, l)) > 0) then
                  edge_included(l) = .true.
                  cycle
               end if
            end if
            if (tmp_edge_faces(2, l) > 0) then
               if (inverse_face_map(tmp_edge_faces(2, l)) > 0) edge_included(l) = .true.
            end if
         end do
         numEdge = count(edge_included)

         call realloc(flowgeom%edge_map, numEdge, keepExisting=.false., fill=0)
         n = 0
         do l = 1, numl2d
            if (edge_included(l)) then
               n = n + 1
               flowgeom%edge_map(n) = l
            end if
         end do
      end if

      ! =========================================================
      ! Build geometry
      ! =========================================================

      call realloc(flowgeom%edge_type, numEdge, fill=-999, keepExisting=.false.)
      call reallocP(flowgeom%mesh2d%edge_nodes, [2, numEdge], fill=-999, keepExisting=.false.)
      call reallocP(flowgeom%mesh2d%edge_faces, [2, numEdge], fill=-999)
      call reallocP(flowgeom%mesh2d%edgex, numEdge, fill=dmiss, keepExisting=.false.)
      call reallocP(flowgeom%mesh2d%edgey, numEdge, fill=dmiss, keepExisting=.false.)
      call reallocP(flowgeom%mesh2d%nodex, numk, fill=dmiss, keepExisting=.false.)
      call reallocP(flowgeom%mesh2d%nodey, numk, fill=dmiss, keepExisting=.false.)
      call reallocP(flowgeom%mesh2d%nodez, numk, fill=dmiss, keepExisting=.false.)

      ! facex/facey: owned memory when masked (non-contiguous gather), pointer slice otherwise.
      if (use_mask) then
         allocate (flowgeom%mesh2d%facex(numFace))
         allocate (flowgeom%mesh2d%facey(numFace))
         do i = 1, numFace
            flowgeom%mesh2d%facex(i) = xz(flowgeom%face_map(i))
            flowgeom%mesh2d%facey(i) = yz(flowgeom%face_map(i))
         end do
      else
         flowgeom%mesh2d%facex => xz(1:ndx2d)
         flowgeom%mesh2d%facey => yz(1:ndx2d)
      end if

      !> find max polygon size (up to 6) to allocate face_nodes.
      numNodes = 0
      do i = 1, numFace
         if (use_mask) then
            n = flowgeom%face_map(i)
         else
            n = i
         end if
         numNodes = max(numNodes, size(nd(n)%nod))
      end do
      call reallocP(flowgeom%mesh2d%face_nodes, [numNodes, numFace], fill=-999)

      associate (edge_nodes => flowgeom%mesh2d%edge_nodes, &
                 edge_faces => flowgeom%mesh2d%edge_faces, &
                 face_nodes => flowgeom%mesh2d%face_nodes, &
                 edge_type => flowgeom%edge_type, &
                 xue => flowgeom%mesh2d%edgex, yue => flowgeom%mesh2d%edgey, &
                 x2dn => flowgeom%mesh2d%nodex, y2dn => flowgeom%mesh2d%nodey, z2dn => flowgeom%mesh2d%nodez)

         ! Gather edge data from the full grid into the output arrays.
         do i = 1, numEdge
            if (use_mask) then
               l = flowgeom%edge_map(i)
            else
               l = i
            end if
            edge_nodes(:, i) = tmp_edge_nodes(:, l)
            edge_faces(:, i) = tmp_edge_faces(:, l)
            edge_type(i) = tmp_edge_type(l)
            xue(i) = tmp_xue(l)
            yue(i) = tmp_yue(l)
         end do

         ! Remap edge_faces from full-grid face indices to output face indices.
         ! Excluded adjacent faces (inverse_face_map == 0) become -999.
         ! In the unmasked case this is skipped; full-grid indices == output indices already.
         if (use_mask) then
            do i = 1, numEdge
               if (edge_faces(1, i) > 0) then
                  edge_faces(1, i) = inverse_face_map(edge_faces(1, i))
               end if
               if (edge_faces(2, i) > 0) then
                  edge_faces(2, i) = inverse_face_map(edge_faces(2, i))
               end if
               if (edge_faces(1, i) == 0) then
                  edge_faces(1, i) = -999
               end if
               if (edge_faces(2, i) == 0) then
                  edge_faces(2, i) = -999
               end if
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
            if (edge_nodes(1, i) > 0) then
               edge_nodes(1, i) = kc(edge_nodes(1, i))
            end if
            if (edge_nodes(2, i) > 0) then
               edge_nodes(2, i) = kc(edge_nodes(2, i))
            end if
         end do

         do i = 1, numFace
            if (use_mask) then
               n = flowgeom%face_map(i)
            else
               n = i
            end if
            nnSize = size(nd(n)%nod)
            do l = 1, nnSize
               nn = nd(n)%nod(l)
               if (nn > 0) then
                  face_nodes(l, i) = kc(nn)
               end if
            end do
         end do

         if (use_mask) then
            call realloc(flowgeom%node_map, netNodeReMappedIndex, keepExisting=.false., fill=0)
            do nn = 1, numk
               if (kc(nn) > 0) then
                  flowgeom%node_map(kc(nn)) = nn
               end if
            end do
         end if

         flowgeom%mesh2d%meshName = mesh2dname
         flowgeom%mesh2d%dim = 2
         flowgeom%mesh2d%start_index = 1
         flowgeom%mesh2d%numNode = netNodeReMappedIndex
         flowgeom%mesh2d%numEdge = numEdge
         flowgeom%mesh2d%numFace = numFace
         flowgeom%mesh2d%maxNumFaceNodes = numNodes

         ! Derive 2D edge category counts from edge_type in the output set.
         flowgeom%lnx2d_int = 0
         flowgeom%lnx2d_bnd = 0
         flowgeom%numl2d_closed = 0
         do i = 1, numEdge
            select case (edge_type(i))
            case (UG_EDGETYPE_INTERNAL)
               flowgeom%lnx2d_int = flowgeom%lnx2d_int + 1
            case (UG_EDGETYPE_BND)
               flowgeom%lnx2d_bnd = flowgeom%lnx2d_bnd + 1
            case (UG_EDGETYPE_INTERNAL_CLOSED, UG_EDGETYPE_BND_CLOSED)
               flowgeom%numl2d_closed = flowgeom%numl2d_closed + 1
            end select
         end do

      end associate

   end subroutine build_flowgeom_2d

!> Constructs the 1D mesh geometry object, decoupled from direct m_flowgeom/network_data usage at the call site.
!! Populates flowgeom%mesh1D (coordinates, connectivity) and the 1D-specific mapping arrays
!! (edgetoln, contactstoln, contacts, contacttype). The writer unc_write_1D_flowgeom_ugrid
!! then calls ug_write_mesh_arrays using these plus the branch-metadata from m_save_ugrid_state.
!!
!! When node_mask is provided, only selected internal 1D nodes enter the output set.
!! 1D edges follow an AND rule: both endpoint nodes must be in the output set.
!! Contacts are dropped when either endpoint (1D node or 2D face) is excluded.
!! All stored indices are output-mesh indices, not full-grid indices.
!! flowgeom%node_map_1d(i) gives the full-grid flow node index for output 1D node i.
!! flowgeom%edgetoln(i) gives the full-grid flow link number for output 1D edge i.
   subroutine build_flowgeom_1d(flowgeom, jabndnd, node_mask)
      use m_flowgeom, only: ndxi, ndx2d, ndx1db, nd, xz, yz, &
                            lnx1d, lnxi, lnx1db, ln, kcu, xu, yu, ln2lne
      use m_save_ugrid_state, only: mesh1dname, meshgeom1d
      use network_data, only: Lperm
      use m_missing, only: dmiss
      use m_alloc, only: realloc, reallocP
      use precision, only: dp
      implicit none

      type(t_fm_flowgeom), intent(inout) :: flowgeom
      integer, intent(in) :: jabndnd
      logical, intent(in), optional :: node_mask(:)

      integer :: ndx1d, n1d_write, last_1d, n1dedges, n1d2dcontacts, n1d_out
      integer :: n, L, k1, L1, face_2d, i
      integer :: node_out_1, node_out_2
      logical :: use_mask
      integer, allocatable :: inverse_node_map_1D(:), inverse_face_map(:)
      integer, allocatable :: links_1d(:) !< flat list of all 1D-related flow link indices to process
      integer :: nlinks_1d

      use_mask = present(node_mask)

      ! Reconstruct inverse_face_map from face_map (already populated by build_flowgeom_2d).
      allocate (inverse_face_map(ndx2d))
      if (allocated(flowgeom%face_map)) then
         ! Masked case: face_map is a sparse subset; invert it.
         inverse_face_map = 0
         do i = 1, size(flowgeom%face_map)
            inverse_face_map(flowgeom%face_map(i)) = i
         end do
      else
         ! Unmasked case: identity mapping; all faces are in the output set.
         inverse_face_map = [(i, i=1, ndx2d)]
      end if

      ! --- Resolve ranges ---
      ndx1d = ndxi - ndx2d
      if (jabndnd == 1) then
         n1d_write = ndx1db - ndx2d
         last_1d = ndx1db
      else
         n1d_write = ndx1d
         last_1d = ndxi
      end if

      ! =========================================================
      ! Build 1D node index maps
      ! =========================================================
      ! node_map_1d(i) = full-grid flow node index for output 1D node i
      ! inverse_node_map_1D(n) = output node index for local-1D node n (0 = excluded)
      !
      ! Internal 1D nodes (1:ndx1d) are subject to node_mask.
      ! Boundary 1D nodes (ndx1d+1:n1d_write) are always included when jabndnd == 1.
      ! In the unmasked case node_map and inverse_node_map are trivial mappings.

      allocate (inverse_node_map_1D(n1d_write))
      inverse_node_map_1D = 0

      if (use_mask) then
         n1d_out = count(node_mask(1:ndx1d))
         if (jabndnd == 1) n1d_out = n1d_out + (ndx1db - ndxi)
      else
         n1d_out = n1d_write
      end if

      call realloc(flowgeom%node_map_1d, n1d_out, keepExisting=.false., fill=0)

      ! Internal nodes: subject to mask.
      n = 0
      do i = 1, ndx1d
         if (use_mask) then
            if (.not. node_mask(i)) cycle
         end if
         n = n + 1
         flowgeom%node_map_1d(n) = ndx2d + i
         inverse_node_map_1D(i) = n
      end do

      ! Boundary nodes: always included when jabndnd == 1.
      do i = ndx1d + 1, n1d_write
         n = n + 1
         flowgeom%node_map_1d(n) = ndx2d + i
         inverse_node_map_1D(i) = n
      end do

      ! Build a flat list of all 1D-related flow link indices once:
      !   internal 1D links:  1 .. lnx1d
      !   boundary 1D links:  lnxi+1 .. lnx1db  (only when jabndnd == 1)
      if (jabndnd == 1) then
         nlinks_1d = lnx1d + (lnx1db - lnxi)
      else
         nlinks_1d = lnx1d
      end if
      allocate (links_1d(nlinks_1d))
      links_1d(1:lnx1d) = [(i, i=1, lnx1d)]
      if (jabndnd == 1) then
         links_1d(lnx1d + 1:) = [(lnxi + i, i=1, lnx1db - lnxi)]
      end if
      ! =========================================================
      ! Count edges and contacts in the output set
      ! =========================================================
      ! 1D edge:  include if BOTH endpoint nodes are in the output set (AND rule, per partition.F90).
      ! Contact:  include if BOTH the 1D node AND the 2D face are in their respective output sets.

      n1dedges = 0
      n1d2dcontacts = 0
      do i = 1, nlinks_1d
         L = links_1d(i)

         if (abs(kcu(L)) == 1) then
            node_out_1 = ln(1, L) - ndx2d
            node_out_2 = ln(2, L) - ndx2d
            if (node_out_1 >= 1 .and. node_out_1 <= n1d_write) then
               if (inverse_node_map_1D(node_out_1) > 0) then
                  if (node_out_2 >= 1 .and. node_out_2 <= n1d_write) then
                     if (inverse_node_map_1D(node_out_2) > 0) then
                        n1dedges = n1dedges + 1
                     end if
                  end if
               end if
            end if
         else if (kcu(L) == 3 .or. kcu(L) == 4 .or. kcu(L) == 5 .or. kcu(L) == 7) then
            if (ln(1, L) > ndx2d) then
               node_out_1 = ln(1, L) - ndx2d
               face_2d = ln(2, L)
            else
               node_out_1 = ln(2, L) - ndx2d
               face_2d = ln(1, L)
            end if
            if (node_out_1 >= 1 .and. node_out_1 <= n1d_write) then
               if (inverse_node_map_1D(node_out_1) > 0) then !> valid 1D node
                  if (face_2d >= 1 .and. face_2d <= ndx2d) then
                     if (inverse_face_map(face_2d) > 0) then !> valid 2D face
                        n1d2dcontacts = n1d2dcontacts + 1
                     end if
                  end if
               end if
            end if
         end if
      end do

      ! --- Allocate mesh1D geometry arrays ---
      call reallocP(flowgeom%mesh1D%nodex, n1d_out, fill=dmiss, keepExisting=.false.)
      call reallocP(flowgeom%mesh1D%nodey, n1d_out, fill=dmiss, keepExisting=.false.)
      call reallocP(flowgeom%mesh1D%edge_nodes, [2, n1dedges], fill=-999, keepExisting=.false.)
      call reallocP(flowgeom%mesh1D%edgex, n1dedges, fill=dmiss, keepExisting=.false.)
      call reallocP(flowgeom%mesh1D%edgey, n1dedges, fill=dmiss, keepExisting=.false.)
      call realloc(flowgeom%edgetoln, n1dedges, fill=0, keepExisting=.false.)
      call realloc(flowgeom%contactstoln, n1d2dcontacts, fill=0, keepExisting=.false.)
      call realloc(flowgeom%contacts, [2, n1d2dcontacts], fill=-999, keepExisting=.false.)
      call realloc(flowgeom%contacttype, n1d2dcontacts, fill=0, keepExisting=.false.)

      ! Branch topology arrays (only when branch/network topology was read).
      if (associated(meshgeom1d%ngeopointx)) then
         call reallocP(flowgeom%mesh1D%nodebranchidx, n1d_out, keepExisting=.false.)
         call reallocP(flowgeom%mesh1D%nodeoffsets, n1d_out, keepExisting=.false.)
         call reallocP(flowgeom%mesh1D%edgebranchidx, n1dedges, keepExisting=.false.)
         call reallocP(flowgeom%mesh1D%edgeoffsets, n1dedges, keepExisting=.false.)
      end if

      ! =========================================================
      ! Fill node coordinates (and branch metadata) via node_map_1d
      ! =========================================================
      ! WARNING: this loop juggles FOUR different node-index spaces. Keep them apart:
      !
      !   (1) output 1D node index    n              : 1 .. n1d_out, index into the mesh1D arrays being written.
      !   (2) flow node index         node_map_1d(n) : global flow-node numbering, ordered [2D cells | 1D nodes | bnd].
      !                                                1D nodes start at ndx2d+1, so node_map_1d holds values > ndx2d.
      !       local 1D index          i = node_map_1d(n) - ndx2d
      !   (3) current net-node index  nd(node_map_1d(n))%nod(1) : net node attached to the 1D flow node, in the
      !                                                renumbered (post-setnodadm) network_data numbering.
      !   (4) original net-node index nodePermutation(...)      : net-node numbering as read from *_net.nc (pre-setnodadm).
      !       mesh1D node index       meshgeom1d%nodeidx_inverse(...) : 1 .. numnode index into the branch-metadata
      !                                                arrays (nodebranchidx, nodeoffsets), keyed by the ORIGINAL numbering.
      !
      ! The two-step translation below converts (3) -> (4) -> mesh1D index, because the branch-metadata in
      ! meshgeom1d is stored against the original net.nc numbering:
      !
      !   k1 = nodePermutation(current net node)   ! (3) -> (4): undo setnodadm renumbering
      !   k1 = meshgeom1d%nodeidx_inverse(k1)      ! (4) -> mesh1D node index (1..numnode)
      !
      ! PRECONDITION: nodePermutation must be the real permutation built by setnodadm (gridoperations.F90, only
      ! filled when called with jacrosscheck_ >= 10). If left as the identity, step (3)->(4) is a no-op and
      ! nodeidx_inverse is indexed with a current net-node number, reading uninitialised slots -> garbage indices.

      do n = 1, n1d_out
         i = flowgeom%node_map_1d(n) - ndx2d ! local 1D index
         flowgeom%mesh1D%nodex(n) = xz(flowgeom%node_map_1d(n))
         flowgeom%mesh1D%nodey(n) = yz(flowgeom%node_map_1d(n))

         if (i <= ndx1d .and. associated(meshgeom1d%ngeopointx)) then
            k1 = nd(flowgeom%node_map_1d(n))%nod(1)
            if (associated(meshgeom1d%nodeidx_inverse)) then
               k1 = meshgeom1d%nodeidx_inverse(k1)
            end if
            if (k1 >= 1 .and. k1 <= size(meshgeom1d%nodebranchidx)) then
               flowgeom%mesh1D%nodebranchidx(n) = meshgeom1d%nodebranchidx(k1)
               flowgeom%mesh1D%nodeoffsets(n) = meshgeom1d%nodeoffsets(k1)
            end if
         end if
      end do

      ! =========================================================
      ! Fill edges and contacts
      ! =========================================================
      n1dedges = 0
      n1d2dcontacts = 0
      do i = 1, nlinks_1d
         L = links_1d(i)

         if (abs(kcu(L)) == 1) then !> 1D edges
            node_out_1 = ln(1, L) - ndx2d
            node_out_2 = ln(2, L) - ndx2d
            if (node_out_1 < 1 .or. node_out_1 > n1d_write .or. inverse_node_map_1D(node_out_1) == 0) then
               cycle !> skip edge if node 1 is not in output set
            end if
            if (node_out_2 < 1 .or. node_out_2 > n1d_write .or. inverse_node_map_1D(node_out_2) == 0) then
               cycle !> skip edge if node 2 is not in output set
            end if

            n1dedges = n1dedges + 1
            flowgeom%mesh1D%edge_nodes(1, n1dedges) = inverse_node_map_1D(node_out_1)
            flowgeom%mesh1D%edge_nodes(2, n1dedges) = inverse_node_map_1D(node_out_2)
            flowgeom%mesh1D%edgex(n1dedges) = xu(L)
            flowgeom%mesh1D%edgey(n1dedges) = yu(L)
            flowgeom%edgetoln(n1dedges) = L
            if (associated(meshgeom1d%ngeopointx)) then
               L1 = Lperm(ln2lne(L))
               if (L1 > size(meshgeom1d%edgebranchidx)) then
                  L1 = n1dedges
               end if
               flowgeom%mesh1D%edgebranchidx(n1dedges) = meshgeom1d%edgebranchidx(L1)
               flowgeom%mesh1D%edgeoffsets(n1dedges) = meshgeom1d%edgeoffsets(L1)
            end if

         else if (kcu(L) == 3 .or. kcu(L) == 4 .or. kcu(L) == 5 .or. kcu(L) == 7) then !> 1D-2D contacts
            if (ln(1, L) > ndx2d) then
               node_out_1 = ln(1, L) - ndx2d
               face_2d = ln(2, L)
            else
               node_out_1 = ln(2, L) - ndx2d
               face_2d = ln(1, L)
            end if
            if (node_out_1 >= 1 .and. node_out_1 <= n1d_write) then
               if (inverse_node_map_1D(node_out_1) > 0) then !> valid 1D node
                  if (face_2d >= 1 .and. face_2d <= ndx2d) then
                     if (inverse_face_map(face_2d) > 0) then !> valid 2D face
                        n1d2dcontacts = n1d2dcontacts + 1
                        flowgeom%contactstoln(n1d2dcontacts) = L
                        flowgeom%contacttype(n1d2dcontacts) = kcu(L)
                        flowgeom%contacts(1, n1d2dcontacts) = inverse_node_map_1D(node_out_1)
                        flowgeom%contacts(2, n1d2dcontacts) = inverse_face_map(face_2d)
                     end if
                  end if
               end if
            end if
         end if
      end do

      ! --- Populate t_ug_meshgeom scalars ---
      flowgeom%mesh1D%meshName = mesh1dname
      flowgeom%mesh1D%dim = 1
      flowgeom%mesh1D%start_index = 1
      flowgeom%mesh1D%numNode = n1d_out
      flowgeom%mesh1D%numEdge = n1dedges
      flowgeom%mesh1D%numFace = 0
      flowgeom%mesh1D%maxNumFaceNodes = 0

      ! --- Populate t_fm_flowgeom scalars ---
      flowgeom%n1d2dcontacts = n1d2dcontacts

      ! Total output node count (2D faces + 1D nodes), used as 3D work array loop bound.
      flowgeom%ndx_out = flowgeom%mesh2d%numFace + n1d_out

      if (.not. use_mask) then
         deallocate (flowgeom%node_map_1D) ! was only used for construction, but is trivial map
      end if

   end subroutine build_flowgeom_1d

!> Builds the complete flow geometry object for both 1D and 2D meshes.
!! Delegates to build_flowgeom_2d and build_flowgeom_1d in order; the call site
!! owns the t_fm_flowgeom object and receives a fully populated result.
!! cell_mask covers all internal cells (1:ndxi): the 2D slice (1:ndx2d) is
!! forwarded to build_flowgeom_2d and the 1D slice (ndx2d+1:ndxi) is forwarded
!! to build_flowgeom_1d as the node mask.
   function build_flowgeom(jabndnd, md_polygon_file) result(flowgeom)
      use m_flowgeom, only: ndx2d
      use m_pol_to_cellmask, only: cell_mask_from_polygon_file
      implicit none

      type(t_fm_flowgeom) :: flowgeom !< Populated geometry object for both 1D and 2D meshes.
      integer, intent(in) :: jabndnd !< Include boundary nodes (1) or not (0).
      character(len=*), intent(in), optional :: md_polygon_file !< File containing output polygon (e.g., *_output.pol)
      logical, allocatable :: cell_mask(:) !< Selection mask over all ndxi internal cells; if absent, all cells are included.

      if (present(md_polygon_file)) then
         cell_mask = cell_mask_from_polygon_file(md_polygon_file)
         call build_flowgeom_2d(flowgeom, cell_mask(1:ndx2d))
         call build_flowgeom_1d(flowgeom, jabndnd, cell_mask(ndx2d + 1:))
      else
         call build_flowgeom_2d(flowgeom)
         call build_flowgeom_1d(flowgeom, jabndnd)
      end if

   end function build_flowgeom

end module
