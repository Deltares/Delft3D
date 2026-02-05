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

!> @file
!! @brief Implementation submodule for output restriction to polygon regions.
!! @details This submodule contains the actual implementation of all procedures
!! defined in the m_output_to_polygon module. It provides the core functionality
!! for identifying and managing model elements within polygon boundaries.
!!
!> @brief Submodule implementation for polygon output restriction
!! @ingroup output_to_polygon
submodule(m_output_to_polygon) m_output_to_polygon_sub
   use precision, only: dp
   use messagehandling, only: idLen
   implicit none

   !> @brief Pointer to values array for temporary storage
   !! @details Used internally for array operations and remapping
   real(kind=dp), pointer, dimension(:) :: values

contains

   !> @brief Create and initialize all mask arrays for polygon region
   !! @details This subroutine is the main entry point for setting up polygon-based
   !! output restriction. It allocates and initializes all necessary arrays for
   !! tracking which model elements (cells, links, nodes) fall within the polygon.
   !! If no polygon is defined, it initializes arrays with all elements included.
   !!
   !! The subroutine performs the following operations:
   !! - Allocates memory for all mask and index arrays.
   !! - If polygon is defined: calls specialized routines to find elements inside polygon.
   !! - If polygon is not defined: includes all elements (no masking).
   !! - Sets up connectivity information for links and nodes.
   !!
   !! @param[in,out] this Polygon variables object to initialize.
   module subroutine create_mask_arrays_impl(this)
      use messagehandling, only: msgbuf, fatal_flush
      use m_reapol, only: reapol
      use network_data, only: numL, numl1d, numk, numl, kn
      use geometry_module, only: dbpinpol
      use m_alloc, only: realloc
      use m_flowgeom, only: ndx, lnx, ln, ndxi, ndx2d, ndx1db, lnx1d, lnx1db, lnxi

      class(t_variables_inside_polygon), intent(inout) :: this !< Polygon variables object

      integer :: k

      if (allocated(this%cell_indices)) then
         return
      end if
      this%ndx = ndx
      this%lnx = lnx
      this%ndxi = ndxi
      this%ndx2d = ndx2d
      this%ndx1db = ndx1db
      this%numk = numk
      this%numl = numl

      call allocate_arrays(this)
      this%cells_mask = 0

      if (.not. this%is_defined) then
         ! Set to defaults.
         this%cell_indices = [(k, k=1, ndx)]
         this%cells_mask = [(k, k=1, ndx)]

         this%link_indices = [(k, k=1, lnx)]
         this%links_mask = [(k, k=1, lnx)]
         this%link_to_nodes = ln

         this%netnode_indices = [(k, k=1, numk)]
         this%netnodes_mask = [(k, k=1, numk)]

         this%netlink_indices = [(k, k=1, numL)]
         this%netlinks_mask = [(k, k=1, numL)]
         do k = 1, numL
            this%netlink_to_netnodes(1:2, k) = kn(1:2, k)
         end do

         this%ndxi = ndxi
         this%ndx2d = ndx2d
         this%ndx1db = ndx1db
         this%lnx1d = lnx1d
         this%lnx1db = lnx1db
         this%lnxi = lnxi
         this%numl1d = numl1d

         return
      end if

      associate (cells_mask => this%cells_mask, &
                 links_mask => this%links_mask, &
                 num_vertices_inside => this%lnx, &
                 net_cells_mask => this%netnodes_mask, &
                 net_links_mask => this%netlinks_mask)

         call this%findcells_inside_polygon()

         this%ndxi = get_newindex(cells_mask, ndxi)
         this%ndx2d = get_newindex(cells_mask, ndx2d)
         this%ndx1db = get_newindex(cells_mask, ndx1db)

         call find_vertices_inside_polygon(this)
         this%lnx1d = get_newindex(links_mask, lnx1d)
         this%lnx1db = get_newindex(links_mask, lnx1db)
         this%lnxi = get_newindex(links_mask, lnxi)

         call find_netnodes_inside_polygon(this)
         call find_netlinks_inside_polygon(this)
         this%numl1d = get_newindex(net_links_mask, numl1d)
      end associate

      !> Reallocate arrays to trimmed sizes
      call allocate_arrays(this)

   end subroutine create_mask_arrays_impl

   !> @brief Get the new index for the highest numbered element in mask.
   !! @details This function finds the highest index value in the mask array
   !! up to a specified limit. It's used to determine the maximum valid index
   !! for different types of elements (cells, links, nodes) within the polygon.
   !!
   !! @param[in] mask Mask array containing remapped indices.
   !! @param[in] old_index Maximum index to search up to.
   !! @return new_index Highest index value found, or 0 if none found.
   function get_newindex(mask, old_index) result(new_index)
      integer, intent(in) :: mask(:) !< Mask array.
      integer, intent(in) :: old_index !< Maximum index to search up to.
      integer :: new_index !< Resulting highest index value.

      integer :: k

      new_index = 0
      do k = old_index, 1, -1
         if (mask(k) > 0) then
            new_index = mask(k)
            exit
         end if
      end do
   end function get_newindex

   !> @brief Find all flow cells inside the polygon boundary.
   !! @details This subroutine identifies which flow cells have their centers
   !! located within the specified polygon region. It reads the polygon file,
   !! tests each cell center using point-in-polygon algorithms, and creates
   !! a mask array and index mapping for cells inside the polygon.
   !!
   !! The subroutine:
   !! - Opens and reads the polygon file
   !! - Tests each cell center coordinate against polygon boundary
   !! - Creates mask array with remapped indices for cells inside polygon
   !! - Stores original cell indices for later remapping operations
   !!
   !! @param[in,out] this Polygon variables object containing filename and output arrays
   module subroutine findcells_inside_polygon_impl(this)
      use m_reapol, only: reapol
      use m_polygon, only: npl, xpl, ypl, zpl
      use geometry_module, only: dbpinpol
      use m_missing, only: dmiss, jins
      use m_cell_geometry, only: xz, yz
      use m_flowgeom, only: ndx
      use m_polygon, only: npl, xpl, ypl, zpl, savepol, restorepol
      use m_filez, only: oldfil, doclose

      class(t_variables_inside_polygon), intent(inout) :: this !< Polygon variables object
      integer :: k, inside, minp
      call savepol()
      call oldfil(minp, this%filename)
      call reapol(minp, 0)

      associate (cell_indices => this%cell_indices, num_cells_inside => this%ndx)

         inside = -1
         num_cells_inside = 0
         this%cells_mask(1) = -1
         do k = 1, ndx ! Updated to use this%ndx instead of ndx
            call dbpinpol(xz(k), yz(k), this%cells_mask(k), dmiss, jins, npl, xpl, ypl, zpl) ! Updated to use this%cells_mask
            if (this%cells_mask(k) == 1) then ! Updated to use this%cells_mask
               num_cells_inside = num_cells_inside + 1
               this%cells_mask(k) = num_cells_inside ! Updated to use this%cells_mask
               cell_indices(num_cells_inside) = k
            end if
         end do

      end associate

      call restorepol()
      call doclose(minp)

   end subroutine findcells_inside_polygon_impl

   !> @brief Find flow links (vertices/edges) connecting cells inside polygon
   !! @details This subroutine identifies flow links where both connected cells
   !! are located within the polygon boundary. These links represent the internal
   !! flow connections that need to be included in the polygon-restricted output.
   !!
   !! A link is included if:
   !! - Both of its connected cells are marked as inside the polygon.
   !! - The link represents a valid flow connection.
   !!
   !! @param[in,out] this Polygon variables object with cell masks and link arrays.
   subroutine find_vertices_inside_polygon(this)
      use m_flowgeom, only: lnx, ln

      class(t_variables_inside_polygon), intent(inout) :: this

      integer :: L

      associate (cells_mask => this%cells_mask, &
                 link_indices => this%link_indices, &
                 link_to_nodes => this%link_to_nodes, &
                 num_vertices_inside => this%lnx, &
                 links_mask => this%links_mask)
         num_vertices_inside = 0
         links_mask = 0
         do L = 1, lnx
            if (cells_mask(ln(1, L)) > 0 .and. cells_mask(ln(2, L)) > 0) then
               num_vertices_inside = num_vertices_inside + 1
               link_indices(num_vertices_inside) = L
               links_mask(L) = num_vertices_inside
               link_to_nodes(1, num_vertices_inside) = cells_mask(ln(1, L))
               link_to_nodes(2, num_vertices_inside) = cells_mask(ln(2, L))
            end if
         end do
      end associate

   end subroutine find_vertices_inside_polygon

   !> @brief Find network nodes inside polygon boundary
   !! @details This subroutine identifies network nodes (mesh vertices) that
   !! fall within the polygon region. Network nodes are different from flow cells
   !! and represent the actual mesh vertices used in the computational grid.
   !!
   !! The subroutine tests each network node coordinate against the polygon
   !! boundary and creates appropriate mask and index arrays for nodes that
   !! are found to be inside the polygon.
   !!
   !! @param[in,out] this Polygon variables object with netnode arrays
   subroutine find_netnodes_inside_polygon(this)
      use m_flowgeom, only: nd
      use network_data, only: numk

      class(t_variables_inside_polygon), intent(inout) :: this !< Polygon variables object

      integer :: i, k, max_netnodes

      this%netnodes_mask = 0

      associate (ndx => this%ndx, &
                 netnode_indices => this%netnode_indices, &
                 netnodes_mask => this%netnodes_mask)
         do k = 1, ndx
            do i = 1, size(nd(this%cell_indices(k))%nod)
               netnodes_mask(nd(this%cell_indices(k))%nod(i)) = 1
            end do
         end do

         max_netnodes = 0
         do k = 1, numk
            if (netnodes_mask(k) == 1) then
               max_netnodes = max_netnodes + 1
               netnode_indices(max_netnodes) = k
               netnodes_mask(k) = max_netnodes
            end if
         end do
         this%numk = max_netnodes
      end associate

   end subroutine find_netnodes_inside_polygon

   !> @brief Find network links connecting nodes inside polygon
   !! @details This subroutine identifies network links (mesh edges) where both
   !! connected network nodes are located within the polygon boundary. These links
   !! represent the mesh connectivity that needs to be preserved in the polygon-
   !! restricted output.
   !!
   !! A network link is included if:
   !! - Both of its connected network nodes are inside the polygon
   !! - The link represents a valid mesh edge
   !!
   !! @param[in,out] this Polygon variables object with netnode masks and netlink arrays
   subroutine find_netlinks_inside_polygon(this)
      use network_data, only: numL, kn

      class(t_variables_inside_polygon), intent(inout) :: this !< Polygon variables object

      integer :: L, node1, node2, max_netlinks

      associate (ndx => this%ndx, &
                 netnode_indices => this%netnode_indices, &
                 netlink_indices => this%netlink_indices, &
                 netlinks_mask => this%netlinks_mask, &
                 netnodes_mask => this%netnodes_mask, &
                 netlink_to_netnodes => this%netlink_to_netnodes)

         max_netlinks = 0
         do L = 1, numL
            node1 = kn(1, L)
            node2 = kn(2, L)

            if (node1 /= 0) then
               if (netnodes_mask(abs(node1)) == 0) then
                  cycle
               end if
            end if
            if (node2 /= 0) then
               if (netnodes_mask(abs(node2)) == 0) then
                  cycle
               end if
            end if

            max_netlinks = max_netlinks + 1
            netlink_indices(max_netlinks) = L
            netlinks_mask(L) = max_netlinks
            netlink_to_netnodes(1, max_netlinks) = netnodes_mask(abs(node1))
            netlink_to_netnodes(2, max_netlinks) = netnodes_mask(abs(node2))
         end do
         this%numl = max_netlinks
      end associate

   end subroutine find_netlinks_inside_polygon

   !> @brief Count unique 2D network nodes in polygon region
   !! @details This function counts the number of unique 2D network nodes that
   !! are connected to network links within the polygon. It's used to determine
   !! memory allocation requirements and array sizes for 2D mesh output.
   !!
   !! The function:
   !! - Iterates through all 2D network links (excluding 1D links)
   !! - Identifies unique network nodes connected to these links
   !! - Uses a marking array to avoid double-counting nodes
   !!
   !! @param[in] this Polygon variables object with netlink connectivity
   !! @return numk2d Number of unique 2D network nodes
   module function count_2d_netnodes_impl(this) result(numk2d)
      use network_data, only: kn
      implicit none

      class(t_variables_inside_polygon), intent(in) :: this
      integer :: numk2d

      integer :: L, k, nn
      integer, allocatable :: kc(:)

      ! Allocate and initialize node counter array
      allocate (kc(this%numk))
      kc = 0
      numk2d = 0

      ! Count unique 2D net nodes
      do L = this%numl1d + 1, this%numl
         do k = 1, 2
            nn = this%netlink_to_netnodes(k, L)
            if (kc(nn) == 0) then
               numk2d = numk2d + 1
               kc(nn) = 1
            end if
         end do
      end do

      deallocate (kc)
   end function count_2d_netnodes_impl

   !module function remap_integer_impl(this, input_array, start_index, end_index, loc_type) result(remapped_array)
   !   implicit none
   !
   !   class(t_variables_inside_polygon), target, intent(in) :: this
   !   integer, dimension(:), intent(in) :: input_array
   !   integer, intent(in) :: start_index
   !   integer, intent(in) :: end_index
   !   integer, intent(in) :: loc_type
   !   integer, dimension(:), allocatable :: remapped_array
   !
   !   integer :: i
   !
   !   ! Allocate the remapped array with the size of elements inside polygon
   !   allocate(remapped_array(end_index - start_index + 1))
   !
   !   ! Fill the remapped array using the cell indices
   !   do i = 1, this%ndx
   !      remapped_array(i) = input_array(this%cell_indices(i))
   !   end do
   !
   !end function remap_integer_impl
   !
   !> @brief Remap double precision array from full domain to polygon region
   !! @details This function creates a subset of a double precision array containing
   !! only the elements that correspond to the polygon region. It supports different
   !! location types (cells, links, nodes) and uses the appropriate index mapping.
   !!
   !! The function:
   !! - Selects appropriate index array based on location type
   !! - If polygon is defined: uses stored cell indices to extract subset
   !! - If polygon is not defined: returns the original array slice
   !! - Returns a pointer to avoid unnecessary data copying
   !!
   !! @param[in] this Polygon variables object with index mappings
   !! @param[in] input_array Full domain array to be remapped
   !! @param[in] start_index Starting index for the range to remap
   !! @param[in] end_index Ending index for the range to remap
   !! @param[in] loc_type Location type (UNC_LOC_S for cells, UNC_LOC_U for links, etc.)
   !! @return remapped_array Pointer to remapped array subset
   module function remap_double_impl(this, input_array, start_index, end_index, loc_type) result(remapped_array)
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U, UNC_LOC_CN
      implicit none

      class(t_variables_inside_polygon), target, intent(in) :: this !< Polygon variables object
      real(kind=dp), dimension(:), target, intent(in) :: input_array !< Input array to remap
      integer, intent(in) :: start_index !< Starting index for remapping
      integer, intent(in) :: end_index !< Ending index for remapping
      integer, intent(in) :: loc_type !< Location type (UNC_LOC_S, UNC_LOC_U, etc.)
      real(kind=dp), dimension(:), pointer :: remapped_array !< Pointer to remapped array
      integer, pointer, dimension(:) :: index_array !< Pointer to selected index array

      integer :: i

      select case (loc_type)
      case (UNC_LOC_S)
         index_array => this%cell_indices
      case (UNC_LOC_U)
         index_array => this%link_indices
      case (UNC_LOC_CN)
         index_array => this%netnode_indices
      end select

      if (this%is_defined) then
         ! Fill the remapped array using the cell indices
         remapped_array => values(1:end_index - start_index + 1)
         do i = 0, end_index - start_index
            remapped_array(i + 1) = input_array(index_array(start_index + i))
         end do
      else
         remapped_array => input_array(start_index:end_index)
      end if
   end function remap_double_impl

   !> @brief Allocate all mask and index arrays for polygon variables
   !! @details This subroutine allocates memory for all arrays needed to store
   !! polygon masking information including cell indices, link connectivity,
   !! network node and link mappings. All arrays are allocated with their
   !! full domain sizes initially.
   !!
   !! @param[in,out] this Polygon variables object to allocate arrays for
   subroutine allocate_arrays(this)

      use m_alloc, only: realloc, reallocP

      implicit none

      class(t_variables_inside_polygon), intent(inout) :: this !< Polygon variables object

      call realloc(this%cell_indices, this%ndx, keepExisting=.true.)
      call realloc(this%link_indices, this%lnx, keepExisting=.true.)
      call realloc(this%link_to_nodes, [2, this%lnx], keepExisting=.true.)
      call realloc(this%netnode_indices, this%numk, keepExisting=.true.)
      call realloc(this%netlink_indices, this%numL, keepExisting=.true.)
      call realloc(this%netlink_to_netnodes, [2, this%numL], keepExisting=.true.)
      if (.not. allocated(this%cells_mask)) then
         ! only allocate values array once
         call realloc(this%cells_mask, this%ndx, keepExisting=.true.)
         call realloc(this%links_mask, this%lnx, keepExisting=.true.)
         call realloc(this%netnodes_mask, this%numk, keepExisting=.true.)
         call realloc(this%netlinks_mask, this%numL, keepExisting=.true.)
         call reallocP(values, max(this%ndx, this%lnx, this%numk, this%numl))
      end if
   end subroutine allocate_arrays

end submodule m_output_to_polygon_sub
