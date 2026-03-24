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
!! @brief Module for handling output restriction to polygon regions
!! @details This module provides functionality to restrict model output to specific
!! polygon regions, allowing for selective data export and masking capabilities.
!! The module handles flow cells, links, and network nodes within polygon boundaries.
!!
!!> @defgroup output_to_polygon Output to Polygon
!! @{
!! Provides functionality for restricting output to polygon regions
!! @}

!> @brief Module for handling output restriction to polygon regions
!! @ingroup output_to_polygon
module m_output_to_polygon
   use precision, only: dp
   use messagehandling, only: idLen
   implicit none

   !> @brief Type for managing variables and indices inside a polygon region
   !! @details This derived type contains all necessary data structures for handling
   !! model variables that fall within a specified polygon boundary. It includes
   !! indices for cells, links, nodes, and associated masking arrays.
   !! @ingroup output_to_polygon
   type, public :: t_variables_inside_polygon
      character(len=idLen) :: filename !< Polygon file name.
      logical :: is_polygon_file_defined !< Flag indicating if there is an output polygon defined.
      integer, allocatable, dimension(:) :: cell_indices !< Global indices of cells inside the polygon.
      integer, allocatable, dimension(:) :: cells_mask !< Mask array for global cells. (0 = outside polygon, otherwise local cell index)

      integer, allocatable, dimension(:) :: link_indices !< Global indices of links inside the polygon.
      integer, allocatable, dimension(:) :: links_mask !< Mask array for global links. (0 = outside polygon, otherwise local link index)
      integer, allocatable, dimension(:, :) :: link_to_nodes !< Link to node connectivity.

      integer, allocatable, dimension(:) :: netnodes_mask !< Mask array for global net nodes (0 = outside polygon, otherwise local net node index)
      integer, allocatable, dimension(:) :: netnode_indices !< Global indices of network nodes inside polygon

      integer, allocatable, dimension(:) :: netlink_indices !< Global indices of network links inside polygon
      integer, allocatable, dimension(:) :: netlinks_mask !< Mask array for global network links, (0 = outside polygon, otherwise local netlink index)
      integer, allocatable, dimension(:, :) :: netlink_to_netnodes !< Network link to node connectivity

      integer :: ndx !< Number of flow cells inside polygon
      integer :: ndxi !< Number of internal cells inside polygon
      integer :: ndx2d !< Number of 2D cells inside polygon
      integer :: ndx1db !< Number of 1D boundary cells inside polygon
      integer :: lnx !< Number of links inside polygon
      integer :: lnx1d !< Number of 1D links inside polygon
      integer :: lnx1db !< Number of 1D boundary links inside polygon
      integer :: lnx1d2d !< Number of 1D-2D connection links inside polygon
      integer :: lnxi !< Number of internal links inside polygon
      integer :: numk !< Number of network nodes inside polygon
      integer :: numl !< Number of network links inside polygon
      integer :: numl1d !< Number of 1D network links inside polygon
      real(kind=dp), pointer :: cell_values(:) !< Cell values for remapping
   contains
      !> @brief Create mask arrays for polygon region
      !! @details Initializes and populates all mask arrays for identifying
      !! model elements (cells, links, nodes) that fall within the polygon boundary
      procedure :: create_mask_arrays => create_mask_arrays_impl

      !> @brief Reset mask arrays for polygon region
      !! @details Deallocates and resets all mask arrays for the polygon region
      procedure :: reset => reset_mask_arrays_impl

      !> @brief Find cells inside polygon boundary
      !! @details Determines which flow cells are located within the specified
      !! polygon region and creates appropriate mask arrays
      
      procedure :: findcells_inside_polygon => findcells_inside_polygon_impl
      !> @brief Count 2D network nodes inside polygon
      !! @details Counts the number of 2D network nodes that fall within
      !! the polygon boundary
      !! @return Number of 2D network nodes inside polygon
      procedure :: count_2d_netnodes => count_2d_netnodes_impl

      !> @brief Remap double precision array to polygon region
      !! @details Maps a double precision array from the full model domain
      !! to only the elements within the polygon region
      !! @param[in] input_array Input array covering full domain
      !! @param[in] start_index Starting index for remapping
      !! @param[in] end_index Ending index for remapping
      !! @param[in] loc_type Location type (cell, link, or node)
      !! @return Pointer to remapped array
      procedure :: remap_double => remap_double_impl

      !> @brief Generic interface for remapping arrays
      !! @details Provides a generic interface that automatically selects
      !! the appropriate remapping procedure based on data type
      generic :: remap => remap_double
   end type t_variables_inside_polygon

   !> @brief Interface for create_mask_arrays implementation
   !! @details Creates and initializes all mask arrays for polygon regions
   interface
      module subroutine create_mask_arrays_impl(this)
         implicit none
         class(t_variables_inside_polygon), intent(inout) :: this !< Polygon variables object
      end subroutine create_mask_arrays_impl
   end interface

   !> @brief Reset mask arrays for polygon region
   !! @details Deallocates and resets all mask arrays for the polygon region
   interface
      module subroutine reset_mask_arrays_impl(this)
         implicit none
         class(t_variables_inside_polygon), intent(inout) :: this !< Polygon variables object
      end subroutine reset_mask_arrays_impl
   end interface

   !> @brief Interface for findcells_inside_polygon implementation
   !! @details Finds all flow cells within the polygon boundary
   interface
      module subroutine findcells_inside_polygon_impl(this)
         implicit none
         class(t_variables_inside_polygon), intent(inout) :: this !< Polygon variables object
      end subroutine findcells_inside_polygon_impl
   end interface

   !> @brief Interface for count_2d_netnodes implementation
   !! @details Counts 2D network nodes within polygon
   interface
      module function count_2d_netnodes_impl(this) result(numk2d)
         implicit none
         class(t_variables_inside_polygon), intent(in) :: this !< Polygon variables object
         integer :: numk2d !< Number of 2D network nodes
      end function count_2d_netnodes_impl

   end interface

   !interface
   !   module function remap_integer_impl(this, input_array, start_index, end_index, loc_type) result(remapped_array)
   !      implicit none
   !      class(t_variables_inside_polygon), target, intent(in) :: this
   !      integer, dimension(:), intent(in) :: input_array
   !      integer, intent(in) :: start_index, end_index, loc_type
   !      integer, dimension(:), pointer :: remapped_array
   !   end function remap_integer_impl
   !
   !end interface

   !> @brief Interface for remap_double implementation
   !! @details Remaps double precision arrays from full domain to polygon region
   interface
      module function remap_double_impl(this, input_array, start_index, end_index, loc_type) result(remapped_array)
         implicit none
         class(t_variables_inside_polygon), target, intent(in) :: this !< Polygon variables object
         real(kind=dp), dimension(:), target, intent(in) :: input_array !< Input array to remap
         integer, intent(in) :: start_index !< Starting index for remapping
         integer, intent(in) :: end_index !< Ending index for remapping
         integer, intent(in) :: loc_type !< Location type (UNC_LOC_S, UNC_LOC_U, etc.)
         real(kind=dp), dimension(:), pointer :: remapped_array !< Pointer to remapped array
      end function remap_double_impl

   end interface
   private

contains
end module m_output_to_polygon
!> @}
