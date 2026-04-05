!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

!> Module for handling dambreak data in the model
module m_dambreak_breach
   use precision, only: dp
   use m_ec_parameters, only: ec_undef_int

   implicit none

   private

   integer, public, protected :: n_db_links !< number of dambreak links
   integer, public, protected :: n_db_signals !< number of dambreak signals

   ! This module also holds pulic functions/subroutines after contains
   ! They use 1) only basic modules, 2) only data from the module, and 3) they are small!

   public :: adjust_bobs_for_dambreaks, update_dambreak_breach, fill_dambreak_values, &
             set_dambreak_widening_method, get_dambreak_depth_c_loc, &
             get_dambreak_breach_width_c_loc, get_dambreak_upstream_level_c_loc, &
             get_dambreak_downstream_level_c_loc, update_dambreak_administration, &
             update_dambreak_administration_old, reset_dambreak_counters, &
             have_dambreaks_links, should_write_dambreaks, set_flow_areas_for_dambreaks, &
             indicate_links_that_contain_dambreaks, get_active_dambreak_index, &
             get_dambreak_names, retrieve_set_of_flowlinks_dambreak, &
             update_counters_for_dambreaks, add_dambreak_signal

   interface
      module subroutine adjust_bobs_for_dambreaks()
      end subroutine adjust_bobs_for_dambreaks

      module function update_dambreak_breach(current_time, time_step) result(error)
         real(kind=dp), intent(in) :: current_time !< current time
         real(kind=dp), intent(in) :: time_step !< time step
         integer :: error !< error code
      end function update_dambreak_breach

      module subroutine fill_dambreak_values(time_step, values)
         real(kind=dp), intent(in) :: time_step !< time step
         real(kind=dp), dimension(:, :), intent(inout) :: values !< dambreak values
      end subroutine fill_dambreak_values

      module subroutine set_dambreak_widening_method(method_string)
         character(len=*), intent(inout) :: method_string !< method for dambreak widening
      end subroutine set_dambreak_widening_method

      module function get_dambreak_depth_c_loc(item_index) result(res)
         use iso_c_binding, only: c_ptr
         integer, intent(in) :: item_index
         type(c_ptr) :: res
      end function get_dambreak_depth_c_loc

      module function get_dambreak_breach_width_c_loc(item_index) result(res)
         use iso_c_binding, only: c_ptr
         integer, intent(in) :: item_index
         type(c_ptr) :: res
      end function get_dambreak_breach_width_c_loc

      module function get_dambreak_upstream_level_c_loc(item_index) result(res)
         use iso_c_binding, only: c_ptr
         integer, intent(in) :: item_index
         type(c_ptr) :: res
      end function get_dambreak_upstream_level_c_loc

      module function get_dambreak_downstream_level_c_loc(item_index) result(res)
         use iso_c_binding, only: c_ptr
         integer, intent(in) :: item_index
         type(c_ptr) :: res
      end function get_dambreak_downstream_level_c_loc

      module subroutine update_dambreak_administration(dambridx, lftopol)
         integer, dimension(:), intent(in) :: dambridx !< the index of the dambreak in the structure list.
         integer, dimension(:), intent(in) :: lftopol !< the mapping array from flow link to intersecting polyline segment.
      end subroutine update_dambreak_administration

      module subroutine update_dambreak_administration_old(dambridx, lftopol)
         integer, dimension(:), intent(in) :: dambridx !< the index of the dambreak in the structure list.
         integer, dimension(:), intent(in) :: lftopol !< the mapping array from flow link to intersecting polyline segment.
      end subroutine update_dambreak_administration_old

      pure module subroutine indicate_links_that_contain_dambreaks(does_link_contain_structures)
         logical, intent(inout) :: does_link_contain_structures(:) !< array of logicals indicating if the link contains structures
      end subroutine indicate_links_that_contain_dambreaks

      pure module function should_write_dambreaks() result(res)
         logical :: res
      end function should_write_dambreaks

      module subroutine set_flow_areas_for_dambreaks(hu, au)
         real(kind=dp), dimension(:), intent(in) :: hu !< source
         real(kind=dp), dimension(:), intent(inout) :: au !< results
      end subroutine set_flow_areas_for_dambreaks

      pure module function get_active_dambreak_index(dambreak_name) result(index)
         character(len=*), intent(in) :: dambreak_name !< Id/name of the requested dambreak
         integer :: index !< Returned index of the found dambreak; -1 when not found.
      end function get_active_dambreak_index

      module function retrieve_set_of_flowlinks_dambreak(index) result(res)
         integer, intent(in) :: index !< index of the dambreak
         integer, dimension(:), allocatable :: res !< the dambreak links
      end function retrieve_set_of_flowlinks_dambreak

      module subroutine update_counters_for_dambreaks(id, number_of_links, dambridx, index_structure, kedb, kegen)
         character(len=*), intent(in) :: id !< the id of the structure.
         integer, intent(in) :: number_of_links !< the number of flow links.
         integer, dimension(:), allocatable, intent(inout) :: dambridx !< the index of the structure.
         integer, intent(in) :: index_structure !< the index of the structure.
         integer, dimension(:), allocatable, intent(inout) :: kedb !< edge oriented dambreak
         integer, dimension(:), allocatable, intent(in) :: kegen !< placeholder for the link snapping of all structure types.
      end subroutine update_counters_for_dambreaks

      module subroutine add_dambreak_signal(index_in_structure, dambridx, n_dambreak_links, n_current_dambreak_links)
         integer, intent(in) :: index_in_structure !< the index of the structure in the structure list.
         integer, dimension(:), intent(inout) :: dambridx !< the index of the dambreak in the structure list.
         integer, intent(inout) :: n_dambreak_links !< the total number of flow links for dambreaks.
         integer, intent(in) :: n_current_dambreak_links !< the number of flow links for the current dambreak signal.
      end subroutine add_dambreak_signal

      pure module function get_dambreak_names() result(names)
         character(len=128), dimension(:), allocatable :: names !< the dambreak names
      end function get_dambreak_names

   end interface

   ! This type was moved to the module level to work around a linker issue with intel oneapi 2025.3.2 and the gfortran linker on linux.
   ! The symbols were not correctly exported when this was in a submodule.
   type :: t_dambreak !< data for a single dambreak
      integer :: algorithm = 0 !< algorithm for the dambreak breach growth
      integer :: breach_start_link = -1 !< index of the starting link in the breach growth
      integer :: index_structure = 0 !< index of the structure
      integer :: ec_item = ec_undef_int !< item for EC module to get crest level and width from a tim file
      integer :: number_of_links = 0 !< number of links in the dambreak
      integer :: link_map_offset = 0 !< offset of the local array in the global link array
      integer, dimension(:), allocatable :: link_indices !< link indices of the dambreak
      integer, dimension(:), allocatable :: active_links !< active links of the dambreak
      integer, dimension(:), allocatable :: upstream_link_ids !< upstream link indices
      integer, dimension(:), allocatable :: downstream_link_ids !< downstream link indices
      character(len=128) :: name = "" !< name of the dambreak
      real(kind=dp) :: breach_depth = 0.0_dp !< depth of the breach
      real(kind=dp) :: breach_width = 0.0_dp !< width of the breach
      real(kind=dp) :: breach_width_ini = 0.0_dp !< initial width of the breach
      real(kind=dp) :: breach_width_derivative !< derivative of the breach width
      real(kind=dp) :: crest_level !< crest level of the breach
      real(kind=dp) :: crest_level_ini !< initial crest level of the breach
      real(kind=dp) :: crest_level_min !< minimum crest level of the breach
      real(kind=dp) :: upstream_level !< upstream water level
      real(kind=dp) :: downstream_level !< downstream water level
      real(kind=dp), dimension(2) :: crest_level_and_width !< array to communicate with EC module
      real(kind=dp) :: width = 0.0_dp !< width of the breach
      real(kind=dp) :: maximum_width = 0.0_dp !< maximum width of the breach
      real(kind=dp) :: maximum_allowed_width = -1.0_dp !< maximum allowed width of the breach
      real(kind=dp) :: water_level_jump !< water level jump of the breach
      real(kind=dp) :: normal_velocity !< normal velocity of the breach
      real(kind=dp) :: u_crit !< critical velocity for the breach growth
      real(kind=dp) :: t0 = 0.0_dp !< time of the start of the dambreak
      real(kind=dp) :: time_to_breach_to_maximum_depth !< time to breach to maximum depth
      real(kind=dp) :: a_coeff !< coefficient a for the breach growth
      real(kind=dp) :: b_coeff !< coefficient b for the breach growth
      real(kind=dp) :: f1 !< coefficient f1 for the breach width derivative
      real(kind=dp) :: f2 !< coefficient f2 for the breach width derivative
      real(kind=dp), dimension(:), allocatable :: link_actual_width !< actual width of the links in the dambreak
      real(kind=dp), dimension(:), allocatable :: link_effective_width !< effective width of the links in the dambreak
      procedure(calculate_breach_growth_using_any_model), pointer :: calculate_breach_growth => null()
   contains
      procedure :: array_allocation => allocate_arrays
      final :: deallocate_arrays
   end type

   abstract interface
      subroutine calculate_breach_growth_using_any_model(dambreak, time, time_step)
         use precision, only: dp
         import t_dambreak
         class(t_dambreak), intent(inout) :: dambreak !< dambreak data for a single dambreak
         real(kind=dp), intent(in) :: time !< current time
         real(kind=dp), intent(in) :: time_step !< time step
      end subroutine calculate_breach_growth_using_any_model
   end interface
contains

   !> alllocate internal arrays for a dambreak
   subroutine allocate_arrays(this)
      class(t_dambreak), intent(inout) :: this

      allocate (this%link_indices(this%number_of_links), source=0)
      allocate (this%active_links(this%number_of_links), source=0)
      allocate (this%upstream_link_ids(this%number_of_links), source=0)
      allocate (this%downstream_link_ids(this%number_of_links), source=0)
      allocate (this%link_actual_width(this%number_of_links), source=0.0_dp)
      allocate (this%link_effective_width(this%number_of_links), source=0.0_dp)

   end subroutine allocate_arrays

   !> dealllocate internal arrays for a dambreak
   subroutine deallocate_arrays(this)
      type(t_dambreak), intent(inout) :: this

      if (allocated(this%link_indices)) then
         deallocate (this%link_indices)
         deallocate (this%active_links)
         deallocate (this%upstream_link_ids)
         deallocate (this%downstream_link_ids)
         deallocate (this%link_actual_width)
         deallocate (this%link_effective_width)
      end if
   end subroutine deallocate_arrays

!> Initialize the dambreak data
   subroutine reset_dambreak_counters()

      n_db_links = 0 ! nr of dambreak links
      n_db_signals = 0 ! nr of dambreak signals

   end subroutine reset_dambreak_counters

   !> Check if there are any dambreak links
   pure function have_dambreaks_links() result(res)
      logical :: res !< True if there are any dambreak links

      res = n_db_links > 0

   end function have_dambreaks_links

end module m_dambreak_breach
