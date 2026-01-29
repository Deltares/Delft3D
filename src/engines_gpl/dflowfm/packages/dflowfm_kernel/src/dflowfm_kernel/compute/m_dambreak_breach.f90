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

contains

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
