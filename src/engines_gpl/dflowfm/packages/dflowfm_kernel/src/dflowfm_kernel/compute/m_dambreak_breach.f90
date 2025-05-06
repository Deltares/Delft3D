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

!> Module for handling dambreak data in the model
module m_dambreak_breach
   use precision, only: dp

   implicit none

   private

   integer, public, protected :: n_db_links !< number of dambreak links
   integer, public, protected :: n_db_signals !< number of dambreak signals
   integer, dimension(:), allocatable :: dambreak_structure !< dambreak indices in structures
   integer, dimension(:), allocatable :: first_link !< first dambreak link for each signal
   integer, dimension(:), allocatable :: last_link !< last dambreak link for each signal
   integer, dimension(:), allocatable :: link_index !< dambreak links index array
   real(kind=dp), dimension(:), allocatable :: link_effective_width !< dambreak effective flow widths
   real(kind=dp), dimension(:), allocatable :: link_actual_width !< dambreak actual flow widths
   character(len=128), dimension(:), allocatable :: dambreak_names !< dambreak names

   ! This module also holds pulic functions/subroutines after contains
   ! They use 1) only basic modules, 2) only data from the module, and 3) they are small!
   
   public :: adjust_bobs_for_dambreaks, update_dambreak_breach, fill_dambreak_values, &
       set_dambreak_widening_method, get_dambreak_depth_c_loc, &
       get_dambreak_breach_width_c_loc, get_dambreak_upstream_level_c_loc, &
       get_dambreak_downstream_level_c_loc, update_dambreak_administration, &
       update_dambreak_administration_old, reset_dambreak_counters, &
       exist_dambreak_links, should_write_dambreaks, multiply_by_dambreak_link_actual_width, &
       indicate_links_that_contain_dambreaks, get_active_dambreak_index, &
       get_dambreak_names, retrieve_set_of_flowlinks_dambreak, &
       update_counters_for_dambreaks, allocate_dambreak_width_arrays, add_dambreak_signal

   interface
      module subroutine adjust_bobs_for_dambreaks()
      end subroutine adjust_bobs_for_dambreaks

      module function update_dambreak_breach(start_time, delta_time) result(error)
         real(kind=dp), intent(in) :: start_time !< start time
         real(kind=dp), intent(in) :: delta_time !< delta time
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
         integer, dimension(:), intent(in) :: lftopol !< the link number of the flow link.
      end subroutine update_dambreak_administration
         
      module subroutine update_dambreak_administration_old(dambridx, lftopol)
         integer, dimension(:), intent(in) :: dambridx !< the index of the dambreak in the structure list.
         integer, dimension(:), intent(in) :: lftopol !< the link number of the flow link.
      end subroutine update_dambreak_administration_old
    
   end interface

contains

!> Initialize the dambreak data
   subroutine reset_dambreak_counters()

      n_db_links = 0 ! nr of dambreak links
      n_db_signals = 0 ! nr of dambreak signals

   end subroutine reset_dambreak_counters

   !> Check if there are any dambreak links
   pure function exist_dambreak_links() result(res)
      logical :: res !< True if there are any dambreak links

      res = n_db_links > 0

   end function exist_dambreak_links

   pure function should_write_dambreaks() result(res)

      logical :: res
      integer :: objects !< total number of objects to write
      integer :: n !< loop index

      ! Count the number of active links for each signal
      objects = n_db_signals
      do n = 1, n_db_signals
         if (first_link(n) > last_link(n)) then
            objects = objects - 1
         end if
      end do

      res = objects > 0
   end function should_write_dambreaks

   !> set correct flow areas for dambreaks, using the actual flow width
   subroutine multiply_by_dambreak_link_actual_width(hu, au)

      real(kind=dp), dimension(:), intent(in) :: hu !< source
      real(kind=dp), dimension(:), intent(inout) :: au !< results

      integer :: n !< loop index
      integer :: k !< loop index
      integer :: link !< link index

      do n = 1, n_db_signals
         do k = first_link(n), last_link(n)
            link = abs(link_index(k))
            au(link) = hu(link) * link_actual_width(k)
         end do
      end do

   end subroutine multiply_by_dambreak_link_actual_width

   !> update array of logicals indicating if the link contains dambreaks
   pure subroutine indicate_links_that_contain_dambreaks(does_link_contain_structures)

      logical, intent(inout) :: does_link_contain_structures(:) !< array of logicals indicating if the link contains structures

      integer :: n !< loop index
      integer :: k !< loop index

      if (exist_dambreak_links()) then
         do n = 1, n_db_signals
            if (dambreak_structure(n) /= 0) then
               do k = first_link(n), last_link(n)
                  does_link_contain_structures(abs(link_index(k))) = .true.
               end do
            end if
         end do
      end if

   end subroutine indicate_links_that_contain_dambreaks

   !> Get the index of the active dambreak for a given dambreak name
   pure function get_active_dambreak_index(dambreak_name) result(index)
      character(len=*), intent(in) :: dambreak_name !< Id/name of the requested dambreak
      integer :: index !< Returned index of the found dambreak; -1 when not found.

      integer :: i !< loop index

      index = -1
      do i = 1, n_db_signals
         if (trim(dambreak_names(i)) == trim(dambreak_name)) then
            if (last_link(i) - first_link(i) >= 0) then
               ! Only return this dambreak index if dambreak is active in flowgeom (i.e., at least 1 flow link associated)
               index = i
               exit
            end if
         end if
      end do
   end function get_active_dambreak_index

   !> provides dambreak names
   pure function get_dambreak_names() result(names)
      character(len=128), dimension(:), allocatable :: names !< the dambreak names

      names = [(dambreak_names(i), integer :: i=1, n_db_signals)]

   end function get_dambreak_names
   
   !> Get the dambreak links for a given dambreak index
   function retrieve_set_of_flowlinks_dambreak(index) result(res)
      use messagehandling, only: msgbuf, LEVEL_ERROR, SetMessage
      integer, intent(in) :: index !< index of the dambreak
      integer, dimension(:), allocatable :: res !< the dambreak links
      
      if (index < 1 .or. index > n_db_signals) then
         write (msgbuf, *) 'get_dambreak_links: the index ', index, &
            ' is out of range. The range is 1 to ', n_db_signals
         call SetMessage(LEVEL_ERROR, msgbuf)
         allocate(res(0))
      else
         res = [(link_index(i), integer :: i = first_link(index), last_link(index))]
      end if
      
   end function retrieve_set_of_flowlinks_dambreak
   
   subroutine update_counters_for_dambreaks(id, numgen, dambridx, i, kedb, kegen)
      use m_update_counters_for_structures, only: update_counters_for_dambreak_or_pump
      character(len=*), intent(in) :: id !< the id of the structure.
      integer, intent(in) :: numgen !< the number of flow links.
      integer, dimension(:), allocatable, intent(inout) :: dambridx !< the index of the structure.
      integer, intent(in) :: i !< the index of the structure.
      integer, dimension(:), allocatable, intent(inout) :: kedb !< edge oriented dambreak??? Do we need this array?
      integer, dimension(:), allocatable, intent(in) :: kegen !< placeholder for the link snapping of all structure types.

      call update_counters_for_dambreak_or_pump(id, numgen, n_db_signals, first_link, last_link, dambridx, i)
      kedb(first_link(n_db_signals):last_link(n_db_signals)) = kegen(1:numgen)
      
   end subroutine update_counters_for_dambreaks

   !> allocate and intialize dambreak link arrays 
   subroutine allocate_dambreak_width_arrays(numl)
      use m_alloc, only: realloc

      integer, intent(in) :: numl !< number of links
      
      call realloc(link_effective_width, numl)
      call realloc(link_actual_width, numl, fill=0.0_dp)
      
   end subroutine allocate_dambreak_width_arrays
   
   subroutine add_dambreak_signal(index_in_structure, dambridx, n_dambreak_links, n_current_dambreak_links)
      use messagehandling, only: msgbuf, LEVEL_ERROR, SetMessage
      use m_alloc, only: realloc

      integer, intent(in) :: index_in_structure !< the index of the structure in the structure list.
      integer, dimension(:), intent(inout) :: dambridx !< the index of the dambreak in the structure list.
      integer, intent(inout) :: n_dambreak_links !< the total number of flow links for dambreaks.
      integer, intent(in) :: n_current_dambreak_links !< the number of flow links for the current dambreak signal.
      
      if (n_dambreak_links /= n_db_links) then
         write (msgbuf, '(a,i8,a,i8)') 'n_dambreak_links = ', n_dambreak_links, ' /= n_db_links = ', n_db_links
         call SetMessage(LEVEL_ERROR, msgbuf)
      end if
      n_db_signals = n_db_signals + 1
      dambridx(n_db_signals) = index_in_structure
      call realloc(first_link, n_db_signals)
      first_link(n_db_signals) = n_dambreak_links + 1
      call realloc(last_link, n_db_signals)
      last_link(n_db_signals) = n_dambreak_links + n_current_dambreak_links
      n_dambreak_links = n_dambreak_links + n_current_dambreak_links
      n_db_links = n_dambreak_links
      
   end subroutine add_dambreak_signal
   
end module m_dambreak_breach
