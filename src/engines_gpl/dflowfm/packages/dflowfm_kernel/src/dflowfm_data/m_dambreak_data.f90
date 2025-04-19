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
module m_dambreak_data
   use precision, only: dp

   implicit none

   public

   integer, allocatable :: dambreaks(:) !< store the dambreaks indexes among all structures
   integer, target :: n_db_links !< nr of dambreak links
   integer, target :: n_db_signals !< nr of dambreak signals
   integer, allocatable :: db_first_link(:) !< first dambreak link for each signal
   integer, allocatable :: db_last_link(:) !< last dambreak link for each signal
   integer, allocatable :: db_active_links(:) !< db_active_links, open dambreak links
   integer, allocatable :: breach_start_link(:) !< the starting link, the closest to the breach point
   integer, allocatable :: db_upstream_link_ids(:) !< dambreak links index array
   integer, allocatable :: db_downstream_link_ids(:) !< dambreak links index array
   integer, allocatable :: db_link_ids(:) !< dambreak links index array
   real(kind=dp), allocatable, target :: db_levels_widths_table(:) !< dambreak widths and heights
   character(len=128), allocatable, target :: db_ids(:) !< the dambreak ids
   real(kind=dp), dimension(:), allocatable, public :: db_link_effective_width !< dambreak effective flow widths
   real(kind=dp), dimension(:), allocatable, public :: db_link_actual_width !< dambreak actual flow widths
   
   integer, protected, pointer :: p_n_db_links => n_db_links
   integer, protected, pointer :: p_n_db_signals => n_db_signals
   

contains

!> Initialize the dambreak data
   subroutine default_dambreak_data()

      n_db_links = 0 ! nr of dambreak links
      n_db_signals = 0 ! nr of dambreak signals

   end subroutine default_dambreak_data
   
   !> Check if there are any dambreak links
   pure function exist_dambreak_links() result(res)
      logical :: res !< True if there are any dambreak links

      res = n_db_links > 0

   end function exist_dambreak_links
   
   !> Retrieve the set of snapped flowlinks for a dambreak
   pure function retrieve_set_of_flowlinks_dambreak(i_dambreak) result(links)

      integer, intent(in) :: i_dambreak !< Index of the dambreak
      integer, dimension(:), allocatable :: links !< The set of flowlinks that this dambreak has been snapped to

      integer :: n_links !< Total number of flowlinks in the set
      integer :: k, i

      n_links = db_last_link(i_dambreak) + 1 - db_first_link(i_dambreak)
      allocate (links(n_links), source=-999)

      i = 0
      do k = db_first_link(i_dambreak), db_last_link(i_dambreak)
         i = i + 1
         links(i) = db_link_ids(k)
      end do

   end function retrieve_set_of_flowlinks_dambreak

   pure function should_write_dambreaks() result(res)
   
      logical :: res 
      integer :: objects !< total number of objects to write
      integer :: n !< loop index

      ! Count the number of active links for each signal
      objects = n_db_signals
      do n = 1, n_db_signals
         if (db_first_link(n) > db_last_link(n)) then
            objects = objects - 1
         end if
      end do
        
      res = objects > 0
   end function should_write_dambreaks
   
   
   !> set correct flow areas for dambreaks, using the actual flow width
   subroutine multiply_by_dambreak_link_actual_width(hu, au)
   
      real(kind=dp) , intent(in) :: hu(:) !< source
      real(kind=dp) , intent(inout) :: au(:) !< results

      integer :: n !< loop index
      integer :: k !< loop index
      integer :: link !< link index
      
      do n = 1, n_db_signals
         do k = db_first_link(n), db_last_link(n)
            link = abs(db_link_ids(k))
            au(link) = hu(link) * db_link_actual_width(k)
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
            if (dambreaks(n) /= 0) then
               do k = db_first_link(n), db_last_link(n)
                  does_link_contain_structures(abs(db_link_ids(k))) = .true.
               end do
            end if
         end do
      end if
      
   end subroutine indicate_links_that_contain_dambreaks    
         
end module m_dambreak_data
