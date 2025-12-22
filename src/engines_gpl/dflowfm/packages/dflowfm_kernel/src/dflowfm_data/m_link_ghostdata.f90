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

module m_link_ghostdata

   implicit none

   private

   public :: link_ghostdata

contains

!> determine if flow link is ghost link, flow link domain number and ghost level
!!
!!   a flow link is owned by the adjacent cell with the smallest domain number
!!   thus, a link is a ghost link if:
!!                it connects two ghost cells, or
!! ACTIVATED ->   it connects only one ghost cell, and the other domain number is smaller than the own domain number, or
!                 it connects connects a cell in the own subdomain with ghostlevel >0 (at the boundary)
   subroutine link_ghostdata(domain, left_domain, right_domain, is_ghost_link, link_domain_number, &
                             left_ghost_level, right_ghost_level, link_ghost_level)
      integer, intent(in) :: domain !< domain number based on which the ghost-checking is done (typically my_rank)
      integer, intent(in) :: left_domain !< domain number of left neighboring cell
      integer, intent(in) :: right_domain !< domain number of right neighboring cell
      integer, intent(out) :: is_ghost_link !< flow link is ghost link (1) or not (0)
      integer, intent(out) :: link_domain_number !< flow link domain number
      integer, intent(in), optional :: left_ghost_level !< ghost level of left neighboring cell
      integer, intent(in), optional :: right_ghost_level !< ghost level of right neighboring cell
      integer, intent(out), optional :: link_ghost_level !< flow link ghost level (if ghostlevels specified, otherwise 0)

      is_ghost_link = 0
      link_domain_number = domain
      if (present(link_ghost_level)) then
         link_ghost_level = 0
      end if

      if ((left_domain /= domain .and. right_domain /= domain) .or. &
          (left_domain == domain .and. right_domain < domain) .or. &
          (left_domain < domain .and. right_domain == domain) &
          ) then
         is_ghost_link = 1
      else if (present(left_ghost_level) .and. present(right_ghost_level)) then
         if ((left_domain == domain .and. left_ghost_level > 0) .or. &
             (right_domain == domain .and. right_ghost_level > 0) &
             ) then
            is_ghost_link = 1
         end if
      end if

      if (is_ghost_link == 1) then

         if (present(left_ghost_level) .and. present(right_ghost_level) .and. present(link_ghost_level)) then
            link_domain_number = min(left_domain, right_domain) ! a choice

!           ghost domain cannot be own domain
            if (link_domain_number == domain) then
               link_domain_number = left_domain + right_domain - domain
            end if
            link_ghost_level = min(left_ghost_level, right_ghost_level)

!           ghost level may be zero
            if (link_ghost_level == 0) then
               link_ghost_level = max(left_ghost_level, right_ghost_level)
            end if
         end if
      end if

   end subroutine link_ghostdata

end module m_link_ghostdata
