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

!
!
!> This module contains various functions that determine link type, validity, etc.
!! Since link type is shared between both flow and netlinks some functions can be used for both.
!! In case a function can only be used for netlinks, then that is reflected in the name.
module m_inquire_link_type
   use network_data, only: lnn, lne, kn, LINK_1D, LINK_2D, LINK_1D2D_INTERNAL, LINK_1D2D_LONGITUDINAL, LINK_1D2D_STREETINLET, LINK_1D_MAINBRANCH, LINK_1D2D_ROOF, LINK_ALL

   implicit none

   private

   public :: linktypeToInt, is_valid_2d2d_netlink, is_valid_1d2d_netlink, is_valid_1d_netlink, count_1d_edges, count_1d_nodes

contains

   !> Parses a link type/mesh contact's type string into an integer
   !! that can be used to compare agains kn(3,:) codes.
   !!
   !! Currently supported names: internal, lateral, embedded, longitudinal, streetInlet, roofGutterPipe, all.
   function linktypeToInt(link_typeString) result(res)
      use string_module, only: str_tolower
      character(len=*), intent(in) :: link_typeString !< Type value as given in input file.
      integer :: res !< The returned link type integer code. (3/4/5/7). -1 for unknown type.

      select case (str_tolower(trim(link_typeString)))
      case ('internal', 'lateral', 'embedded')
         res = LINK_1D2D_INTERNAL
      case ('longitudinal')
         res = LINK_1D2D_LONGITUDINAL
      case ('streetinlet')
         res = LINK_1D2D_STREETINLET
      case ('roofgutterpipe')
         res = LINK_1D2D_ROOF
      case ('all') ! Special type to support selecting any link type
         res = LINK_ALL
      case default
         res = -1
      end select

   end function linktypeToInt

   !> Determine whether a netlink is of type 1D and the link has 2 valid end nodes.
   elemental function is_valid_1d_netlink(link) result(res)
      logical :: res
      integer, intent(in) :: link !< Net link number, present in the current network_data state.

      res = is_1d_link_type(kn(3, link)) .and. lnn(link) == 2
   end function is_valid_1d_netlink

   !> Determine whether a netlink is of type 1D2D and the link has 2 nodes of which one 1D node.
   elemental function is_valid_1d2d_netlink(link) result(res)
      use network_data, only: nump, lne
      logical :: res
      integer, intent(in) :: link !< Net link number, present in the current network_data state.

      res = is_1d2d_link_type(kn(3, link)) .and. lnn(link) == 2 &
            .and. min(abs(lne(1, link)), abs(lne(2, link))) <= nump .and. max(abs(lne(1, link)), abs(lne(2, link))) > nump
   end function is_valid_1d2d_netlink

   !> Determine whether a netlink is of type 1D2D but the link has 2 2D nodes (2D-2D contact)
   elemental function is_valid_2d2d_netlink(link) result(res)
      use network_data, only: nump, lne
      logical :: res
      integer, intent(in) :: link !< Net link number, present in the current network_data state.

      res = is_1d2d_link_type(kn(3, link)) .and. lnn(link) == 2 &
            .and. abs(lne(1, link)) <= nump .and. abs(lne(2, link)) <= nump
   end function is_valid_2d2d_netlink

   !> Determine whether the link_type is of type 1D2D.
   elemental function is_1d2d_link_type(link_type) result(res)
      logical :: res
      integer, intent(in) :: link_type !< Net link type to be checked.

      res = link_type == LINK_1D2D_INTERNAL .or. link_type == LINK_1D2D_LONGITUDINAL .or. &
            link_type == LINK_1D2D_STREETINLET .or. link_type == LINK_1D2D_ROOF
   end function is_1d2d_link_type

   !> Determine whether the link_type is of type 1D.
   elemental function is_1d_link_type(link_type) result(res)
      logical :: res
      integer, intent(in) :: link_type !< Net link type to be checked.

      res = link_type == LINK_1D .or. link_type == LINK_1D_MAINBRANCH
   end function is_1d_link_type

   !> Count the number of valid 1D netlinks up to numl1d.
   function count_1d_edges(numl1d) result(res)
      integer, intent(in) :: numl1d !< Number of links to check.
      integer :: res
      integer :: l

      res = count(is_valid_1d_netlink([(l, l=1, numl1d)]))
   end function count_1d_edges

   !> Count the number of valid 1D netnodes
   function count_1d_nodes(numl1d, kc) result(res)
      integer, intent(in) :: numl1d !< Number of 1d links to check for valid 1d netnodes
      integer, dimension(:), intent(out) :: kc !< permutation array to keep track of "found" nodes
      integer :: res
      integer :: l, numk1d, k1, k2

      kc(:) = 0 !> this is assumed to be allocated!
      numk1d = 0
      do L = 1, NUML1D
         if (is_valid_1d_netlink(l)) then
            K1 = KN(1, L)
            K2 = KN(2, L)
            if (KC(K1) == 0) then
               NUMK1D = NUMK1D + 1
               KC(K1) = -NUMK1D ! Remember new node number
            end if
            if (KC(K2) == 0) then
               NUMK1D = NUMK1D + 1
               KC(K2) = -NUMK1D ! Remember new node number
            end if
         end if
      end do
      res = numk1d
   end function count_1d_nodes

end module m_inquire_link_type
