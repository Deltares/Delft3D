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

!
!

module m_linktypetoint

   implicit none

contains

   !> Parses a link type/mesh contact's type string into an integer
   !! that can be used to compare agains kn(3,:) codes.
   !!
   !! Currently supported names: internal, lateral, embedded, longitudinal, streetInlet, roofGutterPipe, all.
   function linkTypeToInt(linkTypeString) result(res)
      use string_module, only: str_tolower
      use m_inquire_flowgeom
      character(len=*), intent(in) :: linkTypeString !< Type value as given in input file.
      integer :: res !< The returned link type integer code. (3/4/5/7). -1 for unknown type.

      select case (str_tolower(trim(linkTypeString)))
      case ('internal', 'lateral', 'embedded')
         res = IFLTP_1D2D_INT
      case ('longitudinal')
         res = IFLTP_1D2D_LONG
      case ('streetinlet')
         res = IFLTP_1D2D_STREET
      case ('roofgutterpipe')
         res = IFLTP_1D2D_ROOF
      case ('all') ! Special type to support selecting any link type
         res = IFLTP_ALL
      case default
         res = -1
      end select

   end function linkTypeToInt

end module m_linktypetoint
