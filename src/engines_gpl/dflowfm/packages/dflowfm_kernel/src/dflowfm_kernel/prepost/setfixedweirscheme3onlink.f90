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

!
!

module m_setfixedweirscheme3onlink


   use precision, only: dp
   implicit none

   private

   public :: setfixedweirscheme3onlink

contains

   subroutine setfixedweirscheme3onlink(L)
      use m_flowgeom, only: teta, iadv, iadv_tabellenboek_weir, iadv_villemonte_weir, ln, nd, iadv_subgrid_weir

      integer :: L, nn, n12, kk, LL

      teta(L) = 1.0_dp

      if (iadv(L) /= IADV_TABELLENBOEK_WEIR .and. iadv(L) /= IADV_VILLEMONTE_WEIR) then ! no change in advection for Tabellenboek and Villemonte
         do nn = 1, 2
            n12 = ln(nn, L)
            do kk = 1, nd(n12)%lnx ! and flag non-21 links to perot incoming only
               LL = abs(nd(n12)%ln(kk))
               if (iadv(LL) < IADV_SUBGRID_WEIR .or. iadv(LL) > IADV_VILLEMONTE_WEIR) then
                  iadv(LL) = 4
               end if
               teta(LL) = 1.0_dp
            end do
         end do
      end if

   end subroutine setfixedweirscheme3onlink

end module m_setfixedweirscheme3onlink
