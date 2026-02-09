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

module m_switchiadvnearlink

   implicit none

   private

   public :: switchiadvnearlink

contains

   subroutine switchiadvnearlink(L)
      use m_flowgeom, only: ln, iadv_original_lateral_overflow, nd, iadv, iadv_general_structure, kcu
      use m_flow, only: iadvec, u0
      implicit none
      integer :: L, k1, k2, kk, LL, iadv1, iadv2

      k1 = ln(1, L)
      k2 = ln(2, L)

      if (iadvec == 0) then
         iadv1 = 0
         iadv2 = 0
      elseif (u0(L) > 0) then
         iadv1 = IADV_ORIGINAL_LATERAL_OVERFLOW ! piaczek incoming upwind
         iadv2 = 0 ! noadv downstream
      else if (u0(L) < 0) then
         iadv1 = 0
         iadv2 = IADV_ORIGINAL_LATERAL_OVERFLOW
      else ! == (now safe for grid direction)
         iadv1 = IADV_ORIGINAL_LATERAL_OVERFLOW
         iadv2 = IADV_ORIGINAL_LATERAL_OVERFLOW
      end if

      do kk = 1, nd(k1)%lnx
         LL = abs(nd(k1)%ln(kk))
         if (iadv(LL) /= IADV_GENERAL_STRUCTURE .and. (kcu(LL) == 1 .or. kcu(LL) == 2)) then ! Only for regular 1D or 2D.
            iadv(LL) = iadv1
         end if
      end do
      do kk = 1, nd(k2)%lnx
         LL = abs(nd(k2)%ln(kk))
         if (iadv(LL) /= IADV_GENERAL_STRUCTURE .and. (kcu(LL) == 1 .or. kcu(LL) == 2)) then ! Only for regular 1D or 2D.
            iadv(LL) = iadv2
         end if
      end do

   end subroutine switchiadvnearlink

end module m_switchiadvnearlink
