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
module m_links_to_centers
   use precision, only: dp
   use m_flow, only: lnkx, ndkx, kmx, kmxn, ktop, kbot, lbot, ltop
   use m_flowgeom, only: lnx, ln, wcL, ndx

   implicit none
   private

   public :: links_to_centers

contains
   !> Set flow node value based on flow link values, where vlin is real(kind=dp)
   subroutine links_to_centers(vnod, vlin)
      real(kind=dp), intent(out), contiguous :: vnod(:)
      real(kind=dp), intent(in) :: vlin(lnkx)
      integer :: L, k1, k2, LL, kk, k_start, k_end, k

      vnod = 0.0_dp

      if (kmx == 0) then
         do L = 1, lnx
            k1 = ln(1, L)
            k2 = ln(2, L)
            vnod(k1) = vnod(k1) + vlin(L) * wcL(1, L)
            vnod(k2) = vnod(k2) + vlin(L) * wcL(2, L)
         end do
      else
         do LL = 1, lnx
            do L = Lbot(LL), Ltop(LL)
               k1 = ln(1, L)
               k2 = ln(2, L)
               vnod(k1) = vnod(k1) + vlin(L) * wcL(1, LL)
               vnod(k2) = vnod(k2) + vlin(L) * wcL(2, LL)
            end do
         end do

         !$OMP PARALLEL DO PRIVATE(kk, k_start, k_end, k)
         do kk = 1, ndx
            k_start = ktop(kk) + 1
            k_end = kbot(kk) + kmxn(kk) - 1
            !$OMP SIMD ASSERT
            do k = k_start, k_end
               if (k <= ndkx) then ! Vector/simd code may go out of bounds without explicit check
                  vnod(k) = vnod(ktop(kk))
               end if
            end do
         end do
         !$OMP END PARALLEL DO
      end if

   end subroutine links_to_centers

end module m_links_to_centers
