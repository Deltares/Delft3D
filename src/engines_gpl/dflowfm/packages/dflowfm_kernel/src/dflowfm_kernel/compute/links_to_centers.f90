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
module m_links_to_centers
   implicit none
   private

   public :: links_to_centers

   interface links_to_centers
      module procedure links_to_centers_dp
      module procedure links_to_centers_sp
      module procedure links_to_centers_dp_rank_2
   end interface links_to_centers
contains
   !> Set flow node value based on flow link values, where vlin is real(kind=dp)
   subroutine links_to_centers_dp(vnod, vlin)
      use precision, only: dp
      use m_flow, only: lnkx, ndkx, kmx, kmxn
      use m_flowgeom, only: lnx, ln, wcL, ndx
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_Lbot_Ltop, only: getLbotLtop

      real(kind=dp), intent(out) :: vnod(ndkx)
      real(kind=dp), intent(in) :: vlin(lnkx)
      integer :: L, k1, k2, LL, Lb, Lt, kk, kb, kt, k

      vnod = 0.0_dp

      if (kmx == 0) then
         do L = 1, lnx
            k1 = ln(1, L); k2 = ln(2, L)
            vnod(k1) = vnod(k1) + vlin(L) * wcL(1, L)
            vnod(k2) = vnod(k2) + vlin(L) * wcL(2, L)
         end do
      else
         do LL = 1, lnx
            call getLbotLtop(LL, Lb, Lt)
            do L = Lb, Lt
               k1 = ln(1, L); k2 = ln(2, L)
               vnod(k1) = vnod(k1) + vlin(L) * wcL(1, LL)
               vnod(k2) = vnod(k2) + vlin(L) * wcL(2, LL)
            end do
         end do

         !$OMP PARALLEL DO &
         !$OMP PRIVATE(kk,kb,kt,k)
         do kk = 1, ndx
            call getkbotktop(kk, kb, kt)
            do k = kt + 1, kb + kmxn(kk) - 1
               vnod(k) = vnod(kt)
            end do
         end do
         !$OMP END PARALLEL DO
      end if
   end subroutine links_to_centers_dp

   !> Set flow node value based on flow link values, where vlin is single precision
   subroutine links_to_centers_sp(vnod, vlin) ! set flow node value based on flow link values scalar
      use precision, only: dp, sp
      use m_flow, only: lnkx, ndkx, kmx, kmxn
      use m_flowgeom, only: lnx, ln, wcL, ndx
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_Lbot_Ltop, only: getLbotLtop

      real(kind=dp), intent(out) :: vnod(ndkx)
      real(kind=sp), intent(in) :: vlin(lnkx)
      integer :: L, k1, k2, LL, Lb, Lt, kk, kb, kt, k

      vnod = 0.0_dp

      if (kmx == 0) then
         do L = 1, lnx
            k1 = ln(1, L); k2 = ln(2, L)
            vnod(k1) = vnod(k1) + vlin(L) * wcL(1, L)
            vnod(k2) = vnod(k2) + vlin(L) * wcL(2, L)
         end do
      else
         do LL = 1, lnx
            call getLbotLtop(LL, Lb, Lt)
            do L = Lb, Lt
               k1 = ln(1, L); k2 = ln(2, L)
               vnod(k1) = vnod(k1) + vlin(L) * wcL(1, LL)
               vnod(k2) = vnod(k2) + vlin(L) * wcL(2, LL)
            end do
         end do

         !$OMP PARALLEL DO &
         !$OMP PRIVATE(kk,kb,kt,k)
         do kk = 1, ndx
            call getkbotktop(kk, kb, kt)
            do k = kt + 1, kb + kmxn(kk) - 1
               vnod(k) = vnod(kt)
            end do
         end do
         !$OMP END PARALLEL DO
      end if
   end subroutine links_to_centers_sp

   !> Set flow node value based on flow link values, where vlin and vlin2 are mapped to vnod(1,:) and vnod(2,:), respectively
   subroutine links_to_centers_dp_rank_2(vnod, vlin, vlin2)
      use precision, only: dp
      use m_flow, only: lnkx, ndkx, kmx, kmxn
      use m_flowgeom, only: lnx, ln, wcL, ndx
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_Lbot_Ltop, only: getLbotLtop
      use precision, only: dp

      real(kind=dp), dimension(2, ndkx), intent(out) :: vnod
      real(kind=dp), intent(in) :: vlin(lnkx)
      real(kind=dp), intent(in) :: vlin2(lnkx)
      integer :: L, k1, k2, LL, Lb, Lt, kk, kb, kt, k

      vnod = 0.0_dp
      if (kmx == 0) then
         do L = 1, lnx
            k1 = ln(1, L); k2 = ln(2, L)
            vnod(1, k1) = vnod(1, k1) + vlin(L) * wcL(1, L)
            vnod(1, k2) = vnod(1, k2) + vlin(L) * wcL(2, L)
            vnod(2, k1) = vnod(2, k1) + vlin2(L) * wcL(1, L)
            vnod(2, k2) = vnod(2, k2) + vlin2(L) * wcL(2, L)
         end do
      else
         do LL = 1, lnx
            call getLbotLtop(LL, Lb, Lt)
            do L = Lb, Lt
               k1 = ln(1, L); k2 = ln(2, L)
               vnod(1, k1) = vnod(1, k1) + vlin(L) * wcL(1, LL)
               vnod(1, k2) = vnod(1, k2) + vlin(L) * wcL(2, LL)
               vnod(2, k1) = vnod(2, k1) + vlin2(L) * wcL(1, LL)
               vnod(2, k2) = vnod(2, k2) + vlin2(L) * wcL(2, LL)
            end do
         end do

         !$OMP PARALLEL DO &
         !$OMP PRIVATE(kk,kb,kt,k)
         do kk = 1, ndx
            call getkbotktop(kk, kb, kt)
            do k = kt + 1, kb + kmxn(kk) - 1
               vnod(1, k) = vnod(1, kt)
               vnod(2, k) = vnod(2, kt)
            end do
         end do
         !$OMP END PARALLEL DO
      end if
   end subroutine links_to_centers_dp_rank_2
end module m_links_to_centers
