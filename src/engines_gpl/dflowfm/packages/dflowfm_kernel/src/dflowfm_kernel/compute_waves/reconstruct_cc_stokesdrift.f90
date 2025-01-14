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

module m_reconstruct_cc_stokesdrift

   implicit none

   private

   public :: reconstruct_cc_stokesdrift

contains

   subroutine reconstruct_cc_stokesdrift(ndkx, ust_x, ust_y)
      use precision, only: dp
      use m_flowgeom, only: lnx, ln, wcx1, wcx2, wcy1, wcy2
      use m_flow, only: kmx
      use m_waves, only: ustokes
      use m_get_Lbot_Ltop_max

      implicit none

      ! Input variables
      integer, intent(in) :: ndkx
      real(kind=dp), dimension(ndkx), intent(out) :: ust_x, ust_y

      ! Local variables
      integer :: L, LL, Lb, Lt, k1, k2
      real(kind=dp) :: ustL

      ust_x = 0d0; ust_y = 0d0

      if (kmx == 0) then
         do L = 1, lnx
            k1 = ln(1, L); k2 = ln(2, L)
            ustL = ustokes(L)
            ust_x(k1) = ust_x(k1) + wcx1(L) * ustL
            ust_x(k2) = ust_x(k2) + wcx2(L) * ustL
            ust_y(k1) = ust_y(k1) + wcy1(L) * ustL
            ust_y(k2) = ust_y(k2) + wcy2(L) * ustL
         end do
      else
         do LL = 1, lnx
            call getLbotLtopmax(LL, Lb, Lt)
            do L = Lb, Lt
               k1 = ln(1, L); k2 = ln(2, L)
               ustL = ustokes(L)
               ust_x(k1) = ust_x(k1) + wcx1(LL) * ustL
               ust_x(k2) = ust_x(k2) + wcx2(LL) * ustL
               ust_y(k1) = ust_y(k1) + wcy1(LL) * ustL
               ust_y(k2) = ust_y(k2) + wcy2(LL) * ustL
            end do
         end do
      end if
   end subroutine reconstruct_cc_stokesdrift

end module m_reconstruct_cc_stokesdrift
