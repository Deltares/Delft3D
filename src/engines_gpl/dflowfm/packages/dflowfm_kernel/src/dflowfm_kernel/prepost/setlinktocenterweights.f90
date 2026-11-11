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
!> @file setlinktocenterweights.f90
!! Subroutine for allocating center related link x- and y weights.
module m_setlinktocenterweights
   use precision, only: dp

   implicit none

   public :: setlinktocenterweights

   real(kind=dp), dimension(:), allocatable :: area_normalization

contains

   !> set center related linkxy weights
   subroutine setlinktocenterweights()
      use m_flow
      use m_netw
      use m_flowgeom
      use m_sferic
      use m_lin2nodx, only: lin2nodx
      use m_lin2nody, only: lin2nody
      use network_data, only: LINK_1D2D_INTERNAL, LINK_1D2D_STREETINLET
      use m_alloc, only: realloc

      real(kind=dp) :: wud, wuL1, wuL2, cs, sn
      integer :: L, n, kk, n12, lnxmax
      integer :: k1, k2, LL
      real(kind=dp) :: aa1, wcw, alf

      wcx1 = 0
      wcy1 = 0
      wcx2 = 0
      wcy2 = 0
      wcL = 0
      wcxy = 0
      wc = 0
      call realloc(area_normalization, ndx, keepExisting=.false., fill=0.0_dp)

      do L = 1, lnx

         if (kcu(L) == LINK_1D2D_INTERNAL) then
            cycle ! no contribution from 1D2D internal links
         end if

         k1 = ln(1, L)
         k2 = ln(2, L) !left and right node
         wud = wu(L) * dx(L) !flow surface area at link

         wuL1 = acl(L) * wud ! 2d center factor
         wcL(1, L) = wuL1
         wc(k1) = wc(k1) + wuL1

         wuL2 = (1.0_dp - acl(L)) * wud
         wcL(2, L) = wuL2
         wc(k2) = wc(k2) + wuL2

         cs = lin2nodx(L, 1, csu(L), snu(L))
         sn = lin2nody(L, 1, csu(L), snu(L))
         wcx1(L) = cs * wuL1
         wcy1(L) = sn * wuL1

         cs = lin2nodx(L, 2, csu(L), snu(L))
         sn = lin2nody(L, 2, csu(L), snu(L))
         wcx2(L) = cs * wuL2
         wcy2(L) = sn * wuL2

         if (kcu(L) == LINK_1D2D_STREETINLET .and. k1 <= ndx2d) then
            area_normalization(k1) = area_normalization(k1) + wuL1
         else
            wcxy(1, k1) = wcxy(1, k1) + abs(wcx1(L))
            wcxy(2, k1) = wcxy(2, k1) + abs(wcy1(L))
         end if

         if (kcu(L) == LINK_1D2D_STREETINLET .and. k2 <= ndx2d) then
            area_normalization(k2) = area_normalization(k2) + wuL2
         else
            wcxy(1, k2) = wcxy(1, k2) + abs(wcx2(L))
            wcxy(2, k2) = wcxy(2, k2) + abs(wcy2(L))
         end if
      end do

      ! correct for the area of 1D2D street inlet links, 
      where (area_normalization > 0.0_dp)
         area_normalization = 1.0_dp / (1.0_dp + area_normalization * bai) ! = ba / (ba + sum(pipe_areas), to ensure the weightfactors sum to 1.
      end where

      lnxmax = 0
      do n = 1, mxwalls ! wall contribution to scalar linktocenterweights
         k1 = walls(1, n)
         aa1 = 2.0_dp * walls(17, n)
         wcw = 0.0_dp
         lnxmax = max(lnxmax, nd(k1)%lnx)
         call realloc(wwL, lnxmax, keepExisting=.false.)
         do kk = 1, size(nd(k1)%ln)
            LL = abs(nd(k1)%ln(kk))
            n12 = 1
            alf = acL(LL)
            if (k1 /= ln(1, LL)) then
               n12 = 2
               alf = 1.0_dp - acL(LL)
            end if
            wuL1 = alf * dx(LL) * wu(LL)
            cs = walls(8, n) ! outward positive
            sn = -walls(7, n)
            wwL(kk) = abs(cs * csu(LL) + sn * snu(LL))
            wwL(kk) = wwL(kk) * wuL1
            wcw = wcw + wwL(kk)
         end do
         if (wcw > 0.0_dp) then
            wc(k1) = wc(k1) + aa1
            do kk = 1, size(nd(k1)%ln)
               LL = abs(nd(k1)%ln(kk))
               n12 = 1
               alf = acL(LL)
               if (k1 /= ln(1, LL)) then
                  n12 = 2
                  alf = 1.0_dp - acL(LL)
               end if
               wcL(n12, LL) = wcL(n12, LL) + wwL(kk) * aa1 / wcw
            end do
         end if
      end do

      do L = 1, lnx
         k1 = ln(1, L)
         k2 = ln(2, L)
         if (abs(kcu(L)) == 2 .or. abs(kcu(L)) == 4) then ! 2D links and 1D2D lateral links
            if (kfs(K1) == 0) then ! kfs temporarily used as cutcell flag, set in cutcelwu
               wcx1(L) = wcx1(L) * bai(k1)
               wcy1(L) = wcy1(L) * bai(k1)
            else
               if (wcxy(1, k1) /= 0) then
                  wcx1(L) = wcx1(L) / wcxy(1, k1)
               end if
               if (wcxy(2, k1) /= 0) then
                  wcy1(L) = wcy1(L) / wcxy(2, k1)
               end if
            end if

            if (kfs(K2) == 0) then
               wcx2(L) = wcx2(L) * bai(k2)
               wcy2(L) = wcy2(L) * bai(k2)
            else
               if (wcxy(1, k2) /= 0) then
                  wcx2(L) = wcx2(L) / wcxy(1, k2)
               end if
               if (wcxy(2, k2) /= 0) then
                  wcy2(L) = wcy2(L) / wcxy(2, k2)
               end if
            end if
         else
            wcx1(L) = wcx1(L) * bai(k1)
            wcy1(L) = wcy1(L) * bai(k1)
            wcx2(L) = wcx2(L) * bai(k2)
            wcy2(L) = wcy2(L) * bai(k2)
         end if

         ! correct by area normalization for 1D2D street inlet links
         if (area_normalization(k1) > 0.0_dp) then
            wcx1(L) = wcx1(L) * area_normalization(k1)
            wcy1(L) = wcy1(L) * area_normalization(k1)
         end if
         if (area_normalization(k2) > 0.0_dp) then
            wcx2(L) = wcx2(L) * area_normalization(k2)
            wcy2(L) = wcy2(L) * area_normalization(k2)
         end if

         if (wc(k1) > 0.0_dp) then
            wcL(1, L) = wcL(1, L) / wc(k1)
         end if
         if (wc(k2) > 0.0_dp) then
            wcL(2, L) = wcL(2, L) / wc(k2)
         end if

      end do

      if (Perot_weight_update == PEROT_STATIC) then
         if (allocated(wc)) then
            deallocate (wc)
         end if
         if (allocated(wwL)) then
            deallocate (wwL)
         end if
         if (allocated(wcxy)) then
            deallocate (wcxy)
         end if
      end if
   end subroutine setlinktocenterweights
end module m_setlinktocenterweights
