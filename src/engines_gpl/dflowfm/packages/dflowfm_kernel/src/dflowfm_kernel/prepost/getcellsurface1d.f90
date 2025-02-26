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
!> @file getcellsurface1d.f90
!! Subroutines to compute bottom are of a cell for 1D.

module m_getcellsurface1d

   implicit none

   private

   public :: getcellsurface1d

contains

   !> Computes the bottom area of a cell for 1d coordinates.
   subroutine getcellsurface1d(ba, bai)

      use m_flowgeom, only: n1Dend, lnx, ndx2d, dx, wu, ln, lnxi, mx1dend, kcu, ndx1Db, ndx
      use precision, only: dp

      implicit none

      real(kind=dp), dimension(ndx), intent(inout) :: ba
      real(kind=dp), dimension(ndx), intent(inout) :: bai

      integer L
      integer k1
      integer k2
      integer k
      integer n
      real(kind=dp) :: hdx

      do L = 1, lnx ! for all 1d links, set area at neighbouring flow nodes to zero
         if (kcu(L) == 1 .or. kcu(L) == -1 .or. kcu(L) == 4 .or. kcu(L) == 5 .or. kcu(L) == 7) then
            k1 = ln(1, L)
            k2 = ln(2, L)
            if (k1 > ndx2d) ba(k1) = 0
            if (k2 > ndx2d) ba(k2) = 0
         end if
      end do

      do L = 1, lnx ! for all 1d links, add half the flowlink length*width to the neighbouring flow nodes
         if (kcu(L) == 1 .or. kcu(L) == -1 .or. kcu(L) == 4 .or. kcu(L) == 5 .or. kcu(L) == 7) then
            ! TODO: UNST-6592: consider excluding ghost links here and do an mpi_allreduce sum later
            hdx = 0.5d0 * dx(L)
            k1 = ln(1, L)
            k2 = ln(2, L)
            if (k1 > ndx2d) ba(k1) = ba(k1) + hdx * wu(L) ! todo, on 1d2d nodes, choose appropriate wu1DUNI = min ( wu1DUNI, intersected 2D face)
            if (k2 > ndx2d) ba(k2) = ba(k2) + hdx * wu(L)
         end if
      end do

      ! set outside flow node area at 1d boundary links to inside flow area
      do L = lnxi + 1, Lnx
         k1 = ln(1, L)
         k2 = ln(2, L)
         ba(k1) = ba(k2) ! set bnd ba to that of inside point
      end do

      ! for hanging 1d nodes set the value to twice the original
      do k = 1, mx1Dend
         k1 = n1Dend(k)
         ba(k1) = 2d0 * ba(k1)
      end do

      ! compute inverse area of 1D nodes
      do n = ndx2D + 1, ndx1Db
         if (ba(n) > 0d0) then
            bai(n) = 1d0 / ba(n) ! initially, ba based on 'max wet envelopes', take bai used in linktocentreweights
         end if
      end do

   end subroutine getcellsurface1d

end module m_getcellsurface1d
