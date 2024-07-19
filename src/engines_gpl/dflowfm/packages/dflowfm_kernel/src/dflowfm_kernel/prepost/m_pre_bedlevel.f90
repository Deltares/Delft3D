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

!> Contains subroutines for preprocessing bed level
module m_pre_bedlevel

   implicit none

   private !Prevent used modules from being exported

   public :: extrapolate_bedlevel_at_boundaries

contains

!> Extrapolates the bed level at the boundaries.
!! This is necessary for obtaining the right `hu` for
!! analytical cases.
   subroutine extrapolate_bedlevel_at_boundaries()

      use m_flowgeom, only: bl, dxi, csu, snu

      use fm_external_forcings_data, only: kbndz, kbndu, nbndz, nbndu

      implicit none

      integer :: L, k1, k2, kb
      double precision, allocatable :: dzdx(:), dzdy(:)

      call bed_slope_at_z(dzdx, dzdy)

!water level boundaries
!
!we want that at an H-boundary the bed level of the ghost cell is
!the correct one, as if the sloping bed would continue. In this
!way, when computing the mean of the bed levels at cell centres,
!the elevation at the dowsntream link will be the correct one and
!`hu` will be correct if the water level at the cell centre of the
!ghost cell (i.e., using `izbndpos=0` is `\Delta x` down from the
!correct value. Note that it is `\Delta x` and not `\Delta x/2`.
      do kb = 1, nbndz
         k1 = kbndz(1, kb)
         k2 = kbndz(2, kb)
         L = kbndz(3, kb)

         !independently of whether the H-boundary is upstream or downstream, we always add.
         bl(k1) = bl(k2) + csu(L) * dzdx(k2) / dxi(L) + snu(L) * dzdy(k2) / dxi(L)

      end do !kb

!velocity boundaries
!
!we want that at a U-boundary the bed level of the ghost cell is
!the same as that of the second-inside cell. In this way, when
!computing the mean of the bed levels at cell centres, the elevation
!at the upstream link will be the same as that of the second link.
!As the water level of the first inside cell is (1) downwinded for
!computing `hu` at the second link and (2) copied to the upstream
!ghost cell, we will obtain the same correct `hu` at the first link.
!
!For a case of a sloping bed in the positive x direction and a u-boundary
!imposed upstream, `dxz` will be negative upstream, hence obtaining the
!elevation of the second inside cell.
      do kb = 1, nbndu
         k1 = kbndu(1, kb)
         k2 = kbndu(2, kb)
         L = kbndz(3, kb)

         !The elevation will depend on whether flow is towards the model (upstream BC is Q) or
         !out of the model (downstream BC is Q). As this extrapolation is only useful for analytical
         !solution, we assume flow is towards the model.

         !if flow is towards the model:
         bl(k1) = bl(k2) + csu(L) * dzdx(k2) / dxi(L) + snu(L) * dzdy(k2) / dxi(L)

         !if flow is out of the model:
         !bl(k1)=bl(k2)+bedslopex/dxi(L)*csu(L)-bedslopey/dxi(L)*snu(L)
      end do

!deallocate
      if (allocated(dzdx)) deallocate (dzdx)
      if (allocated(dzdy)) deallocate (dzdy)

   end subroutine extrapolate_bedlevel_at_boundaries

!> Compute the bed slope at cell centres.
   subroutine bed_slope_at_z(dzdx, dzdy)

      use m_flowgeom, only: ln, bl, dxi, ndx, lnxi, csu, snu

      implicit none

      integer :: istat
      integer :: L, k1, k2

      double precision, allocatable, intent(out) :: dzdx(:), dzdy(:)

      allocate (dzdx(1:ndx), dzdy(1:ndx), stat=istat)

      dzdx = 0d0; dzdy = 0d0
!only internal links because the bed level at the ghost is wrong (mirrored)
      do L = 1, lnxi
         k1 = ln(1, L); k2 = ln(2, L)
         dzdx(k1) = dzdx(k1) - csu(L) * (bl(k2) - bl(k1)) * dxi(L)
         dzdy(k1) = dzdy(k1) - snu(L) * (bl(k2) - bl(k1)) * dxi(L)
         dzdx(k2) = dzdx(k2) - csu(L) * (bl(k2) - bl(k1)) * dxi(L)
         dzdy(k2) = dzdy(k2) - snu(L) * (bl(k2) - bl(k1)) * dxi(L)
      end do

   end subroutine bed_slope_at_z

end module m_pre_bedlevel
