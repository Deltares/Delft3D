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

!> compute growth velocity vectors at grid points
subroutine comp_vel(mc, xc, yc, edgevel, vel)
   use m_missing, only: dmiss, dxymis
   use m_sferic
   use m_spline2curvi, only: dtolLR
   use geometry_module, only: dbdistance, normalout
   use m_get_lr

   implicit none

   integer, intent(in) :: mc !< number of grid points
   double precision, dimension(mc), intent(in) :: xc, yc !< coordinates of grid points
   double precision, dimension(mc - 1), intent(in) :: edgevel !< edge normal-velocity (spherical: coordinates in meters)
   double precision, dimension(2, mc), intent(out) :: vel !< velocity vectors at grid points (spherical: spherical coordinates)
   
   double precision, dimension(2) :: nL, nR, vL, vR
   double precision :: cosphi, vR_vL, Rai
   integer :: i, iL, iR
   double precision, parameter :: dtolcos = 1d-8 ! not the module variable

   vel = DMISS

   Rai = 1d0 / Ra

   do i = 1, mc
      if (xc(i) == DMISS .or. yc(i) == DMISS) cycle

!     first, compute the normal vector

!     grid nodes may be on top of each other: find left neighboring node
      call get_LR(mc, xc, yc, i, iL, iR)

!     check if the right and left neighboring nodes are not on top of each other
      if (dbdistance(xc(iL), yc(iL), xc(iR), yc(iR), jsferic, jasfer3D, dmiss) <= dtolLR) then
         cycle
      end if

!     check for one-sided differentials
      if (dbdistance(xc(iL), yc(iL), xc(i), yc(i), jsferic, jasfer3D, dmiss) <= dtolLR .or. &
          dbdistance(xc(iR), yc(iR), xc(i), yc(i), jsferic, jasfer3D, dmiss) <= dtolLR) then
         call normalout(xc(iR), yc(iR), xc(iL), yc(iL), nL(1), nL(2), jsferic, jasfer3D, dmiss, dxymis)

         if (jsferic == 1) then
            nL(1) = nL(1) * cos(dg2rd * 0.5d0 * (yc(iL) + yc(iR)))
         end if

         nR = nL
      else
         call normalout(xc(i), yc(i), xc(iL), yc(iL), nL(1), nL(2), jsferic, jasfer3D, dmiss, dxymis)
         call normalout(xc(iR), yc(iR), xc(i), yc(i), nR(1), nR(2), jsferic, jasfer3D, dmiss, dxymis)
!         dnormal = (hL+hR) / (1d0+dot_product(hL,hR)+1d-8)

         if (jsferic == 1) then
            nL(1) = nL(1) * cos(dg2rd * 0.5d0 * (yc(iL) + yc(i)))
            nR(1) = nR(1) * cos(dg2rd * 0.5d0 * (yc(iR) + yc(i)))
         end if

      end if

!     compute the growth velocity vector

!        circularly connected grid
      if (iL == mc) then
         cycle
      end if
      cosphi = dot_product(nL, nR)
      vL = edgevel(iL) * nL
      vR = edgevel(iR - 1) * nR
      vR_vL = edgevel(iR - 1) / edgevel(iL)

      if (cosphi < -1d0 + dtolcos) then
         continue
         cycle
      end if

      if (cosphi < 0d0) then
         continue
      end if

      if ((vR_vL > cosphi .and. 1d0 / vR_vL > cosphi) .or. cosphi <= dtolcos) then
         vel(:, i) = ((1d0 - vR_vL * cosphi) * vL + (1d0 - (1d0 / vR_vL) * cosphi) * vR) / (1d0 - cosphi**2)
      else if (vR_vL < cosphi) then
         vel(:, i) = vR_vL / cosphi * vL
      else
         vel(:, i) = 1d0 / (vR_vL * cosphi) * vR
      end if

!     spherical coordinates
      if (jsferic == 1) then
         vel(1, i) = vel(1, i) * Rai * rd2dg / cos(dg2rd * yc(i))
         vel(2, i) = vel(2, i) * Rai * rd2dg
      end if
   end do

   return
end subroutine comp_vel
