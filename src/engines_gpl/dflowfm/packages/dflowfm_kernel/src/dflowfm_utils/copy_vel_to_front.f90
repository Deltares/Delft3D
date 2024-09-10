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

!> copy growth velocities to the front, and add points in the front at corners
subroutine copy_vel_to_front(mc, j, vel, ifront, nf, numf, xf, yf, velf, idxf)
   use m_missing
   use m_qnerror

   implicit none

   integer, intent(in) :: mc !< number of grid points
   integer, intent(in) :: j !< grid layer

   double precision, dimension(2, mc), intent(in) :: vel !  growth velocity vector at grid layer, per node

   integer, dimension(mc), intent(inout) :: ifront !< active nodes (1) or not (0)

   integer, intent(inout) :: nf !< front dimension
   integer, intent(in) :: numf !< array size
   double precision, dimension(numf), intent(inout) :: xf, yf !< front point coordinates
   double precision, dimension(2, numf), intent(inout) :: velf !< front growth velocity vectors
   integer, dimension(2, numf), intent(inout) :: idxf !< (i,j)-indices of front points
   integer :: i, ii, iprev, jprev, inext, jnext, num

   logical :: LL, LR

   velf = 0d0

   num = 0 ! number of cornernodes (for ouput purposes only)
   i = 0
   do while (i < nf)
      i = i + 1
      if (idxf(2, i) == j .and. ifront(idxf(1, i)) == 1) then
         velf(:, i) = vel(:, idxf(1, i))
         if (velf(1, i) == DMISS) velf(:, i) = 0d0

!        check for cornernodes
         iprev = idxf(1, max(i - 1, 1))
         jprev = idxf(2, max(i - 1, 1))
         inext = idxf(1, min(i + 1, nf))
         jnext = idxf(2, min(i + 1, nf))

         LL = (iprev == idxf(1, i) - 1 .and. jprev == idxf(2, i) .and. ifront(iprev) == 0)
         LR = (inext == idxf(1, i) + 1 .and. jnext == idxf(2, i) .and. ifront(inext) == 0)
         LL = LL .or. (iprev == idxf(1, i) .and. jprev < idxf(2, i))
         LR = LR .or. (inext == idxf(1, i) .and. jnext < idxf(2, i))
         if (LL .or. LR) then ! stationary edge
            num = num + 1
            if (nf + 1 > numf) then
               call qnerror('growlayer: numf too small', ' ', ' ')
               cycle
            end if
!            if ( num.eq.1 ) write(6,"('cornernode: ', $)")
!            write (6,"(I0, ' ', $)") idxf(1,i)
            do ii = nf, i, -1
               xf(ii + 1) = xf(ii)
               yf(ii + 1) = yf(ii)
               velf(:, ii + 1) = velf(:, ii)
               idxf(:, ii + 1) = idxf(:, ii)
            end do
            nf = nf + 1
            if (LL) then
               velf(:, i) = 0d0
            else
               velf(:, i + 1) = 0d0
            end if
            i = i + 1
         end if
      end if
   end do

!   if ( num.gt.0 ) write(6,*)

   return
end subroutine copy_vel_to_front
