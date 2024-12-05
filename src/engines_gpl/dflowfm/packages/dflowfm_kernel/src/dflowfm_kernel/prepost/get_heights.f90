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

!> get the grid heights from the cross spline information
subroutine get_heights()
   use precision, only: dp
   use m_comp_subheights
   use m_splines
   use m_spline2curvi
   use m_missing
   use m_alloc

   implicit none

   integer :: is, j, js, k, kk, ks, kks, ncs, num, numj
   integer :: kL, kR ! left and right neighboring splines at the cross spline w.r.t. the center spline
   integer :: numnew
   real(kind=dp), dimension(:), allocatable :: xlist, ylist
   logical :: Lorient
   real(kind=dp), external :: splinelength_int

!  allocate
   allocate (xlist(1), ylist(1))

   do is = 1, mcs
!     determine the number of control points in the spline
      call nump(is, num)
      if (num <= 2) cycle ! center splines only

      do j = 1, splineprops(is)%ncs
         js = splineprops(is)%ics(j)

         call nump(js, numj)

         ncs = splineprops(js)%ncs

!        for this cross spline, find the left and right neighboring splines w.r.t. the center spline
         kL = 0
         kR = 0
         do k = 1, ncs
            ks = splineprops(js)%ics(k)
            if (ks == is) then
!               if ( k.gt.1 )   kL = splineprops(js)%ics(k-1)
!               if ( k.lt.ncs ) kR = splineprops(js)%ics(k+1)

               do kk = k - 1, 1, -1
                  kks = splineprops(js)%ics(kk)
                  if (splineprops(kks)%id == -ks) then
                     kL = kks
                     exit
                  end if
               end do

               do kk = k + 1, ncs
                  kks = splineprops(js)%ics(kk)
                  if (splineprops(kks)%id == -ks) then
                     kR = splineprops(js)%ics(kk)
                     exit
                  end if
               end do

               exit
            end if
         end do

         Lorient = splineprops(is)%Lorient(j) ! orientation of the cross spline

!        reallocate if necessary
         if (numj > ubound(xlist, 1)) then
            numnew = int(1.2d0 * dble(numj)) + 1
            call realloc(xlist, numnew)
            call realloc(ylist, numnew)
         end if

         xlist(1:numj) = xsp(js, 1:numj)
         ylist(1:numj) = ysp(js, 1:numj)

         call comp_subheights(is, Lorient, numj, xlist, ylist, &
                              splineprops(js)%ncs, splineprops(js)%ics, splineprops(js)%t, &
                              splineprops(is)%NsubL(j), splineprops(is)%NsubR(j), splineprops(is)%hL(:, j), splineprops(is)%hR(:, j))
      end do
   end do

!  deallocate
   deallocate (xlist, ylist)

   return
end subroutine get_heights
