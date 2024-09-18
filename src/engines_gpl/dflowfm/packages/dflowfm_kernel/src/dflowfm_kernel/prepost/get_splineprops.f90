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

!> derive center spline propererties from cross splines
subroutine get_splineprops(mcs_old, id, iLRmfac)

   use m_splines
   use m_spline2curvi
   use m_alloc
   use m_deallocate_spline_props

   implicit none

   integer, intent(in) :: mcs_old !< number of original splines
   integer, dimension(mcs_old), intent(in) :: id !< original settings
   integer, dimension(3, mcs_old), intent(in) :: iLRmfac !< original settings

   double precision, dimension(:), allocatable :: xlist, ylist

   integer :: i, j, is, js, imiddle, ismiddle
   integer :: num, ncs, numnew

   double precision :: dslength, hsumL, hsumR, hmax

   double precision, external :: splinelength

!  allocate
   allocate (xlist(1), ylist(1))
   if (allocated(splineprops)) call deallocate_splineprops()
   call allocate_splineprops()

   do is = 1, mcs
!     determine the number of control points in the spline
      call nump(is, num)

!     reallocate if necessary
      if (num > ubound(xlist, 1)) then
         numnew = int(1.2d0 * dble(num)) + 1
         call realloc(xlist, numnew)
         call realloc(ylist, numnew)
      end if
      xlist(1:num) = xsp(is, 1:num)
      ylist(1:num) = ysp(is, 1:num)

      splineprops(is)%length = splinelength(num, xlist, ylist)

      call get_crosssplines(num, xlist, ylist, splineprops(is)%ncs, splineprops(is)%ics, splineprops(is)%Lorient, splineprops(is)%t, splineprops(is)%cosphi)
   end do ! do is=1,mcs

!  determine whether a spline is a center spline or a bounding spline

!  first, select all non-cross splines only
   do is = 1, mcs
      call nump(is, num)
      if (num > 2) then
         splineprops(is)%id = 0
      else
         splineprops(is)%id = 1
      end if
   end do

! then, check the cross splines; the center spline is the middle spline that crosses the cross spline
   do js = 1, mcs
      call nump(js, num)
      if (num /= 2) cycle ! cross splines only

      ncs = splineprops(js)%ncs

      if (ncs < 1) cycle

!     determine the middle
      imiddle = min(ncs / 2 + 1, ncs)
      ismiddle = splineprops(js)%ics(imiddle)

!     ncs is even: check if the middle spline has already been assigned as a bounding spline
      if (splineprops(ismiddle)%id /= 0 .and. 2 * (imiddle - 1) == ncs) then
         imiddle = min(imiddle + 1, ncs)
         ismiddle = splineprops(js)%ics(imiddle)
      end if

      if (splineprops(ismiddle)%id == 0) then
!        associate bounding splines with the middle spline
         ismiddle = splineprops(js)%ics(imiddle)
         do i = 1, imiddle - 1
            is = splineprops(js)%ics(i)
            splineprops(is)%id = -ismiddle
         end do
         do i = imiddle + 1, ncs
            is = splineprops(js)%ics(i)
            splineprops(is)%id = -ismiddle
         end do
      end if
   end do

!  restore original splines
   if (mcs_old > 0) then
      do is = 1, mcs_old
         splineprops(is)%id = id(is)
         splineprops(is)%iL = iLRmfac(1, is)
         splineprops(is)%iR = iLRmfac(2, is)
         splineprops(is)%mfac = iLRmfac(3, is)
      end do

!     mark new splines as artificial cross splines
      do is = mcs_old + 1, mcs
         splineprops(is)%id = 3
!        deactivate crosssplines that are not nearly orthogonal to the center spline(s)
!         do j=1,splineprops(is)%ncs
!            js = splineprops(is)%ics(j)
!            cosphi = splineprops(is)%cosphi(js)
!            if ( cosphi.ne.DMISS .and. abs(cosphi).lt.0.95d0 ) then
!               exit
!            end if
!         end do
      end do
   end if

!  get the grid heights
   call get_heights()

!  determine maximum grid height for this spline
   do is = 1, mcs
      ncs = splineprops(is)%ncs
      dslength = splineprops(is)%length
      if (ncs == 0) then
         splineprops(is)%hmax = daspect * dslength
      else
         hmax = 0d0
         do i = 1, ncs
            hsumL = 0d0
            hsumR = 0d0
            do j = 1, splineprops(is)%NsubL(i)
               hsumL = hsumL + splineprops(is)%hL(j, i)
            end do
            do j = 1, splineprops(is)%NsubR(i)
               hsumR = hsumR + splineprops(is)%hR(j, i)
            end do
            hmax = max(hmax, max(hsumL, hsumR))
         end do
         splineprops(is)%hmax = hmax
      end if
   end do

!  deallocate
   deallocate (xlist, ylist)

   return
end subroutine get_splineprops
