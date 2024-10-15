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

!> merge grids from spline2curvi
subroutine merge_spline2curvigrids()
   use m_grid
   use m_alloc
   use m_missing
   use geometry_module, only: dbdistance, get_startend
   use m_sferic, only: jsferic, jasfer3D
   use m_increase_grid

   implicit none

   double precision, dimension(:, :), allocatable :: xcnew, ycnew

   integer, dimension(2) :: iupperold, ilowerold, iupper, ilower

   integer :: i, iother, ipoint
   integer :: istart, iend, jstart, jend, jDj
   integer :: istartother, iendother, jstartother, jendother
   integer :: jmin, jmax, jminother, jmaxother
   integer :: istartnew, iendnew

   logical :: Lconnected

   double precision, parameter :: dtol = 1d-6

!  allocate
   allocate (xcnew(1, 1), ycnew(1, 1))

   ipoint = 1
   jDj = 0
   istartnew = 1
   do while (ipoint < mc)
!     get start and end indices of this part of the first gridline
      call get_startend(mc - ipoint + 1, xc(ipoint:mc, 1), yc(ipoint:mc, 1), istart, iend, dmiss)
      istart = istart + ipoint - 1
      iend = iend + ipoint - 1

      if (istart >= mc .or. istart >= iend) then ! done
         exit
      end if

!     see if this part is connected to another part
      istartother = iend + 2
      iendother = istartother + (iend - istart)
      Lconnected = .true.
      jmin = nc + 1
      jmax = 0
      jminother = nc + 1
      jmaxother = 0
      do i = istart, iend
!        get the grid sizes in j-direction
         call get_startend(nc, xc(i, 1:nc), yc(i, 1:nc), jstart, jend, dmiss)
         jmin = min(jmin, jstart)
         jmax = max(jmax, jend)

         iother = iend + 2 + (iend - i)

         if (iother > mc) then ! no more grid available
            Lconnected = .false.
         else
            if (dbdistance(xc(i, 1), yc(i, 1), xc(iother, 1), yc(iother, 1), jsferic, jasfer3D, dmiss) > dtol) then ! not on top of each other
               Lconnected = .false.
            else
!              get the grid sizes in j-direction, other side
               call get_startend(nc, xc(iother, 1:nc), yc(iother, 1:nc), jstartother, jendother, dmiss)
               jminother = min(jminother, jstartother) ! should be 1
               jmaxother = max(jmaxother, jendother)
            end if
         end if
      end do

      iendnew = istartnew + iend - istart

      if (Lconnected) then
         ipoint = iendother + 2
      else
         jmaxother = 1
         ipoint = iend + 2
      end if

!     reallocate
      iupperold = ubound(xcnew)
      ilowerold = lbound(xcnew)
      iupper = (/iendnew, max(jmax, iupperold(2))/)
      ilower = (/1, min(2 - jmaxother, ilowerold(2))/)
      call realloc(xcnew, iupper, ilower, keepExisting=.true., fill=DMISS)
      call realloc(ycnew, iupper, ilower, keepExisting=.true., fill=DMISS)

!     fill
      xcnew(istartnew:iendnew, 1:jmax) = xc(istart:iend, 1:jmax)
      ycnew(istartnew:iendnew, 1:jmax) = yc(istart:iend, 1:jmax)
      if (Lconnected) then
         xcnew(istartnew:iendnew, 1:2 - jmaxother:-1) = xc(iendother:istartother:-1, 1:jmaxother)
         ycnew(istartnew:iendnew, 1:2 - jmaxother:-1) = yc(iendother:istartother:-1, 1:jmaxother)
      end if
      istartnew = iendnew + 2
   end do

!  increase grid
   iupper = ubound(xcnew)
   ilower = lbound(xcnew)
   mc = iupper(1) - ilower(1) + 1
   nc = iupper(2) - ilower(2) + 1
   call increasegrid(mc, nc)

!  fill
   xc = DMISS
   yc = DMISS
   xc(1:mc, 1:nc) = xcnew
   yc(1:mc, 1:nc) = ycnew

!  deallocate
   if (allocated(xcnew)) deallocate (xcnew)
   if (allocated(ycnew)) deallocate (ycnew)

   return
end subroutine merge_spline2curvigrids
