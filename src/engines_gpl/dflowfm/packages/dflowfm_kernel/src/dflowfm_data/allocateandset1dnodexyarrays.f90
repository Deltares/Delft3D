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

module m_allocateandset1dnodexyarrays
   use m_dcross, only: dcross

   implicit none

contains

   subroutine allocateandset1Dnodexyarrays(n) ! only for plotting ....
      use precision, only: dp
      use m_netw
      use m_flowgeom
      use m_sferic
      use m_missing
      use geometry_module, only: getdx, getdy
      use gridoperations
      implicit none

      integer :: n, LL

      ! locals
      integer :: m, k, no, L, La, n1, n2, ierr, JACROS
      real(kind=dp) :: x1, y1, x2, y2, hwu, cs, sn
      real(kind=dp) :: x1a, y1a, x2a, y2a, x1b, y1b, x2b, y2b
      real(kind=dp) :: SL, SM, XCR, YCR, CRP
      integer :: K1, K2
      real(kind=dp) :: dxt, dyt, phi
      integer, allocatable :: linnrs(:)
      real(kind=dp), allocatable :: arglins(:), arglin(:)
      real(kind=dp), allocatable :: xx(:), yy(:)
      integer :: num
      integer :: jausedualnetcell

      jausedualnetcell = 0

      m = 3 * nd(n)%lnx + 1
      if (nd(n)%lnx == 1) m = m + 1

      allocate (nd(n)%x(m), nd(n)%y(m), stat=ierr)
      call aerr('nd(n)%x(m), nd(n)%y(m)', ierr, m * 2)

      ! Sort nd%ln in counterclockwise order
      allocate (linnrs(nd(n)%lnx), arglins(nd(n)%lnx), arglin(nd(n)%lnx))
      do L = 1, nd(n)%lnx
         K1 = LN(1, abs(nd(n)%ln(L))); K2 = LN(2, abs(nd(n)%ln(L)))
         if (K2 == n) then
            K2 = K1
            K1 = n
         end if
         dxt = getdx(xz(k1), yz(k1), xz(k2), yz(k2), jsferic)
         dyt = getdy(xz(k1), yz(k1), xz(k2), yz(k2), jsferic)
         if (abs(dxt) < 1d-14 .and. abs(dyt) < 1d-14) then
            if (dyt < 0) then
               phi = -pi / 2
            else
               phi = pi / 2
            end if
         else
            phi = atan2(dyt, dxt)
         end if
         arglin(L) = phi
      end do

      linnrs = 0
! Do a basic insertion sort (#links is small)
      do L = 1, nd(n)%lnx
         do LL = 1, L - 1
            if (arglin(L) < arglins(LL)) then
               exit
            end if
         end do
         arglins(LL + 1:L) = arglins(LL:L - 1)
         arglins(LL) = arglin(L)
         linnrs(LL + 1:L) = linnrs(LL:L - 1)
         linnrs(LL) = nd(n)%ln(L)
      end do
! links are locally sorted in linnrs

      m = 0
      x1 = xz(n)
      y1 = yz(n)

! For each link, save its 'bottom' side (wrt counterclockwise view) and its tip (i.e., 3 points)
! The intersection between this bottom side and the 'top' side of previous link
! is computed. This yields a good contour line in nd%x/y.

! Use last link to prepare connection for 1st link in following loop:
      La = abs(linnrs(nd(n)%lnx))
      cs = csu(La); sn = snu(La)
      hwu = 0.5d0 * wu(La)
      if (jsferic == 1) then
         hwu = rd2dg * hwu / ra
      end if
      no = ln(2, La)
      if (no == n) then
         no = ln(1, La)
         cs = -cs ! Flip link means: negate normal components
         sn = -sn
      end if
      x1b = x1 - sn * hwu
      y1b = y1 + cs * hwu
      x2b = 0.5 * (x1 + xz(no)) - sn * hwu
      y2b = 0.5 * (y1 + yz(no)) + cs * hwu

      do LL = 1, nd(n)%lnx
         L = linnrs(LL)
         La = abs(L)

         if (kcu(La) /= 1 .and. kcu(La) /= 4) then
            cycle ! Only use real 1D links in the flow node contour, no 1d2d links
         end if

         n1 = ln(1, La); n2 = ln(2, La)
         cs = csu(La); sn = snu(La)
         hwu = 0.5d0 * wu(La)
         if (jsferic == 1) then
            hwu = rd2dg * hwu / ra
         end if
         no = n2 ! N on Other side = no
         if (no == n) then
            no = n1
            cs = -cs
            sn = -sn
         end if
         x2 = 0.5 * (x1 + xz(no))
         y2 = 0.5 * (y1 + yz(no))

         x1a = x1 + sn * hwu
         y1a = y1 - cs * hwu
         x2a = x2 + sn * hwu
         y2a = y2 - cs * hwu

         SL = dmiss; SM = dmiss
         call dCROSS(x1b, y1b, x2b, y2b, x1a, y1a, x2a, y2a, JACROS, SL, SM, XCR, YCR, CRP)
         !if (SL /= dmiss .and. SM /= dmiss) then
         if (JACROS == 1) then
            x1a = xcr
            y1a = ycr
            ! else: parallel, use original x1a, y1a
         end if

         m = m + 1
         nd(n)%x(m) = x1a
         nd(n)%y(m) = y1a

         m = m + 1
         nd(n)%x(m) = x2a
         nd(n)%y(m) = y2a

         x1b = x1 - sn * hwu
         y1b = y1 + cs * hwu
         x2b = x2 - sn * hwu
         y2b = y2 + cs * hwu

         m = m + 1
         nd(n)%x(m) = x2b
         nd(n)%y(m) = y2b

      end do
      ! Only a single link, add fourth (extra) point.
      if (nd(n)%lnx == 1) then
         m = m + 1
         nd(n)%x(m) = x1b
         nd(n)%y(m) = y1b
      end if

      ! For pfiller, close contour
      m = m + 1
      nd(n)%x(m) = nd(n)%x(1)
      nd(n)%y(m) = nd(n)%y(1)
      call realloc(nd(n)%x, m, keepExisting=.true.)
      call realloc(nd(n)%y, m, keepExisting=.true.)
      deallocate (linnrs, arglins, arglin)

      if (jausedualnetcell == 1) then
         k = nd(n)%nod(1)
         m = 10 * nmk(k)
         call realloc(xx, m, keepExisting=.false.)
         call realloc(yy, m, keepExisting=.false.)

         call make_dual_cell(k, m, 1d0, xx, yy, num, Wu1Duni)

         call realloc(nd(n)%x, num + 1, keepExisting=.true.)
         call realloc(nd(n)%y, num + 1, keepExisting=.true.)

         nd(n)%x = (/xx(1:num), xx(1)/)
         nd(n)%y = (/yy(1:num), yy(1)/)
      end if

      !------------------------------------------------------

   end subroutine allocateandset1Dnodexyarrays

end module m_allocateandset1dnodexyarrays
