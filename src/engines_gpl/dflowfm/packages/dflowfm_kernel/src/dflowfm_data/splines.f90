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
!
module m_splines
   use precision, only: dp
   use m_missing, only: dxymis
   use m_readyy

   implicit none

   real(kind=dp), dimension(:, :), allocatable :: xsp, ysp, xsp2, ysp2
   integer, allocatable :: lensp(:), lensp2(:) !< Length of each spline

   integer :: spline_capacity = 5 !< Number of splines that storage can accommodate.
   integer :: points_per_spline_capacity = 10 !< Number of points that storage can accommodate per spline.
   integer :: mcs = 0 !< Current nr. of splines
   integer :: mcs2 = 0 !< Current nr. of backup splines
   integer :: mp = 0 !< Nr. of active spline.
   integer :: np = 0 !< Nr. of active spline point.
contains

!> Ensures storage for the requested number of splines and points per spline.
   subroutine resize_spline_capacity(required_splines, required_points_per_spline)
      use m_alloc, only: realloc, aerr

      implicit none

      integer, value, intent(in) :: required_splines !< Minimum number of splines to accommodate.
      integer, value, intent(in) :: required_points_per_spline !< Minimum number of points to accommodate per spline.

      integer :: ierr

      spline_capacity = max(10, spline_capacity)
      points_per_spline_capacity = max(100, points_per_spline_capacity)
      if (allocated(xsp)) then
         spline_capacity = max(spline_capacity, size(xsp, 1))
         points_per_spline_capacity = max(points_per_spline_capacity, size(xsp, 2))
      end if
      if (required_splines > spline_capacity) then
         spline_capacity = int(1.2_dp * required_splines)
      end if
      if (required_points_per_spline > points_per_spline_capacity) then
         points_per_spline_capacity = int(1.2_dp * required_points_per_spline)
      end if

      associate (coordinate_shape => [spline_capacity, points_per_spline_capacity])
         if (allocated(xsp) .and. allocated(ysp) .and. allocated(xsp2) .and. allocated(ysp2) .and. &
             allocated(lensp) .and. allocated(lensp2)) then
            if (all(shape(xsp) == coordinate_shape) .and. all(shape(ysp) == coordinate_shape) .and. &
                all(shape(xsp2) == coordinate_shape) .and. all(shape(ysp2) == coordinate_shape) .and. &
                size(lensp) == spline_capacity .and. size(lensp2) == spline_capacity) then
               return
            end if
         end if

         call realloc(xsp, coordinate_shape, stat=ierr, fill=dxymis)
         call aerr('xsp(spline_capacity, points_per_spline_capacity)', ierr, product(coordinate_shape))
         call realloc(ysp, coordinate_shape, stat=ierr, fill=dxymis)
         call aerr('ysp(spline_capacity, points_per_spline_capacity)', ierr, product(coordinate_shape))
         call realloc(xsp2, coordinate_shape, stat=ierr, fill=dxymis)
         call aerr('xsp2(spline_capacity, points_per_spline_capacity)', ierr, product(coordinate_shape))
         call realloc(ysp2, coordinate_shape, stat=ierr, fill=dxymis)
         call aerr('ysp2(spline_capacity, points_per_spline_capacity)', ierr, product(coordinate_shape))
      end associate
      call realloc(lensp, spline_capacity, stat=ierr, fill=0)
      call aerr('lensp(spline_capacity)', ierr, spline_capacity / 2)
      call realloc(lensp2, spline_capacity, stat=ierr, fill=0)
      call aerr('lensp2(spline_capacity)', ierr, spline_capacity / 2)

   end subroutine resize_spline_capacity

   subroutine nump(m, npts)
      integer, intent(in) :: m !< Index of spline
      integer, intent(out) :: npts !< Nr. of points in spline #i

      if (m > mcs) then
         npts = -1
         return
      end if
      npts = lensp(m)
   end subroutine nump

   subroutine saveSplines()
      xsp2 = xsp
      ysp2 = ysp
      lensp2 = lensp
      mcs2 = mcs
   end subroutine saveSplines

   subroutine restoreSplines()
      xsp = xsp2
      ysp = ysp2
      lensp = lensp2
      mcs = mcs2
   end subroutine restoreSplines

   subroutine newSpline(len)
      integer, optional, intent(in) :: len
      if (present(len)) then
         call resize_spline_capacity(mcs + 1, len)
      else
         call resize_spline_capacity(mcs + 1, required_points_per_spline=0)
      end if
      mcs = mcs + 1
      if (present(len)) then
         lensp(mcs) = 0 ! len
      else
         lensp(mcs) = 0
      end if

   end subroutine newSpline

   subroutine setSplinePoint(m, n, xp, yp)
      use precision, only: dp
      integer, intent(in) :: m, n
      real(kind=dp), intent(in) :: xp, yp
      xsp(m, n) = xp
      ysp(m, n) = yp
   end subroutine setSplinePoint

   subroutine insertSplinePoint(m, n, xp, yp)
      use precision, only: dp
      integer, intent(in) :: m, n
      real(kind=dp), intent(in) :: xp, yp

      integer :: j

      if (m < 0 .or. m > mcs + 1) then
         return
      end if
      if (m == 0 .or. m > mcs .or. n == 0) then
!       EEN NIEUWE SPLINE
         call newSpline()
         mp = mcs
         np = 1
         call addSplinePoint(mp, xp, yp)
      else if (n <= lensp(m)) then
!       EEN NIEUW PUNT OP EEN BESTAANDE SPLINE TUSSENVOEGEN
         call resize_spline_capacity(required_splines=0, required_points_per_spline=lensp(m) + 1)
         do j = lensp(m), n + 1, -1
            xsp(m, j + 1) = xsp(m, j)
            ysp(m, j + 1) = ysp(m, j)
         end do
         lensp(m) = lensp(m) + 1
         mp = m
         np = n + 1
         call setSplinePoint(mp, np, xp, yp)
      else
         return
      end if
   end subroutine insertSplinePoint

   subroutine addSplinePoint(m, x, y)
      use precision, only: dp
      integer, intent(in) :: m
      real(kind=dp), intent(in) :: x, y

      call addSplinePoints(m, [x], [y])
   end subroutine addSplinePoint

   subroutine addSplinePoints(m, x, y)
      use precision, only: dp
      integer, intent(in) :: m
      real(kind=dp), intent(in) :: x(:), y(:)
      integer :: npts

      if (m > mcs) then
         if (m == mcs + 1) then
            call newSpline()
         else
            return
         end if
      end if

      npts = size(x)
      call resize_spline_capacity(mcs, lensp(m) + npts)

      xsp(m, lensp(m) + 1:lensp(m) + npts) = x
      ysp(m, lensp(m) + 1:lensp(m) + npts) = y
      lensp(m) = lensp(m) + npts

   end subroutine addSplinePoints

   subroutine delSpline(m)
      integer, intent(in) :: m

      integer :: k

      if (m > mcs) then
         return
      end if
      do k = m, mcs - 1
         xsp(k, :) = xsp(k + 1, :)
         ysp(k, :) = ysp(k + 1, :)
         lensp(k) = lensp(k + 1)
      end do
      xsp(mcs, :) = dxymis
      ysp(mcs, :) = dxymis
      lensp(mcs) = 0
      mcs = mcs - 1
      if (mp == m) then
         mp = 0
         np = 0
      end if
   end subroutine delSpline

   subroutine delSplines()
      xsp = dxymis
      ysp = dxymis
      lensp = 0
      mcs = 0
      mp = 0
      np = 0
   end subroutine delSplines

!> Remove a single point from a spline.
   subroutine delSplinePoint(m, n)
      integer, intent(in) :: m, n

      integer :: j

      if (m == 0 .or. m > mcs) then
         !call okay(0)
      else if (n == 0 .or. n > lensp(m)) then
         !call okay(0)
      else if (lensp(m) <= 2) then
         ! laatste twee punten van een spline, dus delete de hele spline
         call delSpline(m)
      else
         ! een willekeurig ander punt
         do j = n, lensp(m) - 1
            xsp(m, j) = xsp(m, j + 1)
            ysp(m, j) = ysp(m, j + 1)
         end do
         xsp(m, lensp(m)) = dxymis
         ysp(m, lensp(m)) = dxymis
         lensp(m) = lensp(m) - 1
      end if
   end subroutine delSplinePoint

!> Finds a spline point within a certain radius of a clicked point.
   subroutine isSplinePoint(xl, yl, rcir, mv, nv)
      use precision, only: dp
      use m_dispnode2, only: dispnode2

      real(kind=dp), intent(inout) :: xl, yl !< The clicked point
      real(kind=dp), intent(in) :: rcir !< The search radius around the point
      integer, intent(out) :: mv, nv !< The spline nr and spline-point nr found.

      integer :: mvold, nvold, ishot, m1, n1, m2, n2, i, j
      data MVOLd/0/, NVOLd/0/
      MV = 0
      NV = 0
      ISHOT = 0

666   continue
      if (ISHOT == 0 .and. MVold /= 0) then
         M1 = max(1, MVold - 3)
         N1 = max(1, NVold - 3)
         M2 = min(mcs, MVold + 3)
         N2 = min(points_per_spline_capacity, NVold + 3)
         ISHOT = 1
      else
         M1 = 1
         N1 = 1
         M2 = mcs
         N2 = points_per_spline_capacity
         ISHOT = 0
      end if

      do I = M1, M2
         do J = N1, min(N2, lensp(i))
            if (xsp(I, J) /= dXYMIS) then
               if (abs(XL - xsp(I, J)) < rcir) then
                  if (abs(YL - ysp(I, J)) < rcir) then
                     MV = I
                     NV = J
                     XL = xsp(I, J)
                     YL = ysp(I, J)
                     MVold = MV
                     NVold = NV
                     call DISPNODE2(MVold, NVold)
                     return
                  end if
               end if
            end if
         end do
      end do
      if (ISHOT == 1) then
         goto 666
      end if
      MVold = 0
      NVold = 0
      call DISPNODE2(MVold, NVold)
      return
   end subroutine isSplinePoint

!>  read splines in TEKAL format
   subroutine readSplines(mspl)
      use precision, only: dp
      use m_filez, only: doclose
      integer :: mspl

      character REC * 4
      integer :: j, nrow, ncol
      real(kind=dp) :: xp, yp

      ! if jadoorladen...
      call delSplines()

      call READYY('Reading Spline File', 0.0_dp)

10    continue

      read (MSPL, '(A)', end=9999) REC
      if (REC(1:1) == '*') then
         goto 10
      else
         read (MSPL, *, end=9999) NROW, NCOL
      end if

      call newSpline(nrow)
      do j = 1, nrow
         read (mspl, *) xp, yp
         call addSplinePoint(mcs, xp, yp)
      end do
      goto 10

9999  continue

      call READYY(' ', 1.0_dp)
      call READYY(' ', -1.0_dp)
      call DOCLOSE(MSPL)
      return

   end subroutine readSplines

!> write splines in TEKAL format
   subroutine writeSplines(mspl)
      use m_firstlin, only: firstlin
      use m_filez, only: doclose
      implicit none
      integer :: mspl
      character MATR * 5
      integer :: i, j
      MATR = 'S   '
      call FIRSTLIN(MSPL)
      call READYY('Writing Spline File', 0.0_dp)
      do I = 1, mcs
         call READYY('Writing Spline File', real(I, kind=dp) / real(mcs, kind=dp))
         write (MATR(2:5), '(I4.4)') I
         write (MSPL, '(A5)') MATR
         write (MSPL, '(I4,A4)') lensp(i), '   2'
         do J = 1, lensp(i)
            write (MSPL, '(1PE14.6, 1X, 1PE14.6)') xsp(I, J), ysp(I, J)
         end do
      end do
      call READYY(' ', -1.0_dp)
      call doclose(mspl)
      return
   end subroutine writeSplines

end module M_splines

