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

module interp

   implicit none
   
contains
!
! NAME
!    linear_interp
! SYNOPSIS
!    Interpolate linearly into an array Y given the value of X.
!    Return both the interpolated value and the position in the
!    array.
!
! ARGUMENTS
!    - X: independent array (sorted in ascending order)
!    - Y: array whose values are a function of X
!    - XX: specified value, interpolating in first array
!    - YY: interpolation result
!    - INDINT: (optional) index in array (x(indint) <= xx <= x(indint+1))
!
! SOURCE
!
   subroutine LINEAR_INTERP(X, Y, N, XX, YY, INDINT)
      use precision_basics, only: dp

      integer, intent(in) :: n
      real(dp), dimension(n), intent(in) :: x
      real(dp), dimension(n), intent(in) :: y
      real(dp), intent(in) :: xx
      real(dp), intent(out) :: yy
      integer, intent(out), optional :: indint
      !****
      !
      ! CODE: linear interpolation
      !
      !
      real(dp) :: a, b, dyy
      integer :: j

      yy = 0.0d0
      if (present(indint)) then
         indint = 0
      end if

      if (N <= 0) return
      !
      ! *** N GREATER THAN 0
      !
      if (N == 1) then
         YY = Y(1)
         return
      end if

      call binary_search(x, N, xx, j)

      if (j <= 0) then
         yy = y(1)
      elseif (j >= n) then
         yy = y(n)
      else
         a = x(j + 1)
         b = x(j)
         if (a == b) then
            dyy = 0.0d0
         else
            dyy = (y(j + 1) - y(j)) / (a - b)
         end if
         yy = y(j) + (xx - x(j)) * dyy
      end if

      if (present(indint)) then
         indint = j
      end if

      return

   end subroutine LINEAR_INTERP

   !****f* Interpolation/binary_search
   !
   ! NAME
   !    binary_search
   ! SYNOPSIS
   !    Perform a binary search in an ordered real array
   !    to find the largest entry equal or lower than a given value:
   !    Given an array XX of length N, given value X, return a value J
   !    such that X is between XX(J) en XX (J+1)
   !
   !    XX must be monotonic, either decreasing or increasing
   !    J=0 or J=N indicates X is out of range.
   !
   ! ARGUMENTS
   !    - XX: ordered array of values
   !    - X: value to be found
   !    - J: index such that X is between XX(J) and XX(J+1)
   !
   ! SOURCE
   !
   subroutine BINARY_SEARCH(XX, N, X, J)
      use precision_basics, only: dp

      integer, intent(in) :: N
      real(dp), dimension(N), intent(in) :: xx
      real(dp), intent(in) :: x
      integer, intent(out) :: j
      !****
      !
      ! CODE: binary search in (real) arrays
      !
      ! Requirement:
      !    Parameter wp set to the proper kind
      !
      ! Subroutine from 'Numerical recipes' Fortran  edition.
      ! Given an array XX of length N, given value X, return a value J
      ! such that X is between XX(J) en XX (J+1)
      ! XX must be monotonic, either decreasing or increasing
      ! J=0 or J=N indicates X is out of range.

      !
      ! Local variables
      !
      integer jl, ju, jm
      logical L1, L2

      JL = 0
      JU = N + 1
10    if (JU - JL > 1) then
         JM = (JU + JL) / 2
         L1 = XX(N) > XX(1)
         L2 = X > XX(JM)
         if ((L1 .and. L2) .or. (.not. (L1 .or. L2))) then
            JL = JM
         else
            JU = JM
         end if
         goto 10
      end if

      J = JL

      return

   end subroutine BINARY_SEARCH

   subroutine grmap(f1, n1, f2, n2, iref, &
                  & w, np, iprint)
  !!--description-----------------------------------------------------------------
      !
      ! compute interpolated values for all points on grid 2
      !
      ! special treatment of points on grid 2 that are outside
      ! grid 1; in that case iref(1,i2)=0 AND w(ip,i2)=0 for all ip
      !
      ! Iref(1,i2)   i1    ifac   F2(i2)*ifac     Result
      !
      !      0        1      1      F2(i2)        Old value is kept
      !    j,j>0      j      0       0.           F2 is initialized
      !
  !!--pseudo code and references--------------------------------------------------
      ! NONE
  !!--declarations----------------------------------------------------------------
      use precision_basics, only: dp

      implicit none
      !
      ! Global variables
      !
      integer, intent(in) :: iprint
      integer, intent(in) :: n1
      integer, intent(in) :: n2
      integer, intent(in) :: np
      integer, dimension(np, n2), intent(in) :: iref
      real(dp), dimension(n1), intent(in) :: f1
      real(dp), dimension(n2) :: f2
      real(dp), dimension(np, n2), intent(in) :: w
      !
      ! Local variables
      !
      integer :: i
      integer :: i1
      integer :: i2
      integer :: ip
      !
  !! executable statements -------------------------------------------------------
      !
      if (iprint == 1) write (*, *) 'in grmap n1 n2', n1, n2
      do i2 = 1, n2
         i = iref(1, i2)
         if (i > 0) then
            f2(i2) = 0.d0
            !        i1 = max(i, 1)
            !        ifac = 1 - i/i1
            !        f2(i2) = f2(i2)*ifac
            !
            ! Function values at grid 2 are expressed as weighted average
            ! of function values in Np surrounding points of grid 1
            !
            if (iprint == 1 .and. i2 <= n2) &
          & write (*, '(1X,A,I6,4(1X,E11.4))') ' i2 w ', i2, (w(ip, i2), ip=1,  &
          & np)
            do ip = 1, np
               i = iref(ip, i2)
               i1 = max(i, 1)
               if (iprint == 1 .and. i2 <= n2) write (*, *) ' i1,f1(i1) ', i1, f1(i1)
               f2(i2) = f2(i2) + w(ip, i2) * f1(i1)
            end do
         end if
      end do
   end subroutine grmap

   subroutine ipon(xq, yq, n, xp, yp, inout)
      !--description----------------------------------------------------------------
      !
      ! Deltares                                                               *
      ! AUTHOR : J.A.ROELVINK                                                  *
      ! DATE   : 22-12-1988                                                    *
      ! DETERMINE WHETHER POINT (xp,yp) LIES IN POLYGON (x,y) OF n POINTS      *
      ! POINT n+1 IS SET EQUAL TO POINT 1                                      *
      ! (ARRAY MUST HAVE DIMENSION n+1 IN MAIN PROGRAMME                       *
      ! inpout = -1 :  OUTSIDE POLYGON                                         *
      ! inpout =  0 :  ON EDGE OF POLYGON                                      *
      ! inpout =  1 :  INSIDE POLYGON                                          *
      ! USED METHOD :         - DRAW A VERTICAL LINE THROUGH (xp,yp)           *
      !                       - DETERMINE NUMBER OF INTERSECTIONS WITH POLYGON *
      !                         UNDER yp : nunder                              *
      !                       - IF nunder IS EVEN, THEN THE POINT LIES OUTSIDE *
      !                         THE POLYGON, OTHERWISE IT LIES INSIDE          *
      !                       - THE EDGE IS TREATED SEPARATELY                 *
      !
      !--pseudo code and references-------------------------------------------------
      ! NONE
      !--declarations---------------------------------------------------------------
      !
      use precision_basics, only: dp

      implicit none
      !
      ! Global variables
      !
      integer, intent(out) :: inout
      integer, intent(in) :: n
      real(dp), intent(in) :: xp
      real(dp), intent(in) :: yp
      real(dp), dimension(*) :: xq
      real(dp), dimension(*) :: yq
      !
      ! Local variables
      !
      integer :: i
      integer :: ierr
      integer :: nunder
      real(4) :: ysn
      real(4), dimension(:), allocatable :: x
      real(4), dimension(:), allocatable :: y
      !
      ! executable statements ------------------------------------------------------
      !
      allocate (x(n + 1))
      allocate (y(n + 1))
      do i = 1, n
         x(i) = real(xq(i) - xp, 4)
         y(i) = real(yq(i) - yp, 4)
      end do
      x(n + 1) = x(1)
      y(n + 1) = y(1)
      nunder = 0
      do i = 1, n
         if ((x(i) < 0. .and. x(i + 1) >= 0.) .or. (x(i + 1) < 0. .and. x(i) >= 0.)) then
            if (y(i) < 0. .and. y(i + 1) < 0.) then
               nunder = nunder + 1
            elseif ((y(i) <= 0. .and. y(i + 1) >= 0.) .or.                       &
                  & (y(i + 1) <= 0. .and. y(i) >= 0.)) then
               ysn = (y(i) * x(i + 1) - x(i) * y(i + 1)) / (x(i + 1) - x(i))
               if (ysn < 0.) then
                  nunder = nunder + 1
               elseif (ysn <= 0.) then
                  !
                  ! Edge
                  !
                  inout = 0
                  goto 100
               else
               end if
            else
            end if
         elseif (abs(x(i)) < 1.0e-8 .and. abs(x(i + 1)) < 1.0e-8) then
            if ((y(i) <= 0. .and. y(i + 1) >= 0.) .or. (y(i + 1) <= 0. .and. y(i) >= 0.)) &
              & then
               !
               ! Edge
               !
               inout = 0
               goto 100
            end if
         else
         end if
      end do
      if (mod(nunder, 2) == 0) then
         !
         ! Outside
         !
         inout = -1
      else
         !
         ! Inside
         !
         inout = 1
      end if
100   continue
      deallocate (x, stat=ierr)
      deallocate (y, stat=ierr)
   end subroutine ipon

   subroutine hunt(xx, n, x, jlo)
  !!--description-----------------------------------------------------------------
      ! NONE
  !!--pseudo code and references--------------------------------------------------
      ! NONE
  !!--declarations----------------------------------------------------------------
      !
      use precision_basics, only: dp

      implicit none
      !
      ! Global variables
      !
      integer :: jlo
      integer, intent(in) :: n
      real(dp), intent(in) :: x
      real(dp), dimension(n), intent(in) :: xx
      !
      ! Local variables
      !
      integer :: inc
      integer :: jhi
      integer :: jm
      logical :: ascnd
      !
  !! executable statements -------------------------------------------------------
      !
      ascnd = xx(n) >= xx(1)
      if (jlo <= 0 .or. jlo > n) then
         jlo = 0
         jhi = n + 1
         goto 3
      end if
      inc = 1
      if (x >= xx(jlo) .eqv. ascnd) then
1        continue
         jhi = jlo + inc
         if (jhi > n) then
            jhi = n + 1
         elseif (x >= xx(jhi) .eqv. ascnd) then
            jlo = jhi
            inc = inc + inc
            goto 1
         else
         end if
      else
         jhi = jlo
2        continue
         jlo = jhi - inc
         if (jlo < 1) then
            jlo = 0
         elseif (x < xx(jlo) .eqv. ascnd) then
            jhi = jlo
            inc = inc + inc
            goto 2
         else
         end if
      end if
3     continue
      if (jhi - jlo == 1) then
         return
      end if
      jm = (jhi + jlo) / 2
      if (x > xx(jm) .eqv. ascnd) then
         jlo = jm
      else
         jhi = jm
      end if
      goto 3
   end subroutine hunt
   subroutine bilin5(xa, ya, x0, y0, w, ier)
  !!--description-----------------------------------------------------------------
      ! NONE
  !!--pseudo code and references--------------------------------------------------
      !
      ! Author: H. Petit
      !
  !!--declarations----------------------------------------------------------------
      use precision_basics, only: dp

      implicit none
      !
      ! Global variables
      !
      integer, intent(out) :: ier
      real(dp), intent(in) :: x0
      real(dp), intent(in) :: y0
      real(dp), dimension(4), intent(out) :: w
      real(dp), dimension(4), intent(in) :: xa
      real(dp), dimension(4), intent(in) :: ya
      !
      ! Local variables
      !
      real(dp) :: a
      real(dp) :: a21
      real(dp) :: a22
      real(dp) :: a31
      real(dp) :: a32
      real(dp) :: a41
      real(dp) :: a42
      real(dp) :: b
      real(dp) :: c
      real(dp) :: det
      real(dp) :: discr
      real(dp) :: eta
      real(dp) :: x
      real(dp) :: x1
      real(dp) :: x2
      real(dp) :: x3
      real(dp) :: x3t
      real(dp) :: x4
      real(dp) :: xi
      real(dp) :: xt
      real(dp) :: y
      real(dp) :: y1
      real(dp) :: y2
      real(dp) :: y3
      real(dp) :: y3t
      real(dp) :: y4
      real(dp) :: yt
      !
  !! executable statements -------------------------------------------------------
      !
      ! read(12,*)x1,y1,f1
      x1 = xa(1)
      y1 = ya(1)
      ! read(12,*)x2,y2,f2
      x2 = xa(2)
      y2 = ya(2)
      ! read(12,*)x3,y3,f3
      x3 = xa(3)
      y3 = ya(3)
      ! read(12,*)x4,y4,f4
      x4 = xa(4)
      y4 = ya(4)
      x = x0
      y = y0
      !
      ! The bilinear interpolation problem is first transformed
      ! to the quadrangle with nodes (0,0),(1,0),(x3t,y3t),(0,1)
      ! and required location (xt,yt)
      !
      a21 = x2 - x1
      a22 = y2 - y1
      a31 = x3 - x1
      a32 = y3 - y1
      a41 = x4 - x1
      a42 = y4 - y1
      det = a21 * a42 - a22 * a41
      if (abs(det) < 1.0e-20) then
         ier = 1
         goto 99999
      end if
      x3t = (a42 * a31 - a41 * a32) / det
      y3t = (-a22 * a31 + a21 * a32) / det
      xt = (a42 * (x - x1) - a41 * (y - y1)) / det
      yt = (-a22 * (x - x1) + a21 * (y - y1)) / det
      if ((x3t < 0.0) .or. (y3t < 0.0)) then
         ! write (*, *) 'distorted quadrangle'
         ier = 1
         goto 99999
      end if
      if (abs(x3t - 1.0d0) < 1.0e-7) then
         xi = xt
         if (abs(y3t - 1.0d0) < 1.0e-7) then
            eta = yt
         elseif (abs(1.0d0 + (y3t - 1.0d0) * xt) < 1.0e-6) then
            ! write (*, *) 'extrapolation over too large a distance'
            ier = 1
            goto 99999
         else
            eta = yt / (1.0d0 + (y3t - 1.0d0) * xt)
         end if
      elseif (abs(y3t - 1.0d0) < 1.0e-6) then
         eta = yt
         if (abs(1.0d0 + (x3t - 1.0d0) * yt) < 1.0e-6) then
            ! write (*, *) 'extrapolation over too large a distance'
            ier = 1
            goto 99999
         else
            xi = xt / (1.0d0 + (x3t - 1.0d0) * yt)
         end if
      else
         a = y3t - 1.0d0
         b = 1.0d0 + (x3t - 1.0d0) * yt - (y3t - 1.0d0) * xt
         c = -xt
         discr = b * b - 4.0d0 * a * c
         if (discr < 1.0e-6) then
            ! write (*, *) 'extrapolation over too large a distance'
            ier = 1
            goto 99999
         end if
         xi = (-b + sqrt(discr)) / (2.0d0 * a)
         eta = ((y3t - 1.0d0) * (xi - xt) + (x3t - 1.0d0) * yt) / (x3t - 1.0d0)
      end if
      w(1) = (1.0d0 - xi) * (1.0d0 - eta)
      w(2) = xi * (1.0d0 - eta)
      w(3) = xi * eta
      w(4) = eta * (1.0d0 - xi)
      return
99999 continue
   end subroutine bilin5

   subroutine trapezoidal(x, y, n, x1_in, x2_in, integ)
      ! Compute integral over function y(x) from x1 to x2
      ! x is equidistant
      !(c)2014 Dano Roelvink
      use precision_basics, only: dp

      implicit none
      integer, intent(in) :: n
      real(dp), dimension(n), intent(in) :: x, y ! arrays x and y(x)
      real(dp), intent(in) :: x1_in, x2_in ! integration limits
      real(dp), intent(out) :: integ ! integral
      real(dp) :: x1, y1, x2, y2, Ifirst, Imid, Ilast, dx
      integer :: i1, i2, i, i1p1, i2p1
      if (x1_in > x(n) .or. x2_in < x(1)) then
         integ = 0
      else

         x1 = max(x1_in, x(1) + 1d-60)
         x2 = min(x2_in, x(n) - 1d-60)
         dx = (x(n) - x(1)) / (n - 1)
         i1 = floor((x1 - x(1)) / dx) + 1
         i2 = floor((x2 - x(1)) / dx) + 1
         i1p1 = min(i1 + 1, n)
         i2p1 = min(i2 + 1, n)
         ! first partial trapezoid
         y1 = y(i1) + (x1 - x(i1)) / dx * (y(i1p1) - y(i1))
         Ifirst = .5 * (x(i1p1) - x1) * (y(i1p1) + y1)
         ! middle part
         Imid = .5 * y(i1p1)
         do i = i1 + 2, i2 - 1
            Imid = Imid + y(i)
         end do
         Imid = Imid + .5 * y(i2)
         Imid = Imid * dx
         ! last partial trapezoid
         y2 = y(i2) + (x2 - x(i2)) / dx * (y(i2p1) - y(i2))
         Ilast = .5 * (x2 - x(i2)) * (y2 + y(i2))
         integ = Ifirst + Imid + Ilast
      end if
   end subroutine trapezoidal

   subroutine trapezoidal_cyclic(x, y, n, xcycle, x1, x2, integ)
      ! Compute integral over function y(x) from x1 to x2
      ! x is equidistant, function is cyclic so y(x+k*xcycle)=y(x)
      !(c)2014 Dano Roelvink
      use precision_basics, only: dp

      implicit none
      integer, intent(in) :: n
      real(dp), dimension(n), intent(in) :: x, y ! arrays x and y(x)
      real(dp), intent(in) :: x1, x2, xcycle ! integration limits,cycle length
      real(dp), intent(out) :: integ ! integral
      real(dp), dimension(:), allocatable :: xp, yp
      real(dp) :: dx
      integer :: ip, indt, np

      dx = x(2) - x(1)
      np = floor(x2 / dx) - (floor(x1 / dx) + 1) + 2
      allocate (xp(np))
      allocate (yp(np))
      xp(1) = x1
      do ip = 2, np - 1
         xp(ip) = (floor(x1 / dx) + ip - 1) * dx
      end do
      xp(np) = x2
      if (xcycle > 0) then
         call interp_in_cyclic_function(x, y, n, xcycle, xp, np, yp)
      else
         do ip = 1, np
            call linear_interp(x, y, n, xp(ip), yp(ip), indt)
         end do
      end if
      integ = 0.d0
      do ip = 1, np - 1
         integ = integ + .5 * (xp(ip + 1) - xp(ip)) * (yp(ip + 1) + yp(ip))
      end do

      deallocate (xp)
      deallocate (yp)

   end subroutine trapezoidal_cyclic

   subroutine interp_using_trapez_rule(x, y, n, xcycle, xtarg, ytarg, ntarg)
      use precision_basics, only: dp

      implicit none
      ! Compute integral over function y(x) from x1 to x2
      ! x is equidistant, function is cyclic so y(x+k*xcycle)=y(x)
      !(c)2014 Dano Roelvink
      integer, intent(in) :: n, ntarg
      real(dp), dimension(n), intent(in) :: x, y ! arrays x and y(x)
      real(dp), intent(in) :: xcycle ! cycle length
      real(dp), dimension(ntarg), intent(in) :: xtarg ! x values to interpolate to
      real(dp), dimension(ntarg), intent(out) :: ytarg ! y values to interpolate to
      real(dp) :: dx, x1, x2, integ
      integer :: itarg

      dx = xtarg(2) - xtarg(1)
      do itarg = 1, ntarg
         x1 = xtarg(itarg) - .5 * dx
         x2 = xtarg(itarg) + .5 * dx
         call trapezoidal_cyclic(x, y, n, xcycle, x1, x2, integ)
         ytarg(itarg) = integ / dx
      end do

   end subroutine interp_using_trapez_rule

   subroutine interp_in_cyclic_function(x, y, n, xcycle, xp, np, yp)
      use precision_basics, only: dp

      implicit none
      integer, intent(in) :: n
      real(dp), dimension(n), intent(in) :: x, y ! arrays x and y(x)
      real(dp), dimension(:), allocatable :: xc, yc ! complemented cyclic arrays
      real(dp), intent(in) :: xcycle ! cycle length
      integer, intent(in) :: np
      real(dp), dimension(np), intent(in) :: xp ! points to interpolate to
      real(dp), dimension(np), intent(out) :: yp ! interpolated yp values
      integer :: icycle, ip, ileft, iright, nc, i
      real(dp) :: dx, yleft, yright, facleft, facright

      dx = x(2) - x(1)
      icycle = nint(xcycle / dx)
      allocate (xc(icycle + 1))
      allocate (yc(icycle + 1))
      if (n > icycle + 1) then
         ! nonsense; error
      elseif (n == icycle + 1) then
         ! e.g. 0, 30, 60,...360
         xc(1:n) = x
         yc(1:n) = y
      elseif (n == icycle) then
         ! e.g. 30, 60 ...360 or 15, 45 ...345 with n=12 and icycle=12
         ! interpolate between n-th and 1st value
         xc(1:n) = x
         yc(1:n) = y
         xc(n + 1) = xc(n) + dx
         yc(n + 1) = yc(1)
      elseif (n < icycle) then
         ! do not interpolate between n-th and 1st value; set values in gap to 0
         xc(1:n) = x
         yc(1:n) = y
         do i = n + 1, icycle
            xc(i) = xc(i - 1) + dx
            yc(i) = 0.d0
         end do
         xc(icycle + 1) = xc(icycle) + dx
         yc(icycle + 1) = yc(1)
      end if

      nc = icycle + 1

      do ip = 1, np
         ileft = floor((xp(ip) - xc(1)) / dx) + 1
         do while (ileft < 1)
            ileft = ileft + icycle
         end do
         do while (ileft > nc)
            ileft = ileft - icycle
         end do
         if (ileft > nc .or. ileft < 1) then
            yleft = 0
         else
            yleft = yc(ileft)
         end if
         iright = ileft + 1
         do while (iright < 1)
            iright = iright + icycle
         end do
         do while (iright > nc)
            iright = iright - icycle
         end do
         if (iright > nc .or. iright < 1) then
            yright = 0
         else
            yright = yc(iright)
         end if
         facright = mod(xp(ip), dx) / dx
         facleft = 1.d0 - facright
         yp(ip) = facleft * yleft + facright * yright

      end do
      deallocate (xc)
      deallocate (yc)

   end subroutine interp_in_cyclic_function

end module interp
