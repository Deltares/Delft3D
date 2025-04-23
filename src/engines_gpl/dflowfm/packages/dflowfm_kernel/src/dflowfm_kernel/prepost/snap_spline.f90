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

!> snap spline to nearest land boundary
module m_snap_spline
   use m_toland, only: toland

   implicit none

   private

   public :: snap_spline

contains

   subroutine snap_spline(ispline)
      use precision, only: dp
      use m_confrm
      use m_comp_afinespline
      use m_landboundary
      use m_splines
      use m_alloc
      use unstruc_display, only: plotSplines, ncolsp
      use geometry_module, only: dbdistance, gaussj
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D
      use m_spline
      use m_wall_clock_time
      use m_comp_curv

      integer, intent(in) :: ispline !< spline number

      real(kind=dp), dimension(:, :), allocatable :: A, AtWA, AtWAi
      real(kind=dp), dimension(:), allocatable :: xf, yf ! sample points
      real(kind=dp), dimension(:), allocatable :: xb, yb ! sample points projected on land boundary
      real(kind=dp), dimension(:), allocatable :: AtWxb, AtWyb ! (A, W xb) and (A, W yb)
      real(kind=dp), dimension(:), allocatable :: rhsx, rhsy ! right-hand side vectors
      real(kind=dp), dimension(:, :), allocatable :: B, C ! constraints matrics Bx+Cy=d
      real(kind=dp), dimension(:), allocatable :: d ! constraints rhs
      real(kind=dp), dimension(:), allocatable :: lambda ! Lagrangian multipliers
      real(kind=dp), dimension(:, :), allocatable :: E ! E lambda = f
      real(kind=dp), dimension(:), allocatable :: w ! weights

      real(kind=dp), dimension(:), allocatable :: xspp, yspp ! second order spline derivatives

      real(kind=dp) :: dis, rL, curv, dsx, dsy

      real(kind=dp) :: dn1x, dn1y, dn2x, dn2y, xx1, yy1, xx2, yy2 ! constraints: (x(1)-xx1)nx1 + (y(1)-yy1)ny1 = 0, etc.

      real(kind=dp) :: t0, t1 ! for timing

      integer :: ierror
      integer :: i, iL, iR, j, ja, k, num, Numnew, Numconstr

      integer, parameter :: Nref = 19 ! number of additional points between spline control points for sampled spline

      ierror = 1

      call nump(ispline, num)

!  remember initial first and last spline node coordinates for contraints
      Numconstr = 2
      xx1 = xsp(ispline, 1)
      yy1 = ysp(ispline, 1)
      xx2 = xsp(ispline, num)
      yy2 = ysp(ispline, num)

!   do i=1,num
!      x1 = xsp(ispline,i)
!      y1 = ysp(ispline,i)
!      call toland(x1, y1, 1, MXLAN, 2, xn, yn, dis, j, rL)
!      xsp(ispline,i) = xn
!      ysp(ispline,i) = yn
!   end do

!  compute the spline to fine-spline matrix
      Numnew = 1
      do
         call realloc(A, (/Numnew, num/))
         call comp_Afinespline(num, Nref, Numnew, A, ierror)
!     check if the arrays were large enough and reallocate if not so
         if (ierror /= 2) then
            exit
         end if
      end do

!  allocate
      allocate (xf(Numnew), yf(Numnew), xb(Numnew), yb(Numnew))
      allocate (AtWA(num, num), AtWAi(num, num))
      allocate (AtWxb(num), AtWyb(num))
      allocate (rhsx(num), rhsy(num))
      allocate (B(Numconstr, num), C(Numconstr, num), d(Numconstr), lambda(Numconstr))
      allocate (E(Numconstr, Numconstr))
      allocate (xspp(num), yspp(num))
      allocate (w(Numnew))

!  compute sample points
      xf = matmul(A, xsp(ispline, 1:num))
      yf = matmul(A, ysp(ispline, 1:num))

!  compute weights
      do i = 1, Numnew
         iL = max(i - 1, 1)
         iR = min(i + 1, Numnew)
         w(i) = 1d0 / sqrt(dbdistance(xf(iL), yf(iL), xf(ir), yf(iR), jsferic, jasfer3D, dmiss) / dble(iR - iL))
      end do

!  compute normal vectors at contrained spline nodes
      call spline(xsp(ispline, 1:num), num, xspp)
      call spline(ysp(ispline, 1:num), num, yspp)
      call comp_curv(num, xsp(ispline, 1:num), ysp(ispline, 1:num), xspp, yspp, 0d0, curv, dn1x, dn1y, dsx, dsy)
      call comp_curv(num, xsp(ispline, 1:num), ysp(ispline, 1:num), xspp, yspp, dble(num - 1), curv, dn2x, dn2y, dsx, dsy)

! DEBUG
!   w = 1d0
! END DEBUG

!  make matrix
      do i = 1, num
         do j = 1, num
            AtWA(i, j) = 0d0
            do k = 1, Numnew
               AtWA(i, j) = AtWA(i, j) + A(k, i) * w(k) * A(k, j)
            end do
         end do
      end do

!  compute inverse matrix
      AtWAi = AtWA
      rhsx = 0d0 ! dummy for now
      call gaussj(AtWAi, num, num, rhsx, 1, 1)

!  make the contraints
      B = 0d0
      C = 0d0
      B(1, 1) = dn1y; C(1, 1) = -dn1x; d(1) = dn1y * xx1 - dn1x * yy1
      B(2, num) = dn2y; C(2, num) = -dn2x; d(2) = dn2y * xx2 - dn2x * yy2
!  compute Schur complement
      E = matmul(B, matmul(AtWAi, transpose(B))) + matmul(C, matmul(AtWAi, transpose(C)))
      lambda = 0d0
!  invert Schur complement
      call gaussj(E, Numconstr, Numconstr, lambda, 1, 1)

      do
!     compute projected sample points
         call wall_clock_time(t0)
         do i = 1, Numnew
            call toland(xf(i), yf(i), 1, MXLAN, 2, xb(i), yb(i), dis, j, rL)
         end do
         call wall_clock_time(t1)

         write (6, "('elapsed time:', F7.2, ' sec.')") t1 - t0

         do i = 1, num
            AtWxb(i) = 0d0
            AtWyb(i) = 0d0
            do k = 1, Numnew
               AtWxb(i) = AtWxb(i) + A(k, i) * w(k) * xb(k)
               AtWyb(i) = AtWyb(i) + A(k, i) * w(k) * yb(k)
            end do
         end do

         do i = 1, num
            do j = 1, num
            end do
         end do

!!     plot projected sample points
!      call movabs(xb(1),yb(1))
!      do i=2,Numnew
!         call clnabs(xb(i),yb(i),31)
!      end do

!     compute Lagrangian multipliers
         lambda = matmul(E, matmul(matmul(B, AtWAi), AtWxb) + matmul(matmul(C, AtWAi), AtWyb) - d)

!     make rhs
         rhsx = AtWxb - matmul(transpose(B), lambda)
         rhsy = AtWyb - matmul(transpose(C), lambda)

!     whipe out spline
         call plotsplines(ispline, ispline, 0)

!     update spline control point coordinates
         xsp(ispline, 1:num) = matmul(AtWAi, rhsx)
         ysp(ispline, 1:num) = matmul(AtWAi, rhsy)

         call plotsplines(ispline, ispline, ncolsp)

         ja = 1
         call confrm('Continue?', ja)
         if (ja /= 1) exit

!     compute sample points
         xf = matmul(A, xsp(ispline, 1:num))
         yf = matmul(A, ysp(ispline, 1:num))

      end do

      ierror = 0
1234  continue

!  deallocate
      if (allocated(A)) then
         deallocate (A)
      end if
      if (allocated(xf)) then
         deallocate (xf)
      end if
      if (allocated(yf)) then
         deallocate (yf)
      end if
      if (allocated(xb)) then
         deallocate (xb)
      end if
      if (allocated(yb)) then
         deallocate (yb)
      end if
      if (allocated(AtWxb)) then
         deallocate (AtWxb)
      end if
      if (allocated(AtWyb)) then
         deallocate (AtWyb)
      end if
      if (allocated(AtWA)) then
         deallocate (AtWA)
      end if
      if (allocated(AtWAi)) then
         deallocate (AtWAi)
      end if
      if (allocated(rhsx)) then
         deallocate (rhsx)
      end if
      if (allocated(rhsy)) then
         deallocate (rhsy)
      end if
      if (allocated(B)) then
         deallocate (B)
      end if
      if (allocated(C)) then
         deallocate (C)
      end if
      if (allocated(d)) then
         deallocate (d)
      end if
      if (allocated(lambda)) then
         deallocate (lambda)
      end if
      if (allocated(E)) then
         deallocate (E)
      end if
      if (allocated(xspp)) then
         deallocate (xspp)
      end if
      if (allocated(yspp)) then
         deallocate (yspp)
      end if
      if (allocated(w)) then
         deallocate (w)
      end if

      return
   end subroutine snap_spline

end module m_snap_spline
