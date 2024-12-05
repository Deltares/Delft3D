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

    !> Merges two polylines, indicated by two start/end points
    !!
    !! Multiple polylines are stored in one large array, separated by dmiss.
    !! Possibly, one or two of the polylines is flipped and then glued to the other.
    subroutine mergepoly(X, Y, Z, maxpol, n, i1, i2)
       use precision, only: dp
       use M_MISSING
       implicit none
       integer, intent(in) :: MAXPOL !< Length of polyline coordinate arrays.
       real(kind=dp), intent(inout) :: X(MAXPOL), Y(MAXPOL) !< Entire polyline coordinate arrays
       real(kind=dp), intent(inout) :: Z(MAXPOL) !< polyline Z-value array
       integer, intent(inout) :: N !< Index of last filled polyline point (npol<=maxpol)
       integer, intent(inout) :: i1, i2 !< Indices of polyline start/ends which need to be connected.

       integer :: i, im, in, ih, ii, n2, nh
       logical :: jadiff, jaflip
       real(kind=dp), allocatable :: xh(:), yh(:), zh(:)
       real(kind=dp) :: xt, yt, zt

       if (i1 == i2 .or. i1 <= 0 .or. i1 > n .or. i2 <= 0 .or. i2 > n) return
       if (X(i1) == dmiss .or. X(i2) == dmiss) return

       ! Handle 'leftmost(in array)' polyline first
       if (i1 > i2) then
          ih = i1
          i1 = i2
          i2 = ih
       end if

       ! Check whether i1 and i2 are two different polylines. If not: return.
       jadiff = .false.
       do i = i1, i2
          if (X(i) == dmiss) then
             jadiff = .true.
             exit
          end if
       end do

       if (.not. jadiff) then
          return
       end if

       ! If i1 is *first* point, then flip, such that it becomes the
       ! last (to enable coupling to i2-polyline later on)
       jaflip = .false.
       if (i1 < n) then
          if (X(i1 + 1) /= dmiss) then
             jaflip = .true.
          end if
       end if

       ! Flip first polyline if necessary.
       if (jaflip) then
          ! Find end point first
          im = n ! Default end index: n
          do i = i1 + 1, n
             if (X(i) == dmiss) then
                im = i - 1
                exit
             end if
          end do

          ! End point found, now flip from i1 to im.
          ih = (i1 + im - 1) / 2
          do i = i1, ih
             ii = im - i + i1
             xt = x(i); yt = y(i); zt = z(i)
             x(i) = x(ii); y(i) = y(ii); z(i) = z(ii)
             x(ii) = xt; y(ii) = yt; z(ii) = zt
          end do

          ! Flip indices, such that i1 is the rightmost
          ih = i1
          i1 = im
          im = ih
       end if

       ! If i2 is *last* point, then flip, such that it becomes the first (to enable coupling to im later on)
       jaflip = .false.
       if (X(i2 - 1) /= dmiss) then
          jaflip = .true.
       end if

       ! Flip second polyline if necessary
       if (jaflip) then
          in = i2
          do i = i2 - 1, 1, -1
             if (X(i) == dmiss) then
                in = i + 1
                exit
             end if
          end do

          ! Start point found, now flip from in to i2
          ih = (in + i2 - 1) / 2
          do i = in, ih
             ii = i2 - i + in
             xt = x(i); yt = y(i); zt = z(i)
             x(i) = x(ii); y(i) = y(ii); z(i) = z(ii)
             x(ii) = xt; y(ii) = yt; z(ii) = zt
          end do

          ! Flip indices, such that i2 is the leftmost
          ih = i2
          i2 = in
          in = ih
       else
          ! No flip, but do find the last (rightmost) element of 2nd polyline.
          in = n
          do i = i2 + 1, n
             if (X(i) == dmiss) then
                in = i - 1
                exit
             end if
          end do
       end if

       n2 = in - i2 + 1

       ! If there is a non-zero-length polyline between i1 and i2, back it up.
       if (i2 - i1 >= 3) then
          nh = i2 - i1 - 3
          allocate (xh(nh), yh(nh), zh(nh))
          do i = 1, nh
             xh(i) = x(i1 + 1 + i)
             yh(i) = y(i1 + 1 + i)
             zh(i) = z(i1 + 1 + i)
          end do
       else
          nh = 0
       end if

       ! Now shift second polyline to the left, such that i2 gets directly next to i1
       do i = i2, in
          ih = i1 + 1 + i - i2
          x(ih) = x(i)
          y(ih) = y(i)
          z(ih) = z(i)
       end do

       x(i1 + n2 + 1) = dmiss
       y(i1 + n2 + 1) = dmiss
       z(i1 + n2 + 1) = dmiss

       if (nh > 0) then
          do ih = 1, nh
             i = i1 + n2 + 1 + ih
             x(i) = xh(ih)
             y(i) = yh(ih)
             z(i) = zh(ih)
          end do
          deallocate (xh, yh, zh)

          x(i1 + n2 + 1 + nh + 1) = dmiss
          y(i1 + n2 + 1 + nh + 1) = dmiss
          z(i1 + n2 + 1 + nh + 1) = dmiss
          ih = i1 + n2 + nh + 3 ! Index of next non-dmiss entry
       else
          ih = i1 + n2 + 2
       end if

       if (n > in) then
          do i = in + 2, n
             ii = ih + i - in - 2
             x(ii) = x(i)
             y(ii) = y(i)
             z(ii) = z(i)
          end do
          x(ih + n - in - 1:n) = dmiss
          y(ih + n - in - 1:n) = dmiss
          z(ih + n - in - 1:n) = dmiss
       end if
       n = ih + n - in - 2

    end subroutine mergepoly
