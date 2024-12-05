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

module m_crosslinkpoly

   implicit none

   private

   public :: crosslinkpoly

contains

   !> Finds the crossing of link L with the current polyline.
  !! returns first crossing, if found, JA=1
   subroutine CROSSLINKPOLY(L, num, ipoly, jdxL, pdxL, XM, YM, JA)
      use precision, only: dp

      use m_missing, only: dmiss
      use m_netw
      use kdtree2Factory
      use m_sferic, only: jsferic, jasfer3D
      use unstruc_messages
      use geometry_module, only: dbdistance, crossinbox
      use m_qnerror

      integer :: L, JA
      real(kind=dp) :: XM, YM
      integer, intent(in) :: num !< number of polygon sections that intersect netlink L
      integer, intent(in) :: ipoly !< polygon identifier
      integer, dimension(num), intent(in) :: jdxL !< polygon sections that intersect netlink L
      integer, dimension(num), intent(in) :: pdxL !< polygon numbers that intersect netlink L

      integer :: jacros
      integer :: k, k_, kend
      integer :: k1
      integer :: k2
      integer :: ku
      real(kind=dp) :: XP1, YP1, XP2, YP2, SL, SM, XCR, YCR, CRP, dis

      real(kind=dp), parameter :: dtol = 1d-8

      integer :: i
      integer :: janew
      integer :: numcrossed
      integer, parameter :: MAXCROSS = 100
      real(kind=dp), dimension(MAXCROSS) :: xcross, ycross

      K1 = KN(1, L); K2 = KN(2, L)

! initialization
      xm = 0d0
      ym = 0d0

      JA = 0
      numcrossed = 0

      if (num == 0) then
         kend = NPL
      else
         kend = num
      end if

      do K_ = 1, kend

         if (num == 0) then
            k = k_
         else
            if (pdxL(k_) /= ipoly) cycle
            k = jdxL(k_)
         end if

         KU = K + 1
         if (K == NPL) KU = 1
         XP1 = XPL(K); YP1 = YPL(K)
         XP2 = XPL(KU); YP2 = YPL(KU)

! Formerly:
!     CALL DCROSS (XP1, YP1, XP2, YP2, XK(K1), YK(K1), XK(K2), YK(K2), JACROS, SL, SM, XM, YM, CRP)
!     IF (JACROS == 1) THEN
!        IF (SL > 0D0 .AND. SL < 1D0 .AND. SM > 0D0 .AND. SM < 1D0) THEN
!           JA = 1
!           EXIT
!        ENDIF
!     ENDIF
! New and equivalent (apart from '<' vs. '.le.'):
         !CALL CROSSinbox (XP1, YP1, XP2, YP2, XK(K1), YK(K1), XK(K2), YK(K2), jacros, SL, SM, XCR, YCR, CRP)

         call CROSSinbox(XK(K1), YK(K1), XK(K2), YK(K2), XP1, YP1, XP2, YP2, jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)

!    fix for spherical coordinates (enforce same reference point for local projections)
         if (jsferic == 1 .and. SM > 0.75d0 .and. jacros == 1) then
            call CROSSinbox(XK(K1), YK(K1), XK(K2), YK(K2), XP2, YP2, XP1, YP1, jacros, SL, SM, XCR, YCR, CRP, jsferic, dmiss)
         end if

         if (jacros == 1) then

            !IF (SL > 0D0 .AND. SL < 1D0 .AND. SM > 0D0 .AND. SM < 1D0) THEN
            !    JA = 1
            !    XM = XCR
            !    YM = YCR
            !   EXIT
            !ENDIF

!       check if this cross is different from previous
            janew = 1
            do i = 1, numcrossed
               dis = dbdistance(xcr, ycr, xcross(i), ycross(i), jsferic, jasfer3D, dmiss)
               if (dis <= dtol) then
                  janew = 0
                  exit
               end if

            end do

            if (janew == 1) then
               numcrossed = numcrossed + 1
               if (numcrossed > MAXCROSS) then
                  write (msgbuf, '(a,i0,i0)') 'crosslinkpoly: numcrossed>MAXCROSS', numcrossed, MAXCROSS
                  call qnerror(trim(msgbuf), ' ', ' ')
               end if
               xcross(numcrossed) = xcr
               ycross(numcrossed) = ycr
            end if
         end if
      end do

      if (mod(numcrossed, 2) == 0) then
         !  even number of intersections: no intersection
         ja = 0
      else
         !  odd number of intersections: take one (average)
         ja = 1
         xm = 0d0
         ym = 0d0
         xm = xcross(1)
         ym = ycross(1)
      end if

   end subroutine CROSSLINKPOLY

end module m_crosslinkpoly
