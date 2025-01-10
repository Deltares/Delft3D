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

module m_copylandboundaryto1dnetwork

   implicit none

   private

   public :: copylandboundaryto1dnetwork

contains

   subroutine copylandboundaryto1Dnetwork()
      use m_regrid1d, only: regrid1d
      use m_maptopolyline, only: maptopolyline
      use precision, only: dp
      use m_accumulatedistance, only: accumulatedistance
      use m_polygon
      use m_landboundary
      use m_netw
      use m_missing
      use gridoperations
      use m_set_nod_adm

      integer :: MX = 1000000
      integer :: k, kk, k1, k2, n, LL, NL, ierr, nh
      real(kind=dp) :: D, D1D, DTOT
      real(kind=dp), allocatable :: DLan(:), XH(:), YH(:), DH(:)

      if (mxlan == 0 .and. numl > 0) then
         call regrid1D(1) ! based on 1D net
         return
      end if

      allocate (DLan(MXLAN), STAT=IERR)
      call aerr('DLan(MXLAN)', IERR, mxlan)
      allocate (XH(MX), YH(MX), DH(MX), STAT=IERR)
      call AERR('XH(MX),YH(MX),DH(MX)', IERR, MX)

      call INCREASENETW(100000, 100000)

      k1 = 0; k2 = 0
      do k = 1, mxlan
         if (xlan(k) == dmiss) then
            k2 = -k2
         else
            if (k1 == 0) then
               k1 = k
            end if
            k2 = k
         end if

         if (k1 /= 0 .and. k2 < 0) then
            k2 = -k2
            nL = k2 - k1 + 1

            call accumulateDistance(Xlan(k1:k2), Ylan(k1:k2), DLan(k1:k2), NL)

            DTOT = DLAN(K2)
            NH = max(1, ceiling(DTOT / unidx1D))

            if (NH > size(DH)) then
               deallocate (XH, YH, DH)
               allocate (XH(NH), YH(NH), DH(NH), STAT=IERR)
               call AERR('XH(NH),YH(NH),DH(NH)', IERR, MX)
            end if

            if (NH >= 1) then
               D1D = DTOT / NH

               D = 0
               do N = 1, NH
                  D = D + D1D
                  DH(N) = D
               end do

               call mapToPolyline(Xlan(k1:k2), Ylan(k1:k2), DLAN(k1:k2), NL, XH, YH, DH, NH) ! HAAL HUIDIGE PUNTEN OP

               KK = NUMK + 1; LL = NUML
               call INCREASENETW(KK + nh, LL + nh)
               XK(KK) = Xlan(k1); YK(KK) = Ylan(k1)
               do N = 1, NH
                  KK = KK + 1
                  XK(KK) = XH(N); YK(KK) = YH(N); ZK(KK) = dmiss
                  LL = LL + 1
                  KN(1, LL) = KK - 1; KN(2, LL) = KK; KN(3, LL) = 1 ! NOTE: 1D endpoints now don't have KN(3,L)=4 automatically.
               end do
               NUMK = KK; NUML = LL

            end if

            k1 = 0; k2 = 0
         end if
      end do

      deallocate (DLan, xh, yh, dh)
      call setnodadm(0)

   end subroutine copylandboundaryto1Dnetwork

end module m_copylandboundaryto1dnetwork
