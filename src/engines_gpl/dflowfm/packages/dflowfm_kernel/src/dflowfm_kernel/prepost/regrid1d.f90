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

module m_regrid1d

   implicit none

   private

   public :: regrid1d

contains

   subroutine regrid1D(jaregrid) ! based on 1D net itself, 1 = regrid, otherwise 1dgrid to pol
      use m_maptopolyline, only: maptopolyline
      use precision, only: dp
      use m_accumulatedistance, only: accumulatedistance
      use m_flowgeom
      use m_flow
      use m_netw
      use m_polygon
      use m_missing
      use gridoperations
      use m_set_nod_adm
      use m_set_branch_lc

      integer :: jaregrid
      real(kind=dp) :: dxa, xlb
      real(kind=dp), allocatable :: xh(:), yh(:), zh(:)
      integer :: L, LL, k, n, nh, ibr, LA, k1, k2, ium

      if (jaregrid == 1) then
         call savepol()
      end if
      call SAVENET()

      npl = 0

      call SETBRANCH_LC(ium)

      numk = 0; numl = 0; n = 0
      do ibr = 1, mxnetbr ! SET UP BRANCH DISTANCE COORDINATE
         XLB = 0d0
         do LL = 1, netbr(ibr)%NX
            L = netbr(ibr)%ln(LL); LA = abs(L)
            if (L > 0) then
               k1 = kn0(1, La); k2 = kn0(2, LA)
            else
               k2 = kn0(1, La); k1 = kn0(2, LA)
            end if
            if (LL == 1) then
               if (jaregrid == 1) then
                  n = 1
               else
                  n = n + 1
               end if
               xpl(n) = xk0(k1); ypl(n) = yk0(k1); zpl(n) = zk0(k1)
            end if
            n = n + 1
            if (n > maxpol) then
               call INCREASEPOL(int(1.5 * n), 1)
            end if
            xpl(n) = xk0(k2); ypl(n) = yk0(k2); zpl(n) = zk0(k2)
         end do

         if (jaregrid == 1) then
            call accumulateDistance(XPL, YPL, ZPL, N)

            nh = zpl(n) / Unidx1D + 1
            dxa = zpl(n) / nh
            nh = nh + 1
            allocate (xh(nh), yh(nh), zh(nh))
            zh(1) = 0d0
            do k = 2, nh
               zh(k) = zh(k - 1) + dxa
            end do
            call mapToPolyline(XPL, YPL, ZPL, N, XH, YH, ZH, NH) ! HAAL HUIDIGE PUNTEN OP

            numk = numk + 1
            xk(numk) = xh(1); yk(numk) = yh(1)
            do k = 2, nh
               numk = numk + 1
               numL = numL + 1

               if (numk > numk0 .or. numL > numL0) then
                  call increasenetw(2 * numk, 2 * numl)
               end if

               xk(numk) = xh(k); yk(numk) = yh(k)
               kn(2, numl) = numk; kn(1, numl) = numk - 1; kn(3, numl) = 1 ! NOTE: regridded 1D now does not have kn(3,L)=4 at end points.
            end do

            deallocate (xh, yh, zh)

         else

            n = n + 1
            xpl(n) = dmiss; ypl(n) = dmiss; zpl(n) = dmiss

         end if

      end do

      if (jaregrid == 1) then
         call SETNODADM(0)
         call restorepol()
      else
         npl = n - 1
         call restore()
      end if

   end subroutine regrid1D

end module m_regrid1d
