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

! $Id: addbarocl.f90 142295 2023-01-10 08:25:27Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/trunk/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/addbarocl.f90 $

module m_addbarocl

   implicit none

   private

   public :: addbarocL, addbarocLorg, addbarocLrho_w

contains

   subroutine addbarocL(LL, Lb, Lt)
      use precision, only: dp
      use m_flowgeom
      use m_flow
      use m_flowtimes

      implicit none
      integer, intent(in) :: LL, Lb, Lt

      integer :: L, k1, k2, k1t, k2t, k, kt, kz, ktz, insigpart, morelayersleft
      real(kind=dp) :: gradpu(kmxx), rhovol(kmxx), gr3
      real(kind=dp) :: rv1, rv2, gr1, gr2, rvk, grk, fzu, fzd, dzz, rhow0, rhow1

      gradpu(1:Lt - Lb + 1) = 0d0

      if (zws(ln(1, Lt)) - zws(ln(1, Lb)) < 0.1d0 .or. zws(ln(2, Lt)) - zws(ln(2, Lb)) < 0.1d0) then
         return ! no baroclinic pressure in thin water layers
      end if

      insigpart = 0
      if (numtopsig > 0) then
         if (kmxn(ln(1, LL)) <= numtopsig .or. kmxn(ln(2, LL)) <= numtopsig) then
            insigpart = 1 ! one of the nodes is in the sigma part
         end if
      end if

      if (kmxn(ln(1, LL)) > kmxn(ln(2, LL))) then
         morelayersleft = 1
      else if (kmxn(ln(1, LL)) < kmxn(ln(2, LL))) then
         morelayersleft = 2
      else
         morelayersleft = 0
      end if

      do L = Lt, Lb, -1
         k1 = ln(1, L); k1t = k1
         k2 = ln(2, L); k2t = k2
         if (L == Lt) then
            k1t = ktop(ln(1, LL)); k2t = ktop(ln(2, LL))
         end if

         rhovol(L - Lb + 1) = 0.5d0 * ((zws(k1t) - zws(k1 - 1)) * rho(k1) + (zws(k2t) - zws(k2 - 1)) * rho(k2))
         if (jarhoxu > 0) then
            rhou(L) = rhovol(L - Lb + 1) / (0.5d0 * (zws(k1t) - zws(k1 - 1) + zws(k2t) - zws(k2 - 1)))
         end if
         rhovol(L - Lb + 1) = rhovol(L - Lb + 1) * dx(LL)

         rv1 = rvdn(k1)
         rv2 = rvdn(k2)
         gr1 = grn(k1)
         gr2 = grn(k2)

         if (L == Lb .and. morelayersleft /= 0) then ! extrapolate at 'bed' layer of deepest side

            if (morelayersleft == 1) then ! k=deep side, kz=shallow side
               k = k1; kt = ktop(ln(1, LL))
               kz = k2; ktz = ktop(ln(2, LL))
            else
               k = k2; kt = ktop(ln(2, LL))
               kz = k1; ktz = ktop(ln(1, LL))
            end if

            if (ktz - kz > 0) then ! shallow side extrapolates, coeffs based on shallow side:
               fzu = (zws(kz + 1) - zws(kz)) / (zws(kz + 1) - zws(kz - 1)); fzd = 1d0 - fzu
               rhow1 = fzu * rho(k + 1) + fzd * rho(k)
               rhow0 = 2d0 * rho(k) - rhow1
            else ! one layerr
               rhow1 = rho(k)
               rhow0 = rhow1
            end if

            rhow1 = rhow1 - rhomean
            rhow0 = rhow0 - rhomean
            if (insigpart == 0) then
               dzz = zws(kz) - zws(kz - 1) ! shallow side

               rhovol(1) = dzz * 0.5d0 * (rho(k) + rho(kz)) * dx(LL)
               if (jarhoxu > 0) then
                  rhou(L) = 0.5d0 * (rho(k) + rho(kz))
               end if

            else
               dzz = zws(k) - zws(k - 1) ! deep side
            end if

            rvk = rvdn(k + 1) + 0.5d0 * dzz * (rhow1 + rhow0)
            grk = (rvdn(k + 1) + 0.5d0 * dzz * (2d0 * rhow1 + rhow0) / 3d0) * dzz

            if (morelayersleft == 1) then ! k1=deepest
               rv1 = rvk; gr1 = grk
            else
               rv2 = rvk; gr2 = grk
            end if

            if (insigpart == 0) then
               gr3 = 0d0 ! no skewness for zlay jump at bed
            else
               gr3 = 0.5d0 * (rv1 + rv2) * (zws(k1 - 1) - zws(k2 - 1))
            end if

         else
            gr3 = 0.5d0 * (rv1 + rv2) * (zws(k1 - 1) - zws(k2 - 1))
         end if

         gradpu(L - Lb + 1) = gradpu(L - Lb + 1) + gr1 - gr2 + gr3
         if (L > Lb) then
            gradpu(L - Lb) = gradpu(L - Lb) - gr3 ! ceiling of ff# downstairs neighbours
         end if
      end do

      call barocLtimeint(gradpu, rhovol, LL, Lb, Lt)

   end subroutine addbarocL

   subroutine addbarocLrho_w(LL, Lb, Lt)
      use precision, only: dp
      use m_flowgeom
      use m_flow
      use m_flowtimes
      use m_transport, only: ISALT, ITEMP, constituents
      use m_physcoef, only: rhomean
      use m_densfm, only: densfm

      implicit none
      integer, intent(in) :: LL, Lb, Lt

      integer :: L, k1, k2, k1t, k2t, k, kt, kz, ktz, insigpart, morelayersleft, i
      real(kind=dp) :: gradpu(kmxx), rhovol(kmxx), gr3
      real(kind=dp) :: rv1, rv2, gr1, gr2, rvk, grk, saw0, saw1, tmw0, tmw1, fzu, fzd, dzz, rhow0, rhow1, pdb, p0d

      gradpu(1:Lt - Lb + 1) = 0d0

      if (zws(ln(1, Lt)) - zws(ln(1, Lb)) < 0.1d0 .or. zws(ln(2, Lt)) - zws(ln(2, Lb)) < 0.1d0) then
         return ! no baroclini pressure in thin water layers
      end if

      insigpart = 0
      if (numtopsig > 0) then
         if (kmxn(ln(1, LL)) <= numtopsig .or. kmxn(ln(2, LL)) <= numtopsig) then
            insigpart = 1 ! one of the nodes is in the sigma part
         end if
      end if

      if (kmxn(ln(1, LL)) > kmxn(ln(2, LL))) then
         morelayersleft = 1
      else if (kmxn(ln(1, LL)) < kmxn(ln(2, LL))) then
         morelayersleft = 2
      else
         morelayersleft = 0
      end if

      do L = Lt, Lb, -1
         k1 = ln(1, L); k1t = k1
         k2 = ln(2, L); k2t = k2
         if (L == Lt) then
            k1t = ktop(ln(1, LL)); k2t = ktop(ln(2, LL))
         end if

         rhovol(L - Lb + 1) = 0.5d0 * ((zws(k1t) - zws(k1 - 1)) * rho(k1) + (zws(k2t) - zws(k2 - 1)) * rho(k2))
         if (jarhoxu > 0) then
            rhou(L) = rhovol(L - Lb + 1) / (0.5d0 * (zws(k1t) - zws(k1 - 1) + zws(k2t) - zws(k2 - 1)))
         end if
         rhovol(L - Lb + 1) = rhovol(L - Lb + 1) * dx(LL)

         rv1 = rvdn(k1)
         rv2 = rvdn(k2)
         gr1 = grn(k1)
         gr2 = grn(k2)

         if (L == Lb .and. morelayersleft /= 0) then ! extrapolate at 'bed' layer of deepest side

            if (morelayersleft == 1) then ! k=deep side, kz=shallow side
               k = k1; kt = ktop(ln(1, LL))
               kz = k2; ktz = ktop(ln(2, LL))
            else
               k = k2; kt = ktop(ln(2, LL))
               kz = k1; ktz = ktop(ln(1, LL))
            end if

            if (ktz - kz > 0) then ! shallow side extrapolates, coeffs based on shallow side:
               fzu = (zws(kz + 1) - zws(kz)) / (zws(kz + 1) - zws(kz - 1)); fzd = 1d0 - fzu
               rhow1 = fzu * rho(k + 1) + fzd * rho(k)
               rhow0 = 2d0 * rho(k) - rhow1
            else ! one layer
               rhow1 = rho(k)
               rhow0 = rhow1
            end if

            rhow1 = rhow1 - rhomean
            rhow0 = rhow0 - rhomean
            if (insigpart == 0) then
               dzz = zws(kz) - zws(kz - 1) ! shallow side

               rhovol(1) = dzz * 0.5d0 * (rho(k) + rho(kz)) * dx(LL)
               if (jarhoxu > 0) then
                  rhou(L) = 0.5d0 * (rho(k) + rho(kz))
               end if

            else
               dzz = zws(k) - zws(k - 1) ! deep side
            end if

            saw1 = fzu * constituents(isalt, k + 1) + fzd * constituents(isalt, k)
            tmw1 = fzu * constituents(itemp, k + 1) + fzd * constituents(itemp, k)
            saw0 = 2d0 * constituents(isalt, k) - saw1
            tmw0 = 2d0 * constituents(itemp, k) - tmw1

            if (.not. density_is_pressure_dependent()) then
               rhow0 = densfm(saw0, tmw0, 0d0) - rhomean
            else
               pdb = (zws(ktz) - zws(kz - 1)) * rhomean
               rvk = rvdn(k + 1) + 0.5d0 * dzz * (rhosww(k) + rhosww(k - 1))
               do i = 1, maxitpresdens
                  p0d = ag * (rvk + pdb) ! total pressure
                  rhow0 = densfm(saw0, tmw0, p0d) - rhomean
                  rvk = rvdn(k + 1) + 0.5d0 * dzz * (rhosww(k) + rhow0)
               end do
            end if

            rvk = rvdn(k + 1) + 0.5d0 * dzz * (rhosww(k) + rhow0)
            grk = (rvdn(k + 1) + 0.5d0 * dzz * (2d0 * rhosww(k) + rhow0) / 3d0) * dzz

            if (morelayersleft == 1) then ! k1=deepest
               rv1 = rvk; gr1 = grk
            else
               rv2 = rvk; gr2 = grk
            end if

            if (insigpart == 0) then
               gr3 = 0d0 ! no skewness for zlay jump at bed
            else
               gr3 = 0.5d0 * (rv1 + rv2) * (zws(k1 - 1) - zws(k2 - 1))
            end if

         else
            gr3 = 0.5d0 * (rv1 + rv2) * (zws(k1 - 1) - zws(k2 - 1))
         end if

         gradpu(L - Lb + 1) = gradpu(L - Lb + 1) + gr1 - gr2 + gr3
         if (L > Lb) then
            gradpu(L - Lb) = gradpu(L - Lb) - gr3 ! ceiling of ff# downstairs neighbours
         end if
      end do

      call barocLtimeint(gradpu, rhovol, LL, Lb, Lt)

   end subroutine addbarocLrho_w

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

! $Id: addbarocl.f90 142295 2023-01-10 08:25:27Z klapwijk $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/trunk/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/addbarocl.f90 $

   subroutine addbarocLorg(LL, Lb, Lt)
      use precision, only: dp
      use m_flowgeom
      use m_flow
      use m_flowtimes

      implicit none
      integer, intent(in) :: LL, Lb, Lt

      integer :: L, k1, k2, k1t, k2t
      real(kind=dp) :: gradpu(kmxx), rhovol(kmxx), gr3

      do L = Lb, Lt
         k1 = ln(1, L); k1t = k1
         k2 = ln(2, L); k2t = k2
         if (L == Lt) then
            k1t = ktop(ln(1, LL)); k2t = ktop(ln(2, LL))
         end if

         rhovol(L - Lb + 1) = 0.5d0 * (zws(k1t) - zws(k1 - 1) + zws(k2t) - zws(k2 - 1)) * dx(LL) * 0.5d0 * (rho(k1) + rho(k2)) ! write in rvdn
         if (jarhoxu > 0) then
            rhou(L) = 0.5d0 * (rho(k1) + rho(k2))
         end if

         gr3 = 0.5d0 * (rvdn(k1) + rvdn(k2)) * (zws(k1 - 1) - zws(k2 - 1))
         gradpu(L - Lb + 1) = grn(k1) - grn(k2) + gr3
         if (L > Lb) then
            gradpu(L - Lb) = gradpu(L - Lb) - gr3 ! ceiling of ff# downstairs neighbours
         end if
      end do

      call barocLtimeint(gradpu, rhovol, LL, Lb, Lt)

   end subroutine addbarocLorg

   subroutine barocLtimeint(gradpu, rhovol, LL, Lb, Lt)
      use precision, only: dp
      use m_flow
      use m_flowtimes

      implicit none

      integer :: LL, Lb, Lt
      real(kind=dp) :: gradpu(kmxx), rhovol(kmxx)

      integer :: L
      real(kind=dp) :: barocL, ft

      ! this last piece is identical to addbaroc2, that will be removed at some moment
      if (jabaroctimeint == 3) then ! original AB implementation

         do L = Lb, Lt
            if (rhovol(L - Lb + 1) > 0d0) then
               barocl = ag * gradpu(L - Lb + 1) / rhovol(L - Lb + 1) !
               adve(L) = adve(L) - 1.5d0 * barocl + 0.5d0 * dpbdx0(L)
               dpbdx0(L) = barocL
            end if
         end do

      else if (abs(jabaroctimeint) == 4) then ! AB + better drying flooding

         ft = 0.5d0 * dts / dtprev
         do L = Lb, Lt
            if (rhovol(L - Lb + 1) > 0d0) then
               barocl = ag * gradpu(L - Lb + 1) / rhovol(L - Lb + 1)
               if (dpbdx0(L) /= 0d0) then
                  adve(L) = adve(L) - (1d0 + ft) * barocl + ft * dpbdx0(L)
               else
                  adve(L) = adve(L) - barocl
               end if
               dpbdx0(L) = barocL
            end if
         end do

         do L = Lt + 1, Lb + kmxL(LL) - 1
            dpbdx0(L) = 0d0
         end do

      else

         do L = Lb, Lt
            if (rhovol(L - Lb + 1) > 0d0) then
               barocl = ag * gradpu(L - Lb + 1) / rhovol(L - Lb + 1) !  Explicit
               adve(L) = adve(L) - barocl
            end if
         end do

      end if

   end subroutine barocLtimeint

end module m_addbarocl
