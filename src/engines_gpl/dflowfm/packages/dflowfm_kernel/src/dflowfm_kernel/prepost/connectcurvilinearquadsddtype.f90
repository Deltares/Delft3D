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

module m_connectcurvilinearquadsddtype
   use m_tegenovernodesandlink, only: tegenovernodesandlink
   use m_nextcel, only: nextcel
   use m_islinkadjacenttolink, only: islinkadjacenttolink
   use m_dellink, only: dellink

   implicit none

   private

   public :: connectcurvilinearquadsddtype

contains

   subroutine connectcurvilinearquadsDDtype()
      use precision, only: dp
      use m_closeenough, only: closeenough
      use m_netw
      use stdlib_sorting, only: sort_index
      use geometry_module, only: dbdistance, cross
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D
      use m_mergenodes
      use gridoperations
      use m_set_nod_adm
      use m_new_link

      integer :: ins
      integer :: ip
      integer :: ja
      integer :: ja1
      integer :: ja2
      integer :: jab
      integer :: k
      integer :: k1
      integer :: k1a
      integer :: k1b
      integer :: k1d
      integer :: k1e
      integer :: k1k
      integer :: k2
      integer :: k2a
      integer :: k2b
      integer :: k2d
      integer :: k2e
      integer :: k2k
      integer :: k3
      integer :: k4
      integer :: km
      integer :: km1
      integer :: km1b
      integer :: km2
      integer :: km2b
      integer :: km3
      integer :: kmd
      integer :: l
      integer :: l1
      integer :: l2
      integer :: la
      integer :: lb
      integer :: ld
      integer :: le
      integer :: li
      integer :: ll
      integer :: lnu
      integer :: m
      integer :: mer
      integer :: n
      integer :: nlinks
      integer :: np
      integer :: npb
      integer :: npd
      integer :: npe
      integer :: num
      integer :: nx = 5, ny
      integer, allocatable :: nnq(:), nadjq(:, :), L1adjq(:), LLadjq(:), L2adjq(:, :), merg(:, :), kins(:), kins2(:)

      real(kind=dp) :: r2, xm, ym, xkkn1, ykkn1

      integer, allocatable :: nnp(:), nnl(:), nrl(:), nnl2(:, :), k1L(:), k2L(:)

      real(kind=dp) :: sL, sm, xcr, ycr, crp
      integer :: i, jacross, k1dum
      real(kind=dp), dimension(:), allocatable :: dist
      integer, dimension(:), allocatable :: idx
      integer, dimension(:), allocatable :: kdum

      call findcells(0) ! find quads

      ny = 4 * nump
      allocate (nnq(ny), nadjq(nx, ny), L1adjq(ny), LLadjq(ny), L2adjq(nx, ny))
      nnq = 0; nadjq = 0; L1adjq = 0; LLadjq = 0; L2adjq = 0
      allocate (merg(2, ny), kins(2 * nx), kins2(2 * nx))
      merg = 0; kins = 0; kins2 = 0

      allocate (dist(2 * nx))
      dist = 0d0
      allocate (idx(2 * nx))
      idx = 0
      allocate (kdum(2 * nx))
      idx = 0

      n = 0
      do L = 1, numl ! locally store potential link nrs and their face nrs, only those with four edges
         if (lnn(L) == 1 .and. lc(L) == 1) then
            np = lne(1, L)
            if (netcell(np)%n == 4) then
               n = n + 1
            end if
         end if
      end do
      ny = n

      allocate (nnp(ny), nnl(ny), nrl(ny), nnl2(nx, ny), k1L(ny), k2L(ny))
      nnp = 0; nnl = 0; nrl = 0; nnl2 = 0; k1L = 0; k2L = 0

      n = 0
      do L = 1, numl ! locally store potential link nrs and their face nrs, only those with four edges
         if (lnn(L) == 1 .and. lc(L) == 1) then
            np = lne(1, L)
            if (netcell(np)%n == 4) then
               n = n + 1
               nnp(n) = np; nnl(n) = L
            end if
         end if
      end do

      kc(1:numk) = 1
      Lc(1:numk) = 1

      nlinks = n
      do L = 1, nlinks ! count nr of adjacent links per link and store adjacent links
         L1 = nnl(L)
         do LL = 1, nlinks
            L2 = nnl(LL)
            if (L /= LL) then
               call islinkadjacenttolink(L1, L2, ja, k1k, k2k)
               if (ja == 1) then
                  nrl(L) = nrl(L) + 1; nnl2(nrl(L), L) = LL ! nr of adj lnks      adj lnk nrs
                  if (k1k > 0) k1L(L) = k1k ! merg nod 1 to nod k1k
                  if (k2k > 0) k2L(L) = k2k ! merg nod 2 to nod k2k
               end if
            end if
         end do
      end do

      mer = 0
      do L = 1, nlinks
         if (nrl(L) >= 1) then
            L1 = nnl(L); np = nnp(L)
            k1 = kn(1, L1); k2 = kn(2, L1)

            xkkn1 = xk(k1)
            ykkn1 = yk(k1)

            kins = 0
            ins = 0
            do Li = 1, nrl(L)
               LL = nnl2(Li, L)
               L2 = nnl(LL)
               k3 = kn(1, L2); k4 = kn(2, L2)
               if (nrl(L) >= nrl(LL)) then
                  km1 = k1L(L) ! k merge 1
                  if (km1 > 0) then
                     if (kc(k1) == 1) then
                        mer = mer + 1; merg(1, mer) = k1; merg(2, mer) = km1; kc(k1) = -1
                     end if
                  end if

                  km2 = k2L(L) ! k merge 2
                  if (km2 > 0) then
                     if (kc(k2) == 1) then
                        mer = mer + 1; merg(1, mer) = k2; merg(2, mer) = km2; kc(k2) = -1
                     end if
                  end if

                  if (nrl(L) > nrl(LL)) then
                     if (kc(k3) == 1 .and. k3 /= km1 .and. k3 /= km2) then
                        ins = ins + 1; kins(ins) = k3; kc(k3) = 0
                     end if
                     if (kc(k4) == 1 .and. k4 /= km1 .and. k4 /= km2) then
                        ins = ins + 1; kins(ins) = k4; kc(k4) = 0
                     end if
                  end if

                  if (Lc(L1) == 1 .and. lc(L2) == 1) then
                     Lc(L1) = 0; kn(1, L1) = 0; kn(2, L1) = 0
                  end if
               end if
            end do

            if (ins > 0) then
!          SPvdP: first order kins
!          compute distance from kn(1,L1) to kins
               do i = 1, ins
                  dist(i) = dbdistance(xkkn1, ykkn1, xk(kins(i)), yk(kins(i)), jsferic, jasfer3D, dmiss)
               end do
!          get permutation array
               call sort_index(dist(1:ins), idx(1:ins))
!          order on increasing distance
               kdum = kins
               do i = 1, ins
                  kins(i) = kdum(idx(i))
               end do

!          get opposing netlink and nodes
               call tegenovernodesandlink(np, L1, k1a, k2a, La)

!          if lines kins(1)-k1a and kins(ins)-k2a cross: invert
               call cross(xk(k1), yk(k1), xk(k1a), yk(k1a), xk(k2), yk(k2), xk(k2a), yk(k2a), jacross, sL, sm, xcr, ycr, crp, jsferic, dmiss)
               if (jacross == 1) then
                  k1dum = k1a
                  k1a = k2a
                  k2a = k1dum
               end if

               if (ins == 1) then ! 1-on-2 coupling:
                  call newlink(kins(1), k1a, lnu) ! two diagonals from inside point to opp. corners
                  call newlink(kins(1), k2a, lnu)
               else if (ins == 2) then ! 1-on-3 coupling:
                  xm = 0.5d0 * (xk(k1a) + xk(k2a)) !
                  ym = 0.5d0 * (yk(k1a) + yk(k2a)) ! +-+---1---+
                  call dsetnewpoint(xm, ym, km) ! | | / |   |
                  call newlink(kins(1), km, lnu) ! +-i   |   |
                  call newlink(kins(1), k1a, lnu) ! | | > m   |
                  call newlink(kins(2), km, lnu) ! +-i   |   |
                  call newlink(kins(2), k2a, lnu) ! | | \ |   |
                  ! +-+---2---+
                  call newlink(km, k1a, lnu)
                  call newlink(km, k2a, lnu)

                  call nextcel(np, La, npb, k1b, k2b, Lb)
                  call dellink(La)

                  if (npb == 0) cycle
                  call newlink(km, k1b, lnu)
                  call newlink(km, k2b, lnu)

               else if (ins == 3) then

                  xm = 0.5d0 * (xk(k1a) + xk(k2a))
                  ym = 0.5d0 * (yk(k1a) + yk(k2a))
                  call dsetnewpoint(xm, ym, km)
                  call newlink(kins(2), km, lnu)
                  call newlink(km, k2a, lnu)
                  call newlink(km, k1a, lnu)

                  call newlink(kins(1), km, lnu)
                  call newlink(kins(1), k1a, lnu)

                  call newlink(kins(3), km, lnu)
                  call newlink(kins(3), k2a, lnu)

                  call nextcel(np, La, npb, k1b, k2b, Lb)
                  call dellink(La)

                  if (npb == 0) cycle
                  call newlink(km, k1b, lnu)
                  call newlink(km, k2b, lnu)

               else if (ins == 4) then

                  call nextcel(np, La, npb, k1b, k2b, Lb)
                  call nextcel(npb, Lb, npd, k1d, k2d, Ld)
                  call nextcel(npd, LD, npe, k1e, k2e, Le)
! SPvdP: check and fix orientation if applicable
                  if (k1b > 0 .and. k2b > 0) then
                     call cross(xk(k1a), yk(k1a), xk(k1b), yk(k1b), xk(k2a), yk(k2a), xk(k2b), yk(k2b), jacross, sL, sm, xcr, ycr, crp, jsferic, dmiss)
                     if (jacross == 1) then
                        k1dum = k1b
                        k1b = k2b
                        k2b = k1dum
                     end if

                     if (k1d > 0 .and. k2d > 0) then
                        call cross(xk(k1b), yk(k1b), xk(k1d), yk(k1d), xk(k2b), yk(k2b), xk(k2d), yk(k2d), jacross, sL, sm, xcr, ycr, crp, jsferic, dmiss)
                        if (jacross == 1) then
                           k1dum = k1d
                           k1d = k2d
                           k2d = k1dum
                        end if

                        if (k1e > 0 .and. k2e > 0) then
                           call cross(xk(k1d), yk(k1d), xk(k1e), yk(k1e), xk(k2d), yk(k2d), xk(k2e), yk(k2e), jacross, sL, sm, xcr, ycr, crp, jsferic, dmiss)
                           if (jacross == 1) then
                              k1dum = k1e
                              k1e = k2e
                              k2e = k1dum
                           end if
                        end if
                     end if
                  end if

                  xm = 0.75d0 * xk(k1a) + 0.25d0 * xk(k2a)
                  ym = 0.75d0 * yk(k1a) + 0.25d0 * yk(k2a)
                  call dsetnewpoint(xm, ym, km1)

                  xm = 0.50d0 * xk(k1a) + 0.50d0 * xk(k2a)
                  ym = 0.50d0 * yk(k1a) + 0.50d0 * yk(k2a)
                  call dsetnewpoint(xm, ym, km2)

                  xm = 0.25d0 * xk(k1a) + 0.75d0 * xk(k2a)
                  ym = 0.25d0 * yk(k1a) + 0.75d0 * yk(k2a)
                  call dsetnewpoint(xm, ym, km3)

                  call newlink(kins(2), km2, lnu)
                  call newlink(kins(3), km2, lnu)

                  call newlink(kins(2), km1, lnu)
                  call newlink(kins(3), km3, lnu)

                  call newlink(kins(1), km1, lnu)
                  call newlink(kins(4), km3, lnu)

                  call newlink(k1a, km1, lnu)
                  call newlink(km1, km2, lnu)
                  call newlink(km2, km3, lnu)
                  call newlink(km3, k2a, lnu)

                  call dellink(La)
                  if (npb == 0) cycle

                  xm = 0.66d0 * xk(k1b) + 0.34d0 * xk(k2b)
                  ym = 0.66d0 * yk(k1b) + 0.34d0 * yk(k2b)
                  call dsetnewpoint(xm, ym, km1b)

                  xm = 0.34d0 * xk(k1b) + 0.66d0 * xk(k2b)
                  ym = 0.34d0 * yk(k1b) + 0.66d0 * yk(k2b)
                  call dsetnewpoint(xm, ym, km2b)

                  call newlink(km1, km1b, lnu)
                  call newlink(km1b, km2, lnu)
                  call newlink(km2, km2b, lnu)
                  call newlink(km2b, km3, lnu)

                  call newlink(k1b, km1b, lnu)
                  call newlink(km1b, km2b, lnu)
                  call newlink(km2b, k2b, lnu)

                  call dellink(Lb)
                  if (npd == 0) cycle

                  xm = 0.5d0 * xk(k1d) + 0.5d0 * xk(k2d)
                  ym = 0.5d0 * yk(k1d) + 0.5d0 * yk(k2d)
                  call dsetnewpoint(xm, ym, kmd)

                  call newlink(km1b, kmd, lnu)
                  call newlink(km2b, kmd, lnu)

                  call newlink(k1d, kmd, lnu)
                  call newlink(k2d, kmd, lnu)

                  call dellink(Ld)
                  if (npe == 0) cycle

                  call newlink(kmd, k2e, lnu)
                  call newlink(kmd, k1e, lnu)
               end if
            end if

         end if
      end do

      do m = 1, mer
         k1 = merg(1, m); k2 = merg(2, m)
         if (kc(k2) /= 0) then
            call mergenodes(k1, k2, ja, .false.)
            kc(k2) = 0
         end if
      end do

      call setnodadm(0)
      kc(1:numk) = 1

      goto 1234
      return

      mer = 0
      do np = 1, nump
         if (nnq(np) >= 1) then ! cell with neighbours
            L1 = L1adjq(np)
            K1 = kn(1, L1); K2 = kn(2, L1)

            jab = 0
            if (lc(L1) == 1) then ! eigen link bestaat nog
               kn(1, L1) = 0; kn(2, L1) = 0; jab = 1 ! direct link grote cel opheffen
            end if

            ins = 0 ! nr of 'inside' points of small cell (i.e. not cornering points)
            ip = 1 ! in adjacent cell 1-3 and 2-4 instead of 1-4- and 2-3
            do num = 1, nnq(np)
               if (jab == 1) then
                  LC(L2adjq(num, np)) = -1 ! via -1 vlaggen dat de buren niet ook stuk hoeven
               end if

               L2 = L2adjq(num, np)
               k3 = kn(1, L2); k4 = kn(2, L2)

               if (k3 == 0 .or. k4 == 0) exit

               r2 = DBDISTANCE(XK(K3), YK(K3), XK(K4), YK(K4), jsferic, jasfer3D, dmiss); r2 = 0.3d0 * r2

               if (kc(k3) > 0) then
                  call closeenough(XK(K3), YK(K3), XK(K1), YK(K1), r2, ja1)
                  call closeenough(XK(K3), YK(K3), XK(K2), YK(K2), r2, ja2)
                  if (ja1 == 1) then
                     mer = mer + 1; merg(1, mer) = k1; merg(2, mer) = k3; kc(k3) = -1
                  else if (ja2 == 1) then
                     mer = mer + 1; merg(1, mer) = k2; merg(2, mer) = k3; kc(k3) = -1; ip = -1
                  else
                     if (nnq(np) > 1) then
                        ins = ins + 1; kins(ins) = k3; kc(k3) = -k3
                     end if
                  end if
               end if

               if (kc(k4) > 0) then
                  call closeenough(XK(K4), YK(K4), XK(K1), YK(K1), r2, ja1)
                  call closeenough(XK(K4), YK(K4), XK(K2), YK(K2), r2, ja2)
                  if (ja1 == 1) then
                     mer = mer + 1; merg(1, mer) = k1; merg(2, mer) = k4; kc(k4) = -1; ip = -1
                  else if (ja2 == 1) then
                     mer = mer + 1; merg(1, mer) = k2; merg(2, mer) = k4; kc(k4) = -1
                  else
                     if (nnq(np) > 1) then
                        ins = ins + 1; kins(ins) = k4; kc(k4) = -k4
                     end if
                  end if
               end if

            end do

            if (ip < 1) then
               kins2 = kins
               do k = 1, ins
                  kins(k) = kins2(ins - k + 1)
               end do
            end if

            if (ins >= 1) then ! connection of inside points to opposite face
               call tegenovernodesandlink(np, L1, k1a, k2a, La)
               if (ins == 1) then
                  call newlink(kins(1), k1a, lnu)
                  call newlink(kins(1), k2a, lnu)
               else if (ins == 2) then

                  xm = 0.5d0 * (xk(k1a) + xk(k2a))
                  ym = 0.5d0 * (yk(k1a) + yk(k2a))
                  call dsetnewpoint(xm, ym, km)
                  call newlink(kins(1), km, lnu)
                  call newlink(kins(1), k1a, lnu)
                  call newlink(kins(2), km, lnu)
                  call newlink(kins(2), k2a, lnu)

                  call newlink(km, k1a, lnu)
                  call newlink(km, k2a, lnu)

                  call nextcel(np, La, npb, k1b, k2b, Lb)
                  call dellink(La)

                  if (npb == 0) exit
                  call newlink(km, k1b, lnu)
                  call newlink(km, k2b, lnu)

               else if (ins == 3) then

                  xm = 0.5d0 * (xk(k1a) + xk(k2a))
                  ym = 0.5d0 * (yk(k1a) + yk(k2a))
                  call dsetnewpoint(xm, ym, km)
                  call newlink(kins(2), km, lnu)
                  call newlink(km, k2a, lnu)
                  call newlink(km, k1a, lnu)

                  call newlink(kins(1), km, lnu)
                  call newlink(kins(1), k1a, lnu)

                  call newlink(kins(3), km, lnu)
                  call newlink(kins(3), k2a, lnu)

                  call nextcel(np, La, npb, k1b, k2b, Lb)
                  call dellink(La)

                  if (npb == 0) exit
                  call newlink(km, k1b, lnu)
                  call newlink(km, k2b, lnu)

               else if (ins == 4) then

                  call nextcel(np, La, npb, k1b, k2b, Lb)
                  call nextcel(npb, Lb, npd, k1d, k2d, Ld)
                  call nextcel(npd, LD, npe, k1e, k2e, Le)

                  xm = 0.75d0 * xk(k1a) + 0.25d0 * xk(k2a)
                  ym = 0.75d0 * yk(k1a) + 0.25d0 * yk(k2a)
                  call dsetnewpoint(xm, ym, km1)

                  xm = 0.50d0 * xk(k1a) + 0.50d0 * xk(k2a)
                  ym = 0.50d0 * yk(k1a) + 0.50d0 * yk(k2a)
                  call dsetnewpoint(xm, ym, km2)

                  xm = 0.25d0 * xk(k1a) + 0.75d0 * xk(k2a)
                  ym = 0.25d0 * yk(k1a) + 0.75d0 * yk(k2a)
                  call dsetnewpoint(xm, ym, km3)

                  call newlink(kins(2), km2, lnu)
                  call newlink(kins(3), km2, lnu)

                  call newlink(kins(2), km1, lnu)
                  call newlink(kins(3), km3, lnu)

                  call newlink(kins(1), km1, lnu)
                  call newlink(kins(4), km3, lnu)

                  call newlink(k1a, km1, lnu)
                  call newlink(km1, km2, lnu)
                  call newlink(km2, km3, lnu)
                  call newlink(km3, k2a, lnu)

                  call dellink(La)
                  if (npb == 0) exit

                  xm = 0.66d0 * xk(k1b) + 0.34d0 * xk(k2b)
                  ym = 0.66d0 * yk(k1b) + 0.34d0 * yk(k2b)
                  call dsetnewpoint(xm, ym, km1b)

                  xm = 0.34d0 * xk(k1b) + 0.66d0 * xk(k2b)
                  ym = 0.34d0 * yk(k1b) + 0.66d0 * yk(k2b)
                  call dsetnewpoint(xm, ym, km2b)

                  call newlink(km1, km1b, lnu)
                  call newlink(km1b, km2, lnu)
                  call newlink(km2, km2b, lnu)
                  call newlink(km2b, km3, lnu)

                  call newlink(k1b, km1b, lnu)
                  call newlink(km1b, km2b, lnu)
                  call newlink(km2b, k2b, lnu)

                  call dellink(Lb)
                  if (npd == 0) exit

                  xm = 0.5d0 * xk(k1d) + 0.5d0 * xk(k2d)
                  ym = 0.5d0 * yk(k1d) + 0.5d0 * yk(k2d)
                  call dsetnewpoint(xm, ym, kmd)

                  call newlink(km1b, kmd, lnu)
                  call newlink(km2b, kmd, lnu)

                  call newlink(k1d, kmd, lnu)
                  call newlink(k2d, kmd, lnu)

                  call dellink(Ld)
                  if (npe == 0) exit

                  call newlink(kmd, k2e, lnu)
                  call newlink(kmd, k1e, lnu)

               end if
            end if

         end if

      end do

      do m = 1, mer
         k1 = merg(1, m); k2 = merg(2, m)
         if (kc(k2) /= 0) then
            call mergenodes(k2, k1, ja, .false.)
         end if
      end do

      call setnodadm(0)
      kc(1:numk) = 1

1234  continue

      deallocate (nnq, nadjq, L1adjq, LLadjq, L2adjq, merg, kins, kins2)

      deallocate (dist, idx, kdum)

   end subroutine connectcurvilinearquadsDDtype

end module m_connectcurvilinearquadsddtype
