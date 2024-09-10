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

   subroutine Triangulatesamplestonetwork(JADOORLADEN)
      use m_netw, only: numk, numl, kn, xk, yk, zk, nb, LMAX, KMAX
      use M_SAMPLES
      use m_ec_triangle
      use M_ALLOC
      use m_missing, only: dmiss, JINS
      use m_ec_basic_interpolation, only: dlaun
      use geometry_module, only: pinpok, dbpinpol, get_startend
      use gridoperations
      use m_polygon ! , only: savepol, restorepol
      use m_mergenodes
      use m_readyy
      implicit none
      integer :: jadoorladen ! ,npl
      !double precision :: xpl(npl),ypl(npl)
      double precision :: af
      integer :: in
      integer :: ja
      integer :: k
      integer :: k0
      integer :: k1
      integer :: k2
      integer :: k3
      integer :: ksx
      integer :: l
      integer :: l0
      integer :: ll
      integer :: n
      integer :: nn
      integer :: nsdl
      integer :: nsin
      integer :: nmod

      integer :: jstart, jend

      integer :: IERR

      integer, allocatable :: KS(:)
      double precision :: XP, YP, THIRD, phimin, phimax

      THIRD = 1d0 / 3d0

      call FINDCELLS(0)

      call MAKENETNODESCODING()

      if (JADOORLADEN == 0) then
         K0 = 0
         L0 = 0
      else
         K0 = NUMK
         L0 = NUML
      end if

      ! CALL SAVEPOL()
      call SAVESAM()

      N = 0
      do K = 1, NS ! SELECT SAMPLES IN POLYGON
         if (NPL /= 0) then
            call PINPOK(XS(K), YS(K), NPL, XPL, YPL, IN, jins, dmiss)
         else
            IN = 1
         end if
         if (IN == 1) then
            N = N + 1
            XS(N) = XS2(K)
            YS(N) = YS2(K)
            ZS(N) = ZS2(K)
         end if
      end do
      NSIN = N

      call INCREASENETW(K0 + NSIN, L0 + 6 * NSIN)

      KSX = 6 * NSIN + 100000
      allocate (KS(KSX))

      N = 0 ! ADD SELECTED SAMPLES TO NETWORK + ADMIN NODE NRS
      do K = K0 + 1, K0 + NSIN
         N = N + 1
         XK(K) = XS(N)
         YK(K) = YS(N)
         ZK(K) = ZS(N)
         KS(N) = K
      end do

      N = NSIN ! ADD NETPOINTS IN ORIGINAL NUMK SET TO SAMPLES
      if (NPL > 0) then
         do K = 1, K0 ! NUMK of old net
            if (NB(K) == 2 .or. NB(K) == 3) then
               N = N + 1
               call INCREASESAM(N)
               XS(N) = XK(K)
               YS(N) = YK(K)
               ZS(N) = ZK(K)
               KS(N) = K
            end if
         end do
      end if

      if (N < 1 .and. NPL > 0) then ! IF THERE AREN'T ANY SAMPLES YET, USE THE POLYGON
         call get_startend(NPL, XPL, YPL, jstart, jend, dmiss)
         NSIN = max(jend - jstart + 1, 0)
         call increasesam(NSIN)
         call INCREASENETW(K0 + NSIN, L0 + 6 * NSIN)
         KSX = 6 * NSIN + 100000
         call REALLOC(KS, KSX)

         do k = jstart, jend
            N = N + 1
            XS(N) = XPL(K)
            YS(N) = YPL(K)
            ! ZS(N) = ZPL(K)

            XK(K) = XS(N)
            YK(K) = YS(N)
            ! ZK(K) = ZS(N)
            KS(N) = K
         end do

      end if

      NSDL = N

      call READYY('TRIANGULATING', 0d0)

      call DLAUN(XS, YS, NSDL, 3, ierr)

      call READYY('TRIANGULATING', 0.3d0)

      IN = -1
      ! Check triangles and disable some links if necessary.
      NMOD = int(NUMTRI / 40.0) + 1
      do N = 1, NUMTRI
         if (mod(N, NMOD) == 0) then
            AF = 0.3d0 + 0.4d0 * dble(N) / dble(NUMTRI)
            call READYY('TRIANGULATING', AF)
         end if

         JA = 1
         call CHECKTRIANGLE(N, JA, phimin, phimax)
         ! Mark an edge with minus sign if triangle is correct!
         if (JA == 0) then
            cycle
         end if

         K1 = INDX(1, N); K2 = INDX(2, N); K3 = INDX(3, N)
         K1 = KS(K1); K2 = KS(K2); K3 = KS(K3)
         XP = THIRD * (XK(K1) + XK(K2) + XK(K3))
         YP = THIRD * (YK(K1) + YK(K2) + YK(K3))
         call DBPINPOL(XP, YP, IN, dmiss, JINS, NPL, xpl, ypl, ypl)
         if (IN == 0) then
            cycle
         else
            ! Mark an edge with minus sign if triangle is correct!
            do NN = 1, 3
               K1 = TRIEDGE(NN, N)
               EDGEINDX(1, K1) = -abs(EDGEINDX(1, K1))
            end do
         end if
      end do

      ! All triangles were just checked, and for all good ones, their edges
      ! were marked with a minus sign. Add only these to kn array.
      NMOD = int(NUMEDGE / 30.0) + 1
      L = L0
      do LL = 1, NUMEDGE
         if (mod(N, NMOD) == 0) then
            AF = 0.7d0 + 0.3d0 * dble(LL) / dble(NUMEDGE)
            call READYY('TRIANGULATING', AF)
         end if
         if (EDGEINDX(1, LL) > 0) then
            cycle
         else
            EDGEINDX(1, LL) = abs(EDGEINDX(1, LL))
         end if
         L = L + 1
         KN(1, L) = KS(EDGEINDX(1, LL))
         KN(2, L) = KS(EDGEINDX(2, LL))
         KN(3, L) = 2

         call setcol(31)
         call movabs(xk(kn(1, L)), yk(kn(1, L)))
         call lnabs(xk(kn(2, L)), yk(kn(2, L)))

         if (L > LMAX) then
            write (*, *) 'INCREASENETW(KMAX, INT(1.2d0*NUML) )', NUML
            call INCREASENETW(KMAX, int(1.2d0 * NUML))
         end if

      end do

      call READYY('TRIANGULATING', -1d0)

      NUMK = K0 + NSIN
      NUML = L

      ! merge nodes in polygon
      !call mergenodesinpolygon()

      ns = 0 ! call delsam(1)
      npl = 0 ! call delpol()
      call SETNODADM(0) ! No cross checks for now.

      deallocate (KS, NB)
      if (allocated(TRIEDGE)) then
         deallocate (TRIEDGE, EDGEINDX)
      end if

      return
   end subroutine Triangulatesamplestonetwork
