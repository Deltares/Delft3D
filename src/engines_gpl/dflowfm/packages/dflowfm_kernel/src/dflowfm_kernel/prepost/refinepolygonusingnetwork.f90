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

   subroutine REFINEPOLYGONUSINGNETWORK()
      use m_closein, only: closein
      use m_checktriangle, only: checktriangle
      use m_netw
      use M_SAMPLES
      use m_ec_triangle
      use M_POLYGON
      use m_ec_basic_interpolation, only: dlaun
      use geometry_module, only: pinpok, dpinpok, dbdistance
      use m_missing, only: dmiss, jins
      use m_sferic, only: jsferic, jasfer3D
      use gridoperations
      use m_readyy
      use m_set_nod_adm

      implicit none

      double precision :: a
      double precision :: af
      double precision :: disav, TRIANGLESIZE
      integer :: ierr
      integer :: in
      integer :: innump
      integer :: ja
      integer :: jadoorladen
      integer :: jarand
      integer :: k
      integer :: k0
      integer :: k1
      integer :: k1l
      integer :: k2
      integer :: k2l
      integer :: kk
      integer :: l
      integer :: l0
      integer :: ll
      integer :: n
      integer :: n1
      integer :: n2
      integer :: nav
      integer :: new
      integer :: nh
      integer :: nkin
      integer :: nn
      integer :: nn2
      integer :: nsdl
      integer :: nsin
      double precision :: rln, rlp, xa, ya, xkk, ykk, phimin, phimax

      double precision :: X1, Y1, X2, Y2
      double precision, allocatable :: XH(:), YH(:)
      integer, allocatable :: KIN(:), KS(:)

      if (NPL <= 2) return

      call FINDCELLS(0)

      JADOORLADEN = 1
      if (JADOORLADEN == 0) then
         K0 = 0
         L0 = 0
      else
         K0 = NUMK
         L0 = NUML
      end if

      allocate (KIN(NUMK), STAT=IERR); KIN = 0
      call AERR('KIN(NUMK)', IERR, NUMK)
      N = 0
      do K = 1, NUMK ! SELECT outer GRIDPOINTS INSIDE POLYGON
         JARAND = 0
         do NN = 1, NMK(K)
            L = NOD(K)%LIN(NN)
            if (LNN(L) == 1) JARAND = 1
         end do
         if (JARAND == 1) then
            call PINPOK(XK(K), YK(K), NPL, XPL, YPL, IN, jins, dmiss)
            if (IN == 1) then
               N = N + 1
               KIN(N) = K
            end if
         end if
      end do
      NKIN = N

      DISAV = 0; NAV = 0 ! AVERAGE GRIDSIZE SELECTED CELLS
      do N = 1, NKIN
         K1 = KIN(N)
         do NN = 1, NMK(K1)
            L = NOD(K1)%LIN(NN)
            call OTHERNODE(K1, L, K2)
            DISAV = DISAV + DBDISTANCE(XK(K1), YK(K1), XK(K2), YK(K2), jsferic, jasfer3D, dmiss)
            NAV = NAV + 1
         end do

      end do
      if (NAV /= 0) then
         DISAV = DISAV / dble(NAV)
         TRIANGLESIZE = DISAV
      else
         DISAV = TRIANGLESIZE
      end if

      RLP = 0d0
      do N = 1, NPL ! COUNT NR OF POINTS ON POLYGON
         N1 = N
         N2 = N + 1; if (N == NPL) N2 = 1
         X1 = XPL(N1); Y1 = YPL(N1); X2 = XPL(N2); Y2 = YPL(N2)
         RLP = RLP + DBDISTANCE(X1, Y1, X2, Y2, jsferic, jasfer3D, dmiss)
      end do
      NH = 4 * RLP / DISAV
      allocate (XH(NH), YH(NH), STAT=IERR)
      call AERR('XH(NH), YH(NH)', IERR, NH * 2)

      K = 0
      do N = 1, NPL ! SET NEW POINTS ON POLYGON
         N1 = N
         N2 = N + 1; if (N == NPL) N2 = 1
         X1 = XPL(N1); Y1 = YPL(N1); X2 = XPL(N2); Y2 = YPL(N2)
         RLN = DBDISTANCE(X1, Y1, X2, Y2, jsferic, jasfer3D, dmiss)
         NN = nint(RLN / DISAV)
         call INCELLS(X1, Y1, INNUMP)
         if (INNUMP == 0) then
            K = K + 1
            XH(K) = X1; YH(K) = Y1
         end if
         NN2 = 3 * NN
         do N2 = 1, NN2
            A = dble(N2) / dble(NN2)
            XA = (1 - A) * X1 + A * X2
            YA = (1 - A) * Y1 + A * Y2
            call INCELLS(XA, YA, INNUMP) ! XA, YA ZIT IN CELL NR INNUMP
            if (INNUMP == 0) then
               if (mod(N2, 3) == 0) then
                  K = K + 1
                  XH(K) = XA; YH(K) = YA
               end if
            else
               call CLOSEIN(XA, YA, INNUMP, KIN, NKIN, KK) ! KK IS HET MEEST DICHTBIJ GELEGEN POINT VAN INNUMP
               if (KK /= 0) then
                  XKK = XK(KK); YKK = YK(KK)
                  if (K == 0) then
                     K = K + 1; XH(K) = XKK; YH(K) = YKK
                  else if (XKK /= XH(K)) then
                     K = K + 1; XH(K) = XKK; YH(K) = YKK
                  end if
               end if
            end if
         end do
      end do
      NPL = K
      XPL(1:NPL) = XH(1:NPL)
      YPL(1:NPL) = YH(1:NPL)

      deallocate (XH, YH, KIN)

      return

      call INCREASENETW(K0 + NSIN, L0 + 6 * NSIN)

      allocate (KS(6 * NSIN))

      N = 0 ! ADD SELECTED SAMPLES TO NETWORK
      do K = K0 + 1, K0 + NSIN
         N = N + 1
         XK(K) = XS(N)
         YK(K) = YS(N)
         ZK(K) = ZS(N)
         KS(N) = K
      end do

      N = NSIN ! ADD NETPOINTS IN ORIGINAL NUMK SET TO SAMPLES
      if (NPL > 0) then
         do K = 1, NUMK ! NUM STILL OLD
            call DPINPOK(XK(K), YK(K), ZK(K), NPL, XPL, YPL, IN, jins, dmiss)
            if (IN == 1) then
               N = N + 1
               XS(N) = XK(K)
               YS(N) = YK(K)
               ZS(N) = ZK(K)
               KS(N) = K
            end if
         end do
      end if
      NSDL = N

      call READYY('TRIANGULATING', 0d0)

      call DLAUN(XS, YS, NSDL, 1, ierr)

      call READYY('TRIANGULATING', 0.3d0)

      L = L0
      do N = 1, NUMTRI
         AF = 0.3d0 + 0.7d0 * dble(N) / dble(NUMTRI)
         call READYY('TRIANGULATING', AF)

         JA = 1
         call CHECKTRIANGLE(N, JA, phimin, phimax)
         if (JA == 0) then
            cycle
         end if
         do NN = 1, 3
            N1 = NN; N2 = N1 + 1; if (N1 == 3) N2 = 1
            K1 = INDX(N1, N); K2 = INDX(N2, N)
            K1 = KS(K1); K2 = KS(K2)

            NEW = 1
            do LL = NUML, 1, -1
               K1L = KN(1, LL); K2L = KN(2, LL)
               if (K1 == K1L .and. K2 == K2L .or. &
                   K2 == K1L .and. K1 == K2L) then
                  NEW = 0; exit
               end if
            end do

            if (NEW == 0) cycle

            L = L + 1; 
            if (L > LMAX) then
               call INCREASENETW(int(1.2d0 * NUMK), int(1.2d0 * NUML))
            end if
            NUML = L
            KN(1, L) = K1; KN(2, L) = K2

         end do
      end do

      deallocate (KS)

      call READYY('TRIANGULATING', -1d0)

      NUMK = K0 + NSIN
      NUML = L

      call SETNODADM(1)

      return
   end subroutine REFINEPOLYGONUSINGNETWORK
