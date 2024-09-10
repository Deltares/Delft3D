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

   subroutine MAKENET(japaramscreen)

      use m_netw
      use m_makenet ! NTYP ANGLE SIZE THICK NRX NRY
      use m_polygon
      use m_grid
      use m_missing
      use m_sferic
      use geometry_module, only: pinpok
      use gridoperations
      use m_flowparameters, only: bedslope
      use m_mergenodes
      use m_readyy

      implicit none

      integer, intent(in) :: japaramscreen !< Load parameter screen or not (1/0)
      double precision :: ael, cs, dx, dy, hs
      integer :: in, jn, k0, l0, m, mh, n, nh, nn, numkn, numln, jaklaar, jafive, L, k1, k2, n12, i, k, LL, mou2
      double precision :: siz
      double precision :: sn
      double precision :: xplmax, xplmin, xx, yplmax, yplmin, yy, asp, c, phi, dphi, dxt, rr, f, rl, dd, z2, zbn

      double precision :: X(8), Y(8), Z(8), XD, YD
      character(len=20) :: fnam

      if (japaramscreen == 1) then
         !ntyp = 7 ; nrx = 32 ; bedslope = 1d-4
         call MAKENETPARAMETERS()
      end if

      if (ntyp <= 5) then

         ! get parameters from polygon if available
         call pol2netparams()

         AEL = PI * THICK * THICK / 4 ! RDIAM in mm
         SIZ = SIZE
         HS = SIZE * 0.5d0
         CS = cos(ANGLE * PI / 180.); SN = sin(ANGLE * PI / 180.)

         if (NTYP <= 2) then
            DX = DX0
            DY = DY0
         else if (NTYP <= 3) then
            DX = SIZ * cos(ANGLE * PI / 180.)
            DY = SIZ * sin(ANGLE * PI / 180.)
         else if (NTYP == 4) then
            DX = 0.5d0 * SIZ; DY = DX * sqrt(3d0)
         else if (NTYP == 5) then
            DX = HS; DY = sqrt(3d0) * DX
         end if

         if (NPL > 0) then
            if (NRX <= 1) then
               NRX = (XPLMAX - XPLMIN) / DX
               NRY = (YPLMAX - YPLMIN) / DY
            else if (DX == 0) then
               DX = (XPLMAX - XPLMIN) / NRX
               DY = (YPLMAX - YPLMIN) / NRY
            end if
         end if

         if (NTYP == 0) then
            MC = NRX + 1; NC = NRY + 1
            call INCREASEGRID(MC, NC)
            do N = 1, NC
               do M = 1, MC
                  XC(M, N) = X0 + (M - 1) * DX * CS - (N - 1) * DY * SN
                  YC(M, N) = Y0 + (M - 1) * DX * SN + (N - 1) * DY * CS
                  if (jsferic == 1 .and. n > 1) then
                     c = cos(dg2rd * yc(m, n - 1))
                     asp = c * 1d0 + (1d0 - c) * 0.3d0
                     dy = dx * c * asp
                     YC(M, N) = YC(M, N - 1) + dy
                  end if

               end do
               if (jsferic == 1) then
                  if (dy * dg2rd * ra < 20000.d0) then
                     nc = n + 1
                     yc(1:mc, nc) = 90d0
                     xc(1:mc, nc) = xc(1:mc, nc - 1)
                     exit
                  end if
               end if
            end do

            call del_grid_outside_pol()

            ! CALL GRIDTONET()
            ! MC = 0 ; NC = 0; XC = DMISS; YC = DMISS

         else

            K0 = NUMK
            L0 = NUML
            NUMKN = (NRX + 1) * (NRY + 1)
            NUMLN = 6 * NUMKN

            call INCREASENETW(K0 + NUMKN, L0 + NUMLN)

            call readyy('makenet', 0d0)

            Z = Z0
            do N = 1, NRY
               call readyy('makenet', dble(n - 1) / dble(nry - 1))
               do M = 1, NRX
                  if (NTYP == 0) then
                     XX = dble(M - 1) * DX0
                     YY = dble(N - 1) * DY0
                     X(1) = X0 + XX * CS - YY * SN; NN = 4
                     Y(1) = Y0 + YY * CS + XX * SN
                     XX = XX + DX0
                     X(2) = X0 + XX * CS - YY * SN; NN = 4
                     Y(2) = Y0 + YY * CS + XX * SN
                     YY = YY + DY0
                     X(3) = X0 + XX * CS - YY * SN; NN = 4
                     Y(3) = Y0 + YY * CS + XX * SN
                     XX = XX - DX0
                     X(4) = X0 + XX * CS - YY * SN; NN = 4
                     Y(4) = Y0 + YY * CS + XX * SN
                     XD = 0.25d0 * (X(1) + X(2) + X(3) + X(4))
                     YD = 0.25d0 * (Y(1) + Y(2) + Y(3) + Y(4))
                  else if (NTYP == 1) then
                     XD = X0 + DX + dble(M - 1) * 2 * DX; NN = 4
                     YD = Y0 + DY + dble(N - 1) * 2 * DY
                     X(1) = XD; Y(1) = YD - DY
                     X(2) = XD + DX; Y(2) = YD
                     X(3) = XD; Y(3) = YD + DY
                     X(4) = XD - DX; Y(4) = YD
                  else if (NTYP == 2) then
                     JN = mod(M - 1, 2)
                     XD = X0 + DX + HS + dble(M - 1) * (DX + 2 * HS); NN = 6
                     YD = Y0 + DY + JN * DY + dble(N - 1) * (2 * DY)
                     X(1) = XD - HS; Y(1) = YD - DY
                     X(2) = XD + HS; Y(2) = YD - DY
                     X(3) = XD + HS + DX; Y(3) = YD
                     X(4) = XD + HS; Y(4) = YD + DY
                     X(5) = XD - HS; Y(5) = YD + DY
                     X(6) = XD - HS - DX; Y(6) = YD
                  else if (NTYP == 3) then
                     XD = X0 + DX + HS + dble(M - 1) * 2 * (DX + HS); NN = 6
                     YD = Y0 + DY + dble(N - 1) * 2 * DY
                     X(1) = XD - HS; Y(1) = YD - DY
                     X(2) = XD + HS; Y(2) = YD - DY
                     X(3) = XD + HS + DX; Y(3) = YD
                     X(4) = XD + HS; Y(4) = YD + DY
                     X(5) = XD - HS; Y(5) = YD + DY
                     X(6) = XD - HS - DX; Y(6) = YD
                  else if (NTYP == 4) then
                     XD = X0 + DX + dble(M - 1) * 2 * DX; NN = 6
                     YD = Y0 + DY + dble(N - 1) * 2 * DY
                     X(1) = XD - DX; Y(1) = YD - DY
                     X(2) = XD + DX; Y(2) = YD - DY
                     X(3) = XD + DX + DX; Y(3) = YD
                     X(4) = XD + DX; Y(4) = YD + DY
                     X(5) = XD - DX; Y(5) = YD + DY
                     X(6) = XD; Y(6) = YD
                  else if (NTYP == 5) then
                     mh = nrx / 2; nh = nry / 2
                     JN = mod(M - 1, 2)
                     XD = X0 + DX - HS + dble(M - 1 - mh) * (DX + 2 * HS); NN = 6
                     YD = Y0 + JN * DY + dble(N - 1 - nh) * (2 * DY) - dy
                     X(1) = XD - HS; Y(1) = YD - DY
                     X(2) = XD + HS; Y(2) = YD - DY
                     X(3) = XD + HS + DX; Y(3) = YD
                     X(4) = XD + HS; Y(4) = YD + DY
                     X(5) = XD - HS; Y(5) = YD + DY
                     X(6) = XD - HS - DX; Y(6) = YD
                  end if
                  call PINPOK(XD, YD, NPL, XPL, YPL, IN, jins, dmiss)
                  if (IN == 1) then
                     call ADDMAZE(X, Y, Z, NN, JAFIVE)
                  end if
               end do
            end do
         end if

      else if (NTYP == 6) then

         dx0 = 360d0 / nrx
         dy0 = dx0
         JAFIVE = 0
         jsferic = 1; jasfer3D = 1; jaklaar = 0; z = dmiss
         YY = 0d0
         dy0 = dx0

         K0 = NUMK
         L0 = NUML
         NUMKN = (NRX + 1) * (NRY + 1)
         NUMLN = 6 * NUMKN
         call INCREASENETW(K0 + NUMKN, L0 + NUMLN)

         do N = 1, NRY
            call readyy('makenet', dble(n - 1) / dble(nry - 1))

            call getdeltay(yy, dx0, dy0)

            if (yy + 1.5d0 * dy0 > 90d0) then
               dy0 = 90d0 - YY; jaklaar = 1; jafive = 0
            else
               if (dy0 * dg2rd * ra < dxdouble .and. jafive == 0) then
                  dx0 = 2d0 * dx0; jafive = 1
                  call getdeltay(yy, dx0, dy0)
               else
                  jafive = 0; n12 = 0
               end if
               if (yy + 1.5d0 * dy0 > 90d0) then
                  dy0 = 0.51d0 * (90d0 - yy)
               end if
            end if

            do M = 1, NRX
               XX = dble(M - 1) * DX0 + xwleft

               X(1) = XX
               Y(1) = YY

               if (jafive == 0) then
                  X(2) = XX + dx0
                  Y(2) = YY
                  X(3) = XX + dx0
                  Y(3) = YY + dy0
                  X(4) = XX
                  Y(4) = YY + dy0
                  NN = 4
               else
                  x(2) = XX + 0.5d0 * DX0
                  y(2) = YY
                  X(3) = XX + dx0
                  Y(3) = YY
                  X(4) = XX + dx0
                  Y(4) = YY + dy0
                  X(5) = XX
                  Y(5) = YY + dy0
                  NN = 5
               end if

               call PINPOK(XD, YD, NPL, XPL, YPL, IN, jins, dmiss)
               if (IN == 1 .and. x(3) <= xwleft + 360d0) then
                  jafive = 0
                  call ADDMAZE(X, Y, Z, NN, JAFIVE)
                  call ADDMAZE(X, -Y, Z, NN, JAFIVE)
               end if
            end do
            if (jaklaar == 1) exit

            yy = yy + dy0
         end do
         call MERGENODESINPOLYGON()

         do L = 1, numL
            k1 = kn(1, L); k2 = kn(2, L)
            if (k1 /= 0 .and. k2 /= 0) then ! jammer dan, nb na setnodadm nog zooi
               if ((nmk(k1) == 5 .or. nmk(k1) == 6) .and. (nmk(k2) == 5 .or. nmk(k2) == 6)) then
                  if (yk(k1) == yk(k2)) then
                     call DELLINK(L)
                  end if
               end if
            end if
         end do

      else if (NTYP == 7 .and. radius > 0) then

         K0 = 0
         L0 = 0
         NUMKN = NRX + 1
         NUMLN = NRX
         call INCREASENETW(2 * NUMKN, 2 * NUMLN)
         dphi = -pi / nrx
         dxt = 0d0
         rr = radius
         dx0 = dphi * rr
         do i = 1, 3
            phi = angle + pi + 0.5d0 * dphi
            dxt = 0d0
            k = k0 + 1
            L = L0
            xk(k) = x0 + rr * cos(phi)
            yk(k) = y0 + rr * sin(phi)
            zk(k) = zkuni - 0.5d0 * dx0 * bedslope
            do LL = 1, nrx - 1
               phi = phi + dphi; k = k + 1; L = L + 1
               xk(k) = x0 + rr * cos(phi)
               yk(k) = y0 + rr * sin(phi)
               call dbdistancehk(xk(k - 1), yk(k - 1), xk(k), yk(k), dx0)
               zk(k) = zk(k - 1) - bedslope * dx0
               dxt = dxt + dx0
               kn(1, L + L0) = k - 1
               kn(2, L + L0) = k
               kn(3, L + L0) = 1
            end do
            dxt = dxt + dx0
            f = pi * radius / dxt
            rr = rr * f
         end do

         k = k + 1
         xk(k) = x0 - 1.5d0 * radius
         yk(k) = y0 + 0.5d0 * dx0
         zk(k) = zkuni - 0.5d0 * dx0 * bedslope

         !call add2Dcell(xk(k)+3d0*radius,yk(k),zkuni,bedslope)

         do LL = 1, nrx - 1
            k = k + 1; L = L + 1
            xk(k) = xk(k - 1)
            yk(k) = yk(k - 1) + dx0
            zk(k) = zk(k - 1) - bedslope * dx0
            kn(1, L + L0) = k - 1
            kn(2, L + L0) = k
            kn(3, L + L0) = 1
         end do

         numk = k; numl = L

         rl = pi * radius
         dd = rl / dble(nrx)
         z2 = -(rl + dd) * bedslope
         zbn = -5d0 - (rl - 0.5d0 * dd) * bedslope

         fnam = 'c128_0001.tim'
         write (fnam(2:4), '(i3.3)') nrx
         call newfil(mou2, fnam)
         write (mou2, *) '0d0  ', z2 ! zk(k) + 5d0 - bedslope*dx0
         write (mou2, *) '9d10 ', z2 ! zk(k) + 5d0 - bedslope*dx0
         call doclose(mou2)

      else if (NTYP == 8) then ! 90 degrees bend 1D

         NUMKN = NRX + 1
         NUMLN = NRX
         call INCREASENETW(2 * NUMKN, 2 * NUMLN)

         K = 1; xk(k) = x0; yk(k) = y0; zk(k) = zkuni - 0.5 * bedslope * dx0
         L = 0; xd = 1d0; yd = 0d0
         do LL = 1, nrx
            k = k + 1; L = L + 1
            if (LL > nrx / 2) then
               xd = -0.5d0 * sqrt(2d0)
               yd = xd
            end if
            xk(k) = xk(k - 1) + dx0 * xd
            yk(k) = yk(k - 1) + dx0 * yd
            zk(k) = zk(k - 1) - bedslope * dx0
            kn(1, L) = k - 1
            kn(2, L) = k
            kn(3, L) = 1
         end do

         numk = k; numl = L

      end if

      call SETNODADM(0)
      call readyy('makenet', -1d0)

      return
   end subroutine MAKENET
