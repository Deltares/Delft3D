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

   subroutine ORTHOGONISENET_old()

      use m_netw
      use M_FLOWGEOM
      use M_POLYGON
      use M_SFERIC
      use m_orthosettings
      use m_missing
      use geometry_module, only: dbdistance, cross, normaloutchk, GETCIRCUMCENTER, dlinedis
      use gridoperations
      use m_readyy

      implicit none

      double precision :: X0, Y0, X1, Y1, W0, XL, YL, XR, YR, ZZZ
      double precision :: X2, Y2, X3, Y3, X4, Y4, A, B, DIS2, DIS3, XN, YN
      integer :: JACROS
      double precision :: SL, SM, XCR, YCR, CRP
      double precision :: R01, R23
      double precision, allocatable :: WW(:, :)
      integer, allocatable :: KK1(:, :)
      integer :: I, N, NO, NN, L, LL, K, KK, K0, K1, K2, K3, KL, KR, kprev, knext, kdone, NMKX, NR, K1L, JA, JA2, JA3
      double precision :: ATPF1
      integer, save :: NUMKO = 0, NUMLO = 0
      double precision :: area, areatot, xzwr, yzwr, rout
      double precision, external :: dprodin
      double precision :: relaxin, relax1

      integer :: JSFERICold

      double precision, allocatable :: xbd(:, :), ybd(:, :), xv(:), yv(:)
      integer, allocatable :: KC00(:)

      integer, allocatable :: kccell(:), lnnl(:)

      ATPF1 = 1 - ATPF

      allocate (xv(4), yv(4), lnnl(4))
      call FINDCELLS(0)

      ! Mark flow geometry as reset to prevent any crashes on redrawing with incomplete xz/yz arrays:
      ! Moreover: xz ordering is here still by netcell order, and *before* the flow node renumbering.
      ndx = 0
      lnx = 0

      JSFERICold = JSFERIC

      ! NMKX is max nr of neighbouring netnodes for any node.
      NMKX = 0
      do K = 1, NUMK
         NMKX = max(NMKX, NMK(K))
      end do
      NMKX = NMKX + 1 ! Possibly one additional dummy point at boundary nodes.
      if (allocated(XK1)) deallocate (XK1, YK1)
      if (allocated(WW)) deallocate (WW, KK1)
      allocate (XK1(NUMK), YK1(NUMK))
      allocate (WW(NMKX, NUMK), KK1(NMKX, NUMK)) !< Relative attraction weight of all neighbour nodes for all numk nodes.
      allocate (xbd(2, numk), ybd(2, numk)) !< Fake nodes attached to bd nodes (inefficient mem usage, but fast access)
      !< One per edge node, two for a corner node
      allocate (kccell(nump), KC00(NUMK))
      kccell = 0

      K1 = 0
      do K = 1, NUMK ! KK ADMIN
         if (KC(K) == 1) then
            do KK = 1, NMK(K)
               L = NOD(K)%LIN(KK)
               call OTHERNODE(K, L, KK1(KK, K))
            end do
         end if
      end do
      KC00 = KC
      A = 0.95d0; B = 1d0 - A

      call MAKENETNODESCODING() ! No relinking at the moment: makenetnodescoding outside of numortho loop

      ! Orthogonalization consists of 3 steps (see below). All three are done itatp times.
      ! The actual moving of points (step 3) is done (itatp*)itbnd*itin times.

      call READYY('Orthogonise net', 0d0)
      do NO = 1, ITATP

         WW = 0

         !     CALL REMOVESMALLLINKS()

         NUMKO = NUMK; NUMLO = NUML

         XK1(1:NUMK) = XK(1:NUMK); YK1(1:NUMK) = YK(1:NUMK)

!
! 0. Simple smoothing
!
         !DO K0 = 1, 0 ! NUMK
         !   IF (NB(K0) == 1) THEN
         !      X0 = XK1(K0) ; Y0 = YK1(K0)
         !      X1 = 0       ; Y1 = 0
         !      DO KK  = 1,NMK(K0)
         !         K1L = KK1(KK,K0)
         !         X1  = X1 + XK1(K1L) ; Y1  = Y1 + YK1(K1L)
         !      ENDDO
         !      X1 = X1/NMK(K0) ; Y1 = Y1/NMK(K0)
         !      XK(K0) = A*X0 + B*X1
         !      YK(K0) = A*Y0 + B*Y1
         !   ENDIF
         !ENDDO

!
! 1. Create mirrored points for all boundary points (including corner points)
!
         KC00 = 0 ! KC00 is used here to mark which boundary points have been mirrorred so far.
         do K0 = 1, numk ! Loop over K0: the current netnode
            if (kc00(K0) == 1) cycle ! was already mirrored

            if (nb(k0) == 2 .or. nb(k0) == 3) then ! Edge or corner point
               kprev = 0 ! Previous boundary point (used to detect and prevent overlap of two mirrored nodes)
               K = K0 ! Current boundary point
               do ! Determine mirrored point for current node ...
                  ! ... and start looping connected boundary points
                  if (K == 0) then
                     exit ! No further boundary nodes connected to this chain
                  end if
                  X0 = XK1(K); Y0 = YK1(K)
                  NR = 0 ! Nr. of boundary nodes connected to node K that were found already
                  kccell = 0 ! Marks whether each cell (nump) was already included in areatot for current node K
                  areatot = 0d0 ! Cumulative area of all internal 2D cells that contain netnode K
                  knext = 0
                  kdone = 0
                  do KK = 1, NMK(K) ! Consider netlink L between current node K and node K1L
                     L = NOD(K)%LIN(KK)
                     if (LNN(L) == 0) cycle
                     K1L = KK1(KK, K)
                     X1 = XK(K1L); Y1 = YK(K1L)
                     R01 = DBDISTANCE(X0, Y0, X1, Y1, jsferic, jasfer3D, dmiss)

                     ! Sum up all cell areas (also for corner nodes, may have multiple internal links connected)
                     if (lne(1, L) > 0) then
                        if (kccell(lne(1, L)) == 0) then
                           call getcellsurface(lne(1, L), area, xzwr, yzwr)
                           areatot = areatot + area
                           kccell(lne(1, L)) = 1
                        end if
                     end if
                     if (LNN(L) == 2 .and. lne(2, L) > 0) then ! INTERN
                        if (kccell(lne(2, L)) == 0) then
                           call getcellsurface(lne(2, L), area, xzwr, yzwr)
                           areatot = areatot + area
                           kccell(lne(2, L)) = 1
                        end if
                        x4 = x1 ! Remember an internal point (#4)
                        y4 = y1 ! (just an arbitrary one of the nmk-2 total)
                     else ! RAND
                        NR = NR + 1
                        ! Remember the two connected boundary points (#2 an #3)
                        if (NR == 1) then
                           X2 = X1; Y2 = Y1; K2 = K1L
                        else if (NR == 2) then
                           X3 = X1; Y3 = Y1; K3 = K1L
                        end if
                        WW(KK, K) = R01 ! AvD: TODO: onnodig?
                     end if
                  end do
                  if (nb(K) == 3 .and. NMK(K) == 2) then
                     ! Corner point with no internal links (just two edge links)
                     ! Determine area of the single corner cell.
                     call getcellsurface(lne(1, nod(K)%lin(1)), areatot, xzwr, yzwr)
                     x4 = xzwr ! Dummy internal point
                     y4 = yzwr
                  end if

                  if (nb(K) == 3) then
                     ! Fake point at one side of corner:
                     R01 = DBDISTANCE(X0, Y0, X2, Y2, jsferic, jasfer3D, dmiss)
                     rout = areatot / R01

                     if (jsferic == 1) then
                        rout = rout * RD2DG / ra
                     end if

                     call normaloutchk(x0, y0, x2, y2, x4, y4, xn, yn, JA, jsferic, jasfer3D, dmiss, dxymis)
                     xn = xn * rout
                     yn = yn * rout
                     xbd(1, K) = x0 + xn
                     ybd(1, K) = y0 + yn
                     call movabs(x0, y0)
                     call clnabs(xbd(1, K), ybd(1, K), 51)

                     ! Fake point at other side of corner: ! TODO : WAAROM DEZE MIDDELING TUSSEN 2 EN 3

                     R01 = DBDISTANCE(X0, Y0, X3, Y3, jsferic, jasfer3D, dmiss)
                     rout = areatot / R01

                     if (jsferic == 1) then
                        rout = rout * RD2DG / ra
                     end if

                     call normaloutchk(x0, y0, x3, y3, x4, y4, xn, yn, JA, jsferic, jasfer3D, dmiss, dxymis)
                     xn = xn * rout
                     yn = yn * rout
                     xbd(2, K) = x0 + xn
                     ybd(2, K) = y0 + yn
                     call movabs(x0, y0)
                     call clnabs(xbd(2, K), ybd(2, K), 31)

                     ! Note: xbd(1 and 2,..) are stored in same order as
                     ! the two boundary links in nod(..)%lin(:).

                  else
                     ! Compute outward edge length 'rout' as:
                     ! total cells' area / distance between two boundary points.
                     R23 = DBDISTANCE(X2, Y2, X3, Y3, jsferic, jasfer3D, dmiss)
                     rout = areatot / R23
                     if (jsferic == 1) then
                        rout = rout * RD2DG / ra
                     end if
                     call normaloutchk(x2, y2, x3, y3, x4, y4, xn, yn, JA, jsferic, jasfer3D, dmiss, dxymis) ! AvD: Not 100% safe: x4 is inside wrt x0, but maybe not wrt line 2-3 (seldomly)

                     xn = xn * rout
                     yn = yn * rout

                     ! The mirrored boundary point is now bd point + outward vector
                     xbd(1, K) = x0 + xn
                     ybd(1, K) = y0 + yn
                     call movabs(x0, y0)
                     call clnabs(xbd(1, K), ybd(1, K), 211)

                  end if

                  ! In the two connected boundary points 2 and 3:
                  ! * Pick one that is also edge (not corner) and not handled yet
                  !   and use it as knext (possibly empty/0)
                  ! * Pick one that was already handled and use it as kdone.
                  if (nb(k2) == 2) then
                     if (kc00(k2) == 0) then
                        knext = k2
                     else
                        kdone = k2
                     end if
                  end if
                  if (nb(k3) == 2) then
                     if (kc00(k3) == 0) then
                        knext = k3
                     else
                        kdone = k3
                     end if
                  end if
                  ! If there was no prev boundary point in this chain (inner do),
                  ! we just started a new one. Check if it connects to a chain
                  ! that was previously handled. (happens when starting with k0
                  ! somehere in the middle of a boundary, i.e. not in corner, and
                  ! at a later k0 continue the second half).
                  if (kprev == 0 .and. kdone > 0) then
                     kprev = kdone
                  end if

                  ! Check whether the produced mirror edge crosses with the
                  ! neighbouring mirror edge (only for two edge/non-corner points).
                  if (kprev > 0) then
                     if (nb(kprev) == 2 .and. nb(K) == 2) then
                        call CROSS(x0, y0, xbd(1, K), ybd(1, K), xk(kprev), yk(kprev), xbd(1, kprev), ybd(1, kprev), &
                                   JACROS, SL, SM, XCR, YCR, CRP, jsferic, dmiss)
                        if (jacros == 1) then
                           ! 'Pull back' the TWO mirrored points to the crossing point
                           call movabs(x0, y0)
                           call clnabs(xbd(1, K), ybd(1, K), 0)
                           xbd(1, K) = XCR
                           ybd(1, K) = YCR
                           call movabs(x0, y0)
                           call clnabs(xbd(1, K), ybd(1, K), 212)

                           call movabs(xk(kprev), yk(kprev))
                           call clnabs(xbd(1, kprev), ybd(1, kprev), 0)
                           xbd(1, kprev) = XCR
                           ybd(1, kprev) = YCR
                           call movabs(xk(kprev), yk(kprev))
                           call clnabs(xbd(1, kprev), ybd(1, kprev), 212)
                        end if
                     end if
                  end if

                  ! Current point is done, proceed to next connected bd point
                  KC00(K) = 1 ! Mark netnode K as done
                  kprev = k
                  K = knext ! One of the points #2/#3, or none when ready (0)
               end do ! loop across connected edge points
            end if
         end do ! numk

         call READYY('Orthogonise net', dble(NO - 1 + .35d0) / ITATP)

!
! 2. Compute attraction parameters for all nodes surrounding each node (incl. mirrored nodes from step 1)
!
         numka: do K0 = 1, NUMK ! ATTRACTION PARAMETERS
            X0 = XK1(K0); Y0 = YK1(K0)
            W0 = 0

            if (NB(K0) == 1) then ! INTERNAL
               do KK = 1, NMK(K0)
                  L = NOD(K0)%LIN(KK)
                  KL = LNE(1, L)
                  KR = LNE(2, L)
                  if (LNN(L) == 2) then
                     call GETCELLWEIGHTEDCENTER(KL, XL, YL, ZZZ)
                     call GETCELLWEIGHTEDCENTER(KR, XR, YR, ZZZ)

                     XZ(KL) = XL; YZ(KL) = YL
                     XZ(KR) = XR; YZ(KR) = YR
                     WW(KK, K0) = DBDISTANCE(XL, YL, XR, YR, jsferic, jasfer3D, dmiss)

                     K1L = KK1(KK, K0)
                     ! If any connected node is unmasked (i.e. outside of polygon), fix this point
                     if (KC(K1L) /= 1) then
                        NB(K0) = 0
                        cycle numka
                     end if
                     X1 = XK(K1L); Y1 = YK(K1L)
                     R01 = DBDISTANCE(X0, Y0, X1, Y1, jsferic, jasfer3D, dmiss)
                     R01 = ATPF1 + ATPF * R01

                     if (R01 /= 0) then
                        WW(KK, K0) = WW(KK, K0) / R01
                        if (JSFERIC == 1) then
                           WW(KK, K0) = WW(KK, K0) / cos(DG2RD * 0.5d0 * (Y0 + Y1))
                        end if
                        W0 = W0 + WW(KK, K0)
                     end if
                  end if
               end do
            else if (NB(K0) == 2) then ! EDGE NODES
               NR = 0
               do KK = 1, NMK(K0)
                  L = NOD(K0)%LIN(KK)
                  KL = LNE(1, L)
                  KR = LNE(2, L)

                  K1L = KK1(KK, K0)
                  X1 = XK(K1L); Y1 = YK(K1L)

                  ! If any connected node is unmasked (i.e. outside of polygon), fix this point
                  if (KC(K1L) /= 1) then
                     NB(K0) = 0
                     cycle numka
                  end if

                  if (LNN(L) == 1) then ! Neighbour nodes at boundary
                     call GETCELLWEIGHTEDCENTER(KL, XL, YL, ZZZ)
                     !call cirr(XL,YL,71)

                     K1L = kk1(KK, K0)
                     nn = 0 ! Nr of ghost point at node K1L (1 or 2)
                     if (nb(K1L) == 3) then
                        ! Determine which ghostpoint this is going to be, the first or second (=index in xbd)
                        do K = 1, NMK(K1L)
                           LL = nod(K1L)%lin(K)
                           if (lnn(LL) == 1) then
                              nn = nn + 1
                           else
                              cycle ! No boundary link, try next
                           end if

                           call othernode(K1L, LL, K1)
                           if (K1 == K0) then
                              exit ! This is the connecting link, NN now has the correct value for use in xbd.
                           end if
                        end do
                     else
                        nn = 1
                     end if
                     ! Corrupted networks with overlapping links may contain points with >2 links with lnn=1
                     nn = min(nn, 2)
                     xv(1) = XK(K0); yv(1) = YK(K0)
                     xv(2) = XK(K1L); yv(2) = YK(K1L)
                     xv(3) = xbd(1, K0); yv(3) = ybd(1, K0)
                     xv(4) = xbd(NN, K1L); yv(4) = ybd(NN, K1L)
                     lnnl(1:4) = 2
                     call GETCIRCUMCENTER(4, xv, yv, lnnl, XR, YR, jsferic, jasfer3D, jglobe, jins, dmiss, dxymis, dcenterinside)
                     !XR  = .25d0*(XK(K0) + XK(K1L) + xbd(K0) + xbd(K1L))
                     !YR  = .25d0*(YK(K0) + YK(K1L) + ybd(K0) + ybd(K1L))
                     !call cirr(XR,YR,41)

                     NR = NR + 1
                     if (NR == 1) then ! Store first bd point (#2)
                        K2 = K1L
                        X2 = XR; Y2 = YR
                     else if (NR == 2) then ! Store second bd point (#3)
                        K3 = K1L
                        X3 = XR; Y3 = YR
                     end if
                  elseif (LNN(L) == 2) then ! Internal neighbouring nodes
                     call GETCELLWEIGHTEDCENTER(KL, XL, YL, ZZZ)
                     call GETCELLWEIGHTEDCENTER(KR, XR, YR, ZZZ)

                     XZ(KL) = XL; YZ(KL) = YL
                     XZ(KR) = XR; YZ(KR) = YR
                  end if
                  WW(KK, K0) = DBDISTANCE(XL, YL, XR, YR, jsferic, jasfer3D, dmiss)
                  R01 = DBDISTANCE(X0, Y0, X1, Y1, jsferic, jasfer3D, dmiss)
                  R01 = ATPF1 + ATPF * R01
                  if (R01 /= 0) then
                     WW(KK, K0) = WW(KK, K0) / R01
                     W0 = W0 + WW(KK, K0)
                  end if
               end do !KK = 1,NMK(K0)

               KK = nmk(K0) + 1
               XL = X2; YL = Y2; XR = X3; YR = Y3

               WW(KK, K0) = DBDISTANCE(XL, YL, XR, YR, jsferic, jasfer3D, dmiss)

               X1 = xbd(1, K0); Y1 = ybd(1, K0)
               R01 = DBDISTANCE(X0, Y0, X1, Y1, jsferic, jasfer3D, dmiss)
               R01 = ATPF1 + ATPF * R01

               if (R01 /= 0) then
                  WW(KK, K0) = WW(KK, K0) / R01
                  W0 = W0 + WW(KK, K0)
               end if
            end if

            if (W0 /= 0) then ! NORMALISING
               do KK = 1, NMK(K0)
                  WW(KK, K0) = WW(KK, K0) / W0
               end do
               if (NB(K0) == 2) then
                  KK = NMK(K0) + 1
                  WW(KK, K0) = WW(KK, K0) / W0
               end if
            end if

         end do numka

         call READYY('Orthogonise net', dble(NO - 1 + .8d0) / ITATP)

!
! 3. Solve the 'Laplacian' for orthogonalization/Move all points in a few iteration steps.
!

!     call toemaar()

         relaxin = 0.5d0
         relax1 = 1d0 - relaxin
         do I = 1, ITBND
            do N = 1, ITIN
               ndki: do K = 1, NUMK
                  if (NB(K) == 1) then ! Only internal points
                     X0 = 0d0; Y0 = 0d0
                     do KK = 1, NMK(K)
                        if (WW(KK, K) /= 0) then
                           X0 = X0 + WW(KK, K) * XK(KK1(KK, K))
                           Y0 = Y0 + WW(KK, K) * YK(KK1(KK, K))
                        end if
                     end do
                     XK1(K) = relaxin * X0 + relax1 * xk(k) !hk: trying to remove wiggles in high aspect ratio cells
                     YK1(K) = relaxin * y0 + relax1 * yk(k)
                  end if
               end do ndki
               XK(1:NUMK) = XK1(1:NUMK); YK(1:NUMK) = YK1(1:NUMK)
            end do ! ITIN

            ndkb: do K = 1, NUMK
               if (NB(K) == 2) then ! Only edge points (not corner)
                  X0 = 0d0; Y0 = 0d0 ! was 0
                  NR = 0
                  do KK = 1, NMK(K)
                     ! AvD: TEMP: do not move points connected to a corner.
                     if (NB(KK1(KK, K)) == 3) then
                        XK1(K) = XK(K); YK1(K) = YK(K)
                        cycle ndkb
                     end if

                     if (WW(KK, K) /= 0) then
                        X0 = X0 + WW(KK, K) * XK(KK1(KK, K))
                        Y0 = Y0 + WW(KK, K) * YK(KK1(KK, K))
                     end if

                     if (LNN(NOD(K)%LIN(KK)) == 1) then
                        ! Remember the two boundary neighbours in ORIGINAL net.
                        NR = NR + 1
                        if (NR == 1) then
                           X2 = XK0(KK1(KK, K)); Y2 = YK0(KK1(KK, K))
                        else if (NR == 2) then
                           X3 = XK0(KK1(KK, K)); Y3 = YK0(KK1(KK, K))
                        end if
                     end if
                  end do

                  ! For edge nodes, include attraction by mirrored node xbd too.
                  KK = NMK(K) + 1
                  if (WW(KK, K) /= 0) then
                     X0 = X0 + WW(KK, K) * xbd(1, K)
                     Y0 = Y0 + WW(KK, K) * ybd(1, K)
                  end if

                  ! Project the moved boundary point back onto the closest
                  ! ORIGINAL edge (netlink) (either between 0 and 2 or 0 and 3)
                  call DLINEDIS(X0, Y0, XK0(K), YK0(K), X2, Y2, JA2, DIS2, X2, Y2, jsferic, jasfer3D, dmiss)
                  call DLINEDIS(X0, Y0, XK0(K), YK0(K), X3, Y3, JA3, DIS3, X3, Y3, jsferic, jasfer3D, dmiss)
                  if (DIS2 < DIS3) then
                     X0 = X2; Y0 = Y2
                  else
                     X0 = X3; Y0 = Y3
                  end if

                  ! Smoothing (necessary?)
                  XK1(K) = X0; YK1(K) = Y0

                  XK1(K) = X0; YK1(K) = Y0
               end if
            end do ndkb
            XK(1:NUMK) = XK1(1:NUMK); YK(1:NUMK) = YK1(1:NUMK)
         end do !ITBND

         call READYY('Orthogonise net', dble(NO) / ITATP)

      end do !ITATP

      call READYY('Orthogonise net', -1d0)

!   CALL REMOVESMALLLINKS()

      !IF (JSFERICOLD == 1) THEN
      !    CALL MAKEY1D(XK,YK,NUMK)
      !    CALL MAKEY1D(XK0,YK0,NUMK)
      !    JSFERIC = JSFERICOLD
      !ENDIF

      call update_cell_circumcenters()
      call cosphiunetcheck(0)
  !!!
      deallocate (xv, yv, lnnl)
      deallocate (xbd, ybd)
      deallocate (kccell, KC00)
      deallocate (WW, KK1)
      !, NB ) ! AvD: TODO: this is for showing node codes (during ortho), but also introduces memleak.

   end subroutine ORTHOGONISENET_old
