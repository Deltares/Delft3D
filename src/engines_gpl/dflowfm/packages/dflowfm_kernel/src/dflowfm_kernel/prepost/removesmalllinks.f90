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

   subroutine REMOVESMALLLINKS() ! 1 REMOVES IF FLOW LINK DISTANCES ARE SMALL RELATIVE TO CONNECTED CELL SIZES

      use m_netw ! 2 REMOVES SMALL TRIANGLES NEXT TO
      use M_FLOWGEOM
      use unstruc_messages
      use geometry_module, only: dbdistance, dcosphi, dlinedis
      use m_missing, only: dmiss, dxymis
      use m_sferic, only: jsferic, jasfer3D, dtol_pole
      use gridoperations
      use m_mergenodes
      use m_set_nod_adm
      implicit none

      double precision :: R01, R02, AN1, AN2, XL, YL, XR, YR, XZWr, YZWr, ZZZ
      integer :: KL1, KL2, KN1a, KN2a, L, jaremove

      double precision :: AREA, TAREA, COSMIN, COSPHI, FRAC, DIS, XN, YN
      integer :: NAAST, N, NN, NUMT, LL, K0, K1, K2, LU, LD, KA, KB, KH, K, JA, IERR, NW
      integer :: LLA, LLB, LLC, L0, L1, L2, LT, LI, KK, NL, NR

      double precision, allocatable :: XNW(:), YNW(:)
      integer, allocatable :: NNW(:, :)

      call SAVENET()

      call SETNODADM(0) !

      call REMOVECOINCIDINGTRIANGLES() !

      call FINDCELLS(0)

!   take dry cells into account (after findcells)
      call delete_dry_points_and_areas()

      JAREMOVE = 0
      do L = 1, NUML ! REMOVE SMALL CIRCUMCENTRE DISTANCES
         if (LC(L) == 1) then
            KL1 = KN(1, L); KL2 = KN(2, L)
            if (KL1 > 0 .and. KL2 > 0) then
               KN1a = LNE(1, L); KN2a = LNE(2, L)
               if (KN1a > 0 .and. KN2a > 0) then
                  NL = netcell(KN1a)%N; NR = netcell(KN2a)%N

!!              SPvdP: only remove small flow links, if adjacent cells are triangles
!               IF ( (NL == 3 .or. NL == 4) .and. (NR == 3 .or. NR == 4) ) THEN
                  if ((NL == 3 .and. NR == 3 .and. maxfaceallow == 4) .or. &
                      (NL == 3 .or. NL == 4) .and. (NR == 3 .or. NR == 4) .and. maxfaceallow == 5) then
                     call GETCELLSURFACE(KN1a, AN1, XZWr, YZWr)
                     call GETCELLSURFACE(KN2a, AN2, XZWr, YZWr)
                     call GETCELLWEIGHTEDCENTER(KN1a, XL, YL, ZZZ)
                     call GETCELLWEIGHTEDCENTER(KN2a, XR, YR, ZZZ)
                     R01 = 0.5d0 * (sqrt(AN1) + sqrt(AN2)) ! TYPICAL SIDE
                     R02 = DBDISTANCE(XL, YL, XR, YR, jsferic, jasfer3D, dmiss) ! CIRCUMDISTANCE
                     if (R02 < removesmalllinkstrsh * R01) then
                        KN(1, L) = 0; KN(2, L) = 0; KN(3, L) = -1 ! CALL DELLINK(L)
                        JAREMOVE = 1
                     end if
                  end if
               end if
            end if
         end if
      end do

      if (JAREMOVE == 1) then
         call FINDCELLS(0)
      end if

      allocate (XNW(NUMK), YNW(NUMK), NNW(3, NUMK), STAT=IERR)
      call AERR('XNW(NUMK),YNW(NUMK),NNW(3,NUMK)', IERR, NUMK * 3)

      JAREMOVE = 0; NW = 0
      do LT = 1, NUML

         N = LNE(1, LT); NN = 0
         if (N > 0) NN = NETCELL(N)%N

         if (LNN(LT) == 1 .and. NN == 3) then ! SMALL BOUNDARY TRIANGLES

            TAREA = 0d0; NUMT = 0
            do LL = 1, 3 ! ESTABLISH TYPICAL CELLSIZE ADJACENT QUADS
               L = NETCELL(N)%LIN(LL)
               if (LNN(L) == 2) then
                  KN1a = LNE(1, L); KN2a = LNE(2, L)
                  if (KN1A == N) then
                     NAAST = KN2A
                  else
                     NAAST = KN1A
                  end if
                  if (NETCELL(NAAST)%N > 3) then ! ADJACENT QUADS ETC
                     call GETCELLSURFACE(NAAST, AREA, XZWr, YZWr)
                     TAREA = TAREA + AREA; NUMT = NUMT + 1
                  end if
               end if
            end do
            if (NUMT /= 0) then
               TAREA = TAREA / NUMT
            end if
            if (TAREA /= 0) then
               call GETCELLSURFACE(N, AREA, XZWr, YZWr)
               if (AREA > 0d0) then
                  FRAC = AREA / TAREA
                  if (FRAC < TRIAREAREMFRAC) then
                     COSMIN = 1d0
                     do LL = 1, 3 ! FIND KHOEK
                        LU = LL + 1; if (LU == 4) LU = 1
                        LD = LL - 1; if (LD == 0) LD = 3
                        K0 = NETCELL(N)%NOD(LD); L0 = NETCELL(N)%LIN(LD)
                        K1 = NETCELL(N)%NOD(LL); L1 = NETCELL(N)%LIN(LL)
                        K2 = NETCELL(N)%NOD(LU); L2 = NETCELL(N)%LIN(LU)
                        COSPHI = abs(DCOSPHI(XK(K0), YK(K0), XK(K1), YK(K1), XK(K1), YK(K1), XK(K2), YK(K2), jsferic, jasfer3D, dxymis))
                        if (COSPHI < COSMIN) then
                           COSMIN = COSPHI
                           KA = K0; KH = K1; KB = K2; LLA = L0; LLB = L1; LLC = L2
                        end if
                     end do

                     if (COSMIN < 0.2 .and. LNN(LLC) == 1) then
                        call dLINEDIS(XK(KH), YK(KH), XK(KA), YK(KA), XK(KB), YK(KB), JA, DIS, XN, YN, jsferic, jasfer3D, dmiss)

                        NW = NW + 1
                        XNW(NW) = XN
                        YNW(NW) = YN
                        NNW(1, NW) = KH
                        NNW(2, NW) = KA
                        NNW(3, NW) = KB
                        JAREMOVE = 1; cycle
                     end if

                  end if
               end if
            end if
         end if
      end do

      if (JAREMOVE == 1) then

         do K = 1, NW
            KH = NNW(1, K)
            KA = NNW(2, K)
            KB = NNW(3, K)
            XK(KH) = XNW(K)
            YK(KH) = YNW(K)
            LI = 0
            do KK = 1, NMK(KA) ! NOG EVEN CHECKEN OF DE LINK TUSSEN KA EN KH NIET MEER DAN ENKELVOUDIG INTERN VERBONDEN IS
               L = NOD(KA)%LIN(KK)
               if (LNN(L) == 2) then
                  LI = LI + 1
               end if
            end do
            if (LI == 1) then
               call MERGENODES(KA, KH, JA)
            end if
            LI = 0
            do KK = 1, NMK(KB)
               L = NOD(KB)%LIN(KK)
               if (LNN(L) == 2) then
                  LI = LI + 1
               end if
            end do
            if (LI == 1) then
               call MERGENODES(KB, KH, JA)
            end if
         end do

         call SETNODADM(0)

!     netcell administration out of date
         netstat = NETSTAT_CELLS_DIRTY

      end if

      deallocate (XNW, YNW, NNW)

   end subroutine REMOVESMALLLINKS
