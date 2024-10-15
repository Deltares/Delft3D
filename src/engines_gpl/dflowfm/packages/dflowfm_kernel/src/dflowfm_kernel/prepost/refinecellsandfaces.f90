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

  subroutine REFINECELLSANDFACES()
     use m_netw
     use m_samples
     use m_flowtimes, only: dt_max
     use m_physcoef, only: ag
     use m_missing
     use m_ec_interpolationsettings
     use m_sferic, only: jsferic, jasfer3D, dtol_pole
     use m_ec_basic_interpolation, only: TerrorInfo
     use fm_external_forcings_data, only: transformcoef
     use m_qnerror
     use m_set_nod_adm
     use m_dlinedis2
     use m_new_link

     implicit none

     integer :: IERR, JA, K1, K2, K, KP, L, L1, L2, LNU, N, NN, NR, KA, KB, JADOEN, KK, JA2
     integer :: JACOURANTNETWORK, JDLA, N1, N2, N6
     integer :: ic1, ic2, numL_old, kkm1, kkp1, kkm2, kkp2, Lm2, Lp2, numtris, num, iter, MAXITER
     double precision :: XL, YL, ZL, CELLSIZE, COURANT, C, DIS, XN, YN, RS
     integer, allocatable :: KPL(:, :), KP2(:), NA(:)
     double precision, allocatable :: XC(:), YC(:), ZC(:), AR(:)

     double precision, allocatable :: XX(:, :), YY(:, :)
     integer, allocatable :: NNN(:)
     type(TerrorInfo) :: errorInfo

     call SAVENET()

     JACOURANTNETWORK = 1
     ZL = ZKUNI
     JDLA = 1

10   JADOEN = 0

     call FINDCELLS(0)

     numL_old = numL

     if (NS < 3) then
        JACOURANTNETWORK = 0
     end if

     allocate (XC(NUMP), STAT=IERR)
     call AERR('XC(NUMP)', IERR, NUMP)
     allocate (YC(NUMP), STAT=IERR)
     call AERR('YC(NUMP)', IERR, NUMP)
     allocate (AR(NUMP), STAT=IERR)
     call AERR('AR(NUMP)', IERR, NUMP)
     AR = DMISS

     do N = 1, NUMP
        call getcellsurface(N, AR(N), XC(N), YC(N))
     end do

     allocate (ZC(NUMP), STAT=IERR)
     call AERR('ZC(NUMP)', IERR, NUMP)
     ZC = DMISS

! First interpolate bottom level in netcell-based zc, then use zc as cellmask
     if (JACOURANTNETWORK == 1) then
        allocate (NA(NUMP), STAT=IERR)
        call AERR('NA(NUMP)', IERR, NUMP)
        NA = 0

        if (interpolationtype == INTP_INTP) then
           if (MXSAM > 0 .and. MYSAM > 0) then
!          bilinear interpolation of structured sample data
              call bilin_interp(Numk, xc, yc, zc, dmiss, XS, YS, ZS, MXSAM, MYSAM, jsferic)
           else
              call triinterp2(XC, YC, ZC, NUMP, JDLA, &
                              XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)
           end if
        else if (interpolationtype == INTP_AVG) then
           n6 = 6
           allocate (XX(N6, NUMP), YY(N6, NUMP), NNN(NUMP), STAT=IERR)
           call AERR('XX(N6,NUMP), YY(N6,NUMP), NNN(NUMP)', IERR, (1 + 2 * N6) * NUMP)
           do N = 1, NUMP
              NNN(N) = NETCELL(N)%N
              do NN = 1, NNN(N)
                 XX(NN, N) = XK(NETCELL(N)%NOD(NN))
                 YY(NN, N) = YK(NETCELL(N)%NOD(NN))
              end do
           end do
           call averaging2(1, NS, XS, YS, ZS, IPSAM, XC, YC, ZC, NUMP, XX, YY, N6, NNN, 0, &
                           dmiss, jsferic, jasfer3D, JINS, NPL, xpl, ypl, zpl, errorInfo)
           deallocate (XX, YY, NNN)
        end if

        do N = 1, NUMP

!        IF (ZC(N) .NE. DMISS .AND. NETCELL(N)%N == 4) THEN
           if (ZC(N) /= DMISS .and. (NETCELL(N)%N == 3 .or. NETCELL(N)%N == 4)) then
              CELLSIZE = sqrt(AR(N))
              C = sqrt(AG * abs(ZC(N)))
              COURANT = C * DT_MAX / CELLSIZE
              if (COURANT < 1d0 .and. CELLSIZE > 2 * SMALLESTSIZEINCOURANT) then
                 ZC(N) = COURANT
                 JADOEN = 1
              else
                 ZC(N) = DMISS
              end if
           else
              ZC(N) = DMISS
           end if
        end do

        do K = 1, NUMITCOURANT
           do L = 1, NUML
              if (LNN(L) == 2) then
                 N1 = LNE(1, L); N2 = LNE(2, L)
                 if (ZC(N1) /= DMISS) then !  .AND. ZC(N2) ==   DMISS) THEN
                    NA(N2) = NA(N2) + 1
                 end if
                 if (ZC(N2) /= DMISS) then !  .AND. ZC(N2) .NE. DMISS) THEN
                    NA(N1) = NA(N1) + 1
                 end if
              end if
           end do

           do N = 1, NUMP
              if (NA(N) > 0) then ! 1) THEN
                 ZC(N) = 0.5 ! ANY VALUE < 1 TO FLAG CELL MUST BE SPLIT
              end if
           end do
        end do
     else ! SPvdP: make cellmask based on nodemask
        do n = 1, nump
           zc(n) = dmiss
           call ALLIN(N, JA)
           if (ja == 1) zc(n) = 0.5d0
        end do
     end if

     allocate (KPL(3, NUML), STAT=IERR); KPL = 0 ! PER LINK REF NAAR LINKER EN RECHTER CENTRAAL NIEUW CELPUNT, 3rd: original endpoint
     call AERR('KPL(3,NUML)', IERR, 2 * NUML)
     allocate (KP2(NUML), STAT=IERR); KP2 = 0 ! PER LINK REF NAAR BIJGEPLAATST MIDDEN LINK PUNT
     call AERR('KP2(  NUML)', IERR, NUML)

     do N = 1, NUMP
!     CALL ALLIN(N,JA)
!     IF (JA == 0) CYCLE

!     IF (JACOURANTNETWORK == 1) THEN
        if (ZC(N) == DMISS) cycle
!     ENDIF

        NN = netcell(N)%N

        if (NN /= 3) then ! SPvdP: create new center node for non-triangles only
           call dSETNEWPOINT(XC(N), YC(N), KP) ! HET CENTRALE PUNT IN CEL
        else
           kp = netcell(N)%nod(1) ! use first point
        end if

        do K = 1, NN
           K1 = netcell(N)%NOD(K)
           NR = NMK(K1)
           L1 = netcell(N)%LIN(K)
           ka = kn(1, L1); kb = kn(2, l1)
           JA2 = 0
           if (NN == 3 .or. NN == 4) then ! FOR ALL LINKS OF TRIANGLES AND QUADS
              JA2 = 1
           else
              call DLINEDIS2(XC(N), YC(N), XK(KA), YK(KA), XK(KB), YK(KB), JA, DIS, XN, YN, RS)
              if (RS > 0.3d0 .and. RS < 0.7d0) then ! ZIT HET CENTRALE PUNT 'MIDDEN' TUSSEN EEN CELRAND DAN VERFIJN DIE RAND
                 JA2 = 1
              end if
           end if
           if (JA2 == 1) then
!           IF (KPL(1,L1) == 0) THEN
!               KPL(1,L1) = KP !; NCL(1,L1) = K
!           ELSE
!               KPL(2,L1) = KP !; NCL(2,L1) = K
!           ENDIF
              if (lne(1, L1) == n) then
                 KPL(1, L1) = KP !; NCL(1,L1) = K
              else
                 KPL(2, L1) = KP !; NCL(2,L1) = K
              end if

           end if
        end do

     end do
     do L = 1, NUML
        if (KPL(1, L) /= 0 .or. KPL(2, L) /= 0) then
           K1 = KN(1, L); K2 = KN(2, L)
           XL = 0.5d0 * (XK(K1) + XK(K2))
           YL = 0.5d0 * (YK(K1) + YK(K2))
           call dSETNEWPOINT(XL, YL, KP); KP2(L) = KP ! PUNT OP LINK ZELF
           KPL(3, L) = KN(2, L)
           KN(2, L) = KP
           call NEWLINK(KP, K2, LNU)
           if (KPL(1, L) /= 0) then
              if (netcell(lne(1, L))%N /= 3) then ! SPvdP: no center point for triangles
                 call NEWLINK(KP, KPL(1, L), LNU)
              end if
           end if
           if (KPL(2, L) /= 0) then
              if (netcell(lne(2, L))%N /= 3) then ! SPvdP: no center point for triangles
                 call NEWLINK(KP, KPL(2, L), LNU)
              end if
           end if
        end if
     end do
! SPvdP: check for triangles with two sides refined and refine the third
     MAXITER = 1
     do iter = 1, MAXITER
        numtris = 0
        do n = 1, nump
           NN = netcell(n)%N
           if (NN /= 3) cycle ! triangles only
           num = 0
           do kk = 1, NN
              L = netcell(n)%lin(kk)
              if (kp2(L) /= 0) then
                 num = num + 1
                 if (num == 1) L1 = L
              end if
           end do
           if (num /= 1) cycle ! one unrefined side only
!       refine the single unrefined side
           if (KPL(1, L1) == 0) KPL(1, L1) = kn(1, L1)
           if (KPL(2, L1) == 0) KPL(2, L1) = kn(1, L1)
           K1 = KN(1, L1); K2 = KN(2, L1)
           XL = 0.5d0 * (XK(K1) + XK(K2))
           YL = 0.5d0 * (YK(K1) + YK(K2))
           call dSETNEWPOINT(XL, YL, KP); KP2(L1) = KP ! PUNT OP LINK ZELF
           KPL(3, L1) = KN(2, L1)
           KN(2, L1) = KP
           call NEWLINK(KP, K2, LNU)
        end do

        if (numtris == 0) exit
     end do
     if (numtris /= 0) then
        call qnerror('refinecellsandfaces: numtris.ne.0', ' ', ' ')
     end if

! SPvdP: make links inside triangles
     do n = 1, nump
        NN = netcell(n)%N
        if (NN /= 3) cycle
        do kk = 1, 3
           k1 = kp2(netcell(n)%lin(kk))
           k2 = kk + 1; if (k2 > NN) k2 = k2 - NN
           k2 = kp2(netcell(n)%lin(k2))
           if (k1 > 0 .and. k2 > 0 .and. k1 /= k2) then
              call newlink(k1, k2, Lnu)
           end if
        end do
     end do
! SPvdP: connect new nodes with inactive part of the net
     do L = 1, numL_old
!     check and see if this link neighbors a refined and an unrefined cell
        if (lnn(L) < 2) cycle
        ic1 = lne(1, L)
        ic2 = lne(2, L)
        if ((zc(ic1) == DMISS .and. zc(ic2) == DMISS) .or. &
            (zc(ic1) /= DMISS .and. zc(ic2) /= DMISS)) cycle

!     find the refined neighboring cell
        if (zc(ic1) == DMISS) then
           n = ic1
        else
           n = ic2
        end if
        NN = netcell(n)%N

!     find link in the netcell administration
        do kk = 1, NN
           if (netcell(n)%lin(kk) == L) exit
        end do
        if (netcell(n)%lin(kk) /= L) cycle ! should never happen

        kp = kp2(L)
        if (kp /= 0) then
           kkm1 = kk - 1; if (kkm1 < 1) kkm1 = kkm1 + NN
           L1 = netcell(n)%lin(kkm1) ! left-connected link
           kkp1 = kk + 1; if (kkp1 > NN) kkp1 = kkp1 - NN
           L2 = netcell(n)%lin(kkp1) ! right-connected link

!          nearest node on the left-connected link
           kA = kp2(L1)
           if (kA < 1) then
!             check if the next link has a refinement
              kkm2 = kkm1 - 1; if (kkm2 < 1) kkm2 = kkm2 + NN
              Lm2 = netcell(n)%lin(kkm2)

              if (kp2(Lm2) == 0) then ! next link has no refinement
!                note: original nodes of link L are: kn(1,L) and kpL(3,L)
                 if (kn(1, L1) == kn(1, L) .or. kn(1, L1) == kpL(3, L)) then
                    kA = kn(2, L1)
                 else
                    kA = kn(1, L1)
                 end if
              else
                 kA = kp2(Lm2)
              end if
           end if

!           nearest node on the right-connected link
           kB = kp2(L2)
           if (kB < 1) then
!             check if the next link has a refinement
              kkp2 = kkp1 + 1; if (kkp2 > NN) kkp2 = kkp2 - NN
              Lp2 = netcell(n)%lin(kkp2)

              if (kp2(Lp2) == 0) then ! next link has no refinement
!                note: original nodes of link L are: kn(1,L) and kpL(3,L)
                 if (kn(1, L2) == kn(1, L) .or. kn(1, L2) == kpL(3, L)) then
                    kB = kn(2, L2)
                 else
                    kB = kn(1, L2)
                 end if
              else
                 kB = kp2(Lp2)
              end if
           end if

           call newlink(kA, kp, Lnu)
           call newlink(kp, kB, Lnu)
        end if
     end do

     deallocate (XC, YC, AR, KPL, KP2)
     if (allocated(ZC)) then
        deallocate (ZC)
     end if
     if (allocated(NA)) deallocate (NA)

     netstat = NETSTAT_CELLS_DIRTY
     call SETNODADM(0)

     if (JACOURANTNETWORK == 1 .and. JADOEN == 1 .and. NUMK < 1e6) then ! NOT BEYOND 4*1 MILLION GRIDPOINTS
        goto 10
     end if

  end subroutine REFINECELLSANDFACES
