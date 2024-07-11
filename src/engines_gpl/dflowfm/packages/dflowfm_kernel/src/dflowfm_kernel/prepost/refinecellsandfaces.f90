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

  SUBROUTINE REFINECELLSANDFACES()
  use m_netw
  USE m_polygon
  USE M_SAMPLES
  USE M_FLOWTIMES
  USE m_physcoef
  USE m_missing
  USE m_ec_interpolationsettings
  use m_sferic, only: jsferic, jasfer3D, dtol_pole
  use m_ec_basic_interpolation, only: triinterp2, bilin_interp, averaging2, TerrorInfo
  use fm_external_forcings_data, only: transformcoef
  use gridoperations

  implicit none

  INTEGER                       :: IERR, JA, KM, K1, K2, K, KP, L, L1, L2, LNU, N, NN, NR, KA, KB, JADOEN, N3, KK, JA2
  INTEGER                       :: JACOURANTNETWORK, JDLA, N1, N2, K2A, K2B, KKP, KKN, N6
  integer                       :: ic1, ic2, numL_old, kkm1, kkp1, kkm2, kkp2, Lm2, Lp2, numtris, num, iter, MAXITER
  DOUBLE PRECISION              :: XL, YL, ZL, CELLSIZE, COURANT, CELLAREA, C, DIS, XN,YN, RS
  INTEGER, ALLOCATABLE          :: KPL(:,:), KP2(:), NA(:)
  DOUBLE PRECISION, ALLOCATABLE :: XC(:), YC(:), ZC(:), AR(:)

  DOUBLE PRECISION, ALLOCATABLE :: XX(:,:), YY(:,:)
  INTEGER         , ALLOCATABLE :: NNN (:)
  type(TerrorInfo)              :: errorInfo

  CALL SAVENET()

  JACOURANTNETWORK = 1
  ZL               = ZKUNI
  JDLA             = 1

10  JADOEN = 0

  CALL FINDCELLS(0)

  numL_old = numL

  IF (NS < 3 ) THEN
      JACOURANTNETWORK = 0
  ENDIF

  ALLOCATE ( XC(NUMP) , STAT = IERR)
  CALL AERR('XC(NUMP)', IERR, NUMP )
  ALLOCATE ( YC(NUMP) , STAT = IERR)
  CALL AERR('YC(NUMP)', IERR, NUMP )
  ALLOCATE ( AR(NUMP) , STAT = IERR)
  CALL AERR('AR(NUMP)', IERR, NUMP )
  AR = DMISS

  DO N  = 1,NUMP
     CALL getcellsurface ( N, AR(N), XC(N), YC(N) )
  ENDDO

  ALLOCATE( ZC(NUMP) , STAT = IERR)
  CALL AERR('ZC(NUMP)', IERR, NUMP )
  ZC = DMISS

! First interpolate bottom level in netcell-based zc, then use zc as cellmask
  IF (JACOURANTNETWORK == 1) THEN
     ALLOCATE ( NA(NUMP) , STAT = IERR)
     CALL AERR('NA(NUMP)', IERR, NUMP )
     NA = 0

     if (interpolationtype == INTP_INTP) then
        if ( MXSAM.gt.0 .and. MYSAM.gt.0 ) then
!          bilinear interpolation of structured sample data
           call bilin_interp(Numk, xc, yc, zc, dmiss, XS, YS, ZS, MXSAM, MYSAM, jsferic)
        else
           CALL triinterp2(XC,YC,ZC,NUMP,JDLA, &
                           XS, YS, ZS, NS, dmiss, jsferic, jins, jasfer3D, NPL, MXSAM, MYSAM, XPL, YPL, ZPL, transformcoef)
        end if
     else if (interpolationtype == INTP_AVG) then
        n6 = 6
        ALLOCATE( XX(N6,NUMP), YY(N6,NUMP), NNN(NUMP), STAT = IERR )
        CALL AERR('XX(N6,NUMP), YY(N6,NUMP), NNN(NUMP)', IERR, (1+2*N6)*NUMP)
        DO N = 1,NUMP
           NNN(N) = NETCELL(N)%N
           DO NN = 1, NNN(N)
              XX(NN,N) = XK(NETCELL(N)%NOD(NN))
              YY(NN,N) = YK(NETCELL(N)%NOD(NN))
           ENDDO
        ENDDO
        call averaging2(1,NS,XS,YS,ZS,IPSAM,XC,YC,ZC,NUMP,XX,YY,N6,NNN,0,&
                        dmiss, jsferic, jasfer3D, JINS, NPL, xpl, ypl, zpl, errorInfo)
        DEALLOCATE(XX,YY,NNN)
     endif

     DO N = 1,NUMP

!        IF (ZC(N) .NE. DMISS .AND. NETCELL(N)%N == 4) THEN
       IF (ZC(N) .NE. DMISS .AND. ( NETCELL(N)%N == 3 .or. NETCELL(N)%N == 4)) THEN
           CELLSIZE  = SQRT( AR(N) )
           C         = SQRT(AG*ABS(ZC(N)) )
           COURANT   = C*DT_MAX/CELLSIZE
           IF (COURANT < 1D0 .AND. CELLSIZE > 2*SMALLESTSIZEINCOURANT) THEN
              ZC(N)  = COURANT
              JADOEN = 1
           ELSE
              ZC(N)  = DMISS
           ENDIF
        ELSE
           ZC(N) = DMISS
        ENDIF
     ENDDO

     DO K = 1,NUMITCOURANT
        DO L = 1,NUML
           IF (LNN(L) == 2) THEN
              N1 = LNE(1,L) ; N2 = LNE(2,L)
              IF (ZC(N1) .NE. DMISS) THEN   !  .AND. ZC(N2) ==   DMISS) THEN
                 NA(N2)  = NA(N2) + 1
              ENDIF
              IF (ZC(N2) .NE. DMISS) THEN   !  .AND. ZC(N2) .NE. DMISS) THEN
                 NA(N1)  = NA(N1) + 1
              ENDIF
           ENDIF
        ENDDO

        DO N = 1,NUMP
           IF (NA(N) > 0) THEN   ! 1) THEN
              ZC(N) = 0.5 ! ANY VALUE < 1 TO FLAG CELL MUST BE SPLIT
           ENDIF
        ENDDO
     ENDDO
  else   ! SPvdP: make cellmask based on nodemask
     do n=1,nump
        zc(n) = dmiss
        CALL ALLIN(N,JA)
        if ( ja.eq.1 ) zc(n) = 0.5d0
     end do
  ENDIF


  ALLOCATE ( KPL(3,NUML) , STAT=IERR  ) ; KPL = 0  ! PER LINK REF NAAR LINKER EN RECHTER CENTRAAL NIEUW CELPUNT, 3rd: original endpoint
  CALL AERR('KPL(3,NUML)', IERR, 2*NUML )
  ALLOCATE ( KP2(  NUML) , STAT=IERR  ) ; KP2 = 0  ! PER LINK REF NAAR BIJGEPLAATST MIDDEN LINK PUNT
  CALL AERR('KP2(  NUML)', IERR, NUML )

  DO N  = 1,NUMP
!     CALL ALLIN(N,JA)
!     IF (JA == 0) CYCLE

!     IF (JACOURANTNETWORK == 1) THEN
        IF (ZC(N) == DMISS) CYCLE
!     ENDIF

     NN = netcell(N)%N

     if ( NN.ne.3 ) then   ! SPvdP: create new center node for non-triangles only
        CALL dSETNEWPOINT(XC(N), YC(N), KP)           ! HET CENTRALE PUNT IN CEL
     else
        kp = netcell(N)%nod(1)   ! use first point
     end if


     DO K   = 1,NN
        K1  = netcell(N)%NOD(K)
        NR  = NMK(K1)
        L1  = netcell(N)%LIN(K)
        ka  = kn(1,L1) ; kb = kn(2,l1)
        JA2 = 0
        IF (NN == 3 .OR. NN == 4) THEN ! FOR ALL LINKS OF TRIANGLES AND QUADS
           JA2  = 1
        ELSE
           call DLINEDIS2(XC(N),YC(N), XK(KA),YK(KA),XK(KB),YK(KB),JA,DIS,XN,YN,RS)
           IF (RS > 0.3D0 .AND. RS < 0.7D0) THEN      ! ZIT HET CENTRALE PUNT 'MIDDEN' TUSSEN EEN CELRAND DAN VERFIJN DIE RAND
              JA2 = 1
           ENDIF
        ENDIF
        IF (JA2 == 1) THEN
!           IF (KPL(1,L1) == 0) THEN
!               KPL(1,L1) = KP !; NCL(1,L1) = K
!           ELSE
!               KPL(2,L1) = KP !; NCL(2,L1) = K
!           ENDIF
           IF ( lne(1,L1).eq.n ) THEN
               KPL(1,L1) = KP !; NCL(1,L1) = K
           ELSE
               KPL(2,L1) = KP !; NCL(2,L1) = K
           ENDIF

        ENDIF
     ENDDO

  ENDDO
  DO L = 1,NUML
     IF (KPL(1,L) .NE. 0 .or. KPL(2,L) .NE. 0) THEN
        K1 = KN(1,L)  ; K2 = KN(2,L)
        XL = 0.5D0*( XK(K1) + XK(K2) )
        YL = 0.5D0*( YK(K1) + YK(K2) )
        CALL dSETNEWPOINT(XL,YL,KP); KP2(L) = KP   ! PUNT OP LINK ZELF
        KPL(3,L) = KN(2,L)
        KN(2,L)  = KP
        CALL NEWLINK(KP, K2, LNU)
        IF (KPL(1,L) .NE. 0) THEN
           if ( netcell(lne(1,L))%N.ne.3 ) then ! SPvdP: no center point for triangles
              CALL NEWLINK(KP, KPL(1,L), LNU)
           end if
        END IF
        IF (KPL(2,L) .NE. 0) THEN
           if ( netcell(lne(2,L))%N.ne.3 ) then ! SPvdP: no center point for triangles
              CALL NEWLINK(KP, KPL(2,L), LNU)
           end if
        ENDIF
     ENDIF
  ENDDO
! SPvdP: check for triangles with two sides refined and refine the third
  MAXITER = 1
  do iter=1,MAXITER
     numtris = 0
     do n=1,nump
        NN = netcell(n)%N
        if ( NN.ne.3 ) cycle  ! triangles only
        num = 0
        do kk=1,NN
           L = netcell(n)%lin(kk)
           if ( kp2(L).ne.0 ) then
              num = num+1
              if ( num.eq.1 ) L1 = L
           end if
        end do
        if ( num.ne.1 ) cycle ! one unrefined side only
!       refine the single unrefined side
        if ( KPL(1,L1).eq.0 ) KPL(1,L1) = kn(1,L1)
        if ( KPL(2,L1).eq.0 ) KPL(2,L1) = kn(1,L1)
        K1 = KN(1,L1)  ; K2 = KN(2,L1)
        XL = 0.5D0*( XK(K1) + XK(K2) )
        YL = 0.5D0*( YK(K1) + YK(K2) )
        CALL dSETNEWPOINT(XL,YL,KP); KP2(L1) = KP   ! PUNT OP LINK ZELF
        KPL(3,L1) = KN(2,L1)
        KN(2,L1)  = KP
        CALL NEWLINK(KP, K2, LNU)
     end do

     if ( numtris.eq.0 ) exit
  end do
  if ( numtris.ne.0 ) then
     call qnerror('refinecellsandfaces: numtris.ne.0', ' ', ' ')
  end if

! SPvdP: make links inside triangles
  do n=1,nump
     NN = netcell(n)%N
     if ( NN.ne.3 ) cycle
        do kk=1,3
           k1 = kp2(netcell(n)%lin(kk))
           k2 = kk+1; if ( k2.gt.NN ) k2=k2-NN
           k2 = kp2(netcell(n)%lin(k2))
           if ( k1.gt.0 .and. k2.gt.0 .and. k1.ne.k2 ) then
              call newlink(k1,k2,Lnu)
           end if
        end do
  end do
! SPvdP: connect new nodes with inactive part of the net
   do L=1,numL_old
!     check and see if this link neighbors a refined and an unrefined cell
      if ( lnn(L).lt.2 ) cycle
      ic1 = lne(1,L)
      ic2 = lne(2,L)
      if ( (zc(ic1).eq.DMISS .and. zc(ic2).eq.DMISS) .or.  &
           (zc(ic1).ne.DMISS .and. zc(ic2).ne.DMISS) ) cycle

!     find the refined neighboring cell
      if ( zc(ic1).eq.DMISS ) then
         n = ic1
      else
         n = ic2
      end if
      NN = netcell(n)%N

!     find link in the netcell administration
      do kk=1,NN
         if ( netcell(n)%lin(kk).eq.L ) exit
      end do
      if ( netcell(n)%lin(kk).ne.L ) cycle   ! should never happen

        kp = kp2(L)
        if ( kp.ne.0 ) then
           kkm1 = kk-1; if ( kkm1.lt.1  ) kkm1=kkm1+NN
           L1   = netcell(n)%lin(kkm1)  ! left-connected link
           kkp1 = kk+1; if ( kkp1.gt.NN ) kkp1=kkp1-NN
           L2   = netcell(n)%lin(kkp1)  ! right-connected link

!          nearest node on the left-connected link
           kA = kp2(L1)
           if ( kA.lt.1 ) then
!             check if the next link has a refinement
              kkm2 = kkm1-1; if ( kkm2.lt.1 ) kkm2=kkm2+NN
              Lm2  = netcell(n)%lin(kkm2)

              if ( kp2(Lm2).eq.0 ) then   ! next link has no refinement
!                note: original nodes of link L are: kn(1,L) and kpL(3,L)
                 if ( kn(1,L1).eq.kn(1,L) .or. kn(1,L1).eq.kpL(3,L) ) then
                    kA = kn(2,L1)
                 else
                    kA = kn(1,L1)
                  end if
               else
                  kA = kp2(Lm2)
               end if
           end if

!           nearest node on the right-connected link
           kB = kp2(L2)
           if ( kB.lt.1 ) then
!             check if the next link has a refinement
              kkp2 = kkp1+1; if ( kkp2.gt.NN ) kkp2=kkp2-NN
              Lp2  = netcell(n)%lin(kkp2)

              if ( kp2(Lp2).eq.0 ) then   ! next link has no refinement
!                note: original nodes of link L are: kn(1,L) and kpL(3,L)
                 if ( kn(1,L2).eq.kn(1,L) .or. kn(1,L2).eq.kpL(3,L) ) then
                    kB = kn(2,L2)
                 else
                    kB = kn(1,L2)
                 end if
              else
                 kB = kp2(Lp2)
              end if
           end if

           call newlink(kA,kp,Lnu)
           call newlink(kp,kB,Lnu)
        end if
   end do

  DEALLOCATE(XC,YC,AR,KPL,KP2)
  IF (ALLOCATED(ZC)) THEN
     DEALLOCATE(ZC)
  ENDIF
  if ( allocated(NA) ) deallocate(NA)

  netstat = NETSTAT_CELLS_DIRTY
  CALL SETNODADM (0)

  IF (JACOURANTNETWORK == 1 .AND. JADOEN == 1 .AND. NUMK < 1E6) THEN  ! NOT BEYOND 4*1 MILLION GRIDPOINTS
     GOTO 10
  ENDIF

  END SUBROUTINE REFINECELLSANDFACES
