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

   SUBROUTINE REFINEPOLYGONUSINGNETWORK()
   use m_netw
   USE M_SAMPLES
   use m_ec_triangle
   USE M_POLYGON
   use m_ec_basic_interpolation, only: dlaun
   use geometry_module, only: pinpok, dpinpok, dbdistance
   use m_missing, only: dmiss, jins
   use m_sferic, only: jsferic, jasfer3D
   use gridoperations

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
   double precision :: rln, rlp, xa, ya, xkk, ykk, phimin,phimax

   DOUBLE PRECISION     :: X1, Y1, X2, Y2
   double precision, ALLOCATABLE :: XH (:), YH(:)
   INTEGER, ALLOCATABLE :: KIN(:), KS(:)

   IF (NPL .LE. 2) RETURN

   CALL FINDCELLS(0)

   JADOORLADEN = 1
   IF (JADOORLADEN .EQ. 0) THEN
      K0 = 0
      L0 = 0
   ELSE
      K0 = NUMK
      L0 = NUML
   ENDIF


   ALLOCATE ( KIN(NUMK), STAT=IERR) ; KIN = 0
   CALL AERR('KIN(NUMK)',IERR,NUMK)
   N = 0
   DO K = 1,NUMK                  ! SELECT outer GRIDPOINTS INSIDE POLYGON
      JARAND = 0
      DO NN = 1,NMK(K)
          L = NOD(K)%LIN(NN)
          IF (LNN(L) == 1) JARAND = 1
      ENDDO
      IF (JARAND == 1) THEN
         CALL PINPOK(XK(K), YK(K), NPL, XPL, YPL, IN, jins, dmiss)
         IF (IN == 1) THEN
             N     = N + 1
             KIN(N) = K
         ENDIF
      ENDIF
   ENDDO
   NKIN = N

   DISAV  = 0; NAV = 0                  ! AVERAGE GRIDSIZE SELECTED CELLS
   DO N   = 1,NKIN
      K1  = KIN(N)
      DO NN = 1,NMK(K1)
         L = NOD(K1)%LIN(NN)
         CALL OTHERNODE(K1,L,K2)
         DISAV = DISAV + DBDISTANCE (XK(K1),YK(K1),XK(K2),YK(K2), jsferic, jasfer3D, dmiss)
         NAV   = NAV   + 1
      ENDDO

   ENDDO
   IF (NAV .NE. 0) THEN
      DISAV = DISAV / dble(NAV)
      TRIANGLESIZE = DISAV
   ELSE
      DISAV = TRIANGLESIZE
   ENDIF


   RLP = 0D0
   DO N   = 1,NPL                       ! COUNT NR OF POINTS ON POLYGON
      N1  = N
      N2  = N+1 ; IF (N == NPL) N2 = 1
      X1  = XPL(N1) ; Y1 = YPL(N1) ; X2 = XPL(N2) ; Y2 = YPL(N2)
      RLP = RLP + DBDISTANCE(X1,Y1,X2,Y2, jsferic, jasfer3D, dmiss)
   ENDDO
   NH = 4*RLP / DISAV
   ALLOCATE ( XH(NH), YH(NH) , STAT=IERR )
   CALL AERR('XH(NH), YH(NH)', IERR, NH*2)

   K     = 0
   DO N  = 1,NPL                        ! SET NEW POINTS ON POLYGON
      N1 = N
      N2 = N+1 ; IF (N == NPL) N2 = 1
      X1    = XPL(N1) ; Y1 = YPL(N1) ; X2 = XPL(N2) ; Y2 = YPL(N2)
      RLN   = DBDISTANCE(X1,Y1,X2,Y2,jsferic, jasfer3D, dmiss)
      NN    = NINT(RLN / DISAV)
      CALL INCELLS(X1,Y1,INNUMP)
      IF (INNUMP == 0) THEN
         K     = K + 1
         XH(K) = X1 ; YH(K) = Y1
      ENDIF
      NN2 = 3*NN
      DO N2 = 1,NN2
         A  = dble(N2)/dble(NN2)
         XA = (1-A)*X1 + A*X2
         YA = (1-A)*Y1 + A*Y2
         CALL INCELLS(XA,YA,INNUMP)     ! XA, YA ZIT IN CELL NR INNUMP
         IF (INNUMP == 0) THEN
            IF ( MOD(N2,3) == 0 ) THEN
               K     = K + 1
               XH(K) = XA ; YH(K) = YA
            ENDIF
         ELSE
            CALL CLOSEIN(XA,YA,INNUMP,KIN,NKIN,KK)  ! KK IS HET MEEST DICHTBIJ GELEGEN POINT VAN INNUMP
            IF (KK .NE. 0) THEN
               XKK = XK(KK) ; YKK= YK(KK)
               IF (K == 0) THEN
                  K  = K + 1 ; XH(K) = XKK ; YH(K) = YKK
               ELSE IF (XKK .NE. XH(K)) THEN
                  K  = K + 1 ; XH(K) = XKK ; YH(K) = YKK
               ENDIF
            ENDIF
         ENDIF
      ENDDO
   ENDDO
   NPL = K
   XPL(1:NPL) = XH(1:NPL)
   YPL(1:NPL) = YH(1:NPL)

   DEALLOCATE (XH,YH,KIN)

   RETURN


   CALL  INCREASENETW(K0+NSIN,L0+6*NSIN)

   ALLOCATE (KS(6*NSIN) )


   N = 0                           ! ADD SELECTED SAMPLES TO NETWORK
   DO K = K0+1, K0+NSIN
      N     = N + 1
      XK(K) = XS(N)
      YK(K) = YS(N)
      ZK(K) = ZS(N)
      KS(N) = K
   ENDDO


   N    = NSIN                     ! ADD NETPOINTS IN ORIGINAL NUMK SET TO SAMPLES
   IF (NPL > 0) THEN
      DO K = 1,NUMK                ! NUM STILL OLD
         CALL DPINPOK(XK(K), YK(K), ZK(K), NPL, XPL, YPL, IN, jins, dmiss)
         IF (IN == 1) THEN
            N     = N + 1
            XS(N) = XK(K)
            YS(N) = YK(K)
            ZS(N) = ZK(K)
            KS(N) = K
         ENDIF
      ENDDO
   ENDIF
   NSDL = N

   CALL READYY('TRIANGULATING', 0d0)

   CALL DLAUN(XS,YS,NSDL,1,ierr)

   CALL READYY('TRIANGULATING', 0.3d0)

   L = L0
   DO N = 1,NUMTRI
      AF = 0.3d0 + 0.7d0*dble(N)/dble(NUMTRI)
      CALL READYY('TRIANGULATING', AF)


      JA = 1
      CALL CHECKTRIANGLE(N,JA,phimin,phimax)
      IF (JA == 0) THEN
         CYCLE
      ENDIF
      DO NN = 1,3
         N1 = NN ; N2 = N1 + 1 ; IF (N1 == 3) N2 = 1
         K1 = INDX(N1,N) ; K2 = INDX(N2,N)
         K1 = KS(K1)     ; K2 = KS(K2)

         NEW = 1
         DO LL  = NUML, 1, -1
            K1L = KN(1,LL) ; K2L = KN(2,LL)
            IF (K1 .EQ. K1L .AND. K2 .EQ. K2L .OR.    &
                K2 .EQ. K1L .AND. K1 .EQ. K2L ) THEN
                NEW = 0 ; EXIT
            ENDIF
         ENDDO

         IF (NEW .EQ. 0) CYCLE

         L = L + 1 ;
         IF (L > LMAX) THEN
            CALL INCREASENETW(INT(1.2d0*NUMK), INT(1.2d0*NUML) )
         ENDIF
         NUML = L
         KN(1,L) = K1 ; KN(2,L) = K2

      ENDDO
   ENDDO

   DEALLOCATE (KS)

   CALL READYY('TRIANGULATING', -1d0)

   NUMK = K0 + NSIN
   NUML = L

   CALL SETNODADM(1)

   RETURN
   END SUBROUTINE REFINEPOLYGONUSINGNETWORK
