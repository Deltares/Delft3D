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

      SUBROUTINE POLTONET(L1,L2)   ! PULL POLYGON TO NETWORK, KEEPING SUITABLE TRIANGLES TO OUTSIDE

      use m_netw
      use m_polygon
      use m_missing
      use m_wearelt
      use m_sferic, only: jsferic, jasfer3D, dtol_pole
      use gridoperations

      implicit none
      integer :: l1
      integer :: l2

      double precision :: d1, d2, xp1, xp2, yp1, yp2
      integer :: i
      integer :: ja
      integer :: k
      integer :: k1
      integer :: k2
      integer :: kk
      integer :: kl1
      integer :: kl2
      integer :: kn1
      integer :: knaar
      integer :: l
      integer :: ll
      integer :: n
      integer :: n1
      integer :: n2

      DOUBLE PRECISION :: XR, YR, XN, YN, XR1, YR1, XR2, YR2, AR1, DIS
      CALL SAVEPOL()

      IF (L1 > L2) THEN
         LL = L1 ; L1 = L2 ; L2 = LL
      ENDIF

      XP1 = XPL(L1) ; YP1 = YPL(L1)
      XP2 = XPL(L2) ; YP2 = YPL(L2)

      NPL    = 4                    ! CHANGE POLYGON TO VISIBLE AREA
      XPL(1) = X1 ; YPL(1) = Y1
      XPL(2) = X2 ; YPL(2) = Y1
      XPL(3) = X2 ; YPL(3) = Y2
      XPL(4) = X1 ; YPL(4) = Y2

      CALL FINDCELLS(0)

      CALL MAKENETNODESCODING()

      CALL CLOSENETBNDLINK(XP1,YP1,N1)
      CALL CLOSENETBNDLINK(XP2,YP2,N2)

      IF (N1 == 0 .OR. N2 == 0) THEN
         CALL QNERROR('NO CLOSE NET POINT FOUND', ' ',' ')
      ENDIF


      DO N = 1,L1-1
         XPL(N) = XPH(N) ; YPL(N) = YPH(N)
      ENDDO


      L   = N2
      K1  = KN(1,L) ; K2 = KN(2,L)
      XP1 = 0.5D0*(XK(K1) + YK(K2) )
      YP1 = 0.5D0*(YK(K1) + YK(K2) )

!      CALL TEKNODE(K1,221)
!      CALL TEKNODE(K1,31)

      L   = N1
      K1  = KN(1,L) ; K2 = KN(2,L)
      D1  = SQRT( (XK(K1) - XP1)**2 + (YK(K1) - YP1)**2 )
      D2  = SQRT( (XK(K2) - XP1)**2 + (YK(K2) - YP1)**2 )

      IF (D1 > D2) THEN
         K  = K1 ; KNAAR = K2
      ELSE
         K  = K2 ; KNAAR = K1
      ENDIF

      N = L1 - 1


      DO WHILE (N < MAXPOL)
         JA = 0
         DO KK = 1,NMK(K)
            LL = NOD(K)%LIN(KK)
            CALL OTHERNODE(K,LL,K2)
            IF (L == N1 .AND. K2 == KNAAR .OR. LC(LL) .NE. -1 .AND. LNN(LL) == 1) THEN
               JA = 1
               L  = LL
               CALL OTHERNODE(K,L,K2)
               K  = K2; LC(L) = -1

               KL1 = KN(1,L) ; KL2 = KN(2,L)
               XR1 = XK(KL1) ; XR2 = XK(KL2)
               YR1 = YK(KL1) ; YR2 = YK(KL2)

               KN1 = LNE(1,L)
               CALL GETCELLSURFACE (KN1,AR1, XR, YR)
               CALL MIRRORLINE2(XR,YR,XR1,YR1,XR2,YR2,JA,DIS,XN,YN)

               N = N + 1
               XPL(N) = XN
               YPL(N) = YN

               ! CALL RCIRC( XPL(N), YPL(N) )
               ! CALL WAIT ()
               EXIT
            ENDIF
         ENDDO
         IF (L == N2) EXIT
      ENDDO

      DO I = L2+1, NPH
         N = N + 1
         XPL(N) = XPH(I)
         YPL(N) = YPH(I)
      ENDDO

      NPL = N

      END SUBROUTINE POLTONET
