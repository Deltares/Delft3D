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

  SUBROUTINE CUTCELLS(n12)
  use m_netw
  use gridoperations
  implicit none
  integer, intent(in) :: N12
  integer :: ja, KMOD
  integer :: K, KM, K1, K2, K3, K4, L, LL, LNU,N,N1,N2,NN, IN
  INTEGER , ALLOCATABLE :: KNP(:), KNEW(:)
  INTEGER :: KK(4)

  DOUBLE PRECISION      :: XM, YM

  CALL READYY('CUTCELLS',0d0)

  CALL FINDCELLS(0)                                      ! ALL FACES INSIDE LANDBOUNDARY PIECE

  ALLOCATE (KNP (NUMP)); KNP  = 0
  ALLOCATE (KNEW(NUML)); KNEW = 0

  DO N = 1,NUMP
     NN = netcell(N)%N
     IF ( NN >= 4 ) THEN
        K1     = NETCELL(N)%NOD(1)
        KNP(N) = KC(K1)
        DO K = 2,NN
           K1 = NETCELL(N)%NOD(K)
           KNP(N) = KNP(N)*KC(K1)                        ! COMPLETELY INSIDE = 1
        ENDDO
     ENDIF
  ENDDO

  KMOD = MAX(1,NUMP/100)
  DO N = 1,NUMP

     if (mod(n,KMOD) == 0) CALL READYY('CUTCELLS', dble(n)/dble(nump))

     IF ( KNP(N) == 0 ) THEN                             ! AT LEAST 1 POINT OUTSIDE POLYGON

        NN = netcell(N)%N

        DO LL = 1,NN

           L  = netcell(N)%LIN(LL)

           IF (KNEW(L) == 0) THEN

              CALL CROSSLINKPOLY(L,0,0,(/0/),(/0/),XM,YM,JA)

              IF ( JA == 1 ) THEN
                 CALL DSETNEWPOINT( XM, YM, KM )
                 KNEW(L) = KM
              ENDIF

           ENDIF

        ENDDO

     ENDIF

  ENDDO

  DO N  = 1,NUMP

     K  = 0
     NN = netcell(N)%N
     DO LL = 1,NN

        L  = netcell(N)%LIN(LL)
        K1 = KN(1,L) ; K2 = KN(2,L)

        IF ( KNP(N) == 0 ) THEN                             ! SHOULD BE HANDLED

           IF (KNEW(L) .NE. 0) THEN                         ! NIEUW PUNT KOPPELEN

              IF (KNEW(L) > 0) THEN
                 IF (KC(K1) == 1) THEN
                    CALL NEWLINK( KNEW(L), K2, LNU)
                 ELSE
                    CALL NEWLINK( KNEW(L), K1, LNU)
                 ENDIF
                 KNEW(L) = -1*KNEW(L)
              ENDIF
              K     = K + 1
              KK(K) = IABS(KNEW(L))

           ENDIF

        ENDIF

     ENDDO


     IF (K >= 2) THEN
        CALL NEWLINK(KK(1), KK(2), LNU)
     ENDIF

     IF (K >= 3) THEN
        CALL NEWLINK(KK(2), KK(3), LNU)
     ENDIF

     IF (K >= 4) THEN
        CALL NEWLINK(KK(3), KK(4), LNU)
     ENDIF
  ENDDO

  IF (N12 .NE. 4) THEN
     DO L = 1, NUML
        K1 = KN(1,L) ; K2 = KN(2,L) ! NETPUNTEN DIE NIET IN NUMP VOORKWAMEN OOK MAAR GELIJK WEG
        IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
           IF  (KC(K1) == 1 .OR. KC(K2) == 1) THEN
               KN(1,L) = 0 ; KN(2,L) = 0
           ENDIF
        ENDIF
     ENDDO
  ENDIF

  DEALLOCATE(KNP,KNEW)

  CALL SETNODADM(0)

  CALL READYY('CUTCELLS', -1d0)

  END SUBROUTINE CUTCELLS
