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

  SUBROUTINE MAKECOARSE2FINETRIANGLECONNECTIONCELLS()
  use m_netw
  use m_sferic, only: jsferic, jasfer3D, dtol_pole
  use m_missing, only : dxymis
  use geometry_module, only: dcosphi
  use gridoperations

  implicit none
  INTEGER          :: N3(6), N4(4)
  DOUBLE PRECISION :: ARN, XCN, YCN
  INTEGER          :: N, NN, K3, K, K0, NR, KA, KB, K1, K2, L1, L2, LNU, K01, KP, K7, KK3, K03, NN3, KK, L, K23

  CALL FINDCELLS(0)

  DO N = 1,NUMP
     NN = netcell(N)%N
     IF (NN == 5 .OR. NN ==6) THEN
        K3 = 0 ; N3 = 0
        DO K  = 1,NN
           K0 = netcell(N)%NOD(K)
           NR = NMK(K0)
           IF (NR == 3) THEN
              KA = K + 1; IF (KA > NN) KA = KA - NN ; K1 = netcell(N)%NOD(KA)      ! L2  L1  K0  K1  K2
              KA = K + 2; IF (KA > NN) KA = KA - NN ; K2 = netcell(N)%NOD(KA)
              KA = K - 1; IF (KA < 1 ) KA = KA + NN ; L1 = netcell(N)%NOD(KA)
              KA = K - 2; IF (KA < 1 ) KA = KA + NN ; L2 = netcell(N)%NOD(KA)

              IF (    ABS(dcosphi(XK(L2), YK(L2), XK(L1), YK(L1) , XK(L1), YK(L1), XK(K0), YK(K0), jsferic, jasfer3D, dxymis ) ) < 0.3   .AND.  &
                      ABS(dcosphi(XK(K0), YK(K0), XK(K1), YK(K1) , XK(K1), YK(K1), XK(K2), YK(K2), jsferic, jasfer3D, dxymis ) ) < 0.3  ) THEN

                 IF ( ABS(dcosphi(XK(L1), YK(L1), XK(K0), YK(K0) , XK(K0), YK(K0), XK(K1), YK(K1), jsferic, jasfer3D, dxymis ) ) > 0.7 ) THEN
                     ! PROBABLY THE SMALL SIDES AROUND THE CENTRAL POINT
                     K3     = K3 + 1
                     N3(K3) = K
                 ENDIF

              ENDIF

           ENDIF
        ENDDO
        IF (K3 == 3) THEN
           K3 = 1D0* K3
        ENDIF

        IF (K3 == 1 .AND. NN == 5) THEN
           K0 = netcell(N)%NOD(N3(K3))
           KA = N3(K3) + 2 ; IF (KA > NN) KA = KA - NN ; K1 = netcell(N)%NOD(KA)
           KB = N3(K3) - 2 ; IF (KB < 1)  KB = KB + NN ; K2 = netcell(N)%NOD(KB)
           CALL NEWLINK(K0, K1, LNU)
           CALL NEWLINK(K0, K2, LNU)
        ELSE IF (K3 == 2 .AND. NN == 6 ) THEN
           K3 = 1
           K0 = netcell(N)%NOD(N3(K3)) ; K01 = K0
           KA = N3(K3) + 2 ; IF (KA > NN) KA  = KA - NN ; K1 = netcell(N)%NOD(KA)
           KB = N3(K3) - 2 ; IF (KB < 1)  KB  = KB + NN ; K2 = netcell(N)%NOD(KB)
           CALL NEWLINK(K0, K1, LNU)
           CALL NEWLINK(K0, K2, LNU)
           K3 = 2
           K0 = netcell(N)%NOD(N3(K3))
           KA = N3(K3) + 2 ; IF (KA > 6) KA = KA - 6 ; K1 = netcell(N)%NOD(KA)
           KB = N3(K3) - 2 ; IF (KB < 1) KB = KB + 6 ; K2 = netcell(N)%NOD(KB)
           IF (K1 .NE. K01) CALL NEWLINK(K0, K1, LNU)
           IF (K2 .NE. K01) CALL NEWLINK(K0, K2, LNU)
        ELSE IF (K3 == -3 .AND. NN == 7 ) THEN           ! NAAR CENTRAAL POINT MET VIJF LINKJES, TWEE VANUIT HOEKEN, DIRE VANUIT K3 = 3
           CALL getcellsurface ( N, ARN, XCN, YCN)      ! SORRY, 7 IS EILAND EN WORDT DUS SOWIESO NIET HERKEND, EVEN VERGETEN
           CALL dSETNEWPOINT(XCN, YCN, KP)

           DO K3 = 1,3
              K0 = netcell(N)%NOD(N3(K3))
              CALL NEWLINK(K0, KP, LNU)
           ENDDO

           DO K7  = 1,7
              K   = NETCELL(N)%NOD(K7)
              KK3 = 0                     ! VOOR PUNTEN DIE GEEN K3 ZIJN
              DO K3 = 1,3
                 K03 = N3(K3)
                 KK3 = K03
              ENDDO
              IF (KK3 == 0) THEN

                 NN3 = 0
                 DO KK = 1, NMK(K)
                    L  = NETCELL(N)%LIN(K7)
                    CALL OTHERNODE(K,L,K2)
                    DO K3 = 1,3
                       K23 = N3(K3)
                       IF (K23 == K2) THEN
                          NN3 = NN3 + 1
                       ENDIF
                    ENDDO
                    IF (NN3 .NE. 2) THEN
                       CALL NEWLINK(K0, KP, LNU)
                    ENDIF
                 ENDDO
              ENDIF
           ENDDO

        ENDIF

     ENDIF

  ENDDO

  CALL SETNODADM(0)

  END SUBROUTINE MAKECOARSE2FINETRIANGLECONNECTIONCELLS
