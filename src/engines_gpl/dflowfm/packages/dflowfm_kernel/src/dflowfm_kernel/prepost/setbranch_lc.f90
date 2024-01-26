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

  SUBROUTINE SETBRANCH_LC(nrl1d)
  USE M_NETW
  use gridoperations

  IMPLICIT NONE

  INTEGER :: NRL1D, NRL, NRLO, L, JONCE, K, K1, K2, K3, IBR, N, JASTOP, JASTART, IERR, IBX, KS, KK, KE, ja, JA1, JA2
  INTEGER :: NRL1D6, KN316, NRL1D16, NUM0, J

  call setnodadm(0)

  IF (ALLOCATED(NMK0) ) DEALLOCATE(NMK0) ; ALLOCATE(NMK0(NUMK)) ; NMK0  = 0

  LC    = 0 ; NRL1D = 0 ; NRL1D6 = 0
  DO L  = 1,NUML
     IF (KN(3,L) == 1 .or. KN(3,L) == 6) THEN
        K1 = KN(1,L) ; K2 = KN(2,L) ; K3 = KN(3,L)
        NMK0(K1) = NMK0(K1) +  1
        NMK0(K2) = NMK0(K2) +  1
        IF (KN(3,L) == 1) THEN
           NRL1D  = NRL1D + 1                 ! count 1D links
        ELSE IF (KN(3,L) == 6) THEN
           NRL1D6 = NRL1D6 + 1
        ENDIF
     ELSE
        LC(L) = -1
     ENDIF
  ENDDO

  if (NRL1D + NRL1D6 == 0) then
      netstat = NETSTAT_OK ; return
  endif

  IF (ALLOCATED(IBN)) DEALLOCATE(IBN,LIB,K1BR,NRLB)
  ALLOCATE (IBN(NUML), LIB(NUML), K1BR(NUML), NRLB(NUML) ) ; IBN = 0; LIB = 0; K1BR = 0; NRLB = 0

  IBR  = 0; NRL = 0

  DO J = 1,2

     IF (J == 1) THEN
        KN316 = 6 ; NRL1D16 = NRL1D6
     ELSE
        KN316 = 1 ; NRL1D16 = NRL1D6 + NRL1D
     ENDIF

     DO WHILE (NRL < NRL1D16)

        NRLO = NRL
        DO L = 1,NUML
           IF (LC(L) == 0) THEN
              JASTART = 0
              CALL GAANWESTARTEN(L,K1,KN316,JASTART)
              IF (JASTART == 1) THEN
                 IBR = IBR + 1
                 CALL WALK1D(K1,IBR,NRL,JASTOP,KN316)
              ENDIF
           ENDIF
        ENDDO

        IF (NRL == NRLO) THEN ! REPAIR CODE, FILL IN ISOLATED BRANCHES
           DO L = 1,NUML
              IF ( LC(L) == 0 .AND. KN316 == KN(3,L) ) THEN
                 IBR = IBR + 1
                 LC(L) = IBR ; NRL = NRL + 1
                 LIB(NRL) = L ; K1BR(NRL) = KN(1,L) ; IBN(NRL) = IBR; NRLB(L) = NRL
              ENDIF
           ENDDO
        ENDIF

     ENDDO


  ENDDO

  IBX = IBR ; MXNETBR = IBR

  IF ( ALLOCATED(NETBR) ) DEALLOCATE(NETBR)
  ALLOCATE ( NETBR(IBX) ,STAT=IERR)
  CALL AERR('NETBR(IBX)',IERR,NUML)

  IBR   = 1
  KS    = 1
  NRL1D = NRL1D16

  DO K  = 1,NRL1D

     ja = 0
     if ( k < NRL1D ) then
        if ( IBR .NE. IBN(K+1) ) then
           ja = 1
        endif
     else
        ja = 1
     endif
     IF ( ja == 1 ) THEN
        KE = K
        N  = KE - KS + 1
        ALLOCATE ( NETBR(IBR)%LN(N) ,STAT=IERR )
        CALL AERR('NETBR(IBR)%LN(N)',IERR,  IBR)
        NETBR(IBR)%NX = N
        DO KK = KS, KE
           L  = LIB(KK)
           K1 = K1BR(KK)
           IF ( K1 == KN(1,L) ) THEN
              NETBR(IBR)%LN(KK-KS+1) = L
           ELSE IF (K1 == KN(2,L) ) THEN
              NETBR(IBR)%LN(KK-KS+1) = -L
           ELSE
              CALL OKAY(0) ! PROGRAMMING NO GOOD
           ENDIF

        ENDDO

        if ( k < NRL1D ) then
           IBR = IBN(K+1)
           KS  = K + 1
        ENDIF

     ENDIF

  ENDDO

  netstat = NETSTAT_OK

  END SUBROUTINE SETBRANCH_LC
