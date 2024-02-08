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

  SUBROUTINE CUTCELLSORG()

  use m_netw
  use m_missing, only: dmiss, JINS
  use m_polygon, only: NPL, xpl, ypl, zpl
  use geometry_module, only: dbpinpol
  use gridoperations

  implicit none
  integer :: iabs
  integer :: ja, KMOD
  integer :: k
  integer :: k1
  integer :: k2
  integer :: k3
  integer :: k4
  integer :: km
  integer :: l
  integer :: ll
  integer :: lnu
  integer :: n
  integer :: n1
  integer :: n2
  integer :: nn
  integer :: nr
  INTEGER , ALLOCATABLE :: KNP(:), KNEW(:), LDIN(:), LD1(:), LD2(:)
  INTEGER :: KK(4)

  DOUBLE PRECISION      :: XM, YM


  IF (MXLAN == 0) RETURN

  CALL READYY('CUTCELLS',0d0)

  CALL SAVEPOL()

  ALLOCATE(LDIN(MXLAN), LD1(1000), LD2(1000))
           LDIN = 0   ; LD1 = 0  ; LD2 = 0


  LDIN(1) = -1
  DO K = 1,MXLAN
     CALL DBPINPOL( XLAN(K), YLAN(K), LDIN(K), dmiss, jins, NPL, xpl, ypl, zpl ) ! ALL LDB POINTS INSIDE POLYGON
  ENDDO

  NR = 0; N1 = 0; N2 = 0
  DO K = 1,MXLAN ! + 1 ! TODO [AvD] allocate met +1 en even doorlopen
     IF (XLAN(K) .NE. -999D0 .AND. LDIN(K) == 1) THEN
        IF (N1 == 0) N1 = K
        N2 = K
        IF (LDIN(K) == 1) JA = 1                            ! SOME POINT OF LDB IS INSIDE POL,
     ELSE IF (N1 .NE. 0) THEN                               ! THIS LDB SEGMENT WILL BE HANDLED
        IF (JA == 1) THEN
          NR = NR + 1; LD1(NR) = N1; LD2(NR) = N2
        ENDIF
        N1 = 0; N2 = 0
     ENDIF
  ENDDO

  DO NN = 1,NR

     N1 = LD1(NN) ; N2 = LD2(NN)
     CALL COPYLDBPIECETOPOL(N1,N2)
     CALL FINDCELLS(4)                                      ! ALL FACES INSIDE LANDBOUNDARY PIECE

     ALLOCATE (KNP (NUMP)); KNP  = 0
     ALLOCATE (KNEW(NUML)); KNEW = 0

     DO N = 1,NUMP
        IF ( netcell(N)%N == 4 ) THEN
           K1     = netcell(N)%NOD(1)
           K2     = netcell(N)%NOD(2)
           K3     = netcell(N)%NOD(3)
           K4     = netcell(N)%NOD(4)
           KNP(N) = KC(K1)*KC(K2)*KC(K3)*KC(K4)             ! COMPLETELY INSIDE = 1
        ENDIF
     ENDDO

     KMOD = MAX(1,NUMP/100)
     DO N = 1,NUMP

        if (mod(n,KMOD) == 0) CALL READYY('CUTCELLS', dble(n)/dble(nump))

        IF ( KNP(N) == 0 ) THEN                             ! AT LEAST 1 POINT OUTSIDE POLYGON

           DO LL = 1,4

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

     DO N = 1,NUMP

        K = 0
        DO LL = 1,4

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
                 K = K + 1 ; KK(K) = IABS(KNEW(L))

              ENDIF

           ENDIF


           IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
              IF  (KC(K1) == 1 .OR. KC(K2) == 1) THEN
                  KN(1,L) = 0 ; KN(2,L) = 0
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

     CALL SETNODADM(0)

     DEALLOCATE(KNP,KNEW)

  ENDDO

  DEALLOCATE ( LDIN, LD1, LD2 )


  CALL RESTOREPOL()

  CALL SETNODADM(0)

  CALL READYY('CUTCELLS', -1d0)

  END SUBROUTINE CUTCELLSORG
