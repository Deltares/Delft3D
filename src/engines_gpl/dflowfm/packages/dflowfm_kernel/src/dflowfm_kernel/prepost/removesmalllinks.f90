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

   SUBROUTINE REMOVESMALLLINKS()  ! 1 REMOVES IF FLOW LINK DISTANCES ARE SMALL RELATIVE TO CONNECTED CELL SIZES

   use m_netw                     ! 2 REMOVES SMALL TRIANGLES NEXT TO
   use M_FLOWGEOM
   use unstruc_messages
   use geometry_module, only: dbdistance, dcosphi, dlinedis
   use m_missing, only: dmiss, dxymis
   use m_sferic, only: jsferic, jasfer3D, dtol_pole
   use gridoperations
   use m_mergenodes
   implicit none

   integer            :: minp

   DOUBLE PRECISION :: R01, R02, AN1, AN2, XL, YL, XR, YR, XZWr, YZWr, ZZZ
   INTEGER          :: KL1, KL2, KN1a, KN2a, L, jaremove

   DOUBLE PRECISION :: AREA, TAREA, COSMIN, COSPHI, FRAC, DIS, XN, YN
   INTEGER          :: NAAST, N, NN, NUMT, LL, K0, K1, K2, KHOEK, LU, LD, KA, KB, KH, K, JA, KNEW, IERR, NW
   INTEGER          :: LLA, LLB, LLC, L0, L1, L2, LT, LI, KK, NL, NR

   DOUBLE PRECISION, ALLOCATABLE :: XNW(:), YNW(:)
   INTEGER         , ALLOCATABLE :: NNW(:,:)

   CALL SAVENET()

   CALL SETNODADM(0)                                   !

   CALL REMOVECOINCIDINGTRIANGLES()                    !

   CALL FINDCELLS(0)

!   take dry cells into account (after findcells)
    call delete_dry_points_and_areas()

   JAREMOVE = 0
   DO L = 1, NUML                                      ! REMOVE SMALL CIRCUMCENTRE DISTANCES
      IF (LC(L) == 1) THEN
         KL1 = KN(1,L) ; KL2 = KN(2,L)
         IF (KL1 > 0 .AND. KL2 > 0) THEN
            KN1a = LNE(1,L) ; KN2a = LNE(2,L)
            IF (KN1a > 0 .AND. KN2a > 0) THEN
               NL = netcell(KN1a)%N ; NR = netcell(KN2a)%N

!!              SPvdP: only remove small flow links, if adjacent cells are triangles
!               IF ( (NL == 3 .or. NL == 4) .and. (NR == 3 .or. NR == 4) ) THEN
               IF ( (NL == 3 .and. NR == 3 .and. maxfaceallow == 4) .or.  &
                    (NL == 3 .or. NL == 4) .and. (NR == 3 .or. NR == 4)  .and. maxfaceallow == 5 ) THEN
                  CALL GETCELLSURFACE (KN1a,AN1, XZWr, YZWr)
                  CALL GETCELLSURFACE (KN2a,AN2, XZWr, YZWr)
                  CALL GETCELLWEIGHTEDCENTER(KN1a, XL,  YL, ZZZ)
                  CALL GETCELLWEIGHTEDCENTER(KN2a, XR,  YR, ZZZ)
                  R01 = 0.5d0*(SQRT(AN1) + SQRT(AN2))  ! TYPICAL SIDE
                  R02 = DBDISTANCE (XL, YL, XR, YR,jsferic, jasfer3D, dmiss)    ! CIRCUMDISTANCE
                  IF (R02 < removesmalllinkstrsh*R01) THEN
                     KN(1,L)  = 0 ; KN(2,L) =  0 ; KN(3,L) = -1      ! CALL DELLINK(L)
                     JAREMOVE = 1
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDDO

   IF (JAREMOVE == 1) THEN
      CALL FINDCELLS(0)
   ENDIF

   ALLOCATE ( XNW(NUMK),YNW(NUMK),NNW(3,NUMK) , STAT=IERR   )
   CALL AERR('XNW(NUMK),YNW(NUMK),NNW(3,NUMK)', IERR, NUMK*3)

   JAREMOVE = 0 ; NW = 0
   DO LT = 1,NUML

      N  = LNE(1,LT) ; NN = 0
      IF  (N > 0 ) NN = NETCELL(N)%N

      IF (LNN(LT) == 1 .AND. NN == 3) THEN    ! SMALL BOUNDARY TRIANGLES

         TAREA = 0D0; NUMT = 0
         DO LL = 1,3                          ! ESTABLISH TYPICAL CELLSIZE ADJACENT QUADS
            L  = NETCELL(N)%LIN(LL)
            IF (LNN(L) == 2 ) THEN
               KN1a = LNE(1,L) ; KN2a = LNE(2,L)
               IF (KN1A == N) THEN
                  NAAST = KN2A
               ELSE
                  NAAST = KN1A
               ENDIF
               IF (NETCELL(NAAST)%N > 3) THEN ! ADJACENT QUADS ETC
                  CALL GETCELLSURFACE ( NAAST, AREA, XZWr, YZWr)
                  TAREA = TAREA + AREA; NUMT = NUMT + 1
               ENDIF
            ENDIF
         enddo
         IF (NUMT .NE. 0) THEN
            TAREA = TAREA / NUMT
         ENDIF
         IF (TAREA .NE. 0) THEN
            CALL GETCELLSURFACE ( N, AREA, XZWr, YZWr)
            IF (AREA > 0D0) THEN
               FRAC = AREA/TAREA
               IF (FRAC < TRIAREAREMFRAC) THEN
                  COSMIN = 1D0
                  DO LL = 1,3                   ! FIND KHOEK
                     LU = LL + 1 ; IF (LU == 4) LU = 1
                     LD = LL - 1 ; IF (LD == 0) LD = 3
                     K0 = NETCELL(N)%NOD(LD)  ; L0 = NETCELL(N)%LIN(LD)
                     K1 = NETCELL(N)%NOD(LL)  ; L1 = NETCELL(N)%LIN(LL)
                     K2 = NETCELL(N)%NOD(LU)  ; L2 = NETCELL(N)%LIN(LU)
                     COSPHI = ABS( DCOSPHI(XK(K0),YK(K0),XK(K1),YK(K1),XK(K1),YK(K1),XK(K2),YK(K2), jsferic, jasfer3D, dxymis) )
                     IF (COSPHI < COSMIN) THEN
                        COSMIN  = COSPHI
                        KA = K0; KH = K1; KB = K2; LLA = L0; LLB = L1; LLC = L2
                     ENDIF
                  ENDDO

                  IF (COSMIN < 0.2 .AND. LNN(LLC) == 1) THEN
                     CALL dLINEDIS(XK(KH),YK(KH),XK(KA),YK(KA),XK(KB),YK(KB),JA,DIS,XN,YN, jsferic, jasfer3D, dmiss)

                     NW        = NW + 1
                     XNW  (NW) = XN
                     YNW  (NW) = YN
                     NNW(1,NW) = KH
                     NNW(2,NW) = KA
                     NNW(3,NW) = KB
                     JAREMOVE = 1 ; CYCLE
                  ENDIF

               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDDO


   IF (JAREMOVE == 1) THEN

      DO K = 1,NW
         KH     = NNW(1,K)
         KA     = NNW(2,K)
         KB     = NNW(3,K)
         XK(KH) = XNW(  K)
         YK(KH) = YNW(  K)
         LI     = 0
         DO KK = 1, NMK(KA)    ! NOG EVEN CHECKEN OF DE LINK TUSSEN KA EN KH NIET MEER DAN ENKELVOUDIG INTERN VERBONDEN IS
            L = NOD(KA)%LIN(KK)
            IF (LNN(L) == 2) THEN
               LI = LI + 1
            ENDIF
         ENDDO
         IF (LI == 1) THEN
            CALL MERGENODES(KA,KH,JA)
         ENDIF
         LI     = 0
         DO KK = 1, NMK(KB)
            L = NOD(KB)%LIN(KK)
            IF (LNN(L) == 2) THEN
               LI = LI + 1
            ENDIF
         ENDDO
         IF (LI == 1) THEN
            CALL MERGENODES(KB,KH,JA)
         ENDIF
      ENDDO


      CALL SETNODADM(0)

!     netcell administration out of date
      netstat = NETSTAT_CELLS_DIRTY


   ENDIF

   DEALLOCATE(XNW,YNW,NNW)

   END SUBROUTINE REMOVESMALLLINKS
