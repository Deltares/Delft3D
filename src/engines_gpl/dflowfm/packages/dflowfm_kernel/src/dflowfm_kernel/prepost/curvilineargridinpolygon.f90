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

      !SUBROUTINE SPLINESFROMLANDBOUNDARY()
      !USE M_SPLINES
      !USE M_GRIDSETTINGS
      !use m_missing
      !
      !END SUBROUTINE SPLINESFROMLANDBOUNDARY
      SUBROUTINE curvilinearGRIDinpolygon()
      USE M_POLYGON
      USE M_SAMPLES
      USE M_GRID
      USE M_GRIDSETTINGS
      use m_orthosettings
      use m_missing
      use m_netw
      use m_sferic, only: jsferic, jasfer3D
      use geometry_module, only: dcosphi
      implicit none

      double precision :: atpfo
      double precision :: dp
      double precision :: dpok1
      double precision :: ff
      integer :: ierr
      integer :: jam
      integer :: jan
      integer :: k
      integer :: k1
      integer :: ka
      integer :: km
      integer :: mfo
      integer :: mout
      integer :: n
      integer :: n1
      integer :: n2
      integer :: ndraw
      integer :: ndraw8org
      integer :: nfo
      integer :: npo
      integer :: nr
      common /drawthis/ ndraw(50)
      double precision :: dprodin


      DOUBLE PRECISION, ALLOCATABLE :: XH(:,:), YH(:,:)

      DOUBLE PRECISION, ALLOCATABLE :: XPA(:), YPA(:), DPA(:)
      DOUBLE PRECISION, ALLOCATABLE :: XPO(:), YPO(:), DPO(:)

      DOUBLE PRECISION              :: TXO, DXO, PRIN
      INTEGER                       :: MNX, MAXP
      integer                       :: npc(5)
      integer                       :: ierror

      if (npl < 4) return

!     create O-type pillar grid if the pillar radius .ne. 0d0
      if ( pil_rad.ne.0d0 ) then
         call pillargrid(ierror)
         if ( ierror.eq.0 ) return  ! otherwise, generate non-pillar grid
      end if


      CALL SAVEPOL()

      DO K = 1,NPL
         IF (XPL(K) .NE. xymis) THEN
            KM = K
         ELSE
            EXIT
         ENDIF
      ENDDO
      NPL = KM


      IF ( XPL(1) .NE. XPL(NPL) ) THEN
         NPL      = NPL + 1
         XPL(NPL) = XPL(1)
         YPL(NPL) = YPL(1)
      ENDIF

      NPO = NPL
      ALLOCATE ( DPO(NPO) , XPO(NPO), YPO(NPO) , STAT = IERR) ; DPO =0D0
      CALL AERR('DPO(NPO) , XPO(NPO), YPO(NPO)', IERR,   NPO)
      XPO(1:NPO) = XPL(1:NPO)
      YPO(1:NPO) = YPL(1:NPO)

      NR      = 1  ! FIRST
      NPC(NR) = 1

      !CALL SETCOL(31)
      !CALL RCIRC ( XPL(1), YPL(1) )

      DO N = 2,NPL - 1
         prin = dcosphi(XPO(N-1), YPO(N-1),  XPO(N)  , YPO(N)  , &
                        XPO(N)  , YPO(N)  ,  XPO(N+1), YPO(N+1), jsferic, jasfer3D, dxymis)
         prin = dabs(prin)
         IF (PRIN < 0.5d0) THEN
            CALL RCIRC ( XPL(1), YPL(1) )
            NR = NR + 1
            IF (NR <= 4) THEN
               NPC(NR) = N
            ENDIF
         ENDIF

      ENDDO

      IF (NR < 4) THEN
         CALL QNERROR('LESS THAN FOUR CORNERS FOUND',' ',' ')
         CALL RESTOREPOL()
         DEALLOCATE (DPO, XPO, YPO)
         RETURN
      ELSE IF (NR > 4) THEN
         CALL QNERROR('MORE THAN 4 CORNERS FOUND',' ',' ')
         CALL RESTOREPOL()
         DEALLOCATE (DPO, XPO, YPO)
         RETURN
      ENDIF

      NR = NR + 1
      NPC(NR) = NPL

      MFO = MFAC ; NFO = NFAC
      MC  = MFAC + 1
      NC  = NFAC + 1

      IF (MFO == 0) THEN
         MC = NPC(2) - NPC(1) + 1 ; MFAC = MC - 1
         JAM = 1
      ENDIF

      IF (NFO == 0) THEN
         NC = NPC(5) - NPC(4) + 1 ; NFAC = NC - 1
         JAN = 1
      ENDIF

      call INCREASEGRID(MC,NC)

      MNX = 5*MAX(MC,NC)
      ALLOCATE ( XH(MNX,4), YH(MNX,4) )

      ALLOCATE ( DPA(MNX) , XPA(MNX), YPA(MNX) , STAT = IERR) ; DPA =0D0
      CALL AERR('DPA(MNX) , XPA(MNX), YPA(MNX)', IERR,   MNX)

      CALL accumulateDistance(XPO, YPO , DPO, NPO)       ! OORSPRONKELIJKE LENGTECOORDINAAT

      KA = 1
      DO N  = 1,4

         N1   = NPC(N)
         N2   = NPC(N + 1)
         MAXP = NC
         IF (N == 1 .OR. N == 3) MAXP = MC

         TXO = DPO(N2) - DPO(N1) ; DXO = TXO/(MAXP-1)


         DP = DPO(N1) ; DPA = 0D0
         DO K = 1,MAXP
            DPA(K) = DP
            DP     = DP + DXO
         ENDDO
         IF (N == 3 .OR. N == 4) THEN
            CALL ANDERSOM(DPA, MAXP)
         ENDIF

         IF (MFO == 0) THEN
            IF (N == 1) THEN                                     ! COPY FROM FIRST SEGMENT
               DPA(1:MAXP) = DPO(1:MAXP)
            ELSE IF (N == 3) THEN                                ! REVERSED COPY FROM ALSO FIRST SEGMENT
               FF = TXO / ( DPO(NPC(2)) - DPO(NPC(1)) )
               DPA(1) = DPO(N2)
               DO K = 2,MAXP
                  DPOK1  = DPO(K) - DPO(1)
                  DPA(K) = DPA(1) - DPOK1*FF
               ENDDO
               K = K
            ENDIF
         ENDIF
         IF (NFO == 0) THEN
            IF (N == 2) THEN                                     ! REVERSED COPY FROM FOURTH SEGMENT
               K1 = NPC(5)
               FF = TXO / ( DPO(NPC(5)) - DPO(NPC(4)) )
               DPA(1) = DPO(N1)
               DO K = 2,MAXP
                  K1     = K1 - 1
                  DPOK1  = DPO(K1) - DPO(NPC(5))
                  DPA(K) = DPA(1)  - DPOK1*FF
               ENDDO
               K = K
            ELSE IF (N == 4) THEN                                ! REVERSED FOURTH SEGMENT
               DO K = 1,MAXP
                  DPA(K) = DPO(NPC(5) - K + 1)
               ENDDO
               K = K
            ENDIF
         ENDIF

         CALL maptoPolyline(XPO, YPO, DPO, NPO, XH(1,N), YH(1,N), DPA, MAXP) ! HAAL HUIDIGE PUNTEN OP

         CALL maptoPolyline(XPO, YPO, DPO, NPO, XPA(KA), YPA(KA), DPA, MAXP) ! HAAL HUIDIGE PUNTEN OP

         KA = KA + MAXP

      ENDDO

      ! NPA = KA-1
      ! XPL(1:NPA) = XPA(1:NPA)
      ! YPL(1:NPA) = YPA(1:NPA)
      ! NPL = NPA

      ! RETURN
                                                                      ! POLYG       TRANSF
      CALL TRANFN2( XH(1,4), XH(1,2), XH(1,1), XH(1,3),            &  ! . 3 .       . 4 .
                    YH(1,4), YH(1,2), YH(1,1), YH(1,3),            &  ! 4   2       1   2
                    MNMAX, MMAX, NMAX, XC, YC)                        ! . 1 .       . 3 .

      zc = 0d0 !zkuni



      NDRAW8ORG  = NDRAW(8) ; NDRAW(8) = 0
      IF (MFO .NE. 0 .AND. NFO .NE. 0) THEN
         ATPFO = ATPF ; ATPF = 0.
      ENDIF

      ! CALL ORTHOGRID(1,1,MC,NC)

      NDRAW(8)   = NDRAW8ORG
      IF (MFO .NE. 0 .AND. NFO .NE. 0) THEN
         ATPF = ATPFO
      ENDIF

      MFAC = MFO ; NFAC = NFO

      call newfil(mout, 'gridnow.grd')
      call WRIRGF(mout, 'gridnow.grd')

      !CALL GRIDTONET()

      !XC = DXYMIS; YC = DXYMIS; MC = 0 ; NC = 0

      CALL RESTOREPOL()

      DEALLOCATE (DPA, XPA, YPA, DPO, XPO, YPO, XH, YH)

      END SUBROUTINE curvilinearGRIDinpolygon
