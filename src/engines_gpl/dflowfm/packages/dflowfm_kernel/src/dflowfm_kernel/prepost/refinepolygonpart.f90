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

      !> Refine part of a polygon, indicated by start and end index.
      !! If the polygon/line ends between i1 and i2 (dmiss), then refinement
      !! stops there (i.e. refinement is only within *one* polygon).
      SUBROUTINE REFINEPOLYGONpart(i1, i2,jauniform) !DPLA = ACTUELE LENGTECOOR, DXA = ACTUELE GRIDSIZE, DXS = STREEF GRIDSIZE, ALLEN OP POLYGONPOINTS
      USE M_POLYGON
      USE M_MISSING
      use m_ec_triangle
      USE M_SAMPLES
      use m_alloc
      implicit none

      integer :: i1, i2
      integer, intent(in) :: jauniform !< use uniform spacing (1) or not (0)

      double precision :: dxs1
      double precision :: dxs2
      double precision :: dxsm
      integer :: ierr
      integer :: ja
      integer :: kk
      integer :: n, nmid
      integer :: nmn
      integer :: nmx
      integer :: no, nplo
      double precision :: rma, rmx
                                 !DPL  = IDEM, OORSPRONKELIJK

      DOUBLE PRECISION, ALLOCATABLE :: XPLO(:) ,YPLO(:), ZPLO(:), DPL (:)
      DOUBLE PRECISION, ALLOCATABLE :: XH  (:) ,  YH(:),   ZH(:), DPLA(:), DXA(:), DXS(:)

      DOUBLE PRECISION              :: TXS, TXA, RMN, THIRD,  TWOTHIRD
      INTEGER                       :: NX, JDLA

      JDLA  = 1
      THIRD = 1D0/3D0 ; TWOTHIRD = 1D0 - THIRD

      CALL SAVEPOL()

      i1 = max(1, min(i1,npl))
      i2 = max(1, min(i2,npl))

      if (i1 < i2) then
        ! Check whether *before* i2 there is already a dmiss
          NO = i2-i1+1 ! Nr of polygon points between i1 and i2 (including them)
          do kk=i1,i2
            if (xpl(kk) == dmiss) then
                NO = kk-i1 ! Nr of polygon points between i1 and i2 (including them)
                exit
            end if
          enddo
      else
        ! First flip i1<->i2 such that i1 < i2
        kk = i1
        i1 = i2
        i2 = kk
        ! Now, walk from i2 to i1 (=backwards) and check whether *before* i1 there is already a dmiss
        NO = i2-i1+1 ! Nr of polygon points between i1 and i2 (including them)
        do kk=i2,i1,-1
            if (xpl(kk) == dmiss) then
                NO = i2-kk  ! Nr of polygon points between i1 and i2 (including them)
                exit
            end if
        end do
      end if

      if ( jauniform.ne.1 ) then
         if (NO < 4 ) return
      else
         if ( NO.lt.2 ) return
      end if

      NPLO = NPL ! Back up current poly length
      NPL = NO
      NX = 10*NO

      ALLOCATE ( XPLO(NPLO), YPLO(NPLO), ZPLO(NPLO), DPL(NPLO) , STAT = IERR)
      CALL AERR('XPLO(NPLO), YPLO(NPLO), ZPLO(NPLO), DPL(NPLO)', IERR, 3*NPLO)
      do kk=i1,NPLO
        XPLO(kk-i1+1) = XPL(kk)
        YPLO(kk-i1+1) = YPL(kk)
        ZPLO(kk-i1+1) = ZPL(kk)
      end do


      ALLOCATE ( XH(NX), YH(NX) , ZH(NX) , STAT= IERR ) ; XH = DXYMIS ; YH = DXYMIS ; ZH = dxymis
      CALL AERR('XH(NX), YH(NX) , ZH(NX)', IERR, 2*NX )
      ALLOCATE ( DPLA(NX), DXA(NX), DXS(NX) , STAT = IERR)
      CALL AERR('DPLA(NX), DXA(NX), DXS(NX)', IERR,   3*NX)


      CALL accumulateDistance(XPLO, YPLO, DPL, NO)  ! OORSPRONKELIJKE LENGTECOORDINAAT
      CALL averageDiff       (DPL , DXA , NO)       ! OORSPRONKELIJKE SEGMENTSIZE

      if ( jauniform.ne.1 ) then
        DXS1        = 1d0*DXA(1)                      ! Start segment
        DXS2        = 1d0*DXA(NO)                     ! Eind segment
      else
         DXS1 = min(dxuni, DPL(NO))
         DXS2 = DXS1
      end if
      DPLA(1:NO)  = DPL(1:NO)
      TXA         = DPLA(NO)


      JA = 1
      DO WHILE (JA == 1)

         DO KK = 1,20

            CALL mapToPolyline(XPLO, YPLO, DPL, NO, XH, YH, DPLA, NPL) ! HAAL HUIDIGE PUNTEN OP
           ! CALL DISP2C(dble(XH), dble(YH), NPL, 0.5*RCIR, 50+8*KK)
           ! CALL WAITESC()
            CALL averageDiff(DPLA, DXA , NPL)                  ! GET ACTUELE GRIDSIZE
            DXS = DXYMIS
            !IF (NS .GE. 3) THEN                                 ! ALS ER SAMPLES ZIJN, DAN ZIJN ZE HET GRIDSIZE CONTROL FIELD
            !   NPH = NPL ; NPL = 0
            !   CALL INTDXSTRI(XH,YH,DXS,NPH,JDLA)
            !   NPL = NPH                                        ! ROEIEN OMHEEN DE NPL CONSTRUCTIE IN TRIINT
            !ELSE
               CALL interpOnPolyline(DPLA, DXS, NPL, DXS1, DXS2)! TRIANGLESIZE, TRIANGLESIZE)      ! INTERPOLATE STREEFGRIDSIZE ! LATER TRIANGULATIE
            !ENDIF
            DO N = 1,NPL                                        ! EN VOOR DE VEILIGHEID:
               IF (DXS(N) == DXYMIS) THEN
                  DXS(N) = DXA(N)
               ENDIF
            ENDDO
            TXS = SUM(DXS(1:NPL)) - 0.5D0*( DXS(1)+DXS(NPL) )   ! Som van gewenste delta xjes

            CALL SMODPLA(DPLA, DXS, NPL)                 ! SMOOTH WITH WEIGHTFACTOR DESIRED
         ENDDO


         RMN =  1E9 ; NMN = 0
         RMX = -1E9 ; NMX = 0; DXSM = 1E30
         DO N  = 1,NPL-1                                 ! CHECK SMALLEST AND LARGEST RATIOS OF ACTUAL VS DESIRED
            DXSM = MIN(DXS(N), DXSM)

            RMA   = DXA(N)/DXS(N)

            IF (N > 1)  THEN
               IF (RMA < RMN) THEN                        ! ZOEK BESTE WEGGOOIER
                  NMN = N ; RMN = RMA                     ! POTENTIEEL WEGGOOIPUNT, KLEINE GRIDSIZE VS STREEFSIZE
               ENDIF
            ENDIF
            IF (RMA > RMX) THEN
              NMX = N ; RMX = RMA                         ! POTENTIEEL BIJZETPUNT, GROTE GRIDSIZE VS STREEFSIZE
            ENDIF
         ENDDO


         IF  (NMN .NE. 0 .AND. TXS-1.5d0*DXS(max(NMN,1)) > TXA) THEN  ! TOT STREEFLENGTE MIN KLEINSTE STREEF LENGTE GROTER DAN TOTLENGTE
                                                         ! => KLEINSTE VERWIJDEREN
            NPL  = NPL - 1
            DO N = NMN, NPL
               DPLA(N) = DPLA(N+1)
            ENDDO
            JA = 1

         ELSE IF (TXS + 0.5d0*DXA(NMX) < TXA) THEN         ! TOT STREEFLENGTE PLUS HALVE GROOTSTE KLEINER DAN TOTLENGTE
                                                         ! => BIJZETTEN BIJ DE GROOTSTE
            NPL  = NPL + 1
            IF (NPL > NX) THEN
               NX = 1.5*NX
               CALL REALLOC(XH  , NX)
               CALL REALLOC(YH  , NX)
               CALL REALLOC(ZH  , NX)
               CALL REALLOC(DPLA, NX)
               CALL REALLOC(DXA , NX)
               CALL REALLOC(DXS , NX)
            ENDIF

            DO N = NPL, NMX + 2, -1
               DPLA(N) = DPLA(N-1)
            ENDDO

            DPLA(NMX+1) = 0.5d0*( DPLA(NMX) + DPLA(NMX+2) )

            JA = 1
         ELSE
            JA = 0
         ENDIF

      ENDDO


      IF (NPL+i1-1+nplo-i2 > SIZE(XPL) ) THEN
         NX = 1.5*(NPL+i1-1+nplo-i2)
         CALL REALLOC(XPL , NX)
         CALL REALLOC(YPL , NX)
         CALL REALLOC(ZPL , NX)
         CALL REALLOC(XPH , NX)
         CALL REALLOC(YPH , NX)
         CALL REALLOC(ZPH , NX)
         MAXPOL = NX
      ENDIF
      do kk=nplo,i2+1,-1
        XPL(i1+npl+kk-i2-1) = XPL(kk)
        YPL(i1+npl+kk-i2-1) = YPL(kk)
        ZPL(i1+npl+kk-i2-1) = ZPL(kk)
      end do
      do kk=1,NPL
        XPL(i1+kk-1) = XH(kk)
        YPL(i1+kk-1) = YH(kk)
        ZPL(i1+kk-1) = ZH(kk)
      end do
!     SPvdP: copy remaining part of original polygon
      do kk=1,NPLO-i1-NO+1
         XPL(i1+NPL+kk-1) = XPLO(NO+kk)
         YPL(i1+NPL+kk-1) = YPLO(NO+kk)
         ZPL(i1+NPL+kk-1) = ZPLO(NO+kk)
      end do

      NPL=NPLO-NO+NPL

      DEALLOCATE (XPLO, YPLO, ZPLO, DPL, XH, YH, ZH, DPLA, DXA, DXS)

      END SUBROUTINE REFINEPOLYGONpart
