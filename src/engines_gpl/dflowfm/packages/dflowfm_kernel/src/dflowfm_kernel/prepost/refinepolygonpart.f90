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
      subroutine REFINEPOLYGONpart(i1, i2, jauniform) !DPLA = ACTUELE LENGTECOOR, DXA = ACTUELE GRIDSIZE, DXS = STREEF GRIDSIZE, ALLEN OP POLYGONPOINTS
         use M_POLYGON
         use M_MISSING
         use m_ec_triangle
         use M_SAMPLES
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
         integer :: n
         integer :: nmn
         integer :: nmx
         integer :: no, nplo
         double precision :: rma, rmx
         !DPL  = IDEM, OORSPRONKELIJK

         double precision, allocatable :: XPLO(:), YPLO(:), ZPLO(:), DPL(:)
         double precision, allocatable :: XH(:), YH(:), ZH(:), DPLA(:), DXA(:), DXS(:)

         double precision :: TXS, TXA, RMN, THIRD, TWOTHIRD
         integer :: NX, JDLA

         JDLA = 1
         THIRD = 1d0 / 3d0; TWOTHIRD = 1d0 - THIRD

         call SAVEPOL()

         i1 = max(1, min(i1, npl))
         i2 = max(1, min(i2, npl))

         if (i1 < i2) then
            ! Check whether *before* i2 there is already a dmiss
            NO = i2 - i1 + 1 ! Nr of polygon points between i1 and i2 (including them)
            do kk = i1, i2
               if (xpl(kk) == dmiss) then
                  NO = kk - i1 ! Nr of polygon points between i1 and i2 (including them)
                  exit
               end if
            end do
         else
            ! First flip i1<->i2 such that i1 < i2
            kk = i1
            i1 = i2
            i2 = kk
            ! Now, walk from i2 to i1 (=backwards) and check whether *before* i1 there is already a dmiss
            NO = i2 - i1 + 1 ! Nr of polygon points between i1 and i2 (including them)
            do kk = i2, i1, -1
               if (xpl(kk) == dmiss) then
                  NO = i2 - kk ! Nr of polygon points between i1 and i2 (including them)
                  exit
               end if
            end do
         end if

         if (jauniform /= 1) then
            if (NO < 4) return
         else
            if (NO < 2) return
         end if

         NPLO = NPL ! Back up current poly length
         NPL = NO
         NX = 10 * NO

         allocate (XPLO(NPLO), YPLO(NPLO), ZPLO(NPLO), DPL(NPLO), STAT=IERR)
         call AERR('XPLO(NPLO), YPLO(NPLO), ZPLO(NPLO), DPL(NPLO)', IERR, 3 * NPLO)
         do kk = i1, NPLO
            XPLO(kk - i1 + 1) = XPL(kk)
            YPLO(kk - i1 + 1) = YPL(kk)
            ZPLO(kk - i1 + 1) = ZPL(kk)
         end do

         allocate (XH(NX), YH(NX), ZH(NX), STAT=IERR); XH = DXYMIS; YH = DXYMIS; ZH = dxymis
         call AERR('XH(NX), YH(NX) , ZH(NX)', IERR, 2 * NX)
         allocate (DPLA(NX), DXA(NX), DXS(NX), STAT=IERR)
         call AERR('DPLA(NX), DXA(NX), DXS(NX)', IERR, 3 * NX)

         call accumulateDistance(XPLO, YPLO, DPL, NO) ! OORSPRONKELIJKE LENGTECOORDINAAT
         call averageDiff(DPL, DXA, NO) ! OORSPRONKELIJKE SEGMENTSIZE

         if (jauniform /= 1) then
            DXS1 = 1d0 * DXA(1) ! Start segment
            DXS2 = 1d0 * DXA(NO) ! Eind segment
         else
            DXS1 = min(dxuni, DPL(NO))
            DXS2 = DXS1
         end if
         DPLA(1:NO) = DPL(1:NO)
         TXA = DPLA(NO)

         JA = 1
         do while (JA == 1)

            do KK = 1, 20

               call mapToPolyline(XPLO, YPLO, DPL, NO, XH, YH, DPLA, NPL) ! HAAL HUIDIGE PUNTEN OP
               ! CALL DISP2C(dble(XH), dble(YH), NPL, 0.5*RCIR, 50+8*KK)
               ! CALL WAITESC()
               call averageDiff(DPLA, DXA, NPL) ! GET ACTUELE GRIDSIZE
               DXS = DXYMIS
               !IF (NS .GE. 3) THEN                                 ! ALS ER SAMPLES ZIJN, DAN ZIJN ZE HET GRIDSIZE CONTROL FIELD
               !   NPH = NPL ; NPL = 0
               !   CALL INTDXSTRI(XH,YH,DXS,NPH,JDLA)
               !   NPL = NPH                                        ! ROEIEN OMHEEN DE NPL CONSTRUCTIE IN TRIINT
               !ELSE
               call interpOnPolyline(DPLA, DXS, NPL, DXS1, DXS2) ! TRIANGLESIZE, TRIANGLESIZE)      ! INTERPOLATE STREEFGRIDSIZE ! LATER TRIANGULATIE
               !ENDIF
               do N = 1, NPL ! EN VOOR DE VEILIGHEID:
                  if (DXS(N) == DXYMIS) then
                     DXS(N) = DXA(N)
                  end if
               end do
               TXS = sum(DXS(1:NPL)) - 0.5d0 * (DXS(1) + DXS(NPL)) ! Som van gewenste delta xjes

               call SMODPLA(DPLA, DXS, NPL) ! SMOOTH WITH WEIGHTFACTOR DESIRED
            end do

            RMN = 1e9; NMN = 0
            RMX = -1e9; NMX = 0; DXSM = 1e30
            do N = 1, NPL - 1 ! CHECK SMALLEST AND LARGEST RATIOS OF ACTUAL VS DESIRED
               DXSM = min(DXS(N), DXSM)

               RMA = DXA(N) / DXS(N)

               if (N > 1) then
                  if (RMA < RMN) then ! ZOEK BESTE WEGGOOIER
                     NMN = N; RMN = RMA ! POTENTIEEL WEGGOOIPUNT, KLEINE GRIDSIZE VS STREEFSIZE
                  end if
               end if
               if (RMA > RMX) then
                  NMX = N; RMX = RMA ! POTENTIEEL BIJZETPUNT, GROTE GRIDSIZE VS STREEFSIZE
               end if
            end do

            if (NMN /= 0 .and. TXS - 1.5d0 * DXS(max(NMN, 1)) > TXA) then ! TOT STREEFLENGTE MIN KLEINSTE STREEF LENGTE GROTER DAN TOTLENGTE
               ! => KLEINSTE VERWIJDEREN
               NPL = NPL - 1
               do N = NMN, NPL
                  DPLA(N) = DPLA(N + 1)
               end do
               JA = 1

            else if (TXS + 0.5d0 * DXA(NMX) < TXA) then ! TOT STREEFLENGTE PLUS HALVE GROOTSTE KLEINER DAN TOTLENGTE
               ! => BIJZETTEN BIJ DE GROOTSTE
               NPL = NPL + 1
               if (NPL > NX) then
                  NX = 1.5 * NX
                  call REALLOC(XH, NX)
                  call REALLOC(YH, NX)
                  call REALLOC(ZH, NX)
                  call REALLOC(DPLA, NX)
                  call REALLOC(DXA, NX)
                  call REALLOC(DXS, NX)
               end if

               do N = NPL, NMX + 2, -1
                  DPLA(N) = DPLA(N - 1)
               end do

               DPLA(NMX + 1) = 0.5d0 * (DPLA(NMX) + DPLA(NMX + 2))

               JA = 1
            else
               JA = 0
            end if

         end do

         if (NPL + i1 - 1 + nplo - i2 > size(XPL)) then
            NX = 1.5 * (NPL + i1 - 1 + nplo - i2)
            call REALLOC(XPL, NX)
            call REALLOC(YPL, NX)
            call REALLOC(ZPL, NX)
            call REALLOC(XPH, NX)
            call REALLOC(YPH, NX)
            call REALLOC(ZPH, NX)
            MAXPOL = NX
         end if
         do kk = nplo, i2 + 1, -1
            XPL(i1 + npl + kk - i2 - 1) = XPL(kk)
            YPL(i1 + npl + kk - i2 - 1) = YPL(kk)
            ZPL(i1 + npl + kk - i2 - 1) = ZPL(kk)
         end do
         do kk = 1, NPL
            XPL(i1 + kk - 1) = XH(kk)
            YPL(i1 + kk - 1) = YH(kk)
            ZPL(i1 + kk - 1) = ZH(kk)
         end do
!     SPvdP: copy remaining part of original polygon
         do kk = 1, NPLO - i1 - NO + 1
            XPL(i1 + NPL + kk - 1) = XPLO(NO + kk)
            YPL(i1 + NPL + kk - 1) = YPLO(NO + kk)
            ZPL(i1 + NPL + kk - 1) = ZPLO(NO + kk)
         end do

         NPL = NPLO - NO + NPL

         deallocate (XPLO, YPLO, ZPLO, DPL, XH, YH, ZH, DPLA, DXA, DXS)

      end subroutine REFINEPOLYGONpart
