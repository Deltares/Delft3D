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
      subroutine curvilinearGRIDinpolygon()
         use M_POLYGON
         use M_SAMPLES
         use M_GRID
         use M_GRIDSETTINGS
         use m_orthosettings
         use m_missing
         use m_netw
         use m_sferic, only: jsferic, jasfer3D
         use geometry_module, only: dcosphi
         use m_drawthis
         use m_qnerror
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
         integer :: ndraw8org
         integer :: nfo
         integer :: npo
         integer :: nr

         double precision, allocatable :: XH(:, :), YH(:, :)

         double precision, allocatable :: XPA(:), YPA(:), DPA(:)
         double precision, allocatable :: XPO(:), YPO(:), DPO(:)

         double precision :: TXO, DXO, PRIN
         integer :: MNX, MAXP
         integer :: npc(5)
         integer :: ierror

         if (npl < 4) return

!     create O-type pillar grid if the pillar radius .ne. 0d0
         if (pil_rad /= 0d0) then
            call pillargrid(ierror)
            if (ierror == 0) return ! otherwise, generate non-pillar grid
         end if

         call SAVEPOL()

         do K = 1, NPL
            if (XPL(K) /= xymis) then
               KM = K
            else
               exit
            end if
         end do
         NPL = KM

         if (XPL(1) /= XPL(NPL)) then
            NPL = NPL + 1
            XPL(NPL) = XPL(1)
            YPL(NPL) = YPL(1)
         end if

         NPO = NPL
         allocate (DPO(NPO), XPO(NPO), YPO(NPO), STAT=IERR); DPO = 0d0
         call AERR('DPO(NPO) , XPO(NPO), YPO(NPO)', IERR, NPO)
         XPO(1:NPO) = XPL(1:NPO)
         YPO(1:NPO) = YPL(1:NPO)

         NR = 1 ! FIRST
         NPC(NR) = 1

         !CALL SETCOL(31)
         !CALL RCIRC ( XPL(1), YPL(1) )

         do N = 2, NPL - 1
            prin = dcosphi(XPO(N - 1), YPO(N - 1), XPO(N), YPO(N), &
                           XPO(N), YPO(N), XPO(N + 1), YPO(N + 1), jsferic, jasfer3D, dxymis)
            prin = abs(prin)
            if (PRIN < 0.5d0) then
               call RCIRC(XPL(1), YPL(1))
               NR = NR + 1
               if (NR <= 4) then
                  NPC(NR) = N
               end if
            end if

         end do

         if (NR < 4) then
            call QNERROR('LESS THAN FOUR CORNERS FOUND', ' ', ' ')
            call RESTOREPOL()
            deallocate (DPO, XPO, YPO)
            return
         else if (NR > 4) then
            call QNERROR('MORE THAN 4 CORNERS FOUND', ' ', ' ')
            call RESTOREPOL()
            deallocate (DPO, XPO, YPO)
            return
         end if

         NR = NR + 1
         NPC(NR) = NPL

         MFO = MFAC; NFO = NFAC
         MC = MFAC + 1
         NC = NFAC + 1

         if (MFO == 0) then
            MC = NPC(2) - NPC(1) + 1; MFAC = MC - 1
            JAM = 1
         end if

         if (NFO == 0) then
            NC = NPC(5) - NPC(4) + 1; NFAC = NC - 1
            JAN = 1
         end if

         call INCREASEGRID(MC, NC)

         MNX = 5 * max(MC, NC)
         allocate (XH(MNX, 4), YH(MNX, 4))

         allocate (DPA(MNX), XPA(MNX), YPA(MNX), STAT=IERR); DPA = 0d0
         call AERR('DPA(MNX) , XPA(MNX), YPA(MNX)', IERR, MNX)

         call accumulateDistance(XPO, YPO, DPO, NPO) ! OORSPRONKELIJKE LENGTECOORDINAAT

         KA = 1
         do N = 1, 4

            N1 = NPC(N)
            N2 = NPC(N + 1)
            MAXP = NC
            if (N == 1 .or. N == 3) MAXP = MC

            TXO = DPO(N2) - DPO(N1); DXO = TXO / (MAXP - 1)

            DP = DPO(N1); DPA = 0d0
            do K = 1, MAXP
               DPA(K) = DP
               DP = DP + DXO
            end do
            if (N == 3 .or. N == 4) then
               call ANDERSOM(DPA, MAXP)
            end if

            if (MFO == 0) then
               if (N == 1) then ! COPY FROM FIRST SEGMENT
                  DPA(1:MAXP) = DPO(1:MAXP)
               else if (N == 3) then ! REVERSED COPY FROM ALSO FIRST SEGMENT
                  FF = TXO / (DPO(NPC(2)) - DPO(NPC(1)))
                  DPA(1) = DPO(N2)
                  do K = 2, MAXP
                     DPOK1 = DPO(K) - DPO(1)
                     DPA(K) = DPA(1) - DPOK1 * FF
                  end do
                  K = K
               end if
            end if
            if (NFO == 0) then
               if (N == 2) then ! REVERSED COPY FROM FOURTH SEGMENT
                  K1 = NPC(5)
                  FF = TXO / (DPO(NPC(5)) - DPO(NPC(4)))
                  DPA(1) = DPO(N1)
                  do K = 2, MAXP
                     K1 = K1 - 1
                     DPOK1 = DPO(K1) - DPO(NPC(5))
                     DPA(K) = DPA(1) - DPOK1 * FF
                  end do
                  K = K
               else if (N == 4) then ! REVERSED FOURTH SEGMENT
                  do K = 1, MAXP
                     DPA(K) = DPO(NPC(5) - K + 1)
                  end do
                  K = K
               end if
            end if

            call maptoPolyline(XPO, YPO, DPO, NPO, XH(1, N), YH(1, N), DPA, MAXP) ! HAAL HUIDIGE PUNTEN OP

            call maptoPolyline(XPO, YPO, DPO, NPO, XPA(KA), YPA(KA), DPA, MAXP) ! HAAL HUIDIGE PUNTEN OP

            KA = KA + MAXP

         end do

         ! NPA = KA-1
         ! XPL(1:NPA) = XPA(1:NPA)
         ! YPL(1:NPA) = YPA(1:NPA)
         ! NPL = NPA

         ! RETURN
         ! POLYG       TRANSF
         call TRANFN2(XH(1, 4), XH(1, 2), XH(1, 1), XH(1, 3), & ! . 3 .       . 4 .
                      YH(1, 4), YH(1, 2), YH(1, 1), YH(1, 3), & ! 4   2       1   2
                      MNMAX, MMAX, NMAX, XC, YC) ! . 1 .       . 3 .

         zc = 0d0 !zkuni

         NDRAW8ORG = NDRAW(8); NDRAW(8) = 0
         if (MFO /= 0 .and. NFO /= 0) then
            ATPFO = ATPF; ATPF = 0.
         end if

         ! CALL ORTHOGRID(1,1,MC,NC)

         NDRAW(8) = NDRAW8ORG
         if (MFO /= 0 .and. NFO /= 0) then
            ATPF = ATPFO
         end if

         MFAC = MFO; NFAC = NFO

         call newfil(mout, 'gridnow.grd')
         call WRIRGF(mout, 'gridnow.grd')

         !CALL GRIDTONET()

         !XC = DXYMIS; YC = DXYMIS; MC = 0 ; NC = 0

         call RESTOREPOL()

         deallocate (DPA, XPA, YPA, DPO, XPO, YPO, XH, YH)

      end subroutine curvilinearGRIDinpolygon
