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

!*******************  BOUNDARY TREATMENT *****************************
module m_bndsmt
use m_dismin, only: dismin

implicit none

private

public :: bndsmt

contains

      subroutine BNDSMT(XR, YR, XI2, YI2, XJ2, YJ2, ATP, M1, N1, M2, N2)
         use precision, only: dp
         use m_grid
         use m_gridsettings
         use m_get_ij

         real(kind=dp) :: bfe
         integer :: i
         integer :: iff
         integer :: ifr
         integer :: il
         integer :: ilr
         integer :: in
         integer :: int
         integer :: ir
         integer :: irr
         integer :: j
         integer :: jf
         integer :: jfr
         integer :: jl
         integer :: jlr
         integer :: jr
         integer :: jrr
         integer :: kc
         integer :: m1
         integer :: m2
         integer :: n1
         integer :: n2
         integer :: num
         real(kind=dp) :: qb
         real(kind=dp) :: qbc
         real(kind=dp) :: qc
         real(kind=dp) :: rn
         real(kind=dp) :: x0
         real(kind=dp) :: x1
         real(kind=dp) :: x2
         real(kind=dp) :: x3
         real(kind=dp) :: y0
         real(kind=dp) :: y1
         real(kind=dp) :: y2
         real(kind=dp) :: y3
!     RANDPUNTEN OF INTERNE PUNTEN
!     TERUGZETTEN OP DE SPLINE TUSSEN OUDE POSITIE OP RAND (BFAC = 0)
!     EN PROJECTIE OP SPLINE VAN NABIJ PUNT (BFAC = 1)
!     BIJ NCODE IS 5, INT(ERNAL) HORIZONTAAL, 6 = VERTICAAL
         real(kind=dp) :: XR(MMAX, NMAX), YR(MMAX, NMAX), &
                          XI2(MMAX, NMAX), XJ2(MMAX, NMAX), ATP(MMAX, NMAX), &
                          YI2(MMAX, NMAX), YJ2(MMAX, NMAX)

         real(kind=dp), allocatable :: XH(:), YH(:), XH2(:), YH2(:)
         real(kind=dp) :: XX1, XX2, YY1, YY2, TV, XV, YV, XV2, YV2, DIS

         allocate (XH(MNMAX), YH(MNMAX), XH2(MNMAX), YH2(MNMAX))

         if (BFAC == 0) return
         BFE = 1 - BFAC

!     DE HORIZONTALEN
         do JR = 1, NC
            IN = 0
            INT = 0
            J = JR
            do IR = 1, MC
               KC = abs(IJC(IR, JR))
               if (KC == 11 .or. KC == 14) then
                  IFR = IR + 1
                  IFF = IR
               else if (KC == 1) then
                  IN = 1
               else if (KC == 3) then
                  IN = -1
               else if (KC == 5) then
                  IN = 1
                  INT = 1
               else if (KC == 12 .or. KC == 13 .and. IN /= 0) then
                  ILR = IR - 1
                  IL = IR
                  NUM = IL - IFF + 1
                  call GETIJ(XC, XH, MMAX, NMAX, MNMAX, IFF, IL, J, J)
                  call GETIJ(XJ2, XH2, MMAX, NMAX, MNMAX, IFF, IL, J, J)
                  call GETIJ(YC, YH, MMAX, NMAX, MNMAX, IFF, IL, J, J)
                  call GETIJ(YJ2, YH2, MMAX, NMAX, MNMAX, IFF, IL, J, J)

                  do IRR = IFR, ILR
                     if (IRR >= M1 .and. IRR <= M2 .and. &
                         JR >= N1 .and. JR <= N2 .and. &
                         IJC(IRR, JR) > 0) then
                        XX1 = XR(IRR, JR + IN)
                        YY1 = YR(IRR, JR + IN)
                        X0 = XR(IRR, JR)
                        Y0 = YR(IRR, JR)
                        XX1 = XX1 * BFAC + X0 * BFE
                        YY1 = YY1 * BFAC + Y0 * BFE
                        TV = IRR - IFF
                        if (IN == 1) then
!                       onder
                           X1 = XR(IRR - 1, JR)
                           X3 = XR(IRR + 1, JR)
                           X2 = XR(IRR, JR + 1)
                           Y1 = YR(IRR - 1, JR)
                           Y3 = YR(IRR + 1, JR)
                           Y2 = YR(IRR, JR + 1)
                           QB = ATP(IRR - 1, JR)
                           QC = ATP(IRR, JR)
                           QBC = 1d0 / QB + 1d0 / QC
                           RN = QB + QC + QBC
                           XX1 = (QB * X1 + QBC * X2 + QC * X3 + Y3 - Y1) / RN
                           YY1 = (QB * Y1 + QBC * Y2 + QC * Y3 + X1 - X3) / RN
                        else if (IN == -1) then
!                       boven
                           X1 = XR(IRR - 1, JR)
                           X3 = XR(IRR + 1, JR)
                           X2 = XR(IRR, JR - 1)
                           Y1 = YR(IRR - 1, JR)
                           Y3 = YR(IRR + 1, JR)
                           Y2 = YR(IRR, JR - 1)
                           QB = ATP(IRR - 1, JR - 1)
                           QC = ATP(IRR, JR - 1)
                           QBC = 1d0 / QB + 1d0 / QC
                           RN = QB + QC + QBC
                           XX1 = (QB * X1 + QBC * X2 + QC * X3 + Y1 - Y3) / RN
                           YY1 = (QB * Y1 + QBC * Y2 + QC * Y3 + X3 - X1) / RN
                        end if
!                    CALL RCIRC(XX1,YY1)
                        call DISMIN(XH, XH2, YH, YH2, XX1, YY1, NUM, DIS, TV, XV, YV)
                        if (INT == 1) then
!                       for internal boundary points
                           XX2 = XR(IRR, JR - IN)
                           YY2 = YR(IRR, JR - IN)
                           XX2 = XX2 * BFAC + X0 * BFE
                           YY2 = YY2 * BFAC + Y0 * BFE
                           call DISMIN(XH, XH2, YH, YH2, XX2, YY2, NUM, DIS, TV, XV2, YV2)
                           XV = (XV + XV2) / 2
                           YV = (YV + YV2) / 2
                        end if
                        XR(IRR, JR) = XV
                        YR(IRR, JR) = YV
                     end if
                  end do
                  IN = 0
                  INT = 0
               end if
            end do
         end do

!     CALL WAITESC()

!     DE VERTICALEN
         do IR = 1, MC
            IN = 0
            INT = 0
            I = IR
            do JR = 1, NC
               KC = abs(IJC(IR, JR))
               if (KC == 11 .or. KC == 12) then
                  JFR = JR + 1
                  JF = JR
               else if (KC == 4) then
                  IN = 1
               else if (KC == 2) then
                  IN = -1
               else if (KC == 6) then
                  IN = 1
                  INT = 1
               else if (KC == 14 .or. KC == 13 .and. IN /= 0) then
                  JLR = JR - 1
                  JL = JR
                  NUM = JL - JF + 1
                  call GETIJ(XC, XH, MMAX, NMAX, MNMAX, I, I, JF, JL)
                  call GETIJ(XI2, XH2, MMAX, NMAX, MNMAX, I, I, JF, JL)
                  call GETIJ(YC, YH, MMAX, NMAX, MNMAX, I, I, JF, JL)
                  call GETIJ(YI2, YH2, MMAX, NMAX, MNMAX, I, I, JF, JL)

                  do JRR = JFR, JLR
                     if (JRR >= N1 .and. JRR <= N2 .and. &
                         IR >= M1 .and. IR <= M2 .and. &
                         IJC(IR, JRR) > 0) then
                        XX1 = XR(IR + IN, JRR)
                        YY1 = YR(IR + IN, JRR)
                        X0 = XR(IR, JRR)
                        Y0 = YR(IR, JRR)
                        XX1 = XX1 * BFAC + X0 * BFE
                        YY1 = YY1 * BFAC + Y0 * BFE
                        TV = JRR - JF
                        if (IN == 1) then
!                       links
                           X1 = XR(IR, JRR - 1)
                           X3 = XR(IR, JRR + 1)
                           X2 = XR(IR + 1, JRR)
                           Y1 = YR(IR, JRR - 1)
                           Y3 = YR(IR, JRR + 1)
                           Y2 = YR(IR + 1, JRR)
                           QC = 1d0 / ATP(IR, JRR)
                           QB = 1d0 / ATP(IR, JRR - 1)
                           QBC = 1d0 / QB + 1d0 / QC
                           RN = QB + QC + QBC
                           XX1 = (QB * X1 + QBC * X2 + QC * X3 + Y1 - Y3) / RN
                           YY1 = (QB * Y1 + QBC * Y2 + QC * Y3 + X3 - X1) / RN
                        else if (IN == -1) then
!                       rechts
                           X1 = XR(IR, JRR - 1)
                           X3 = XR(IR, JRR + 1)
                           X2 = XR(IR - 1, JRR)
                           Y1 = YR(IR, JRR - 1)
                           Y3 = YR(IR, JRR + 1)
                           Y2 = YR(IR - 1, JRR)
                           QC = 1d0 / ATP(IR - 1, JRR)
                           QB = 1d0 / ATP(IR - 1, JRR - 1)
                           QBC = 1d0 / QB + 1d0 / QC
                           RN = QB + QC + QBC
                           XX1 = (QB * X1 + QBC * X2 + QC * X3 + Y3 - Y1) / RN
                           YY1 = (QB * Y1 + QBC * Y2 + QC * Y3 + X1 - X3) / RN
                        end if
!                    CALL RCIRC(XX1,YY1)
                        call DISMIN(XH, XH2, YH, YH2, XX1, YY1, NUM, DIS, TV, XV, YV)
                        if (INT == 1) then
!                       for internal boundary points
                           XX2 = XR(IR - IN, JRR)
                           YY2 = YR(IR - IN, JRR)
                           XX2 = XX2 * BFAC + X0 * BFE
                           YY2 = YY2 * BFAC + Y0 * BFE
                           call DISMIN(XH, XH2, YH, YH2, XX2, YY2, NUM, DIS, TV, XV2, YV2)
                           XV = (XV + XV2) / 2
                           YV = (YV + YV2) / 2
                        end if
                        XR(IR, JRR) = XV
                        YR(IR, JRR) = YV
                     end if
                  end do

                  IN = 0
                  INT = 0
               end if
            end do
         end do

         deallocate (XH, YH, XH2, YH2)

         return
      end subroutine BNDSMT

end module m_bndsmt
