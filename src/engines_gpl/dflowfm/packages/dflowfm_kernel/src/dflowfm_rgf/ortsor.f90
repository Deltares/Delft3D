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

      subroutine ORTSOR(XR, YR, A, B, C, D, E, ATP, M1, N1, M2, N2, &
                        XI2, YI2, XJ2, YJ2, XO, YO, &
                        RJAC)
         use precision, only: dp
         use unstruc_colors
         use m_sferic
         use m_grid
         use m_gridsettings
         use m_orthosettings
         use m_drawthis
         use m_readyy
         use m_tek_grd
         implicit none
         integer :: i
         integer :: key
         integer :: m1
         integer :: m2
         integer :: n1
         integer :: n2
         real(kind=dp) :: rjac

         real(kind=dp) :: XR(MMAX, NMAX), YR(MMAX, NMAX), &
                          XI2(MMAX, NMAX), XJ2(MMAX, NMAX), &
                          YI2(MMAX, NMAX), YJ2(MMAX, NMAX), &
                          XO(MMAX, NMAX), YO(MMAX, NMAX), &
                          A(MMAX, NMAX), B(MMAX, NMAX), C(MMAX, NMAX), &
                          D(MMAX, NMAX), E(MMAX, NMAX), ATP(MMAX, NMAX)

         do I = 1, ITBND

            if (NDRAW(8) == 0) call READYY(' ', 0.25d0 + 0.75d0 * (dble(I - 1 + 0.20d0) / dble(ITBND)))
            call SOR(A, B, C, D, E, XR, RJAC, M1, N1, M2, N2)

            if (NDRAW(8) == 0) call READYY(' ', 0.25d0 + 0.75d0 * (dble(I - 1 + 0.60d0) / dble(ITBND)))
            call SOR(A, B, C, D, E, YR, RJAC, M1, N1, M2, N2)

            if (NDRAW(8) == 0) call READYY(' ', 0.25d0 + 0.75d0 * (dble(I - 1 + 1.00d0) / dble(ITBND)))

            call BNDSMT(XR, YR, XI2, YI2, XJ2, YJ2, ATP, M1, N1, M2, N2)

            if (NDRAW(8) == 1 .and. MDESIGN /= 5) then
               if (JSFERIC == 1) then
                  call MAKEY2(XR, YR, XO, YO, MMAX, NMAX)
                  call TEKGRD(XO, YO, MMAX, NMAX, M1, N1, M2, N2, NCOLDG, NDRAW(38), KEY, MC)
               else
                  call TEKGRD(XR, YR, MMAX, NMAX, M1, N1, M2, N2, NCOLDG, NDRAW(38), KEY, MC)
               end if
            end if
         end do
         return
      end subroutine ORTSOR
