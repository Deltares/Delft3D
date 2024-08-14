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

      subroutine ORTHOGRID(M1, N1, M2, N2)
         use unstruc_colors
         use M_GRID
         use M_SFERIC
         use M_GRIDSETTINGS
         use m_orthosettings
         implicit none
         integer :: in
         integer :: it
         integer :: jdla
         integer :: ma1
         integer :: ma2
         integer :: mcr
         integer :: mx
         integer :: na1
         integer :: na2
         integer :: ncr
         integer :: ndraw
         integer :: num
         integer :: nx
         double precision :: rjac

         double precision, dimension(:, :), allocatable :: XR, YR, XI2, XJ2, YI2, YJ2, &
            A, B, C, D, E, ATP, XO, YO

         integer :: M1, N1, M2, N2

         common / DRAWTHIS / ndraw(50)

         if (MC == 0) then
            call QNERROR('First Create or Load a Grid', ' ', ' ')
            NUM = 0
            return
         end if

         call SAVEgrd()

         MX = MMAX; NX = NMAX
         allocate (XR(MX, NX), YR(MX, NX), XI2(MX, NX), XJ2(MX, NX), YI2(MX, NX), YJ2(MX, NX), &
                   A(MX, NX), B(MX, NX), C(MX, NX), D(MX, NX), E(MX, NX), &
                   ATP(MX, NX), XO(MX, NX), YO(MX, NX))

         IN = 1
         PI = acos(-1d0)
         MCR = MC
         NCR = NC

         if (NDRAW(8) == 0) call READYY('ORTHOGONALISATION', 0d0)
         call ISITU()
         if (NDRAW(8) == 0) call READYY(' ', 0.05d0)

         if (JSFERIC == 1) call MAKEF(XC, YC, MMAX, NMAX)

         call GETSPL2(XC, XI2, XJ2, MC, NC, MMAX, NMAX)
         if (NDRAW(8) == 0) call READYY(' ', 0.10d0)

         call GETSPL2(YC, YI2, YJ2, MC, NC, MMAX, NMAX)
         if (NDRAW(8) == 0) call READYY(' ', 0.15d0)

         XR = XC
         YR = YC

         RJAC = 0.9d0
!     RJAC1 = (COS(PI/MCR) * (XM**2)*COS(PI/NCR)) / (1 + XM**2)
!     RJAC2 = 2*(COS(PI/MCR)/XM + COS(PI/NCR)) / (1 + 1/XM)
!     VUL DE COEFFICIENTEN-MATRICES
         do IT = 1, ITATP
            JDLA = 0
            if (IT == 1) JDLA = 1
            MA1 = max(1, M1 - 1)
            NA1 = max(1, N1 - 1)
            MA2 = min(MC - 1, M2)
            NA2 = min(NC - 1, N2)

            call ATPPAR(XR, YR, MA1, NA1, MA2, NA2, ATP, A, B, C, D, E, JDLA)

!        JAMMER IN DEZE LOOP, IJC WORDT EERST VERKLOOT IN SOMDIST
!        CALL SETINTERNALBOUNDARIES(IJC)
            call FIXDDBOUNDARIES()
            if (NDRAW(8) == 0) call READYY('ORTHOGONALISATION', 0.20d0)

            call ORTSOR(XR, YR, A, B, C, D, E, ATP, M1, N1, M2, N2, &
                        XI2, YI2, XJ2, YJ2, XO, YO, &
                        RJAC)
         end do

         if (NDRAW(8) == 0) call READYY('ORTHOGONALISATION', -1d0)

         XC = XR; YC = YR

         if (JSFERIC == 1) call MAKEY(XC, YC, MMAX, NMAX)
!     CALL TEKSHOW(X, Y, MA2, NA2, ATP, 2,'FINAL ATP')

         deallocate (XR, YR, XI2, XJ2, YI2, YJ2, A, B, C, D, E, ATP, XO, YO)

         return
      end subroutine ORTHOGRID
