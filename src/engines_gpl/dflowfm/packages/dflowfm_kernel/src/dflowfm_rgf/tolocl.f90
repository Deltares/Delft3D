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

      subroutine TOLOCL(DX0, DY0, X, Y, mmax, nmax, MP, NP, NTO)
         use m_missing
         use m_sferic
         implicit none
!     TRANSFORMEER NAAR LOCALE OF GLOBALE SYSTEEM IN EEN GRID
!     WEEG TUSSEN MAX(!) VIER MOGELIJKE MEDE RICHTINGBEPALENDE OMLIGGEND
!     NTO = 0 IS NAAR LOKAAL, NTO = 1 IS NAAR GLOBAAL
         integer :: mmax, nmax, mp, np, nto
         double precision :: X(MMAX, NMAX), Y(MMAX, NMAX)

         double precision :: x0, y0, dx0, dy0, x1t, y1t, x1p, x2p, x2t, y2t, xn1, xn2, dx, dy

         X0 = X(MP, NP)
         Y0 = Y(MP, NP)
         X1T = 0
         Y1T = 0
         XN1 = 0
         X2T = 0
         Y2T = 0
         XN2 = 0
         if (MP /= MMAX) then
            X1P = X(MP + 1, NP)
            if (X1P /= XYMIS) then
               X1T = X1P - X0
               Y1T = Y(MP + 1, NP) - Y0
            end if
         end if
         if (MP /= 1) then
            X1P = X(MP - 1, NP)
            if (X1P /= XYMIS) then
               X1T = X1T - X1P + X0
               Y1T = Y1T - Y(MP - 1, NP) + Y0
            end if
         end if

         if (JSFERIC == 1) X1T = X1T * cos(DG2RD * Y0)

         if (NP /= NMAX) then
            X2P = X(MP, NP + 1)
            if (X2P /= XYMIS) then
               X2T = X2P - X0
               Y2T = Y(MP, NP + 1) - Y0
            end if
         end if
         if (NP /= 1) then
            X2P = X(MP, NP - 1)
            if (X2P /= XYMIS) then
               X2T = X2T - X2P + X0
               Y2T = Y2T - Y(MP, NP - 1) + Y0
            end if
         end if

         if (JSFERIC == 1) X2T = X2T * cos(DG2RD * Y0)

         XN1 = sqrt(X1T * X1T + Y1T * Y1T)
         XN2 = sqrt(X2T * X2T + Y2T * Y2T)
!     error bij belgen op nt4.0, onduidelijk, daarom afgevangen
         if (xn1 /= 0) then
            if (NTO == 0) then
               DX = (DX0 * X1T + DY0 * Y1T) / XN1
               DY = (DY0 * X1T - DX0 * Y1T) / XN1
            else
               DX = (DX0 * X1T - DY0 * Y1T) / XN1
               DY = (DX0 * Y1T + DY0 * X1T) / XN1
            end if
         else
            dx = 0
            dy = 0
         end if
         DX0 = DX
         DY0 = DY
         return
      end subroutine TOLOCL
