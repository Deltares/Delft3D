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

      subroutine TEKGPT(X, Y, mmax, nmax, MC, NC, &
                        MP, NP, NCOL, RD1)
!     TEKEN GRIDLIJNEN UITKOMEND OP DIT PUNT
         use m_missing
         use m_wearelt
         implicit none
         integer :: mmax, nmax, mc, nc, mp, np, ncol
         double precision :: X(MMAX, NMAX), Y(MMAX, NMAX), RD1(MMAX, NMAX)

         double precision :: XP, YP
         integer :: MPU, MPD, NPU, NPD, ncolcir
         XP = X(MP, NP)
         if (XP == XYMIS) return
         YP = Y(MP, NP)
         call MOVABS(XP, YP)
         call SETCOL(NCOL)
         MPU = MP + 1
         MPD = MP - 1
         NPU = NP + 1
         NPD = NP - 1
         if (MPU <= MC) then
            if (X(MPU, NP) /= XYMIS) then
               call MOVABS(X(MPU, NP), Y(MPU, NP))
               call LNABS(XP, YP)
            end if
         end if
         if (MPD >= 1) then
            if (X(MPD, NP) /= XYMIS) then
               call MOVABS(X(MPD, NP), Y(MPD, NP))
               call LNABS(XP, YP)
            end if
         end if
         if (NPU <= NC) then
            if (X(MP, NPU) /= XYMIS) then
               call MOVABS(X(MP, NPU), Y(MP, NPU))
               call LNABS(XP, YP)
            end if
         end if
         if (NPD >= 1) then
            if (X(MP, NPD) /= XYMIS) then
               call MOVABS(X(MP, NPD), Y(MP, NPD))
               call LNABS(XP, YP)
            end if
         end if
         call SETXOR(0)
         if (RD1(MP, NP) /= DMISS) then
            call ISOCOL(RD1(MP, NP), NCOLCIR)
            call CIR(RCIR)
            call SETCOL(0)
            call PTABS(XP, YP)
         end if
         call SETXOR(1)
         return
      end subroutine tekgpt
