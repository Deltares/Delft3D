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

      !> This routine operates directly on active grid data from m_grid
      subroutine MODGR1(NPUT, MP, NP, IN, JN) !, NCOL)!XH, YH, mmax, nmax, MC, NC,
         use m_tekgrpt
         use m_missing
         use m_grid
         use unstruc_colors
         use m_okay
         use m_increase_grid
         implicit none

         integer :: nput, mp, np, in, jn
!      double precision :: XH(MMAX,NMAX), YH(MMAX,NMAX)
!     een beetje flauw geprogrammeerd, ook tekenen bij insert mode

         integer :: ja

         if (NPUT == -1) then
            JA = 0
            if (MP >= MMAX - 1) then
               call increasegrid(mp + 2, nmax)
!           CALL OKAY(0)
!           CALL QNERROR('Grid Becomes too Large in M-Dimension',' ',' ')
               return
            else
               if (MP == 1 .and. IN == -1) then
                  call SHIFXY(1, 0, MP, NP) !     XH,     YH,     mmax, nmax, MC,     NC,
               end if
            end if
            if (NP >= NMAX - 1) then
               call increasegrid(mmax, np + 2)
!           CALL OKAY(0)
!           CALL QNERROR('Grid Becomes too Large in N-Dimension',' ',' ')
               return
            else
               if (NP == 1 .and. JN == -1) then
                  call SHIFXY(0, 1, MP, NP) !     XH,     YH,     mmax, nmax, MC,     NC,
               end if
            end if

            if (IN == 1) then
               if (MP == MC - 1) MC = MC + 1
               if (Xc(MP + 2, NP) == XYMIS) then
                  Xc(MP + 2, NP) = 2 * Xc(MP + 1, NP) - Xc(MP, NP)
                  Yc(MP + 2, NP) = 2 * Yc(MP + 1, NP) - Yc(MP, NP)
                  call TEKGRPT(Xc, Yc, mmax, nmax, MC, NC, MP + 2, NP, NCOLDG)
                  JA = 1
               end if
               if (Xc(MP + 2, NP + 1) == XYMIS) then
                  Xc(MP + 2, NP + 1) = 2 * Xc(MP + 1, NP + 1) - Xc(MP, NP + 1)
                  Yc(MP + 2, NP + 1) = 2 * Yc(MP + 1, NP + 1) - Yc(MP, NP + 1)
                  call TEKGRPT(Xc, Yc, mmax, nmax, MC, NC, MP + 2, NP + 1, NCOLDG)
                  JA = 1
               end if
            else if (IN == -1) then
               if (Xc(MP - 1, NP) == XYMIS) then
                  Xc(MP - 1, NP) = 2 * Xc(MP, NP) - Xc(MP + 1, NP)
                  Yc(MP - 1, NP) = 2 * Yc(MP, NP) - Yc(MP + 1, NP)
                  call TEKGRPT(Xc, Yc, mmax, nmax, MC, NC, MP - 1, NP, NCOLDG)
                  JA = 1
               end if
               if (Xc(MP - 1, NP + 1) == XYMIS) then
                  Xc(MP - 1, NP + 1) = 2 * Xc(MP, NP + 1) - Xc(MP + 1, NP + 1)
                  Yc(MP - 1, NP + 1) = 2 * Yc(MP, NP + 1) - Yc(MP + 1, NP + 1)
                  call TEKGRPT(Xc, Yc, mmax, nmax, MC, NC, MP - 1, NP + 1, NCOLDG)
                  JA = 1
               end if
            else if (JN == 1) then
               if (NP == NC - 1) NC = NC + 1
               if (Xc(MP, NP + 2) == XYMIS) then
                  Xc(MP, NP + 2) = 2 * Xc(MP, NP + 1) - Xc(MP, NP)
                  Yc(MP, NP + 2) = 2 * Yc(MP, NP + 1) - Yc(MP, NP)
                  call TEKGRPT(Xc, Yc, mmax, nmax, MC, NC, MP, NP + 2, NCOLDG)
                  JA = 1
               end if
               if (Xc(MP + 1, NP + 2) == XYMIS) then
                  Xc(MP + 1, NP + 2) = 2 * Xc(MP + 1, NP + 1) - Xc(MP + 1, NP)
                  Yc(MP + 1, NP + 2) = 2 * Yc(MP + 1, NP + 1) - Yc(MP + 1, NP)
                  call TEKGRPT(Xc, Yc, mmax, nmax, MC, NC, MP + 1, NP + 2, NCOLDG)
                  JA = 1
               end if
            else if (JN == -1) then
               if (Xc(MP, NP - 1) == XYMIS) then
                  Xc(MP, NP - 1) = 2 * Xc(MP, NP) - Xc(MP, NP + 1)
                  Yc(MP, NP - 1) = 2 * Yc(MP, NP) - Yc(MP, NP + 1)
                  call TEKGRPT(Xc, Yc, mmax, nmax, MC, NC, MP, NP - 1, NCOLDG)
                  JA = 1
               end if
               if (Xc(MP + 1, NP - 1) == XYMIS) then
                  Xc(MP + 1, NP - 1) = 2 * Xc(MP + 1, NP) - Xc(MP + 1, NP + 1)
                  Yc(MP + 1, NP - 1) = 2 * Yc(MP + 1, NP) - Yc(MP + 1, NP + 1)
                  call TEKGRPT(Xc, Yc, mmax, nmax, MC, NC, MP + 1, NP - 1, NCOLDG)
                  JA = 1
               end if
            end if
            if (JA == 1) then
               call OKAY(0)
            else
               call OKAY(0)
            end if
         else if (NPUT == -2) then
            Xc(MP, NP) = XYMIS
            Yc(MP, NP) = XYMIS
            if (MP == 1 .or. MP == MC .or. NP == 1 .or. NP == NC) then
               call ADJUST(Xc, Yc, mmax, nmax, MC, NC)
            end if
         end if
         return
      end subroutine modgr1
