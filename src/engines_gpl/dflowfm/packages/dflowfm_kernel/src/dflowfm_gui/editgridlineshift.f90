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

      subroutine EDITGRIDLINESHIFT(MODE, NFLD, KEY, M1, N1, M2, N2)
         use m_grid
         use unstruc_colors, only: ncolrg
         use m_helpnow
         use m_qnerror
         use m_ktext
         use m_putget_un
         use m_okay
         use m_botlin
         use m_draw_nu
         use m_restore_grd
         use m_tekln2
         implicit none
         integer :: MODE, NFLD, KEY, M1, N1, M2, N2
         integer :: newmode
         character TEX * 20, FIELDOP * 40

         integer :: JA, NUM, NWHAT, NPUT, NUMB, JONCE, mp, np, m, n, NCOL
         double precision :: xp, yp
         TEX = ' '//FIELDOP(NFLD)
         WRDKEY = FIELDOP(NFLD)
         NLEVEL = 3
         JA = 0
         NUM = 0
         NWHAT = 0
         NPUT = 20
         NUMB = 7
         NCOL = NCOLRG
         JONCE = 0

         MP = 0
         NP = 0
         call BOTLIN(0, NUMB, KEY)

10       continue
         call DRAWNU(KEY)
         call KTEXT(TEX, 1, 2, 15)
         call KTEXT(' Now Shift the Line ', 1, 3, 15)
         call TEKLN2(Xc, Yc, mmax, nmax, M1, N1, M2, N2, NCOL)

20       continue
         call putget_un(NUM, NWHAT, NPUT, NUMB, XP, YP, KEY)
         if (KEY /= 23) JONCE = 0

         if (NUM /= 0) then
!        ER IS EEN KEUZE
            if (NUM == 4) then
               MODE = NWHAT
               return
            else
               call QNERROR('Menu is disabled, leave SHIFT LINE ', &
                            '(Esc or right mouse button)', ' ')
               NUM = 0
!           CALL CHOICES(MODE,NUM,NWHAT,KEY)
            end if
         else if (KEY >= 577) then ! Alt+letter switches edit mode.
            call selecteditmode(newmode, key)
            if (newmode > 0 .and. newmode /= mode) then
               mode = newmode
               return
            end if
         else if (KEY == 21) then
!        INS KEY
            if (NPUT == 20) then
!           kijken welk punt bij oppakken
               call ISPOIN(Xc, Yc, mmax, nmax, MC, NC, Zc, &
                           XP, YP, MP, NP)
!           moet wel op lijn liggen
               if (M1 == M2) then
                  if (MP == M1) then
                     if (NP < N1 .or. NP > N2) then
                        call QNERROR('Only shift points on the indicated', 'line', ' ')
                        MP = 0
                     end if
                  else
                     call QNERROR('Only shift points on the indicated', 'line', ' ')
                     MP = 0
                  end if
               end if
               if (N1 == N2) then
                  if (NP == N1) then
                     if (MP < M1 .or. MP > M2) then
                        call QNERROR('Only shift points on the indicated', 'line', ' ')
                        MP = 0
                     end if
                  else
                     call QNERROR('Only shift points on the indicated', 'line', ' ')
                     MP = 0
                  end if
               end if
            end if
            if (NPUT == 20 .and. MP /= 0) then
!           punt oppakken
               call TEKGRPT(Xc, Yc, mmax, nmax, MC, NC, &
                            MP, NP, 0)
               NPUT = 1
            else if (NPUT == 1 .and. MP /= 0) then
!           punt neerzetten
               Xc(MP, NP) = XP
               Yc(MP, NP) = YP
               call TEKGRPT(Xc, Yc, mmax, nmax, MC, NC, &
                            MP, NP, NCOL)
               NPUT = 20
            end if
            goto 20
         else if (KEY == 22) then
!        ENTER KEY
            return
         else if (KEY == 23) then
!        ESCAPE KEY
            JONCE = JONCE + 1
            if (JONCE == 1) then
               call RESTOREgrd()
               KEY = 3
            else
               return
            end if
         else if (KEY == 27) then
!        TAB
            call SHWXYZ(Xc, Yc, Zc, mmax, nmax, MC, NC, 0, KEY, M, N)
         else if (KEY == 98) then
!        b RINGS BELL
            call KTEXT('B Rings Bell', 2, 6, 11)
            call OKAY(0)
         end if
!
         goto 10
!                           7
      end subroutine editgridlineshift
