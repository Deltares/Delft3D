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

      subroutine EDITGRID(MODE, NFLD, KEY)
         use unstruc_colors
         use m_grid
         implicit none
         integer :: mode, nfld, key

         integer :: L, NLEVEL, JA, NUM, NWHAT, NPUT, NUMB, MP, NP, MD, ND, &
                    ML, NL, MH, NH, NUMP, NLOC, IN, JN, INSIDE, ndraw, NCOL
         integer :: newmode

         common / HELPNOW / WRDKEY, NLEVEL
         common / DRAWTHIS / ndraw(50)

         character TEX * 20, WRDKEY * 40, FIELDOP * 40

         double precision :: xp, yp, wf(4)

         TEX = ' '//FIELDOP(NFLD)
         L = len_trim(TEX)
         WRDKEY = FIELDOP(NFLD)
         NLEVEL = 3
         JA = 0
         NUM = 0
         NWHAT = 0
         NPUT = 0
         NUMB = 17
         NCOL = NCOLDG

         MP = 0
         NP = 0
         call BOTLIN(0, NUMB, KEY)

10       continue
         call DRAWNU(KEY)
         call KTEXT(TEX, 1, 2, 15)
         call KTEXT(' Click Grid Points  ', 1, 3, 15)
         call putget_un(NUM, NWHAT, NPUT, NUMB, XP, YP, KEY)

         if (NUM /= 0) then
!        ER IS EEN KEUZE
            if (NUM == 4) then
               MODE = NWHAT
               return
            else
               call CHOICES(NUM, NWHAT, KEY)
            end if
         else if (KEY >= 577) then ! Alt+letter switches edit mode.
            call selecteditmode(newmode, key)
            if (newmode > 0 .and. newmode /= mode) then
               mode = newmode
               return
            end if
         else if (KEY == 21) then
!        INS KEY
            if (NPUT == 0 .or. NPUT == -2) then
!           kijken welk punt bij deleten en bij oppakken
               call ISPOIN(xc, yc, mmax, nmax, MC, NC, zc, &
                           XP, YP, MP, NP)
            end if
            if (NPUT == 0 .and. MP /= 0) then
!           punt oppakken
               call TEKGRPT(xc, yc, mmax, nmax, MC, NC, &
                            MP, NP, 0)
               NPUT = 1
            else if (NPUT == 1 .and. MP /= 0) then
!           punt neerzetten
               if (NFLD == 1) then
                  call SAVEGRD()
                  xc(MP, NP) = XP
                  yc(MP, NP) = YP
                  call TEKGRPT(xc, yc, mmax, nmax, MC, NC, &
                               MP, NP, NCOL)
               else if (NFLD == 2) then
                  NUMP = 80
                  NLOC = 1
                  ML = max(1, MP - NUMP)
                  MH = min(MC, MP + NUMP)
                  NL = max(1, NP - NUMP)
                  NH = min(NC, NP + NUMP)
                  call TEKGRD(xc, yc, mmax, nmax, ML, NL, MH, NH, 0, NDRAW(38), key, mc)
                  call TEKGRD(xch, ych, mmax, nmax, ML, NL, MH, NH, 0, NDRAW(16), key, mch)
                  call SAVEGRD()
                  xc(MP, NP) = XP
                  yc(MP, NP) = YP
                  call MODFLD(xc, yc, xch, ych, mmax, nmax, &
                              MC, NC, MP, NP, &
                              NUMP, NLOC, 1, 1)
                  call TEKGRD(xc, yc, mmax, nmax, ML, NL, MH, NH, NCOL, NDRAW(38), key, mc)
                  call TEKGRD(xch, ych, mmax, nmax, ML, NL, MH, NH, NCOLRG, NDRAW(16), key, mch)
               end if
               NPUT = 0
            else if (NPUT == -1) then
!           punt toevoegen
               call FINDNM(XP, YP, xc, yc, mmax, nmax, &
                           MC, NC, INSIDE, &
                           MP, NP, IN, JN, wf)
               if (INSIDE == 1) then
                  call SAVEGRD()
                  call MODGR1(NPUT, & ! xc,  yc, mmax, nmax, MC, NC,
                              MP, NP, IN, JN) !, NCOL)
               else
                  call OKAY(0)
               end if
            else if (NPUT == -2 .and. MP /= 0) then
!           punt deleten
               call SAVEGRD()
               call TEKGRPT(xc, yc, mmax, nmax, MC, NC, &
                            MP, NP, 0)
               call MODGR1(NPUT, & !xc, yc, mmax, nmax, MC, NC,
                           MP, NP, IN, JN) !, NCOL)
            end if
         else if (KEY == 22) then
!        ENTER KEY ENKEL DISPLAY
            call ISPOIN(xc, yc, mmax, nmax, MC, NC, zc, &
                        XP, YP, MD, ND)
         else if (KEY == 23) then
!        ESCAPE KEY
            call RESTOREGRD()
            KEY = 3
         else if (KEY == 27) then
!        TAB
            !CALL SHWXYZ(xc, yc, zc,MC,NC,0,KEY,M,N)
         else if (KEY == 73 .or. KEY == 73 + 32) then
            if (NPUT /= 1) then
!           kijken welk punt dit is t.b.v insert mode
               call ISPOIN(xc, yc, mmax, nmax, MC, NC, zc, &
                           XP, YP, MP, NP)
            end if
            NPUT = -1
         else if (KEY == 8) then ! Backspace KEY
!        delete entire network (within polygon if any) and stay in previous mode.
            call delgrd(KEY, 1, 1)
            key = 3
         else if (KEY == 68 .or. KEY == 68 + 32) then
!        delete mode
            NPUT = -2
         else if (KEY == 82 .or. KEY == 82 + 32 .and. NPUT /= 1) then
!        replace mode, maar niet bij zetten
            NPUT = 0
         else if (KEY == 85 .or. KEY == 85 + 32) then ! U-KEY, UPDATE PARTITIONING COUNT
            call TEKnumnetcells(1)
            KEY = 3
         else if (KEY == 98) then
!        b RINGS BELL
            call KTEXT(' B Rings Bell', 2, 6, 11)
            call OKAY(0)
         else if (KEY == 76 .or. KEY == 76 + 32) then
!        CALL TEKHOOK(XP,YP)
         end if
!
         goto 10
!
      end subroutine editgrid
