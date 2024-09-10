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

      subroutine EDITGRIDBLOK(MODE, NFLD, KEY)
         use m_grid
         use unstruc_colors
         use m_helpnow
         use m_drawthis
         use m_grid_block
         use m_qnerror
         implicit none

         integer :: mode, nfld, key
         integer :: newmode
         integer :: num, nwhat, numb, mp, np
         character TEX * 20, FIELDOP * 40
         integer :: m1b, n1b, m2b, n2b, ipt, ja, jonce, m, n, nput
         double precision :: xp, yp

         TEX = ' '//FIELDOP(NFLD)
         WRDKEY = FIELDOP(NFLD)
         NLEVEL = 3
         NUM = 0
         NWHAT = 0
         NUMB = 8
         MP = 0
         NP = 0
         ITYPE = 2
         jonce = 0

         NPUT = 8
         call RESETB(NPUT)
         call BOTLIN(0, NUMB, KEY)

10       continue
         call DRAWNU(KEY)
         call TEKB(Xc, Yc, MMAX, NMAX, NCOLLN)
         call KTEXT(TEX, 1, 2, 15)
         call KTEXT(' Indicate a Block   ', 1, 3, 15)

         call putget_un(NUM, NWHAT, NPUT, NUMB, XP, YP, KEY)
         if (KEY /= 23) JONCE = 0

         if (NUM /= 0) then
!        ER IS EEN KEUZE
            if (NUM == 4) then
               MODE = NWHAT
               call TEKB(Xc, Yc, MMAX, NMAX, 0)
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
!        kijken welk punt
            call ISPOIN(Xc, Yc, mmax, nmax, MC, NC, Zc, &
                        XP, YP, MP, NP)
            if (MP /= 0) then
               if (NPUT == 16) then
                  MB(IPT) = MP
                  NB(IPT) = NP
                  call CIRR(Xc(MP, NP), Yc(MP, NP), NCOLLN)
                  if (NPT == 1) NPUT = 9
                  if (NPT == 2) NPUT = 17
                  if (NPT == 3) NPUT = 18
                  if (NPT == 4) NPUT = 19
               else
                  call NEWBLOCKPOINT(MP, NP, JA, IPT)
                  if (JA == 1) then
!                 voeg punt toe
                     call SAVEB(NPUT)
                     NPT = NPT + 1
                     MB(NPT) = MP
                     NB(NPT) = NP
                     call CIRR(Xc(MB(NPT), NB(NPT)), Yc(MB(NPT), NB(NPT)), NCOLLN)
                     if (NPT == 1) NPUT = 9
                     if (NPT == 2) NPUT = 17
                     if (NPT == 3) NPUT = 18
                     if (NPT == 4) NPUT = 19
                  else if (JA == -1) then
!                 niet meer toevoegen
                     call QNERROR('4 POINTS: CONTINUE = RIGHT MOUSE OR', 'Enter,', ' ')
                  else if (JA == 0) then
!                 oud punt geclickt; uitgummen
                     call SAVEB(NPUT)
                     call CIRR(Xc(MB(IPT), NB(IPT)), Yc(MB(IPT), NB(IPT)), 0)
                     if (IPT <= 2) call TEKB(Xc, Yc, MMAX, NMAX, 0)
                     MB(IPT) = 0
                     NB(IPT) = 0
                     NPUT = 16
                  end if
               end if
            end if
         else if (KEY == 22) then
!        ENTER KEY
            if (NPT <= 1) then
               call QNERROR('FIRST PRESS MORE POINTS WITH LEFT MOUSE BUTTON', ' ', ' ')
            else
               call TEKB(Xc, Yc, MMAX, NMAX, 0)
               call POSITIVEBLOK()
               M1B = max(MB(3) - 1, 1)
               N1B = max(NB(3) - 1, 1)
               M2B = min(MB(4) + 2, MC)
               N2B = min(NB(4) + 2, NC)
               call TEKGRD(Xc, Yc, mmax, nmax, M1B, &
                           N1B, M2B, N2B, 0, NDRAW(38), key, mc)
               if (allocated(xch)) then
                  call TEKGRD(Xch, Ych, mmax, nmax, M1B, &
                              N1B, M2B, N2B, 0, NDRAW(16), key, mc)
               end if

!           Begin Operatie
               call SAVEgrd()
               if (NFLD == 14) then
                  call NULFIELD(Xc, Yc, mmax, nmax)
               else if (NFLD == 15) then
                  call CUTFIELD(Xc, Yc, mmax, nmax, MC, NC)
               else if (NFLD == 16) then
                  !CALL ORTHO(X, Y, MB(3), NB(3), MB(4), NB(4), MC, NC, NUM, MMAX,NMAX)!!!
                  call ORTHOGRID(MB(3), NB(3), MB(4), NB(4))
               else if (NFLD == 17) then
                  call DOSMOOTH(NFLD) !Xc,Yc,mmax, nmax, MC,NC,NFLD,IJC,IJYES)
               end if
!           Einde Operatie
               call TEKGRD(Xc, Yc, mmax, nmax, M1B, &
                           N1B, M2B, N2B, NCOLDG, NDRAW(38), key, mc)
               if (allocated(xch)) then
                  call TEKGRD(Xch, Ych, mmax, nmax, M1B, &
                              N1B, M2B, N2B, NCOLRG, NDRAW(16), key, mc)
               end if

               if (NFLD == 14) then
                  if (MB(3) == 1 .or. MB(4) == MC .or. &
                      NB(3) == 1 .or. NB(4) == NC) then
                     call ADJUST(Xc, Yc, mmax, nmax, MC, NC)
                  end if
               else if (NFLD == 15) then
                  call ADJUST(Xc, Yc, mmax, nmax, MC, NC)
                  KEY = 3
               end if
               call RESETB(NPUT)
               NPUT = 8
            end if
         else if (KEY == 23) then
!        ESC
            JONCE = JONCE + 1
            if (JONCE == 1) then
               call RESTOREB(NPUT)
            else if (JONCE == 2) then
               NPUT = 10
               call RESETB(NPUT)
            else if (JONCE == 3) then
               call RESTOREgrd()
            end if
            KEY = 3
         else if (KEY == 27) then
!        TAB
            call SHWXYZ(Xc, Yc, Zc, mmax, nmax, MC, NC, 0, KEY, M, N)
         end if
!
         goto 10
!
      end subroutine editgridblok
