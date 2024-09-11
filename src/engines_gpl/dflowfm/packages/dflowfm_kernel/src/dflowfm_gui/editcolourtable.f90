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

      subroutine EDITCOLOURTABLE(MODE, KEY)
         use unstruc_colors
         use m_helpnow
         use m_ktext
         use m_putget_un
         use m_okay
         implicit none
         integer :: key
         integer :: mode
         integer :: n1
         integer :: n1c
         integer :: n2
         integer :: n2c
         integer :: n3
         integer :: n3c
         integer :: nput
         integer :: num
         integer :: numb
         integer :: numcol
         integer :: numcolc
         integer :: nwhat
         double precision :: xp
         double precision :: yp

         character TEX * 26, TEX2 * 4

         TEX = ' EDIT COLORTABLE          '
         WRDKEY = TEX
         NLEVEL = 2
         NUM = 0
         NWHAT = 0
         NPUT = 33
         NUMB = 14

10       continue
         call DRAWNU(KEY)
         call ALLCOLOURS()
         call KTEXT(TEX, 1, 2, 15)
         call putget_un(NUM, NWHAT, NPUT, NUMB, XP, YP, KEY)

         if (NUM /= 0) then
!        ER IS EEN KEUZE
            if (NUM == 4) then
               MODE = NWHAT
               KEY = 3
               return
            else
               call CHOICES(NUM, NWHAT, KEY)
            end if
         else if (KEY == 21) then
!        INS KEY
            if (NPUT == 33) then
               call GETCOLORNUMBER(XP, YP, NUMCOLC, N1C, N2C, N3C)
!           WRITE(TEX2,'(I4)') NUMCOLC
!           CALL QNMESSAGE('COLOUR NR 1 = '//TEX2)
               NPUT = 34
            else if (NPUT == 34) then
               call GETCOLORNUMBER(XP, YP, NUMCOL, N1, N2, N3)
               write (TEX2, '(I4)') NUMCOL
!           CALL QNMESSAGE('IS CHANGED TO THE COLOUR OF NR : '//TEX2)
               call IGRPALETTERGB(NUMCOLC, N1, N2, N3)
               NPUT = 33
            end if
         else if (KEY == 22) then
!        ENTER KEY
            call CHANGECOLOR(XP, YP)
         else if (KEY == 23) then
!        ESC
            call IGRPALETTERGB(NUMCOLC, N1C, N2C, N3C)
         else if (KEY == 98) then
!        b RINGS BELL
            call KTEXT('B RINGS BELL', 2, 6, 11)
            call OKAY(0)
         end if
!
         goto 10
!
      end
