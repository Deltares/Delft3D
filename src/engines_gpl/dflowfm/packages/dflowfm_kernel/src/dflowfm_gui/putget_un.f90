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

      subroutine putget_un(NUM, NWHAT, NPUT, NUMB, XP, YP, KEY)
         implicit none
         integer :: ja
         integer :: key
         integer :: ndraw
         integer :: nput
         integer :: num
         integer :: numb
         integer :: nwhat
         double precision :: xp
         double precision :: yp
         common / DRAWTHIS / ndraw(50)

!
         call DISPUT(NPUT)

!     IF (KEY .EQ. 3) THEN
         call MENUH(0, NUM, NWHAT)
         call BOTLIN(0, NUMB, KEY)
         call FRAMES(31)
!     ENDIF

!
20       continue
         call READLOCATOR(XP, YP, KEY)
!
         if (KEY >= 24 .and. KEY <= 26) then
            call FKEYS(KEY)
            if (KEY == 3) return
         else if (KEY == 1) then
!        BOVEN
            JA = KEY
            call MENUH(JA, NUM, NWHAT)
            call BOTLIN(0, NUMB, KEY)
            if (JA /= 0) return
         else if (KEY == 2) then
!        ONDER
            JA = KEY
            call BOTLIN(JA, NUMB, KEY)
            if (JA /= 0) return
         else if (KEY == 90 .or. KEY == 90 + 32) then
!        Z(oomin)
            call ZOOMIN(KEY, NPUT)
            return
         else if (KEY == 65 .or. KEY == 65 + 32) then
!        A(nchor)
            call ANCHOR(XP, YP)
         else if (KEY == 170 .or. KEY == 80 .or. KEY == 80 + 32) then
            NDRAW(10) = 1
            KEY = 3
            return
         else
            return
         end if
         goto 20
      end
