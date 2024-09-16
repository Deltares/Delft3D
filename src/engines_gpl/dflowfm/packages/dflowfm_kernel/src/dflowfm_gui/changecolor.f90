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

!----------------------------------------------------------------------
! subroutines from rest.F90
!----------------------------------------------------------------------
      subroutine CHANGECOLOR(XP, YP)
         use unstruc_colors
         use m_depmax
         use m_helpnow
         use m_disvalcolors
         use m_disput
         use m_set_col
         
         implicit none
         integer :: ic
         integer :: key
         integer :: n1
         integer :: n2
         integer :: n3
         integer :: numcol

         double precision :: xp
         double precision :: yp

         integer NCL(3)

         IC = 1

         call IMOUSECURSORHIDE()
         call DISPUT(35)

         call GETCOLORNUMBER(XP, YP, NUMCOL, N1, N2, N3)
         NCL(1) = N1
         NCL(2) = N2
         NCL(3) = N3

         call SETCOL(NUMCOL)
         call DISVALCOLORS(NUMCOL, NCL(1), NCL(2), NCL(3), IC)

20       continue

         call INKEYEVENT(KEY)

         if (KEY == 131) then
            IC = IC - 1
            if (IC == 0) IC = 3
         else if (KEY == 130) then
            IC = IC + 1
            if (IC == 4) IC = 1
         else if (KEY == 128) then
            NCL(IC) = min(255, NCL(IC) + 1)
            call IGRPALETTERGB(NUMCOL, NCL(1), NCL(2), NCL(3))
         else if (KEY == 129) then
            NCL(IC) = max(0, NCL(IC) - 1)
            call IGRPALETTERGB(NUMCOL, NCL(1), NCL(2), NCL(3))
         else if (KEY == 171) then
            call HELP(WRDKEY, 3)
         else if (KEY == 13 .or. KEY >= 251 .and. KEY <= 253) then
            call ORGLOCATOR(XP, YP)
            call IMOUSECURSORSHOW()
            return
         else if (KEY == 27) then
            call IGRPALETTERGB(NUMCOL, N1, N2, N3)
            call ORGLOCATOR(XP, YP)
            call IMOUSECURSORSHOW()
            return
         end if

         call SETCOL(NUMCOL)
         call DISVALCOLORS(NUMCOL, NCL(1), NCL(2), NCL(3), IC)
         call ALLCOLOURS()

         goto 20
      end
