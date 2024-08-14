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

      subroutine CHADEP(XP, YP, RD, KEY)
         use M_MISSING
         implicit none
         double precision :: XP, YP, RD
         integer :: KEY

         double precision :: f
         double precision :: fac
         integer :: jplus
         double precision :: rdol
         character WRDKEY * 40
         WRDKEY = 'CHANGE SCALAR VALUE'
         RDOL = RD
         JPLUS = 0
         call DISPUT(21)
10       continue
         call DISVAL1(RD)
         call KCIR(XP, YP, RD)
         call INKEYEVENT(KEY)

         if (KEY == 171) then
            call HELP(WRDKEY, 3)
         else if (KEY == 45 .or. KEY == 160) then
            if (RD == dmiss) RD = 6.9d0
            if (JPLUS /= -1) then
               FAC = 1d0
               F = max(.001d0, .01d0 * RD)
            end if
            RD = RD - F * FAC
            FAC = FAC * 1.01d0
            JPLUS = -1
         else if (KEY == 43 .or. KEY == 162) then
            if (RD == dmiss) RD = 6.9d0
            if (JPLUS /= 1) then
               FAC = 1d0
               F = max(.001d0, .01d0 * RD)
            end if
            RD = RD + F * FAC
            FAC = FAC * 1.01d0
            JPLUS = 1
         else if (KEY == 32) then
            call TYPEVALUE(RD, KEY)
            call DISVAL1(RD)
            call KCIR(XP, YP, RD)
            return
         else if (KEY == 68 .or. KEY == 68 + 32 .or. KEY == 143) then
            RD = dmiss
            call DISVAL1(RD)
            call KCIR(XP, YP, RD)
            return
         else if (KEY == 27) then
            RD = RDOL
            call DISVAL1(RD)
            call KCIR(XP, YP, RD)
            return
         else if (KEY /= 254 .and. KEY /= 257) then
            return
         end if
         goto 10
      end subroutine CHADEP
