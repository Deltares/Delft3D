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

!
      subroutine SCROLH(NUMCHC, HLPTXT, NUMTXT, NLEVEL, IH, JOFND, JATAB)
         implicit none
         integer :: ih
         integer :: jatab
         integer :: jofnd
         integer :: key
         integer :: nlevel
         integer :: numchc
         integer :: numtxt
!     Controls NUMCHC, the desired line, 0 means exit
!     The value of NUMCHC is checked against limits in this routine
!     JOFIND : search, JATAB : keywordwindow
         character HLPTXT(NUMTXT) * (*)
!
         call TIMLIN()
         call InKeyEvent(KEY)
         call TIMLIN()
         if (KEY == 128) then
            call NEXT(-1, NLEVEL, NUMCHC, HLPTXT, NUMTXT)
         else if (KEY == 129) then
            call NEXT(1, NLEVEL, NUMCHC, HLPTXT, NUMTXT)
         else if (KEY == 130) then
            NLEVEL = min(4, NLEVEL + 1)
            call NEXT(1, NLEVEL, NUMCHC, HLPTXT, NUMTXT)
         else if (KEY == 131) then
            NLEVEL = max(1, NLEVEL - 1)
            call NEXT(-1, NLEVEL, NUMCHC, HLPTXT, NUMTXT)
         else if (KEY == 132) then
            NUMCHC = max(1, NUMCHC - IH)
         else if (KEY == 133) then
            NUMCHC = min(NUMTXT, NUMCHC + IH)
         else if (KEY == 140) then
            NUMCHC = 1
         else if (KEY == 141) then
            NUMCHC = NUMTXT
         else if (KEY == 177) then
            JOFND = -1
         else if (KEY == 27) then
            NUMCHC = 0
         else if (KEY == 9) then
            JATAB = 1 - JATAB
         end if
         return
      end
