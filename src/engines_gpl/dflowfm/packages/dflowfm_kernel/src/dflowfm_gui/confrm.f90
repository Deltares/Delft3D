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

      subroutine CONFRM(TEXT, JAZEKR)
         use unstruc_display
         use m_helpnow
         use m_timlin
         use m_fkeys
         implicit none

         character TEXT * (*)
         integer :: jazekr

         integer :: imenutwo
         integer :: infoattribute
         integer :: infoinput
         integer :: iopt
         integer :: iw
         integer :: ixp
         integer :: iyp
         integer :: key
         integer :: nbckgr
         integer :: nforgr

         if (jaGUI /= 1) then
            if (jazekr /= 1) then
               jazekr = 0
            end if
            return
         end if

         IW = NPOS(3)
         IXP = NPOS(1) + (IWS - IW) / 2
         IYP = NPOS(2)
!     IXP    = INFOCURSOR(1)
!     IYP    = INFOCURSOR(2)
         NFORGR = InfoAttribute(13)
         NBCKGR = InfoAttribute(14)
         call INPOPUP('ON')
20       continue
         call ITEXTCOLOUR('BWHITE', 'RED')
         call INHIGHLIGHT('BLUE', 'BWHITE')
         call TIMLIN()
         if (jazekr == 1) then ! SPvdP: if jazekr.eq.1, default to yes
            IOPT = IMenuTwo('NO', 'YES', IXP, IYP, TEXT, 1, 2)
         else
            IOPT = IMenuTwo('NO', 'YES', IXP, IYP, TEXT, 1, 1)
         end if
         call TIMLIN()
         KEY = InfoInput(55)
         call INFLUSH()
         if (KEY >= 24 .and. KEY <= 26) then
            NLEVEL = 3
            WRDKEY = TEXT
            call FKEYS(KEY)
            if (KEY == 3) then
               call INPOPUP('OFF')
               call ITEXTCOLOURN(NFORGR, NBCKGR)
               return
            end if
            goto 20
         else if (KEY == 21 .or. KEY == 22) then
            if (IOPT == 2) then
               JAZEKR = 1
            else
               JAZEKR = 0
            end if
         else if (KEY == 23) then
            JAZEKR = 0
         else
            goto 20
         end if
         call INPOPUP('OFF')
         call ITEXTCOLOURN(NFORGR, NBCKGR)

         return
      end
