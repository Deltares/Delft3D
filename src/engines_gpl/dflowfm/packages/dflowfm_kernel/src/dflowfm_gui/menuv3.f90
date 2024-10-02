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

module m_menuv3

implicit none

contains

      subroutine MENUV3(NWHAT, OPTION, MAXOPT)
         use unstruc_files, only: msgbuf, msg_flush
         use m_devices, only: nopsys
         use m_helpnow
         use m_timlin
         use m_fkeys
         use m_botlin

         integer :: imenuvertic, IXP, IYP
         integer :: infoinput
         integer :: infocursor
         integer :: ja
         integer :: key
         integer :: maxop
         integer :: maxopt
         integer :: nstart
         integer :: nwhat
         parameter(MAXOP=64)
         character(len=40) OPTION(MAXOP)
!     Keuzemenu verticaal
!
         NSTART = NWHAT
10       continue
         call BOTLIN(0, 1, KEY)
!
         IXP = INFOCURSOR(1)
         IXP = INFOINPUT(62) - 1
         IYP = 2
         call TIMLIN()
         if (NOPSYS == 1) then
            call ITEXTCOLOUR('BBLUE', 'BWHITE')
         else
            call ITEXTCOLOUR('BLACK', 'BWHITE')
         end if
         call INHIGHLIGHT('BWHITE', 'RED')
         call INPOPUP('ON')
         NWHAT = IMENUVERTIC(OPTION, MAXOPT, IXP, IYP, ' ', 0, 0, NSTART)
         call INPOPUP('OFF')
         call TIMLIN()
!
         KEY = InfoInput(55)
         if (KEY /= 23) then
            NLEVEL = 3
            WRDKEY = OPTION(NWHAT)
         end if

         if (KEY == 21) then
!        INS KEY
            write (msgbuf, '(A)') WRDKEY
            call msg_flush()
            JA = 0
            return
         else if (KEY == 22) then
!        ENTER KEY
            JA = 0
            return
         else if (KEY == 23 .or. KEY == -2) then
!        ESC OR OUTSIDE
            JA = 0
            NWHAT = 0
            return
         else if (KEY >= 24 .and. KEY <= 26) then
            call FKEYS(KEY)
            if (KEY == 3) return
         end if
         goto 10
!
      end

end module m_menuv3
