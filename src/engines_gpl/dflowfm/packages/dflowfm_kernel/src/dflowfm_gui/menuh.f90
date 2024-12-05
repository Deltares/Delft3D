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
module m_menuh
   use m_menuv1

   implicit none
contains
   subroutine MENUH(JA, NUM, NWHAT)
      use m_devices
      use m_helpnow
      use m_timlin
      use m_fkeys
      use m_botlin

      integer :: ja
      integer :: num
      integer :: nwhat

      integer :: infoinput
      integer :: imenuhoriz
      integer :: iw
      integer :: key
      integer :: maxop
      integer :: maxopt
      parameter(MAXOP=20)
      character(len=10) OPTION(MAXOP)
!
!     Keuzemenu horizontaal
!
      OPTION(1) = 'FILES     '
      OPTION(2) = 'OPERATIONS'
      OPTION(3) = 'DISPLAY   '
      OPTION(4) = 'EDIT      '
      OPTION(5) = 'ADDSUBDEL '
      OPTION(6) = 'VARIOUS   '
      MAXOPT = 6
      KEY = 0
!
      IW = IWS
!
10    continue
!
20    continue
      if (JA == 1) then
         call TIMLIN()
         call BOTLIN(0, 1, KEY)
         if (NOPSYS == 1) then
            call ITEXTCOLOUR('BBLUE', 'BWHITE')
         else
            call ITEXTCOLOUR('BLACK', 'BWHITE')
         end if
         call INHIGHLIGHT('BWHITE', 'RED')
         NUM = IMenuHoriz(OPTION, MAXOPT, 1, 1, IW, 0, 1)
         call TIMLIN()
      end if
      if (NOPSYS == 1) then
         call InHighlight('BWHITE', 'WHITE')
         call ITEXTCOLOUR('BWHITE', 'WHITE')
      else
         call InHighlight('BLACK', 'WHITE')
         call ITEXTCOLOUR('BLACK', 'WHITE')
      end if
      call IOUTMenuHoriz(OPTION, MAXOPT, 1, 1, IW, 0, 1)
      if (JA /= 1) return
!
      KEY = InfoInput(55)
      if (KEY /= 23) then
         NLEVEL = 1
         WRDKEY = OPTION(NUM)
      end if
      if (KEY == 21 .or. KEY == 22) then
!        INS KEY
         call MENUV1(NUM, NWHAT)
         if (NWHAT == 0) goto 20
         call IOUTSTRINGXY(1, 2, ' OPTION : '//WRDKEY)
         return
      else if (KEY == 23 .or. KEY == -2) then
!        ESC OR OUTSIDE
         NUM = 0
         return
      else
         call FKEYS(KEY)
         if (KEY == 3) return
      end if
      goto 10
!
   end
end module m_menuh
