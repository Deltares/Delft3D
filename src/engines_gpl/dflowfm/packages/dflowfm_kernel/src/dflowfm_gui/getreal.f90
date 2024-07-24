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

      ! Now a double precision (double precision ::)
      subroutine GETREAL(TEXT, value)
         use m_devices
         use M_MISSING
         implicit none
         integer :: infoattribute
         integer :: infoinput
         integer :: ixp
         integer :: iyp
         integer :: key
         integer :: nbckgr
         integer :: nforgr
         integer :: nlevel
         double precision :: val
         double precision :: value
         character WRDKEY * 40, TEXT * (*)
         common / HELPNOW / WRDKEY, NLEVEL

         VAL = value
         IXP = IWS / 2
         IYP = IHS / 2
         NFORGR = InfoAttribute(13)
         NBCKGR = InfoAttribute(14)
         call INPOPUP('ON')
20       continue
         call ITEXTCOLOUR('BWHITE', 'RED')
         call INHIGHLIGHT('BLUE', 'BWHITE')
         call TIMLIN()
!      CALL INDOUBLEXYDEF(IXP,IYP,TEXT,1,VAL,6,'(F6.1)')
         call INDOUBLEXYDEF(IXP, IYP, TEXT, 1, VAL, 12, '(F12.1)')
         call TIMLIN()
         KEY = InfoInput(55)
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
            value = VAL
         else
            value = dmiss
         end if
         call INPOPUP('OFF')
         call ITEXTCOLOURN(NFORGR, NBCKGR)
         return
      end
