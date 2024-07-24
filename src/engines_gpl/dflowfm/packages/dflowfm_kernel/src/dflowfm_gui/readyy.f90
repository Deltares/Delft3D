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

!>    plot a statusbar in the GUI
      subroutine READYY(TEXT, AF)
         use m_devices
         use unstruc_display, only: jaGUI
         implicit none

         character TEXT * (*), BALK * 400
         double precision :: af

         integer, save :: ih
         integer, save :: ini = 0
         integer, save :: iw
         integer, save :: ixp
         integer, save :: iyp
         integer :: naf

         if (jaGUI /= 1) return

         if (INI == 0) then
            INI = 1
            IXP = 10
            IYP = 10
            IW = IWS - 10 - 10
            IH = 2
            call ITEXTCOLOUR('BWHITE', 'BLUE')
            call IWinAction('FCP')
            call IWinOpenTitle(IXP, IYP, IW, IH, TEXT)
            call FILLUP(BALK, ' ', IW)
            call ITEXTCOLOUR('BLACK', 'BWHITE')
            call IWinOutStringXY(2, 2, BALK(1:IW))
         else
            NAF = max(AF * IW, 1d0)
            call FILLUP(BALK, 'X', NAF)
            call IWinOutStringXY(1, 2, BALK(1:NAF))
         end if
         if (AF == -1) then
            call IWinClose(1)
            INI = 0
            return
         end if
         return
      end subroutine READYY
