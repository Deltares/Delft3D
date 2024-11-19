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

module m_showreal

implicit none

contains

      ! Now a double precision (double precision ::)
      subroutine SHOWREAL(TEXT, value)
         use m_devices
         use m_helpnow
         implicit none
         integer :: infoattribute
         integer :: ixp
         integer :: iyp
         integer :: len
         integer :: nbckgr
         integer :: nforgr
         double precision :: val
         double precision :: value
         character TEXT * (*)
         
         VAL = value
         IXP = IWS / 2
         IYP = IHS / 2
         NFORGR = InfoAttribute(13)
         NBCKGR = InfoAttribute(14)
         LEN = len_trim(TEXT)
         call INPOPUP('ON')
         call ITEXTCOLOUR('BWHITE', 'BLUE')
!      CALL IWINOPEN(IXP,IYP,LEN+8,1)
         call IWINOPEN(IXP, IYP, LEN + 11, 1)
         call ITEXTCOLOUR('BBLUE', 'BWHITE')
         call IWINOUTSTRINGXY(1, 1, TEXT)
!      CALL IWINOUTDOUBLEXY(1+LEN,1,VALUE,'(F8.1)')
         call IWINOUTDOUBLEXY(1 + LEN, 1, value, '(F11.1)')
         call ITEXTCOLOURN(NFORGR, NBCKGR)
         return
      end

end module m_showreal
