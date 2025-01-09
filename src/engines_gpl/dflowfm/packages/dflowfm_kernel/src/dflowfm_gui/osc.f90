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
module m_osc
   use m_histor, only: histor

   implicit none
contains
   subroutine OSC(KEY)
      use m_devices
      use messagehandling, only: msgbuf, msg_flush
      use m_help

      integer :: infoinput
      integer :: ixp
      integer :: iyp
      integer :: key
      integer :: len
      integer :: nlevel
      character STRING * 58, WRDKEY * 40
      IXP = 2
      IYP = 10
      if (NOPSYS == 1) then
         call ISCREENMODE('T', 80, 25, 16)
      else
         return
      end if
10    continue
!     CALL BOTLIN(0,1,KEY)
!     CALL ITEXTCOLOURN(MNUFOR,MNUBCK)
      call ITEXTCOLOUR('WHITE', 'BLUE')
      call INPOPUP('ON')
      call InStringXY(IXP, IYP, 'enter OS-command ; ', 1, STRING, LEN)
      call INPOPUP('OFF')
      KEY = InfoInput(55)
      if (KEY == 24) then
         WRDKEY = 'OS-command'
         NLEVEL = 2
         call HELP(WRDKEY, NLEVEL)
      else if (KEY == 25) then
         call HISTOR()
      else if ((KEY == 21 .or. KEY == 22) .and. LEN >= 1) then
         write (msgbuf, '(A,A)') 'OPERATING SYSTEM COMMAND: ', STRING(:LEN)
         call msg_flush()
         call IOsCommand(STRING(:LEN))
      else if (KEY == 23) then
         if (NOPSYS == 1) call ISCREENMODE('GR', NPX, NPY, NCOLR)
         KEY = 3
         return
      end if
      goto 10
   end
end module m_osc
