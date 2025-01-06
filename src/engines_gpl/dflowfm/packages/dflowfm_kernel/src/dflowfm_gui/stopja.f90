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

module m_stopja
use m_stopint, only: stopint


   implicit none

contains

   subroutine STOPJA(JA)
      use unstruc_files
      use m_devices
      use m_okay
      implicit none
      integer :: imenutwo
      integer :: infocursor
      integer :: iopt
      integer :: ixp
      integer :: iyp
      integer :: ja
      IXP = INFOCURSOR(1)
      IYP = INFOCURSOR(2)
      call INPOPUP('ON')
      call ITEXTCOLOUR('BWHITE', 'RED')
      call INHIGHLIGHT('BLUE', 'BWHITE')
      call OKAY(0)
      IOPT = IMenuTwo &
             ('NO', 'YES', (IWS - 41) / 2, IHS / 2, 'DO YOU REALLY WANT TO '// &
              'QUIT THE PROGRAM ? ', 1, 1)
      call INPOPUP('OFF')
      if (IOPT == 1) then
         JA = 0
      else
         write (msgbuf, '(A)') 'YOU STOPPED THE PROGRAM'
         call msg_flush()
         call IWinClose(1)
         call STOPINT()
      end if
      return
   end

end module m_stopja
