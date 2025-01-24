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

module m_qnmessage

   implicit none

contains

   subroutine QNMESSAGE(TEX)
      use unstruc_display_data, only: npos
      use unstruc_colors
      use m_devices, only: iws, ihs
      use messagehandling, only: msgbuf, msg_flush

      integer :: ih
      integer :: iw
      integer :: ixp
      integer :: iyp
      character TEX * (*)

      IW = NPOS(3)
      IXP = NPOS(1) + (IWS - IW) / 2
      IYP = NPOS(2)
      IH = IHS - 9

      write (msgbuf, '(A)') TEX
      call msg_flush()

      call ITEXTCOLOURN(HLPFOR, HLPBCK)
      call IWinAction('FPC')
      call IWinOpen(IXP, IHS - 1, IW, 2)
      call IWINOUTCENTRE(1, TEX)
      call IWINOUTCENTRE(2, 'press F2 to read this message')
      call IOSWAIT(200)
      call IWinClose(1)

      return
   end

end module m_qnmessage
