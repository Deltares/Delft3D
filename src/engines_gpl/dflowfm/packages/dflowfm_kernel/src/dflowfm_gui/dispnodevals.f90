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

module m_dispnodevals
use m_drcirc


implicit none

contains

  subroutine DISPNODEVALS(KP)
     use m_netw
     use m_devices, only: iws
     use m_ktext

     integer :: KP

     integer :: l
     integer :: n
     character TEX * 23
     if (KP == 0) return
     call DRCIRC(XK(KP), YK(KP), ZK(KP))

     TEX = 'NODE NR    :           '
     write (TEX(14:), '(I10)') KP
     call KTEXT(TEX, IWS - 22, 4, 15)

     TEX = 'X COORD    :           '
     write (TEX(14:), '(E10.3)') XK(KP)
     call KTEXT(TEX, IWS - 22, 13, 15)

     TEX = 'Y COORD    :           '
     write (TEX(14:), '(E10.3)') YK(KP)
     call KTEXT(TEX, IWS - 22, 14, 15)

     TEX = 'Z COORD    :           '
     write (TEX(14:), '(E10.3)') ZK(KP)
     call KTEXT(TEX, IWS - 22, 15, 15)

     TEX = 'ELEM       :           '
     do N = 1, NMK(KP)
        L = NOD(KP)%LIN(N)
        write (TEX(6:11), '(I6 )') N
        write (TEX(14:23), '(I10)') L
        call KTEXT(TEX, IWS - 22, 15 + N, 15)
     end do

     if (netflow == 2) return

     TEX = 'NR OF ELEMS:           '
     write (TEX(14:), '(I10)') NMK(KP)
     call KTEXT(TEX, IWS - 22, 6, 15)

     return
  end subroutine DISPNODEVALS

end module m_dispnodevals
