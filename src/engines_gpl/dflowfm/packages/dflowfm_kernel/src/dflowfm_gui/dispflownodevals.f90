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

subroutine DISPFLOWNODEVALS(KP)
   use m_flowgeom
   use m_flow
   use M_DEVICES
   use m_ktext

   implicit none
   double precision :: ZNOD
   integer :: KP

   integer :: l
   integer :: n
   character TEX * 23

   if (KP == 0) return
   call DRCIRC(XZ(KP), YZ(KP), BL(KP))

   TEX = 'NODE NR    :           '
   write (TEX(14:), '(I10)') KP
   call KTEXT(TEX, IWS - 22, 4, 15)

   TEX = 'X COORD    :           '
   write (TEX(14:), '(E10.3)') Xz(KP)
   call KTEXT(TEX, IWS - 22, 13, 15)

   TEX = 'Y COORD    :           '
   write (TEX(14:), '(E10.3)') Yz(KP)
   call KTEXT(TEX, IWS - 22, 14, 15)

   TEX = 'Z COORD    :           '
   write (TEX(14:), '(E10.3)') bl(KP)
   call KTEXT(TEX, IWS - 22, 15, 15)

   TEX = 'Z COORD    :           '
   write (TEX(14:), '(e10.3)') znod(kp)
   call KTEXT(TEX, IWS - 22, 16, 15)

   TEX = 'link       :           '
   do N = 1, Nd(kp)%lnx
      L = ND(KP)%LN(N)
      write (TEX(6:11), '(I6 )') N
      write (TEX(14:23), '(I10)') L
      call KTEXT(TEX, IWS - 22, 16 + N, 15)
   end do
end subroutine DISPFLOWNODEVALS
