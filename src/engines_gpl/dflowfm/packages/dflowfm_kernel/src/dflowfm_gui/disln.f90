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
module m_disln
   implicit none
contains
   subroutine DISLN(LL) ! print link values
      use m_flowgeom, only: ln, xz, yz
      use m_devices, only: iws
      use m_ktext
      use m_gtext
      use m_zlin

      integer :: LL
      character TEX * 23

      if (LL <= 0) then
         TEX = 'NO FLOW LINK FOUND    '
         call KTEXT(TEX, IWS - 22, 4, 15)
      else
         TEX = 'FLOW LINK NR:         '
         write (TEX(14:), '(I10)') LL
         call KTEXT(TEX, IWS - 22, 4, 15)
         TEX = 'VAL=                  '
         write (TEX(6:), '(E18.11)') ZLIN(LL)
         call KTEXT(TEX, IWS - 22, 5, 15)
         TEX = 'Nd1:         '
         write (TEX(6:), '(I10)') LN(1, LL)
         call KTEXT(TEX, IWS - 22, 6, 15)
         call gtext(tex, xz(ln(1, LL)), yz(ln(1, LL)), 221)
         TEX = 'Nd2:         '
         write (TEX(6:), '(I10)') LN(2, LL)
         call KTEXT(TEX, IWS - 22, 7, 15)
         call gtext(tex, xz(ln(2, LL)), yz(ln(2, LL)), 221)
      end if

      return
   end subroutine DISLN
end module m_disln
