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

   subroutine CLOSEWORLD()
      use precision, only: dp
      use M_NETW
      use M_SFERIC
      use m_mergenodes
      implicit none
      integer :: K1, K2, ja
      real(kind=dp) :: xmn, xmx

      if (JSFERIC == 0) return

      XMN = minval(XK(1:numk))
      XMX = maxval(XK(1:numk))

      if (abs(XMN) < 1d-10 .and. abs(XMX - 360d0) < 1d-10) then !MAKE YOUR OWN 0-360 CONNECTIONS, only once

         do K1 = 1, NUMK
            if (real(XK(K1)) == 0.0) then
               do K2 = 1, NUMK
                  if (real(XK(K2)) == 360.0) then
                     if (abs(YK(K1) - YK(K2)) < 1d-10) then
                        call MERGENODES(K2, K1, JA)
                        exit
                     end if
                  end if
               end do
            end if
         end do

      end if

   end subroutine CLOSEWORLD
