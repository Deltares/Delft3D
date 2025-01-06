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

module m_externaltrianglestoouterquads
use m_setpoint, only: setpoint
use m_getquad, only: getquad


implicit none

private

public :: externaltrianglestoouterquads

contains

   subroutine externaltrianglestoouterquads()
      use precision, only: dp

      use m_netw
      use m_polygon
      use m_missing, only: jins, dmiss
      use geometry_module, only: dpinpok
      use gridoperations

      implicit none

      integer :: in
      integer :: k1
      integer :: k2
      integer :: k3
      integer :: k4
      integer :: kp
      integer :: l
      integer :: lnu
      real(kind=dp) :: xp
      real(kind=dp) :: yp
      real(kind=dp) :: zp

      real(kind=dp) :: XL, YL, ZL = 0d0

      do L = 1, NUML
         K1 = KN(1, L); K2 = KN(2, L)
         if (NMK(K1) <= 3 .and. NMK(K2) <= 3) then
            XL = 0.5d0 * (XK(K1) + XK(K2))
            YL = 0.5d0 * (YK(K1) + YK(K2))
            call DPINPOK(XL, YL, ZL, NPL, XPL, YPL, IN, jins, dmiss)
            if (IN == 1) then
               call GETQUAD(L, K1, K2, K3, K4)
               if (K3 /= 0) then
                  XP = 0.5d0 * (2 * XK(K1) - XK(K4) + 2 * XK(K2) - XK(K3))
                  YP = 0.5d0 * (2 * YK(K1) - YK(K4) + 2 * YK(K2) - YK(K3))
                  call GIVENEWNODENUM(KP)
                  call SETPOINT(XP, YP, ZP, KP)
                  call CONNECTDB(K2, KP, LNU)
                  call CONNECTDB(KP, K1, LNU)
               end if
            end if
         end if
      end do

   end subroutine externaltrianglestoouterquads

end module m_externaltrianglestoouterquads
