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

   subroutine SELLLINKSINPOL(LIN, N)
      use m_netw
      use m_missing, only: dmiss, jins
      use geometry_module, only: pinpok
      use m_min_max_pol
      implicit none
      integer :: N
      integer :: LIN(N)

      integer :: in
      integer :: in2
      integer :: k1
      integer :: k2
      integer :: l
      double precision :: xp1
      double precision :: xp2
      double precision :: xplmax
      double precision :: xplmin
      double precision :: yp1
      double precision :: yp2
      double precision :: yplmax
      double precision :: yplmin

      if (NPL < 3) then
         LIN = 1
      else
         call MINMAXPOL(XplMIN, YplMIN, XplMAX, YplMAX)
         do L = 1, NUML
            K1 = KN(1, L); Xp1 = XK(K1); Yp1 = yK(K1)
            K2 = KN(2, L); Xp2 = XK(K2); Yp2 = yK(K2)
            if (Xp1 >= XplMIN .and. Xp1 <= XplMAX .and. Yp1 >= YplMIN .and. Yp1 <= YplMAX .and. &
                Xp2 >= XplMIN .and. Xp2 <= XplMAX .and. Yp2 >= YplMIN .and. Yp2 <= YplMAX) then
               call PINPOK(Xp1, Yp1, NPL, XPL, YPL, IN, jins, dmiss)
               call PINPOK(Xp2, Yp2, NPL, XPL, YPL, IN2, jins, dmiss)
               LIN(L) = in * in2
            else
               LIN(L) = 0
            end if
         end do
      end if
   end subroutine SELLLINKSINPOL
