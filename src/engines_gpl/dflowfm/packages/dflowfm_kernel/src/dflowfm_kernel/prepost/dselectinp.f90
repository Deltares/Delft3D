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

   subroutine DSELECTINP(X, Y, N, KIN)
      use precision, only: dp
      use M_POLYGON
      use m_missing, only: dmiss, jins
      use geometry_module, only: dpinpok
      use m_min_max_pol
      implicit none
      integer :: N
      real(kind=dp) :: X(N), Y(N), ZK
      integer :: KIN(N)

      integer :: in
      integer :: k
      real(kind=dp) :: xmaxp
      real(kind=dp) :: xminp
      real(kind=dp) :: ymaxp
      real(kind=dp) :: yminp
      ZK = 1d0

      if (NPL < 3) then
         KIN = 1
      else
         call MINMAXPOL(XMINp, YMINp, XMAXp, YMAXp)
         do K = 1, N
            IN = 0
            if (X(K) >= XMINp .and. X(K) <= XMAXp .and. Y(K) >= YMINp .and. Y(K) <= YMAXp) then
               call DPINPOK(X(K), Y(K), ZK, NPL, XPL, YPL, IN, jins, dmiss)
            end if
            KIN(K) = IN
         end do
      end if
   end subroutine DSELECTINP
