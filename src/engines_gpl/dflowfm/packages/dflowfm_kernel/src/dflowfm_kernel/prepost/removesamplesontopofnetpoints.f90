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

   subroutine REMOVESAMPLESONTOPOFNETPOINTS(XS, YS, NS)
      use m_netw
      implicit none
      integer :: ns
      double precision :: XS(NS), YS(NS)

      double precision :: dx
      double precision :: dy
      integer :: jaontop
      integer :: k
      integer :: ks
      integer :: n
      double precision :: tolnet
      TOLNET = 0.1d0
      N = 0
      do KS = 1, NS
         JAONTOP = 0
         do K = 1, NUMK
            DX = abs(XK(K) - XS(KS)); DY = abs(YK(K) - YS(KS))
            if (DX < TOLNET .and. DY < TOLNET) then
               JAONTOP = 1; cycle
            end if
         end do
         if (JAONTOP == 0) then
            N = N + 1
            XS(N) = XS(KS); YS(N) = YS(KS)
         end if
      end do
      NS = N
   end subroutine REMOVESAMPLESONTOPOFNETPOINTS
