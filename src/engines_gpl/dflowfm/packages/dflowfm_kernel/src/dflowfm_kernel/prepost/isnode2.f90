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

  subroutine ISNODE2(KP, XP, YP, ZP) ! X,Y,Z MOETEN ALLEN KLOPPEN
     use m_netw
     use m_wearelt
     implicit none
     integer :: KP
     double precision :: XP, YP, ZP

     double precision :: eps
     integer :: jav
     integer :: jview
     integer :: k
     double precision :: xyz

     common / HOWTOVIEW / JVIEW, JAV, XYZ ! 1,2,3 OF 4
     KP = 0
     EPS = 0.01d0 * RCIR

     do K = NUMK, 1, -1
        if (abs(XK(K) - XP) < EPS .and. abs(YK(K) - YP) < EPS .and. abs(ZK(K) - ZP) < EPS) then
           KP = K
           return
        end if
     end do
     return
  end subroutine ISNODE2
