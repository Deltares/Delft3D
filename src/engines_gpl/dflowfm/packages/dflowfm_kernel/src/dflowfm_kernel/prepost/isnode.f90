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

  subroutine ISNODE(KP, XP, YP, ZP)

     use m_netw
     use m_wearelt, only: cr, rcir
     use m_missing, only: dmiss
     use m_sferic
     use m_sferzoom
     use m_dispnode
     use m_dbdistance_hk

     implicit none

     integer :: KP
     double precision :: XP, YP, ZP
     double precision :: rcy, dis
     integer :: K, KPREV

     if (KP < 0) then
        KPREV = abs(KP)
     else
        KPREV = 0
     end if

     if (jsfertek > 0) then
        rcy = cr * dyh * ra * dg2rd
        ! call setrcirxy(xp,yp,rcx,rcy)
     end if

     KP = 0
     ZP = dmiss
     do K = 1, NUMK
        if (jsfertek > 0) then
           call dbdistancehk(xk(k), yk(k), xp, yp, dis)
           if (dis < rcy) then
              kp = k
           end if
        else
           if (abs(XK(k) - XP) < rcir .and. abs(YK(k) - YP) < rcir) then
              KP = K
           end if
        end if
        if (kp > 0) then
           call DISPNODE(KP)
           ZP = ZK(kp)
!         XYZ = ZKK
           return
        end if
     end do

  end subroutine ISNODE
