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

  subroutine COPYPOLTo1Dnet()
     use m_polygon
     use M_netw
     use M_MISSING
     use network_data, only: kn3typ
     implicit none

     integer :: k, L, kn3o

     kn3o = kn3typ; kn3typ = 1

     ! CALL INCREASENETW(NUMK+NPL, NUML+NPL-1)
     do K = 2, NPL

        if (xpl(k) /= dmiss .and. xpl(K - 1) /= dmiss) then
           call addnetlink(xpl(k - 1), ypl(k - 1), xpl(k), ypl(k), L)
        end if

     end do

     kn3typ = kn3o
     call DELPOL()
     return
  end subroutine COPYPOLTo1Dnet
