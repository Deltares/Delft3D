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

module m_nodtoall
use m_setpoint, only: setpoint

implicit none

private

public :: nodtoall

contains

  subroutine NODTOALL()
     use precision, only: dp
     use m_addelem, only: addelem
     use m_netw
     use gridoperations

     integer :: k
     integer :: k1
     integer :: n1
     real(kind=dp) :: XX, YY, ZZ
     N1 = NUMK
     XX = 0.5d0; YY = 0.5d0; ZZ = 0d0
     call GIVENEWNODENUM(K1)
     call SETPOINT(XX, YY, ZZ, K1)
     do K = 1, N1
        call ADDELEM(K1, K)
     end do
     return
  end subroutine NODTOALL

end module m_nodtoall
