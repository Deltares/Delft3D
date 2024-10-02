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
module m_new_link
   implicit none
contains
  subroutine NEWLINK(K1, K2, LNU) ! no checks
     use m_netw
     use unstruc_colors, only: ncoldn
     use gridoperations, only: increasenetw
     use m_tek_link

     integer :: K1, K2, LNU

     NUML = NUML + 1
     if (NUML >= LMAX) then
        call INCREASENETW(NUMK, NUML)
     end if
     KN(1, NUML) = K1
     KN(2, NUML) = K2
     KN(3, NUML) = KN3TYP

     LNU = NUML

     call TEKLINK(NUML, NCOLDN)

  end subroutine NEWLINK
end module m_new_link
