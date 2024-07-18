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

  subroutine OGIVENEWLINKNUM(LNU)

     use m_netw
     use gridoperations

     implicit none
     integer :: LNU

     integer :: kx
     integer :: l
     integer :: lx

     do L = 1, NUML
        if (KN(1, L) == 0 .and. KN(2, L) == 0) then
           LNU = L
           return
        end if
     end do
     if (NUML < LMAX) then
        NUML = NUML + 1
        LNU = NUML
     else
        KX = 1.2 * NUMK; LX = 1.2 * NUML
        call INCREASENETW(KX, LX)
     end if
     return
  end subroutine OGIVENEWLINKNUM
