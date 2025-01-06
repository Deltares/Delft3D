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

module m_gaanwestoppen

implicit none

private

public :: gaanwestoppen

contains

  subroutine GAANWESTOPPEN(K, KN316, JASTOP) !SET JASTOP = 1 ALS WE GAAN STOPPEN
     use M_NETW

     integer :: KN316, JASTOP, N1, N6, KK, L, K

     JASTOP = 0; N1 = 0; N6 = 0

     if (NMK0(K) == 1) then
        JASTOP = 1; return
     end if

     do KK = 1, NMK(K)
        L = NOD(K)%LIN(KK)
        if (KN(3, L) == 1) then
           N1 = N1 + 1
        else if (KN(3, L) == 6) then
           N6 = N6 + 1
        end if
     end do
     if (KN316 == 1) then
        if (N1 + N6 /= 2) then ! altijd stoppen bij niet doorgaande node
           JASTOP = 1
        end if
     else if (KN316 == 6) then ! alleen stoppen bij aantal 6 jes ongelijk 2
        if (N6 /= 2) then
           JASTOP = 1
        end if
     end if

  end subroutine GAANWESTOPPEN

end module m_gaanwestoppen
