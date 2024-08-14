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

  subroutine TEKLINK(L, NCOL)
     use m_netw
     use unstruc_colors
     implicit none
     integer :: L, NCOL
     integer :: k1
     integer :: k2

     call SETLINKCOLOUR(L, NCOL)

     K1 = KN(1, L)
     K2 = KN(2, L)
     if (K1 /= 0 .and. K2 /= 0) then
        call MOVABS(XK(K1), YK(K1))
        call LNABS(XK(K2), YK(K2))
        if (NCOL > 0) then
           call SETCOL(NCOLNN)
           call PTABS(XK(K1), YK(K1))
           call PTABS(XK(K2), YK(K2))
        end if
     end if
     return
  end subroutine TEKLINK
