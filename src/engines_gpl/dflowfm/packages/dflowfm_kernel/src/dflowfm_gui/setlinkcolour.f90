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

  subroutine SETLINKCOLOUR(L, NCOL)
     use m_netw
     use unstruc_colors
     implicit none
     integer :: L, NCOL, NCL
     if (NCOL == 0) then ! ERASE
        NCL = 0
     else if (NCOL == 1) then ! 1 MEANS: DRAW IN KN3 PREDEFINED COLOUR
        if (KN(3, L) == 0) then
           NCL = 31
        else if (KN(3, L) == 1) then ! 1D
           NCL = NCOLRG
        else if (KN(3, L) == 2) then ! 2D
           NCL = NCOLDN
        else if (KN(3, L) == 3) then ! 1d2d internal
           NCL = NCOLNN
        else if (KN(3, L) == 4) then ! 1d2d longitudinal
           NCL = NCOLRN
        else if (KN(3, L) == 5) then ! 1d2d internal pipe streetinlet
           NCL = NCOLSP
        else if (KN(3, L) == 6) then ! 1d mainbranch
           NCL = KLSAM
        else if (KN(3, L) == 7) then ! 1d2d internal pipe roofgutter
           NCL = NCOLSP + 5
        end if
     else
        NCL = NCOL
     end if
     call SETCOL(NCL)
     return
  end
