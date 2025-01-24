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
module m_wripol
   implicit none
contains
   subroutine WRIPOL(MPOL)
      use precision, only: dp
      use M_POLYGON
      use m_missing
      use m_wrildb

      integer :: mpol, numnampli
      integer :: NCLAN(0)
      real(kind=dp) :: ZSH(0)

      if (NPL <= 0) return
      numnampli = size(nampli)
      if (zpl(1) == dmiss) then ! No third column for z-values
         call WRILDB(MPOL, XPL, YPL, NPL, NCLAN, 0, ZSH, 0, nampli, 64, numnampli)
      else
         call WRILDB(MPOL, XPL, YPL, NPL, NCLAN, 0, ZPL, NPL, nampli, 64, numnampli)
      end if
   end subroutine wripol
end module m_wripol
