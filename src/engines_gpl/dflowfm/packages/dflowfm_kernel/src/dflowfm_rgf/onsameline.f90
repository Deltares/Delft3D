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

module m_onsameline

   implicit none

   private

   public :: onsameline

contains

   subroutine ONSAMELINE(IPT, MP, NP, JA)
      use m_grid_block

      integer :: mp, np, ja, ipt
      integer :: md, nd

      JA = 1
      if (ITYPE == 1) then
         if (IPT == 1 .and. MB(2) /= 0) then
            MD = MP - MB(2)
            ND = NP - NB(2)
            if (MD /= 0 .and. ND /= 0) JA = 0
         else if (IPT == 2) then
            MD = MP - MB(1)
            ND = NP - NB(1)
            if (MD /= 0 .and. ND /= 0) JA = 0
         end if
      end if
      return
   end subroutine onsameline

end module m_onsameline
