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

!
module m_setellips

   implicit none

   private

   public :: setellips

contains

   subroutine SETELLIPS(IELL)
      use m_ellips

      integer :: iell

      A = 6378137d0
      E = 0.081819d0

      if (IELL == 1) then ! Hayford
         A = 6378388d0
         E = 0.081992d0
      elseif (IELL == 2) then ! Bessel
         A = 6377397d0
         E = 0.081690d0
      elseif (IELL == 3) then ! WGS 84
         A = 6378137d0
         E = 0.081819d0
      elseif (IELL == 4) then ! Clarke 1880
         A = 6378249d0
         E = 0.082478d0
      elseif (IELL == 5) then ! India 1830
         A = 6377276.345d0
         E = 0.081473d0
      end if
      return
   end subroutine SETELLIPS

end module m_setellips
