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

module m_addtopol

   implicit none

   private

   public :: addtopol

contains

   !> add polygon to global polygons
   subroutine addtopol(XCRA, YCRA, NCRA)
      use precision, only: dp
      use m_polygon
      use m_alloc
      use m_missing

      integer, intent(in) :: NCRA
      real(kind=dp), dimension(NCRA), intent(in) :: XCRA, YCRA

      integer :: i

      if (NCRA <= 0) return

      call increasepol(NPL + NCRA + 1, 1)

      if (NPL > 0 .and. NCRA > 0) then
         NPL = NPL + 1
         xpl(NPL) = DMISS
         ypl(NPL) = DMISS
         zpl(NPL) = DMISS
      end if

      do i = 1, NCRA
         NPL = NPL + 1
         xpl(NPL) = XCRA(i)
         ypl(NPL) = YCRA(i)
         zpl(NPL) = DMISS
      end do

      return
   end subroutine addtopol

end module m_addtopol
