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

!>    determine sample bounding box
      subroutine get_samples_boundingbox()
         use m_samples
         use m_missing
         implicit none

         integer :: i

         xsammin = huge(1d0)
         xsammax = -huge(1d0)
         ysammin = huge(1d0)
         ysammax = -huge(1d0)

         do i = 1, NS
            if (xs(i) /= DMISS .and. ys(i) /= DMISS .and. zs(i) /= DMISS) then
               xsammin = min(xsammin, xs(i))
               xsammax = max(xsammax, xs(i))
               ysammin = min(ysammin, ys(i))
               ysammax = max(ysammax, ys(i))
            end if
         end do

         return
      end subroutine get_samples_boundingbox
