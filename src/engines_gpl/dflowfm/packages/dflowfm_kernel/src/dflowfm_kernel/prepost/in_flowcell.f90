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

module m_in_flowcell

implicit none

private

public :: in_flowcell

contains

   subroutine in_flowcell(xp, yp, kk)
      use precision, only: dp
      use m_flowgeom
      use unstruc_display
      use m_missing, only: jins, dmiss
      use geometry_module, only: pinpok, dbdistance

      real(kind=dp) :: xp, yp
      integer :: inn, k, kk, nn

      kk = 0

      do K = 1, ndx2D
         if (.not. allocated(nd(K)%x)) cycle
         NN = size(nd(K)%x)
         call PINPOK(xp, yp, NN, nd(K)%x, nd(K)%y, inn, jins, dmiss)
         if (inn == 1) then
            KK = K; return
         end if
      end do

   end subroutine in_flowcell

end module m_in_flowcell
