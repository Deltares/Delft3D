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

!> allocate splineprops array
module m_allocate_splineprops

   implicit none

   private

   public :: allocate_splineprops

contains

   subroutine allocate_splineprops()
      use m_splines
      use m_spline2curvi
      use m_missing

      integer :: ispline

      allocate (splineprops(mcs))

      do ispline = 1, mcs
         allocate (splineprops(ispline)%ics(mcs))
         allocate (splineprops(ispline)%Lorient(mcs))
         allocate (splineprops(ispline)%t(mcs))
         allocate (splineprops(ispline)%cosphi(mcs))
         allocate (splineprops(ispline)%hL(Nsubmax, mcs))
         allocate (splineprops(ispline)%hR(Nsubmax, mcs))
         allocate (splineprops(ispline)%NsubL(mcs))
         allocate (splineprops(ispline)%NsubR(mcs))

!     initialize
         splineprops(ispline)%id = -999
         splineprops(ispline)%ncs = 0
         splineprops(ispline)%length = DMISS
         splineprops(ispline)%hmax = DMISS
         splineprops(ispline)%ics(:) = 0
         splineprops(ispline)%Lorient(:) = .true.
         splineprops(ispline)%t(:) = DMISS
         splineprops(ispline)%cosphi(:) = DMISS
         splineprops(ispline)%hL(:, :) = DMISS
         splineprops(ispline)%hR(:, :) = DMISS
         splineprops(ispline)%NsubL(:) = 0
         splineprops(ispline)%NsubR(:) = 0

         splineprops(ispline)%mfac = 0
         splineprops(ispline)%nfacL(:) = 0
         splineprops(ispline)%nfacR(:) = 0
         splineprops(ispline)%iL = 0
         splineprops(ispline)%iR = 0
      end do

   end subroutine allocate_splineprops

end module m_allocate_splineprops
