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
module m_deallocate_spline_props
   implicit none
contains
!> deallocate splineprops array
   subroutine deallocate_splineprops()
      use m_spline2curvi

      integer :: ispline

      if (.not. allocated(splineprops)) return

      do ispline = 1, ubound(splineprops, 1)
         if (allocated(splineprops(ispline)%ics)) deallocate (splineprops(ispline)%ics)
         if (allocated(splineprops(ispline)%Lorient)) deallocate (splineprops(ispline)%Lorient)
         if (allocated(splineprops(ispline)%t)) deallocate (splineprops(ispline)%t)
         if (allocated(splineprops(ispline)%cosphi)) deallocate (splineprops(ispline)%cosphi)
         if (allocated(splineprops(ispline)%hL)) deallocate (splineprops(ispline)%hL)
         if (allocated(splineprops(ispline)%hR)) deallocate (splineprops(ispline)%hR)
         if (allocated(splineprops(ispline)%NsubL)) deallocate (splineprops(ispline)%NsubL)
         if (allocated(splineprops(ispline)%NsubR)) deallocate (splineprops(ispline)%NsubR)
      end do

      deallocate (splineprops)

      return
   end subroutine deallocate_splineprops
end module m_deallocate_spline_props
