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

module m_interponpolyline

implicit none

private

public :: interponpolyline

contains

      !> Performs linear interpolation between two values along a polyline.
      !! The interpolation is done along a polyline at the distances
      !! measured along the consecutive polyline segments.
      subroutine interpOnPolyline(DPL, DXS, NPL, DXS1, DXS2)
         use precision, only: dp

         integer :: npl
         real(kind=dp), intent(in) :: DPL(NPL) !< Accumulated distance at each point.
         real(kind=dp), intent(out) :: DXS(NPL) !< Interpolated values of dxs1--dxs2 on polyline points.
         real(kind=dp), intent(in) :: dxs1 !< Value at first polyline point.
         real(kind=dp), intent(in) :: dxs2 !< Value at last polyline point.

         real(kind=dp) :: f
         real(kind=dp) :: f1
         integer :: n

         if (NPL <= 1) return

         do N = 1, NPL
            F = DPL(N) / DPL(NPL); F1 = 1 - F
            DXS(N) = F1 * DXS1 + F * DXS2
         end do

      end subroutine interpOnPolyline

end module m_interponpolyline
