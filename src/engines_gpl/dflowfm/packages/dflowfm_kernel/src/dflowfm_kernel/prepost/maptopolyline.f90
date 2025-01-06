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

module m_maptopolyline

implicit none

private

public :: maptopolyline

contains

      !> Maps a list of distances to a list of points.
      !! The points are placed onto a polyline at the distances measured along
      !! the consecutive polyline segments.
      subroutine mapToPolyline(XHO, YHO, DPL, NO, XH, YH, DPLA, NPL) ! HAAL HUIDIGE PUNTEN OP
         use precision, only: dp
         use m_interpolateOnPolyline

         integer, intent(in) :: NO !< Nr. of polyline points.
         integer, intent(in) :: npl !< Nr. of points to be interpolated.
         real(kind=dp), intent(in) :: XHO(NO), YHO(NO) !< Polyline points.
         real(kind=dp), intent(in) :: DPL(NO) !< Accumulated segment sizes along polyline.
         real(kind=dp), intent(out) :: XH(NPL), YH(NPL) !< Output points interpolated on polyline.
         real(kind=dp), intent(in) :: DPLA(NPL) !< Desired distances for all points.

         integer :: ja
         integer :: n

         do N = 1, NPL
            call interpolateOnPolyline(XHO, YHO, YHO, DPL, NO, XH(N), YH(N), YH(N), DPLA(N), JA)
         end do

      end subroutine mapToPolyline

end module m_maptopolyline
