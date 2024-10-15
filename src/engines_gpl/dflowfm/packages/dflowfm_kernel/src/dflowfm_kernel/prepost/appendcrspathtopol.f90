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
module m_append_crspath_to_pol
   implicit none
contains
!> Appends the polyline of a cross section path to the current global
!! polyline. Useful for converting cross sections, thin dams or thin
!! dykes back to editable polylines.
subroutine appendCRSPathToPol(path)
   use m_crspath, only: tcrspath, path
   use m_polygon, only: npl, xpl, ypl, zpl, increasepol
   use m_missing, only: dmiss

   type(tcrspath), intent(in) :: path

   integer :: ip

   call increasepol(npl + 1 + path%np, 1)

! Insert dmiss seperator behind existing polylines, if any.
   if (npl > 0) then
      npl = npl + 1
      xpl(npl) = dmiss
      xpl(npl) = dmiss
      zpl(npl) = dmiss
   end if

   do ip = 1, path%np
      npl = npl + 1
      xpl(npl) = path%xp(ip)
      ypl(npl) = path%yp(ip)
      zpl(npl) = path%zp(ip)
   end do
end subroutine appendCRSPathToPol
end module m_append_crspath_to_pol
