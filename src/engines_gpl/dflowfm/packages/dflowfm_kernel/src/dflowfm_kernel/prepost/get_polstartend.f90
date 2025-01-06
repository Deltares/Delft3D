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

!> find the start and end index of a polygon
module m_get_polstartend

implicit none

private

public :: get_polstartend

contains

subroutine get_polstartend(NPL, XPL, YPL, ipol, jstart, jend)
   use precision, only: dp
   use m_missing, only: dmiss
   use geometry_module, only: get_startend

   integer, intent(in) :: NPL !< polygon size
   real(kind=dp), dimension(NPL), intent(in) :: XPL !< polygon x-coordinates
   real(kind=dp), dimension(NPL), intent(in) :: YPL !< polygon y-coordinates

   integer, intent(in) :: ipol !< index of a polygon point
   integer, intent(out) :: jstart, jend !< start and end indices of polygon

   integer :: jpoint

   jpoint = 1
   jstart = 1
   jend = 0
   do while ((ipol < jstart .or. ipol > jend) .and. jpoint <= NPL)
      call get_startend(NPL - jpoint + 1, xpl(jpoint:NPL), ypl(jpoint:NPL), jstart, jend, dmiss)
      jstart = jstart + jpoint - 1
      jend = jend + jpoint - 1
      jpoint = jend + 2
   end do

   return
end subroutine get_polstartend

end module m_get_polstartend
