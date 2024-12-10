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

module m_getvminmax

   implicit none

contains

   subroutine getvminmax(num, vmin, vmax, v, n)
      use precision, only: dp
      use unstruc_display_data
      use m_missing, only: dmiss

      integer :: n
      integer, intent(in) :: num
      real(kind=dp) :: vmin, vmax, v(n)

      if (profmin(num) == dmiss) then
         vmin = 1d9
         vmin = min(vmin, minval(v(1:n)))
      else
         vmin = profmin(num)
      end if

      if (profmax(num) == dmiss) then
         vmax = -1d9
         vmax = max(vmax, maxval(v(1:n)), vmin + 1d-5)
      else
         vmax = profmax(num)
      end if
   end subroutine getvminmax

end module m_getvminmax
