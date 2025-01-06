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

module m_get_isub

implicit none

private

public :: get_isub

contains

integer function get_isub(j, nfac1, j_loc) !< gets the subinterval of grid layer j
   use m_spline2curvi

   integer, intent(in) :: j !< grid layer index
   integer, dimension(Nsubmax), intent(in) :: nfac1 !< subinterval lengths
   integer, intent(out) :: j_loc !< grid layer in the subinterval

   integer :: isum, isub

   j_loc = j - 1
   if (j > sum(nfac1)) then
      isub = 0
      goto 1234
   end if

   isub = 1
   isum = 1 + nfac1(isub)
   do while (isum <= j .and. isub < Nsubmax)
      isub = isub + 1
      isum = isum + nfac1(isub)
   end do

   j_loc = j - isum + nfac1(isub)

!  error handling
1234 continue

   get_isub = isub

   return
end function

end module m_get_isub
