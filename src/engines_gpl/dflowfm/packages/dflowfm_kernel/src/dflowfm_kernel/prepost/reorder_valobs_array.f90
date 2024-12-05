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

!> Reorders a 1D array obsArray in valobs, so that values on inactive layers get fillValue. For sigma-layer model, obsArray are actually not changed.
subroutine reorder_valobs_array(n, obsArray, kb, kt, nlayb, fillValue)
   use precision, only: dp
   use m_alloc
   implicit none
   integer, intent(in) :: n !< Size of this 1D array
   real(kind=dp), dimension(n), intent(inout) :: obsArray !< The 1D array from valobs
   integer, intent(in) :: kb !< Index on bottom active layer
   integer, intent(in) :: kt !< Index on top active layer
   integer, intent(in) :: nlayb !< Layer number for the bottom layer (in 1:kmx)
   real(kind=dp), intent(in) :: fillValue !< Fill value for obsArray

   real(kind=dp), allocatable :: tmpArray(:)
   integer :: k, kk, klay

   if (kt < kb - 1) then
      return
   end if

   call realloc(tmpArray, n)

   ! copy obsArray to tmpArray
   do k = 1, n
      tmpArray(k) = obsArray(k)
   end do

   ! reset obsArray to fillValue
   obsArray = fillValue

   ! set values in correct order in obsArray
   k = 1
   do kk = kb - 1, kt
      klay = kk - kb + nlayb + 1
      obsArray(klay) = tmpArray(k)
      k = k + 1
   end do

end subroutine reorder_valobs_array
