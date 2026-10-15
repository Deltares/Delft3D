module m_array_or_scalar
!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2026.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!

   use precision, only: dp

   implicit none

   type t_array_or_scalar
      real(dp) :: scalar = 0.0_dp
      real(dp), allocatable :: values(:) ! allocated only in the array case
   contains
      procedure :: get
   end type

contains

   elemental function get(this, k) result(val)
      class(t_array_or_scalar), intent(in) :: this
      integer, intent(in) :: k
      real(dp) :: val
      if (allocated(this%values)) then
         val = this%values(k)
      else
         val = this%scalar
      end if
   end function

end module m_array_or_scalar
