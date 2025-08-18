module m_array_or_scalar
!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
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

   private

   interface realloc
      module procedure realloc_t_array
      module procedure realloc_t_scalar
   end interface

   public :: realloc, assign_pointer_to_t_array

   ! Abstract base type for dicoww
   type, public, abstract :: t_array_or_scalar
   contains
      procedure(get_array_or_scalar), deferred :: get
   end type t_array_or_scalar

   ! Interface for deferred binding
   abstract interface
      pure function get_array_or_scalar(this, k) result(val)
         import :: t_array_or_scalar, dp
         class(t_array_or_scalar), intent(in) :: this
         integer, intent(in) :: k
         real(kind=dp) :: val
      end function get_array_or_scalar
   end interface

   ! Concrete type for scalar dicoww
   type, extends(t_array_or_scalar) :: t_scalar
      real(kind=dp) :: value
   contains
      procedure :: get => get_t_scalar
   end type t_scalar

   ! Concrete type for array dicoww
   type, extends(t_array_or_scalar) :: t_array
      real(kind=dp), dimension(:), allocatable, public :: values
   contains
      procedure :: get => get_t_array
   end type t_array

contains

   !> Scalar implementation of get_dicoww
   pure function get_t_scalar(this, k) result(val)
      class(t_scalar), intent(in) :: this !< t_scalar object to obtain value from regardless of index k
      integer, intent(in) :: k !< dummy index to make call consistent
      real(kind=dp) :: val
      associate (k_dummy => k)
      end associate
      val = this%value
   end function get_t_scalar

   !> Array implementation of get_dicoww
   pure function get_t_array(this, k) result(val)
      class(t_array), intent(in) :: this !< t_array object to obtain value from at index k
      integer, intent(in) :: k !< index in the array to obtain value from
      real(kind=dp) :: val
      val = this%values(k)
   end function get_t_array

!> (re)allocate dicoww as scalar regardless of previous status
   subroutine realloc_t_scalar(array_or_scalar, value)
      class(t_array_or_scalar), allocatable, intent(inout) :: array_or_scalar !< array_or_scalar object to be reallocated
      real(kind=dp), intent(in) :: value !< value to be set in the scalar array_or_scalar

      if (allocated(array_or_scalar)) then
         deallocate (array_or_scalar)
      end if

      allocate (t_scalar :: array_or_scalar)
      select type (scalar => array_or_scalar)
      type is (t_scalar)
         scalar%value = value
      end select
   end subroutine realloc_t_scalar

!> (re)allocate array_or_scalar as array regardless of previous status, optionally with a fill_value and a pointer to the values array
   subroutine realloc_t_array(array_or_scalar, n, fill_value)
      class(t_array_or_scalar), allocatable, target, intent(inout) :: array_or_scalar !< array_or_scalar object to be reallocated
      integer, intent(in) :: n !< size of the array to allocate
      real(kind=dp), optional, intent(in) :: fill_value !< value to fill the array with, if present

      if (allocated(array_or_scalar)) then
         deallocate (array_or_scalar)
      end if

      allocate (t_array :: array_or_scalar)

      select type (array => array_or_scalar)
      type is (t_array)
         if (allocated(array%values)) then
            if (size(array%values /= n)) then
               deallocate (array%values)
            end if
         end if
         if (.not. allocated(array%values)) then
            allocate (array%values(n))
         end if
         if (present(fill_value)) then
            array%values = fill_value
         end if
      end select
   end subroutine realloc_t_array

   subroutine assign_pointer_to_t_array(array_or_scalar, values_ptr, ierr)
      class(t_array_or_scalar), allocatable, target, intent(in) :: array_or_scalar !< array_or_scalar object to assign pointer to
      real(kind=dp), dimension(:), pointer, intent(out) :: values_ptr !< pointer to the values array to assign
      integer, intent(out) :: ierr !< error code, 0 if successful, 1 if array_or_scalar is not allocated

      ierr = 0
      if (.not. allocated(array_or_scalar)) then
         ierr = 1 !< set error code if array_or_scalar is not allocated
         return !< silent exit, no error if array_or_scalar is not allocated
      end if
      select type (array => array_or_scalar)
      type is (t_array)
         values_ptr => array%values
      type is (t_scalar)
         ierr = 1 !< set error code if array_or_scalar is a scalar
      end select
   end subroutine assign_pointer_to_t_array


end module m_array_or_scalar
