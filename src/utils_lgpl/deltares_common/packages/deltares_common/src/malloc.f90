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
!-------------------------------------------------------------------------------
!
!
!> Utility routines for memory (re)allocation.
module m_alloc
   implicit none
   private

   public realloc, reallocP, aerr, allocSize, reserve_sufficient_space

! TODO: Handle nondefault kinds properly? [AvD]

!> Reallocates memory for an existing array. Arrays of most intrinsic
!! data types up to rank 4 are accepted and they may still be unallocated.
!! realloc is mainly intended for increasing array sizes, but it may also
!! be used for \e decreasing them. Use m_alloc::realloc for allocatable arrays and
!! use m_alloc::reallocP for pointer arrays<sup>1</sup>.
!!
!! The actual values in the new array depend on two optional parameters:
!! \a keepExisting and \a fill.
!! By default, where the old and new dimensions overlap, the original array
!! data is preserved (i.e., for a larger upperbound, all data is preserved).
!! This behaviour can be switched off by passing the optional argument
!! <tt>keepExisting=.false.</tt> (for example, to prevent unnecessary data copy).
!!
!! An optional fill value may be specified to set the non-overlapping
!! elements. For example: <tt>call realloc(x, newmax, stat=istat, fill=-999d0)</tt>
!! The original array elements are NOT overwritten by \a fill, unless
!! <tt>keepExisting=.false.</tt>
!!
!! When <tt>keepExisting=.false.</tt> and no fill value is specified,
!! the resulting values are unspecified<sup>2</sup>.
!!
!! <b>Example usage:</b>\code
!!   integer, allocatable :: iarr(:), itens(:,:,:)
!!   call realloc(iarr, 100)
!!   call realloc(iarr, 1000, fill = -1, keepExisting=.false.)
!!   allocate(itens(10,20,30))
!!   call realloc(itens, (/ 100, 200, 300 /), fill = 0)
!! \endcode
!!
!! \param[in,out] arr Array (up to rank 4) to be reallocated.
!! \param[in]     uindex Desired new size (upper index) for array, scalar
!!      when arr has rank 1, or rank 1 array with size ra when arr
!!      has rank ra>1.
!! \param[in]     lindex (optional) Lower index for new array, defaults
!!      to lindex(1:ra)==1.
!! \param[out]   stat (optional) Result status of allocate command for the
!!      array.
!! \param[in]     fill (optional) Scalar value to fill any empty spots in
!!      the new array. Empty spots occur when the new size is larger than
!!      the old size, or when keepExisting==.false.
!! \param[in]     shift (optional) Shift original data by this increment in
!!      the new array, defaults to shift(1:ra)==0.
!! \param[in]     keepExisting (optional) Whether to preserve the original
!!      data in arr (defaults to .true.). When set to .false. and the
!!      parameter fill is not present, the resulting data is unspecified.
!!
!! <small>(1. Although the Intel compiler is able to
!! distinguish interfaces with allocatable and pointer arrays, the official
!! FORTRAN 2003 standard does not support distinguishing interfaces based on the
!! allocatable/pointer attribute; therefore, the two sets of routines have
!! been put into separate interfaces. The routine syntax is identical for
!! realloc and reallocP.)</small>
!!
!! <small>(2. When the array size remains identical to the original and
!! \a keepExisting is either true or false, and \a fill is not present
!! the original array is preserved anyway, to prevent unnecessary assignments.
!! This is not a guaranteed feature and is subject to change.)</small>
   interface realloc
      module procedure reallocInt
      module procedure reallocInt2
      module procedure reallocInt2x
      module procedure reallocInt3
      module procedure reallocInt4
      module procedure reallocCharacter
      module procedure reallocCharacter2
      module procedure reallocCharacter2x
      module procedure reallocCharacter3
      module procedure reallocCharacter4
      module procedure reallocString
      module procedure reallocReal
      module procedure reallocReal2
      module procedure reallocReal2x
      module procedure reallocReal3
      module procedure reallocReal3x
      module procedure reallocReal4
      module procedure reallocDouble
      module procedure reallocDouble2
      module procedure reallocDouble2x
      module procedure reallocDouble3
      module procedure reallocDouble4
      module procedure reallocLogical
      module procedure reallocLogical2
      module procedure reallocLogical3
      module procedure reallocLogical4
      module procedure reallocBool
      module procedure reallocBool2
      module procedure reallocBool3
      module procedure reallocBool4
      module procedure reallocByte2
   end interface

!> Reallocates memory for an existing \a pointer array. behaviour and arguments
!! are identical to \ref m_alloc::realloc.
   interface reallocP
      module procedure reallocPInt
      module procedure reallocPInt2
      module procedure reallocPInt3
      module procedure reallocPInt4
      module procedure reallocPCharacter
      module procedure reallocPCharacter2
      module procedure reallocPCharacter3
      module procedure reallocPCharacter4
      module procedure reallocPReal
      module procedure reallocPReal2
      module procedure reallocPReal3
      module procedure reallocPReal4
      module procedure reallocPDouble
      module procedure reallocPDouble2
      module procedure reallocPDouble3
      module procedure reallocPDouble4
      module procedure reallocPLogical
      module procedure reallocPLogical2
      module procedure reallocPLogical3
      module procedure reallocPLogical4
      module procedure reallocPBool
      module procedure reallocPBool2
      module procedure reallocPBool3
      module procedure reallocPBool4
   end interface

   interface reserve_sufficient_space
      module procedure reserve_sufficient_space_int
   end interface

   interface allocSize
      module procedure allocSizeDouble
   end interface

contains
!
!
!
!===============================================================================
!> Emit an allocation error message *if* an allocation error has occurred.
!! The error message goes through the MessageHandling output channels, as configured by the calling application.
   subroutine aerr(name, iostat, isize, errmsg)
      use MessageHandling, only: msgbuf, dbg_flush, err_flush
      use precision

      character(len=*), intent(in) :: name !< Name of the allocated array(s) or other description.
      integer, intent(in) :: iostat !< IO status as returned by ALLOCATE(..stat=iostat) statement. When zero, do nothing.
      integer, intent(in) :: isize !< Size (nr of bytes divided by 8) of original ALLOCATE statement (i.e., for double precision arrays simply the array length).
      character(len=*), intent(in), optional :: errmsg !< Optional error message as returned by ALLOCATE(..errmsg=errormsg) statement

      real(kind=hp), save :: rmemtot = 0d0

      integer :: i3

      if (iostat == 0) then
!$OMP CRITICAL
         rmemtot = rmemtot + isize
         i3 = 8 * rmemtot * 1e-6 ! convert size (in double/8 byte units) to megabytes
         if (abs(isize) > 1000) then
            write (msgbuf, *) i3, isize * 1e-6, ' ', trim(name)
            call dbg_flush()
         end if
!$OMP END CRITICAL
      else
         if (present(errmsg)) then
            write (msgbuf, *) ' Allocation Error: ', trim(name), ', Allocate status = ', iostat, &
               ', Integer parameter = ', isize, '=>', trim(errmsg)
         else
            write (msgbuf, *) ' Allocation Error: ', trim(name), ', Allocate status = ', iostat, ', Integer parameter = ', isize
         end if
         call err_flush()
      end if

   end subroutine aerr

!> Determines size of an allocatable array, returning 0 when it is not allocated.
   function allocSizeDouble(arr) result(isize)
      implicit none
      double precision, allocatable, intent(inout) :: arr(:) !< Array for which the extent must be determined. Is allowed to be not allocated.
      integer :: isize !< Array length, 0 when it was not allocated.

      if (allocated(arr)) then
         isize = size(arr)
      else
         isize = 0
      end if
   end function allocSizeDouble

!> Allocate or reallocate an integer array. At first the size will be set to 10, in case of a realloc
!! the size of the array is doubled.
   subroutine reserve_sufficient_space_int(arr, required_size, fill)
      integer, allocatable, dimension(:), intent(inout) :: arr !< Array for which the resize might be required.
      integer, intent(in) :: required_size !< Minimal required size of the array.
      integer, intent(in) :: fill !< Fill value for the new values.

      integer length
      if (allocated(arr)) then
         if (required_size > size(arr)) then
            length = max(required_size, 2 * size(arr))
            !call realloc(arr, length, fill=fill, keepexisting=.true.)
         end if
      else
         length = max(required_size, 10)
         !call realloc(arr, length, fill=fill)
      end if
   end subroutine reserve_sufficient_space_int

!> Helper function to fill a string
   subroutine fill_string(string, fill, fill_offset)
      implicit none
      character(len=*), intent(inout) :: string
      character(len=*), intent(in) :: fill
      integer, intent(in) :: fill_offset

      integer :: string_size, fill_size, fill_offset_, i
      character(len=len(fill)) :: rotated_fill

      string_size = len(string)
      fill_size = len(fill)

      fill_offset_ = modulo(fill_offset, fill_size)
      rotated_fill(1:fill_size - fill_offset_) = fill(1 + fill_offset_:fill_size)
      rotated_fill(fill_size - fill_offset_ + 1:fill_size) = fill(1:fill_offset_)

      do i = 1, string_size, fill_size
         string(i:min(i + fill_size - 1, string_size)) = rotated_fill(1:min(fill_size, string_size - i + 1))
      end do
   end subroutine fill_string

!> Reallocates a single allocatable string.
!! NOTE: Do not confuse this with an allocatable array of strings!
   subroutine reallocString(string, newlen, stat, fill, shift, keepExisting)
      implicit none
      character(len=:), allocatable, intent(inout) :: string
      integer, intent(in) :: newlen
      integer, intent(out), optional :: stat
      character(len=*), intent(in), optional :: fill
      integer, intent(in), optional :: shift
      logical, intent(in), optional :: keepExisting

      character(len=:), allocatable :: temp
      integer :: original_size, data_l_index, data_u_index, shift_, new_size
      integer :: local_err
      logical :: keepExisting_
      logical :: equal_bounds
      logical :: fill_available

      if (present(shift)) then
         shift_ = shift
      else
         shift_ = 0
      end if

      if (present(keepExisting)) then
         keepExisting_ = keepExisting
      else
         keepExisting_ = .true.
      end if

      if (present(fill)) then
         fill_available = (len(fill) /= 0)
      else
         fill_available = .false.
      end if

      new_size = max(0, newlen)

      local_err = 0
      if (allocated(string)) then
         original_size = len(string)
         if (original_size == new_size .and. shift_ == 0) then
            if (.not. keepExisting_ .and. fill_available) then
               call fill_string(string, fill, 0)
            end if
            if (present(stat)) stat = 0
            return
         end if
      end if

      allocate (character(len=new_size) :: temp, stat=local_err)
      if (local_err /= 0) then
         goto 999
      end if

      if (keepExisting_ .and. allocated(string)) then
         data_l_index = max(1 + shift_, 1)
         data_u_index = min(original_size + shift_, new_size)
         ! string access below is safe, because:
         ! data_l_index - shift_ >= (1 + shift_) - shift_ = 1
         ! data_u_index - shift_ <= (original_size + shift_) - shift_ = original_size
         temp(data_l_index:data_u_index) = string(data_l_index - shift_:data_u_index - shift_)
         if (fill_available) then
            call fill_string(temp(1:data_l_index - 1), fill, 0)
            call fill_string(temp(data_u_index + 1:new_size), fill, data_u_index)
         end if
      elseif (fill_available) then
         call fill_string(temp, fill, 0)
      end if
      call move_alloc(temp, string)
999   continue
      if (present(stat)) then
         stat = local_err
      end if
   end subroutine reallocString
!
!===============================================================================
! Rank 2x/3x convenience wrappers (scalar dimension arguments)
!
!===============================================================================
   subroutine reallocReal2x(arr, u1, u2, l1, l2, stat, keepExisting)
      implicit none
      real, allocatable, intent(inout) :: arr(:, :)
      integer, intent(in) :: u1, u2
      integer, intent(in), optional :: l1, l2
      integer, intent(out), optional :: stat
      logical, intent(in), optional :: keepExisting
      integer :: uindex(2), lindex(2)
      uindex = (/u1, u2/)
      if (present(l1)) then
         lindex = (/l1, l2/)
         call reallocReal2(arr, uindex, lindex, stat=stat, keepExisting=keepExisting)
      else
         call reallocReal2(arr, uindex, stat=stat, keepExisting=keepExisting)
      end if
   end subroutine reallocReal2x

   subroutine reallocDouble2x(arr, u1, u2, l1, l2, stat)
      implicit none
      double precision, allocatable, intent(inout) :: arr(:, :)
      integer, intent(in) :: u1, u2
      integer, intent(in), optional :: l1, l2
      integer, intent(out), optional :: stat
      integer :: uindex(2), lindex(2)
      uindex = (/u1, u2/)
      if (present(l1)) then
         lindex = (/l1, l2/)
         call reallocDouble2(arr, uindex, lindex, stat=stat)
      else
         call reallocDouble2(arr, uindex, stat=stat)
      end if
   end subroutine reallocDouble2x

   subroutine reallocInt2x(arr, u1, u2, l1, l2, stat)
      implicit none
      integer, allocatable, intent(inout) :: arr(:, :)
      integer, intent(in) :: u1, u2
      integer, intent(in), optional :: l1, l2
      integer, intent(out), optional :: stat
      integer :: uindex(2), lindex(2)
      uindex = (/u1, u2/)
      if (present(l1)) then
         lindex = (/l1, l2/)
         call reallocInt2(arr, uindex, lindex, stat=stat)
      else
         call reallocInt2(arr, uindex, stat=stat)
      end if
   end subroutine reallocInt2x

   subroutine reallocCharacter2x(arr, u1, u2, l1, l2, stat)
      implicit none
      character(len=*), allocatable, intent(inout) :: arr(:, :)
      integer, intent(in) :: u1, u2
      integer, intent(in), optional :: l1, l2
      integer, intent(out), optional :: stat
      integer :: uindex(2), lindex(2)
      uindex = (/u1, u2/)
      if (present(l1)) then
         lindex = (/l1, l2/)
         call reallocCharacter2(arr, uindex, lindex, stat=stat)
      else
         call reallocCharacter2(arr, uindex, stat=stat)
      end if
   end subroutine reallocCharacter2x

   subroutine reallocReal3x(arr, u1, u2, u3, l1, l2, l3, stat)
      implicit none
      real, allocatable, intent(inout) :: arr(:, :, :)
      integer, intent(in) :: u1, u2, u3
      integer, intent(in), optional :: l1, l2, l3
      integer, intent(out), optional :: stat
      integer :: uindex(3), lindex(3)
      uindex = (/u1, u2, u3/)
      if (present(l1)) then
         lindex = (/l1, l2, l3/)
         call reallocReal3(arr, uindex, lindex, stat=stat)
      else
         call reallocReal3(arr, uindex, stat=stat)
      end if
   end subroutine reallocReal3x
!
!===============================================================================
!
!===============================================================================
! Rank 1 - shared macros
!
#define DRANK (:)
#define DINDEX integer
#define ALLOCATE_TEMP allocate(temp(new_l_index:new_u_index))
#define OVERLAP_NONEMPTY data_l_index <= data_u_index
#define COPY_SECTION temp(data_l_index:data_u_index) = arr(data_l_index - shift_:data_u_index - shift_)
#define BOUNDS_UNCHANGED new_l_index == old_l_index .and. new_u_index == old_u_index .and. shift_ == 0
#define GET_BOUNDS old_l_index = lbound(arr, 1); old_u_index = ubound(arr, 1)

!
!===============================================================================
! Rank 1 - allocatable
!
#define DATTR allocatable
#define IS_ALLOCATED(x) allocated(x)
#define MOVE_ALLOC call move_alloc(temp, arr)
!
!===============================================================================
   subroutine reallocDouble(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE double precision
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocDouble

   subroutine reallocReal(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE real
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocReal

   subroutine reallocInt(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE integer
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocInt

   subroutine reallocLogical(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE logical
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocLogical

   subroutine reallocCharacter(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE character(len=*)
#define DTYPE_FILL character(len=*)
#define DTYPE_TEMP character(len=len(arr))
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
#undef DTYPE_FILL
#undef DTYPE_TEMP
   end subroutine reallocCharacter

   subroutine reallocBool(arr, uindex, lindex, stat, fill, shift, keepExisting)
      use stdlib_kinds, only: c_bool
#define DTYPE logical(kind=c_bool)
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocBool
!
!===============================================================================
! Rank 1 - pointer
!
#undef DATTR
#undef IS_ALLOCATED
#undef MOVE_ALLOC
#define DATTR pointer
#define IS_ALLOCATED(x) associated(x)
#define MOVE_ALLOC if (associated(arr)) deallocate(arr); arr => temp
!
!===============================================================================
   subroutine reallocPDouble(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE double precision
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPDouble

   subroutine reallocPReal(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE real
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPReal

   subroutine reallocPInt(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE integer
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPInt

   subroutine reallocPLogical(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE logical
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPLogical

   subroutine reallocPCharacter(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE character(len=*)
#define DTYPE_FILL character(len=*)
#define DTYPE_TEMP character(len=len(arr))
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
#undef DTYPE_FILL
#undef DTYPE_TEMP
   end subroutine reallocPCharacter

   subroutine reallocPBool(arr, uindex, lindex, stat, fill, shift, keepExisting)
      use stdlib_kinds, only: c_bool
#define DTYPE logical(kind=c_bool)
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPBool
!
!===============================================================================
! End rank 1
!
#undef DATTR
#undef IS_ALLOCATED
#undef MOVE_ALLOC
#undef DINDEX
#undef ALLOCATE_TEMP
#undef OVERLAP_NONEMPTY
#undef COPY_SECTION
#undef BOUNDS_UNCHANGED
#undef DRANK
#undef GET_BOUNDS
!
!===============================================================================
!
!===============================================================================
! Rank 2 - shared macros
!
#define DRANK (:,:)
#define DINDEX integer, dimension(2)
#define ALLOCATE_TEMP allocate(temp(new_l_index(1):new_u_index(1), new_l_index(2):new_u_index(2)))
#define OVERLAP_NONEMPTY all(data_l_index <= data_u_index)
#define COPY_SECTION temp(data_l_index(1):data_u_index(1), data_l_index(2):data_u_index(2)) = arr(data_l_index(1) - shift_(1):data_u_index(1) - shift_(1), data_l_index(2) - shift_(2):data_u_index(2) - shift_(2))
#define BOUNDS_UNCHANGED all(new_l_index == old_l_index) .and. all(new_u_index == old_u_index) .and. all(shift_ == 0)
#define GET_BOUNDS old_l_index = lbound(arr); old_u_index = ubound(arr)
!
!===============================================================================
! Rank 2 - allocatable
!
#define DATTR allocatable
#define IS_ALLOCATED(x) allocated(x)
#define MOVE_ALLOC call move_alloc(temp, arr)
!
!===============================================================================
   subroutine reallocDouble2(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE double precision
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocDouble2

   subroutine reallocReal2(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE real
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocReal2

   subroutine reallocInt2(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE integer
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocInt2

   subroutine reallocLogical2(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE logical
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocLogical2

   subroutine reallocCharacter2(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE character(len=*)
#define DTYPE_TEMP character(len=len(arr))
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocCharacter2

   subroutine reallocBool2(arr, uindex, lindex, stat, fill, shift, keepExisting)
      use stdlib_kinds, only: c_bool
#define DTYPE logical(kind=c_bool)
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocBool2

   subroutine reallocByte2(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE integer(kind=1)
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocByte2
!
!===============================================================================
! Rank 2 - pointer
!
#undef DATTR
#undef IS_ALLOCATED
#undef MOVE_ALLOC
#define DATTR pointer
#define IS_ALLOCATED(x) associated(x)
#define MOVE_ALLOC if (associated(arr)) deallocate(arr); arr => temp
!
!===============================================================================
   subroutine reallocPDouble2(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE double precision
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPDouble2

   subroutine reallocPReal2(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE real
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPReal2

   subroutine reallocPInt2(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE integer
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPInt2

   subroutine reallocPLogical2(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE logical
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPLogical2

   subroutine reallocPCharacter2(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE character(len=*)
#define DTYPE_TEMP character(len=len(arr))
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPCharacter2

   subroutine reallocPBool2(arr, uindex, lindex, stat, fill, shift, keepExisting)
      use stdlib_kinds, only: c_bool
#define DTYPE logical(kind=c_bool)
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPBool2
!
!===============================================================================
! End rank 2
!
#undef DATTR
#undef IS_ALLOCATED
#undef MOVE_ALLOC
#undef DINDEX
#undef ALLOCATE_TEMP
#undef OVERLAP_NONEMPTY
#undef COPY_SECTION
#undef DRANK
!
!===============================================================================
!
!===============================================================================
! Rank 3 - shared macros
!
#define DRANK (:,:,:)
#define DINDEX integer, dimension(3)
#define ALLOCATE_TEMP allocate(temp(new_l_index(1):new_u_index(1), new_l_index(2):new_u_index(2), new_l_index(3):new_u_index(3)))
#define OVERLAP_NONEMPTY all(data_l_index <= data_u_index)
#define COPY_SECTION temp(data_l_index(1):data_u_index(1), data_l_index(2):data_u_index(2), data_l_index(3):data_u_index(3)) = arr(data_l_index(1) - shift_(1):data_u_index(1) - shift_(1), data_l_index(2) - shift_(2):data_u_index(2) - shift_(2), data_l_index(3) - shift_(3):data_u_index(3) - shift_(3))
!
!===============================================================================
! Rank 3 - allocatable
!
#define DATTR allocatable
#define IS_ALLOCATED(x) allocated(x)
#define MOVE_ALLOC call move_alloc(temp, arr)
!
!===============================================================================
   subroutine reallocDouble3(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE double precision
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocDouble3

   subroutine reallocReal3(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE real
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocReal3

   subroutine reallocInt3(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE integer
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocInt3

   subroutine reallocLogical3(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE logical
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocLogical3

   subroutine reallocCharacter3(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE character(len=*)
#define DTYPE_TEMP character(len=len(arr))
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocCharacter3

   subroutine reallocBool3(arr, uindex, lindex, stat, fill, shift, keepExisting)
      use stdlib_kinds, only: c_bool
#define DTYPE logical(kind=c_bool)
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocBool3
!
!===============================================================================
! Rank 3 - pointer
!
#undef DATTR
#undef IS_ALLOCATED
#undef MOVE_ALLOC
#define DATTR pointer
#define IS_ALLOCATED(x) associated(x)
#define MOVE_ALLOC if (associated(arr)) deallocate(arr); arr => temp
!
!===============================================================================
   subroutine reallocPDouble3(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE double precision
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPDouble3

   subroutine reallocPReal3(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE real
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPReal3

   subroutine reallocPInt3(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE integer
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPInt3

   subroutine reallocPLogical3(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE logical
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPLogical3

   subroutine reallocPCharacter3(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE character(len=*)
#define DTYPE_TEMP character(len=len(arr))
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPCharacter3

   subroutine reallocPBool3(arr, uindex, lindex, stat, fill, shift, keepExisting)
      use stdlib_kinds, only: c_bool
#define DTYPE logical(kind=c_bool)
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPBool3
!
!===============================================================================
! End rank 3
!
#undef DATTR
#undef IS_ALLOCATED
#undef MOVE_ALLOC
#undef DRANK
#undef DINDEX
#undef ALLOCATE_TEMP
#undef OVERLAP_NONEMPTY
#undef COPY_SECTION
!===============================================================================
!
!===============================================================================
! Rank 4 - shared macros
!
#define DRANK (:,:,:,:)
#define DINDEX integer, dimension(4)
#define ALLOCATE_TEMP allocate(temp(new_l_index(1):new_u_index(1), new_l_index(2):new_u_index(2), new_l_index(3):new_u_index(3), new_l_index(4):new_u_index(4)))
#define OVERLAP_NONEMPTY all(data_l_index <= data_u_index)
#define COPY_SECTION temp(data_l_index(1):data_u_index(1), data_l_index(2):data_u_index(2), data_l_index(3):data_u_index(3), data_l_index(4):data_u_index(4)) = arr(data_l_index(1) - shift_(1):data_u_index(1) - shift_(1), data_l_index(2) - shift_(2):data_u_index(2) - shift_(2), data_l_index(3) - shift_(3):data_u_index(3) - shift_(3), data_l_index(4) - shift_(4):data_u_index(4) - shift_(4))
!
!===============================================================================
! Rank 4 - allocatable
!
#define DATTR allocatable
#define IS_ALLOCATED(x) allocated(x)
#define MOVE_ALLOC call move_alloc(temp, arr)
!
!===============================================================================
   subroutine reallocDouble4(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE double precision
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocDouble4

   subroutine reallocReal4(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE real
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocReal4

   subroutine reallocInt4(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE integer
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocInt4

   subroutine reallocLogical4(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE logical
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocLogical4

   subroutine reallocCharacter4(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE character(len=*)
#define DTYPE_TEMP character(len=len(arr))
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocCharacter4

   subroutine reallocBool4(arr, uindex, lindex, stat, fill, shift, keepExisting)
      use stdlib_kinds, only: c_bool
#define DTYPE logical(kind=c_bool)
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocBool4
!
!===============================================================================
! Rank 4 - pointer
!
#undef DATTR
#undef IS_ALLOCATED
#undef MOVE_ALLOC
#define DATTR pointer
#define IS_ALLOCATED(x) associated(x)
#define MOVE_ALLOC if (associated(arr)) deallocate(arr); arr => temp
!
!===============================================================================
   subroutine reallocPDouble4(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE double precision
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPDouble4

   subroutine reallocPReal4(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE real
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPReal4

   subroutine reallocPInt4(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE integer
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPInt4

   subroutine reallocPLogical4(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE logical
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPLogical4

   subroutine reallocPCharacter4(arr, uindex, lindex, stat, fill, shift, keepExisting)
#define DTYPE character(len=*)
#define DTYPE_TEMP character(len=len(arr))
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPCharacter4

   subroutine reallocPBool4(arr, uindex, lindex, stat, fill, shift, keepExisting)
      use stdlib_kinds, only: c_bool
#define DTYPE logical(kind=c_bool)
#include "malloc_includes/malloc_body.inc"
#undef DTYPE
   end subroutine reallocPBool4
!
!===============================================================================
! End rank 4
!
#undef DATTR
#undef IS_ALLOCATED
#undef MOVE_ALLOC
#undef DRANK
#undef DINDEX
#undef ALLOCATE_TEMP
#undef OVERLAP_NONEMPTY
#undef COPY_SECTION
#undef BOUNDS_UNCHANGED
!===============================================================================
end module m_alloc
