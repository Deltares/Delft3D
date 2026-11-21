!> String conversion and list marshalling helpers for the Fortran wrapper.
module dflowfm_io_helpers
   use, intrinsic :: iso_c_binding
   use dflowfm_io_c, only: c_mdu_set_path_list, c_mdu_set_string_list
   implicit none(type, external)
   private

   public :: set_string_list_impl, f_to_c_string, c_string_to_f, c_string_array_to_f

contains

   !> Copy a Fortran list-of-strings into a local C pointer array and call the
   !! string- or path-list setter, returning the result code.
   function set_string_list_impl(is_path, handle, key, values) result(res)
      logical, intent(in) :: is_path !< Select the path-list setter instead of the string-list setter.
      type(c_ptr), intent(in) :: handle !< Model handle.
      character(len=*), intent(in) :: key !< Property key.
      character(len=*), dimension(:), intent(in) :: values !< Strings to copy.
      integer :: res
      character(len=:), dimension(:), allocatable, target :: buffers
      type(c_ptr), dimension(:), allocatable :: ptrs
      integer :: i, n

      n = size(values)
      allocate (character(len=len_trim_max(values) + 1) :: buffers(max(n, 1)))
      allocate (ptrs(max(n, 1)))
      do i = 1, n
         buffers(i) = trim(values(i))//c_null_char
         ptrs(i) = c_loc(buffers(i))
      end do

      if (is_path) then
         res = int(c_mdu_set_path_list(handle, f_to_c_string(key), ptrs, int(n, c_int64_t)))
      else
         res = int(c_mdu_set_string_list(handle, f_to_c_string(key), ptrs, int(n, c_int64_t)))
      end if
   end function

   pure function len_trim_max(values) result(max_len)
      character(len=*), dimension(:), intent(in) :: values
      integer :: max_len, i

      max_len = 0
      do i = 1, size(values)
         max_len = max(max_len, len_trim(values(i)))
      end do
   end function

   !> Append a null terminator to a Fortran string for passing to C.
   pure function f_to_c_string(str) result(c_str)
      character(len=*), intent(in) :: str !< String to trim and terminate.
      character(len=len_trim(str) + 1, kind=c_char) :: c_str

      c_str = trim(str)//c_null_char
   end function

   !> Copy a null-terminated C string (valid until the next C call) into an
   !! allocatable Fortran string.
   function c_string_to_f(cptr) result(str)
      type(c_ptr), intent(in) :: cptr !< Borrowed C string pointer, or null for an empty string.
      character(len=:), allocatable :: str
      character(kind=c_char), dimension(:), pointer :: chars
      integer :: length, i

      if (.not. c_associated(cptr)) then
         str = ''
         return
      end if

      length = c_strlen(cptr)
      allocate (character(len=length) :: str)
      if (length > 0) then
         call c_f_pointer(cptr, chars, [length])
         do i = 1, length
            str(i:i) = chars(i)
         end do
      end if
   end function

   !> Copy a C array of null-terminated strings into a Fortran string array.
   function c_string_array_to_f(list_ptr, n) result(values)
      type(c_ptr), intent(in) :: list_ptr !< Borrowed C string-pointer array, or null for an empty list.
      integer(c_int64_t), intent(in) :: n !< Number of strings in the array.
      character(len=:), dimension(:), allocatable :: values
      type(c_ptr), dimension(:), pointer :: ptrs
      integer :: i, count, max_len

      count = int(n)
      if (.not. c_associated(list_ptr) .or. count <= 0) then
         allocate (character(len=0) :: values(0))
         return
      end if

      call c_f_pointer(list_ptr, ptrs, [count])
      max_len = 0
      do i = 1, count
         max_len = max(max_len, c_strlen(ptrs(i)))
      end do

      allocate (character(len=max_len) :: values(count))
      do i = 1, count
         values(i) = c_string_to_f(ptrs(i))
      end do
   end function

   function c_strlen(cptr) result(length)
      type(c_ptr), intent(in) :: cptr
      integer :: length
      character(kind=c_char), dimension(:), pointer :: chars
      integer, parameter :: MAX_LEN = 1048576

      length = 0
      if (.not. c_associated(cptr)) then
         return
      end if
      call c_f_pointer(cptr, chars, [MAX_LEN])
      do
         if (length >= MAX_LEN) then
            exit
         end if
         if (chars(length + 1) == c_null_char) then
            exit
         end if
         length = length + 1
      end do
   end function

end module dflowfm_io_helpers
