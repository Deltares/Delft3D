module m_util_helpers
   implicit none(type, external)
   private
   public :: cstr
contains
   !> Convert a string to a null-terminated string (C style string).
   !! Helpful for the F90TW macros, because those expect strings to be null terminated.
   function cstr(string) result(res)
      use iso_c_binding, only: c_null_char
      character(len=*), intent(in) :: string
      character(len=:), allocatable :: res
      res = trim(string)//c_null_char
   end function cstr
end module m_util_helpers