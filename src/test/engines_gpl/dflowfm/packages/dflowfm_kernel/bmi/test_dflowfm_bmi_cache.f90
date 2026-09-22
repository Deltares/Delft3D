!! Copyright (C) Stichting Deltares, 2026.
!!
!! This program is free software: you can redistribute it and/or modify
!! it under the terms of the GNU General Public License version 3,
!! as published by the Free Software Foundation.

program test_dflowfm_bmi_cache
   use iso_c_binding, only: c_char, c_double, c_f_pointer, c_int, c_loc, c_null_char, c_ptr, c_null_ptr
   implicit none

   integer, parameter :: maxstrlen = 1024
   integer, parameter :: maxdims = 6
   real(c_double), parameter :: updated_level = 123.456789_c_double
   real(c_double), parameter :: tolerance = 1.0e-10_c_double

   interface
      integer(c_int) function initialize(c_config_file) bind(C, name="initialize")
         use iso_c_binding, only: c_char, c_int
         character(kind=c_char), intent(in) :: c_config_file(1024)
      end function initialize

      integer(c_int) function finalize() bind(C, name="finalize")
         use iso_c_binding, only: c_int
      end function finalize

      subroutine get_var_shape(c_var_name, shape) bind(C, name="get_var_shape")
         use iso_c_binding, only: c_char, c_int
         character(kind=c_char), intent(in) :: c_var_name(*)
         integer(c_int), intent(inout) :: shape(6)
      end subroutine get_var_shape

      subroutine get_var(c_var_name, x) bind(C, name="get_var")
         use iso_c_binding, only: c_char, c_ptr
         character(kind=c_char), intent(in) :: c_var_name(*)
         type(c_ptr), intent(inout) :: x
      end subroutine get_var

      subroutine set_var(c_var_name, xptr) bind(C, name="set_var")
         use iso_c_binding, only: c_char, c_ptr
         character(kind=c_char), intent(in) :: c_var_name(*)
         type(c_ptr), value, intent(in) :: xptr
      end subroutine set_var
   end interface

   character(kind=c_char), dimension(maxstrlen) :: config_file
   character(kind=c_char), dimension(maxstrlen) :: s1_name
   character(kind=c_char), dimension(maxstrlen) :: observation_name
   integer(c_int), dimension(maxdims) :: shape
   real(c_double), dimension(:), allocatable, target :: updated_s1
   real(c_double), pointer :: observation_value
   type(c_ptr) :: observation_ptr
   real(c_double) :: value_before
   real(c_double) :: value_after
   integer(c_int) :: ierr

   call to_c_string('Flow1d.mdu', config_file)
   call to_c_string('s1', s1_name)
   call to_c_string('observations/TestLocation1/water_level', observation_name)

   ierr = initialize(config_file)
   if (ierr /= 0_c_int) then
      write (*, '(a,i0)') 'D-Flow FM BMI initialize failed with code ', ierr
      error stop 1
   end if

   observation_ptr = c_null_ptr
   call get_var(observation_name, observation_ptr)
   if (.not. c_associated(observation_ptr)) then
      write (*, '(a)') 'Failed to retrieve initial observation water level through BMI.'
      error stop 1
   end if
   call c_f_pointer(observation_ptr, observation_value)
   value_before = observation_value

   shape = 0_c_int
   call get_var_shape(s1_name, shape)
   if (shape(1) <= 0_c_int) then
      write (*, '(a,i0)') 'Invalid BMI shape for s1: ', shape(1)
      error stop 1
   end if

   allocate(updated_s1(shape(1)))
   updated_s1 = updated_level
   call set_var(s1_name, c_loc(updated_s1(1)))

   observation_ptr = c_null_ptr
   call get_var(observation_name, observation_ptr)
   if (.not. c_associated(observation_ptr)) then
      write (*, '(a)') 'Failed to retrieve observation water level after BMI mutation.'
      error stop 1
   end if
   call c_f_pointer(observation_ptr, observation_value)
   value_after = observation_value

   if (abs(value_after - updated_level) > tolerance) then
      write (*, '(a,es24.16)') 'Observation did not reflect same-time BMI mutation. Before: ', value_before
      write (*, '(a,es24.16)') 'Expected after mutation: ', updated_level
      write (*, '(a,es24.16)') 'Actual after mutation:   ', value_after
      error stop 1
   end if

   if (abs(value_after - value_before) <= tolerance) then
      write (*, '(a)') 'Observation value remained unchanged after BMI mutation.'
      error stop 1
   end if

   ierr = finalize()
   if (ierr /= 0_c_int) then
      write (*, '(a,i0)') 'D-Flow FM BMI finalize failed with code ', ierr
      error stop 1
   end if

   write (*, '(a)') 'BMI observation cache invalidation regression test passed.'

contains

   subroutine to_c_string(source, target)
      character(len=*), intent(in) :: source
      character(kind=c_char), dimension(:), intent(out) :: target
      integer :: n

      target = c_null_char
      n = min(len_trim(source), size(target) - 1)
      target(1:n) = transfer(source(1:n), target(1:n))
   end subroutine to_c_string

end program test_dflowfm_bmi_cache
