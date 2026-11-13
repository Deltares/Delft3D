!> C interoperability declarations for the dflowfm_io API.
module dflowfm_io_c
   use, intrinsic :: iso_c_binding
   implicit none(type, external)
   private

   public :: c_mdu_create, c_mdu_destroy
   public :: c_mdu_load_from_file, c_mdu_load_from_string
   public :: c_mdu_save_to_file, c_mdu_save_to_string
   public :: c_mdu_get_int, c_mdu_get_bool, c_mdu_get_double
   public :: c_mdu_get_string, c_mdu_get_path, c_mdu_get_datetime
   public :: c_mdu_get_int_enum, c_mdu_get_string_enum
   public :: c_mdu_get_string_list, c_mdu_get_path_list, c_mdu_get_double_list
   public :: c_mdu_set_int, c_mdu_set_bool, c_mdu_set_double
   public :: c_mdu_set_string, c_mdu_set_path, c_mdu_set_datetime
   public :: c_mdu_set_int_enum, c_mdu_set_string_enum
   public :: c_mdu_set_string_list, c_mdu_set_path_list, c_mdu_set_double_list
   public :: c_mdu_get_issue_list, c_dflowfm_io_get_last_error

   !> Interoperable representation of the C mdu_issue_t structure.
   type, bind(C), public :: c_mdu_issue_t
      integer(c_int32_t) :: line_number !< Source line number.
      integer(c_int32_t) :: severity !< Native diagnostic severity.
      type(c_ptr) :: message !< Borrowed null-terminated diagnostic message.
   end type c_mdu_issue_t

   interface
      !> C binding for creating a model handle.
      function c_mdu_create(out_handle) result(res) bind(C, name="mdu_create")
         import :: c_ptr, c_int32_t
         type(c_ptr), intent(out) :: out_handle !< Created model handle.
         integer(c_int32_t) :: res
      end function

      !> C binding for destroying a model handle.
      function c_mdu_destroy(handle) result(res) bind(C, name="mdu_destroy")
         import :: c_ptr, c_int32_t
         type(c_ptr), intent(inout) :: handle !< Model handle to release.
         integer(c_int32_t) :: res
      end function

      !> C binding for loading a model file.
      function c_mdu_load_from_file(handle, filename) result(res) bind(C, name="mdu_load_from_file")
         import :: c_ptr, c_char, c_int32_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: filename !< Null-terminated filename.
         integer(c_int32_t) :: res
      end function

      !> C binding for loading model text.
      function c_mdu_load_from_string(handle, data, size) result(res) bind(C, name="mdu_load_from_string")
         import :: c_ptr, c_char, c_int32_t, c_int64_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: data !< Model text.
         integer(c_int64_t), value, intent(in) :: size !< Text length.
         integer(c_int32_t) :: res
      end function

      !> C binding for saving a model file.
      function c_mdu_save_to_file(handle, filename) result(res) bind(C, name="mdu_save_to_file")
         import :: c_ptr, c_char, c_int32_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: filename !< Null-terminated filename.
         integer(c_int32_t) :: res
      end function

      !> C binding for serializing a model to borrowed text.
      function c_mdu_save_to_string(handle, out_data) result(res) bind(C, name="mdu_save_to_string")
         import :: c_ptr, c_int32_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         type(c_ptr), intent(out) :: out_data !< Borrowed null-terminated model text.
         integer(c_int32_t) :: res
      end function

      !> C binding for reading an integer property.
      function c_mdu_get_int(handle, key, out_value) result(res) bind(C, name="mdu_get_int")
         import :: c_ptr, c_char, c_int32_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         integer(c_int32_t), intent(out) :: out_value !< Property value.
         integer(c_int32_t) :: res
      end function

      !> C binding for reading a Boolean property.
      function c_mdu_get_bool(handle, key, out_value) result(res) bind(C, name="mdu_get_bool")
         import :: c_ptr, c_char, c_int32_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         integer(c_int32_t), intent(out) :: out_value !< Integer Boolean value.
         integer(c_int32_t) :: res
      end function

      !> C binding for reading a double property.
      function c_mdu_get_double(handle, key, out_value) result(res) bind(C, name="mdu_get_double")
         import :: c_ptr, c_char, c_int32_t, c_double
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         real(c_double), intent(out) :: out_value !< Property value.
         integer(c_int32_t) :: res
      end function

      !> C binding for reading a borrowed string property.
      function c_mdu_get_string(handle, key, out_value) result(res) bind(C, name="mdu_get_string")
         import :: c_ptr, c_char, c_int32_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         type(c_ptr), intent(out) :: out_value !< Borrowed null-terminated property value.
         integer(c_int32_t) :: res
      end function

      !> C binding for reading a borrowed path property.
      function c_mdu_get_path(handle, key, out_value) result(res) bind(C, name="mdu_get_path")
         import :: c_ptr, c_char, c_int32_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         type(c_ptr), intent(out) :: out_value !< Borrowed null-terminated path.
         integer(c_int32_t) :: res
      end function

      !> C binding for reading an optional datetime property.
      function c_mdu_get_datetime(handle, key, out_value, out_has_value) result(res) bind(C, name="mdu_get_datetime")
         import :: c_ptr, c_char, c_int32_t, c_int64_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         integer(c_int64_t), intent(out) :: out_value !< Epoch value.
         integer(c_int32_t), intent(out) :: out_has_value !< Whether a datetime is set.
         integer(c_int32_t) :: res
      end function

      !> C binding for reading an integer enum property.
      function c_mdu_get_int_enum(handle, key, out_value) result(res) bind(C, name="mdu_get_int_enum")
         import :: c_ptr, c_char, c_int32_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         integer(c_int32_t), intent(out) :: out_value !< Enum value.
         integer(c_int32_t) :: res
      end function

      !> C binding for reading a borrowed string enum property.
      function c_mdu_get_string_enum(handle, key, out_value) result(res) bind(C, name="mdu_get_string_enum")
         import :: c_ptr, c_char, c_int32_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         type(c_ptr), intent(out) :: out_value !< Borrowed null-terminated enum value.
         integer(c_int32_t) :: res
      end function

      !> C binding for reading a borrowed string list.
      function c_mdu_get_string_list(handle, key, out_list, out_size) result(res) bind(C, name="mdu_get_string_list")
         import :: c_ptr, c_char, c_int32_t, c_int64_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         type(c_ptr), intent(out) :: out_list !< Borrowed array of string pointers.
         integer(c_int64_t), intent(out) :: out_size !< Number of elements.
         integer(c_int32_t) :: res
      end function

      !> C binding for reading a borrowed path list.
      function c_mdu_get_path_list(handle, key, out_list, out_size) result(res) bind(C, name="mdu_get_path_list")
         import :: c_ptr, c_char, c_int32_t, c_int64_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         type(c_ptr), intent(out) :: out_list !< Borrowed array of path pointers.
         integer(c_int64_t), intent(out) :: out_size !< Number of elements.
         integer(c_int32_t) :: res
      end function

      !> C binding for reading a borrowed double list.
      function c_mdu_get_double_list(handle, key, out_list, out_size) result(res) bind(C, name="mdu_get_double_list")
         import :: c_ptr, c_char, c_int32_t, c_int64_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         type(c_ptr), intent(out) :: out_list !< Borrowed array of doubles.
         integer(c_int64_t), intent(out) :: out_size !< Number of elements.
         integer(c_int32_t) :: res
      end function

      !> C binding for writing an integer property.
      function c_mdu_set_int(handle, key, value) result(res) bind(C, name="mdu_set_int")
         import :: c_ptr, c_char, c_int32_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         integer(c_int32_t), value, intent(in) :: value !< Property value.
         integer(c_int32_t) :: res
      end function

      !> C binding for writing a Boolean property.
      function c_mdu_set_bool(handle, key, value) result(res) bind(C, name="mdu_set_bool")
         import :: c_ptr, c_char, c_int32_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         integer(c_int32_t), value, intent(in) :: value !< Integer Boolean value.
         integer(c_int32_t) :: res
      end function

      !> C binding for writing a double property.
      function c_mdu_set_double(handle, key, value) result(res) bind(C, name="mdu_set_double")
         import :: c_ptr, c_char, c_int32_t, c_double
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         real(c_double), value, intent(in) :: value !< Property value.
         integer(c_int32_t) :: res
      end function

      !> C binding for writing a string property.
      function c_mdu_set_string(handle, key, value) result(res) bind(C, name="mdu_set_string")
         import :: c_ptr, c_char, c_int32_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         character(kind=c_char), dimension(*), intent(in) :: value !< Null-terminated property value.
         integer(c_int32_t) :: res
      end function

      !> C binding for writing a path property.
      function c_mdu_set_path(handle, key, value) result(res) bind(C, name="mdu_set_path")
         import :: c_ptr, c_char, c_int32_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         character(kind=c_char), dimension(*), intent(in) :: value !< Null-terminated path.
         integer(c_int32_t) :: res
      end function

      !> C binding for writing an optional datetime property.
      function c_mdu_set_datetime(handle, key, epoch, has_value) result(res) bind(C, name="mdu_set_datetime")
         import :: c_ptr, c_char, c_int32_t, c_int64_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         integer(c_int64_t), value, intent(in) :: epoch !< Epoch value.
         integer(c_int32_t), value, intent(in) :: has_value !< Whether a datetime is set.
         integer(c_int32_t) :: res
      end function

      !> C binding for writing an integer enum property.
      function c_mdu_set_int_enum(handle, key, value) result(res) bind(C, name="mdu_set_int_enum")
         import :: c_ptr, c_char, c_int32_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         integer(c_int32_t), value, intent(in) :: value !< Enum value.
         integer(c_int32_t) :: res
      end function

      !> C binding for writing a string enum property.
      function c_mdu_set_string_enum(handle, key, value) result(res) bind(C, name="mdu_set_string_enum")
         import :: c_ptr, c_char, c_int32_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         character(kind=c_char), dimension(*), intent(in) :: value !< Null-terminated enum value.
         integer(c_int32_t) :: res
      end function

      !> C binding for writing a string list.
      function c_mdu_set_string_list(handle, key, list, size) result(res) bind(C, name="mdu_set_string_list")
         import :: c_ptr, c_char, c_int32_t, c_int64_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         type(c_ptr), dimension(*), intent(in) :: list !< Array of null-terminated string pointers.
         integer(c_int64_t), value, intent(in) :: size !< Number of elements.
         integer(c_int32_t) :: res
      end function

      !> C binding for writing a path list.
      function c_mdu_set_path_list(handle, key, list, size) result(res) bind(C, name="mdu_set_path_list")
         import :: c_ptr, c_char, c_int32_t, c_int64_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         type(c_ptr), dimension(*), intent(in) :: list !< Array of null-terminated path pointers.
         integer(c_int64_t), value, intent(in) :: size !< Number of elements.
         integer(c_int32_t) :: res
      end function

      !> C binding for writing a double list.
      function c_mdu_set_double_list(handle, key, list, size) result(res) bind(C, name="mdu_set_double_list")
         import :: c_ptr, c_char, c_int32_t, c_int64_t, c_double
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         character(kind=c_char), dimension(*), intent(in) :: key !< Null-terminated dotted key.
         real(c_double), dimension(*), intent(in) :: list !< Array of doubles.
         integer(c_int64_t), value, intent(in) :: size !< Number of elements.
         integer(c_int32_t) :: res
      end function

      !> C binding for reading borrowed diagnostic issues.
      function c_mdu_get_issue_list(handle, out_list, out_size) result(res) bind(C, name="mdu_get_issue_list")
         import :: c_ptr, c_int32_t, c_int64_t
         type(c_ptr), value, intent(in) :: handle !< Model handle.
         type(c_ptr), intent(out) :: out_list !< Borrowed array of C diagnostic issues.
         integer(c_int64_t), intent(out) :: out_size !< Number of issues.
         integer(c_int32_t) :: res
      end function

      !> C binding for reading the borrowed last-error message.
      function c_dflowfm_io_get_last_error() result(ptr) bind(C, name="dflowfm_io_get_last_error")
         import :: c_ptr
         type(c_ptr) :: ptr
      end function
   end interface

end module dflowfm_io_c