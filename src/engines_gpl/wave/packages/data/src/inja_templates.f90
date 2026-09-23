module inja_templates
   use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr
   implicit none(type, external)
   private

   public :: inja_create_context
   public :: inja_add_string
   public :: inja_destroy_context
   public :: inja_render_file

   interface
      !> Creates an empty persistent context for inja template rendering.
      function inja_create_context() result(context) bind(C, name="inja_create_context")
         import :: c_ptr
         type(c_ptr) :: context
      end function inja_create_context

      !> Adds or replaces a string value in the inja context.
      function inja_add_string(context, key, value) result(status) bind(C, name="inja_add_string")
         import :: c_char, c_int, c_ptr
         type(c_ptr), value, intent(in) :: context !< Opaque inja context
         character(kind=c_char), dimension(*), intent(in) :: key !< NUL-terminated key
         character(kind=c_char), dimension(*), intent(in) :: value !< NUL-terminated value
         integer(c_int) :: status !< Zero on success, or -1 on failure
      end function inja_add_string

      !> Destroys an inja context created by inja_create_context.
      subroutine inja_destroy_context(context) bind(C, name="inja_destroy_context")
         import :: c_ptr
         type(c_ptr), value, intent(in) :: context !< Opaque inja context
      end subroutine inja_destroy_context

      !> Renders a template file with the context and writes the result to a file.
      function inja_render_file(context, template_file, dest_file) result(status) bind(C, name="inja_render_file")
         import :: c_char, c_int, c_ptr
         type(c_ptr), value, intent(in) :: context !< Opaque inja context
         character(kind=c_char), dimension(*), intent(in) :: template_file !< NUL-terminated template path
         character(kind=c_char), dimension(*), intent(in) :: dest_file !< NUL-terminated destination path
         integer(c_int) :: status !< Zero on success, or -1 on failure
      end function inja_render_file

   end interface
end module inja_templates
