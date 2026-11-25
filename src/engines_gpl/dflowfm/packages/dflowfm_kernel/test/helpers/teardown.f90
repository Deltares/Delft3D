module m_teardown
   implicit none(type, external)
   private

   abstract interface
      subroutine cleanup_routine()
      end subroutine cleanup_routine
   end interface

   !> Run a cleanup callback when a local guard goes out of scope.
   type, public :: t_teardown
      procedure(cleanup_routine), pointer, nopass :: routine => null()
   contains
      final :: run_teardown
   end type t_teardown

contains

   subroutine run_teardown(self)
      type(t_teardown), intent(in) :: self

      if (associated(self%routine)) then
         call self%routine()
      end if
   end subroutine run_teardown

end module m_teardown
