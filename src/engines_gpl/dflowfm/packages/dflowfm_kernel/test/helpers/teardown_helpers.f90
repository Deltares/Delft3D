module m_teardown_helpers
   use assertions_gtest, only: f90_expect_true
   implicit none(type, external)
   private

   !> Type used to install teardown subroutines in tests.
   !! The `teardown_routine` gets called automatically when the instance goes out of scope.
   !! If you declare a `type(t_teardown)` but do not install a teardown routine with `install`
   !! then your unit test will fail.
   type, public :: t_teardown
      private
      procedure(teardown_routine), pointer, nopass :: routine => missing_teardown
   contains
      procedure, public :: install
      final :: run_teardown
   end type t_teardown

   abstract interface
      subroutine teardown_routine()
      end subroutine teardown_routine
   end interface
contains

   !> Install a teardown routine.
   subroutine install(self, routine)
      class(t_teardown), intent(inout) :: self
      procedure(teardown_routine) :: routine
      self%routine => routine
   end subroutine install

   !> The finalizer of the `t_teardown`. Gets called when the instance goes out of scope.
   subroutine run_teardown(self)
      type(t_teardown), intent(in) :: self
      call self%routine()
   end subroutine run_teardown

   !> Called when teardown is declared but `install` is not called.
   !! Fails the test with an instruction to use `install`.
   subroutine missing_teardown()
      call f90_expect_true(.false., &
         "`type(t_teardown)` declared but no teardown routine installed. Use `teardown%install(teardown_routine)`.")
   end subroutine missing_teardown

end module m_teardown_helpers
