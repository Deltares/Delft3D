module m_message_helpers
   use unstruc_messages, only: threshold_abort
   implicit none(type, external)
   private

   !> Restores the previous abort threshold when the local guard leaves scope, including early returns.
   type, public :: t_abort_threshold_guard
      private
      integer :: previous_threshold
      logical :: active = .false.
   contains
      procedure :: set => set_abort_threshold
      final :: restore_abort_threshold
   end type t_abort_threshold_guard

contains

   !> Set a temporary threshold, saving the original value only on the first call.
   subroutine set_abort_threshold(self, threshold)
      class(t_abort_threshold_guard), intent(inout) :: self !< Local lifetime guard.
      integer, intent(in) :: threshold !< Temporary abort threshold.

      if (.not. self%active) then
         self%previous_threshold = threshold_abort
         self%active = .true.
      end if
      threshold_abort = threshold
   end subroutine set_abort_threshold

   subroutine restore_abort_threshold(self)
      type(t_abort_threshold_guard), intent(inout) :: self

      if (self%active) then
         threshold_abort = self%previous_threshold
      end if
   end subroutine restore_abort_threshold

end module m_message_helpers
