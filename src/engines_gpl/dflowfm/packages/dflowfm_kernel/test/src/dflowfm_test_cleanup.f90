module dflowfm_test_cleanup
   implicit none(type, external)
   private

   public :: reset_dflowfm_after_test

contains

   subroutine reset_dflowfm_after_test() bind(C)
      use m_resetfullflowmodel, only: resetfullflowmodel
      use messagehandling, only: resetmaxerrorlevel

      call resetfullflowmodel()
      call resetmaxerrorlevel()
   end subroutine reset_dflowfm_after_test

end module dflowfm_test_cleanup
