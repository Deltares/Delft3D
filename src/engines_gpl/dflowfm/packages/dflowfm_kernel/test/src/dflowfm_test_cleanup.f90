module dflowfm_test_cleanup
   implicit none(type, external)
   private

   public :: reset_dflowfm_after_test

contains

   subroutine reset_dflowfm_after_test() bind(C)
      use m_resetfullflowmodel, only: resetfullflowmodel
      use messagehandling, only: resetmaxerrorlevel
      use m_polygon, only: m_polygon_destructor
      use assertions_gtest, only: f90_expect_eq

      integer :: ierr

      call resetfullflowmodel()
      ierr = m_polygon_destructor()
      call f90_expect_eq(ierr, 0, "Polygon cleanup should succeed")
      call resetmaxerrorlevel()
   end subroutine reset_dflowfm_after_test

end module dflowfm_test_cleanup
