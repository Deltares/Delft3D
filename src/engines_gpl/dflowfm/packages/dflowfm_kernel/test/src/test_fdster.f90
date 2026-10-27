!> Regression test for the small-grain critical Shields branch.
module test_fdster
   use assertions_gtest, only: f90_expect_near
   use precision, only: dp
   use iso_c_utils, only: cstr
   use m_fdster, only: fdster

   implicit none(type, external)
   private
   public :: test_dstar_one_critical_shields

contains

   !$f90tw TESTCODE(TEST, tests_fdster, test_dstar_one_critical_shields, test_dstar_one_critical_shields,
   !> Dstar = 1 must use the special-case Shields value of 0.24.
   subroutine test_dstar_one_critical_shields() bind(C)
      real(kind=dp), parameter :: expected_theta = 0.24_dp
      real(kind=dp), parameter :: tolerance = 1.0e-7_dp
      real(kind=dp) :: dstar, d50, pclay, fcr, gravity, rhos, rhow
      real(kind=dp) :: theta, tau, expected_tau

      dstar = 1.0_dp
      ! D50 > 62 micrometres, no clay and FCR = 1 make all correction factors one.
      d50 = 1.0e-4_dp
      pclay = 0.0_dp
      fcr = 1.0_dp
      gravity = 9.81_dp
      rhos = 2650.0_dp
      rhow = 1000.0_dp
      expected_tau = (rhos - rhow) * gravity * d50 * expected_theta

      call fdster(dstar, tau, theta, pclay, gravity, d50, rhos, rhow, fcr)

      call f90_expect_near(theta, expected_theta, tolerance, &
                           cstr("Dstar = 1 should retain the special-case Shields value of 0.24"))
      call f90_expect_near(tau, expected_tau, tolerance, &
                           cstr("Critical shear stress should use the special-case Shields value"))
   end subroutine test_dstar_one_critical_shields
   !$f90tw)

end module test_fdster
