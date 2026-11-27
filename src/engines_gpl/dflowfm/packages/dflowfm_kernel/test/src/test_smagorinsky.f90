!> Check the squared strain used in the two Smagorinsky stress calculations.
module test_smagorinsky
   use assertions_gtest, only: f90_expect_near
   use m_setumod, only: compute_smagorinsky_strain_squared
   use precision, only: dp

   implicit none(type, external)

contains

   !$gtest
   !> Retain small normal strains when the two shear derivatives cancel.
   subroutine test_smagorinsky_strain() bind(C)
      real(kind=dp) :: strain_squared

      strain_squared = compute_smagorinsky_strain_squared(1.0e-12_dp, -2.0e-12_dp, 1.0_dp, -1.0_dp)
      call f90_expect_near(strain_squared, 1.0e-23_dp, 1.0e-37_dp, 'Opposing shears erase a small squared strain')

      strain_squared = compute_smagorinsky_strain_squared(1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp)
      call f90_expect_near(strain_squared, 59.0_dp, 1.0e-12_dp, 'Incorrect squared strain')
   end subroutine test_smagorinsky_strain
end module test_smagorinsky
