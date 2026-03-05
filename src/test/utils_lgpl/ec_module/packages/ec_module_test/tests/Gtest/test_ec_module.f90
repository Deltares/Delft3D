module test_ec_module
   use assertions_gtest
   use m_missing, only: dmiss
   use m_ec_basic_interpolation, only: interpolate_linear_from_triangle
   use precision, only: dp
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_ec_module, test_interpolate_linear_from_triangle, test_interpolate_linear_from_triangle,
   subroutine test_interpolate_linear_from_triangle() bind(C)

      integer, parameter :: NDIM = 1 !< sample vector dimension
      real(kind=dp), dimension(3) :: X !< x coordinates of triangle
      real(kind=dp), dimension(3) :: Y !< y coordinates of triangle
      real(kind=dp), dimension(NDIM, 3) :: Z !< z coordinates of triangle
      real(kind=dp) :: wf(3) !< weights for each point in the triangle
      real(kind=dp) :: slo(NDIM)
      integer :: jsferic
      integer :: jslo
      real(kind=dp) :: xp !< x coordinate to be interpolated
      real(kind=dp) :: yp !< y coordinate to be interpolated
      real(kind=dp) :: zp(NDIM) !< z coordinate of interpolated point

      X = [809724.001042720_dp, 809699.760677868_dp, 809676.983950382_dp]
      Y = [179638.029659005_dp, 179654.755887270_dp, 179639.225971817_dp]
      Z = reshape([3.030621639573760e-040_dp, 1.420180850975200e-063_dp, 1.080075836864300e-062_dp], [NDIM, 3])
      slo = 0.0_dp

      XP = X(2)
      YP = Y(2)

      ! This test checks that a sample point on the corner of the triangle gives the correct value from the triangle
      jsferic = 0
      jslo = 0
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_assert_ge(zp(1), Z(1, 2), "test 1: Point on corner fails")

      ! This test checks that a sample point on the corner of the triangle gives the correct value from the triangle
      ! Here the triangle indices have been shifted
      X = cshift(X, 1)
      Y = cshift(Y, 1)
      Z = cshift(Z, 1, 2)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_assert_ge(zp(1), Z(1, 1), "test 2: Point on corner fails")

      ! This test checks that a sample point on the corner of the triangle gives the correct value from the triangle
      ! Here the triangle indices have been shifted
      X = cshift(X, 1)
      Y = cshift(Y, 1)
      Z = cshift(Z, 1, 2)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_assert_eq(zp(1), Z(1, 3), "test 3: Point on corner fails")

      ! This test checks that a sample point outside the triangle gives the value imposed from the plane.
      ! In this case the plane is defined as z = 1.0_dp+x+y, and hence the expected output is equal to 2.0_dp
      X = [0.0_dp, 1.0_dp, 0.0_dp]
      Y = [0.0_dp, 0.0_dp, 1.0_dp]
      Z = reshape([0.0_dp, 1.0_dp, 1.0_dp], [NDIM, 3])
      XP = 1.0_dp
      YP = 1.0_dp
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_assert_eq(zp(1), 2.0_dp, "test4: Point outside triangle fails")

   end subroutine test_interpolate_linear_from_triangle
   !$f90tw)

end module test_ec_module
