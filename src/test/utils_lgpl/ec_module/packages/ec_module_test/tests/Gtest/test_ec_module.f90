module test_ec_module
   use assertions_gtest
   use m_missing, only: dmiss
   use m_ec_basic_interpolation, only: interpolate_linear_in_triangle
   use precision, only: dp
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_ec_module, test_interpolate_linear_in_triangle, test_interpolate_linear_in_triangle,
   subroutine test_interpolate_linear_in_triangle() bind(C)

      integer, parameter :: NDIM = 1 !< sample vector dimension
      real(kind=dp), dimension(3) :: X !< x coordinates of triangle
      real(kind=dp), dimension(3) :: Y !< y coordinates of triangle
      real(kind=dp), dimension(NDIM, 3) :: Z !< z coordinates of triangle
      real(kind=dp) :: wf(3) !< weights for each point in the triangle
      real(kind=dp) :: slo(NDIM)
      integer :: jsferic
      integer :: jatek
      integer :: jslo
      real(kind=dp) :: xp !< x coordinate to be interpolated
      real(kind=dp) :: yp !< y coordinate to be interpolated
      real(kind=dp) :: zp(NDIM) !< z coordinate of interpolated point

      X = [809724.001042720_dp, 809699.760677868_dp, 809676.983950382_dp]
      Y = [179638.029659005_dp, 179654.755887270_dp, 179639.225971817_dp]
      Z = reshape([3.030621639573760e-040_dp, 1.420180850975200e-063_dp, 1.080075836864300e-062_dp], [NDIM, 3])
      slo = 0.0_dp

      XP = 809699.760677868_dp
      YP = 179654.755887270_dp

      jatek = 0
      jsferic = 0
      jslo = 0
      call interpolate_linear_in_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, JATEK, wf, dmiss, jsferic)
      call f90_assert_ge(zp(1), 1.420180850975200e-063_dp, "test 1: Expected positive value")

      X = cshift(X, 1)
      Y = cshift(Y, 1)
      Z = cshift(Z, 1, 2)
      call interpolate_linear_in_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, JATEK, wf, dmiss, jsferic)
      call f90_assert_ge(zp(1), 1.420180850975200e-063_dp, "test 2: Expected positive value")

      X = cshift(X, 1)
      Y = cshift(Y, 1)
      Z = cshift(Z, 1, 2)
      call interpolate_linear_in_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, JATEK, wf, dmiss, jsferic)
      call f90_assert_ge(zp(1), 1.420180850975200e-063_dp, "test 3: Expected positive value")

   end subroutine test_interpolate_linear_in_triangle
   !$f90tw)

end module test_ec_module
