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

   !$f90tw TESTCODE(TEST, test_ec_module, test_triinterp2_pointcloud_boundary_regression, test_triinterp2_pointcloud_boundary_regression,
   !> Regression test for EC-module triangulation boundary case
    !! PointCloud corners: (0,0)=10, (0,30)=20, (40,30)=30, (40,0)=40
    !! Grid: 2 cells x 3 cells -> 3x4 vertices at x=0,20,40 and y=0,10,20,30
   subroutine test_triinterp2_pointcloud_boundary_regression() bind(C)
      use m_ec_basic_interpolation, only: triinterp2
      use m_alloc, only: realloc

      implicit none

      integer, parameter :: ns = 4
      integer, parameter :: ntargets = 12
      real(kind=dp), parameter :: dmiss = -999.0_dp
      real(kind=dp), parameter :: tol = 1.0e-10_dp

      real(kind=dp) :: xs(ns), ys(ns), zs(ns)
      real(kind=dp) :: xt(ntargets), yt(ntargets), bl(ntargets)
      real(kind=dp) :: expected(ntargets)
      real(kind=dp) :: transformcoef(6)
      real(kind=dp), dimension(:), allocatable :: xpl, ypl, zpl
      integer :: jdla, i, ix, iy

      ! PointCloud corner samples
      xs = [0.0_dp, 0.0_dp, 40.0_dp, 40.0_dp]
      ys = [0.0_dp, 30.0_dp, 0.0_dp, 30.0_dp]
      zs = [10.0_dp, 20.0_dp, 40.0_dp, 30.0_dp]

      ! Target grid vertices: x in {0,20,40}, y in {0,10,20,30}, row-major (x varies fastest)
      i = 0
      do iy = 0, 3
         do ix = 0, 2
            i = i + 1
            xt(i) = ix * 20.0_dp
            yt(i) = iy * 10.0_dp
         end do
      end do

      ! Expected values from triangulation interpolation of the 4 corner samples.
      expected = [10.0_dp, & ! (x=0,  y=0)
                  25.0_dp, & ! (x=20, y=0)
                  40.0_dp, & ! (x=40, y=0)
                  13.3333333333333_dp, & ! (x=0,  y=10)
                  28.3333333333333_dp, & ! (x=20, y=10)
                  36.6666666666667_dp, & ! (x=40, y=10)
                  16.6666666666667_dp, & ! (x=0,  y=20)
                  28.3333333333333_dp, & ! (x=20, y=20)
                  33.3333333333333_dp, & ! (x=40, y=20)
                  20.0_dp, & ! (x=0,  y=30)
                  25.0_dp, & ! (x=20, y=30)
                  30.0_dp] ! (x=40, y=30)

      bl = dmiss
      transformcoef = 0.0_dp
      jdla = 1

      call realloc(xpl, 1, keepExisting=.false.)
      call realloc(ypl, 1, keepExisting=.false.)
      call realloc(zpl, 1, keepExisting=.false.)

      call triinterp2(XZ=xt, YZ=yt, BL=bl, NDX=ntargets, JDLA=jdla, &
                      XS=xs, YS=ys, ZS=zs, NS=ns, dmiss=dmiss, &
                      jsferic=0, jins=1, jasfer3D=0, &
                      NPL=0, MXSAM=0, MYSAM=0, &
                      XPL=xpl, YPL=ypl, ZPL=zpl, &
                      transformcoef=transformcoef)

      do i = 1, ntargets
         call f90_expect_false(bl(i) == dmiss, &
                               "No target vertex should be dmiss after triangulation")
         call f90_expect_true(abs(bl(i) - expected(i)) < tol, &
                              "Interpolated value should match bilinear expectation")
      end do

   end subroutine test_triinterp2_pointcloud_boundary_regression
   !$f90tw)

end module test_ec_module
