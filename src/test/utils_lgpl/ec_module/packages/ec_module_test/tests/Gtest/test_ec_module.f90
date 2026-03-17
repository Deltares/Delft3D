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
      call f90_assert_eq(zp(1), 2.0_dp, "test 4: Point outside triangle fails")

      X = [.810741505645373e+06_dp, .810715795061337e+06_dp, .810735820827145e+06_dp]
      Y = [.191320100347323e+06_dp, .191235593530796e+06_dp, .191209251914610e+06_dp]
      Z = reshape([.219641432209429e-18_dp, .169398097079605e-19_dp, .123683600075936e-19_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 3, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_assert_eq(zp(1), Z(1, 3), "test 5: Shifted point on corner fails")

      X = [.806502842601997e+06_dp, .806438395456953e+06_dp, .806470914272868e+06_dp]
      Y = [.159100366306436e+06_dp, .159134438983112e+06_dp, .159083652909205e+06_dp]
      Z = reshape([.464639817153730e-25_dp, .527615391002151e-36_dp, .712069478335024e-37_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 1, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_assert_eq(zp(1), Z(1, 1), "test 6: Shifted point on corner fails")

      X = [.805702388463922e+06_dp, .805692129304944e+06_dp, .805673691129311e+06_dp]
      Y = [.156545533005188e+06_dp, .156584360999117e+06_dp, .156521453008499e+06_dp]
      Z = reshape([.498469040053414e+00_dp, .496177053725623e+00_dp, .496584955956583e+00_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 1, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_assert_eq(zp(1), Z(1, 1), "test 7: Shifted point on corner fails")

      X = [.807150922014422e+06_dp, .807172084442607e+06_dp, .807190212633611e+06_dp]
      Y = [.168781462201362e+06_dp, .168767625702374e+06_dp, .168776505966367e+06_dp]
      Z = reshape([.805482177954693e-44_dp, .000000000000000e+00_dp, .000000000000000e+00_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 3, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_assert_eq(zp(1), Z(1, 3), "test 8: Shifted point on corner fails")

      X = [.806776412841201e+06_dp, .806804789960853e+06_dp, .806769927345308e+06_dp]
      Y = [.149483581166913e+06_dp, .149508431678412e+06_dp, .149506841015623e+06_dp]
      Z = reshape([.199999999999856e+01_dp, .199999999999973e+01_dp, .199999999999186e+01_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 3, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_assert_eq(zp(1), Z(1, 3), "test 9: Shifted point on corner fails")

      X = [.807003062711879e+06_dp, .806908248153497e+06_dp, .806945021331757e+06_dp]
      Y = [.135897014668739e+06_dp, .135946168814630e+06_dp, .135895686424603e+06_dp]
      Z = reshape([.175707024484946e-24_dp, .152351036487821e-35_dp, .943483555475934e-48_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 1, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_assert_eq(zp(1), Z(1, 1), "test 10: Shifted point on corner fails")

      X = [.807110951159965e+06_dp, .807063041293978e+06_dp, .807148892744198e+06_dp]
      Y = [.128259057203519e+06_dp, .128250251604510e+06_dp, .128258617554631e+06_dp]
      Z = reshape([.833192365649538e-12_dp, .144254615653382e-10_dp, .231139206124284e-12_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 3, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_assert_eq(zp(1), Z(1, 3), "test 11: Shifted point on corner fails")

      X = [.811601457857964e+06_dp, .811490516351056e+06_dp, .811532845897478e+06_dp]
      Y = [.171952588549913e+06_dp, .171985485060589e+06_dp, .171943565918315e+06_dp]
      Z = reshape([.418016988243609e-22_dp, .117676256102526e-23_dp, .296107331970948e-23_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 1, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_assert_eq(zp(1), Z(1, 1), "test 12: Shifted point on corner fails")

      X = [.806914046291477e+06_dp, .806893441888252e+06_dp, .806926627972404e+06_dp]
      Y = [.167905985010564e+06_dp, .167907208326401e+06_dp, .167883949377100e+06_dp]
      Z = reshape([.000000000000000e+00_dp, .317584317488459e-44_dp, .000000000000000e+00_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 1, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_assert_eq(zp(1), Z(1, 1), "test 13: Shifted point on corner fails")

      X = [.812434846114502e+06_dp, .812518196067187e+06_dp, .812525177297805e+06_dp]
      Y = [.166909550672643e+06_dp, .166949303495153e+06_dp, .167013146329587e+06_dp]
      Z = reshape([.284986368376641e-27_dp, .267207958876060e-26_dp, .157397562929726e-26_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 2, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_assert_eq(zp(1), Z(1, 2), "test 14: Shifted point on corner fails")

      X = [.809436607241510e+06_dp, .809426717373578e+06_dp, .809463567886946e+06_dp]
      Y = [.191142887944505e+06_dp, .190989535535170e+06_dp, .191044674094134e+06_dp]
      Z = reshape([.196126584784370e-12_dp, .912131269907617e-13_dp, .536104596867851e-13_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 3, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_assert_eq(zp(1), Z(1, 3), "test 15: Shifted point on corner fails")

   end subroutine test_interpolate_linear_from_triangle
   !$f90tw)

   !< Helper subroutine to get a point just outside the triangle by shifting a corner point slightly away from the center of mass of the triangle
   subroutine get_point_just_outside_triangle(X, Y, IDX, XP, YP) bind(C)
      real(kind=dp), dimension(3), intent(in) :: X !x coordinates of triangle
      real(kind=dp), dimension(3), intent(in) :: Y !y coordinates of triangle
      integer, intent(in) :: IDX !corner of triangle to be shifted outside
      real(kind=dp), intent(out) :: XP
      real(kind=dp), intent(out) :: YP

      real(kind=dp) :: XM !center of mass of triangle in x
      real(kind=dp) :: YM !center of mass of triangle in y
      real(kind=dp) :: shift !distance to shift the point
      real(kind=dp) :: Xshift !x component of shift direction
      real(kind=dp) :: Yshift !y component of shift direction
      real(kind=dp) :: norm !norm of the shift direction vector

      shift = 1e-15_dp * max(maxval(abs(X)), maxval(abs(Y)))
      XM = sum(X) / 3.0_dp
      YM = sum(Y) / 3.0_dp
      Xshift = X(IDX) - XM
      Yshift = Y(IDX) - YM
      norm = sqrt(Xshift**2 + Yshift**2)
      XP = X(IDX) + shift * Xshift / norm
      YP = Y(IDX) + shift * Yshift / norm
   end subroutine get_point_just_outside_triangle

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
