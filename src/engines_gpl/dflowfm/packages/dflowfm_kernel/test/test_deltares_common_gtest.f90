module test_deltares_common_gtest
    use assertions_gtest
    use precision, only: sp, dp
    use precision_basics, only: comparereal, equal

    implicit none(type, external)

contains

    !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_dp_equal_within_tolerance, test_dp_equal_within_tolerance,
    !> Two dp numbers 1 ULP apart should be equal (within 2*epsilon)
    subroutine test_dp_equal_within_tolerance() bind(C)
        real(kind=dp) :: a, b
        a = 1.0_dp
        b = a + epsilon(a)
        call f90_expect_true(equal(a, b), "dp values within tolerance should be equal")
    end subroutine test_dp_equal_within_tolerance
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_dp_equal_outside_tolerance, test_dp_equal_outside_tolerance,
    !> Two dp numbers 10 ULPs apart should not be equal
    subroutine test_dp_equal_outside_tolerance() bind(C)
        real(kind=dp) :: a, b
        a = 1.0_dp
        b = a + 10.0_dp * epsilon(a)
        call f90_expect_false(equal(a, b), "dp values outside tolerance should not be equal")
    end subroutine test_dp_equal_outside_tolerance
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_dp_equal_large_values, test_dp_equal_large_values,
    !> Two large dp numbers 1 ULP apart at their scale should be equal (tolerance scales up)
    subroutine test_dp_equal_large_values() bind(C)
        real(kind=dp) :: a, b
        a = 1.0e12_dp
        b = a + epsilon(a) * a
        call f90_expect_true(equal(a, b), "large dp values within scaled tolerance should be equal")
    end subroutine test_dp_equal_large_values
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_dp_equal_small_values, test_dp_equal_small_values,
    !> Two very small dp numbers should be equal due to the max(..., 1.0) floor
    subroutine test_dp_equal_small_values() bind(C)
        real(kind=dp) :: a, b
        a = 1.0e-300_dp
        b = 2.0e-300_dp
        call f90_expect_true(equal(a, b), "very small dp values should be equal due to tolerance floor")
    end subroutine test_dp_equal_small_values
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_sp_equal_within_tolerance, test_sp_equal_within_tolerance,
    !> Two sp numbers 1 ULP apart should be equal (within 2*epsilon)
    subroutine test_sp_equal_within_tolerance() bind(C)
        real(kind=sp) :: a, b
        a = 1.0_sp
        b = a + epsilon(a)
        call f90_expect_true(equal(a, b), "sp values within tolerance should be equal")
    end subroutine test_sp_equal_within_tolerance
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_sp_equal_outside_tolerance, test_sp_equal_outside_tolerance,
    !> Two sp numbers 10 ULPs apart should not be equal
    subroutine test_sp_equal_outside_tolerance() bind(C)
        real(kind=sp) :: a, b
        a = 1.0_sp
        b = a + 10.0_sp * epsilon(a)
        call f90_expect_false(equal(a, b), "sp values outside tolerance should not be equal")
    end subroutine test_sp_equal_outside_tolerance
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_dp_equal_eps_outside_tolerance, test_dp_equal_eps_outside_tolerance,
    !> Two dp numbers outside custom epsilon should not be equal
    subroutine test_dp_equal_eps_outside_tolerance() bind(C)
        real(kind=dp) :: a, b
        a = 1.0_dp
        b = 1.0_dp + 2.0e-6_dp
        call f90_expect_false(equal(a, b, 1.0e-6_dp), "dp values outside custom eps should not be equal")
    end subroutine test_dp_equal_eps_outside_tolerance
    !$f90tw)

    !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_dp_equal_large_values_requires_scaling, test_dp_equal_large_values_requires_scaling,
    !> Two large dp numbers that are equal within scaled tolerance but NOT within unscaled tolerance.
    !! This test would fail with a naive abs(a-b) < 2*epsilon(1.0) check.
    subroutine test_dp_equal_large_values_requires_scaling() bind(C)
        real(kind=dp) :: a, b
        a = 1.0e16_dp
        b = a + 3.0_dp   ! abs(a-b)=3 > 2*epsilon(1.0)~4.4e-16 (naive fails), but 3 < 2*epsilon(a)*a~4.4 (scaled passes)

        ! verify the naive check would indeed fail (3 >> 2*epsilon(1.0))
        call f90_expect_false(abs(a-b) < 2.0_dp * epsilon(1.0_dp), "naive unscaled check should report not equal for large values")

        ! verify the scaled check correctly identifies them as equal (3 < 2*epsilon(a)*1e16 ~ 4.4)
        call f90_expect_true(equal(a, b), "large dp values requiring scaled tolerance should be equal")
    end subroutine test_dp_equal_large_values_requires_scaling
    !$f90tw)

        !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_triinterp2_pointcloud_boundary_regression, test_triinterp2_pointcloud_boundary_regression,
    !> Regression test for EC-module triangulation boundary case
    !! PointCloud corners: (0,0)=10, (0,30)=20, (40,30)=30, (40,0)=40
    !! Grid: 2 cells x 3 cells -> 3x4 vertices at x=0,20,40 and y=0,10,20,30
    subroutine test_triinterp2_pointcloud_boundary_regression() bind(C)
        use m_ec_basic_interpolation, only: triinterp2
        use m_alloc, only: realloc
        use m_polygon, only: xpl, ypl, zpl

        implicit none

        integer, parameter   :: ns = 4
        integer, parameter   :: ntargets = 12
        real(kind=dp), parameter :: dmiss = -999.0_dp
        real(kind=dp), parameter :: tol   = 1.0e-10_dp

        real(kind=dp) :: xs(ns), ys(ns), zs(ns)
        real(kind=dp) :: xt(ntargets), yt(ntargets), bl(ntargets)
        real(kind=dp) :: expected(ntargets)
        real(kind=dp) :: transformcoef(6)
        integer :: jdla, i, ix, iy

        ! PointCloud corner samples
        xs = [0.0_dp,  0.0_dp, 40.0_dp, 40.0_dp]
        ys = [0.0_dp, 30.0_dp,  0.0_dp, 30.0_dp]
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
        expected = [ 10.0_dp,              &  ! (x=0,  y=0)
                     25.0_dp,              &  ! (x=20, y=0)
                     40.0_dp,              &  ! (x=40, y=0)
                     13.3333333333333_dp,  &  ! (x=0,  y=10)
                     28.3333333333333_dp,  &  ! (x=20, y=10)
                     36.6666666666667_dp,  &  ! (x=40, y=10)
                     16.6666666666667_dp,  &  ! (x=0,  y=20)
                     28.3333333333333_dp,  &  ! (x=20, y=20)
                     33.3333333333333_dp,  &  ! (x=40, y=20)
                     20.0_dp,              &  ! (x=0,  y=30)
                     25.0_dp,              &  ! (x=20, y=30)
                     30.0_dp               ]  ! (x=40, y=30)

        bl = dmiss
        transformcoef = 0.0_dp
        jdla = 1

        call realloc(xpl, 1, keepExisting=.false.)
        call realloc(ypl, 1, keepExisting=.false.)
        call realloc(zpl, 1, keepExisting=.false.)

        call triinterp2(XZ=xt, YZ=yt, BL=bl, NDX=ntargets, JDLA=jdla, &
                        XS=xs, YS=ys, ZS=zs, NS=ns, dmiss=dmiss,       &
                        jsferic=0, jins=1, jasfer3D=0,                  &
                        NPL=0, MXSAM=0, MYSAM=0,                        &
                        XPL=xpl, YPL=ypl, ZPL=zpl,                     &
                        transformcoef=transformcoef)

        do i = 1, ntargets
            call f90_expect_false(bl(i) == dmiss, &
                "No target vertex should be dmiss after triangulation")
            call f90_expect_true(abs(bl(i) - expected(i)) < tol, &
                "Interpolated value should match bilinear expectation")
        end do

    end subroutine test_triinterp2_pointcloud_boundary_regression
    !$f90tw)

end module test_deltares_common_gtest