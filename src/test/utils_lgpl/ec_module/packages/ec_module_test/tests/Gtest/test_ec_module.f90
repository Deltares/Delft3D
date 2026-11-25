module test_ec_module
   use assertions_gtest
   use m_missing, only: dmiss
   use m_ec_basic_interpolation, only: interpolate_linear_from_triangle
   use precision, only: dp
   implicit none

contains

   !$gtest
   subroutine test_connection_target_mask_initialization() bind(C)
      use m_ec_module, only: ecCreateInstance, ecFreeInstance, ecCreateConnection, ecSetConnectionTargetMask, &
                             ecCreateConverter, ecSetConnectionConverter
      use m_ec_message, only: clear_ec_message
      use m_ec_support, only: ecSupportFindConnection
      use m_ec_typedefs, only: tEcConnection, tEcInstance

      type(tEcInstance), pointer :: ecInstancePtr
      type(tEcConnection), pointer :: connection
      integer :: connection_id, converter_id
      logical :: success

      ecInstancePtr => null()
      success = ecCreateInstance(ecInstancePtr)
      if (.not. f90_assert_true(success, 'EC instance should be created')) then
         return
      end if
      connection_id = ecCreateConnection(ecInstancePtr)
      connection => ecSupportFindConnection(ecInstancePtr, connection_id)
      success = ecSetConnectionTargetMask(ecInstancePtr, connection_id, [0, 0, 0, 0])
      call f90_expect_true(success, 'all-zero mask should be accepted on a fresh connection')
      call f90_expect_true(allocated(connection%targetMask), 'all-zero mask must remain allocated')
      if (.not. allocated(connection%targetMask)) then
         success = ecFreeInstance(ecInstancePtr)
         call f90_expect_true(success, 'EC instance should be freed')
         call clear_ec_message()
         return
      end if
      call f90_expect_true(all(connection%targetMask == 0), 'all-zero mask must retain its selection')

      success = ecSetConnectionTargetMask(ecInstancePtr, connection_id, [1, 1, 1, 1])
      call f90_expect_false(success, 'a second mask assignment must be rejected')
      call f90_expect_true(all(connection%targetMask == 0), 'rejected assignment must preserve the original mask')

      connection_id = ecCreateConnection(ecInstancePtr)
      connection => ecSupportFindConnection(ecInstancePtr, connection_id)
      success = ecSetConnectionTargetMask(ecInstancePtr, connection_id, [1, 1, 1, 1])
      call f90_expect_true(success, 'trivial mask should be accepted on a fresh connection')
      call f90_expect_false(allocated(connection%targetMask), 'trivial mask must not be allocated')

      converter_id = ecCreateConverter(ecInstancePtr)
      success = ecSetConnectionConverter(ecInstancePtr, connection_id, converter_id)
      call f90_expect_true(success, 'converter should attach to the connection')
      allocate (connection%converterPtr%indexWeight)
      success = ecSetConnectionTargetMask(ecInstancePtr, connection_id, [1, 0, 1, 0])
      call f90_expect_false(success, 'mask assignment must be rejected after index weights are computed')
      call f90_expect_false(allocated(connection%targetMask), 'rejected late mask must not be stored')

      success = ecFreeInstance(ecInstancePtr)
      call f90_expect_true(success, 'EC instance should be freed')
      call clear_ec_message()
   end subroutine test_connection_target_mask_initialization

   !$gtest
   subroutine test_connection_target_mask_size() bind(C)
      use m_ec_module, only: ecCreateInstance, ecFreeInstance, ecCreateConnection, ecSetConnectionTargetMask, ecAddConnectionTargetItem, &
                             ecCreateItem, ecCreateElementSet, ecSetItemElementSet, &
                             ecSetElementSetType, ecSetElementSetNumberOfCoordinates
      use m_ec_message, only: clear_ec_message
      use m_ec_parameters, only: elmSetType_cartesian
      use m_ec_support, only: ecSupportFindConnection
      use m_ec_typedefs, only: tEcConnection, tEcInstance

      type(tEcInstance), pointer :: ecInstancePtr
      type(tEcConnection), pointer :: connection
      integer, dimension(3) :: item_ids
      integer :: connection_id, element_set_id, i
      logical :: success

      ecInstancePtr => null()
      success = ecCreateInstance(ecInstancePtr)
      if (.not. f90_assert_true(success, 'EC instance should be created')) then
         return
      end if
      do i = 1, size(item_ids)
         element_set_id = ecCreateElementSet(ecInstancePtr)
         success = ecSetElementSetType(ecInstancePtr, element_set_id, elmSetType_cartesian)
         call f90_expect_true(success, 'target element set type should be configured')
         success = ecSetElementSetNumberOfCoordinates(ecInstancePtr, element_set_id, i)
         call f90_expect_true(success, 'target coordinate count should be configured')
         item_ids(i) = ecCreateItem(ecInstancePtr)
         success = ecSetItemElementSet(ecInstancePtr, item_ids(i), element_set_id)
         call f90_expect_true(success, 'target item should receive its element set')
      end do

      connection_id = ecCreateConnection(ecInstancePtr)
      connection => ecSupportFindConnection(ecInstancePtr, connection_id)
      success = ecSetConnectionTargetMask(ecInstancePtr, connection_id, [1, 0])
      call f90_expect_true(success, 'mask may be assigned before targets')
      success = ecAddConnectionTargetItem(ecInstancePtr, connection_id, item_ids(1))
      call f90_expect_false(success, 'a target smaller than the mask must be rejected')
      success = ecAddConnectionTargetItem(ecInstancePtr, connection_id, item_ids(3))
      call f90_expect_false(success, 'a target larger than the mask must be rejected')
      call f90_expect_eq(connection%nTargetItems, 0, 'rejected targets must not be attached')
      success = ecAddConnectionTargetItem(ecInstancePtr, connection_id, item_ids(2))
      call f90_expect_true(success, 'a target matching the mask must be accepted')
      call f90_expect_eq(connection%nTargetItems, 1, 'only the matching target should be attached')

      connection_id = ecCreateConnection(ecInstancePtr)
      connection => ecSupportFindConnection(ecInstancePtr, connection_id)
      success = ecAddConnectionTargetItem(ecInstancePtr, connection_id, item_ids(2))
      call f90_expect_true(success, 'target may be attached before assigning a mask')
      success = ecSetConnectionTargetMask(ecInstancePtr, connection_id, [0])
      call f90_expect_false(success, 'a mask smaller than the target must be rejected')
      call f90_expect_false(allocated(connection%targetMask), 'a rejected short mask must not be stored')
      success = ecSetConnectionTargetMask(ecInstancePtr, connection_id, [0, 0, 0])
      call f90_expect_false(success, 'a mask larger than the target must be rejected')
      call f90_expect_false(allocated(connection%targetMask), 'a rejected long mask must not be stored')
      success = ecSetConnectionTargetMask(ecInstancePtr, connection_id, [1, 1, 1])
      call f90_expect_false(success, 'all-enabled masks must be size-checked before being discarded')
      call f90_expect_false(allocated(connection%targetMask), 'a rejected all-enabled mask must not be stored')
      success = ecSetConnectionTargetMask(ecInstancePtr, connection_id, [1, 0])
      call f90_expect_true(success, 'a matching mask must still be accepted after rejected assignments')
      call f90_expect_eq(connection%nTargetItems, 1, 'rejected masks must not change the attached targets')

      connection_id = ecCreateConnection(ecInstancePtr)
      connection => ecSupportFindConnection(ecInstancePtr, connection_id)
      success = ecAddConnectionTargetItem(ecInstancePtr, connection_id, item_ids(2))
      call f90_expect_true(success, 'first unmasked target should be attached')
      success = ecAddConnectionTargetItem(ecInstancePtr, connection_id, item_ids(3))
      call f90_expect_true(success, 'second unmasked target should be attached')
      success = ecSetConnectionTargetMask(ecInstancePtr, connection_id, [1, 0])
      call f90_expect_false(success, 'the mask must match every target, not just the first')
      call f90_expect_false(allocated(connection%targetMask), 'a mask incompatible with a later target must not be stored')
      call f90_expect_eq(connection%nTargetItems, 2, 'rejecting a mask must preserve all attached targets')

      success = ecFreeInstance(ecInstancePtr)
      call f90_expect_true(success, 'EC instance should be freed')
      call clear_ec_message()
   end subroutine test_connection_target_mask_size

   !$gtest
   subroutine test_initial_velocity_netcdf_candidates() bind(C)
      use m_ec_support, only: ecSupportNetcdfGetQuantityCandidateNames
      use iso_c_utils, only: cstr

      character(len=256), dimension(:), allocatable :: variable_names, standard_names, fallback_names

      call ecSupportNetcdfGetQuantityCandidateNames('initialvelocityx', standard_names, variable_names, fallback_names)
      if (.not. f90_assert_true(allocated(variable_names))) then
         return
      end if
      if (.not. f90_assert_true(allocated(standard_names))) then
         return
      end if
      if (.not. f90_assert_true(allocated(fallback_names))) then
         return
      end if
      if (.not. f90_assert_eq(size(variable_names), 1)) then
         return
      end if
      if (.not. f90_assert_eq(size(standard_names), 1)) then
         return
      end if
      if (.not. f90_assert_eq(size(fallback_names), 1)) then
         return
      end if
      call f90_expect_streq(cstr(variable_names(1)), cstr('uo'))
      call f90_expect_streq(cstr(standard_names(1)), cstr('sea_water_x_velocity'))
      call f90_expect_streq(cstr(fallback_names(1)), cstr('eastward_sea_water_velocity'))

      deallocate (variable_names, standard_names, fallback_names)
      call ecSupportNetcdfGetQuantityCandidateNames('initialvelocityy', standard_names, variable_names, fallback_names)
      if (.not. f90_assert_true(allocated(variable_names))) then
         return
      end if
      if (.not. f90_assert_true(allocated(standard_names))) then
         return
      end if
      if (.not. f90_assert_true(allocated(fallback_names))) then
         return
      end if
      if (.not. f90_assert_eq(size(variable_names), 1)) then
         return
      end if
      if (.not. f90_assert_eq(size(standard_names), 1)) then
         return
      end if
      if (.not. f90_assert_eq(size(fallback_names), 1)) then
         return
      end if
      call f90_expect_streq(cstr(variable_names(1)), cstr('vo'))
      call f90_expect_streq(cstr(standard_names(1)), cstr('sea_water_y_velocity'))
      call f90_expect_streq(cstr(fallback_names(1)), cstr('northward_sea_water_velocity'))
   end subroutine test_initial_velocity_netcdf_candidates

   !$gtest
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
      call f90_expect_ge(zp(1), Z(1, 2), "test 1: Point on corner fails")

      ! This test checks that a sample point on the corner of the triangle gives the correct value from the triangle
      ! Here the triangle indices have been shifted
      X = cshift(X, 1)
      Y = cshift(Y, 1)
      Z = cshift(Z, 1, 2)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_expect_ge(zp(1), Z(1, 1), "test 2: Point on corner fails")

      ! This test checks that a sample point on the corner of the triangle gives the correct value from the triangle
      ! Here the triangle indices have been shifted
      X = cshift(X, 1)
      Y = cshift(Y, 1)
      Z = cshift(Z, 1, 2)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_expect_eq(zp(1), Z(1, 3), "test 3: Point on corner fails")

      ! This test checks that a sample point outside the triangle gives the value imposed from the plane.
      ! In this case the plane is defined as z = 1.0_dp+x+y, and hence the expected output is equal to 2.0_dp
      X = [0.0_dp, 1.0_dp, 0.0_dp]
      Y = [0.0_dp, 0.0_dp, 1.0_dp]
      Z = reshape([0.0_dp, 1.0_dp, 1.0_dp], [NDIM, 3])
      XP = 1.0_dp
      YP = 1.0_dp
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_expect_eq(zp(1), 2.0_dp, "test 4: Point outside triangle fails")

      X = [.810741505645373e+06_dp, .810715795061337e+06_dp, .810735820827145e+06_dp]
      Y = [.191320100347323e+06_dp, .191235593530796e+06_dp, .191209251914610e+06_dp]
      Z = reshape([.219641432209429e-18_dp, .169398097079605e-19_dp, .123683600075936e-19_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 3, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_expect_eq(zp(1), Z(1, 3), "test 5: Shifted point on corner fails")

      X = [.806502842601997e+06_dp, .806438395456953e+06_dp, .806470914272868e+06_dp]
      Y = [.159100366306436e+06_dp, .159134438983112e+06_dp, .159083652909205e+06_dp]
      Z = reshape([.464639817153730e-25_dp, .527615391002151e-36_dp, .712069478335024e-37_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 1, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_expect_eq(zp(1), Z(1, 1), "test 6: Shifted point on corner fails")

      X = [.805702388463922e+06_dp, .805692129304944e+06_dp, .805673691129311e+06_dp]
      Y = [.156545533005188e+06_dp, .156584360999117e+06_dp, .156521453008499e+06_dp]
      Z = reshape([.498469040053414e+00_dp, .496177053725623e+00_dp, .496584955956583e+00_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 1, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_expect_eq(zp(1), Z(1, 1), "test 7: Shifted point on corner fails")

      X = [.807150922014422e+06_dp, .807172084442607e+06_dp, .807190212633611e+06_dp]
      Y = [.168781462201362e+06_dp, .168767625702374e+06_dp, .168776505966367e+06_dp]
      Z = reshape([.805482177954693e-44_dp, .000000000000000e+00_dp, .000000000000000e+00_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 3, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_expect_eq(zp(1), Z(1, 3), "test 8: Shifted point on corner fails")

      X = [.806776412841201e+06_dp, .806804789960853e+06_dp, .806769927345308e+06_dp]
      Y = [.149483581166913e+06_dp, .149508431678412e+06_dp, .149506841015623e+06_dp]
      Z = reshape([.199999999999856e+01_dp, .199999999999973e+01_dp, .199999999999186e+01_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 3, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_expect_eq(zp(1), Z(1, 3), "test 9: Shifted point on corner fails")

      X = [.807003062711879e+06_dp, .806908248153497e+06_dp, .806945021331757e+06_dp]
      Y = [.135897014668739e+06_dp, .135946168814630e+06_dp, .135895686424603e+06_dp]
      Z = reshape([.175707024484946e-24_dp, .152351036487821e-35_dp, .943483555475934e-48_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 1, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_expect_eq(zp(1), Z(1, 1), "test 10: Shifted point on corner fails")

      X = [.807110951159965e+06_dp, .807063041293978e+06_dp, .807148892744198e+06_dp]
      Y = [.128259057203519e+06_dp, .128250251604510e+06_dp, .128258617554631e+06_dp]
      Z = reshape([.833192365649538e-12_dp, .144254615653382e-10_dp, .231139206124284e-12_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 3, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_expect_eq(zp(1), Z(1, 3), "test 11: Shifted point on corner fails")

      X = [.811601457857964e+06_dp, .811490516351056e+06_dp, .811532845897478e+06_dp]
      Y = [.171952588549913e+06_dp, .171985485060589e+06_dp, .171943565918315e+06_dp]
      Z = reshape([.418016988243609e-22_dp, .117676256102526e-23_dp, .296107331970948e-23_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 1, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_expect_eq(zp(1), Z(1, 1), "test 12: Shifted point on corner fails")

      X = [.806914046291477e+06_dp, .806893441888252e+06_dp, .806926627972404e+06_dp]
      Y = [.167905985010564e+06_dp, .167907208326401e+06_dp, .167883949377100e+06_dp]
      Z = reshape([.000000000000000e+00_dp, .317584317488459e-44_dp, .000000000000000e+00_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 1, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_expect_eq(zp(1), Z(1, 1), "test 13: Shifted point on corner fails")

      X = [.812434846114502e+06_dp, .812518196067187e+06_dp, .812525177297805e+06_dp]
      Y = [.166909550672643e+06_dp, .166949303495153e+06_dp, .167013146329587e+06_dp]
      Z = reshape([.284986368376641e-27_dp, .267207958876060e-26_dp, .157397562929726e-26_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 2, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_expect_eq(zp(1), Z(1, 2), "test 14: Shifted point on corner fails")

      X = [.809436607241510e+06_dp, .809426717373578e+06_dp, .809463567886946e+06_dp]
      Y = [.191142887944505e+06_dp, .190989535535170e+06_dp, .191044674094134e+06_dp]
      Z = reshape([.196126584784370e-12_dp, .912131269907617e-13_dp, .536104596867851e-13_dp], [NDIM, 3])
      call get_point_just_outside_triangle(X, Y, 3, XP, YP)
      call interpolate_linear_from_triangle(X, Y, Z, NDIM, XP, YP, ZP, JSLO, SLO, wf, dmiss, jsferic)
      call f90_expect_eq(zp(1), Z(1, 3), "test 15: Shifted point on corner fails")

   end subroutine test_interpolate_linear_from_triangle

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

   !$gtest
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

   !$gtest
   subroutine test_netcdf_candidate_helper_returns_predefined_mapping() bind(C)
      use m_ec_support, only: ecSupportNetcdfGetQuantityCandidateNames
      use iso_c_utils, only: cstr

      character(len=256), dimension(:), allocatable :: variable_names, standard_names, fallback_names

      call ecSupportNetcdfGetQuantityCandidateNames('waterlevelbnd', standard_names, &
                                                   variable_names, fallback_names)
      if (.not. f90_assert_true(allocated(variable_names))) then
         return
      end if
      if (.not. f90_assert_true(allocated(standard_names))) then
         return
      end if
      if (.not. f90_assert_eq(size(variable_names), 1)) then
         return
      end if
      if (.not. f90_assert_eq(size(standard_names), 1)) then
         return
      end if
      call f90_expect_streq(cstr(variable_names(1)), cstr('waterlevel'))
      call f90_expect_streq(cstr(standard_names(1)), cstr('sea_surface_height'))
      call f90_expect_false(allocated(fallback_names))
   end subroutine test_netcdf_candidate_helper_returns_predefined_mapping

   !$gtest
   subroutine test_netcdf_candidate_helper_leaves_unknown_quantity_unmapped() bind(C)
      use m_ec_support, only: ecSupportNetcdfGetQuantityCandidateNames
      use m_ec_message, only: clear_ec_message

      character(len=256), dimension(:), allocatable :: variable_names, standard_names, fallback_names

      call clear_ec_message()
      call ecSupportNetcdfGetQuantityCandidateNames('arbitrary_target', standard_names, &
                                                   variable_names, fallback_names)
      call f90_expect_false(allocated(variable_names))
      call f90_expect_false(allocated(standard_names))
      call clear_ec_message()
   end subroutine test_netcdf_candidate_helper_leaves_unknown_quantity_unmapped

   !$gtest
   subroutine test_converter_target_mask_unimagdir() bind(C)
      use m_ec_converter, only: ecConverterPerformConversions
      use m_ec_message, only: clear_ec_message
      use m_ec_parameters, only: convType_unimagdir, interpolate_timespace, EC_OPERAND_ADD, EC_OPERAND_REPLACE, ec_undef_hp
      use m_ec_typedefs, only: tEcConnection, tEcConverter, tEcElementSet, tEcField, tEcItem
      use time_class, only: c_time

      type(tEcConnection) :: connection
      type(tEcConverter), target :: converter
      type(tEcItem), target :: source_item, target_u_item, target_v_item
      type(tEcField), target :: src_t0, src_t1, tgt_u_field, tgt_v_field
      type(tEcElementSet), target :: target_set
      real(dp), dimension(2), target :: src_vals_t0, src_vals_t1, target_u, target_v
      type(c_time) :: time
      logical :: success

      src_vals_t0 = [5.0_dp, 270.0_dp] ! wind from west -> eastward (+u, 0 v)
      src_vals_t1 = [5.0_dp, 270.0_dp]
      src_t0%timesteps = 0.0_dp
      src_t1%timesteps = 1.0_dp
      src_t0%arr1dPtr => src_vals_t0
      src_t1%arr1dPtr => src_vals_t1
      source_item%sourceT0FieldPtr => src_t0
      source_item%sourceT1FieldPtr => src_t1

      target_set%nCoordinates = 2
      tgt_u_field%arr1dPtr => target_u
      tgt_v_field%arr1dPtr => target_v
      target_u_item%elementSetPtr => target_set
      target_u_item%targetFieldPtr => tgt_u_field
      target_v_item%elementSetPtr => target_set
      target_v_item%targetFieldPtr => tgt_v_field

      converter%ofType = convType_unimagdir
      converter%interpolationType = interpolate_timespace
      converter%operandType = EC_OPERAND_ADD

      allocate (connection%sourceItemsPtr(1), connection%targetItemsPtr(2))
      connection%nSourceItems = 1
      connection%nTargetItems = 2
      connection%sourceItemsPtr(1)%ptr => source_item
      connection%targetItemsPtr(1)%ptr => target_u_item
      connection%targetItemsPtr(2)%ptr => target_v_item
      connection%converterPtr => converter
      connection%targetMask = [1, 0]

      call time%set(0.5_dp)
      target_u = [10.0_dp, ec_undef_hp]
      target_v = [20.0_dp, ec_undef_hp]
      success = ecConverterPerformConversions(connection, time)
      call f90_expect_true(success, 'unimagdir ADD must ignore undefined values at masked-out targets')
      call f90_expect_near(target_u(1), 15.0_dp, 1.0e-12_dp, 'active target u must be updated')
      call f90_expect_near(target_v(1), 20.0_dp, 1.0e-12_dp, 'active target v must be updated')
      call f90_expect_near(target_u(2), ec_undef_hp, 1.0e-12_dp, 'masked-out target u must remain unchanged')
      call f90_expect_near(target_v(2), ec_undef_hp, 1.0e-12_dp, 'masked-out target v must remain unchanged')

      converter%operandType = EC_OPERAND_REPLACE
      target_u = [10.0_dp, 99.0_dp]
      target_v = [20.0_dp, -88.0_dp]
      success = ecConverterPerformConversions(connection, time)
      call f90_expect_true(success, 'unimagdir REPLACE must succeed with a target mask')
      call f90_expect_near(target_u(1), 5.0_dp, 1.0e-12_dp, 'active target u must be replaced')
      call f90_expect_near(target_v(1), 0.0_dp, 1.0e-12_dp, 'active target v must be replaced')
      call f90_expect_near(target_u(2), 99.0_dp, 1.0e-12_dp, 'masked-out target u must stay preserved')
      call f90_expect_near(target_v(2), -88.0_dp, 1.0e-12_dp, 'masked-out target v must stay preserved')

      deallocate (connection%sourceItemsPtr, connection%targetItemsPtr, connection%targetMask)
      call clear_ec_message()
   end subroutine test_converter_target_mask_unimagdir

   !$gtest
   subroutine test_converter_target_mask_arcinfo() bind(C)
      use m_ec_converter, only: ecConverterPerformConversions
      use m_ec_message, only: clear_ec_message
      use m_ec_parameters, only: convType_arcinfo, elmSetType_cartesian_equidistant, interpolate_timespace, EC_OPERAND_REPLACE
      use m_ec_typedefs, only: tEcConnection, tEcConverter, tEcElementSet, tEcField, tEcItem
      use time_class, only: c_time

      type(tEcConnection) :: connection
      type(tEcConverter), target :: converter
      type(tEcItem), target :: source_item, target_item
      type(tEcField), target :: src_t0, src_t1, target_field
      type(tEcElementSet), target :: source_set, target_set
      real(dp), dimension(4), target :: src_vals_t0, src_vals_t1
      real(dp), dimension(2), target :: target_x, target_y, target_vals
      type(c_time) :: time
      logical :: success

      source_set%ofType = elmSetType_cartesian_equidistant
      source_set%x0 = 0.0_dp
      source_set%y0 = 0.0_dp
      source_set%dx = 10.0_dp
      source_set%dy = 10.0_dp
      source_set%n_cols = 2
      source_set%n_rows = 2
      src_vals_t0 = [4.0_dp, 4.0_dp, 4.0_dp, 4.0_dp]
      src_vals_t1 = [8.0_dp, 8.0_dp, 8.0_dp, 8.0_dp]
      src_t0%timesteps = 0.0_dp
      src_t1%timesteps = 1.0_dp
      src_t0%arr1dPtr => src_vals_t0
      src_t1%arr1dPtr => src_vals_t1
      source_item%elementSetPtr => source_set
      source_item%sourceT0FieldPtr => src_t0
      source_item%sourceT1FieldPtr => src_t1

      target_x = [5.0_dp, 5.0_dp]
      target_y = [5.0_dp, 5.0_dp]
      target_vals = [10.0_dp, 99.0_dp]
      target_set%nCoordinates = 2
      target_set%x => target_x
      target_set%y => target_y
      target_field%arr1dPtr => target_vals
      target_item%elementSetPtr => target_set
      target_item%targetFieldPtr => target_field

      converter%ofType = convType_arcinfo
      converter%interpolationType = interpolate_timespace
      converter%operandType = EC_OPERAND_REPLACE

      allocate (connection%sourceItemsPtr(1), connection%targetItemsPtr(1))
      connection%nSourceItems = 1
      connection%nTargetItems = 1
      connection%sourceItemsPtr(1)%ptr => source_item
      connection%targetItemsPtr(1)%ptr => target_item
      connection%converterPtr => converter
      connection%targetMask = [1, 0]

      call time%set(0.5_dp)
      success = ecConverterPerformConversions(connection, time)
      call f90_expect_true(success, 'arcinfo conversion must succeed with a target mask')
      call f90_expect_near(target_vals(1), 6.0_dp, 1.0e-12_dp, 'active arcinfo target must be updated')
      call f90_expect_near(target_vals(2), 99.0_dp, 1.0e-12_dp, 'masked-out arcinfo target must remain unchanged')

      deallocate (connection%sourceItemsPtr, connection%targetItemsPtr, connection%targetMask)
      call clear_ec_message()
   end subroutine test_converter_target_mask_arcinfo

   !$gtest
   subroutine test_converter_target_mask_spiderweb() bind(C)
      use m_ec_converter, only: ecConverterPerformConversions
      use m_ec_message, only: clear_ec_message
      use m_ec_parameters, only: convType_spiderweb, EC_OPERAND_REPLACE
      use m_ec_typedefs, only: tEcConnection, tEcConverter, tEcElementSet, tEcField, tEcItem, tEcQuantity
      use time_class, only: c_time

      type(tEcConnection) :: connection
      type(tEcConverter), target :: converter
      type(tEcItem), target :: source_item, target_item
      type(tEcQuantity), target :: source_qty, target_qty
      type(tEcField), target :: src_t0, src_t1, target_field
      type(tEcElementSet), target :: source_set, target_set
      real(dp), dimension(15), target :: p_drop_t0, p_drop_t1
      real(dp), dimension(2), target :: target_x, target_y, target_vals
      type(c_time) :: time
      logical :: success

      source_qty%name = 'p_drop'
      target_qty%name = 'airpressure'
      source_set%n_rows = 3
      source_set%n_cols = 5
      source_set%radius = 500000.0_dp
      source_set%spw_merge_frac = 0.5_dp
      source_set%utmzone = 'undefined'
      source_set%gridunit = 'degree'

      p_drop_t0 = 100.0_dp
      p_drop_t1 = 100.0_dp
      src_t0%timesteps = 0.0_dp
      src_t1%timesteps = 1.0_dp
      src_t0%x_spw_eye = 10.0_dp
      src_t0%y_spw_eye = 10.0_dp
      src_t1%x_spw_eye = 10.0_dp
      src_t1%y_spw_eye = 10.0_dp
      src_t0%arr1dPtr => p_drop_t0
      src_t1%arr1dPtr => p_drop_t1
      source_item%quantityPtr => source_qty
      source_item%elementSetPtr => source_set
      source_item%sourceT0FieldPtr => src_t0
      source_item%sourceT1FieldPtr => src_t1

      target_x = [10.5_dp, 10.5_dp]
      target_y = [10.5_dp, 10.5_dp]
      target_vals = [101325.0_dp, 101325.0_dp]
      target_set%nCoordinates = 2
      target_set%x => target_x
      target_set%y => target_y
      target_field%arr1dPtr => target_vals
      target_item%quantityPtr => target_qty
      target_item%elementSetPtr => target_set
      target_item%targetFieldPtr => target_field

      converter%ofType = convType_spiderweb
      converter%operandType = EC_OPERAND_REPLACE

      allocate (connection%sourceItemsPtr(1), connection%targetItemsPtr(1))
      connection%nSourceItems = 1
      connection%nTargetItems = 1
      connection%sourceItemsPtr(1)%ptr => source_item
      connection%targetItemsPtr(1)%ptr => target_item
      connection%converterPtr => converter
      connection%targetMask = [1, 0]

      call time%set(0.5_dp)
      success = ecConverterPerformConversions(connection, time)
      call f90_expect_true(success, 'spiderweb conversion must succeed with a target mask')
      call f90_expect_near(target_vals(1), 101225.0_dp, 1.0e-12_dp, 'active spiderweb target must be updated')
      call f90_expect_near(target_vals(2), 101325.0_dp, 1.0e-12_dp, 'masked-out spiderweb target must remain unchanged')

      deallocate (connection%sourceItemsPtr, connection%targetItemsPtr, connection%targetMask)
      call clear_ec_message()
   end subroutine test_converter_target_mask_spiderweb

   !$gtest
   subroutine test_converter_target_mask_fourier() bind(C)
      use m_ec_converter, only: ecConverterPerformConversions
      use m_ec_message, only: clear_ec_message
      use m_ec_parameters, only: convType_fourier, interpolate_passthrough, EC_OPERAND_ADD, EC_OPERAND_REPLACE, &
                                 EC_OPERAND_REPLACE_ELEMENT, ec_undef_hp
      use m_ec_typedefs, only: tEcConnection, tEcConverter, tEcElementSet, tEcField, tEcItem, tEcQuantity, tEcTimeFrame
      use time_class, only: c_time

      type(tEcConnection) :: connection
      type(tEcConverter), target :: converter
      type(tEcItem), target :: period_item, mag_item, phase_item, target_item
      type(tEcQuantity), target :: period_qty, mag_qty, phase_qty
      type(tEcField), target :: period_field, mag_field, phase_field, target_field
      type(tEcElementSet), target :: source_set, target_set
      type(tEcTimeFrame), target :: tframe
      real(dp), dimension(1), target :: period_val, mag_val, phase_val, target_vals
      type(c_time) :: time
      logical :: success

      period_qty%name = 'period'
      mag_qty%name = 'magnitude'
      phase_qty%name = 'phase'
      period_val = [0.0_dp]
      mag_val = [3.5_dp]
      phase_val = [0.0_dp]
      source_set%nCoordinates = 1
      tframe%ec_refdate = 0.0_dp
      period_field%arr1dPtr => period_val
      mag_field%arr1dPtr => mag_val
      phase_field%arr1dPtr => phase_val
      period_item%quantityPtr => period_qty
      period_item%elementSetPtr => source_set
      period_item%tframe => tframe
      period_item%sourceT0FieldPtr => period_field
      mag_item%quantityPtr => mag_qty
      mag_item%sourceT0FieldPtr => mag_field
      phase_item%quantityPtr => phase_qty
      phase_item%sourceT0FieldPtr => phase_field

      target_set%nCoordinates = 1
      target_field%arr1dPtr => target_vals
      target_item%elementSetPtr => target_set
      target_item%targetFieldPtr => target_field

      converter%ofType = convType_fourier
      converter%interpolationType = interpolate_passthrough

      allocate (connection%sourceItemsPtr(3), connection%targetItemsPtr(1))
      connection%nSourceItems = 3
      connection%nTargetItems = 1
      connection%sourceItemsPtr(1)%ptr => period_item
      connection%sourceItemsPtr(2)%ptr => mag_item
      connection%sourceItemsPtr(3)%ptr => phase_item
      connection%targetItemsPtr(1)%ptr => target_item
      connection%converterPtr => converter
      connection%targetMask = [0]

      call time%set(0.0_dp)
      converter%operandType = EC_OPERAND_REPLACE
      target_vals = [99.0_dp]
      success = ecConverterPerformConversions(connection, time)
      call f90_expect_true(success, 'fourier REPLACE must succeed when masked out')
      call f90_expect_near(target_vals(1), 99.0_dp, 1.0e-12_dp, 'masked-out fourier target must remain unchanged')

      converter%operandType = EC_OPERAND_ADD
      target_vals = [ec_undef_hp]
      success = ecConverterPerformConversions(connection, time)
      call f90_expect_true(success, 'fourier ADD must skip undefined check when masked out')
      call f90_expect_near(target_vals(1), ec_undef_hp, 1.0e-12_dp, 'masked-out fourier target must stay undefined')

      converter%operandType = EC_OPERAND_REPLACE_ELEMENT
      converter%targetIndex = 1
      success = ecConverterPerformConversions(connection, time)
      call f90_expect_false(success, 'fourier REPLACE_ELEMENT must reject an allocated target mask')

      deallocate (connection%sourceItemsPtr, connection%targetItemsPtr, connection%targetMask)
      call clear_ec_message()
   end subroutine test_converter_target_mask_fourier

   !$gtest
   subroutine test_converter_target_mask_netcdf_samples() bind(C)
      use m_ec_converter, only: ecConverterPerformConversions, ecConverterUpdateWeightFactors
      use m_ec_message, only: clear_ec_message
      use m_ec_parameters, only: convType_netcdf, elmSetType_cartesian, elmSetType_samples, &
                                 interpolate_spacetimeSaveWeightFactors, interpolate_nearest_neighbour, &
                                 EC_OPERAND_REPLACE, EC_COORDS_CARTESIAN, ec_undef_int, ec_undef_hp
      use m_ec_typedefs, only: tEcConnection, tEcConverter, tEcElementSet, tEcField, tEcInstance, tEcItem, tEcQuantity
      use time_class, only: c_time

      type(tEcInstance), target :: instance
      type(tEcInstance), pointer :: instance_ptr
      type(tEcConnection) :: connection
      type(tEcConverter), target :: converter
      type(tEcItem), target :: source_item, target_item
      type(tEcQuantity), target :: source_qty
      type(tEcField), target :: src_t0, src_t1, target_field
      type(tEcElementSet), target :: source_set, target_set
      real(dp), dimension(3), target :: src_x, src_y
      real(dp), dimension(2), target :: tgt_x, tgt_y, target_vals
      type(c_time) :: time
      logical :: success

      instance%coordsystem = EC_COORDS_CARTESIAN
      instance_ptr => instance

      src_x = [0.0_dp, 10.0_dp, 0.0_dp]
      src_y = [0.0_dp, 0.0_dp, 10.0_dp]
      source_set%ofType = elmSetType_samples
      source_set%nCoordinates = 3
      source_set%n_cols = 3
      source_set%n_rows = 1
      source_set%n_layers = 0
      source_set%x => src_x
      source_set%y => src_y
      source_qty%name = 'air_pressure'
      source_qty%fillvalue = -999.0_dp
      src_t0%timesteps = 0.0_dp
      src_t1%timesteps = 1.0_dp
      src_t0%arr1d = [10.0_dp, 10.0_dp, 10.0_dp]
      src_t1%arr1d = [10.0_dp, 10.0_dp, 10.0_dp]
      source_item%quantityPtr => source_qty
      source_item%elementSetPtr => source_set
      source_item%sourceT0FieldPtr => src_t0
      source_item%sourceT1FieldPtr => src_t1

      tgt_x = [2.0_dp, 3.0_dp]
      tgt_y = [2.0_dp, 3.0_dp]
      target_set%ofType = elmSetType_cartesian
      target_set%nCoordinates = 2
      target_set%x => tgt_x
      target_set%y => tgt_y
      target_field%arr1dPtr => target_vals
      target_field%missingValue = ec_undef_hp
      target_item%elementSetPtr => target_set
      target_item%targetFieldPtr => target_field

      converter%ofType = convType_netcdf
      converter%operandType = EC_OPERAND_REPLACE

      allocate (connection%sourceItemsPtr(1), connection%targetItemsPtr(1))
      connection%nSourceItems = 1
      connection%nTargetItems = 1
      connection%sourceItemsPtr(1)%ptr => source_item
      connection%targetItemsPtr(1)%ptr => target_item
      connection%converterPtr => converter
      connection%targetMask = [1, 0]

      call time%set(0.5_dp)

      ! Triangulation path
      converter%interpolationType = interpolate_spacetimeSaveWeightFactors
      success = ecConverterUpdateWeightFactors(instance_ptr, connection)
      if (.not. f90_assert_true(success, 'sample triangulation weights must build with a target mask')) then
         deallocate (connection%sourceItemsPtr, connection%targetItemsPtr, connection%targetMask)
         return
      end if
      call f90_expect_true(all(converter%indexWeight%indices(:, 2) == ec_undef_int), &
                           'masked-out triangulation target must have undefined indices')
      target_vals = [1.0_dp, 99.0_dp]
      success = ecConverterPerformConversions(connection, time)
      call f90_expect_true(success, 'sample triangulation conversion must succeed with a target mask')
      call f90_expect_near(target_vals(1), 10.0_dp, 1.0e-12_dp, 'active sample triangulation target must be updated')
      call f90_expect_near(target_vals(2), 99.0_dp, 1.0e-12_dp, 'masked-out sample triangulation target must remain unchanged')

      deallocate (converter%indexWeight%indices, converter%indexWeight%weightFactors, converter%indexWeight)
      converter%indexWeight => null()

      ! Nearest-neighbour path
      converter%interpolationType = interpolate_nearest_neighbour
      success = ecConverterUpdateWeightFactors(instance_ptr, connection)
      if (.not. f90_assert_true(success, 'sample nearest-neighbour weights must build with a target mask')) then
         deallocate (connection%sourceItemsPtr, connection%targetItemsPtr, connection%targetMask)
         return
      end if
      target_vals = [1.0_dp, 99.0_dp]
      success = ecConverterPerformConversions(connection, time)
      call f90_expect_true(success, 'sample nearest-neighbour conversion must succeed with a target mask')
      call f90_expect_near(target_vals(1), 10.0_dp, 1.0e-12_dp, 'active sample nearest-neighbour target must be updated')
      call f90_expect_near(target_vals(2), 99.0_dp, 1.0e-12_dp, 'masked-out sample nearest-neighbour target must remain unchanged')

      deallocate (converter%indexWeight%indices, converter%indexWeight%weightFactors, converter%indexWeight)
      deallocate (connection%sourceItemsPtr, connection%targetItemsPtr, connection%targetMask)
      call clear_ec_message()
   end subroutine test_converter_target_mask_netcdf_samples

   !$gtest
   subroutine test_converter_target_mask_netcdf_time_interpolation() bind(C)
      use m_ec_converter, only: ecConverterPerformConversions
      use m_ec_message, only: clear_ec_message
      use m_ec_parameters, only: convType_netcdf, elmSetType_cartesian, interpolate_time, interpolate_time_extrapolation_ok, &
                                 EC_OPERAND_ADD, ec_undef_hp
      use m_ec_typedefs, only: tEcConnection, tEcConverter, tEcElementSet, tEcField, tEcItem, tEcQuantity
      use time_class, only: c_time

      type(tEcConnection) :: connection
      type(tEcConverter), target :: converter
      type(tEcItem), target :: source_item, target_item
      type(tEcQuantity), target :: source_qty
      type(tEcField), target :: src_t0, src_t1, target_field
      type(tEcElementSet), target :: source_set, target_set
      real(dp), dimension(2), target :: src_vals_t0, src_vals_t1, target_vals
      type(c_time) :: time
      logical :: success

      source_qty%name = 'air_pressure'
      source_set%ofType = elmSetType_cartesian
      source_set%nCoordinates = 2
      src_vals_t0 = [4.0_dp, 40.0_dp]
      src_vals_t1 = [8.0_dp, 80.0_dp]
      src_t0%arr1dPtr => src_vals_t0
      src_t1%arr1dPtr => src_vals_t1
      source_item%quantityPtr => source_qty
      source_item%elementSetPtr => source_set
      source_item%sourceT0FieldPtr => src_t0
      source_item%sourceT1FieldPtr => src_t1

      target_set%ofType = elmSetType_cartesian
      target_set%nCoordinates = 2
      target_field%arr1dPtr => target_vals
      target_item%elementSetPtr => target_set
      target_item%targetFieldPtr => target_field

      converter%ofType = convType_netcdf
      converter%operandType = EC_OPERAND_ADD

      allocate (connection%sourceItemsPtr(1), connection%targetItemsPtr(1))
      connection%nSourceItems = 1
      connection%nTargetItems = 1
      connection%sourceItemsPtr(1)%ptr => source_item
      connection%targetItemsPtr(1)%ptr => target_item
      connection%converterPtr => converter
      connection%targetMask = [1, 0]

      call time%set(0.5_dp)

      ! Two-field time interpolation
      converter%interpolationType = interpolate_time
      src_t0%timesteps = 0.0_dp
      src_t1%timesteps = 1.0_dp
      target_vals = [10.0_dp, ec_undef_hp]
      success = ecConverterPerformConversions(connection, time)
      call f90_expect_true(success, 'netcdf interpolate_time must respect targetMask and ignore masked undefined values')
      call f90_expect_near(target_vals(1), 16.0_dp, 1.0e-12_dp, 'active netcdf time target must be updated')
      call f90_expect_near(target_vals(2), ec_undef_hp, 1.0e-12_dp, 'masked-out netcdf time target must remain unchanged')

      ! Single-field (t0 > t1) branch
      converter%interpolationType = interpolate_time_extrapolation_ok
      src_t0%timesteps = 1.0_dp
      src_t1%timesteps = 0.0_dp
      target_vals = [10.0_dp, ec_undef_hp]
      success = ecConverterPerformConversions(connection, time)
      call f90_expect_true(success, 'netcdf single-field time branch must respect targetMask')
      call f90_expect_near(target_vals(1), 14.0_dp, 1.0e-12_dp, 'active single-field netcdf target must be updated')
      call f90_expect_near(target_vals(2), ec_undef_hp, 1.0e-12_dp, 'masked-out single-field netcdf target must remain unchanged')

      deallocate (connection%sourceItemsPtr, connection%targetItemsPtr, connection%targetMask)
      call clear_ec_message()
   end subroutine test_converter_target_mask_netcdf_time_interpolation

   !$gtest
   subroutine test_converter_target_mask_netcdf_station_harmonics() bind(C)
      use m_ec_converter, only: ecConverterPerformConversions, ecConverterUpdateWeightFactors
      use m_ec_message, only: clear_ec_message
      use m_ec_parameters, only: convType_netcdf, elmSetType_cartesian, elmSetType_samples, &
                                 interpolate_spacetimeSaveWeightFactors, interpolate_nearest_neighbour, &
                                 EC_OPERAND_ADD, EC_COORDS_CARTESIAN, ec_undef_hp
      use m_ec_typedefs, only: tEcConnection, tEcConverter, tEcElementSet, tEcField, tEcHarmonicsFrame, &
                               tEcInstance, tEcItem, tEcQuantity, tEcTimeFrame
      use time_class, only: c_time

      type(tEcInstance), target :: instance
      type(tEcInstance), pointer :: instance_ptr
      type(tEcConnection) :: connection
      type(tEcConverter), target :: converter
      type(tEcItem), target :: source_item, target_item
      type(tEcQuantity), target :: source_qty
      type(tEcField), target :: src_t0, src_t1, target_field
      type(tEcElementSet), target :: source_set, target_set
      type(tEcHarmonicsFrame), target :: hframe
      type(tEcTimeFrame), target :: tframe
      real(dp), dimension(3), target :: src_x, src_y
      real(dp), dimension(2), target :: tgt_x, tgt_y, target_vals
      type(c_time) :: time
      logical :: success

      instance%coordsystem = EC_COORDS_CARTESIAN
      instance_ptr => instance

      src_x = [0.0_dp, 10.0_dp, 0.0_dp]
      src_y = [0.0_dp, 0.0_dp, 10.0_dp]
      source_set%ofType = elmSetType_samples
      source_set%nCoordinates = 3
      source_set%n_cols = 3
      source_set%n_rows = 1
      source_set%n_layers = 0
      source_set%x => src_x
      source_set%y => src_y
      source_qty%name = 'air_pressure'
      source_qty%fillvalue = -999.0_dp
      src_t1%arr1d = [2.0_dp, 2.0_dp, 2.0_dp]
      allocate (hframe%phases(3, 1))
      hframe%phases = 0.0_dp
      hframe%ec_period = 86400
      tframe%ec_refdate = 59000.0_dp
      source_item%id = 1
      source_item%quantityPtr => source_qty
      source_item%elementSetPtr => source_set
      source_item%sourceT0FieldPtr => src_t0
      source_item%sourceT1FieldPtr => src_t1
      source_item%hframe => hframe
      source_item%tframe => tframe

      tgt_x = [2.0_dp, 3.0_dp]
      tgt_y = [2.0_dp, 3.0_dp]
      target_set%ofType = elmSetType_cartesian
      target_set%nCoordinates = 2
      target_set%x => tgt_x
      target_set%y => tgt_y
      target_field%arr1dPtr => target_vals
      target_field%missingValue = ec_undef_hp
      target_item%id = 2
      target_item%elementSetPtr => target_set
      target_item%targetFieldPtr => target_field

      converter%ofType = convType_netcdf
      converter%operandType = EC_OPERAND_ADD

      allocate (connection%sourceItemsPtr(1), connection%targetItemsPtr(1))
      connection%nSourceItems = 1
      connection%nTargetItems = 1
      connection%sourceItemsPtr(1)%ptr => source_item
      connection%targetItemsPtr(1)%ptr => target_item
      connection%converterPtr => converter
      connection%targetMask = [1, 0]

      call time%set(59000.0_dp)

      ! Triangulation path
      converter%id = 10
      converter%interpolationType = interpolate_spacetimeSaveWeightFactors
      success = ecConverterUpdateWeightFactors(instance_ptr, connection)
      if (.not. f90_assert_true(success, 'harmonic station triangulation weights must build')) then
         deallocate (connection%sourceItemsPtr, connection%targetItemsPtr, connection%targetMask, hframe%phases)
         return
      end if
      target_vals = [10.0_dp, ec_undef_hp]
      success = ecConverterPerformConversions(connection, time)
      call f90_expect_true(success, 'harmonic station triangulation must respect targetMask')
      call f90_expect_near(target_vals(1), 12.0_dp, 1.0e-12_dp, 'active harmonic station target must update')
      call f90_expect_near(target_vals(2), ec_undef_hp, 1.0e-12_dp, 'masked-out harmonic station target must stay unchanged')

      deallocate (converter%indexWeight%indices, converter%indexWeight%weightFactors, converter%indexWeight)
      converter%indexWeight => null()

      ! Nearest-neighbour path
      converter%id = 11
      converter%interpolationType = interpolate_nearest_neighbour
      success = ecConverterUpdateWeightFactors(instance_ptr, connection)
      if (.not. f90_assert_true(success, 'harmonic station nearest-neighbour weights must build')) then
         deallocate (connection%sourceItemsPtr, connection%targetItemsPtr, connection%targetMask, hframe%phases)
         return
      end if
      target_vals = [10.0_dp, ec_undef_hp]
      success = ecConverterPerformConversions(connection, time)
      call f90_expect_true(success, 'harmonic station nearest-neighbour must respect targetMask')
      call f90_expect_near(target_vals(1), 12.0_dp, 1.0e-12_dp, 'active harmonic station target must update')
      call f90_expect_near(target_vals(2), ec_undef_hp, 1.0e-12_dp, 'masked-out harmonic station target must stay unchanged')

      deallocate (converter%indexWeight%indices, converter%indexWeight%weightFactors, converter%indexWeight)
      deallocate (connection%sourceItemsPtr, connection%targetItemsPtr, connection%targetMask, hframe%phases)
      call clear_ec_message()
   end subroutine test_converter_target_mask_netcdf_station_harmonics

   !$gtest
   subroutine test_converter_target_mask_unsupported_errors() bind(C)
      use m_ec_converter, only: ecConverterPerformConversions
      use m_ec_message, only: clear_ec_message
      use m_ec_parameters, only: convType_uniform, convType_uniform_to_magnitude, &
                                 interpolate_timespace, interpolate_time, &
                                 EC_OPERAND_REPLACE_ELEMENT, EC_OPERAND_ADD_ELEMENT, EC_OPERAND_REPLACE, ec_undef_hp
      use m_ec_typedefs, only: tEcConnection, tEcConverter, tEcElementSet, tEcField, tEcItem, tEcQuantity
      use time_class, only: c_time

      type(tEcConnection) :: connection
      type(tEcConverter), target :: converter
      type(tEcItem), target :: source_item, target_item
      type(tEcQuantity), target :: source_qty
      type(tEcField), target :: src_t0, src_t1, target_field
      type(tEcElementSet), target :: source_set, target_set
      real(dp), dimension(1), target :: uniform_src_t0, uniform_src_t1
      real(dp), dimension(2), target :: wind_src_t0, wind_src_t1, target_vals
      type(c_time) :: time
      logical :: success

      uniform_src_t0 = [5.0_dp]
      uniform_src_t1 = [5.0_dp]
      wind_src_t0 = [3.0_dp, 4.0_dp]
      wind_src_t1 = [3.0_dp, 4.0_dp]
      src_t0%timesteps = 0.0_dp
      src_t1%timesteps = 1.0_dp
      src_t0%arr1dPtr => uniform_src_t0
      src_t1%arr1dPtr => uniform_src_t1
      source_qty%name = 'air_pressure'
      source_qty%vectorMax = 1
      source_set%nCoordinates = 1
      source_item%quantityPtr => source_qty
      source_item%elementSetPtr => source_set
      source_item%sourceT0FieldPtr => src_t0
      source_item%sourceT1FieldPtr => src_t1

      target_set%nCoordinates = 2
      target_vals = [10.0_dp, 20.0_dp]
      target_field%arr1dPtr => target_vals
      target_field%missingValue = ec_undef_hp
      target_item%elementSetPtr => target_set
      target_item%targetFieldPtr => target_field

      allocate (connection%sourceItemsPtr(1), connection%targetItemsPtr(1))
      connection%nSourceItems = 1
      connection%nTargetItems = 1
      connection%sourceItemsPtr(1)%ptr => source_item
      connection%targetItemsPtr(1)%ptr => target_item
      connection%converterPtr => converter
      connection%targetMask = [1, 0]

      call time%set(0.5_dp)

      converter%ofType = convType_uniform
      converter%targetIndex = 1
      converter%interpolationType = interpolate_timespace
      converter%operandType = EC_OPERAND_REPLACE_ELEMENT
      success = ecConverterPerformConversions(connection, time)
      call f90_expect_false(success, 'uniform REPLACE_ELEMENT must reject an allocated target mask')

      converter%operandType = EC_OPERAND_ADD_ELEMENT
      success = ecConverterPerformConversions(connection, time)
      call f90_expect_false(success, 'uniform ADD_ELEMENT must reject an allocated target mask')

      converter%interpolationType = interpolate_time
      converter%operandType = EC_OPERAND_REPLACE_ELEMENT
      success = ecConverterPerformConversions(connection, time)
      call f90_expect_false(success, 'uniform interpolate_time REPLACE_ELEMENT must reject an allocated target mask')

      src_t0%arr1dPtr => wind_src_t0
      src_t1%arr1dPtr => wind_src_t1
      converter%ofType = convType_uniform_to_magnitude
      converter%interpolationType = interpolate_time
      converter%operandType = EC_OPERAND_REPLACE
      success = ecConverterPerformConversions(connection, time)
      call f90_expect_false(success, 'uniform_to_magnitude must reject an allocated target mask')

      deallocate (connection%sourceItemsPtr, connection%targetItemsPtr, connection%targetMask)
      call clear_ec_message()
   end subroutine test_converter_target_mask_unsupported_errors

end module test_ec_module
