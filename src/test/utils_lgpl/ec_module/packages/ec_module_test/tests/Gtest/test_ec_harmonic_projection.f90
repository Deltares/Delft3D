module test_ec_harmonic_projection
   use precision, only: dp
   use assertions_gtest
   use m_ec_typedefs
   use m_ec_parameters
   use m_ec_converter, only: prepare_projections, ecConverterHarmonicCoefficients, apply_harmonics, ecConverterPerformConversions
   use m_ec_converter, only: compute_source_harmonic_coefficients, interpolate_harmonic_coefficients, find_harmonic_extrapolation_donors
   use m_mathconstants, only: PI
   use time_class, only: c_time
   use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan

   implicit none(type, external)
   private

   real(dp), dimension(4), parameter :: SOURCE_COS = [2.0_dp, 0.0_dp, -6.0_dp, 0.0_dp]
   real(dp), dimension(4), parameter :: SOURCE_SIN = [0.0_dp, 4.0_dp, 0.0_dp, -8.0_dp]

   type source_fixture
      type(tEcItem) :: source
      type(tEcField) :: amplitudes
      type(tEcQuantity) :: quantity
      type(tEcElementSet) :: source_set
      type(tEcHarmonicsFrame) :: frame
   end type source_fixture

   type, extends(source_fixture) :: coefficient_fixture
      type(tEcItem) :: target
      type(tEcElementSet) :: target_set
      type(tEcConverter) :: converter
      type(tEcIndexWeight) :: weights
   end type coefficient_fixture

contains

   subroutine setup_source(f)
      class(source_fixture), target, intent(inout) :: f !< Source data and pointers to initialize.

      f%source%sourceT1FieldPtr => f%amplitudes
      f%source%quantityPtr => f%quantity
      f%source%elementSetPtr => f%source_set
      f%source%hframe => f%frame
      f%quantity%name = 'air_pressure'
      f%quantity%fillvalue = -999.0_dp
      f%amplitudes%arr1d = [2.0_dp, 4.0_dp, 6.0_dp, 8.0_dp]
      f%source_set%ofType = elmSetType_cartesian
      f%source_set%n_cols = 2
      f%source_set%n_rows = 2
      f%amplitudes%bbox = [1, 1, 2, 2]
   end subroutine setup_source

   !> Both amplitudes and phases cover every grid point.
   subroutine setup_dense(f)
      class(source_fixture), target, intent(inout) :: f !< Dense source data to initialize.

      call setup_source(f)
      allocate (f%frame%phases(2, 2))
      f%frame%phases(:, 1) = [0.0_dp, 90.0_dp]
      f%frame%phases(:, 2) = [180.0_dp, 270.0_dp]
   end subroutine setup_dense

   !> Only selected grid points have amplitudes, indexed by ia/ja; phases still cover the full grid.
   subroutine setup_sparse(f)
      class(source_fixture), target, intent(inout) :: f !< Sparse source data to initialize.

      call setup_source(f)
      f%source_set%ofType = elmSetType_cartesian
      f%source_set%n_cols = 4
      f%source_set%n_rows = 3
      f%amplitudes%issparse = 1
      allocate (f%amplitudes%ia(4), f%amplitudes%ja(4), f%frame%phases(4, 3))
      f%amplitudes%ia = [1, 1, 3, 5]
      f%amplitudes%ja = [2, 3, 2, 3]
      f%frame%phases = -999.0_dp
      f%frame%phases(2:3, 2) = [0.0_dp, 90.0_dp]
      f%frame%phases(2:3, 3) = [180.0_dp, 270.0_dp]
   end subroutine setup_sparse

   !> Source indices identify individual stations rather than corners of a grid cell.
   subroutine setup_stations(f)
      class(source_fixture), target, intent(inout) :: f !< Station source data to initialize.

      call setup_source(f)
      f%source_set%ofType = elmSetType_samples
      f%source_set%n_cols = 4
      f%source_set%n_rows = 1
      allocate (f%frame%phases(4, 1))
      f%frame%phases(:, 1) = [0.0_dp, 90.0_dp, 180.0_dp, 270.0_dp]
   end subroutine setup_stations

   !> One target has valid data, one needs a donor, and one lies outside the grid.
   subroutine setup_extrapolation(f)
      type(coefficient_fixture), target, intent(inout) :: f !< Source/target pair with a missing grid cell and an outside target.

      call setup_source(f)
      f%target%elementSetPtr => f%target_set
      f%converter%indexWeight => f%weights
      f%source_set%n_cols = 3
      f%source_set%n_rows = 2
      f%amplitudes%bbox = [1, 1, 3, 2]
      f%amplitudes%arr1d = [2.0_dp, 2.0_dp, -999.0_dp, 2.0_dp, 2.0_dp, -999.0_dp]
      allocate (f%frame%phases(3, 2))
      f%frame%phases = 0.0_dp
      f%converter%interpolationType = extrapolate_spacetimeSaveWeightFactors
      f%target_set%ofType = elmSetType_cartesian
      f%target_set%nCoordinates = 3
      allocate (f%weights%indices(2, 3), f%weights%weightFactors(4, 3), f%weights%substndx(3))
      f%weights%indices(:, 1) = [1, 1]
      f%weights%indices(:, 2) = [1, 2]
      f%weights%indices(:, 3) = 0
      f%weights%weightFactors = 0.25_dp
      f%weights%substndx = 0
      allocate (f%target_set%x(3), f%target_set%y(3))
      f%target_set%x = [0.5_dp, 1.5_dp, 100.0_dp]
      f%target_set%y = 0.5_dp
   end subroutine setup_extrapolation

   subroutine cleanup_extrapolation(f)
      type(coefficient_fixture), intent(inout) :: f !< Extrapolation fixture whose pointer-owned arrays are released.

      deallocate (f%weights%indices, f%weights%weightFactors, f%weights%substndx, f%target_set%x, f%target_set%y)
   end subroutine cleanup_extrapolation

   !$gtest
   subroutine test_multiple_projections() bind(C)
      type(tEcHarmonicsFrame), target :: frame
      type(tEcHarmonicProjection), pointer :: proj
      ! A shared phase frame needs separate projections when any source, target or converter ID differs.
      integer, dimension(3, 4), parameter :: IDS = reshape([1, 2, 3, &
                                                            4, 2, 3, &
                                                            1, 5, 3, &
                                                            1, 2, 6], [3, 4])
      integer :: i

      do i = 1, size(IDS, 2)
         call prepare_projections(frame, IDS(1, i), IDS(2, i), IDS(3, i), proj)
         call f90_expect_eq(size(frame%projections), i, 'different source, target or converter IDs must create separate entries')
         call f90_expect_false(allocated(proj%cos_weight), 'new projections must not be marked as computed')
         proj%cos_weight = [real(i, dp)]
         proj%sin_weight = [-real(i, dp)]
         proj%mask = [i]
      end do
      do i = 1, size(IDS, 2)
         call prepare_projections(frame, IDS(1, i), IDS(2, i), IDS(3, i), proj)
         call f90_expect_eq(size(frame%projections), 4, 'matching source, target and converter IDs must reuse entries')
         call f90_expect_near(proj%cos_weight(1), real(i, dp), 1.0e-12_dp, 'growth must preserve cosine coefficients')
         call f90_expect_near(proj%sin_weight(1), -real(i, dp), 1.0e-12_dp, 'growth must preserve sine coefficients')
         call f90_expect_eq(proj%mask(1), i, 'growth must preserve masks')
      end do
   end subroutine test_multiple_projections

   subroutine check_source_coefficients(f, phase_col, phase_row)
      type(source_fixture), intent(inout) :: f !< Source layout whose coefficients and missing-value handling are checked.
      integer, intent(in) :: phase_col, phase_row !< Phase indices corresponding to the first amplitude.
      real(dp), dimension(:), allocatable :: source_cos, source_sin
      logical, dimension(:), allocatable :: source_valid
      real(dp) :: missing_value
      integer :: missing_kind

      call compute_source_harmonic_coefficients(f%source, source_cos, source_sin, source_valid)
      call f90_expect_near(source_cos, SOURCE_COS, 1.0e-12_dp, 'source cosine must follow amplitude order')
      call f90_expect_near(source_sin, SOURCE_SIN, 1.0e-12_dp, 'source sine must follow amplitude order')
      call f90_expect_true(all(source_valid), 'complete source data must be valid')
      ! Test a missing amplitude and a missing phase, with both a numeric fill value and NaN.
      do missing_kind = 1, 4
         missing_value = -999.0_dp
         if (missing_kind > 2) then
            missing_value = ieee_value(0.0_dp, ieee_quiet_nan)
         end if
         f%quantity%fillvalue = missing_value
         if (mod(missing_kind, 2) == 1) then
            f%amplitudes%arr1d(1) = missing_value
         else
            f%frame%phases(phase_col, phase_row) = missing_value
         end if
         call compute_source_harmonic_coefficients(f%source, source_cos, source_sin, source_valid)
         call f90_expect_false(source_valid(1), 'missing amplitude or phase must invalidate the source')
         call f90_expect_true(all(source_valid(2:)), 'other source points must remain valid')
         f%amplitudes%arr1d(1) = 2.0_dp
         f%frame%phases(phase_col, phase_row) = 0.0_dp
      end do
   end subroutine check_source_coefficients

   !$gtest
   subroutine test_dense_source_coefficients() bind(C)
      type(source_fixture), target :: f
      call setup_dense(f)
      call check_source_coefficients(f, 1, 1)
   end subroutine test_dense_source_coefficients

   !$gtest
   subroutine test_sparse_source_coefficients() bind(C)
      type(source_fixture), target :: f
      call setup_sparse(f)
      call check_source_coefficients(f, 2, 2)
      deallocate (f%amplitudes%ia, f%amplitudes%ja)
   end subroutine test_sparse_source_coefficients

   !$gtest
   subroutine test_station_source_coefficients() bind(C)
      type(source_fixture), target :: f
      call setup_stations(f)
      call check_source_coefficients(f, 1, 1)
   end subroutine test_station_source_coefficients

   !$gtest
   subroutine test_interpolate_grid_coefficients() bind(C)
      type(source_fixture), target :: f
      type(tEcElementSet) :: target_set
      type(tEcIndexWeight) :: weights
      type(tEcHarmonicProjection) :: proj
      integer, dimension(2, 2), target :: indices
      real(dp), dimension(4, 2), target :: factors
      integer :: sparse
      logical :: success

      call setup_source(f)
      target_set%nCoordinates = 2
      weights%indices => indices
      weights%weightFactors => factors
      indices = 0
      factors(:, 1) = [0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp]
      factors(:, 2) = 0.0_dp
      do sparse = 0, 1
         f%amplitudes%issparse = sparse
         indices(:, 1) = [1, 1 + 2 * sparse]
         success = interpolate_harmonic_coefficients(f%source, target_set, weights, SOURCE_COS, SOURCE_SIN, &
                                                    [.true., .true., .true., .true.], proj)
         if (.not. f90_assert_true(success, 'dense and sparse grid interpolation must succeed')) then
            return
         end if
         ! Bilinear weights follow corners 1, 2, 4, 3, not flattened array order.
         call f90_expect_near(proj%cos_weight, [-2.2_dp, 0.0_dp], 1.0e-12_dp, 'cosine must use bilinear corner order')
         call f90_expect_near(proj%sin_weight, [-1.6_dp, 0.0_dp], 1.0e-12_dp, 'sine must use bilinear corner order')
         call f90_expect_true(all(proj%mask == [1, 0]), 'outside targets must be masked out')

         success = interpolate_harmonic_coefficients(f%source, target_set, weights, SOURCE_COS, SOURCE_SIN, &
                                                    [.false., .true., .true., .true.], proj)
         call f90_expect_true(success, 'missing grid sources must not fail interpolation')
         call f90_expect_true(all(proj%mask == 0), 'a missing grid source must invalidate its target')
      end do
   end subroutine test_interpolate_grid_coefficients

   !$gtest
   subroutine test_interpolate_station_coefficients() bind(C)
      type(source_fixture), target :: f
      type(tEcElementSet) :: target_set
      type(tEcIndexWeight) :: weights
      type(tEcHarmonicProjection) :: proj
      integer, dimension(3, 2), target :: indices
      real(dp), dimension(3, 2), target :: factors
      logical :: success

      call setup_source(f)
      f%source_set%ofType = elmSetType_samples
      f%source_set%n_cols = 4
      target_set%nCoordinates = 2
      weights%indices => indices
      weights%weightFactors => factors
      indices(:, 1) = [1, 3, 4]
      indices(:, 2) = [4, 0, 5]
      factors(:, 1) = [0.2_dp, 0.3_dp, 0.5_dp]
      factors(:, 2) = [1.0_dp, 0.0_dp, 0.0_dp]
      success = interpolate_harmonic_coefficients(f%source, target_set, weights, SOURCE_COS, SOURCE_SIN, &
                                                 [.true., .true., .true., .true.], proj)
      if (.not. f90_assert_true(success, 'station interpolation must succeed')) then
         return
      end if
      call f90_expect_near(proj%cos_weight, [-1.4_dp, 0.0_dp], 1.0e-12_dp, 'station cosine must use station weights')
      call f90_expect_near(proj%sin_weight, [-4.0_dp, -8.0_dp], 1.0e-12_dp, 'single-station mapping must skip invalid indices')
      call f90_expect_true(all(proj%mask == 1), 'valid stations must be active')

      success = interpolate_harmonic_coefficients(f%source, target_set, weights, SOURCE_COS, SOURCE_SIN, &
                                                 [.false., .true., .true., .true.], proj)
      call f90_expect_false(success, 'missing station sources must fail interpolation')
   end subroutine test_interpolate_station_coefficients

   !$gtest
   subroutine test_zero_overlap() bind(C)
      type(source_fixture), target :: f
      real(dp), dimension(:), allocatable :: source_cos, source_sin
      logical, dimension(:), allocatable :: source_valid

      call setup_dense(f)
      ! The reader allocates (-3)*(-3) entries for a 4x4 grid with no overlap.
      f%source_set%n_cols = 4
      f%source_set%n_rows = 4
      f%amplitudes%bbox = [4, 4, 0, 0]
      f%amplitudes%arr1d = spread(-999.0_dp, 1, 9)
      deallocate (f%frame%phases)
      allocate (f%frame%phases(4, 4))
      f%frame%phases = 0.0_dp
      call compute_source_harmonic_coefficients(f%source, source_cos, source_sin, source_valid)
      call f90_expect_eq(size(source_cos), 9, 'empty bounding box storage must be handled without reading phases out of bounds')
      call f90_expect_false(any(source_valid), 'zero overlap must have no valid sources')
   end subroutine test_zero_overlap

   !$gtest
   subroutine test_find_harmonic_donors() bind(C)
      type(tEcElementSet) :: target_set
      type(tEcIndexWeight) :: weights
      real(dp), dimension(4), target :: x, y
      integer, dimension(2, 4), target :: indices
      integer, dimension(4), target :: donors
      logical :: success

      x = [0.5_dp, 1.5_dp, 1.6_dp, 100.0_dp]
      y = 0.5_dp
      target_set%x => x
      target_set%y => y
      weights%indices => indices
      weights%substndx => donors
      indices = 1
      indices(:, 4) = 0
      donors = 0
      success = find_harmonic_extrapolation_donors(target_set, weights, [1, 0, 0, 0])
      call f90_expect_true(success, 'missing targets inside the grid must find donors')
      call f90_expect_true(all(donors == [0, 1, 1, 0]), 'missing targets must not donate to each other; outside targets must be skipped')
   end subroutine test_find_harmonic_donors

   !$gtest
   subroutine test_failed_projection_is_not_published() bind(C)
      type(coefficient_fixture), target :: f
      type(tEcHarmonicProjection) :: proj
      integer, dimension(1, 1), target :: indices
      real(dp), dimension(1, 1), target :: factors
      logical :: success

      call setup_stations(f)
      f%target%elementSetPtr => f%target_set
      f%target_set%nCoordinates = 1
      f%converter%indexWeight => f%weights
      f%weights%indices => indices
      f%weights%weightFactors => factors
      indices = 1
      factors = 1.0_dp
      f%frame%phases(1, 1) = f%quantity%fillvalue
      success = ecConverterHarmonicCoefficients(proj, f%source, f%target, f%converter)
      call f90_expect_false(success, 'missing station phases must fail projection initialization')
      call f90_expect_false(allocated(proj%cos_weight), 'failure must not publish cosine coefficients')
      call f90_expect_false(allocated(proj%sin_weight), 'failure must not publish sine coefficients')
      call f90_expect_false(allocated(proj%mask), 'failure must not publish a mask')

      proj%cos_weight = [11.0_dp]
      proj%sin_weight = [12.0_dp]
      proj%mask = [1]
      success = ecConverterHarmonicCoefficients(proj, f%source, f%target, f%converter)
      call f90_expect_false(success, 'reinitialization with missing station phases must fail')
      call f90_expect_near(proj%cos_weight(1), 11.0_dp, 1.0e-12_dp, 'failure must preserve existing cosine coefficients')
      call f90_expect_near(proj%sin_weight(1), 12.0_dp, 1.0e-12_dp, 'failure must preserve existing sine coefficients')
      call f90_expect_eq(proj%mask(1), 1, 'failure must preserve the existing mask')
   end subroutine test_failed_projection_is_not_published

   !$gtest
   subroutine test_known_angles() bind(C)
      real(dp), dimension(3) :: values
      real(dp), dimension(4), parameter :: ANGLES = [0.0_dp, PI / 2.0_dp, PI, -PI / 2.0_dp]
      real(dp), dimension(4), parameter :: EXPECTED = [2.0_dp, 4.0_dp, -2.0_dp, -4.0_dp]
      integer :: i
      logical :: success

      do i = 1, size(ANGLES)
         values = 7.0_dp
         success = apply_harmonics([2.0_dp, 3.0_dp, 0.0_dp], [4.0_dp, 5.0_dp, 0.0_dp], &
                                   [1, 0, 1], ANGLES(i), EC_OPERAND_REPLACE, values)
         if (.not. f90_assert_true(success, 'evaluation must succeed for positive and negative angles')) then
            return
         end if
         call f90_expect_near(values(1), EXPECTED(i), 1.0e-12_dp, 'evaluation must combine sine and cosine coefficients')
         call f90_expect_near(values(2), 7.0_dp, 1.0e-12_dp, 'inactive targets must remain untouched')
         call f90_expect_near(values(3), 0.0_dp, 1.0e-12_dp, 'zero coefficients must produce zero')
      end do
   end subroutine test_known_angles

   !$gtest
   subroutine test_apply_operands() bind(C)
      integer, dimension(8), parameter :: OPERANDS = [EC_OPERAND_REPLACE, EC_OPERAND_REPLACE_ELEMENT, &
                                                      EC_OPERAND_REPLACE_IF_MISSING, EC_OPERAND_ADD, EC_OPERAND_ADD_ELEMENT, EC_OPERAND_MULTIPLY, &
                                                      EC_OPERAND_MINIMUM, EC_OPERAND_MAXIMUM]
      real(dp), dimension(8), parameter :: EXPECTED = [2.0_dp, 2.0_dp, 7.0_dp, 9.0_dp, 9.0_dp, 14.0_dp, 2.0_dp, 7.0_dp]
      real(dp), dimension(2) :: values
      integer :: i
      logical :: success

      do i = 1, size(OPERANDS)
         values = [7.0_dp, ec_undef_hp]
         success = apply_harmonics([2.0_dp, 2.0_dp], [0.0_dp, 0.0_dp], [1, 0], 0.0_dp, OPERANDS(i), values)
         if (.not. f90_assert_true(success, 'undefined inactive targets must not prevent application')) then
            return
         end if
         call f90_expect_near(values(1), EXPECTED(i), 1.0e-12_dp, 'operand must retain its meaning')
         call f90_expect_near(values(2), ec_undef_hp, 1.0e-12_dp, 'inactive targets must stay undefined')
      end do
      values = ec_undef_hp
      success = apply_harmonics([2.0_dp, 3.0_dp], [0.0_dp, 0.0_dp], [1, 1], 0.0_dp, EC_OPERAND_REPLACE_IF_MISSING, values)
      if (.not. f90_assert_true(success, 'replace-if-missing must accept undefined targets')) then
         return
      end if
      call f90_expect_near(values(1), 2.0_dp, 1.0e-12_dp, 'undefined targets must receive harmonic values')
      call f90_expect_near(values(2), 3.0_dp, 1.0e-12_dp, 'all active undefined targets must be replaced')
   end subroutine test_apply_operands

   !$gtest
   subroutine test_apply_undefined_targets() bind(C)
      integer, dimension(4), parameter :: OPERANDS = [EC_OPERAND_ADD, EC_OPERAND_MULTIPLY, EC_OPERAND_MINIMUM, EC_OPERAND_MAXIMUM]
      real(dp), dimension(2) :: values
      integer :: i
      logical :: success

      do i = 1, size(OPERANDS)
         values = [7.0_dp, ec_undef_hp]
         success = apply_harmonics([2.0_dp, 3.0_dp], [0.0_dp, 0.0_dp], [1, 1], 0.0_dp, OPERANDS(i), values)
         call f90_expect_false(success, 'undefined active targets must reject arithmetic operands')
         call f90_expect_near(values(1), 7.0_dp, 1.0e-12_dp, 'validation failure must not partially update targets')
         call f90_expect_near(values(2), ec_undef_hp, 1.0e-12_dp, 'validation failure must preserve undefined targets')
      end do
   end subroutine test_apply_undefined_targets

   !$gtest
   subroutine test_conversion_reuses_projection() bind(C)
      type(coefficient_fixture), target :: f
      type(tEcConnection) :: connection
      type(tEcField), target :: t0, target_field
      type(tEcTimeFrame), target :: time_frame
      type(c_time) :: time
      real(dp), dimension(3), target :: values
      logical :: success
      integer :: step

      call setup_extrapolation(f)
      f%source%id = 1
      f%target%id = 2
      f%converter%id = 3
      f%converter%ofType = convType_netcdf
      f%converter%operandType = EC_OPERAND_ADD
      f%source_set%n_layers = 0
      f%source%sourceT0FieldPtr => t0
      f%source%tframe => time_frame
      f%target%targetFieldPtr => target_field
      time_frame%ec_refdate = 59000.0_dp
      f%frame%ec_period = 86400
      target_field%arr1dPtr => values
      target_field%missingValue = ec_undef_hp
      allocate (connection%sourceItemsPtr(1), connection%targetItemsPtr(1))
      connection%nSourceItems = 1
      connection%nTargetItems = 1
      connection%sourceItemsPtr(1)%ptr => f%source
      connection%targetItemsPtr(1)%ptr => f%target
      connection%converterPtr => f%converter

      do step = 0, 1
         values = [10.0_dp, 20.0_dp, 30.0_dp]
         call time%set(59000.0_dp + 0.5_dp * real(step, dp))
         success = ecConverterPerformConversions(connection, time)
         if (.not. f90_assert_true(success, 'conversion dispatch must evaluate the harmonic')) then
            deallocate (connection%sourceItemsPtr, connection%targetItemsPtr)
            call cleanup_extrapolation(f)
            return
         end if
         call f90_expect_near(values(1), 10.0_dp + 2.0_dp * (-1.0_dp)**step, 1.0e-12_dp, 'time must advance the harmonic')
         call f90_expect_near(values(2), values(1), 1.0e-12_dp, 'extrapolation must copy the final donor value')
         call f90_expect_near(values(3), 30.0_dp, 1.0e-12_dp, 'outside targets must remain untouched')
         call f90_expect_eq(f%weights%substndx(2), 1, 'coefficient construction must prepare the donor')
         call f90_expect_true(f%converter%extrapolated, 'conversion must report extrapolation')
         call f90_expect_near(target_field%timesteps, time%mjd(), 1.0e-12_dp, 'conversion must update the timestamp')
         if (.not. f90_assert_true(allocated(f%frame%projections), 'dispatch must create the projection')) then
            deallocate (connection%sourceItemsPtr, connection%targetItemsPtr)
            call cleanup_extrapolation(f)
            return
         end if
         call f90_expect_eq(size(f%frame%projections), 1, 'subsequent calls must reuse the projection')
         ! Changing the source after the first call exposes accidental rebuilding of the cached coefficients.
         f%amplitudes%arr1d = 100.0_dp
      end do
      f%frame%ec_period = 0
      success = ecConverterPerformConversions(connection, time)
      call f90_expect_false(success, 'conversion must reject a zero harmonic period')
      deallocate (connection%sourceItemsPtr, connection%targetItemsPtr)
      call cleanup_extrapolation(f)
   end subroutine test_conversion_reuses_projection

end module test_ec_harmonic_projection
