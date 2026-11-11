module test_ec_harmonic_projection
   use precision, only: dp
   use assertions_gtest
   use m_ec_typedefs
   use m_ec_parameters
   use m_ec_converter, only: prepare_projections, ecConverterHarmonicCoefficients, apply_harmonics, ecConverterPerformConversions
   use m_mathconstants, only: PI
   use time_class, only: c_time
   use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan

   implicit none(type, external)
   private

   type coefficient_fixture
      type(tEcItem) :: source, target
      type(tEcField) :: amplitudes
      type(tEcQuantity) :: quantity
      type(tEcElementSet) :: source_set, target_set
      type(tEcHarmonicsFrame) :: frame
      type(tEcConverter) :: converter
      type(tEcIndexWeight) :: weights
   end type coefficient_fixture

contains

   subroutine setup_coefficients(f)
      type(coefficient_fixture), target, intent(inout) :: f

      f%source%sourceT1FieldPtr => f%amplitudes
      f%source%quantityPtr => f%quantity
      f%source%elementSetPtr => f%source_set
      f%source%hframe => f%frame
      f%target%elementSetPtr => f%target_set
      f%converter%indexWeight => f%weights
      f%converter%interpolationType = interpolate_spacetimeSaveWeightFactors
      f%quantity%name = 'air_pressure'
      f%quantity%fillvalue = -999.0_dp
      f%target_set%ofType = elmSetType_cartesian
      f%target_set%nCoordinates = 2
      f%amplitudes%arr1d = [2.0_dp, 4.0_dp, 6.0_dp, 8.0_dp]
   end subroutine setup_coefficients

   !> Both amplitudes and phases cover every grid point.
   subroutine setup_dense(f)
      type(coefficient_fixture), target, intent(inout) :: f

      call setup_coefficients(f)
      f%source_set%ofType = elmSetType_cartesian
      f%source_set%n_cols = 2
      f%source_set%n_rows = 2
      f%amplitudes%bbox = [1, 1, 2, 2]
      allocate (f%frame%phases(2, 2))
      f%frame%phases(:, 1) = [0.0_dp, 90.0_dp]
      f%frame%phases(:, 2) = [180.0_dp, 270.0_dp]
      allocate (f%weights%indices(2, 2), f%weights%weightFactors(4, 2))
      f%weights%indices(:, 1) = [1, 1]
      f%weights%indices(:, 2) = 0
      f%weights%weightFactors(:, 1) = [0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp]
      f%weights%weightFactors(:, 2) = 0.0_dp
   end subroutine setup_dense

   !> Only selected grid points have amplitudes, indexed by ia/ja; phases still cover the full grid.
   subroutine setup_sparse(f)
      type(coefficient_fixture), target, intent(inout) :: f

      call setup_coefficients(f)
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
      allocate (f%weights%indices(2, 2), f%weights%weightFactors(4, 2))
      f%weights%indices(:, 1) = [1, 3]
      f%weights%indices(:, 2) = 0
      f%weights%weightFactors(:, 1) = [0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp]
      f%weights%weightFactors(:, 2) = 0.0_dp
   end subroutine setup_sparse

   !> Source indices identify individual stations rather than corners of a grid cell.
   subroutine setup_stations(f)
      type(coefficient_fixture), target, intent(inout) :: f

      call setup_coefficients(f)
      f%source_set%ofType = elmSetType_samples
      f%source_set%n_cols = 4
      f%source_set%n_rows = 1
      allocate (f%frame%phases(4, 1))
      f%frame%phases(:, 1) = [0.0_dp, 90.0_dp, 180.0_dp, 270.0_dp]
      allocate (f%weights%indices(3, 2), f%weights%weightFactors(3, 2))
      f%weights%indices(:, 1) = [1, 3, 4]
      f%weights%indices(:, 2) = [4, 0, 0]
      f%weights%weightFactors(:, 1) = [0.2_dp, 0.3_dp, 0.5_dp]
      f%weights%weightFactors(:, 2) = [1.0_dp, 0.0_dp, 0.0_dp]
   end subroutine setup_stations

   !> One target has valid data, one needs a donor, and one lies outside the grid.
   subroutine setup_extrapolation(f)
      type(coefficient_fixture), target, intent(inout) :: f

      call setup_coefficients(f)
      f%source_set%ofType = elmSetType_cartesian
      f%source_set%n_cols = 3
      f%source_set%n_rows = 2
      f%amplitudes%bbox = [1, 1, 3, 2]
      f%amplitudes%arr1d = [2.0_dp, 2.0_dp, -999.0_dp, 2.0_dp, 2.0_dp, -999.0_dp]
      allocate (f%frame%phases(3, 2))
      f%frame%phases = 0.0_dp
      f%converter%interpolationType = extrapolate_spacetimeSaveWeightFactors
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

   subroutine cleanup(f)
      type(coefficient_fixture), intent(inout) :: f

      deallocate (f%weights%indices, f%weights%weightFactors)
      if (associated(f%weights%substndx)) then
         deallocate (f%weights%substndx)
      end if
      if (f%amplitudes%issparse == 1) then
         deallocate (f%amplitudes%ia, f%amplitudes%ja)
      end if
      if (associated(f%target_set%x)) then
         deallocate (f%target_set%x, f%target_set%y)
      end if
   end subroutine cleanup

   !$gtest
   subroutine prepare_distinct_projections() bind(C)
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
   end subroutine prepare_distinct_projections

   !$gtest
   subroutine dense_coefficients() bind(C)
      type(coefficient_fixture), target :: f
      type(tEcHarmonicProjection) :: proj
      logical :: success

      call setup_dense(f)
      success = ecConverterHarmonicCoefficients(proj, f%source, f%target, f%converter)
      if (.not. f90_assert_true(success, 'dense projection must succeed')) then
         call cleanup(f)
         return
      end if
      ! Bilinear weights follow corners 1, 2, 4, 3, not flattened array order.
      call f90_expect_near(proj%cos_weight(1), -2.2_dp, 1.0e-12_dp, 'dense cosine must use bilinear corner order')
      call f90_expect_near(proj%sin_weight(1), -1.6_dp, 1.0e-12_dp, 'dense sine must use bilinear corner order')
      call f90_expect_true(all(proj%mask == [1, 0]), 'outside grid targets must be masked out')
      call cleanup(f)
   end subroutine dense_coefficients

   !$gtest
   subroutine sparse_coefficients() bind(C)
      type(coefficient_fixture), target :: f
      type(tEcHarmonicProjection) :: proj
      logical :: success

      call setup_sparse(f)
      success = ecConverterHarmonicCoefficients(proj, f%source, f%target, f%converter)
      if (.not. f90_assert_true(success, 'sparse projection must succeed')) then
         call cleanup(f)
         return
      end if
      call f90_expect_near(proj%cos_weight(1), -2.2_dp, 1.0e-12_dp, 'sparse cosine must use full-grid phase indices')
      call f90_expect_near(proj%sin_weight(1), -1.6_dp, 1.0e-12_dp, 'sparse sine must use full-grid phase indices')
      call f90_expect_true(all(proj%mask == [1, 0]), 'outside sparse targets must be masked out')
      call cleanup(f)
   end subroutine sparse_coefficients

   !$gtest
   subroutine station_coefficients() bind(C)
      type(coefficient_fixture), target :: f
      type(tEcHarmonicProjection) :: proj
      logical :: success

      call setup_stations(f)
      success = ecConverterHarmonicCoefficients(proj, f%source, f%target, f%converter)
      if (.not. f90_assert_true(success, 'station projection must succeed')) then
         call cleanup(f)
         return
      end if
      call f90_expect_near(proj%cos_weight(1), -1.4_dp, 1.0e-12_dp, 'station cosine must use station weights')
      call f90_expect_near(proj%sin_weight(1), -4.0_dp, 1.0e-12_dp, 'station sine must use station weights')
      call f90_expect_near(proj%sin_weight(2), -8.0_dp, 1.0e-12_dp, 'a single station must support nearest-neighbour mapping')
      call f90_expect_true(all(proj%mask == 1), 'valid stations must be active')
      call cleanup(f)
   end subroutine station_coefficients

   subroutine check_missing_sources(f, phase_col, phase_row, expect_success)
      type(coefficient_fixture), target, intent(inout) :: f
      integer, intent(in) :: phase_col, phase_row
      logical, intent(in) :: expect_success
      real(dp) :: missing_value
      integer :: missing_kind
      logical :: success

      ! Test a missing amplitude and a missing phase, with both a numeric fill value and NaN.
      do missing_kind = 1, 4
         block
            type(tEcHarmonicProjection) :: proj
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
            success = ecConverterHarmonicCoefficients(proj, f%source, f%target, f%converter)
            call f90_expect_true(success .eqv. expect_success, 'grid missings must be masked; station missings must fail')
            if (success) then
               call f90_expect_eq(proj%mask(1), 0, 'missing amplitude or phase must invalidate the target')
            else
               call f90_expect_false(allocated(proj%cos_weight), 'failure must not publish incomplete coefficients')
            end if
            f%amplitudes%arr1d(1) = 2.0_dp
            f%frame%phases(phase_col, phase_row) = 0.0_dp
         end block
      end do
   end subroutine check_missing_sources

   !$gtest
   subroutine dense_missing_sources() bind(C)
      type(coefficient_fixture), target :: f
      call setup_dense(f)
      call check_missing_sources(f, 1, 1, .true.)
      call cleanup(f)
   end subroutine dense_missing_sources

   !$gtest
   subroutine sparse_missing_sources() bind(C)
      type(coefficient_fixture), target :: f
      call setup_sparse(f)
      call check_missing_sources(f, 2, 2, .true.)
      call cleanup(f)
   end subroutine sparse_missing_sources

   !$gtest
   subroutine station_missing_sources() bind(C)
      type(coefficient_fixture), target :: f
      call setup_stations(f)
      call check_missing_sources(f, 1, 1, .false.)
      call cleanup(f)
   end subroutine station_missing_sources

   !$gtest
   subroutine no_overlapping_targets() bind(C)
      type(coefficient_fixture), target :: f
      type(tEcHarmonicProjection) :: proj
      logical :: success

      call setup_dense(f)
      ! The reader allocates (-3)*(-3) entries for a 4x4 grid with no overlap.
      f%source_set%n_cols = 4
      f%source_set%n_rows = 4
      f%amplitudes%bbox = [4, 4, 0, 0]
      f%amplitudes%arr1d = spread(-999.0_dp, 1, 9)
      deallocate (f%frame%phases)
      allocate (f%frame%phases(4, 4))
      f%frame%phases = 0.0_dp
      f%weights%indices = 0
      success = ecConverterHarmonicCoefficients(proj, f%source, f%target, f%converter)
      if (.not. f90_assert_true(success, 'an empty bounding box must not access phases out of bounds')) then
         call cleanup(f)
         return
      end if
      call f90_expect_true(all(proj%mask == 0), 'no overlap must produce an inactive projection')
      call cleanup(f)
   end subroutine no_overlapping_targets

   !$gtest
   subroutine coefficient_donors() bind(C)
      type(coefficient_fixture), target :: f
      type(tEcHarmonicProjection) :: proj
      logical :: success

      call setup_extrapolation(f)
      success = ecConverterHarmonicCoefficients(proj, f%source, f%target, f%converter)
      if (.not. f90_assert_true(success, 'projection must find a donor for a missing grid cell')) then
         call cleanup(f)
         return
      end if
      call f90_expect_true(all(proj%mask == [1, 0, 0]), 'only the complete grid cell must be active')
      call f90_expect_near(proj%cos_weight(1), 2.0_dp, 1.0e-12_dp, 'donor coefficients must be projected')
      call f90_expect_near(proj%sin_weight(1), 0.0_dp, 1.0e-12_dp, 'zero phases must have zero sine coefficient')
      call f90_expect_eq(f%weights%substndx(2), 1, 'missing cell must use the valid target as donor')
      call f90_expect_eq(f%weights%substndx(3), 0, 'outside targets must not acquire donors')
      call cleanup(f)
   end subroutine coefficient_donors

   !$gtest
   subroutine apply_known_angles() bind(C)
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
   end subroutine apply_known_angles

   !$gtest
   subroutine apply_operands() bind(C)
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
   end subroutine apply_operands

   !$gtest
   subroutine apply_undefined_targets() bind(C)
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
   end subroutine apply_undefined_targets

   !$gtest
   subroutine conversion_reuses_projection() bind(C)
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
            call cleanup(f)
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
            call cleanup(f)
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
      call cleanup(f)
   end subroutine conversion_reuses_projection

end module test_ec_harmonic_projection
