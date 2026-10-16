module test_ec_harmonic_projection
   use precision, only: dp
   use assertions_gtest
   use m_ec_typedefs
   use m_ec_parameters
   use m_ec_converter, only: ecConverterPerformConversions
   use time_class, only: c_time
   use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan

   implicit none(type, external)
   private

   type fixture
      type(tEcConnection) :: connection
      type(tEcConverter) :: converter
      type(tEcItem) :: source_item, target_item
      type(tEcField) :: t0, t1, target_field
      type(tEcQuantity) :: quantity
      type(tEcElementSet) :: source_set, target_set
      type(tEcHarmonicsFrame) :: harmonics
      type(tEcTimeFrame) :: time_frame
      type(tEcIndexWeight) :: weights
      real(dp), dimension(:), allocatable :: values
   end type fixture

contains

   subroutine setup(f, layout)
      type(fixture), intent(inout), target :: f
      integer, intent(in) :: layout

      allocate (f%connection%sourceItemsPtr(1), f%connection%targetItemsPtr(1))
      f%connection%nSourceItems = 1
      f%connection%nTargetItems = 1
      f%connection%sourceItemsPtr(1)%ptr => f%source_item
      f%connection%targetItemsPtr(1)%ptr => f%target_item
      f%connection%converterPtr => f%converter
      f%converter%id = 1
      f%converter%ofType = convType_netcdf
      f%converter%operandType = EC_OPERAND_REPLACE
      f%converter%interpolationType = interpolate_spacetimeSaveWeightFactors
      f%converter%indexWeight => f%weights
      f%quantity%name = 'air_pressure'
      f%quantity%fillvalue = -999.0_dp
      f%source_item%id = 1
      f%target_item%id = 2
      f%source_item%sourceT0FieldPtr => f%t0
      f%source_item%sourceT1FieldPtr => f%t1
      f%source_item%quantityPtr => f%quantity
      f%source_item%elementSetPtr => f%source_set
      f%source_item%hframe => f%harmonics
      f%source_item%tframe => f%time_frame
      f%time_frame%ec_refdate = 59000.0_dp
      f%harmonics%ec_period = 86400
      f%target_item%targetFieldPtr => f%target_field
      f%target_item%elementSetPtr => f%target_set
      f%target_set%ofType = elmSetType_cartesian
      f%target_set%nCoordinates = 2
      allocate (f%values(2))
      f%target_field%arr1dPtr => f%values
      f%target_field%missingValue = ec_undef_hp
      f%values = 7.0_dp
      allocate (f%t0%arr1d(4), f%t1%arr1d(4))
      f%t0%arr1d = -123.0_dp
      f%t1%arr1d = [2.0_dp, 4.0_dp, 7.0_dp, 11.0_dp]
      f%t0%arr1dPtr => f%t0%arr1d
      f%t1%arr1dPtr => f%t1%arr1d
      f%t0%timesteps = -ec_huge_hp
      f%t1%timesteps = ec_huge_hp
      f%source_set%n_layers = 0
      f%source_set%ofType = elmSetType_cartesian
      f%source_set%n_cols = 2
      f%source_set%n_rows = 2
      f%t0%bbox = [1, 1, 2, 2]
      f%t1%bbox = f%t0%bbox
      if (layout == 2) then
         f%source_set%n_cols = 4
         f%source_set%n_rows = 3
         f%t0%issparse = 1
         f%t1%issparse = 1
         allocate (f%t0%ia(4), f%t0%ja(4))
         f%t0%ia = [1, 1, 3, 5]
         f%t0%ja = [2, 3, 2, 3]
         f%t1%ia => f%t0%ia
         f%t1%ja => f%t0%ja
         allocate (f%harmonics%phases(4, 3))
         f%harmonics%phases = -999.0_dp
         f%harmonics%phases(2:3, 2) = [170.0_dp, -175.0_dp]
         f%harmonics%phases(2:3, 3) = [40.0_dp, 95.0_dp]
      else if (layout == 3) then
         f%source_set%ofType = elmSetType_samples
         f%source_set%n_cols = 4
         f%source_set%n_rows = 1
         allocate (f%harmonics%phases(4, 1))
         f%harmonics%phases(:, 1) = [170.0_dp, -175.0_dp, 40.0_dp, 95.0_dp]
      else
         allocate (f%harmonics%phases(2, 2))
         f%harmonics%phases(:, 1) = [170.0_dp, -175.0_dp]
         f%harmonics%phases(:, 2) = [40.0_dp, 95.0_dp]
      end if
      if (layout == 3) then
         allocate (f%weights%indices(3, 2), f%weights%weightFactors(3, 2))
         f%weights%indices(:, 1) = [1, 3, 4]
         f%weights%weightFactors(:, 1) = [0.2_dp, 0.3_dp, 0.5_dp]
      else
         allocate (f%weights%indices(2, 2), f%weights%weightFactors(4, 2))
         f%weights%indices(:, 1) = [1, 1]
         if (layout == 2) then
            f%weights%indices(:, 1) = [1, 3]
         end if
         f%weights%weightFactors(:, 1) = [0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp]
      end if
      f%weights%indices(:, 2) = 0
      f%weights%weightFactors(:, 2) = 0.0_dp
   end subroutine setup

   subroutine cleanup(f)
      type(fixture), intent(inout) :: f
      deallocate (f%connection%sourceItemsPtr, f%connection%targetItemsPtr)
      deallocate (f%weights%indices, f%weights%weightFactors)
      if (f%t0%issparse == 1) then
         deallocate (f%t0%ia, f%t0%ja)
      end if
   end subroutine cleanup

   function reference_value(f, days) result(value)
      type(fixture), intent(in) :: f
      real(dp), intent(in) :: days
      real(dp) :: value, pi
      real(dp), dimension(4) :: phases, amplitudes

      pi = 4.0_dp * atan(1.0_dp)
      phases = [170.0_dp, -175.0_dp, 40.0_dp, 95.0_dp] * pi / 180.0_dp
      amplitudes = f%t1%arr1d
      if (f%source_set%ofType == elmSetType_samples) then
         value = sum(f%weights%weightFactors(:, 1) * &
                     amplitudes([1, 3, 4]) * cos(2.0_dp * pi * days - phases([1, 3, 4])))
      else
         value = sum(f%weights%weightFactors(:, 1) * &
                     amplitudes([1, 2, 4, 3]) * cos(2.0_dp * pi * days - phases([1, 2, 4, 3])))
      end if
   end function reference_value

   !$f90tw TESTCODE(TEST, test_ec_harmonic_projection, fixed_spatial_projections, fixed_spatial_projections,
   subroutine fixed_spatial_projections() bind(C)
      type(fixture), target :: f
      type(c_time) :: time
      real(dp), dimension(6), parameter :: DAYS = [-0.25_dp, 0.0_dp, 0.125_dp, 0.5_dp, 1.25_dp, 3650.125_dp]
      integer :: layout, step
      logical :: success

      do layout = 1, 3
         block
            type(fixture), target :: current
            call setup(current, layout)
            do step = 1, size(DAYS)
               call time%set(59000.0_dp + DAYS(step))
               success = ecConverterPerformConversions(current%connection, time)
               call f90_assert_true(success, 'harmonic projection should succeed')
               call f90_expect_near(current%values(1), reference_value(current, DAYS(step)), 1.0e-9_dp, &
                                    'cached coefficients must match evaluate-then-interpolate')
               call f90_expect_true(allocated(current%harmonics%projections), 'harmonics frame must own the projection')
               call f90_expect_true(all(current%t0%arr1d == -123.0_dp), 'updates must not reconstruct source fields')
               call f90_expect_near(current%target_field%timesteps, time%mjd(), 1.0e-12_dp, 'target time must update')
               if (layout /= 3) then
                  call f90_expect_near(current%values(2), 7.0_dp, 1.0e-12_dp, 'outside targets must be untouched')
               end if
            end do
            call cleanup(current)
         end block
      end do
      call setup(f, 3)
      f%converter%interpolationType = interpolate_nearest_neighbour
      f%weights%indices(:, 1) = [4, 0, 0]
      f%weights%weightFactors(:, 1) = [1.0_dp, 0.0_dp, 0.0_dp]
      call time%set(59000.0_dp)
      success = ecConverterPerformConversions(f%connection, time)
      call f90_assert_true(success, 'nearest-neighbour harmonic projection should succeed')
      call f90_expect_near(f%values(1), 11.0_dp * cos(95.0_dp * acos(-1.0_dp) / 180.0_dp), &
                           1.0e-12_dp, 'nearest-neighbour must select the requested station')
      call cleanup(f)
   end subroutine fixed_spatial_projections
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ec_harmonic_projection, operands_and_undefined_targets, operands_and_undefined_targets,
   subroutine operands_and_undefined_targets() bind(C)
      integer, dimension(7), parameter :: OPERANDS = [EC_OPERAND_REPLACE, EC_OPERAND_ADD, EC_OPERAND_MULTIPLY, &
                                                      EC_OPERAND_MINIMUM, EC_OPERAND_MAXIMUM, EC_OPERAND_REPLACE_IF_MISSING, EC_OPERAND_ADD]
      type(c_time) :: time
      real(dp) :: provided, expected
      integer :: i
      logical :: success

      call time%set(59000.125_dp)
      do i = 1, size(OPERANDS)
         block
            type(fixture), target :: f
            call setup(f, 2)
            f%converter%operandType = OPERANDS(i)
            f%values(2) = ec_undef_hp
            provided = reference_value(f, 0.125_dp)
            select case (i)
            case (1)
               expected = provided
            case (2)
               expected = 7.0_dp + provided
            case (3)
               expected = 7.0_dp * provided
            case (4)
               expected = min(7.0_dp, provided)
            case (5)
               expected = max(7.0_dp, provided)
            case (6)
               expected = 7.0_dp
            case (7)
               f%values(1) = ec_undef_hp
               expected = ec_undef_hp
            end select
            success = ecConverterPerformConversions(f%connection, time)
            call f90_expect_true(success .eqv. (i /= 7), 'only undefined active targets must fail additive operands')
            call f90_expect_near(f%values(1), expected, 1.0e-12_dp, 'operand semantics must be preserved')
            call f90_expect_near(f%values(2), ec_undef_hp, 1.0e-12_dp, 'inactive targets must remain untouched')
            if (i == 6) then
               f%values(1) = ec_undef_hp
               success = ecConverterPerformConversions(f%connection, time)
               call f90_expect_true(success, 'replace-if-missing should succeed with cached coefficients')
               call f90_expect_near(f%values(1), provided, 1.0e-12_dp, 'undefined target must be replaced')
            end if
            call cleanup(f)
         end block
      end do
   end subroutine operands_and_undefined_targets
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ec_harmonic_projection, missing_amplitudes_and_phases, missing_amplitudes_and_phases,
   subroutine missing_amplitudes_and_phases() bind(C)
      type(c_time) :: time
      integer :: layout, missing_kind
      logical :: success

      call time%set(59000.0_dp)
      do layout = 1, 3
         do missing_kind = 1, 4
            block
               type(fixture), target :: f
               real(dp) :: missing_value
               call setup(f, layout)
               missing_value = -999.0_dp
               if (missing_kind > 2) then
                  missing_value = ieee_value(0.0_dp, ieee_quiet_nan)
                  f%quantity%fillvalue = missing_value
               end if
               if (mod(missing_kind, 2) == 1) then
                  f%t1%arr1d(1) = missing_value
               else if (layout == 2) then
                  f%harmonics%phases(2, 2) = missing_value
               else
                  f%harmonics%phases(1, 1) = missing_value
               end if
               success = ecConverterPerformConversions(f%connection, time)
               call f90_expect_true(success .eqv. (layout /= 3), 'station missings must fail; grid missings must be skipped')
               call f90_expect_near(f%values(1), 7.0_dp, 1.0e-12_dp, 'missing harmonic source must not change target')
               call cleanup(f)
            end block
         end do
      end do
   end subroutine missing_amplitudes_and_phases
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ec_harmonic_projection, extrapolation_copies_final_values, extrapolation_copies_final_values,
   subroutine extrapolation_copies_final_values() bind(C)
      type(fixture), target :: f
      type(c_time) :: time
      integer :: step
      logical :: success

      call setup(f, 1)
      f%converter%interpolationType = extrapolate_spacetimeSaveWeightFactors
      f%converter%operandType = EC_OPERAND_ADD
      f%source_set%n_cols = 3
      f%t0%bbox = [1, 1, 3, 2]
      f%t1%bbox = f%t0%bbox
      deallocate (f%t0%arr1d, f%t1%arr1d, f%harmonics%phases)
      allocate (f%t0%arr1d(6), f%t1%arr1d(6), f%harmonics%phases(3, 2))
      f%t0%arr1dPtr => f%t0%arr1d
      f%t1%arr1dPtr => f%t1%arr1d
      f%t0%arr1d = -123.0_dp
      f%t1%arr1d = [2.0_dp, 2.0_dp, -999.0_dp, 2.0_dp, 2.0_dp, -999.0_dp]
      f%harmonics%phases = 0.0_dp
      deallocate (f%values, f%weights%indices, f%weights%weightFactors)
      f%target_set%nCoordinates = 3
      allocate (f%values(3), f%weights%indices(2, 3), f%weights%weightFactors(4, 3))
      f%target_field%arr1dPtr => f%values
      f%weights%indices(:, 1) = [1, 1]
      f%weights%indices(:, 2) = [1, 2]
      f%weights%indices(:, 3) = 0
      f%weights%weightFactors(:, 1) = [0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp]
      f%weights%weightFactors(:, 2) = f%weights%weightFactors(:, 1)
      f%weights%weightFactors(:, 3) = 0.0_dp
      allocate (f%weights%substndx(3), f%target_set%x(3), f%target_set%y(3))
      f%weights%substndx = 0
      f%weights%substndx(3) = 1
      f%target_set%x = [0.5_dp, 1.5_dp, 100.0_dp]
      f%target_set%y = 0.5_dp
      do step = 0, 1
         f%values = [10.0_dp, 20.0_dp, 30.0_dp]
         call time%set(59000.0_dp + 0.5_dp * real(step, dp))
         success = ecConverterPerformConversions(f%connection, time)
         call f90_assert_true(success, 'harmonic extrapolation should succeed')
         call f90_expect_near(f%values(1), 10.0_dp + 2.0_dp * (-1.0_dp)**step, 1.0e-12_dp, 'donor must receive operand')
         call f90_expect_near(f%values(2), f%values(1), 1.0e-12_dp, 'extrapolation must copy final donor, not add to recipient')
         call f90_expect_near(f%values(3), 30.0_dp, 1.0e-12_dp, 'outside targets must not copy an existing donor index')
         call f90_expect_true(f%converter%extrapolated, 'extrapolation status must be retained')
         call f90_expect_eq(f%weights%substndx(2), 1, 'donor index must be cached')
      end do
      deallocate (f%weights%substndx, f%target_set%x, f%target_set%y)
      call cleanup(f)
   end subroutine extrapolation_copies_final_values
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ec_harmonic_projection, independent_source_target_pairs, independent_source_target_pairs,
   subroutine independent_source_target_pairs() bind(C)
      type(fixture), target :: first, second
      type(c_time) :: time
      logical :: success
      integer :: step

      call setup(first, 2)
      call setup(second, 2)
      second%t1%arr1d = second%t1%arr1d * 3.0_dp
      second%harmonics%ec_period = 43200
      deallocate (first%connection%sourceItemsPtr, first%connection%targetItemsPtr)
      allocate (first%connection%sourceItemsPtr(2), first%connection%targetItemsPtr(2))
      first%connection%nSourceItems = 2
      first%connection%nTargetItems = 2
      first%connection%sourceItemsPtr(1)%ptr => first%source_item
      first%connection%sourceItemsPtr(2)%ptr => second%source_item
      first%connection%targetItemsPtr(1)%ptr => first%target_item
      first%connection%targetItemsPtr(2)%ptr => second%target_item
      do step = 1, 2
         call time%set(59000.0_dp + 0.125_dp * real(step, dp))
         success = ecConverterPerformConversions(first%connection, time)
         call f90_assert_true(success, 'multiple harmonic pairs should succeed')
         call f90_expect_near(first%values(1), reference_value(first, 0.125_dp * real(step, dp)), &
                              1.0e-12_dp, 'first component must retain its coefficients and period')
         call f90_expect_near(second%values(1), reference_value(second, 0.25_dp * real(step, dp)), &
                              1.0e-12_dp, 'second component must have independent coefficients and period')
      end do
      call cleanup(first)
      call cleanup(second)
   end subroutine independent_source_target_pairs
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ec_harmonic_projection, shared_frame_keeps_projections_separate, shared_frame_keeps_projections_separate,
   subroutine shared_frame_keeps_projections_separate() bind(C)
      type(fixture), target :: first, second, third
      type(c_time) :: time
      logical :: success
      integer :: step

      call setup(first, 2)
      call setup(second, 2)
      call setup(third, 2)
      second%source_item%hframe => first%harmonics
      second%source_item%id = 3
      second%target_item%id = 4
      second%converter%id = 2
      second%t1%arr1d = 3.0_dp * second%t1%arr1d
      third%connection%sourceItemsPtr(1)%ptr => first%source_item
      third%target_item%id = 5
      third%converter%id = 3
      third%weights%weightFactors(:, 1) = [0.4_dp, 0.3_dp, 0.2_dp, 0.1_dp]

      do step = 1, 2
         call time%set(59000.0_dp + 0.125_dp * real(step, dp))
         success = ecConverterPerformConversions(first%connection, time)
         call f90_assert_true(success, 'first projection sharing a frame should succeed')
         success = ecConverterPerformConversions(second%connection, time)
         call f90_assert_true(success, 'another quantity sharing the phase frame should succeed')
         success = ecConverterPerformConversions(third%connection, time)
         call f90_assert_true(success, 'another target mapping of the same source should succeed')
         call f90_expect_near(first%values(1), reference_value(first, 0.125_dp * real(step, dp)), &
                              1.0e-12_dp, 'adding projections must preserve the first cached mapping')
         call f90_expect_near(second%values(1), reference_value(second, 0.125_dp * real(step, dp)), &
                              1.0e-12_dp, 'shared phases must not mix amplitudes from different quantities')
         call f90_expect_near(third%values(1), reference_value(third, 0.125_dp * real(step, dp)), &
                              1.0e-12_dp, 'shared sources must retain separate projected target mappings')
         call f90_expect_eq(size(first%harmonics%projections), 3, 'frame should retain exactly three projections')
      end do
      call cleanup(first)
      call cleanup(second)
      call cleanup(third)
   end subroutine shared_frame_keeps_projections_separate
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_ec_harmonic_projection, cached_and_uncached_conversions_agree, cached_and_uncached_conversions_agree,
   subroutine cached_and_uncached_conversions_agree() bind(C)
      type(fixture), target :: f
      type(c_time) :: time
      real(dp) :: cached
      integer :: layout, step
      logical :: success

      do layout = 1, 2
         block
            type(fixture), target :: current
            call setup(current, layout)
            do step = 0, 8
               call time%set(59000.0_dp + 0.125_dp * real(step, dp))
               current%source_set%n_layers = 0
               success = ecConverterPerformConversions(current%connection, time)
               call f90_assert_true(success, 'cached conversion should succeed')
               cached = current%values(1)
               ! A single source layer without target vertical coordinates uses the original 2D conversion.
               current%source_set%n_layers = 1
               success = ecConverterPerformConversions(current%connection, time)
               call f90_assert_true(success, 'original 2D conversion should succeed')
               call f90_expect_near(current%values(1), cached, 1.0e-12_dp, 'cached and original converter paths must agree')
            end do
            call cleanup(current)
         end block
      end do
      call setup(f, 1)
      f%harmonics%ec_period = 0
      call time%set(59000.0_dp)
      success = ecConverterPerformConversions(f%connection, time)
      call f90_expect_false(success, 'zero periods must fail explicitly')
      call cleanup(f)
   end subroutine cached_and_uncached_conversions_agree
   !$f90tw)

end module test_ec_harmonic_projection
