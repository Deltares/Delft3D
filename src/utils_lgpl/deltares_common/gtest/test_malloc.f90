module test_malloc
   use assertions_gtest
   use m_alloc
   use precision, only: sp, dp
   use iso_c_binding
   use precision_basics, only: equal
   implicit none(type, external)

contains

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_realloc_unallocated_with_fill, test_realloc_unallocated_with_fill,
   !> Realloc on unallocated array should allocate and apply fill
   subroutine test_realloc_unallocated_with_fill() bind(C)
      real(dp), allocatable :: arr(:)
      call realloc(arr, 5, fill=-999.0d0)
      call f90_expect_true(allocated(arr))
      call f90_expect_eq(size(arr), 5)
      call f90_expect_true(all(equal(arr, -999.0d0)))
   end subroutine test_realloc_unallocated_with_fill
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_realloc_grow_keeps_existing, test_realloc_grow_keeps_existing,
   !> Growing an array preserves existing data and fills new elements
   subroutine test_realloc_grow_keeps_existing() bind(C)
      real(dp), allocatable :: arr(:)
      call realloc(arr, 3, fill=0.0d0)
      arr = [1.0d0, 2.0d0, 3.0d0]
      call realloc(arr, 5, fill=-1.0d0, keepExisting=.true.)
      call f90_expect_eq(size(arr), 5)
      call f90_expect_true(equal(arr(1), 1.0d0) .and. equal(arr(2), 2.0d0) .and. equal(arr(3), 3.0d0))
      call f90_expect_true(equal(arr(4), -1.0d0) .and. equal(arr(5), -1.0d0))
   end subroutine test_realloc_grow_keeps_existing
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_realloc_shrink_keeps_existing, test_realloc_shrink_keeps_existing,
   !> Shrinking an array preserves data up to new size
   subroutine test_realloc_shrink_keeps_existing() bind(C)
      real(dp), allocatable :: arr(:)
      call realloc(arr, 5, fill=0.0d0)
      arr = [1.0d0, 2.0d0, 3.0d0]
      call realloc(arr, 2, keepExisting=.true.)
      call f90_expect_eq(size(arr), 2)
      call f90_expect_true(equal(arr(1), 1.0d0) .and. equal(arr(2), 2.0d0))
   end subroutine test_realloc_shrink_keeps_existing
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_realloc_same_bounds_no_keepexisting_fill, test_realloc_same_bounds_no_keepexisting_fill,
   !> Same bounds + keepExisting=.false. + fill should overwrite in-place without reallocation
   subroutine test_realloc_same_bounds_no_keepexisting_fill() bind(C)
      real(dp), allocatable, target :: arr(:)
      real(dp), pointer :: ptr_before
      call realloc(arr, 3, fill=1.0d0)
      ptr_before => arr(1)
      call realloc(arr, 3, fill=-999.0d0, keepExisting=.false.)
      call f90_expect_true(all(equal(arr, -999.0d0)))
      call f90_expect_true(associated(ptr_before, arr(1)))
   end subroutine test_realloc_same_bounds_no_keepexisting_fill
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_realloc_same_bounds_no_fill_unchanged, test_realloc_same_bounds_no_fill_unchanged,
   !> Same bounds and no fill should return early leaving array untouched
   subroutine test_realloc_same_bounds_no_fill_unchanged() bind(C)
      real(dp), allocatable, target :: arr(:)
      real(dp), pointer :: ptr_before
      call realloc(arr, 3, fill=1.0d0)
      arr(2) = 42.0d0
      ptr_before => arr(1)
      call realloc(arr, 3)
      call f90_expect_true(equal(arr(2), 42.0d0))
      call f90_expect_true(associated(ptr_before, arr(1)))
   end subroutine test_realloc_same_bounds_no_fill_unchanged
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_realloc_shift, test_realloc_shift,
   !> Shift moves existing data to new position in grown array
   subroutine test_realloc_shift() bind(C)
      real(dp), allocatable :: arr(:)
      call realloc(arr, 3, fill=0.0d0)
      arr = [10.0d0, 20.0d0, 30.0d0]
      call realloc(arr, 5, fill=-1.0d0, shift=2, keepExisting=.true.)
      call f90_expect_eq(size(arr), 5)
      call f90_expect_true(equal(arr(1), -1.0d0) .and. equal(arr(2), -1.0d0))
      call f90_expect_true(equal(arr(3), 10.0d0) .and. equal(arr(4), 20.0d0) .and. equal(arr(5), 30.0d0))
   end subroutine test_realloc_shift
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_realloc_nondefault_lindex, test_realloc_nondefault_lindex,
   !> Non-default lower index produces correct bounds
   subroutine test_realloc_nondefault_lindex() bind(C)
      real(dp), allocatable :: arr(:)
      call realloc(arr, 5, lindex=0, fill=1.0d0)
      call f90_expect_eq(lbound(arr, 1), 0)
      call f90_expect_eq(ubound(arr, 1), 5)
      call f90_expect_true(all(equal(arr, 1.0d0)))
   end subroutine test_realloc_nondefault_lindex
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_reallocp_unassociated, test_reallocp_unassociated,
   !> Pointer realloc on unassociated pointer should allocate without crash
   subroutine test_reallocp_unassociated() bind(C)
      real(dp), pointer :: arr(:)
      nullify (arr)
      call reallocp(arr, 5, fill=-999.0d0)
      call f90_expect_true(associated(arr))
      call f90_expect_eq(size(arr), 5)
      call f90_expect_true(all(equal(arr, -999.0d0)))
      deallocate (arr)
   end subroutine test_reallocp_unassociated
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_realloc_stat_success, test_realloc_stat_success,
   !> stat should be zero on successful allocation
   subroutine test_realloc_stat_success() bind(C)
      real(dp), allocatable :: arr(:)
      integer :: ierr
      call realloc(arr, 5, stat=ierr, fill=0.0d0)
      call f90_expect_eq(ierr, 0)
   end subroutine test_realloc_stat_success
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_reallocp_grow_keeps_existing, test_reallocp_grow_keeps_existing,
   !> Pointer realloc grow preserves existing data
   subroutine test_reallocp_grow_keeps_existing() bind(C)
      real(dp), pointer :: arr(:)
      nullify (arr)
      call reallocp(arr, 3, fill=0.0d0)
      arr = [1.0d0, 2.0d0, 3.0d0]
      call reallocp(arr, 5, fill=-1.0d0, keepExisting=.true.)
      call f90_expect_eq(size(arr), 5)
      call f90_expect_true(equal(arr(1), 1.0d0) .and. equal(arr(2), 2.0d0) .and. equal(arr(3), 3.0d0))
      call f90_expect_true(equal(arr(4), -1.0d0) .and. equal(arr(5), -1.0d0))
      deallocate (arr)
   end subroutine test_reallocp_grow_keeps_existing
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_realloc_lindex_preserved_on_grow, test_realloc_lindex_preserved_on_grow,
   !> Lower bound set on first alloc should be preserved when growing
   subroutine test_realloc_lindex_preserved_on_grow() bind(C)
      real(dp), allocatable :: arr(:)
      call realloc(arr, 5, lindex=0, fill=1.0d0)
      call realloc(arr, 8, lindex=0, fill=2.0d0, keepExisting=.true.)
      call f90_expect_eq(lbound(arr, 1), 0)
      call f90_expect_eq(ubound(arr, 1), 8)
      call f90_expect_true(equal(arr(0), 1.0d0))
      call f90_expect_true(equal(arr(6), 2.0d0))
   end subroutine test_realloc_lindex_preserved_on_grow
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_realloc_string_grow, test_realloc_string_grow,
   !> Reallocating a string to a larger length preserves existing content
   subroutine test_realloc_string_grow() bind(C)
      character(len=:), allocatable :: str
      call realloc(str, 5, fill='x')
      str(1:3) = 'abc'
      call realloc(str, 8, fill='y', keepExisting=.true.)
      call f90_expect_eq(len(str), 8)
      call f90_expect_true(str(1:3) == 'abc')
      call f90_expect_true(str(6:8) == 'yyy')
   end subroutine test_realloc_string_grow
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_realloc_zero_size, test_realloc_zero_size,
   !> Reallocating to size 0 should produce an allocated zero-size array
   subroutine test_realloc_zero_size() bind(C)
      real(dp), allocatable :: arr(:)
      call realloc(arr, 0, fill=1.0d0)
      call f90_expect_true(allocated(arr))
      call f90_expect_eq(size(arr), 0)
   end subroutine test_realloc_zero_size
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_realloc_negative_size, test_realloc_negative_size,
   !> Reallocating to negative size should produce an allocated zero-size array
   subroutine test_realloc_negative_size() bind(C)
      real(dp), allocatable :: arr(:)
      call realloc(arr, -1, fill=1.0d0)
      call f90_expect_true(allocated(arr))
      call f90_expect_eq(size(arr), 0)
   end subroutine test_realloc_negative_size
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_realloc_rank2_fixed_first_dim, test_realloc_rank2_fixed_first_dim,
   !> Rank 2: growing only second dimension preserves all data
   subroutine test_realloc_rank2_fixed_first_dim() bind(C)
      integer, allocatable :: arr(:, :)
      call realloc(arr, [3, 2], fill=0)
      arr(:, 1) = [1, 2, 3]
      arr(:, 2) = [4, 5, 6]
      call realloc(arr, [3, 4], fill=-1, keepExisting=.true.)
      call f90_expect_eq(size(arr, 1), 3)
      call f90_expect_eq(size(arr, 2), 4)
      call f90_expect_true(arr(1, 1) == 1 .and. arr(2, 1) == 2 .and. arr(3, 1) == 3)
      call f90_expect_true(arr(1, 2) == 4 .and. arr(2, 2) == 5 .and. arr(3, 2) == 6)
      call f90_expect_true(arr(1, 3) == -1 .and. arr(1, 4) == -1)
   end subroutine test_realloc_rank2_fixed_first_dim
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_realloc_rank2_grow_keeps_existing, test_realloc_rank2_grow_keeps_existing,
   !> Rank 2: growing both dimensions preserves existing data and fills new elements
   subroutine test_realloc_rank2_grow_keeps_existing() bind(C)
      real(dp), allocatable :: arr(:, :)
      call realloc(arr, [2, 2], fill=0.0d0)
      arr(:, 1) = [1, 2]
      arr(:, 2) = [3, 4]
      call realloc(arr, [3, 3], fill=-1.0d0, keepExisting=.true.)
      call f90_expect_eq(size(arr, 1), 3)
      call f90_expect_eq(size(arr, 2), 3)
      call f90_expect_true(equal(arr(1, 1), 1.0d0) .and. equal(arr(2, 1), 2.0d0))
      call f90_expect_true(equal(arr(1, 2), 3.0d0) .and. equal(arr(2, 2), 4.0d0))
      call f90_expect_true(equal(arr(3, 1), -1.0d0) .and. equal(arr(1, 3), -1.0d0))
   end subroutine test_realloc_rank2_grow_keeps_existing
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_realloc_rank2_nondefault_lindex, test_realloc_rank2_nondefault_lindex,
   !> Rank 2: non-default lower bounds produce correct bounds
   subroutine test_realloc_rank2_nondefault_lindex() bind(C)
      real(dp), allocatable :: arr(:, :)
      call realloc(arr, [3, 3], lindex=[0, 0], fill=1.0d0)
      call f90_expect_eq(lbound(arr, 1), 0)
      call f90_expect_eq(lbound(arr, 2), 0)
      call f90_expect_eq(ubound(arr, 1), 3)
      call f90_expect_eq(ubound(arr, 2), 3)
      call f90_expect_true(all(equal(arr, 1.0d0)))
   end subroutine test_realloc_rank2_nondefault_lindex
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_deltares_common_gtest, test_realloc_rank2_shift, test_realloc_rank2_shift,
   !> Rank 2: shift moves data along both dimensions independently
   subroutine test_realloc_rank2_shift() bind(C)
      real(dp), allocatable :: arr(:, :)
      call realloc(arr, [2, 2], fill=0.0d0)
      arr(:, 1) = [1, 2]
      arr(:, 2) = [3, 4]
      call realloc(arr, [3, 3], fill=-1.0d0, shift=[1, 1], keepExisting=.true.)
      call f90_expect_eq(size(arr, 1), 3)
      call f90_expect_eq(size(arr, 2), 3)
      call f90_expect_true(equal(arr(1, 1), -1.0d0)) ! shifted out of range
      call f90_expect_true(equal(arr(2, 2), 1.0d0))  ! original (1,1) -> (2,2)
      call f90_expect_true(equal(arr(3, 2), 2.0d0))  ! original (2,1) -> (3,2)
      call f90_expect_true(equal(arr(2, 3), 3.0d0))  ! original (1,2) -> (2,3)
      call f90_expect_true(equal(arr(3, 3), 4.0d0))  ! original (2,2) -> (3,3)
   end subroutine test_realloc_rank2_shift
   !$f90tw)

end module test_malloc
