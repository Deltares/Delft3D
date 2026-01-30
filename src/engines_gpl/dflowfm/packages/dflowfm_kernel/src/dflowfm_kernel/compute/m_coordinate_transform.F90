!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
!
!  Module: m_coordinate_transform
!  Purpose: Fast coordinate transformation from node/corner to link frames
!           Uses sparse matrix representation for vectorization
!
!  Usage in setumod:
!     use m_coordinate_transform
!
!     ! Once at initialization:
!     call initialize_coordinate_transform()
!
!     ! Every timestep:
!     call transform_velocities_to_links(ucx, ucy, ucnx, ucny)
!
!     ! Then use the results:
!     duxdn = (ucx_in_link_frame(2,L) - ucx_in_link_frame(1,L)) * dxi(L)
!
!------------------------------------------------------------------------------

module m_coordinate_transform
   use precision, only: dp
   implicit none(external)

   private

   ! Public interface - only these are visible to setumod
   public :: initialize_coordinate_transform
   public :: transform_velocities_to_links
   public :: ucx_in_link_frame
   public :: ucy_in_link_frame
   public :: ucnx_in_link_frame
   public :: ucny_in_link_frame
   public :: cleanup_coordinate_transform

   logical :: use_spherical_transform = .false.

   ! Cartesian-only: store node/corner indices for direct gather
   integer, allocatable :: link_k1(:), link_k2(:) ! Node indices for each 2D link
   integer, allocatable :: link_k3(:), link_k4(:) ! Corner indices for each 2D link

   ! Results - transformed velocities in link coordinate frame
   real(dp), allocatable :: ucx_in_link_frame(:, :) ! (2, lnx)
   real(dp), allocatable :: ucy_in_link_frame(:, :) ! (2, lnx)
   real(dp), allocatable :: ucnx_in_link_frame(:, :) ! (2, lnx)
   real(dp), allocatable :: ucny_in_link_frame(:, :) ! (2, lnx)

   ! Private implementation details (hidden from users)
   type :: sparse_transform_matrix
      integer :: num_rows
      integer :: num_cols
      integer :: num_nonzeros
      real(dp), allocatable :: values(:)
      integer, allocatable :: column_indices(:)
      integer, allocatable :: row_pointers(:)
   end type sparse_transform_matrix

   type(sparse_transform_matrix) :: node_transform
   type(sparse_transform_matrix) :: corner_transform
   logical :: is_initialized = .false.

contains

   !> Initialize transformation matrices (call once at model startup)
   subroutine initialize_coordinate_transform()
      use m_flowgeom, only: lnx, lnx1D, ndx, ln, lncn, csb, snb, csbn, snbn
      use network_data, only: numk
      use m_sferic, only: jsferic, jasfer3D
      use m_alloc, only: aerr

      implicit none

      integer :: ierr, L, num_2d_links

      if (is_initialized) then
         return
      end if

      ! Check if we actually need transformation (spherical coordinates only)
      use_spherical_transform = (jsferic == 1 .and. jasfer3D == 1)

      ! Allocate result arrays
      allocate (ucx_in_link_frame(2, lnx), stat=ierr)
      allocate (ucy_in_link_frame(2, lnx), stat=ierr)
      allocate (ucnx_in_link_frame(2, lnx), stat=ierr)
      allocate (ucny_in_link_frame(2, lnx), stat=ierr)

      if (.not. use_spherical_transform) then
         ! Cartesian: just store indices for direct gather
         num_2d_links = lnx - lnx1D
         allocate (link_k1(num_2d_links), link_k2(num_2d_links), stat=ierr)
         allocate (link_k3(num_2d_links), link_k4(num_2d_links), stat=ierr)

         do L = 1, num_2d_links
            link_k1(L) = ln(1, lnx1D + L)
            link_k2(L) = ln(2, lnx1D + L)
            link_k3(L) = lncn(1, lnx1D + L)
            link_k4(L) = lncn(2, lnx1D + L)
         end do
      else

         ! Build sparse transformation matrices for 2D links
         num_2d_links = lnx - lnx1D

         ! Build sparse matrices (single routine for both)
         call build_transform_matrix(node_transform, num_2d_links, ndx, &
                                     ln(1:2, lnx1D + 1:lnx), &
                                     csb(:, lnx1D + 1:lnx), snb(:, lnx1D + 1:lnx))

         call build_transform_matrix(corner_transform, num_2d_links, numk, &
                                     lncn(1:2, lnx1D + 1:lnx), &
                                     csbn(:, lnx1D + 1:lnx), snbn(:, lnx1D + 1:lnx))
      end if

      is_initialized = .true.

   end subroutine initialize_coordinate_transform

   !> Transform velocities from node/corner frames to link frames
   !! This is the main routine called every timestep in setumod
   subroutine transform_velocities_to_links(ucx, ucy, ucnx, ucny)
      use m_flowgeom, only: lnx, lnx1D

      real(dp), intent(in) :: ucx(:) ! Node x-velocities in global frame
      real(dp), intent(in) :: ucy(:) ! Node y-velocities in global frame
      real(dp), intent(in) :: ucnx(:) ! Corner x-velocities in global frame
      real(dp), intent(in) :: ucny(:) ! Corner y-velocities in global frame

      integer :: L

      if (.not. is_initialized) then
         return
         !todo : error handling
      end if
      if (use_spherical_transform) then
         ! Transform node velocities (2D links only)
         call apply_sparse_transform(node_transform, ucx, ucy, &
                                     ucx_in_link_frame(:, lnx1D + 1:lnx), &
                                     ucy_in_link_frame(:, lnx1D + 1:lnx))

         ! Transform corner velocities (2D links only)
         call apply_sparse_transform(corner_transform, ucnx, ucny, &
                                     ucnx_in_link_frame(:, lnx1D + 1:lnx), &
                                     ucny_in_link_frame(:, lnx1D + 1:lnx))

      else
         ! Cartesian: direct gather (no transformation needed)
         !$OMP PARALLEL DO PRIVATE(L)
         do L = 1, size(link_k1)
            ucx_in_link_frame(1, lnx1D + L) = ucx(link_k1(L))
            ucx_in_link_frame(2, lnx1D + L) = ucx(link_k2(L))
            ucy_in_link_frame(1, lnx1D + L) = ucy(link_k1(L))
            ucy_in_link_frame(2, lnx1D + L) = ucy(link_k2(L))

            ucnx_in_link_frame(1, lnx1D + L) = ucnx(link_k3(L))
            ucnx_in_link_frame(2, lnx1D + L) = ucnx(link_k4(L))
            ucny_in_link_frame(1, lnx1D + L) = ucny(link_k3(L))
            ucny_in_link_frame(2, lnx1D + L) = ucny(link_k4(L))
         end do
         !$OMP END PARALLEL DO
         return
      end if

   end subroutine transform_velocities_to_links

   !> Cleanup (call at program exit)
   subroutine cleanup_coordinate_transform()
      if (is_initialized) then
         deallocate (ucx_in_link_frame)
         deallocate (ucy_in_link_frame)
         deallocate (ucnx_in_link_frame)
         deallocate (ucny_in_link_frame)

         call destroy_sparse_matrix(node_transform)
         call destroy_sparse_matrix(corner_transform)

         is_initialized = .false.
      end if
   end subroutine cleanup_coordinate_transform

   !========================================================================
   ! Private implementation routines (users never see these)
   !========================================================================

   !> Build sparse matrix for coordinate transformation (works for both nodes and corners)
   !! Computes BOTH x and y components in one matrix-vector multiply
   subroutine build_transform_matrix(matrix, num_links, num_points, &
                                     link_point_indices, cs_coef, sn_coef)
      implicit none

      type(sparse_transform_matrix), intent(out) :: matrix
      integer, intent(in) :: num_links, num_points
      integer, intent(in) :: link_point_indices(2, num_links) ! Can be nodes or corners
      real(dp), intent(in) :: cs_coef(2, num_links) ! csb or csbn
      real(dp), intent(in) :: sn_coef(2, num_links) ! snb or snbn

      integer :: L, idx, pt1, pt2

      ! 4 rows per link: ucx(1), ucy(1), ucx(2), ucy(2)
      matrix%num_rows = 4 * num_links
      matrix%num_cols = 2 * num_points ! input vectors stacked [x; y]
      matrix%num_nonzeros = 8 * num_links ! 4 coefficients per side × 2 sides

      allocate (matrix%values(matrix%num_nonzeros))
      allocate (matrix%column_indices(matrix%num_nonzeros))
      allocate (matrix%row_pointers(matrix%num_rows + 1))

      idx = 1
      do L = 1, num_links
         pt1 = link_point_indices(1, L)
         pt2 = link_point_indices(2, L)

         ! Row 1: ucx_link(1,L) = cs(1,L)*input_x(pt1) + sn(1,L)*input_y(pt1)
         matrix%row_pointers(4 * L - 3) = idx
         matrix%values(idx) = cs_coef(1, L)
         matrix%column_indices(idx) = pt1
         idx = idx + 1
         matrix%values(idx) = sn_coef(1, L)
         matrix%column_indices(idx) = num_points + pt1 ! y offset
         idx = idx + 1

         ! Row 2: ucy_link(1,L) = -sn(1,L)*input_x(pt1) + cs(1,L)*input_y(pt1)
         matrix%row_pointers(4 * L - 2) = idx
         matrix%values(idx) = -sn_coef(1, L) ! Note the minus sign!
         matrix%column_indices(idx) = pt1
         idx = idx + 1
         matrix%values(idx) = cs_coef(1, L)
         matrix%column_indices(idx) = num_points + pt1
         idx = idx + 1

         ! Row 3: ucx_link(2,L) = cs(2,L)*input_x(pt2) + sn(2,L)*input_y(pt2)
         matrix%row_pointers(4 * L - 1) = idx
         matrix%values(idx) = cs_coef(2, L)
         matrix%column_indices(idx) = pt2
         idx = idx + 1
         matrix%values(idx) = sn_coef(2, L)
         matrix%column_indices(idx) = num_points + pt2
         idx = idx + 1

         ! Row 4: ucy_link(2,L) = -sn(2,L)*input_x(pt2) + cs(2,L)*input_y(pt2)
         matrix%row_pointers(4 * L) = idx
         matrix%values(idx) = -sn_coef(2, L) ! Note the minus sign!
         matrix%column_indices(idx) = pt2
         idx = idx + 1
         matrix%values(idx) = cs_coef(2, L)
         matrix%column_indices(idx) = num_points + pt2
         idx = idx + 1
      end do

      matrix%row_pointers(matrix%num_rows + 1) = idx

   end subroutine build_transform_matrix

   !> Apply sparse matrix transformation (this is where the speed happens)
   subroutine apply_sparse_transform(matrix, input_x, input_y, output_x, output_y)
      !use, intrinsic :: iso_c_binding
      type(sparse_transform_matrix), intent(in) :: matrix
      real(dp), intent(in) :: input_x(:), input_y(:)
      real(dp), intent(out) :: output_x(:, :), output_y(:, :)

      real(dp), allocatable :: input_vector(:), output_vector(:)
      integer :: row, num_links

      external :: mkl_dcsrgemv
      !
      !interface
      !   subroutine mkl_dcsrgemv(transa, m, a, ia, ja, x, y) bind(C, name='mkl_dcsrgemv')
      !      use, intrinsic :: iso_c_binding
      !      character(c_char), value :: transa
      !      integer(c_int), value :: m
      !      real(c_double) :: a(*)
      !      integer(c_int) :: ia(*)
      !      integer(c_int) :: ja(*)
      !      real(c_double) :: x(*)
      !      real(c_double) :: y(*)
      !   end subroutine mkl_dcsrgemv
      !end interface

      ! Stack input vectors: [input_x; input_y]
      if (.not. allocated(output_vector)) then
         allocate (output_vector(matrix%num_rows))
         allocate (input_vector(2 * size(input_x)))
      end if

      ! Copy data to heap-allocated array instead of using array constructor
      input_vector(1:size(input_x)) = input_x
      input_vector(size(input_x) + 1:2 * size(input_x)) = input_y

!#ifdef USE_MKL_SPARSE
      ! Use Intel MKL via C interface if available (10x faster)
      call mkl_dcsrgemv('N', matrix%num_rows, matrix%values, &
                        matrix%row_pointers, matrix%column_indices, &
                        input_vector, output_vector)
!#else
!      ! Fallback: manual CSR matrix-vector multiply (still fast!)
!      output_vector = 0.0_dp
!      !$OMP PARALLEL DO PRIVATE(row, col_start, col_end, idx)
!      do row = 1, matrix%num_rows
!         col_start = matrix%row_pointers(row)
!         col_end = matrix%row_pointers(row + 1) - 1
!
!         do idx = col_start, col_end
!            output_vector(row) = output_vector(row) + &
!                                 matrix%values(idx) * input_vector(matrix%column_indices(idx))
!         end do
!      end do
!      !$OMP END PARALLEL DO
!#endif

      ! Unstack output: output_vector → output_x, output_y
      ! Matrix structure: [ucx(1,L), ucy(1,L), ucx(2,L), ucy(2,L), ...]
      num_links = matrix%num_rows / 4 ! Changed from /2
      do row = 1, num_links
         output_x(1, row) = output_vector(4 * row - 3) ! ucx_link(1,L)
         output_y(1, row) = output_vector(4 * row - 2) ! ucy_link(1,L)
         output_x(2, row) = output_vector(4 * row - 1) ! ucx_link(2,L)
         output_y(2, row) = output_vector(4 * row) ! ucy_link(2,L)
      end do

      ! deallocate (input_vector, output_vector)

   end subroutine apply_sparse_transform

   subroutine destroy_sparse_matrix(matrix)
      type(sparse_transform_matrix), intent(inout) :: matrix
      if (allocated(matrix%values)) deallocate (matrix%values)
      if (allocated(matrix%column_indices)) deallocate (matrix%column_indices)
      if (allocated(matrix%row_pointers)) deallocate (matrix%row_pointers)
   end subroutine destroy_sparse_matrix

end module m_coordinate_transform
