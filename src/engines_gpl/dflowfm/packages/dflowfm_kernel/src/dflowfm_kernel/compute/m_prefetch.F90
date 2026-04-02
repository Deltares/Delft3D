!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
!
!  Module: m_prefetch
!  Purpose: Store velocities in link-local coordinates for perfect vectorization
!           Uses pre-flattened index maps for vectorizable gather operations
!
!------------------------------------------------------------------------------

module m_prefetch
   use precision, only: dp

   implicit none(type, external)

   private

   ! Public interface
   public :: allocate_prefetch_arrays
   public :: prefetch_node_velocities, prefetch_corner_velocities
   public :: cleanup_prefetch_arrays

   ! Pre-flattened index maps (enables vectorizable gather: ucx_1 = ucx(node_map_1))
   integer, allocatable, dimension(:) :: node_map_1 !< ln(1, lnx1D+1:lnx) flattened
   integer, allocatable, dimension(:) :: node_map_2 !< ln(2, lnx1D+1:lnx) flattened
   integer, allocatable, dimension(:) :: corner_map_1 !< lncn(1, lnx1D+1:lnx) flattened
   integer, allocatable, dimension(:) :: corner_map_2 !< lncn(2, lnx1D+1:lnx) flattened

   ! pre-fetched, link-based velocity arrays. ucx_1(L) = ucx(node_map_1(L)) = ucx(ln(1, L)). Allows vectorized, contiguous memory access in link-based loops. Similarly for corner velocities.
   ! these arrays are prefetched at every time step in prefetch_node_velocities
   real(kind=dp), allocatable, dimension(:), public :: ucx_1
   real(kind=dp), allocatable, dimension(:), public :: ucy_1
   real(kind=dp), allocatable, dimension(:), public :: ucx_2
   real(kind=dp), allocatable, dimension(:), public :: ucy_2
   real(kind=dp), allocatable, dimension(:), public :: ucxq_1
   real(kind=dp), allocatable, dimension(:), public :: ucyq_1
   real(kind=dp), allocatable, dimension(:), public :: ucxq_2
   real(kind=dp), allocatable, dimension(:), public :: ucyq_2
   !> these arrays are prefetched only at model initialization
   real(kind=dp), allocatable, dimension(:), public :: bai_1
   real(kind=dp), allocatable, dimension(:), public :: bai_2
   real(kind=dp), allocatable, dimension(:), public :: csb_1
   real(kind=dp), allocatable, dimension(:), public :: csb_2
   real(kind=dp), allocatable, dimension(:), public :: snb_1
   real(kind=dp), allocatable, dimension(:), public :: snb_2
   real(kind=dp), allocatable, dimension(:), public :: csbn_1
   real(kind=dp), allocatable, dimension(:), public :: csbn_2
   real(kind=dp), allocatable, dimension(:), public :: snbn_1
   real(kind=dp), allocatable, dimension(:), public :: snbn_2
   ! pre-fetched, link-based corner velocity arrays. uxcorner_1 (L) = ucnx(corner_map_1(L)) = ucnx(lncn(1, L)).
   ! these arrays are prefetched at every time step in prefetch_corner_velocities
   real(kind=dp), allocatable, dimension(:), public :: uxcorner_1
   real(kind=dp), allocatable, dimension(:), public :: uycorner_1
   real(kind=dp), allocatable, dimension(:), public :: uxcorner_2
   real(kind=dp), allocatable, dimension(:), public :: uycorner_2

   logical :: is_initialized = .false.
   integer :: lnx = 0

contains

   !> allocate all prefetch arrays, called once during model initialization.
   subroutine allocate_prefetch_arrays()
      use m_flowgeom, only: lnx, ln, lncn
      use m_sferic, only: jsferic, jasfer3D
      use m_flowgeom, only: csb, snb, csbn, snbn, bai
      use m_flow, only: ndkx

      integer :: ierr, L

      if (is_initialized) return

      ! Allocate pre-flattened index maps
      allocate (node_map_1(ndkx), node_map_2(ndkx))
      allocate (corner_map_1(ndkx), corner_map_2(ndkx))

      ! Allocate temporary gather buffers
      allocate (ucx_1(lnx), ucy_1(lnx), stat=ierr)
      allocate (ucx_2(lnx), ucy_2(lnx), stat=ierr)
      allocate (ucxq_1(lnx), ucyq_1(lnx), stat=ierr)
      allocate (ucxq_2(lnx), ucyq_2(lnx), stat=ierr)
      allocate (uxcorner_1(lnx), uycorner_1(lnx))
      allocate (uxcorner_2(lnx), uycorner_2(lnx))
      allocate (bai_1(lnx), bai_2(lnx))

      ! unfortunately, these prefetch of the sines is necessary despite csb etc already being link-based, but since Fortran is column-major,
      ! csb(1, L) is not contiguous in memory and thus not vectorizable
      if (jsferic == 1 .and. jasfer3D == 1) then
         allocate (csb_1(lnx), csb_2(lnx), snb_1(lnx), snb_2(lnx))
         allocate (csbn_1(lnx), csbn_2(lnx), snbn_1(lnx), snbn_2(lnx))
         do concurrent (L = 1:lnx)
            node_map_1(L)   = ln(1, L)
            node_map_2(L)   = ln(2, L)
            corner_map_1(L) = lncn(1, L)
            corner_map_2(L) = lncn(2, L)
            bai_1(L)        = bai(node_map_1(L))
            bai_2(L)        = bai(node_map_2(L))
            csb_1(L)        = csb(1, L)
            csb_2(L)        = csb(2, L)
            snb_1(L)        = snb(1, L)
            snb_2(L)        = snb(2, L)
            csbn_1(L)       = csbn(1, L)
            csbn_2(L)       = csbn(2, L)
            snbn_1(L)       = snbn(1, L)
            snbn_2(L)       = snbn(2, L)
         end do
      end if
      ! Build flattened index maps (one-time cost at initialization)

      do concurrent (L = 1:lnx)
         corner_map_1(L) = lncn(1, L)
         corner_map_2(L) = lncn(2, L)
         node_map_1(L) = ln(1, L)
         node_map_2(L) = ln(2, L)
         bai_1(L) = bai(node_map_1(L))
         bai_2(L) = bai(node_map_2(L))
      end do

      is_initialized = .true.

   end subroutine allocate_prefetch_arrays

   !> prefetch ucx, ucy, ucxq and ucyq into link-based arrays for contiguous access in link-based loops.
   subroutine prefetch_node_velocities(ucx, ucy, ucxq, ucyq)
      use m_flowgeom, only: lnx, lnx1D

      real(dp), intent(in), dimension(:), contiguous :: ucx !< ucx from m_flow
      real(dp), intent(in), dimension(:), contiguous :: ucy !< ucy from m_flow
      real(dp), intent(in), dimension(:), contiguous :: ucxq !< ucxq from m_flow
      real(dp), intent(in), dimension(:), contiguous :: ucyq !< ucyq from m_flow

      integer :: L1, L2, L

      L1 = lnx1D + 1
      L2 = lnx

      if (.not. is_initialized) return

      do concurrent (L = L1:L2)
         ucx_1(L) = ucx(node_map_1(L))
         ucy_1(L) = ucy(node_map_1(L))
         ucx_2(L) = ucx(node_map_2(L))
         ucy_2(L) = ucy(node_map_2(L))

         ucxq_1(L) = ucxq(node_map_1(L))
         ucyq_1(L) = ucyq(node_map_1(L))
         ucxq_2(L) = ucxq(node_map_2(L))
         ucyq_2(L) = ucyq(node_map_2(L))
      end do

   end subroutine prefetch_node_velocities

   !> prefetch corner velocities (ucnx/ucny) into link-based arrays for contiguous access in link-based loops.
   subroutine prefetch_corner_velocities(ucnx, ucny)
      use m_flowgeom, only: lnx1D, lnx

      real(dp), intent(in), dimension(:), contiguous :: ucnx !< ucnx from m_flow
      real(dp), intent(in), dimension(:), contiguous :: ucny !< ucny from m_flow

      integer :: L1, L2, L

      L1 = lnx1D + 1
      L2 = lnx

      if (.not. is_initialized) return

      do L = L1, L2
         uxcorner_1(L) = ucnx(corner_map_1(L))
         uycorner_1(L) = ucny(corner_map_1(L))
         uxcorner_2(L) = ucnx(corner_map_2(L))
         uycorner_2(L) = ucny(corner_map_2(L))
      end do

   end subroutine prefetch_corner_velocities

   !> deallocate all prefetch arrays, mostly for unit testability
   subroutine cleanup_prefetch_arrays()

      if (is_initialized) then
         deallocate (node_map_1, node_map_2, corner_map_1, corner_map_2)
         deallocate (ucx_1, ucy_1, ucx_2, ucy_2, ucxq_1, ucyq_1, ucxq_2, ucyq_2)
         deallocate (uxcorner_1, uycorner_1)
         deallocate (uxcorner_2, uycorner_2)
         deallocate (bai_1, bai_2)
         is_initialized = .false.
      end if
   end subroutine cleanup_prefetch_arrays

end module m_prefetch
