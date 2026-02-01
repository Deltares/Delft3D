!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
!
!  Module: m_coordinate_transform
!  Purpose: Store velocities in link-local coordinates for perfect vectorization
!           Uses pre-flattened index maps for vectorizable gather operations
!
!------------------------------------------------------------------------------

module m_coordinate_transform
   use precision, only: dp

   implicit none(external)

   private

! node related, dim = ndkx
   !real(kind=dp), allocatable, public :: ucx_link_1(:) !< pre-transformed ucx for link side 1 (stress/viscosity optimization)
   !real(kind=dp), allocatable, public :: ucy_link_1(:) !< pre-transformed ucy for link side 1 (stress/viscosity optimization)
   !real(kind=dp), allocatable, public :: ucx_link_2(:) !< pre-transformed ucx for link side 2 (stress/viscosity optimization)
   !real(kind=dp), allocatable, public :: ucy_link_2(:) !< pre-transformed ucy for link side 2 (stress/viscosity optimization)
   !real(kind=dp), allocatable, public :: ucxq_link_1(:) !< pre-transformed ucxq for link side 1 (Coriolis optimization)
   !real(kind=dp), allocatable, public :: ucyq_link_1(:) !< pre-transformed ucyq for link side 1 (Coriolis optimization)
   !real(kind=dp), allocatable, public :: ucxq_link_2(:) !< pre-transformed ucxq for link side 2 (Coriolis optimization)
   !real(kind=dp), allocatable, public :: ucyq_link_2(:) !< pre-transformed ucyq for link side 2 (Coriolis optimization)
   !real(kind=dp), allocatable, public :: ucnx_link_1(:), ucnx_link_2(:) ! Corner x-velocity in link frame (2, lnx)
   !real(kind=dp), allocatable, public :: ucny_link_1(:), ucny_link_2(:) ! Corner y-velocity in link frame (2, lnx)

   real(kind=dp), allocatable, public :: csb_1(:), csb_2(:) !< cosine of link angle at node 1/2
   real(kind=dp), allocatable, public :: snb_1(:), snb_2(:) !< sine of link angle at node 1/2
   real(kind=dp), allocatable, public :: csbn_1(:), csbn_2(:) !< cosine of link angle at corner 1/2
   real(kind=dp), allocatable, public :: snbn_1(:), snbn_2(:) !< sine of link angle at corner 1/2

   real(kind=dp), allocatable :: ucx_global(:) !< transformed ucx back to global frame
   ! Public interface
   public :: initialize_coordinate_transform
   public :: transform_node_velocities_combined, transform_corner_velocities
   public :: cleanup_coordinate_transform

   ! Pre-flattened index maps (enables vectorizable gather: ux1 = ucx(node_map_1))
   integer, allocatable :: node_map_1(:), node_map_2(:) ! ln(1:2, lnx1D+1:lnx) flattened
   integer, allocatable :: corner_map_1(:), corner_map_2(:) ! lncn(1:2, lnx1D+1:lnx) flattened

   ! Temporary gather buffers (flat arrays for vectorization)
   real(dp), allocatable, public :: ux1(:), uy1(:), ux2(:), uy2(:) ! Node velocities
   real(dp), allocatable, public :: ux3(:), uy3(:), ux4(:), uy4(:) ! Corner velocities

   logical :: use_spherical_transform = .false.
   logical :: is_initialized = .false.
   integer :: lnx = 0

contains

   subroutine initialize_coordinate_transform()
      use m_flowgeom, only: lnx, ln, lncn
      use m_sferic, only: jsferic, jasfer3D
      use m_flowgeom, only: csb, snb, csbn, snbn
      implicit none
      integer :: ierr

      if (is_initialized) return

      use_spherical_transform = (jsferic == 1 .and. jasfer3D == 1)

      ! Allocate link-indexed velocity arrays
      !allocate (ucx_link_1(lnx), ucy_link_1(lnx))
      !allocate (ucx_link_2(lnx), ucy_link_2(lnx))
      !allocate (ucnx_link_1(lnx), ucny_link_1(lnx))
      !allocate (ucnx_link_2(lnx), ucny_link_2(lnx))
      !allocate (ucxq_link_1(lnx), ucxq_link_2(lnx))
      !allocate (ucyq_link_1(lnx), ucyq_link_2(lnx))

      ! Allocate pre-flattened index maps
      allocate (node_map_1(lnx), node_map_2(lnx), stat=ierr)
      allocate (corner_map_1(lnx), corner_map_2(lnx), stat=ierr)

      ! Allocate temporary gather buffers
      allocate (ux1(lnx), uy1(lnx), stat=ierr)
      allocate (ux2(lnx), uy2(lnx), stat=ierr)
      allocate (ux3(lnx), uy3(lnx), stat=ierr)
      allocate (ux4(lnx), uy4(lnx), stat=ierr)

      csb_1 = csb(1, :)
      csb_2 = csb(2, :)
      snb_1 = snb(1, :)
      snb_2 = snb(2, :)
      csbn_1 = csbn(1, :)
      csbn_2 = csbn(2, :)
      snbn_1 = snbn(1, :)
      snbn_2 = snbn(2, :)

      ! Build flattened index maps (one-time cost at initialization)
      node_map_1 = ln(1, 1:lnx)
      node_map_2 = ln(2, 1:lnx)
      corner_map_1 = lncn(1, 1:lnx)
      corner_map_2 = lncn(2, 1:lnx)

      is_initialized = .true.

   end subroutine initialize_coordinate_transform

   subroutine transform_node_velocities_combined(ucx, ucy, ucxq, ucyq)
      use m_flowgeom, only: lnx, lnx1D!, csu, snu

      real(dp), intent(in), contiguous :: ucx(:), ucy(:), ucxq(:), ucyq(:)
      integer :: L1, L2, L

      L1 = lnx1D + 1
      L2 = lnx

      if (.not. is_initialized) return

      do L = L1, L2
         ux1(L) = ucx(node_map_1(L))
         uy1(L) = ucy(node_map_1(L))
         ux2(L) = ucx(node_map_2(L))
         uy2(L) = ucy(node_map_2(L))

         ux3(L) = ucxq(node_map_1(L))
         uy3(L) = ucyq(node_map_1(L))
         ux4(L) = ucxq(node_map_2(L))
         uy4(L) = ucyq(node_map_2(L))
      end do

      !if (use_spherical_transform) then
      !   !$OMP SIMD
      !   do L = L1, L2
      !      ! Node 1: Rotate BOTH velocity types with same coefficients
      !      ucx_link_1(L) = csb_1(L) * ux1(L) + snb_1(L) * uy1(L)
      !      ucy_link_1(L) = -snb_1(L) * ux1(L) + csb_1(L) * uy1(L)
      !      ucxq_link_1(L) = csb_1(L) * ux3(L) + snb_1(L) * uy3(L)
      !      ucyq_link_1(L) = -snb_1(L) * ux3(L) + csb_1(L) * uy3(L)
      !
      !      ! Node 2: Rotate BOTH velocity types with same coefficients
      !      ucx_link_2(L) = csb_2(L) * ux2(L) + snb_2(L) * uy2(L)
      !      ucy_link_2(L) = -snb_2(L) * ux2(L) + csb_2(L) * uy2(L)
      !      ucxq_link_2(L) = csb_2(L) * ux4(L) + snb_2(L) * uy4(L)
      !      ucyq_link_2(L) = -snb_2(L) * ux4(L) + csb_2(L) * uy4(L)
      !   end do
      !else
      !   !$OMP SIMD
      !   do L = L1, L2
      !      ucx_link_1(L) = ux1(L)
      !      ucy_link_1(L) = uy1(L)
      !      ucx_link_2(L) = ux2(L)
      !      ucy_link_2(L) = uy2(L)
      !
      !      ucxq_link_1(L) = csu(L) * ux3(L) + snu(L) * uy3(L)
      !      ucyq_link_1(L) = -snu(L) * ux3(L) + csu(L) * uy3(L)
      !      ucxq_link_2(L) = csu(L) * ux4(L) + snu(L) * uy4(L)
      !      ucyq_link_2(L) = -snu(L) * ux4(L) + csu(L) * uy4(L) 
      !   end do
      !end if

   end subroutine transform_node_velocities_combined

!> Transform corner velocities (ucnx/ucny)
   subroutine transform_corner_velocities(ucnx, ucny)
      use m_flowgeom, only: lnx1D, lnx

      real(dp), intent(in), contiguous :: ucnx(:) ! Corner x-velocities (global frame)
      real(dp), intent(in), contiguous :: ucny(:) ! Corner y-velocities (global frame)

      integer :: L1, L2, L

      L1 = lnx1D + 1
      L2 = lnx

      if (.not. is_initialized) return

      do L = L1, L2
         ux1(L) = ucnx(corner_map_1(L))
         uy1(L) = ucny(corner_map_1(L))
         ux2(L) = ucnx(corner_map_2(L))
         uy2(L) = ucny(corner_map_2(L))
      end do

      !if (use_spherical_transform) then
      !   !$OMP SIMD
      !   do L = L1, L2
      !      ucnx_link_1(L) = csbn_1(L) * ux1(L) + snbn_1(L) * uy1(L)
      !      ucny_link_1(L) = -snbn_1(L) * ux1(L) + csbn_1(L) * uy1(L)
      !      ucnx_link_2(L) = csbn_2(L) * ux2(L) + snbn_2(L) * uy2(L)
      !      ucny_link_2(L) = -snbn_2(L) * ux2(L) + csbn_2(L) * uy2(L)
      !   end do
      !else
      !   !$OMP SIMD
      !   do L = L1, L2
      !      ucnx_link_1(L) = ux1(L)
      !      ucny_link_1(L) = uy1(L)
      !      ucnx_link_2(L) = ux2(L)
      !      ucny_link_2(L) = uy2(L)
      !   end do
      !end if

   end subroutine transform_corner_velocities

   subroutine cleanup_coordinate_transform()

      if (is_initialized) then
         !deallocate (ucx_link_1, ucy_link_1)
         !deallocate (ucx_link_2, ucy_link_2)
         !deallocate (ucnx_link_1, ucny_link_1)
         !deallocate (ucnx_link_2, ucny_link_2)
         !deallocate (ucxq_link_1, ucxq_link_2)
         !deallocate (ucyq_link_1, ucyq_link_2)
         deallocate (node_map_1, node_map_2, corner_map_1, corner_map_2)
         deallocate (ux1, uy1, ux2, uy2, ux3, uy3, ux4, uy4)

         is_initialized = .false.
      end if
   end subroutine cleanup_coordinate_transform

end module m_coordinate_transform
