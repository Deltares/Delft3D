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

   ! Public interface
   public :: initialize_coordinate_transform
   public :: transform_velocities_to_links
   public :: cleanup_coordinate_transform

   ! Velocities stored in link-local coordinates, indexed by LINK (not node!)
   ! Layout: ucx_link(side, L) where side=1,2 and L = lnx1D+1:lnx
   real(dp), allocatable, public :: ucx_link_1(:), ucx_link_2(:)  ! Node x-velocity in link frame (2, lnx)
   real(dp), allocatable, public :: ucy_link_1(:), ucy_link_2(:) ! Node y-velocity in link frame (2, lnx)
   real(dp), allocatable, public :: ucnx_link_1(:), ucnx_link_2(:) ! Corner x-velocity in link frame (2, lnx)
   real(dp), allocatable, public :: ucny_link_1(:), ucny_link_2(:) ! Corner y-velocity in link frame (2, lnx)

   ! Pre-flattened index maps (enables vectorizable gather: ux1 = ucx(node_map_1))
   integer, allocatable :: node_map_1(:), node_map_2(:) ! ln(1:2, lnx1D+1:lnx) flattened
   integer, allocatable :: corner_map_1(:), corner_map_2(:) ! lncn(1:2, lnx1D+1:lnx) flattened

   ! Temporary gather buffers (flat arrays for vectorization)
   real(dp), allocatable :: ux1(:), uy1(:), ux2(:), uy2(:) ! Node velocities
   real(dp), allocatable :: ux3(:), uy3(:), ux4(:), uy4(:) ! Corner velocities

   logical :: use_spherical_transform = .false.
   logical :: is_initialized = .false.
   integer :: num_2d_links = 0

contains

   subroutine initialize_coordinate_transform()
      use m_flowgeom, only: lnx, lnx1D, ln, lncn
      use m_sferic, only: jsferic, jasfer3D

      implicit none
      integer :: ierr

      if (is_initialized) return

      use_spherical_transform = (jsferic == 1 .and. jasfer3D == 1)
      num_2d_links = lnx - lnx1D

      ! Allocate link-indexed velocity arrays
      allocate (ucx_link_1(num_2d_links), ucy_link_1(num_2d_links), stat=ierr)
      allocate (ucx_link_2(num_2d_links), ucy_link_2(num_2d_links), stat=ierr)
      allocate (ucnx_link_1(num_2d_links), ucny_link_1(num_2d_links), stat=ierr)
      allocate (ucnx_link_2(num_2d_links), ucny_link_2(num_2d_links), stat=ierr)

      ! Allocate pre-flattened index maps
      allocate (node_map_1(num_2d_links), node_map_2(num_2d_links), stat=ierr)
      allocate (corner_map_1(num_2d_links), corner_map_2(num_2d_links), stat=ierr)

      ! Allocate temporary gather buffers
      allocate (ux1(num_2d_links), uy1(num_2d_links), stat=ierr)
      allocate (ux2(num_2d_links), uy2(num_2d_links), stat=ierr)
      allocate (ux3(num_2d_links), uy3(num_2d_links), stat=ierr)
      allocate (ux4(num_2d_links), uy4(num_2d_links), stat=ierr)

      ! Build flattened index maps (one-time cost at initialization)
      node_map_1 = ln(1, lnx1D + 1:lnx)
      node_map_2 = ln(2, lnx1D + 1:lnx)
      corner_map_1 = lncn(1, lnx1D + 1:lnx)
      corner_map_2 = lncn(2, lnx1D + 1:lnx)

      is_initialized = .true.

   end subroutine initialize_coordinate_transform

   !> Transform velocities from global to link-local coordinates
   !! Uses vectorizable array indexing for gather operations
   subroutine transform_velocities_to_links(ucx, ucy, ucnx, ucny)
      use m_flowgeom, only: lnx1D, lnx, csb, snb, csbn, snbn

      real(dp), intent(in) :: ucx(:) ! Node x-velocities (global frame)
      real(dp), intent(in) :: ucy(:) ! Node y-velocities (global frame)
      real(dp), intent(in) :: ucnx(:) ! Corner x-velocities (global frame)
      real(dp), intent(in) :: ucny(:) ! Corner y-velocities (global frame)

      integer :: L1, L2, L

      L1 = lnx1D + 1
      L2 = lnx

      if (.not. is_initialized) return

      ! PHASE 1: Gather velocities - VECTORIZABLE with array indexing!
      ! Compiler can optimize: ux1 = ucx(node_map_1) using vector gather instructions
      ux1 = ucx(node_map_1)
      uy1 = ucy(node_map_1)
      ux2 = ucx(node_map_2)
      uy2 = ucy(node_map_2)

      ux3 = ucnx(corner_map_1)
      uy3 = ucny(corner_map_1)
      ux4 = ucnx(corner_map_2)
      uy4 = ucny(corner_map_2)

      if (use_spherical_transform) then
         ! PHASE 2: Rotate - PERFECTLY VECTORIZABLE!
         ! Use csb, snb, csbn, snbn directly from m_flowgeom

         ! Node rotations: [u_link] = [cs  sn] [u_global]
         !                            [-sn cs] [v_global]
      do L = L1, L2
         ucx_link_1(L) = csb(1, L) * ux1(L) + snb(1, L) * uy1(L)
         ucy_link_1(L) = -snb(1, L) * ux1(L) + csb(1, L) * uy1(L)
         ucx_link_2(L) = csb(2, L) * ux2(L) + snb(2, L) * uy2(L)
         ucy_link_2(L) = -snb(2, L) * ux2(L) + csb(2, L) * uy2(L)

         ! Corner rotations
         ucnx_link_1(L) = csbn(1, L) * ux3(L) + snbn(1, L) * uy3(L)
         ucny_link_1(L) = -snbn(1, L) * ux3(L) + csbn(1, L) * uy3(L)
         ucnx_link_2(L) = csbn(2, L) * ux4(L) + snbn(2, L) * uy4(L)
         ucny_link_2(L) = -snbn(2, L) * ux4(L) + csbn(2, L) * uy4(L)
      end do
      else
         ucx_link_1 = ux1
         ucy_link_1 = uy1
         ucx_link_2 = ux2
         ucy_link_2 = uy2

         ucnx_link_1 = ux3
         ucny_link_1 = uy3
         ucnx_link_2 = ux4
         ucny_link_2 = uy4
      end if

   end subroutine transform_velocities_to_links

   subroutine cleanup_coordinate_transform()
      if (is_initialized) then
         deallocate (ucx_link_1, ucx_link_2, ucy_link_1, ucy_link_2, ucnx_link_1, ucnx_link_2, ucny_link_1, ucny_link_2)
         deallocate (node_map_1, node_map_2, corner_map_1, corner_map_2)
         deallocate (ux1, uy1, ux2, uy2, ux3, uy3, ux4, uy4)

         is_initialized = .false.
      end if
   end subroutine cleanup_coordinate_transform

end module m_coordinate_transform
