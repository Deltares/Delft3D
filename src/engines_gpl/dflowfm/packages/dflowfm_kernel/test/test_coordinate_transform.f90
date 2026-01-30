module test_coordinate_transform
   use assertions_gtest
   use m_coordinate_transform
   use precision, only: dp
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_coordinate_transform, test_cartesian_identity, test_cartesian_identity,
   subroutine test_cartesian_identity() bind(C)
      ! Test that in Cartesian coordinates (no transformation needed),
      ! the output equals the input

      use m_flowgeom
      use m_sferic
      use m_alloc

      integer, parameter :: NUM_LINKS = 3
      integer, parameter :: NUM_NODES = 4
      integer, parameter :: NUM_CORNERS = 4
      real(kind=dp), parameter :: tolerance = 1e-10_dp
      real(kind=dp), dimension(:), allocatable :: ucx, ucy
      integer :: ierr, L, numk

      ! Set up minimal flow geometry for Cartesian case
      lnx = NUM_LINKS
      lnx1D = 0 ! All 2D links
      ndx = NUM_NODES
      numk = NUM_CORNERS

      ! Allocate arrays
      allocate (ln(2, lnx), stat=ierr)
      allocate (lncn(2, lnx), stat=ierr)
      allocate (csb(2, lnx), snb(2, lnx), stat=ierr)
      allocate (csbn(2, lnx), snbn(2, lnx), stat=ierr)

      ! Set up link topology
      ln(1, 1) = 1
      ln(2, 1) = 2
      ln(1, 2) = 2
      ln(2, 2) = 3
      ln(1, 3) = 3
      ln(2, 3) = 4

      lncn(1, 1) = 1
      lncn(2, 1) = 2
      lncn(1, 2) = 2
      lncn(2, 2) = 3
      lncn(1, 3) = 3
      lncn(2, 3) = 4

      ! Cartesian: identity transformation
      jsferic = 0
      jasfer3D = 0
      csb = 1.0_dp
      snb = 0.0_dp
      csbn = 1.0_dp
      snbn = 0.0_dp

      ! Initialize transform
      call initialize_coordinate_transform()

      ! Set up test velocities

      ucx = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]
      ucy = [0.5_dp, 1.5_dp, 2.5_dp, 3.5_dp]
      ucnx = [0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp]
      ucny = [0.6_dp, 0.7_dp, 0.8_dp, 0.9_dp]

      ! Transform
      call transform_velocities_to_links(ucx, ucy, ucnx, ucny)

      ! Check: in Cartesian, output should equal input
      ! Link 1: nodes 1 and 2
      call f90_expect_near(ucx_link(1, 1), ucx(1), tolerance, "Link 1, side 1, ucx mismatch")
      call f90_expect_near(ucx_link(2, 1), ucx(2), tolerance, "Link 1, side 2, ucx mismatch")
      call f90_expect_near(ucy_link(1, 1), ucy(1), tolerance, "Link 1, side 1, ucy mismatch")
      call f90_expect_near(ucy_link(2, 1), ucy(2), tolerance, "Link 1, side 2, ucy mismatch")

      ! Link 2: nodes 2 and 3
      call f90_expect_near(ucx_link(1, 2), ucx(2), tolerance, "Link 2, side 1, ucx mismatch")
      call f90_expect_near(ucx_link(2, 2), ucx(3), tolerance, "Link 2, side 2, ucx mismatch")

      ! Corner velocities
      call f90_expect_near(ucnx_link(1, 1), ucnx(1), tolerance, "Link 1, corner 1, ucnx mismatch")
      call f90_expect_near(ucny_link(1, 1), ucny(1), tolerance, "Link 1, corner 1, ucny mismatch")

      ! Cleanup
      call cleanup_coordinate_transform()
      deallocate (ln, lncn, csb, snb, csbn, snbn)

   end subroutine test_cartesian_identity
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_coordinate_transform, test_spherical_rotation, test_spherical_rotation,
   subroutine test_spherical_rotation() bind(C)
      ! Test that spherical coordinate transformation correctly rotates velocities
      ! into the link coordinate frame
      use mathconsts, only: pi
      use m_flowgeom, only: ln, lncn, csb, snb, csbn, snbn, lnx, lnx1D, ndx
      use network_data, only: numk
      use m_sferic, garbage => pi
      use m_alloc
      
      integer, parameter :: NUM_LINKS = 1
      integer, parameter :: NUM_NODES = 2
      integer, parameter :: NUM_CORNERS = 2
      real(kind=dp), parameter :: tolerance = 1e-10_dp
      real(kind=dp), allocatable :: ucx(:), ucy(:)
      real(kind=dp), allocatable :: ucnx(:), ucny(:)  ! <-- ADD THIS LINE
      integer :: ierr
      real(kind=dp) :: angle, cs, sn
      real(dp) :: expected_ucx_node1 ,expected_ucy_node1 ,expected_ucx_node2, expected_ucy_node2

      ! Set up minimal flow geometry for spherical case
      lnx = NUM_LINKS
      lnx1D = 0  ! All 2D links
      ndx = NUM_NODES
      numk = NUM_CORNERS
      
      ! Allocate arrays
      allocate(ln(2, lnx), stat=ierr)
      allocate(lncn(2, lnx), stat=ierr)
      allocate(csb(2, lnx), snb(2, lnx), stat=ierr)
      allocate(csbn(2, lnx), snbn(2, lnx), stat=ierr)
      allocate(ucx(2), ucy(2), ucnx(2), ucny(2), stat=ierr)

      ! Set up link topology (1 link connecting nodes 1 and 2)
      ln(1, 1) = 1
      ln(2, 1) = 2
      lncn(1, 1) = 1
      lncn(2, 1) = 2
      
      ! Spherical mode
      jsferic = 1
      jasfer3D = 1
      
      ! Set up a 45-degree rotation for testing
      ! Link frame is rotated 45 degrees from global frame
      angle = PI / 4.0_dp  ! 45 degrees
      cs = cos(angle)      ! sqrt(2)/2 ≈ 0.707
      sn = sin(angle)      ! sqrt(2)/2 ≈ 0.707
      
      ! Node 1: rotation matrix [cs, sn; -sn, cs]
      csb(1, 1) = cs
      snb(1, 1) = sn
      
      ! Node 2: same rotation
      csb(2, 1) = cs
      snb(2, 1) = sn
      
      ! Corners: same rotation
      csbn(1, 1) = cs
      snbn(1, 1) = sn
      csbn(2, 1) = cs
      snbn(2, 1) = sn
      
      ! Initialize transform
      call initialize_coordinate_transform()
      
      ! Node 1: velocity = (1, 0) in global frame
      ucx(1) = 1.0_dp
      ucy(1) = 0.0_dp
      
      ! Node 2: velocity = (0, 1) in global frame
      ucx(2) = 0.0_dp
      ucy(2) = 1.0_dp
      
      ! Corners: same pattern
      ucnx(1) = 1.0_dp
      ucny(1) = 0.0_dp
      ucnx(2) = 0.0_dp
      ucny(2) = 1.0_dp
      
      ! Transform to link frame
      call transform_velocities_to_links(ucx, ucy, ucnx, ucny)
      
      ! Expected results in LINK frame:
      ! ucx_link = cs*ucx + sn*ucy
      ! For node 1: cs*1 + sn*0 = cs ≈ 0.707
      expected_ucx_node1 = cs
      expected_ucy_node1 = -sn  ! ucy_link = -sn*ucx + cs*ucy = -sn*1 + cs*0
      
      ! For node 2: cs*0 + sn*1 = sn ≈ 0.707
      expected_ucx_node2 = sn
      expected_ucy_node2 = cs   ! -sn*0 + cs*1 = cs
      
      ! Check node transformations
      call f90_expect_near(ucx_link(1, 1), expected_ucx_node1, tolerance, &
                          "Node 1 ucx transformation incorrect")
      call f90_expect_near(ucy_link(1, 1), expected_ucy_node1, tolerance, &
                          "Node 1 ucy transformation incorrect")
      
      call f90_expect_near(ucx_link(2, 1), expected_ucx_node2, tolerance, &
                          "Node 2 ucx transformation incorrect")
      call f90_expect_near(ucy_link(2, 1), expected_ucy_node2, tolerance, &
                          "Node 2 ucy transformation incorrect")
      
      ! Check corner transformations (should be same as nodes)
      call f90_expect_near(ucnx_link(1, 1), expected_ucx_node1, tolerance, &
                          "Corner 1 ucnx transformation incorrect")
      call f90_expect_near(ucny_link(1, 1), expected_ucy_node1, tolerance, &
                          "Corner 1 ucny transformation incorrect")
      
      call f90_expect_near(ucnx_link(2, 1), expected_ucx_node2, tolerance, &
                          "Corner 2 ucnx transformation incorrect")
      call f90_expect_near(ucny_link(2, 1), expected_ucy_node2, tolerance, &
                          "Corner 2 ucny transformation incorrect")
      
      ! Cleanup
      call cleanup_coordinate_transform()
      deallocate(ln, lncn, csb, snb, csbn, snbn)
      
   end subroutine test_spherical_rotation
   !$f90tw)

end module test_coordinate_transform
