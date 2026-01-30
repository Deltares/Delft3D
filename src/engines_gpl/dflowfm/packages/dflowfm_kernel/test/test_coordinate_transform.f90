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
      call f90_expect_near(ucx_in_link_frame(1, 1), ucx(1), tolerance, "Link 1, side 1, ucx mismatch")
      call f90_expect_near(ucx_in_link_frame(2, 1), ucx(2), tolerance, "Link 1, side 2, ucx mismatch")
      call f90_expect_near(ucy_in_link_frame(1, 1), ucy(1), tolerance, "Link 1, side 1, ucy mismatch")
      call f90_expect_near(ucy_in_link_frame(2, 1), ucy(2), tolerance, "Link 1, side 2, ucy mismatch")

      ! Link 2: nodes 2 and 3
      call f90_expect_near(ucx_in_link_frame(1, 2), ucx(2), tolerance, "Link 2, side 1, ucx mismatch")
      call f90_expect_near(ucx_in_link_frame(2, 2), ucx(3), tolerance, "Link 2, side 2, ucx mismatch")

      ! Corner velocities
      call f90_expect_near(ucnx_in_link_frame(1, 1), ucnx(1), tolerance, "Link 1, corner 1, ucnx mismatch")
      call f90_expect_near(ucny_in_link_frame(1, 1), ucny(1), tolerance, "Link 1, corner 1, ucny mismatch")

      ! Cleanup
      call cleanup_coordinate_transform()
      deallocate (ln, lncn, csb, snb, csbn, snbn)

   end subroutine test_cartesian_identity
   !$f90tw)

end module test_coordinate_transform
