module test_coordinate_transform
use assertions_gtest
use m_coordinate_transform
use precision, only: dp
implicit none(external)

contains

!$f90tw TESTCODE(TEST, test_coordinate_transform, test_tangential_velocity_equivalence, test_tangential_velocity_equivalence,
subroutine test_tangential_velocity_equivalence() bind(C)
   ! Test that the tangential velocity computation matches the original code
   ! This is the KEY test for your Coriolis force bug!

   use mathconsts, only: pi
   use m_flowgeom, only: ln, lncn, csb, snb, csbn, snbn, lnx, lnx1D, ndx, csu, snu, bai
   use network_data, only: numk
   use m_sferic, garbage => pi
   use m_nod2linx, only: nod2linx
   use m_nod2liny, only: nod2liny
   use m_alloc

   integer, parameter :: NUM_LINKS = 1
   integer, parameter :: NUM_NODES = 2
   real(kind=dp), parameter :: tolerance = 1e-10_dp
   real(kind=dp), allocatable :: ucx(:), ucy(:), ucxq(:), ucyq(:)
   integer :: ierr, L
   real(kind=dp) :: angle, cs_link, sn_link, cs_node, sn_node

   ! Variables for computing tangential velocity
   real(kind=dp) :: tangential_original_node1, tangential_original_node2
   real(kind=dp) :: tangential_refactor_node1, tangential_refactor_node2
   real(kind=dp) :: link_x_1, link_y_1, link_x_2, link_y_2

   ! Set up geometry
   lnx = NUM_LINKS
   lnx1D = 0
   ndx = NUM_NODES
   numk = NUM_NODES

   allocate (ln(2, lnx), lncn(2, lnx), stat=ierr)
   allocate (csb(2, lnx), snb(2, lnx), stat=ierr)
   allocate (csbn(2, lnx), snbn(2, lnx), stat=ierr)
   allocate (csu(lnx), snu(lnx), stat=ierr)
   allocate (ucx(NUM_NODES), ucy(NUM_NODES), stat=ierr)
   allocate (ucxq(NUM_NODES), ucyq(NUM_NODES), stat=ierr)
   bai = [1, 1] ! Dummy bottom area inverse
   ln(1, 1) = 1
   ln(2, 1) = 2
   lncn(1, 1) = 1
   lncn(2, 1) = 2

   jsferic = 1
   jasfer3D = 1

   ! Link is at 30 degrees from x-axis
   angle = PI / 6.0_dp ! 30 degrees
   cs_link = cos(angle)
   sn_link = sin(angle)
   csu(1) = cs_link
   snu(1) = sn_link

   ! Node coordinate frames: 45 degrees from global
   angle = PI / 4.0_dp
   cs_node = cos(angle)
   sn_node = sin(angle)
   csb(1, 1) = cs_node
   snb(1, 1) = sn_node
   csb(2, 1) = cs_node
   snb(2, 1) = sn_node
   csbn(1, 1) = cs_node
   snbn(1, 1) = sn_node
   csbn(2, 1) = cs_node
   snbn(2, 1) = sn_node

   call allocate_prefetch_arrays()

   ! Test velocities
   ucxq(1) = 1.5_dp
   ucyq(1) = 0.8_dp
   ucxq(2) = 0.6_dp
   ucyq(2) = 1.2_dp

   ! Initialize transform
   call prefetch_node_velocities(ucx, ucy, ucxq, ucyq)

   ! ORIGINAL METHOD (from old setumod.f90 line 245-252):
   ! fvcor = acL(LL) * (-sn * nod2linx(LL, 1, ucxq(k1), ucyq(k1)) + cs * nod2liny(LL, 1, ucxq(k1), ucyq(k1))) * fcor1

   L = 1
   link_x_1 = nod2linx(L, 1, ucxq(1), ucyq(1)) ! Transform to link frame
   link_y_1 = nod2liny(L, 1, ucxq(1), ucyq(1))
   tangential_original_node1 = -snu(L) * link_x_1 + csu(L) * link_y_1 ! Tangential component

   link_x_2 = nod2linx(L, 2, ucxq(2), ucyq(2))
   link_y_2 = nod2liny(L, 2, ucxq(2), ucyq(2))
   tangential_original_node2 = -snu(L) * link_x_2 + csu(L) * link_y_2

   ! REFACTORED METHOD (what your code should compute):
   ! Step 1: ucxq_1, ucyq_1 contain ucxq values (from prefetch_node_velocities)
   ! Step 2: Transform to link frame
   !   ucxq_link_1 = csb_1(L) * ucxq_1(L) + snb_1(L) * ucyq_1(L)  ! link-x component
   !   ucyq_link_1 = -snb_1(L) * ucxq_1(L) + csb_1(L) * ucyq_1(L) ! link-y component
   ! Step 3: Compute tangential
   !   tangential = -snu(L) * ucxq_link_1 + csu(L) * ucyq_link_1

   tangential_refactor_node1 = -snu(L) * (csb_1(L) * ucxq_1(L) + snb_1(L) * ucyq_1(L)) + &
                               csu(L) * (-snb_1(L) * ucxq_1(L) + csb_1(L) * ucyq_1(L))

   tangential_refactor_node2 = -snu(L) * (csb_2(L) * ucxq_2(L) + snb_2(L) * ucyq_2(L)) + &
                               csu(L) * (-snb_2(L) * ucxq_2(L) + csb_2(L) * ucyq_2(L))

   call f90_expect_near(tangential_refactor_node1, tangential_original_node1, tolerance, &
                        "Node 1 tangential velocity mismatch between original and refactored code!")

   call f90_expect_near(tangential_refactor_node2, tangential_original_node2, tolerance, &
                        "Node 2 tangential velocity mismatch between original and refactored code!")

   ! Cleanup
   call cleanup_prefetch_arrays()
   deallocate (ln, lncn, csb, snb, csbn, snbn, csu, snu, ucx, ucy, ucxq, ucyq)

end subroutine test_tangential_velocity_equivalence
!$f90tw)

end module test_coordinate_transform
