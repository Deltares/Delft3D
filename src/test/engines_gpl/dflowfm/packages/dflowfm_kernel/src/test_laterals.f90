!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module test_lateral
   use ftnunit
   use precision_basics, only: dp
   use dfm_error, only: DFM_NOERR
   use m_alloc, only: aerr
   use m_laterals

   implicit none

   real(dp), parameter :: tolerance = 1.0e-10_dp

contains
!
!==============================================================================
!
   subroutine tests_lateral()

      call test(test_add_lateral_load_and_sink, 'Test computation of constituents sinks and sources due to laterals.')
      call test(test_get_lateral_volume_per_layer, 'Test computation of water volume per layer in laterals.')
      call test(test_distribute_lateral_discharge, 'Test the distribution of lateral discharge per layer per lateral '// &
                ' to per layer per lateral per cell.')
   end subroutine tests_lateral
!
!==============================================================================
!> Test computation of sinks and sources (discharge and transport load per cell) due to laterals
   subroutine test_add_lateral_load_and_sink()
      use m_flow, only: vol1, hs, kmxn
      use m_transportdata, only: numconst
      use m_flowgeom, only: ndxi

      real(kind=dp), allocatable, dimension(:, :) :: transport_load !< Load being transported into domain
      real(kind=dp), allocatable, dimension(:, :) :: transport_sink !< sink term due to transport into domain

      real(kind=dp), allocatable, dimension(:, :) :: ref_load
      real(kind=dp) :: refval
      real(kind=dp) :: dvoli
      integer :: iostat ! allocation status
      integer :: i_cell, i_const, i_lateral ! loop counters
      integer :: i_node

      integer :: n_nodes, n_nodes_on_laterals, n_laterals, n_tracers, n_layers

      ! domain of 10 points, 2 laterals (1 incoming with 3 nodes, 1 outgoing with 2 nodes)
      ! 1 layer and 3 constituents. Volume (vol1) of each cell is set to 0.1d0.
      ! consider 3 constituents to represent salt, temperature and tracer transport.
      n_nodes = 10
      n_nodes_on_laterals = 5
      n_laterals = 2
      n_layers = 1
      n_tracers = 3

      ! initialization and allocation of global state variables
      call setup_testcase(n_nodes, n_nodes_on_laterals, n_laterals, n_layers, n_tracers)
      ! test-specific
      n1latsg(1) = 1
      n2latsg(1) = 3
      n1latsg(2) = 4
      n2latsg(2) = 5
      nnlat = (/1, 2, 3, 5, 8/)
      apply_transport(:) = 1
      vol1(:) = 0.1_dp
      hs(:) = 2._dp
      kmxn(:) = 1
      ! top layer, per constituent, lateral 1,
      incoming_lat_concentration(1, :, 1) = (/31.0_dp, 20.0_dp, 0.23_dp/)
      ! top layer, all constituents, lateral 2.
      outgoing_lat_concentration(1, :, 2) = 25_dp

      allocate (transport_load(numconst, ndxi), stat=iostat)
      call aerr('transport_load', iostat, numconst * ndxi, 'test_add_lateral_load_and_sink')
      allocate (transport_sink(numconst, ndxi), stat=iostat)
      call aerr('transport_sink', iostat, numconst * ndxi, 'test_add_lateral_load_and_sink')
      allocate (ref_load(numconst, ndxi), stat=iostat)
      call aerr('ref_load', iostat, numconst * ndxi, 'test_add_lateral_load_and_sink')

      ! initialize transport to zero
      transport_load(:, :) = 0._dp
      transport_sink(:, :) = 0._dp

      ! first check that no discharge means no added transport
      qqlat(:, :) = 0._dp
      call add_lateral_load_and_sink(transport_load, transport_sink, vol1, tolerance)

      call assert_comparable(sum(transport_load), 0._dp, tolerance, "lateral_load value expected to be zero for qqlat=0")
      call assert_comparable(sum(transport_sink), 0._dp, tolerance, "lateral_sink value expected to be zero for qqlat=0")

      ! check transport into the domain
      i_lateral = 1 ! only the first lateral is a source
      do i_node = n1latsg(i_lateral), n2latsg(i_lateral)
         qqlat(1, i_node) = 5._dp
      end do
      call add_lateral_load_and_sink(transport_load, transport_sink, vol1, tolerance)

      do i_const = 1, numconst
         do i_cell = n1latsg(i_lateral), n2latsg(i_lateral)
            i_node = nnlat(i_cell)
            dvoli = 1 / (vol1(i_cell))
            refval = dvoli * incoming_lat_concentration(1, i_const, i_lateral) * qqlat(1, i_cell)
            call assert_comparable(transport_load(i_const, i_node), refval, tolerance, "lateral_load value is not correct")
         end do
      end do
      call assert_comparable(sum(transport_sink), 0._dp, tolerance, "lateral_sink value expected to be zero ")

      ! check transport out of the domain
      i_lateral = 2 ! only the second lateral is a sink
      qqlat = 0._dp
      do i_node = n1latsg(i_lateral), n2latsg(i_lateral)
         qqlat(1, i_node) = -5._dp
      end do
      ! copy values of transport_load
      ref_load(:, :) = transport_load(:, :)
      call add_lateral_load_and_sink(transport_load, transport_sink, vol1, tolerance)
      ! check that transport_load was not changed
      call assert_comparable(sum(transport_load), sum(ref_load), tolerance, "transport_load should not change")
      do i_const = 1, numconst
         do i_cell = n1latsg(i_lateral), n2latsg(i_lateral)
            i_node = nnlat(i_cell)
            dvoli = 1 / (vol1(i_cell))
            refval = dvoli * qqlat(1, i_cell)
            call assert_comparable(transport_sink(i_const, i_node), refval, tolerance, "lateral_sink value is not correct")
         end do
      end do

      deallocate (transport_load)
      deallocate (transport_sink)

      ! deallocation of global state variables
      call finish_testcase()

   end subroutine test_add_lateral_load_and_sink
!
!> initialize and allocate global state variables for lateral testcases
   subroutine setup_testcase(n_nodes, n_nodes_on_laterals, n_laterals, n_layers, n_tracers)
      use m_flow, only: vol1, hs, kmx, kmxn, ndkx, kbot, ktop
      use m_flowtimes, only: dts
      use m_partitioninfo, only: jampi
      use m_transportdata, only: numconst
      use m_flowgeom, only: ndx, ndxi

      integer, intent(in) :: n_nodes
      integer, intent(in) :: n_nodes_on_laterals
      integer, intent(in) :: n_laterals
      integer, intent(in) :: n_layers
      integer, intent(in) :: n_tracers

      integer :: iostat ! allocation status

      jampi = 0
      dts = 1.0e-3_dp
      ndx = n_nodes
      ndxi = n_nodes
      nlatnd = n_nodes_on_laterals
      numlatsg = n_laterals
      numconst = n_tracers
      kmx = n_layers
      ndkx = ndx * kmx

      call initialize_lateraldata(numconst)
      allocate (n1latsg(numlatsg), stat=iostat)
      call aerr('n1latsg', iostat, numlatsg, 'test_lateral, setup_testcase')
      allocate (n2latsg(numlatsg), stat=iostat)
      call aerr('n2latsg', iostat, numlatsg, 'test_lateral, setup_testcase')
      allocate (apply_transport(numlatsg), stat=iostat)
      call aerr('apply_transport', iostat, numlatsg, 'test_lateral, setup_testcase')
      allocate (nnlat(nlatnd), stat=iostat)
      call aerr('nnlat', iostat, nlatnd, 'test_lateral, setup_testcase')
      allocate (vol1(ndxi), stat=iostat)
      call aerr('vol1', iostat, ndxi, 'test_lateral, setup_testcase')
      allocate (hs(ndxi), stat=iostat)
      call aerr('hs', iostat, ndxi, 'test_lateral, setup_testcase')
      allocate (kmxn(ndkx), stat=iostat)
      call aerr('kmxn', iostat, ndx, 'test_lateral, setup_testcase')
      allocate (kbot(ndx), stat=iostat)
      call aerr('kbot', iostat, ndx, 'test_lateral, setup_testcase')
      allocate (ktop(ndx), stat=iostat)
      call aerr('ktop', iostat, ndx, 'test_lateral, setup_testcase')

   end subroutine setup_testcase
!
!> reset to default values and deallocate global state variables
   subroutine finish_testcase()
      use m_flow, only: vol1, hs, kmxn, kbot, ktop
      use m_flowtimes, only: dts
      use m_partitioninfo, only: jampi
      use m_transportdata, only: numconst
      use m_flowgeom, only: ndx, ndxi

      jampi = 1
      dts = 0._dp
      ndx = 0
      ndxi = 0
      numconst = 0

      call reset_lateral()
      call dealloc_lateraldata()

      deallocate (n1latsg)
      deallocate (n2latsg)
      deallocate (apply_transport)
      deallocate (nnlat)
      deallocate (vol1)
      deallocate (hs)
      deallocate (kmxn)
      deallocate (kbot)
      deallocate (ktop)

   end subroutine finish_testcase
!
!==============================================================================
!> Test computation of water volume per layer for laterals.
!> This test assumes a model of dimension (nx,ny,nz) = (3,3,3).
!> In the last node, the model is shallow meaning it has only 2 active layers.
!> The model contains 2 laterals.
   subroutine test_get_lateral_volume_per_layer
      use m_flow, only: vol1, kbot, ktop, kmxn, kmx, ndkx
      use m_flowgeom, only: ndx
      use m_alloc, only: realloc

      integer :: i_cell
      integer :: n_nodes, n_nodes_on_laterals, n_laterals, n_tracers, n_layers

      ! domain of 9 points, 2 laterals (1 incoming with 3 nodes, 1 outgoing with 2 nodes)
      ! max 3 layers and 3 constituents. Volume (vol1) of each cell is set to 0.1d0.
      ! only consider 1 constituent.
      n_nodes = 9
      n_nodes_on_laterals = 4
      n_laterals = 2
      n_tracers = 1
      n_layers = 3

      ! initialization and allocation of global state variables
      call setup_testcase(n_nodes, n_nodes_on_laterals, n_laterals, n_layers, n_tracers)

      ! initialize number of active layers for each node
      kmxn = (/3, 3, 3, 3, 3, 3, 3, 3, 2/) ! the last cell is assumed shallow and contains only two layers

      ! initialize water volume per cell, vol1
      ndkx = ndx * (kmx + 1) - 1 ! one cell is shallow and contains only two layers
      call realloc(vol1, ndkx, keepExisting=.false., fill=0d0)
      vol1(ndx + 1:) = 1d0 ! only volume per cell, per layer is needed; the first ndx elements contain 2D volume (i.e. total volume over all layers) and are not needed for the function tested here, hence not set.

      ! initialize kbot and ktop
      kbot(1) = ndx + 1
      ktop(1) = kbot(1) + kmxn(1) - 1
      do i_cell = 2, ndx
         kbot(i_cell) = ktop(i_cell - 1) + 1
         ktop(i_cell) = kbot(i_cell) + kmxn(i_cell) - 1
      end do

      ! initialize laterals administration
      nnlat = (/1, 2, 8, 9/)
      n1latsg = (/1, 3/)
      n2latsg = (/2, 4/)

      call get_lateral_volume_per_layer(lateral_volume_per_layer)
      call assert_comparable(lateral_volume_per_layer(1, 1), 2d0, tolerance, "get_lateral_volume_per_layer(1,1) output lateral_volume_per_layer is not correct")
      call assert_comparable(lateral_volume_per_layer(2, 1), 2d0, tolerance, "get_lateral_volume_per_layer(2,1) output lateral_volume_per_layer is not correct")
      call assert_comparable(lateral_volume_per_layer(3, 1), 2d0, tolerance, "get_lateral_volume_per_layer(3,1) output lateral_volume_per_layer is not correct")
      call assert_comparable(lateral_volume_per_layer(1, 2), 1d0, tolerance, "get_lateral_volume_per_layer(1,2) output lateral_volume_per_layer is not correct")
      call assert_comparable(lateral_volume_per_layer(2, 2), 2d0, tolerance, "get_lateral_volume_per_layer(2,2) output lateral_volume_per_layer is not correct")
      call assert_comparable(lateral_volume_per_layer(3, 2), 2d0, tolerance, "get_lateral_volume_per_layer(3,2) output lateral_volume_per_layer is not correct")

      ! deallocation of global state variables
      call finish_testcase()

   end subroutine test_get_lateral_volume_per_layer

!==============================================================================
!> Test computation of distribution of lateral discharge per layer to discharge per layer per cell.
!! This test assumes a model of dimension (nx,ny,nz) = (4,2,3). In another word, there are 4 nodes in x-direction, 2 nodes
!! in y-direction, so that ndx = 8.
!! There are 3 layers on all nodes, except for the 8th node which has only 2 active layers. So ndkx = (3+1)*8-1 = 31.
!! The model contains 2 lateralsL: the 1st lateral is on nodes 1 and 2, the 2nd lateral is on nodes 7 and 8.
   subroutine test_distribute_lateral_discharge
      use m_flow, only: vol1, kbot, ktop, kmxn, kmx
      use m_alloc, only: realloc
      use m_flowgeom, only: ndx

      real(kind=dp), allocatable, dimension(:,:) :: lateral_discharge_per_layer_lateral_cell !< Discharge per layer per lateral per cell,
                                                                                             !! dimension=(number_of_layer,numlatsg,number_of_node)
                                                                                             !!          =(kmx,numlatsg,ndkx)
      
      integer :: ierr !< error flag
      integer :: i_node
      integer :: ndkx
      integer :: n_nodes, n_nodes_on_laterals, n_laterals, n_tracers, n_layers
      real(kind=dp), allocatable, dimension(:, :) :: provided_lateral_discharge

      ! domain of 8 points, 2 laterals (1 incoming with 2 nodes, 1 outgoing with 2 nodes)
      ! max 3 layers and 3 constituents. Volume (vol1) of each cell is set to 0.1d0.
      ! only consider 1 constituent.
      n_nodes = 8
      n_nodes_on_laterals = 4
      n_laterals = 2
      n_tracers = 1
      n_layers = 3

      ! initialization and allocation of global state variables
      call setup_testcase(n_nodes, n_nodes_on_laterals, n_laterals, n_layers, n_tracers)

      ! Initialize number of active layers for each cell
      kmxn = [3, 3, 3, 3, 3, 3, 3, 2] ! The 8th cell is assumed shallow and contains only two layers

      ! Initialize water volume per cell per layer, vol1.
      ndkx = ndx * (kmx + 1) - 1 ! one cell is shallow and contains only two layers
      call realloc(vol1, ndkx, stat=ierr, keepExisting=.false., fill=0d0)
      call aerr('vol1', ierr, ndkx, 'test_distribute_lateral_discharge_per_layer_per_cell')

      ! vol1 (ndx+1:ndkx) = [19, 20, 21, ..., 41].
      do i_node = ndx + 1, ndkx
         vol1(i_node) = 10 + i_node ! only volume per cell, per layer is needed; the first ndx elements contain 2D volume
         ! (i.e. total volume over all layers) and are not needed for the function tested here,
         ! hence not set.
      end do

      ! Initialize kbot and ktop
      kbot(1) = ndx + 1
      ktop(1) = kbot(1) + kmxn(1) - 1
      do i_node = 2, ndx
         kbot(i_node) = ktop(i_node - 1) + 1
         ktop(i_node) = kbot(i_node) + kmxn(i_node) - 1
      end do

      ! The 1st lateral is on nodes 1 and 2, the 2nd lateral is on nodes 7 and 8.
      nnlat = [1, 2, 7, 8]
      n1latsg = [1, 3]
      n2latsg = [2, 4]

      ! Initialize lateral volume per layer and discharge per layer.
      call realloc(provided_lateral_discharge, [kmx, numlatsg], stat=ierr, keepExisting=.false., fill=0d0)
      call aerr('provided_lateral_discharge_per_layer', ierr, kmx * numlatsg, 'test_distribute_lateral_discharge_per_layer_per_cell')

      lateral_volume_per_layer(1:kmx,1) = [100, 300, 250] ! Volume for the 1st lateral per layer
      lateral_volume_per_layer(1:kmx,2) = [200, 250, 330] ! Volume for the 2nd lateral per layer
      provided_lateral_discharge(1:kmx,1) = [1000, 1500, 2800] ! Discharge for the 1st lateral per layer
      provided_lateral_discharge(1:kmx,2) = [2000, 3000, 2500] ! Discharge for the 2nd lateral per layer

      call realloc(lateral_discharge_per_layer_lateral_cell, [kmx, nlatnd], stat=ierr, keepExisting=.false., fill=0d0)
      call aerr('lateral_discharge_per_layer_per_cell', ierr, kmx * numlatsg*ndkx, 'test_distribute_lateral_discharge_per_layer_per_cell')

      ! Distribute the lateral discharge
      call distribute_lateral_discharge(provided_lateral_discharge, lateral_discharge_per_layer_lateral_cell)

      ! Compare results with reference results.
      ! The 2 laterals are applied on 4 nodes, i.e. 1, 2, 7 and 8. Considering the layers, in total 4*3-1=11 cells are involved,
      ! so below we compare 11 sets of values, for each cell on each layer.
      ! Take the 1st comparison as an example:
      ! Target:
      !  lateral_discharge_per_layer_per_cell(1,9) is for node 9 on layer 1.
      ! Known:
      !  a. vo1(9) = 19. Node 9 is above node 1 on layer 1, so it belongs to the 1st lateral. Then we need
      !  b. lateral_volume_per_layer(1,1) = 100, which is the volume of lateral 1 at layer 1. Then we need
      !  c. provided_lateral_discharge_per_layer(1,1) = 1000, which is the discharge of lateral 1 at layer 1.
      ! With a, b. and c, we can compute the target:
      !   lateral_discharge_per_layer_per_cell(1,9) = 19/100*1000 = 190.
      call assert_comparable(lateral_discharge_per_layer_lateral_cell(1,1),  190.0d0, tolerance, &
                             "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(1,9)" // &
                             " is not correct.")
      call assert_comparable(lateral_discharge_per_layer_lateral_cell(2,1), 100.0d0, tolerance, &
                             "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(2,10)" // &
                             " is not correct.")
      call assert_comparable(lateral_discharge_per_layer_lateral_cell(3,1), 235.2d0, tolerance, &
                             "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(3,11)" // &
                             " is not correct.")
      call assert_comparable(lateral_discharge_per_layer_lateral_cell(1,2), 220.0d0, tolerance, &
                             "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(1,12)" // &
                             " is not correct.")
      call assert_comparable(lateral_discharge_per_layer_lateral_cell(2,2), 115.0d0, tolerance, &
                             "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(2,13)" // &
                             " is not correct.")
      call assert_comparable(lateral_discharge_per_layer_lateral_cell(3,2), 268.8d0, tolerance, &
                             "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(3,14)" // &
                             " is not correct.")
      call assert_comparable(lateral_discharge_per_layer_lateral_cell(1,3), 370.0d0, tolerance, &
                            "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(1,27)" // &
                             " is not correct.")
      call assert_comparable(lateral_discharge_per_layer_lateral_cell(2,3), 456.0d0, tolerance, &
                             "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(2,13)" // &
                             " is not correct.")
      call assert_comparable(lateral_discharge_per_layer_lateral_cell(3,3), 295.454545454545d0, tolerance, &
                             "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(3,29)" // &
                             " is not correct.")
      call assert_comparable(lateral_discharge_per_layer_lateral_cell(2,4), 480.0d0, tolerance, &
                             "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(2,30)" // &
                             " is not correct.")
      call assert_comparable(lateral_discharge_per_layer_lateral_cell(3,4), 310.606060606061d0, tolerance, &
                             "distribute_lateral_discharge_per_layer_per_cell: output lateral_discharge_per_layer_per_cell(3,31)" // &
                             " is not correct.")
   
      ! deallocation of global state variables
      call finish_testcase()

   end subroutine test_distribute_lateral_discharge

end module test_lateral
