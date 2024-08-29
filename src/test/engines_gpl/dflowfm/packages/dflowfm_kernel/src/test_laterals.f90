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
!
!==============================================================================
!   
subroutine tests_lateral()
   ! initialization of global state variables for all tests in this module
   call setup_testcase()

   call test(test_get_lateral_discharge,     'Test computation of total discharge over laterals.')
   call test(test_add_lateral_load_and_sink, 'Test computation of constituents sinks and sources due to laterals.')

   ! deallocation of global state variables
   call finish_testcase()

   call test(test_get_lateral_volume_per_layer, 'Test computation of water volume per layer in laterals.')
   call test(test_distribute_lateral_discharge, 'Test the distribution of lateral discharge per layer per lateral,' // &
                                                ' which is retrieved from BMI, to per layer per lateral per cell.')
end subroutine tests_lateral
!
!==============================================================================
!> Test computation of sinks and sources (discharge and transport load per cell) due to laterals
subroutine test_get_lateral_discharge()
   use m_flow, only: vol1
   use m_flowgeom, only: ndxi
   
   real(kind=dp), allocatable, dimension(:,:,:)   :: lateral_discharge_in                !< Lateral discharge going into the model (source)
   real(kind=dp), allocatable, dimension(:,:,:)   :: lateral_discharge_out               !< Lateral discharge extracted out of the model (sink)
   real(kind=dp), allocatable, dimension(:,:)   :: reference_lateral_discharge_in      !< Reference lateral discharge going into the model (source)
   real(kind=dp), allocatable, dimension(:,:)   :: reference_lateral_discharge_out     !< Reference lateral discharge extracted out of the model (sink)
   
   integer :: iostat
   integer :: i_cell, i_lateral      ! loop counters

   allocate(lateral_discharge_in(1,numlatsg,ndxi),stat=iostat)
   call aerr('lateral_discharge_in',iostat,numlatsg*ndxi,'test_get_lateral_discharge' )
   allocate(lateral_discharge_out(1,numlatsg,ndxi),stat=iostat)
   call aerr('lateral_discharge_out',iostat,numlatsg*ndxi,'test_get_lateral_discharge' )
   allocate(reference_lateral_discharge_in(numlatsg,ndxi),stat=iostat)
   call aerr('reference_lateral_discharge_in',iostat,numlatsg*ndxi,'test_get_lateral_discharge' )
   allocate(reference_lateral_discharge_out(numlatsg,ndxi),stat=iostat)
   call aerr('reference_lateral_discharge_out',iostat,numlatsg*ndxi,'test_get_lateral_discharge' )

   ! check setup_testcase: lateral inflow over three cells with vol1
   ! lateral outflow over 2 lateral cells with vol1
   reference_lateral_discharge_in = 0._dp
   reference_lateral_discharge_in(1,nnlat(1)) = qplat(1,1)*vol1(1)
   reference_lateral_discharge_in(1,nnlat(2)) = qplat(1,1)*vol1(2)
   reference_lateral_discharge_in(1,nnlat(3)) = qplat(1,1)*vol1(3)
   reference_lateral_discharge_out = 0._dp
   reference_lateral_discharge_out(2,nnlat(4)) = -qplat(1,2)*vol1(1)
   reference_lateral_discharge_out(2,nnlat(5)) = -qplat(1,2)*vol1(1)
   
   call get_lateral_discharge(lateral_discharge_in,lateral_discharge_out,vol1)
   
   do i_lateral = 1,numlatsg
      do i_cell=1,ndxi
         call assert_comparable(lateral_discharge_in(1,i_lateral,i_cell), reference_lateral_discharge_in(i_lateral,i_cell), tolerance, "get_lateral_discharge(): lateral_discharge_in is not correct" )
         call assert_comparable(lateral_discharge_out(1,i_lateral,i_cell), reference_lateral_discharge_out(i_lateral,i_cell), tolerance, "get_lateral_discharge(): lateral_discharge_out is not correct" )         
      end do
   end do
      
   deallocate(lateral_discharge_in)
   deallocate(lateral_discharge_out)
   deallocate(reference_lateral_discharge_in)
   deallocate(reference_lateral_discharge_out)
 
end subroutine test_get_lateral_discharge
!
!==============================================================================
!> Test computation of sinks and sources (discharge and transport load per cell) due to laterals
subroutine test_add_lateral_load_and_sink()
   use m_flow, only: vol1
   use m_transportdata, only: numconst
   use m_flowgeom, only: ndxi
   
   real(kind=dp), allocatable, dimension(:,:,:)   :: discharge_in                !< Lateral discharge going into the model (source)
   real(kind=dp), allocatable, dimension(:,:,:)   :: discharge_out               !< Lateral discharge extracted out of the model (sink)
   real(kind=dp), allocatable, dimension(:,:)   :: transport_load              !< Load being transported into domain
   real(kind=dp), allocatable, dimension(:,:)   :: transport_sink              !< sink term due to transport into domain

   real(kind=dp), allocatable, dimension(:,:)   :: ref_load
   real(kind=dp) :: refval
   real(kind=dp) :: dvoli 
   integer :: iostat                      ! allocation status
   integer :: i_cell, i_const, i_lateral  ! loop counters

   allocate(discharge_in(1,numlatsg,ndxi),stat=iostat)
   call aerr('discharge_in',iostat,numlatsg*ndxi,'test_add_lateral_load_and_sink')
   allocate(discharge_out(1,numlatsg,ndxi),stat=iostat)
   call aerr('discharge_out',iostat,numlatsg*ndxi,'test_add_lateral_load_and_sink')
   allocate(transport_load(numconst,ndxi),stat=iostat)
   call aerr('transport_load',iostat,numconst*ndxi,'test_add_lateral_load_and_sink')
   allocate(transport_sink(numconst,ndxi),stat=iostat)
   call aerr('transport_sink',iostat,numconst*ndxi,'test_add_lateral_load_and_sink')
   allocate(ref_load(numconst,ndxi),stat=iostat)
   call aerr('ref_load',iostat,numconst*ndxi,'test_add_lateral_load_and_sink')

   ! initialize transport to zero
   transport_load(:,:) = 0._dp
   transport_sink(:,:) = 0._dp

   ! first check that no discharge means no added transport
   discharge_in = 0._dp
   discharge_out = 0._dp
   call add_lateral_load_and_sink(transport_load,transport_sink,discharge_in,discharge_out,vol1,tolerance)

   call assert_comparable(sum(transport_load), 0._dp, tolerance, "todo")
   call assert_comparable(sum(transport_sink), 0._dp, tolerance, "todo")

   ! check transport into the domain
   i_lateral = 1 ! only the first lateral is a source
   discharge_in(1,i_lateral,nnlat(1)) = 5._dp
   discharge_in(1,i_lateral,nnlat(2)) = 5._dp
   discharge_in(1,i_lateral,nnlat(3)) = 5._dp
   call add_lateral_load_and_sink(transport_load,transport_sink,discharge_in,discharge_out,vol1,tolerance)

   do i_const = 1,numconst
      do i_cell=1,ndxi
         dvoli = 1/(vol1(i_cell))
         refval = dvoli*incoming_lat_concentration(1,i_const,i_lateral)*discharge_in(1,i_lateral,i_cell)
         call assert_comparable(transport_load(i_const,i_cell),refval,tolerance,"lateral_load value is not correct" )
      end do
   end do
   call assert_comparable(sum(transport_sink), 0._dp, tolerance, "todo")

   ! check transport out of the domain
   i_lateral = 2 ! only the second lateral is a sink
   discharge_in(1,:,:) = 0._dp
   discharge_out(1,i_lateral,nnlat(4)) = -5._dp
   discharge_out(1,i_lateral,nnlat(5)) = -5._dp
   ! copy values of transport_load
   ref_load(:,:) = transport_load(:,:)
   call add_lateral_load_and_sink(transport_load,transport_sink,discharge_in,discharge_out,vol1,tolerance)
   ! check that transport_load was not changed
   call assert_comparable(sum(transport_load), sum(ref_load), tolerance, "todo")
   do i_const = 1,numconst
      do i_cell=1,ndxi
         dvoli = 1/(vol1(i_cell))
         refval = dvoli*discharge_out(1,i_lateral,i_cell)
         call assert_comparable(transport_sink(i_const,i_cell), refval, tolerance, "lateral_sink value is not correct" )
      end do
   end do
      
   deallocate(discharge_in)
   deallocate(discharge_out)
   deallocate(transport_load)
   deallocate(transport_sink)
 
end subroutine test_add_lateral_load_and_sink
!
!> initialize a domain with an incoming and an outgoing lateral for three constituents
subroutine setup_testcase()
   use m_flow, only: vol1, hs
   use m_flowtimes, only: dts
   use m_partitioninfo, only: jampi
   use m_transportdata, only: numconst
   use m_flowgeom, only: ndxi

   integer :: iostat                ! allocation status

   jampi = 0
   dts = 1.0e-3_dp
   ! domain of 10 points, 2 laterals (1 incoming with 3 nodes, 1 outgoing with 2 nodes)
   ! and 3 constituents. Volume (vol1) of each cell is set to 0.1d0.
   ! consider 3 constituents to represent salt, temperature and tracer transport. 
   ndxi = 10
   nlatnd = 5
   numlatsg = 2
   numconst = 3

   call initialize_lateraldata(numconst)
   allocate(n1latsg(numlatsg),stat=iostat)
   call aerr('n1latsg',iostat,numlatsg,'test_lateral, setup_testcase' )
   allocate(n2latsg(numlatsg),stat=iostat)
   call aerr('n2latsg',iostat,numlatsg,'test_lateral, setup_testcase' )
   allocate(apply_transport(numlatsg),stat=iostat)
   call aerr('apply_transport',iostat,numlatsg,'test_lateral, setup_testcase' )
   allocate(nnlat(nlatnd),stat=iostat)
   call aerr('nnlat',iostat,nlatnd,'test_lateral, setup_testcase' )
   allocate(qplat(1,nlatnd),stat=iostat)
   call aerr('qplat',iostat,nlatnd,'test_lateral, setup_testcase' )
   allocate(vol1(ndxi),stat=iostat)
   call aerr('vol1',iostat,ndxi,'test_lateral, setup_testcase' )
   allocate(hs(ndxi),stat=iostat)
   call aerr('hs',iostat,ndxi,'test_lateral, setup_testcase' )

   n1latsg(1) = 1
   n2latsg(1) = 3
   n1latsg(2) = 4
   n2latsg(2) = 5
   nnlat = (/1,2,3,5,8/)
   apply_transport(:)=1
   vol1(:) = 0.1_dp
   hs(:) = 2_dp

   ! positive qplat is considered inflow (source), negative value outflow (sink) 
   qplat(1,:) = (/9_dp,-10_dp/)
   ! top layer, per constituent, lateral 1, 
   incoming_lat_concentration(1,:,1) = (/31.0_dp,20.0_dp,0.23_dp/)  
   ! top layer, all constituents, lateral 2. 
   outgoing_lat_concentration(1,:,2) = 25_dp

end subroutine setup_testcase
!
!> reset to default values and deallocate arrays from other modules
subroutine finish_testcase()
   use m_flow, only: vol1, hs
   use m_partitioninfo, only: jampi
   use m_transportdata, only: numconst
   use m_flowgeom, only: ndxi

   jampi = 1
   ndxi = 0
   numconst = 0

   call reset_lateral()
   call dealloc_lateraldata()
   deallocate(n1latsg)
   deallocate(n2latsg)
   deallocate(apply_transport)
   deallocate(nnlat)
   deallocate(qplat)
   deallocate(vol1)
   deallocate(hs)

end subroutine finish_testcase
!
!==============================================================================
!> Test computation of water volume per layer for laterals.
!> This test assumes a model of dimension (nx,ny,nz) = (3,3,3).
!> In the last node, the model is shallow meaning it has only 2 active layers.
!> The model contains 2 laterals. 
subroutine test_get_lateral_volume_per_layer
   use m_flow, only: vol1, kbot, ktop, kmxn, kmx
   
   real(kind=dp), allocatable, dimension(:,:)   :: lateral_volume_per_layer  !< Water volume per layer in laterals, dimension = (number_of_layer,number_of_lateral) = (kmx,numlatsg)
   
   integer :: iostat
   integer :: ndx, ndkx
   integer :: i_cell
   
   ! specify number of computational cells, ndx
   ndx = 9
   
   ! initialize number of active layers for each node
   allocate(kmxn(ndx),stat=iostat)
   call aerr('kmxn',iostat,ndx,'test_get_lateral_volume_per_layer')
   kmxn = (/3,3,3,3,3,3,3,3,2/) ! the last cell is assumed shallow and contains only two layers

   ! initialize water volume per cell, vol1
   kmx = 3
   ndkx = ndx * (kmx + 1) - 1 ! one cell is shallow and contains only two layers
   allocate(vol1(ndkx),stat=iostat)
   call aerr('vol1',iostat,ndkx,'test_get_lateral_volume_per_layer')
   vol1(ndx+1:) = 1d0 ! only volume per cell, per layer is needed; the first ndx elements contain 2D volume (i.e. total volume over all layers) and are not needed for the function tested here, hence not set.
   
   ! initialize kbot and ktop
   allocate(kbot(ndx),stat=iostat)
   call aerr('kbot',iostat,ndx,'test_get_lateral_volume_per_layer')
   allocate(ktop(ndx),stat=iostat)
   call aerr('ktop',iostat,ndx,'test_get_lateral_volume_per_layer')
   kbot(1) = ndx + 1
   ktop(1) = kbot(1) + kmxn(1) - 1
   do i_cell = 2,ndx
      kbot(i_cell) = ktop(i_cell-1) + 1
      ktop(i_cell) = kbot(i_cell) + kmxn(i_cell) - 1
   end do
   
   ! initialize laterals administration
   numlatsg = 2
   allocate(n1latsg(numlatsg),stat=iostat)
   call aerr('n1latsg',iostat,numlatsg,'test_get_lateral_volume_per_layer')
   allocate(n2latsg(numlatsg),stat=iostat)
   call aerr('n2latsg',iostat,numlatsg,'test_get_lateral_volume_per_layer')
   allocate(nnlat(nlatnd),stat=iostat)
   call aerr('nnlat',iostat,nlatnd,'test_get_lateral_volume_per_layer')
   nnlat = (/1,2,8,9/)
   n1latsg = (/1,3/)
   n2latsg = (/2,4/)
   
   allocate(lateral_volume_per_layer(kmx, numlatsg),stat=iostat)
   call aerr('lateral_volume_per_layer',iostat,kmx*numlatsg,'test_get_lateral_volume_per_layer')

   call get_lateral_volume_per_layer(lateral_volume_per_layer)
   call assert_comparable(lateral_volume_per_layer(1,1), 2d0, tolerance, "get_lateral_volume_per_layer(1,1) output lateral_volume_per_layer is not correct" )
   call assert_comparable(lateral_volume_per_layer(2,1), 2d0, tolerance, "get_lateral_volume_per_layer(2,1) output lateral_volume_per_layer is not correct" )
   call assert_comparable(lateral_volume_per_layer(3,1), 2d0, tolerance, "get_lateral_volume_per_layer(3,1) output lateral_volume_per_layer is not correct" )
   call assert_comparable(lateral_volume_per_layer(1,2), 1d0, tolerance, "get_lateral_volume_per_layer(1,2) output lateral_volume_per_layer is not correct" )
   call assert_comparable(lateral_volume_per_layer(2,2), 2d0, tolerance, "get_lateral_volume_per_layer(2,2) output lateral_volume_per_layer is not correct" )
   call assert_comparable(lateral_volume_per_layer(3,2), 2d0, tolerance, "get_lateral_volume_per_layer(3,2) output lateral_volume_per_layer is not correct" )

   deallocate(kmxn)
   deallocate(vol1)
   deallocate(kbot)
   deallocate(ktop)
   deallocate(n1latsg)
   deallocate(n2latsg)
   deallocate(nnlat)
   deallocate(lateral_volume_per_layer)

end subroutine test_get_lateral_volume_per_layer

!==============================================================================
!> Test computation of distribution of lateral discharge per layer, which is retrieved from BMI,
!! to discharge per layer per cell.
!! This test assumes a model of dimension (nx,ny,nz) = (4,2,3). In another word, there are 4 nodes in x-direction, 2 nodes
!! in y-direction, so that ndx = 8.
!! There are 3 layers on all nodes, except for the 8th node which has only 2 active layers. So ndkx = (3+1)*8-1 = 31.
!! The model contains 2 lateralsL: the 1st lateral is on nodes 1 and 2, the 2nd lateral is on nodes 7 and 8.
subroutine test_distribute_lateral_discharge
   use m_flow,          only: vol1, kbot, ktop, kmxn, kmx
   use m_alloc,         only: realloc

   real(kind=dp), allocatable, dimension(:,:) :: lateral_discharge_per_layer_lateral_cell !< Discharge per layer per lateral per cell,
                                                                                        !! dimension=(number_of_layer,numlatsg,number_of_node)
                                                                                        !!          =(kmx,numlatsg,ndkx)

   integer :: ierr !< error flag
   integer :: ndx, ndkx
   integer :: i_node
   real(kind=dp), allocatable, dimension(:,:) :: provided_lateral_discharge

   ! Specify number of computational cells, ndx
   ndx = 8

   ! Initialize number of active layers for each cell
   call realloc(kmxn, ndx, stat=ierr, keepExisting=.false., fill=0)
   call aerr('kmxn', ierr, ndx, 'test_distribute_lateral_discharge_per_layer_per_cell')
   kmxn = [3,3,3,3,3,3,3,2] ! The 8th cell is assumed shallow and contains only two layers

   ! Initialize water volume per cell per layer, vol1.
   kmx = 3
   ndkx = ndx * (kmx + 1) - 1 ! one cell is shallow and contains only two layers
   call realloc(vol1, ndkx, stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('vol1', ierr, ndkx, 'test_distribute_lateral_discharge_per_layer_per_cell')

   ! vol1 (ndx+1:ndkx) = [19, 20, 21, ..., 41].
   do i_node = ndx+1, ndkx
      vol1(i_node) = 10 + i_node ! only volume per cell, per layer is needed; the first ndx elements contain 2D volume
                                 ! (i.e. total volume over all layers) and are not needed for the function tested here,
                                 ! hence not set.
   end do

   ! Initialize kbot and ktop
   call realloc(kbot, ndx, stat=ierr, keepExisting=.false., fill=0)
   call aerr('kbot', ierr, ndx, 'test_distribute_lateral_discharge_per_layer_per_cell')

   call realloc(ktop, ndx, stat=ierr, keepExisting=.false., fill=0)
   call aerr('ktop', ierr, ndx, 'test_distribute_lateral_discharge_per_layer_per_cell')

   kbot(1) = ndx + 1
   ktop(1) = kbot(1) + kmxn(1) - 1
   do i_node = 2,ndx
      kbot(i_node) = ktop(i_node-1) + 1
      ktop(i_node) = kbot(i_node) + kmxn(i_node) - 1
   enddo

   ! Initialize laterals administration
   numlatsg = 2
   nlatnd = 4
   call realloc(apply_transport, numlatsg, stat=ierr, keepExisting=.false., fill=1)
   call aerr('apply_transport', ierr, numlatsg, 'test_distribute_lateral_discharge_per_layer_per_cell')

   call realloc(n1latsg, numlatsg, stat=ierr, keepExisting=.false., fill=0)
   call aerr('n1latsg', ierr, numlatsg, 'test_distribute_lateral_discharge_per_layer_per_cell')

   call realloc(n2latsg, numlatsg, stat=ierr, keepExisting=.false., fill=0)
   call aerr('n2latsg', ierr, numlatsg, 'test_distribute_lateral_discharge_per_layer_per_cell')

   call realloc(nnlat,   nlatnd,   stat=ierr, keepExisting=.false., fill=0)
   call aerr('nnlat', ierr, nlatnd, 'test_distribute_lateral_discharge_per_layer_per_cell')

   ! The 1st lateral is on nodes 1 and 2, the 2nd lateral is on nodes 7 and 8.
   nnlat   = [1,2,7,8]
   n1latsg = [1,3]
   n2latsg = [2,4]

   ! Initialize lateral volume per layer and discharge per layer.
   call realloc(lateral_volume_per_layer, [kmx, numlatsg], stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('lateral_volume_per_layer', ierr, kmx*numlatsg, 'test_distribute_lateral_discharge_per_layer_per_cell')
   call realloc(provided_lateral_discharge, [kmx, numlatsg], stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('provided_lateral_discharge_per_layer', ierr, kmx*numlatsg, 'test_distribute_lateral_discharge_per_layer_per_cell')
   call realloc(lateral_discharge_per_layer_lateral_cell, [kmx, nlatnd], stat=ierr, keepExisting=.false., fill=0d0)
   call aerr('lateral_discharge_per_layer_per_cell', ierr, kmx*numlatsg*ndkx, 'test_distribute_lateral_discharge_per_layer_per_cell')

   lateral_volume_per_layer(1:kmx,1) = [100, 300, 250] ! Volume for the 1st lateral per layer
   lateral_volume_per_layer(1:kmx,2) = [200, 250, 330] ! Volume for the 2nd lateral per layer
   provided_lateral_discharge(1:kmx,1) = [1000, 1500, 2800] ! Discharge for the 1st lateral per layer
   provided_lateral_discharge(1:kmx,2) = [2000, 3000, 2500] ! Discharge for the 2nd lateral per layer

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
   
   ! Deallocate
   if (allocated(kmxn)) then
      deallocate(kmxn)
   end if
   if (allocated(vol1)) then
      deallocate(vol1)
   end if
   if (allocated(kbot)) then
      deallocate(kbot)
   end if
   if (allocated(ktop)) then
      deallocate(ktop)
   end if
   if (allocated(apply_transport)) then
      deallocate(apply_transport)
   end if
   if (allocated(n1latsg)) then
      deallocate(n1latsg)
   end if
   if (allocated(n2latsg)) then
      deallocate(n2latsg)
   end if
   if (allocated(nnlat)) then
      deallocate(nnlat)
   end if
   if (allocated(lateral_volume_per_layer)) then
      deallocate(lateral_volume_per_layer)
   end if
   if (allocated(provided_lateral_discharge)) then
      deallocate(provided_lateral_discharge)
   end if
   if (allocated(lateral_discharge_per_layer_lateral_cell)) then
      deallocate(lateral_discharge_per_layer_lateral_cell)
   end if

end subroutine test_distribute_lateral_discharge

end module test_lateral
