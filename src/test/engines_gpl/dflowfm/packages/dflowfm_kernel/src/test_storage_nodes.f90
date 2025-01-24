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
module test_storage_nodes
    use ftnunit
    use precision

    implicit none
    real(fp), parameter :: eps = 1.0e-6_fp

contains
!
!
!==============================================================================
subroutine tests_storageNodes
    !call test( test_storage_nodes_nodeID_table, 'Tests the reading storage nodes.' )
end subroutine tests_storageNodes
!
!
!==============================================================================
subroutine test_storage_nodes_nodeID_table
    use gridoperations
    use m_netw
    use m_readStorageNodes
    use m_Storage
    use unstruc_channel_flow
    use unstruc_model
    use m_partitioninfo, only: jampi
    use network_data, only: numk
    use ifport
    use m_resetfullflowmodel, only: resetfullflowmodel
    !
    ! Locals
    integer, parameter                           :: N_StorageNodes = 1
    logical                                      :: success
    double precision                             :: reftable(3,2)
    character(len=40), dimension(N_StorageNodes) :: refids
    character(len=40), dimension(N_StorageNodes) :: refnodeIds
    character(len=40)                            :: mdufile
    
   
    ! reference: id, nodeId, storageArea table of the storage node
    refids(1) = 'storageNode1'
    refnodeIds(1) = 'T2_ConNode'
    data reftable /  1,   2,   3, &
                   0.1, 0.5, 0.8  /
    !
    ! Body
    jampi = 0
    kmax  = 2
    lmax  = 2
    numk  = 0
    call increaseNetw(kmax, lmax)
    call resetFullFlowModel()
    !
    success = CHANGEDIRQQ("storageNodes_nodeId_useTable")
    mdufile = 'Flow1D_table.mdu'
    call loadModel(mdufile) ! storage nodes are read in this subroutine

    success = CHANGEDIRQQ("..")
    ! compare
    call assert_equal     (network%storS%stor(1)%id,     refids(1),     'Id of the storage node incorrect' )
    call assert_equal     (network%storS%stor(1)%node_Id, refnodeIds(1), 'Node Id of the storage node incorrect' )
    call assert_comparable(network%storS%stor(1)%storage_area%x(1), reftable(1,1), eps, 'the first value of "levels" incorrect' )
    call assert_comparable(network%storS%stor(1)%storage_area%x(2), reftable(2,1), eps, 'the second value of "levels" incorrect' )
    call assert_comparable(network%storS%stor(1)%storage_area%x(3), reftable(3,1), eps, 'the third value of "levels" incorrect' )
    
    call assert_comparable(network%storS%stor(1)%storage_area%y(1), reftable(1,2), eps, 'the first value of "storageArea" incorrect' )
    call assert_comparable(network%storS%stor(1)%storage_area%y(2), reftable(2,2), eps, 'the second value of storageArea incorrect' )
    call assert_comparable(network%storS%stor(1)%storage_area%y(3), reftable(3,2), eps, 'the third value of storageArea incorrect' )
    
end subroutine test_storage_nodes_nodeID_table

end module test_storage_nodes
