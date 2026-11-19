!!  Copyright (C)  Stichting Deltares, 2012-2026.
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
   use assertions_gtest, only: f90_assert_true, f90_expect_streq, f90_expect_relative_near
   use precision

   implicit none(type, external)
   private
   real(fp), parameter :: eps = 1.0e-6_fp

contains
!
!
!==============================================================================
!
!
!==============================================================================
   !$gtest
   subroutine test_storage_nodes_nodeID_table() bind(C)
      use gridoperations
      use m_commandline_option, only: iarg_usecaching
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
      integer, parameter :: N_StorageNodes = 1
      logical :: success
      real(kind=dp), dimension(3, 2), parameter :: reftable = reshape([ &
                                                                      1.0_dp, 2.0_dp, 3.0_dp, 0.1_dp, 0.5_dp, 0.8_dp], [3, 2])
      character(len=40), dimension(N_StorageNodes) :: refids
      character(len=40), dimension(N_StorageNodes) :: refnodeIds
      character(len=40) :: mdufile

      ! reference: id, nodeId, storageArea table of the storage node
      refids(1) = 'storageNode1'
      refnodeIds(1) = 'T2_ConNode'
      !
      ! Body
      jampi = 0
      iarg_usecaching = 0
      kmax = 2
      lmax = 2
      numk = 0
      call increaseNetw(kmax, lmax)
      call resetFullFlowModel()
      !
      success = CHANGEDIRQQ("storage_nodes/node_id_table")
      if (.not. f90_assert_true(success, 'Could not change to directory ''storage_nodes/node_id_table''.')) then
         return
      end if
      mdufile = 'Flow1D_table.mdu'
      call loadModel(mdufile) ! storage nodes are read in this subroutine

      success = CHANGEDIRQQ("../..")
      if (.not. f90_assert_true(success, 'Could not restore the working directory from ''storage_nodes/node_id_table''.')) then
         return
      end if
      ! compare
      call f90_expect_streq(trim(network%storS%stor(1)%id), trim(refids(1)), 'Id of the storage node incorrect')
      call f90_expect_streq(trim(network%storS%stor(1)%node_Id), trim(refnodeIds(1)), 'Node Id of the storage node incorrect')
      call f90_expect_relative_near(network%storS%stor(1)%storage_area%x(1), reftable(1, 1), eps, 'the first value of "levels" incorrect')
      call f90_expect_relative_near(network%storS%stor(1)%storage_area%x(2), reftable(2, 1), eps, 'the second value of "levels" incorrect')
      call f90_expect_relative_near(network%storS%stor(1)%storage_area%x(3), reftable(3, 1), eps, 'the third value of "levels" incorrect')

      call f90_expect_relative_near(network%storS%stor(1)%storage_area%y(1), reftable(1, 2), eps, 'the first value of "storageArea" incorrect')
      call f90_expect_relative_near(network%storS%stor(1)%storage_area%y(2), reftable(2, 2), eps, 'the second value of storageArea incorrect')
      call f90_expect_relative_near(network%storS%stor(1)%storage_area%y(3), reftable(3, 2), eps, 'the third value of storageArea incorrect')

   end subroutine test_storage_nodes_nodeID_table

end module test_storage_nodes
