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
   use assertions_gtest, only: f90_assert_eq, f90_assert_true, f90_expect_eq, f90_expect_streq, f90_expect_relative_near
   use precision, only: dp
   implicit none(type, external)
   private

contains

   !$gtest
   subroutine test_storage_nodes_nodeID_table() bind(C)
      use m_network, only: t_network
      use m_node, only: fill_hashtable, t_node, nt_EndNode, dealloc
      use m_storage, only: dealloc
      use m_readStorageNodes, only: readStorageNodes

      type(t_network) :: storage_network
      character(len=*), parameter :: node_id = 'T2_ConNode'
      real(kind=dp), dimension(3), parameter :: levels = [1.0_dp, 2.0_dp, 3.0_dp]
      real(kind=dp), dimension(3), parameter :: areas = [0.1_dp, 0.5_dp, 0.8_dp]
      real(kind=dp), parameter :: tolerance = 1.0e-6_dp

      allocate (storage_network%nds%node(1))
      storage_network%nds%node(1) = t_node(node_id, node_id, 1, nt_EndNode, 0.0_dp, 0.0_dp, 1, 1)
      storage_network%nds%count = 1
      storage_network%nds%size = 1
      call fill_hashtable(storage_network%nds)
      nullify (storage_network%storS%stor)
      storage_network%storS%grows_by = 1

      call readStorageNodes(storage_network, 'storage_nodes/node_id_table/storageNodes_table.ini')

      if (f90_assert_eq(storage_network%storS%count, 1, 'Expected one storage node')) then
         associate (storage => storage_network%storS%stor(1))
            call f90_expect_streq(trim(storage%id), 'storageNode1', 'Storage node ID')
            call f90_expect_streq(trim(storage%node_id), node_id, 'Connection node ID')
            call f90_expect_eq(storage%node_index, 1, 'Connection node index')
            if (f90_assert_true(associated(storage%storage_area), 'Expected a storage area table')) then
               call f90_expect_relative_near(storage%storage_area%x(1:storage%storage_area%length), levels, tolerance, 'Storage levels')
               call f90_expect_relative_near(storage%storage_area%y(1:storage%storage_area%length), areas, tolerance, 'Storage areas')
            end if
         end associate
      end if
      call dealloc(storage_network%nds)
      call dealloc(storage_network%storS)
   end subroutine test_storage_nodes_nodeID_table

end module test_storage_nodes
