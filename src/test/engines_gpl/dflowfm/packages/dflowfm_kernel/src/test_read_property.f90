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
module test_read_property
   use ftnunit
   use precision
   use m_read_property, only: read_property

   implicit none
   real(fp), parameter :: eps = 1.0e-6_fp

contains
!
!
!==============================================================================
   subroutine tests_read_property
      call test(test_generalstructure_2d3d, 'Test reading properties for generalstructure from structure INI file, 2D3D format.')
   end subroutine tests_read_property
!
!
!==============================================================================
!> tests reading of blocks for generalstructure in 2D3D format
   subroutine test_generalstructure_2d3d
      use dfm_error
      use ifport, only: CHANGEDIRQQ
      use properties
      use mathconsts, only: eps_hp
      use m_strucs

      integer :: ierr
      integer :: numblocks
      integer :: i

      type(TREE_DATA), pointer :: str_ptr !< Property tree as read from structures.ini
      character(len=256) :: strvalue !< Returned string value for requested property key.
      character(len=256) :: idvalue !< block idententy string
      character(len=32) :: structurefile !< filename with testdata.
      double precision :: dblvalue !< Returned scalar double value for requested property key, IF possible.
      logical :: is_double !< Tells whether the found value could be parsed into a scalar double value.
      logical :: success !< Whether value was read successfully or not.

      structurefile = "structures.ini"
      success = CHANGEDIRQQ("structures")

      call tree_create(trim(structurefile), str_ptr)
      call prop_inifile(structurefile, str_ptr, ierr)
      call assert_equal(ierr, DFM_NOERR, 'Error reading structure file.')
      success = CHANGEDIRQQ("..")

      numblocks = tree_num_nodes(str_ptr)
      call assert_equal(numblocks, 5, 'File structures/structures.ini is expected to contain 5 blocks.')

      ! read required properties from first block
      call read_property(str_ptr%child_nodes(1)%node_ptr, "id", strvalue, dblvalue, is_double, 'first block structures.ini', success)
      call assert_equal(success, .true., "Something is wrong when reading 'id'.")
      call assert_equal(is_double, .false., "Expected a string.")
      call assert_equal(strvalue, "full_block", "Another value for 'id' was expected.")

      ! try to read a non existing key and check that the return value is .false.
      call read_property(str_ptr%child_nodes(1)%node_ptr, "id_nonexistent", strvalue, dblvalue, is_double, 'first block structures.ini', success)
      call assert_equal(success, .false., "Something is wrong when reading 'id_nonexistent'.")

      do i = 1, numblocks
         associate (node => str_ptr%child_nodes(i)%node_ptr)
            call read_property(node, "type", strvalue, dblvalue, is_double, 'block structures.ini', success)
            call assert_equal(success, .true., "Something is wrong when reading 'type'.")
            call assert_equal(strvalue, 'generalstructure', 'Type "generalstructures" was expected.')

            call read_property(node, "id", idvalue, dblvalue, is_double, 'block structures.ini', success)
            call assert_equal(success, .true., "Something is wrong when reading 'id'.")

            ! for each block read GateLowerEdgeLevel either as string or as double
            call read_property(node, 'GateLowerEdgeLevel', strvalue, dblvalue, is_double, 'first block structures.ini', success)
         end associate
         if (success) then
            select case (trim(idvalue))
            case ('full_block')
               call assert_equal(is_double, .true., "Block 'full_block' in structures.ini: expected a value.")
               call assert_comparable(dblvalue, 1.d0, eps_hp, "Read GateLowerEdgeLevel as a value.")
            case ('local')
               call assert_equal(is_double, .false., "Block 'local': expected a string.")
               call assert_equal(strvalue, 'filename.tim', 'Unexpected string.')
            case ('relative')
               call assert_equal(is_double, .false., "Block 'relative': expected a string.")
               call assert_equal(strvalue, '../filename.tim', 'Unexpected string.')
            case ('windows')
               call assert_equal(is_double, .false., "Block 'windows': expected a string.")
               call assert_equal(strvalue, 'c:\dirname\0000\filename.tim', 'Unexpected string.')
            case ('linux')
               call assert_equal(is_double, .false., "Block 'linux' in structures.ini: expected a string.")
               call assert_equal(strvalue, '/home/usr/UNST_5890/filename.tim', "/home/UNST_5890/filename.tim: Unexpected string.")
            end select
         else
            ! raise an error message when .not. success
            call assert_equal(success, .true., "Something is wrong when reading 'GateLowerEdgeLevel'.")
         end if
      end do

   end subroutine test_generalstructure_2d3d

end module test_read_property
