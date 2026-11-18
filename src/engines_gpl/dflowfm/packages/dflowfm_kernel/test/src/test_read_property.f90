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
module test_read_property
   use assertions_gtest, only: f90_expect_eq, f90_expect_streq
   use test_relative_assertions, only: expect_relative_near
   use precision
   use m_read_property, only: read_property

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
!> tests reading of blocks for generalstructure in 2D3D format
   !$gtest
   subroutine test_generalstructure_2d3d() bind(C)
      use dfm_error
      use ifport, only: CHANGEDIRQQ
      use properties
      use m_mathconstants, only: eps_hp
      use m_strucs

      integer :: ierr
      integer :: numblocks
      integer :: i

      type(TREE_DATA), pointer :: str_ptr !< Property tree as read from structures.ini
      character(len=256) :: strvalue !< Returned string value for requested property key.
      character(len=256) :: idvalue !< block idententy string
      character(len=32) :: structurefile !< filename with testdata.
      real(kind=dp) :: dblvalue !< Returned scalar double value for requested property key, IF possible.
      logical :: is_double !< Tells whether the found value could be parsed into a scalar double value.
      logical :: success !< Whether value was read successfully or not.

      structurefile = "structures.ini"
      success = CHANGEDIRQQ("structures")

      call tree_create(trim(structurefile), str_ptr)
      call prop_inifile(structurefile, str_ptr, ierr)
      call f90_expect_eq(ierr, DFM_NOERR, 'Error reading structure file.')
      success = CHANGEDIRQQ("..")

      numblocks = tree_num_nodes(str_ptr)
      call f90_expect_eq(numblocks, 5, 'File structures/structures.ini is expected to contain 5 blocks.')

      ! read required properties from first block
      call read_property(str_ptr%child_nodes(1)%node_ptr, "id", strvalue, dblvalue, is_double, 'first block structures.ini', success)
      call f90_expect_eq(success, .true., "Something is wrong when reading 'id'.")
      call f90_expect_eq(is_double, .false., "Expected a string.")
      call f90_expect_streq(trim(strvalue), trim("full_block"), "Another value for 'id' was expected.")

      ! try to read a non existing key and check that the return value is .false.
      call read_property(str_ptr%child_nodes(1)%node_ptr, "id_nonexistent", strvalue, dblvalue, is_double, 'first block structures.ini', success)
      call f90_expect_eq(success, .false., "Something is wrong when reading 'id_nonexistent'.")

      do i = 1, numblocks
         associate (node => str_ptr%child_nodes(i)%node_ptr)
            call read_property(node, "type", strvalue, dblvalue, is_double, 'block structures.ini', success)
            call f90_expect_eq(success, .true., "Something is wrong when reading 'type'.")
            call f90_expect_streq(trim(strvalue), trim('generalstructure'), 'Type "generalstructures" was expected.')

            call read_property(node, "id", idvalue, dblvalue, is_double, 'block structures.ini', success)
            call f90_expect_eq(success, .true., "Something is wrong when reading 'id'.")

            ! for each block read GateLowerEdgeLevel either as string or as double
            call read_property(node, 'GateLowerEdgeLevel', strvalue, dblvalue, is_double, 'first block structures.ini', success)
         end associate
         if (success) then
            select case (trim(idvalue))
            case ('full_block')
               call f90_expect_eq(is_double, .true., "Block 'full_block' in structures.ini: expected a value.")
               call expect_relative_near(dblvalue, 1.0_dp, eps_hp, "Read GateLowerEdgeLevel as a value.")
            case ('local')
               call f90_expect_eq(is_double, .false., "Block 'local': expected a string.")
               call f90_expect_streq(trim(strvalue), trim('filename.tim'), 'Unexpected string.')
            case ('relative')
               call f90_expect_eq(is_double, .false., "Block 'relative': expected a string.")
               call f90_expect_streq(trim(strvalue), trim('../filename.tim'), 'Unexpected string.')
            case ('windows')
               call f90_expect_eq(is_double, .false., "Block 'windows': expected a string.")
               call f90_expect_streq(trim(strvalue), trim('c:\dirname\0000\filename.tim'), 'Unexpected string.')
            case ('linux')
               call f90_expect_eq(is_double, .false., "Block 'linux' in structures.ini: expected a string.")
               call f90_expect_streq(trim(strvalue), trim('/home/usr/UNST_5890/filename.tim'), "/home/UNST_5890/filename.tim: Unexpected string.")
            end select
         else
            ! raise an error message when .not. success
            call f90_expect_eq(success, .true., "Something is wrong when reading 'GateLowerEdgeLevel'.")
         end if
      end do

   end subroutine test_generalstructure_2d3d

end module test_read_property
