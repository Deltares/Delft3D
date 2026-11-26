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
   use assertions_gtest, only: f90_expect_true, f90_expect_false, f90_expect_streq, f90_expect_relative_near
   use precision, only: dp
   use m_read_property, only: read_property
   use properties, only: prop_set
   use tree_structures, only: tree_data, tree_create, tree_destroy

   implicit none(type, external)
   private

contains

   !$gtest
   subroutine test_generalstructure_2d3d() bind(C)
      character(len=*), parameter :: key = 'GateLowerEdgeLevel'
      character(len=40), dimension(4), parameter :: paths = [character(len=40) :: &
                                                             'filename.tim', '../filename.tim', 'c:\dirname\0000\filename.tim', '/home/usr/UNST_5890/filename.tim']
      type(tree_data), pointer :: block
      character(len=256) :: string_value
      real(kind=dp) :: numeric_value
      logical :: is_double, success
      integer :: path_index

      string_value = ''
      numeric_value = 0.0_dp
      call tree_create('generalstructure', block)
      call prop_set(block, '', 'type', 'generalstructure')
      call prop_set(block, '', 'id', 'full_block')
      call prop_set(block, '', key, '1.0')

      call read_property(block, 'type', string_value, numeric_value, is_double, 'generalstructure', success)
      call f90_expect_true(success, 'Structure type exists')
      call f90_expect_streq(trim(string_value), 'generalstructure', 'Structure type')
      call read_property(block, 'id', string_value, numeric_value, is_double, 'generalstructure', success)
      call f90_expect_true(success, 'Structure ID exists')
      call f90_expect_false(is_double, 'Structure ID is a string')
      call f90_expect_streq(trim(string_value), 'full_block', 'Structure ID')
      call read_property(block, 'id_nonexistent', string_value, numeric_value, is_double, 'generalstructure', success)
      call f90_expect_false(success, 'A missing property should not be read')

      call read_property(block, key, string_value, numeric_value, is_double, 'generalstructure', success)
      call f90_expect_true(success, 'Numeric gate level exists')
      call f90_expect_true(is_double, 'A numeric gate level should be parsed as a double')
      if (is_double) then
         call f90_expect_relative_near(numeric_value, 1.0_dp, 1.0e-12_dp, 'Numeric gate level')
      end if

      call tree_destroy(block)

      do path_index = 1, size(paths)
         call tree_create('generalstructure', block)
         call prop_set(block, '', key, trim(paths(path_index)))
         call read_property(block, key, string_value, numeric_value, is_double, 'generalstructure', success)
         call f90_expect_true(success, 'Gate-level path exists')
         call f90_expect_false(is_double, 'A gate-level path should stay a string')
         call f90_expect_streq(trim(string_value), trim(paths(path_index)), 'Gate-level path')
         call tree_destroy(block)
      end do
   end subroutine test_generalstructure_2d3d

end module test_read_property
