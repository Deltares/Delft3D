!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module test_generate_partition_mdu_file
   use assertions_gtest
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_generate_partition_mdu_file, test_empty_classmapfile_stays_empty, test_empty_classmapfile_stays_empty,
   !> Regression test for UNST-10193: an empty ClassMapFile must remain empty after partitioning, instead of being
   !> filled with MapFile's value (which used to happen because 'classmapfile' contains the substring 'mapfile').
   subroutine test_empty_classmapfile_stays_empty() bind(C)
      use m_generatepartitionmdufile, only: generate_partition_mdu_file
      use unstruc_model, only: md_mapfile, md_classmap_file
      use string_module, only: str_lower

      character(len=*), parameter :: INPUT_FILE = 'test_input_partition.mdu'
      character(len=*), parameter :: OUTPUT_FILE = 'test_output_partition.mdu'
      character(len=*), parameter :: MAP_FILE_VALUE = 'test_map_0001.nc'

      integer :: unit_input, unit_output, stat
      integer :: equal_pos, comment_pos
      character(len=500) :: line, line_lower, value_str
      logical :: found_classmapfile, found_mapfile

      ! Arrange: simulate a model where MapFile has a value, but ClassMapFile is left empty (as in UNST-10193)
      md_mapfile = MAP_FILE_VALUE
      md_classmap_file = ' '

      open (newunit=unit_input, file=INPUT_FILE, status='replace', action='write')
      write (unit_input, '(a)') 'MapFile                           = '//trim(MAP_FILE_VALUE)//'        # Map file *_map.nc'
      write (unit_input, '(a)') 'ClassMapFile                      =                     # Class map file *_clm.nc'
      close (unit_input)

      ! Act
      call generate_partition_mdu_file(INPUT_FILE, OUTPUT_FILE)

      ! Assert: read back the generated partition file and check that ClassMapFile remained empty
      found_classmapfile = .false.
      found_mapfile = .false.

      open (newunit=unit_output, file=OUTPUT_FILE, status='old', action='read', iostat=stat)
      call f90_assert_eq(stat, 0, 'Error opening generated partition MDU file.')

      do
         read (unit_output, '(a)', iostat=stat) line
         if (stat /= 0) exit

         line_lower = line
         call str_lower(line_lower)

         comment_pos = index(line_lower, '#')
         equal_pos = index(line_lower, '=')
         if (equal_pos == 0) cycle

         if (comment_pos > 0) then
            value_str = adjustl(line(equal_pos + 1:comment_pos - 1))
         else
            value_str = adjustl(line(equal_pos + 1:))
         end if

         if (index(line_lower, 'classmapfile') /= 0) then
            found_classmapfile = .true.
            call f90_expect_eq(len_trim(value_str), 0, 'ClassMapFile was erroneously filled with a value during partitioning.')
         else if (index(line_lower, 'mapfile') /= 0) then
            found_mapfile = .true.
            call f90_expect_true(index(value_str, trim(MAP_FILE_VALUE)) == 1, 'MapFile was not correctly replaced during partitioning.')
         end if
      end do
      close (unit_output, status='delete')

      call f90_assert_true(found_classmapfile, 'ClassMapFile line missing from generated partition MDU file.')
      call f90_assert_true(found_mapfile, 'MapFile line missing from generated partition MDU file.')

      open (newunit=unit_input, file=INPUT_FILE, status='old', action='read')
      close (unit_input, status='delete')
   end subroutine test_empty_classmapfile_stays_empty
   !$f90tw)

end module test_generate_partition_mdu_file
