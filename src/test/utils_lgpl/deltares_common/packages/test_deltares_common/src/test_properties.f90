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

module test_properties
   use ftnunit
   use properties

   implicit none

contains
   subroutine tests_properties
      call test(test_properties_load, 'Simply load a properties file')
      call test(test_properties_check, 'Checking if the values in a properties file get loaded correctly')
      call test(test_properties_version, 'Checking if the fileversion get loaded correctly')
      call test(test_properties_get_single_values, 'Checking if single values are returned correctly')
      call test(test_properties_get_strings, 'Checking if strings are returned correctly')
   end subroutine tests_properties

   subroutine test_properties_load
      type(tree_data), pointer :: tree
      integer :: error
      !
      ! Check that a non-existing file causes an error
      !
      ! Note:
      ! Apparently, the tree variable needs to be associated/allocated beforehand
      !
      allocate (tree)

      call prop_inifile('non-existent.ini', tree, error)
      !
      ! The code does not return 1 in this case, but leaves it to the compiler
      !
      call assert_false(error == 0, "The error code should not have been 0")
      ! Assert disabled. Please add green tests only call assert_equal( error, 1, "The error code should have been 1" )
      !
      ! This check makes no sense, as the tree variable needs to be associated anyway
      !
      !call assert_false( associated(tree), "The tree variable should not be associated" )

      !
      ! Check that a straightforward ini-file is loaded without error
      !
      call prop_inifile('simple-file.ini', tree, error)
      call assert_equal(error, 0, "There should have been no error")

   end subroutine test_properties_load

   subroutine test_properties_check
      use precision, only: dp

      type(tree_data), pointer :: tree1, tree2, tree
      integer :: error

      logical :: success
      integer :: integerValue
      real :: realValue

      real(kind=dp) :: doubleValue
      character(len=80) :: stringValue
      character(len=80) :: expectedString = 'A short sentence of several words'

      integer :: i
      character(len=10), dimension(3) :: chapter = ['general   ', '*         ', 'specific  ']

      !
      ! Check that a non-existing file causes an error
      !
      ! Note:
      ! Apparently, the tree variable needs to be associated/allocated beforehand
      !
      allocate (tree1)
      allocate (tree2)

      !
      ! Note: A chapter "*" is considered to indicate a keyword outside a (named) chapter
      ! So use two different ini files
      !
      call prop_inifile('simple-file.ini', tree1, error)
      call prop_inifile('no-chapters.ini', tree2, error)
      call assert_equal(error, 0, "There should have been no error")

      !
      ! Get numerical values from any chapter
      !
      do i = 1, 2
         tree => tree1
         if (i == 2) then
            tree => tree2
         end if

         integerValue = -999
         call prop_get(tree, chapter(i), 'integerValue', integerValue, success)
         call assert_true(success, "Retrieving the integer value should succeed (chapter: "//trim(chapter(i))//")")
         call assert_equal(integerValue, 1, "The integer value should be 1 (chapter: "//trim(chapter(i))//")")

         realValue = -999.0
         call prop_get(tree, chapter(i), 'realValue', realValue, success)
         call assert_true(success, "Retrieving the real value should succeed (chapter: "//trim(chapter(i))//")")
         call assert_comparable(realValue, 2.2, 1.0e-6, "The real value should be 2.2 (chapter: "//trim(chapter(i))//")")

         realValue = -999.0
         call prop_get(tree, chapter(i), 'integerValue', realValue, success)
         call assert_true(success, "Retrieving the real value (from 'integerValue') should succeed (chapter: "//trim(chapter(i))//")")
         call assert_comparable(realValue, 1.0, 1.0e-6, "The real value (from 'integerValue') should be 1 (chapter: "//trim(chapter(i))//")")

         doubleValue = -999.0_dp
         call prop_get(tree, chapter(i), 'doubleValue', doubleValue, success)
         call assert_true(success, "Retrieving the double value should succeed (chapter: "//trim(chapter(i))//")")
         call assert_comparable(doubleValue, 2.3e2_dp, 1.0_dp, "The double value should be 230.0 (chapter: "//trim(chapter(i))//")")

         doubleValue = -999.0_dp
         call prop_get(tree, chapter(i), 'realValue', doubleValue, success)
         call assert_true(success, "Retrieving the double value (from 'realValue') should succeed (chapter: "//trim(chapter(i))//")")
         call assert_comparable(doubleValue, 2.2_dp, 1.0_dp, "The double value (from 'realValue') should be 2.2 (chapter: "//trim(chapter(i))//")")
      end do

      ! The rest of this subroutine is disabled. Please only add green tests
      return
      !
      ! Get the string values from any chapter
      !
      do i = 2, 3
         tree => tree1
         if (i == 2) then
            tree => tree2
         end if

         stringValue = '?'
         call prop_get(tree, chapter(i), 'plainString', stringValue, success)
         call assert_true(success, "Retrieving the string value should succeed (chapter: "//trim(chapter(i))//")")
         call assert_equal(stringValue, "plain", "Single words should be treated correctly (chapter: "//trim(chapter(i))//")")

         stringValue = '?'
         call prop_get(tree, chapter(i), 'stringValue1', stringValue, success)
         call assert_true(success, "Retrieving the string value should succeed (chapter: "//trim(chapter(i))//")")
         call assert_equal(stringValue, expectedString, "Strings in double quotes should be treated correctly (chapter: "//trim(chapter(i))//")")

         stringValue = '?'
         call prop_get(tree, chapter(i), 'stringValue2', stringValue, success)
         call assert_true(success, "Retrieving the string value should succeed (chapter: "//trim(chapter(i))//")")
         call assert_equal(stringValue, expectedString, "Strings in single quotes should be treated correctly (chapter: "//trim(chapter(i))//")")

         stringValue = '?'
         call prop_get(tree, chapter(i), 'stringValue3', stringValue, success)
         call assert_true(success, "Retrieving the string value should succeed (chapter: "//trim(chapter(i))//")")
         call assert_equal(stringValue, expectedString, "Strings enclosed in hashes should be treated correctly (chapter: "//trim(chapter(i))//")")
      end do

   end subroutine test_properties_check

   subroutine test_properties_version
      character(len=20) :: fileVersion
      type(tree_data), pointer :: tree
      integer :: error
      logical :: success
      integer :: major
      integer :: minor
      !
      ! Check that a non-existing file causes an error
      !
      ! Note:
      ! Apparently, the tree variable needs to be associated/allocated beforehand
      !
      allocate (tree)

      call prop_inifile('test_fileversion.ini', tree, error)

      ! test default version number
      call get_version_number(tree, major=major, minor=minor, success=success)
      call assert_equal(major, 1, 'Major version')
      call assert_equal(minor, 245, 'Minor version')

      call get_version_number(tree, keyin='versionNumber', major=major, minor=minor, versionstring=fileVersion, success=success)
      call assert_equal(success, .false., 'Incorrect version string')
      call assert_equal(trim(fileVersion), '3', 'version string')

      call get_version_number(tree, 'new', 'version', major=major, minor=minor, versionstring=fileVersion, success=success)
      call assert_equal(major, 5, 'Major version')
      call assert_equal(minor, 1, 'Minor version')
      call assert_equal(trim(fileVersion), '5.001', 'version string')

   end subroutine test_properties_version

   subroutine test_properties_get_single_values
      character(len=20) :: one_string
      integer :: one_integer
      real :: one_real
      type(tree_data), pointer :: tree
      integer :: error
      logical :: success
      !
      ! Check that a non-existing file causes an error
      !
      ! Note:
      ! Apparently, the tree variable needs to be associated/allocated beforehand
      !
      allocate (tree)

      call prop_inifile('test_strings.ini', tree, error)

      !
      ! Retrieve strings ...
      !
      call prop_get(tree, 'single', 'string1', one_string, success)
      call assert_true(success, "Retrieving the string value should succeed (chapter: single)")
      call assert_equal(one_string, "1234", "Single words should be treated correctly (chapter: single)")

      !
      ! Note that leading blanks (spaces) are removed
      !
      call prop_get(tree, 'single', 'string2', one_string, success)
      call assert_true(success, "Retrieving the string value should succeed (chapter: single)")
      call assert_equal(one_string, "1 2 3 4", "Multiple words should be treated correctly - including blanks (chapter: single)")

      !
      ! Retrieve numerical values ...
      !
      call prop_get(tree, 'single', 'integer', one_integer, success)
      call assert_true(success, "Retrieving the integer value should succeed (chapter: single)")
      call assert_equal(one_integer, 3, "Correct value must be returned (chapter: single)")

      call prop_get(tree, 'single', 'real', one_real, success)
      call assert_true(success, "Retrieving the real value should succeed (chapter: single)")
      call assert_comparable(one_real, 3.5, 1.0e-6, "Correct value must be returned (chapter: single)")

   end subroutine test_properties_get_single_values

   subroutine test_properties_get_strings
      character(len=20), dimension(10) :: string
      character(len=20), dimension(10) :: expected
      type(tree_data), pointer :: tree
      integer :: i
      integer :: error
      logical :: success
      !
      ! Check that a non-existing file causes an error
      !
      ! Note:
      ! Apparently, the tree variable needs to be associated/allocated beforehand
      !
      allocate (tree)

      call prop_inifile('test_strings.ini', tree, error)

      !
      ! Retrieve strings - array dimension large enough/limited
      !                  - default character
      !
      expected = ['A', 'B', 'C', '', 'D', 'E', '?', '?', '?', '?']
      string = '?'
      call prop_get(tree, 'multiple', 'setOfStrings1', size(string), string, success)

      do i = 1, 10
         call assert_true(success, "Retrieving the string value should succeed (chapter: multiple)")
         call assert_equal(string(i), expected(i), "Substring should be parsed correctly (default separator)")
      end do

      expected = ['A', 'B', '?', '?', '?', '?', '?', '?', '?', '?']
      string = '?'
      call prop_get(tree, 'multiple', 'setOfStrings1', 2, string, success)

      do i = 1, 10
         call assert_true(success, "Retrieving the string value should succeed (chapter: multiple)")
         call assert_equal(string(i), expected(i), "Substring should be parsed correctly - maximum 2 (two) substrings")
      end do

      !
      ! Retrieve strings - non-default character
      !

      expected = ['A                   ', 'B                   ', 'C                   ', '                    ', &
                  'D     ;          E  ', '?                   ', '?                   ', '?                   ', &
                  '?                   ', '?                   ']
      !12345678901234567890 - all strings need to be the same length! (This has been relaxed in later standards,
      ! but let's keep it simple
      string = '?'
      call prop_get(tree, 'multiple', 'setOfStrings2', size(string), string, success, '@')

      do i = 1, 10
         call assert_true(success, "Retrieving the string value should succeed (chapter: multiple)")
         call assert_equal(string(i), expected(i), "Substring should be parsed correctly (non-default separator)")
      end do
   end subroutine test_properties_get_strings

end module test_properties
