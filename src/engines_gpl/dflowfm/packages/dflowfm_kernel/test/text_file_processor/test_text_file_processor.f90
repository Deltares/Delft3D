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
module test_text_file_processor
   use precision
   use assertions_gtest
   use m_text_file_processor, only: TextFileProcessor
   use messagehandling, only: LEVEL_INFO, LEVEL_WARN, LEVEL_ERROR, msgbuf, mess, msg_flush

   implicit none

contains

   !$f90tw TESTCODE(TEST, test_text_file_processor, test_notfound, test_notfound,
   subroutine test_notfound() bind(C)
      type(TextFileProcessor) :: processor

      processor = TextFileProcessor('example.txt')
      call msg_flush()
      call f90_assert_eq(processor%is_error, .true., 'Processor should indicate error for non-existing file.')

      ! Check if msgbuf starts with "File does not exist"
      call f90_assert_streq(msgbuf(1:19)//C_NULL_CHAR, 'File does not exist', 'msgbuf should start with "File does not exist".')
      call msg_flush()

   end subroutine
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_text_file_processor, test_basic, test_basic,
   subroutine test_basic() bind(C)
      type(TextFileProcessor) :: processor

      processor = TextFileProcessor('tt3.mdu')
      call msg_flush()

      call f90_assert_eq(processor%is_error, .false., 'Processor should indicate no error for existing file.')

      call msg_flush()

   end subroutine
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_text_file_processor, test_required_verifier, test_required_verifier,
   subroutine test_required_verifier() bind(C)
      use m_text_file_validators, only: ChapterPropsVerifier
      type(TextFileProcessor) :: processor
      type(ChapterPropsVerifier) :: verifier
      character(len=:), allocatable :: required_strings(:)

      required_strings = [ 'OutputDir', 'MapFormat' ]
      verifier = ChapterPropsVerifier('output', required_strings)

      processor = TextFileProcessor('tt3.mdu')
      call msg_flush()

      call f90_assert_eq(processor%is_error, .false., 'Processor should indicate no error for existing file.')

      ! Verify required strings
      call f90_assert_eq(verifier%verify(processor), .true., 'All required strings should be present in the file.')
      verifier = ChapterPropsVerifier('output', [ 'NonExistingProp' ])
      call f90_assert_eq(verifier%verify(processor), .false., 'Verification should fail for missing required strings.')

      call msg_flush()
   end subroutine
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_text_file_processor, test_and_verifier, test_and_verifier,
   subroutine test_and_verifier() bind(C)
      use m_text_file_validators, only: ChapterPropsVerifier, AndVerifier, ArraysLengthVerifier, TextFileProcessorVerifier, VerifierPtr

      type(TextFileProcessor) :: processor
      class(TextFileProcessorVerifier), allocatable :: verifier1, verifier2
      type(AndVerifier) :: and_verifier
      character(len=:), allocatable :: required_strings1(:), required_strings2(:)
      class(VerifierPtr), DIMENSION(:), allocatable :: verifiers

      required_strings1 = [ 'discharge', 'salinityDelta' ]
      verifier1 = ChapterPropsVerifier('BlockXYMissing', required_strings1)
      verifier2  = ArraysLengthVerifier('BlockXYGood', [ 'xCoordinates', 'yCoordinates' ])

      processor = TextFileProcessor('sorsin3D-new.ext')
      call msg_flush()

      call f90_assert_eq(processor%is_error, .false., 'Processor should indicate no error for existing file.')


      ! Test AndVerifier with all conditions passing
      verifiers = [ VerifierPtr(verifier1), VerifierPtr(verifier2) ]
      and_verifier = AndVerifier(verifiers)
      call f90_assert_eq(and_verifier%verify(processor), .true., 'AndVerifier should pass when all sub-verifiers pass.')
      call msg_flush()

      ! Test AndVerifier with one condition failing
      verifier1 = ChapterPropsVerifier('BlockXYGood', [ 'NonExistentProperty' ])
      verifiers = [ VerifierPtr(verifier1), VerifierPtr(verifier2) ]
      and_verifier = AndVerifier(verifiers)
      call f90_assert_eq(and_verifier%verify(processor), .false., 'AndVerifier should fail when any sub-verifier fails.')

      call msg_flush()
   end subroutine
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_text_file_processor, test_x_y_coord, test_x_y_coord,
   subroutine test_x_y_coord() bind(C)

      use m_text_file_validators, only: TextFileProcessorVerifier, ArraysLengthVerifier


      type(TextFileProcessor) :: processor
      class(TextFileProcessorVerifier), allocatable :: verifier
      character(len=:), allocatable :: required_strings(:)
      character(len=1024) :: cwd_string

      processor = TextFileProcessor('sorsin3D-new.ext')
      call msg_flush()

      call f90_assert_eq(processor%is_error, .false., 'Processor should indicate no error for existing file.')

      verifier  = ArraysLengthVerifier('BlockXYBad', [ 'xCoordinates', 'yCoordinates' ])
      call f90_assert_eq(verifier%verify(processor), .false., 'BlockXYBad should not pass the verification as xCoordinates and yCoordinates have different lengths.')

      verifier  = ArraysLengthVerifier('BlockXYGood', [ 'xCoordinates', 'yCoordinates' ])
      call f90_assert_eq(verifier%verify(processor), .true., 'BlockXYGood should pass the verification as xCoordinates and yCoordinates have the same length.')

      call msg_flush()
   end subroutine
   !$f90tw)
   
   !$f90tw TESTCODE(TEST, test_text_file_processor, test_block_verifier, test_block_verifier,
   subroutine test_block_verifier() bind(C)
      use properties, only: prop_inifile
      use tree_data_types, only: tree_data
      use tree_structures, only: tree_num_nodes, tree_get_name
      use m_text_file_validators, only: ChapterVerifier, ArraysLengthChapterVerifier
      use string_module, only: str_tolower


      type(TextFileProcessor) :: processor, tmpProcessor
      class(ChapterVerifier), allocatable :: verifier

      type(tree_data), pointer   :: tree
      integer :: istat
      integer :: num_items_in_file, i
      type(tree_data), pointer :: block_ptr
      character(len=:), allocatable :: group_name

      processor = TextFileProcessor('sorsin3D-new.ext')
      call f90_assert_eq(processor%is_error, .false., 'Processor should indicate no error for existing file.')

      ! call prop_inifile('sorsin3D-new.ext', tree, istat)
      ! call f90_assert_eq(istat, 0, 'File should be parsed successfully.')
      num_items_in_file = tree_num_nodes(processor%tree)
      do i = 1, num_items_in_file
         block_ptr => processor%tree%child_nodes(i)%node_ptr
         group_name = trim(tree_get_name(block_ptr))
         if (trim(adjustl(str_tolower(group_name))) == trim(adjustl(str_tolower('BlockXYBad')))) then
            tmpProcessor = TextFileProcessor(block_ptr)
            verifier  = ArraysLengthChapterVerifier([ 'xCoordinates', 'yCoordinates' ], 'numCoordinates')
            call f90_assert_eq(verifier%verify(block_ptr), .false., 'BlockXYBad should not pass the verification as xCoordinates and yCoordinates have different lengths.')
         end if

         if (trim(adjustl(str_tolower(group_name))) == trim(adjustl(str_tolower('BlockXYGood')))) then
            tmpProcessor = TextFileProcessor(block_ptr)
            verifier  = ArraysLengthChapterVerifier([ 'xCoordinates', 'yCoordinates' ], 'numCoordinates')
            call f90_assert_eq(verifier%verify(block_ptr), .true., 'BlockXYGood should pass the verification as xCoordinates and yCoordinates have the same length.')
         end if

      end do

   end subroutine
   !$f90tw)
   

end module test_text_file_processor
