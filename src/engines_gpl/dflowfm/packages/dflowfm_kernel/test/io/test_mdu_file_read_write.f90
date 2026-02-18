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
module test_mdu_file_read_write
   use precision
   use assertions_gtest

   implicit none

contains

   !$f90tw TESTCODE(TEST, test_mdu_file_read_write, test_read_write_read, test_read_write_read,
   subroutine test_read_write_read() bind(C)
      use messagehandling, only: LEVEL_INFO, LEVEL_WARN, LEVEL_ERROR, msgbuf, mess
      use unstruc_model, only: readMDUFile, md_obsfile, writeMDUFile, md_crsfile
      use dfm_error, only: DFM_NOERR
      use m_partitioninfo, only: jampi
      use ifport, only: CHANGEDIRQQ
      use m_resetfullflowmodel, only: resetFullFlowModel
      character(len=1024) :: tm_md_obsfile = ' '
      character(len=1024) :: tm_md_crsfile = ' '

      character(len=256) :: output_file = 'test_output.mdu'

      integer :: ierr
      call resetFullFlowModel()

      call F90_ASSERT_TRUE(CHANGEDIRQQ('MDUversion'), '')
      call readMDUFile('stretch_example.mdu', ierr)
      call f90_assert_eq(ierr, DFM_NOERR, 'Error when reading MDU file.')

      tm_md_obsfile = md_obsfile
      tm_md_crsfile = md_crsfile

      call f90_expect_gt(len_trim(tm_md_crsfile), 255, 'md_crsfile is maybe truncated.')

      call writeMDUFile(output_file, ierr)
      call f90_assert_eq(ierr, DFM_NOERR, 'Error when writing MDU file.')

      call resetFullFlowModel()
      call readMDUFile('test_output.mdu', ierr)
      call f90_assert_eq(ierr, DFM_NOERR, 'Error when re-reading MDU file.')

      call F90_EXPECT_STREQ(trim(md_obsfile)//c_null_char, trim(tm_md_obsfile)//c_null_char, 'Difference in md_obsfile after read-write-read cycle.')
      call F90_EXPECT_STREQ(trim(md_crsfile)//c_null_char, trim(tm_md_crsfile)//c_null_char, 'Difference in md_crsfile after read-write-read cycle.')
      call F90_ASSERT_TRUE(CHANGEDIRQQ('..'), '')

   end subroutine
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_mdu_file_read_write, test_mdu_fileversion_model, test_mdu_fileversion_model,
   subroutine test_mdu_fileversion_model() bind(C)
      use unstruc_model, only: readMDUFile
      use dfm_error, only: DFM_NOERR
      use m_partitioninfo, only: jampi
      use ifport, only: CHANGEDIRQQ
      use m_resetfullflowmodel, only: resetFullFlowModel

      integer :: ierr

      jampi = 0

      call resetFullFlowModel()

      call F90_ASSERT_TRUE(CHANGEDIRQQ('MDUversion'), '')
      ! read MDU
      call readMDUFile('old_model.mdu', ierr)
      call F90_ASSERT_TRUE(CHANGEDIRQQ('..'), '')

      call f90_expect_eq(ierr, DFM_NOERR, 'Error when reading old MDU file version with [model] block.')

   end subroutine test_mdu_fileversion_model
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_mdu_file_read_write, test_mdu_fileversion_general, test_mdu_fileversion_general,
   subroutine test_mdu_fileversion_general() bind(C)
      use unstruc_model, only: readMDUFile
      use dfm_error, only: DFM_NOERR
      use m_partitioninfo, only: jampi
      use ifport, only: CHANGEDIRQQ
      use m_resetfullflowmodel, only: resetFullFlowModel

      integer :: ierr

      jampi = 0
      call resetFullFlowModel()

      call F90_ASSERT_TRUE(CHANGEDIRQQ("MDUversion"), '')
      ! read MDU
      call readMDUFile('new_general.mdu', ierr)
      call F90_ASSERT_TRUE(CHANGEDIRQQ(".."), '')

      call f90_expect_eq(ierr, DFM_NOERR, 'Error when reading new MDU file version with [General] block.')
   end subroutine test_mdu_fileversion_general
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_mdu_file_read_write, test_read_stretch_coef, test_read_stretch_coef,
   subroutine test_read_stretch_coef() bind(C)
      use unstruc_model, only: readMDUFile
      use dfm_error, only: DFM_NOERR
      use m_partitioninfo, only: jampi
      use ifport, only: CHANGEDIRQQ
      use m_resetfullflowmodel, only: resetFullFlowModel
      use m_flow, only: laycof
      use precision, only: dp

      integer :: ierr
      real(kind=hp) :: sumlaycof

      jampi = 0
      call resetFullFlowModel()

      call F90_ASSERT_TRUE(CHANGEDIRQQ("MDUversion"), '')
      ! read MDU
      call readMDUFile('stretch_example.mdu', ierr)
      call F90_ASSERT_TRUE(CHANGEDIRQQ(".."), '')

      call f90_expect_eq(ierr, DFM_NOERR, 'Error when reading MDU file with stretch coeff.')
      call f90_expect_eq(size(laycof), 18, "Difference in dimension of laycof")
      sumlaycof = sum(laycof)
      call F90_ASSERT_NEAR(sumlaycof, 100.0_dp, 1e-12_dp, "Difference in sum of laycof for all layers")
   end subroutine test_read_stretch_coef
   !$f90tw)

end module test_mdu_file_read_write
