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
   use messagehandling, only: LEVEL_FATAL
   use unstruc_messages, only: threshold_abort
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_mdu_file_read_write, test_mdu_read_write_read, test_mdu_read_write_read,
   !> Tests if a mdu file can succesfully be read, written, and re-read. Also checks if the obsfile and crsfile is unchanged after read-write-read cycle
   subroutine test_mdu_read_write_read() bind(C)
      use messagehandling, only: LEVEL_INFO, LEVEL_WARN, LEVEL_ERROR, msgbuf, mess
      use unstruc_model, only: readMDUFile, md_obsfile, writeMDUFile, md_crsfile
      use dfm_error, only: DFM_NOERR
      use m_partitioninfo, only: jampi
      use ifport, only: CHANGEDIRQQ
      use m_resetfullflowmodel, only: resetFullFlowModel

      character(len=1024) :: tm_md_obsfile
      character(len=1024) :: tm_md_crsfile
      character(len=256) :: output_file
      integer :: ierr

      jampi = 0

      tm_md_obsfile = ''
      tm_md_crsfile = ''
      output_file = 'test_output.mdu'

      call resetFullFlowModel()

      call F90_ASSERT_TRUE(CHANGEDIRQQ('MDUversion'), '')
      threshold_abort = LEVEL_FATAL
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

   end subroutine test_mdu_read_write_read
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_mdu_file_read_write, test_immersed_boundary_mdu_roundtrip, test_immersed_boundary_mdu_roundtrip,
   subroutine test_immersed_boundary_mdu_roundtrip() bind(C)
      use unstruc_model, only: readMDUFile, writeMDUFile
      use m_flowparameters, only: immersed_boundary, immersed_boundary_min_area_frac, immersed_boundary_cartesian_tol, &
                                 immersed_boundary_tangential_correction, immersed_boundary_small_edge_frac, &
                                 immersed_boundary_projection, immersed_boundary_merge_area_frac
      use dfm_error, only: DFM_NOERR
      use m_partitioninfo, only: jampi
      use ifport, only: CHANGEDIRQQ
      use m_resetfullflowmodel, only: resetFullFlowModel

      character(len=256) :: output_file
      integer :: ierr

      jampi = 0
      output_file = 'test_immersed_boundary.mdu'
      call resetFullFlowModel()

      call F90_ASSERT_TRUE(CHANGEDIRQQ('MDUversion'), '')
      threshold_abort = LEVEL_FATAL
      call readMDUFile('stretch_example.mdu', ierr)
      call f90_assert_eq(ierr, DFM_NOERR, 'Error when reading MDU file.')
      call f90_assert_eq(immersed_boundary, 0, 'ImmersedBoundary default must be off.')
      call F90_ASSERT_NEAR(immersed_boundary_min_area_frac, 0.1_dp, 1.0e-12_dp, 'Unexpected default IBM area fraction.')
      call F90_ASSERT_NEAR(immersed_boundary_merge_area_frac, 0.0_dp, 1.0e-12_dp, 'Unexpected default IBM merge fraction.')
      call F90_ASSERT_NEAR(immersed_boundary_cartesian_tol, 1.0e-6_dp, 1.0e-12_dp, 'Unexpected default IBM Cartesian tolerance.')
      call f90_assert_eq(immersed_boundary_tangential_correction, 0, 'Unexpected default IBM tangential correction.')
      call F90_ASSERT_NEAR(immersed_boundary_small_edge_frac, 0.5_dp, 1.0e-12_dp, 'Unexpected default IBM small-edge fraction.')
      call f90_assert_eq(immersed_boundary_projection, 0, 'Unexpected default IBM projection.')

      immersed_boundary_min_area_frac = 0.2_dp
      immersed_boundary_merge_area_frac = 0.02_dp
      immersed_boundary_cartesian_tol = 2.0e-6_dp
      immersed_boundary_tangential_correction = 1
      immersed_boundary_small_edge_frac = 0.75_dp
      immersed_boundary_projection = 1
      call writeMDUFile(output_file, ierr)
      call f90_assert_eq(ierr, DFM_NOERR, 'Error when writing IBM MDU settings.')

      call resetFullFlowModel()
      call readMDUFile(output_file, ierr)
      call f90_assert_eq(ierr, DFM_NOERR, 'Error when re-reading IBM MDU settings.')
      call f90_assert_eq(immersed_boundary, 0, 'ImmersedBoundary must remain off in the round trip.')
      call F90_ASSERT_NEAR(immersed_boundary_min_area_frac, 0.2_dp, 1.0e-12_dp, 'IBM area fraction did not round-trip.')
      call F90_ASSERT_NEAR(immersed_boundary_merge_area_frac, 0.02_dp, 1.0e-12_dp, 'IBM merge fraction did not round-trip.')
      call F90_ASSERT_NEAR(immersed_boundary_cartesian_tol, 2.0e-6_dp, 1.0e-12_dp, 'IBM Cartesian tolerance did not round-trip.')
      call f90_assert_eq(immersed_boundary_tangential_correction, 1, 'IBM tangential correction did not round-trip.')
      call F90_ASSERT_NEAR(immersed_boundary_small_edge_frac, 0.75_dp, 1.0e-12_dp, 'IBM small-edge fraction did not round-trip.')
      call f90_assert_eq(immersed_boundary_projection, 1, 'IBM projection did not round-trip.')
      call F90_ASSERT_TRUE(CHANGEDIRQQ('..'), '')
   end subroutine test_immersed_boundary_mdu_roundtrip
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
      threshold_abort = LEVEL_FATAL
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
      threshold_abort = LEVEL_FATAL
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
      threshold_abort = LEVEL_FATAL
      call readMDUFile('stretch_example.mdu', ierr)
      call F90_ASSERT_TRUE(CHANGEDIRQQ(".."), '')

      call f90_expect_eq(ierr, DFM_NOERR, 'Error when reading MDU file with stretch coeff.')
      call f90_expect_eq(size(laycof), 18, "Difference in dimension of laycof")
      sumlaycof = sum(laycof)
      call F90_ASSERT_NEAR(sumlaycof, 100.0_dp, 1e-12_dp, "Difference in sum of laycof for all layers")

   end subroutine test_read_stretch_coef
   !$f90tw)

end module test_mdu_file_read_write
