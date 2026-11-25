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

   !$gtest
   !> Tests if a mdu file can succesfully be read, written, and re-read. Also checks if the obsfile and crsfile is unchanged after read-write-read cycle
   subroutine test_mdu_read_write_read() bind(C)
      use messagehandling, only: LEVEL_INFO, LEVEL_WARN, LEVEL_ERROR, msgbuf, mess
      use unstruc_model, only: readMDUFile, md_obsfile, writeMDUFile, md_crsfile
      use m_flowparameters, only: use_restart_bedlevel, use_restart_bedcomp
      use dfm_error, only: DFM_NOERR
      use m_partitioninfo, only: jampi
      use ifport, only: CHANGEDIRQQ
      use m_resetfullflowmodel, only: resetFullFlowModel

      character(len=1024) :: tm_md_obsfile
      character(len=1024) :: tm_md_crsfile
      character(len=256) :: output_file
      character(len=4096) :: line
      integer :: ierr, input_unit, output_unit, ios
      logical :: found_bedlevel, found_bedcomp

      jampi = 0

      tm_md_obsfile = ''
      tm_md_crsfile = ''
      output_file = 'test_output.mdu'

      call resetFullFlowModel()

      if (.not. f90_assert_true(CHANGEDIRQQ('MDUversion'), 'Cannot enter MDUversion')) then
         return
      end if
      threshold_abort = LEVEL_FATAL
      call readMDUFile('stretch_example.mdu', ierr)
      if (.not. f90_assert_eq(ierr, DFM_NOERR, 'Error when reading MDU file.')) then
         call f90_expect_true(CHANGEDIRQQ('..'), 'Cannot restore working directory')
         return
      end if

      tm_md_obsfile = md_obsfile
      tm_md_crsfile = md_crsfile

      call f90_expect_gt(len_trim(tm_md_crsfile), 255, 'md_crsfile is maybe truncated.')

      call F90_EXPECT_TRUE(use_restart_bedlevel)
      call F90_EXPECT_TRUE(use_restart_bedcomp)
      use_restart_bedlevel = .false.
      use_restart_bedcomp = .false.
      call writeMDUFile(output_file, ierr)
      if (.not. f90_assert_eq(ierr, DFM_NOERR, 'Error when writing MDU file.')) then
         call f90_expect_true(CHANGEDIRQQ('..'), 'Cannot restore working directory')
         return
      end if

      open(newunit=input_unit, file=output_file, status='old')
      open(newunit=output_unit, file='test_restart_options.mdu', status='replace')
      found_bedlevel = .false.
      found_bedcomp = .false.
      do
         read(input_unit, '(a)', iostat=ios) line
         if (ios /= 0) exit
         if (index(adjustl(line), 'useRestartBedlevel') == 1) then
            write(output_unit, '(a)') 'RstIgnoreBl = 0'
            found_bedlevel = .true.
         end if
         write(output_unit, '(a)') trim(line)
      end do
      close(input_unit)
      close(output_unit)
      call F90_EXPECT_TRUE(found_bedlevel)
      call F90_EXPECT_TRUE(found_bedcomp)

      call resetFullFlowModel()
      call readMDUFile('test_restart_options.mdu', ierr)
      if (.not. f90_assert_eq(ierr, DFM_NOERR, 'Error when re-reading MDU file.')) then
         call f90_expect_true(CHANGEDIRQQ('..'), 'Cannot restore working directory')
         return
      end if
      call F90_EXPECT_FALSE(use_restart_bedlevel)
      call F90_EXPECT_FALSE(use_restart_bedcomp)

      call f90_expect_streq(trim(md_obsfile), trim(tm_md_obsfile), 'Difference in md_obsfile after read-write-read cycle.')
      call f90_expect_streq(trim(md_crsfile), trim(tm_md_crsfile), 'Difference in md_crsfile after read-write-read cycle.')
      call f90_expect_true(CHANGEDIRQQ('..'), 'Cannot restore working directory')

   end subroutine test_mdu_read_write_read

   !$gtest
   subroutine test_mdu_fileversion_model() bind(C)
      use unstruc_model, only: readMDUFile
      use dfm_error, only: DFM_NOERR
      use m_partitioninfo, only: jampi
      use ifport, only: CHANGEDIRQQ
      use m_resetfullflowmodel, only: resetFullFlowModel

      integer :: ierr

      jampi = 0

      call resetFullFlowModel()

      if (.not. f90_assert_true(CHANGEDIRQQ('MDUversion'), 'Cannot enter MDUversion')) then
         return
      end if
      ! read MDU
      threshold_abort = LEVEL_FATAL
      call readMDUFile('old_model.mdu', ierr)
      call f90_expect_true(CHANGEDIRQQ('..'), 'Cannot restore working directory')

      call f90_expect_eq(ierr, DFM_NOERR, 'Error when reading old MDU file version with [model] block.')

   end subroutine test_mdu_fileversion_model

   !$gtest
   subroutine test_mdu_fileversion_general() bind(C)
      use unstruc_model, only: readMDUFile
      use dfm_error, only: DFM_NOERR
      use m_partitioninfo, only: jampi
      use ifport, only: CHANGEDIRQQ
      use m_resetfullflowmodel, only: resetFullFlowModel

      integer :: ierr

      jampi = 0

      call resetFullFlowModel()

      if (.not. f90_assert_true(CHANGEDIRQQ("MDUversion"), 'Cannot enter MDUversion')) then
         return
      end if
      ! read MDU
      threshold_abort = LEVEL_FATAL
      call readMDUFile('new_general.mdu', ierr)
      call f90_expect_true(CHANGEDIRQQ(".."), 'Cannot restore working directory')

      call f90_expect_eq(ierr, DFM_NOERR, 'Error when reading new MDU file version with [General] block.')

   end subroutine test_mdu_fileversion_general

   !$gtest
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

      if (.not. f90_assert_true(CHANGEDIRQQ("MDUversion"), 'Cannot enter MDUversion')) then
         return
      end if
      threshold_abort = LEVEL_FATAL
      call readMDUFile('stretch_example.mdu', ierr)
      call f90_expect_true(CHANGEDIRQQ(".."), 'Cannot restore working directory')

      if (.not. f90_assert_eq(ierr, DFM_NOERR, 'Error when reading MDU file with stretch coeff.')) then
         return
      end if
      if (.not. f90_assert_true(allocated(laycof), "Stretch coefficients must be allocated")) then
         return
      end if
      call f90_expect_eq(size(laycof), 18, "Difference in dimension of laycof")
      sumlaycof = sum(laycof)
      call f90_expect_near(sumlaycof, 100.0_dp, 1e-12_dp, "Difference in sum of laycof for all layers")

   end subroutine test_read_stretch_coef

   !$gtest
   subroutine test_frozen_2d_velocity() bind(C)
      use m_u1q1, only: update_frozen_1d2d_velocity
      use m_flow, only: au, q1, qa, u1
      use m_flowgeom, only: lnx
      use m_resetfullflowmodel, only: resetFullFlowModel
      use dfm_error, only: DFM_NOERR, DFM_GENERICERROR

      integer :: ierr

      call resetFullFlowModel()
      lnx = 2
      allocate (au(lnx), q1(lnx), qa(lnx), u1(lnx))
      q1 = [6.0_dp, 0.0_dp]
      au = [3.0_dp, 0.0_dp]

      call update_frozen_1d2d_velocity(ierr)
      call f90_expect_eq(ierr, DFM_NOERR)
      call f90_expect_eq(u1(1), 2.0_dp)
      call f90_expect_eq(u1(2), 0.0_dp)

      au(1) = 2.0_dp
      call update_frozen_1d2d_velocity(ierr)
      call f90_expect_eq(ierr, DFM_NOERR)
      call f90_expect_eq(q1(1), 6.0_dp)
      call f90_expect_eq(qa(1), 6.0_dp)
      call f90_expect_eq(u1(1), 3.0_dp)

      threshold_abort = LEVEL_FATAL
      au(1) = 0.0_dp
      call update_frozen_1d2d_velocity(ierr)
      call f90_expect_eq(ierr, DFM_GENERICERROR)
      call f90_expect_eq(q1(1), 6.0_dp)

      call resetFullFlowModel()
   end subroutine test_frozen_2d_velocity

end module test_mdu_file_read_write
