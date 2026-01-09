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
   function get_current_dir() result(cwd)
      use iso_c_binding
      character(len=:), allocatable :: cwd
      character(kind=c_char, len=1024) :: buf
      type(c_ptr) :: res
      integer :: null_pos

      interface
         function getcwd(buf, size) bind(C, name="_getcwd")
            import :: c_char, c_size_t, c_ptr
            type(c_ptr) :: getcwd
            character(kind=c_char) :: buf(*)
            integer(c_size_t), value :: size
         end function
      end interface

      res = getcwd(buf, len(buf, kind=c_size_t))
      if (c_associated(res)) then
         null_pos = index(buf, c_null_char)
         if (null_pos == 0) null_pos = len(buf) + 1
         cwd = buf(:null_pos - 1)
      else
         cwd = ""
      end if
   end function
   !$f90tw TESTCODE(TEST, test_mdu_file_read_write, test_something, test_something,
   subroutine test_something() bind(C)
      use messagehandling, only: LEVEL_INFO, LEVEL_WARN, LEVEL_ERROR, msgbuf, mess
      character(len=:), allocatable :: cwd

      ! Call the function
      cwd = get_current_dir()
      call mess(LEVEL_ERROR, "PWD", cwd)
      call f90_expect_eq(1, 0, cwd)
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
