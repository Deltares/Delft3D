!!  Copyright (C)  Stichting Deltares, 2012-2026.
!!
!!  This program is free software: you can redistribute it and/or
!!  modify it under the terms of the GNU General Public License version 3,
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
module test_dflowfm_version
   use assertions_gtest, only: f90_assert_eq, f90_expect_eq, f90_expect_streq, f90_expect_true
   use dflowfm_version_module, only: write_dflowfm_version

   implicit none(type, external)
   private

   character(len=*), parameter :: digits = '0123456789'

contains

   !$gtest
   subroutine test_headless_version_report() bind(C)
      character(len=*), parameter :: version_prefix = 'Deltares, D-Flow FM Version '
      character(len=*), parameter :: source_prefix = 'Source: '
#ifdef __INTEL_LLVM_COMPILER
      character(len=*), parameter :: compiler_prefix = 'Compiled with Intel ifx, version '
#endif
      character(len=1024) :: line
      integer :: unit, status

      open (newunit=unit, status='scratch', form='formatted', action='readwrite', iostat=status)
      if (.not. f90_assert_eq(status, 0, 'Opening scratch file for the version report')) then
         return
      end if

      call write_dflowfm_version(unit, gui_enabled=.false.)
      rewind (unit)

      call read_report_line(unit, line)
      call f90_expect_streq(line(:len(version_prefix)), version_prefix, 'Product and version prefix')
      call f90_expect_true(scan(line(len(version_prefix) + 1:len(version_prefix) + 1), digits) == 1, 'Version begins with a digit')

      call read_report_line(unit, line)
      call f90_expect_streq(line(:len(source_prefix)), source_prefix, 'Source prefix')
      call f90_expect_true(len_trim(line) > len(source_prefix), 'Source branch is not empty')

#ifdef __INTEL_LLVM_COMPILER
      call read_report_line(unit, line)
      call f90_expect_streq(line(:len(compiler_prefix)), compiler_prefix, 'Intel ifx compiler prefix')
      call f90_expect_true(is_dotted_version(trim(line(len(compiler_prefix) + 1:))), 'Intel ifx version is major.minor.patch')
#endif
      call expect_report_line(unit, 'Compiled with support for:')
      call expect_report_line(unit, 'IntGUI   : no')
      call expect_report_line(unit, 'OpenGL   : no')
      call expect_report_line(unit, 'OpenMP   : yes')
      call expect_report_line(unit, 'MPI      : yes')
      call expect_report_line(unit, 'PETSc    : yes')
      call expect_report_line(unit, 'METIS    : yes')
      call expect_report_line(unit, 'PROJ     : yes')
      call expect_report_line(unit, 'Shapelib : yes')
      call expect_report_line(unit, 'GDAL     : yes')

      read (unit, '(a)', iostat=status) line
      call f90_expect_true(is_iostat_end(status), 'End of version report')
      close (unit)
   end subroutine test_headless_version_report

#ifdef __INTEL_LLVM_COMPILER
   logical function is_dotted_version(version) result(valid)
      character(len=*), intent(in) :: version
      integer :: first_dot, last_dot

      valid = .false.
      first_dot = index(version, '.')
      last_dot = index(version, '.', back=.true.)
      if (first_dot <= 1 .or. last_dot <= first_dot + 1 .or. last_dot == len(version)) then
         return
      end if
      valid = verify(version(:first_dot - 1)//version(first_dot + 1:last_dot - 1)//version(last_dot + 1:), digits) == 0
   end function is_dotted_version
#endif

   subroutine expect_report_line(unit, expected)
      integer, intent(in) :: unit
      character(len=*), intent(in) :: expected
      character(len=1024) :: line

      call read_report_line(unit, line)
      call f90_expect_streq(trim(line), expected)
   end subroutine expect_report_line

   subroutine read_report_line(unit, line)
      integer, intent(in) :: unit
      character(len=*), intent(out) :: line
      integer :: status

      read (unit, '(a)', iostat=status) line
      if (status /= 0) then
         line = ''
      end if
      call f90_expect_eq(status, 0, 'Reading a line of the version report')
   end subroutine read_report_line

end module test_dflowfm_version
