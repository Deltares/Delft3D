!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!

module m_nefis_test_16
   use assertions_gtest
   use tests_nefis_helper
   implicit none

   character(len=30), dimension(3) :: skiplines = ["Version", "version", "-----"]

contains

   !$f90tw TESTCODE(TEST, nefis_tests, test_16, test_16,
   subroutine test_16() bind(C)
      integer * 4 fds_a,&
      &fds_b,&
      &fds_c
      integer clsdat,&
      &clsdef,&
      &getnfv,&
      &NEFERR
      integer i, error
      character ERRSTR * 1024
      character * 255 version
      character(len=30) :: filename1
      character(len=30) :: filename2
      integer file_unit

      ! delete previous output files if they exist
      filename1 = 'test.out'
      filename2 = 'test.scr'
      call delete_file(filename1)
      call delete_file('data_c16a.def')
      call delete_file('data_c16a.dat')
      call delete_file('data_c16b.def')
      call delete_file('data_c16b.dat')
      call delete_file('data_c16c.def')
      call delete_file('data_c16c.dat')

      open (newunit=file_unit, file=filename1)

      error = getnfv(version)
      write (file_unit, *) '-----------------------------------------------'
      write (file_unit, '(a)') 'Version: '//trim(version(5:))
      write (file_unit, *) '-----------------------------------------------'

      write (file_unit, '('' Same test as test test_12'',&
      &          '' but open en close files 10 times '')')

      do i = 1, 10
         write (file_unit, '(i0)') i

         call WriteFile2('data_c16a', fds_a, 33, file_unit)
         call WriteFile2('data_c16b', fds_b, 39, file_unit)
         call WriteFile2('data_c16c', fds_c, 78, file_unit)

         call ReadFile2(fds_a, 33, file_unit)
         call ReadFile2(fds_b, 39, file_unit)
         call ReadFile2(fds_c, 78, file_unit)

         error = Clsdat(fds_a)
         if (error /= 0) then
            error = neferr(0, errstr)
            write (file_unit, '(a)') trim(errstr)
         end if

         error = Clsdat(fds_b)
         if (error /= 0) then
            error = neferr(0, errstr)
            write (file_unit, '(a)') trim(errstr)
         end if

         error = Clsdat(fds_c)
         if (error /= 0) then
            error = neferr(0, errstr)
            write (file_unit, '(a)') trim(errstr)
         end if

         error = Clsdef(fds_a)
         if (error /= 0) then
            error = neferr(0, errstr)
            write (file_unit, '(a)') trim(errstr)
         end if

         error = Clsdef(fds_b)
         if (error /= 0) then
            error = neferr(0, errstr)
            write (file_unit, '(a)') trim(errstr)
         end if

         error = Clsdef(fds_c)
         if (error /= 0) then
            error = neferr(0, errstr)
            write (file_unit, '(a)') trim(errstr)
         end if

      end do

      if (error == 0) then
         error = neferr(0, errstr)
         write (file_unit, '(a)')
         write (file_unit, '(a)') trim(errstr)
      end if

      close (file_unit)
      !
      call compare_text_files(filename1, filename2, skiplines)

   end subroutine test_16
   !$f90tw)

end module m_nefis_test_16
