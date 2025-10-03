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

module m_nefis_test_04
   use assertions_gtest
   use tests_nefis_helper
   implicit none

   character(len=30), dimension(3) :: skiplines = ["Version", "version", "-----"]

contains

   !$f90tw TESTCODE(TEST, nefis_tests, test_04, test_04,
   subroutine test_04() bind(C)
      integer * 4 fds
      integer ::&
      &Clsdat,&
      &Clsdef,&
      &Credat,&
      &Defcel,&
      &Defelm,&
      &Defgrp,&
      &Getnfv,&
      &Opndat,&
      &Opndef,&
      &Putelt,&
      &Neferr
      integer Getelt
      integer error,&
      &idum,&
      &i,&
      &imax,&
      &start,&
      &UINDEX(3, 1)
      real buffer,&
      &cpu1,&
      &cpu2
      character coding * 1
      character * 1024 errstr
      character * 255 version
      character * 20 strdata
      character(len=30) :: filename1
      character(len=30) :: filename2
      integer file_unit

      ! delete previous output files if they exist
      filename1 = 'test.out'
      filename2 = 'test.scr'
      call delete_file(filename1)
      call delete_file('nefis_ex.def')
      call delete_file('nefis_ex.dat')

      open (newunit=file_unit, file=filename1)

      cpu1 = 0.0
      cpu2 = 0.0
      idum = 0
      coding = 'N'
      imax = 1000
      start = 1
!
      call clock(cpu1)
      error = getnfv(version)

      write (file_unit, *) '-----------------------------------------------'
      write (file_unit, '(a)') 'Version: '//trim(version(5:))
      write (file_unit, *) '-----------------------------------------------'

      error = Opndef(fds, 'nefis_ex.def', coding)
      if (error /= 0) goto 9999
!
      error = Defelm(fds, 'ELEM_R_4', 'REAL', 4,&
      &'GROOTHEID 1', 'eenheid 1', 'Beschrijving 1',&
      &0, idum)
      if (error /= 0) goto 9999

      error = Defelm(fds, 'ELEM_STR', 'CHARACTE', 20,&
      &'GROOTHEID 2', 'eenheid 2', 'Beschrijving 2',&
      &0, idum)
      if (error /= 0) goto 9999
!
      error = Defcel(fds, 'CEL_TEST_1', 1, 'ELEM_R_4')
      if (error /= 0) goto 9999

      error = Defcel(fds, 'CEL_TEST_2', 1, 'ELEM_STR')
      if (error /= 0) goto 9999
!
      error = Defgrp(fds, 'GRP_TEST_1', 'CEL_TEST_1', 1, imax, 1)
      if (error /= 0) goto 9999
!
      error = Defgrp(fds, 'GRP_TEST_2', 'CEL_TEST_2', 1, imax, 1)
      if (error /= 0) goto 9999
!==========================================================
      error = Defgrp(fds, 'GRP_TEMP', 'CEL_TEST_1', 1, 1, 1)
      if (error /= 0) goto 9999
!==========================================================
!
      error = Opndat(fds, 'nefis_ex.dat', coding)
      if (error /= 0) goto 9999
!
      error = Credat(fds, 'DATAGRP_TEST_1A', 'GRP_TEST_1')
      if (error /= 0) goto 9999
!
      error = Credat(fds, 'DATAGRP_TEST_1B', 'GRP_TEST_1')
      if (error /= 0) goto 9999
!
      error = Credat(fds, 'DATAGRP_TEST_1C', 'GRP_TEST_2')
      if (error /= 0) goto 9999
!
      call clock(cpu2)
      write (file_unit, '(''Initialisation NEFIS files [sec]'',1PE13.5)')&
      &cpu2 - cpu1
!
      write (file_unit, *)
      write (file_unit, '(''Schrijf elementen'')')
      write (file_unit, *)
!
      call clock(cpu1)
      UINDEX(3, 1) = 1
      do i = 1, imax
         UINDEX(1, 1) = i
         UINDEX(2, 1) = i
         error = Putelt(fds, 'DATAGRP_TEST_1A', '*',&
         &UINDEX, 1, real(i))
         if (error /= 0) goto 9999
      end do
      call clock(cpu2)
      write (file_unit, '(''DATAGRP_TEST_1A written in [sec]'',1PE13.5)')&
      &cpu2 - cpu1

      call clock(cpu1)
      do i = imax, 1, -1
         UINDEX(1, 1) = i
         UINDEX(2, 1) = i
         error = Putelt(fds, 'DATAGRP_TEST_1B', '*',&
         &UINDEX, 1, -1.*real(i))
         if (error /= 0) goto 9999
      end do
      call clock(cpu2)
      write (file_unit, '(''DATAGRP_TEST_1B written in [sec]'',1PE13.5)')&
      &cpu2 - cpu1

      call clock(cpu1)
      do i = imax, 1, -1
         UINDEX(1, 1) = i
         UINDEX(2, 1) = i
         error = Putelt(fds, 'DATAGRP_TEST_1C', '*',&
         &UINDEX, 1, 'ABCDEFGHIJKLMNOPQRST')
         if (error /= 0) goto 9999
      end do

      call clock(cpu2)
      write (file_unit, '(''DATAGRP_TEST_1C written in [sec]'',1PE13.5)')&
      &cpu2 - cpu1

!
!=====================================================================
      write (file_unit, *)
      write (file_unit, '(''Lees elementen'')')
      write (file_unit, *)
!
      call clock(cpu1)
      do i = imax, 1, -1
         UINDEX(1, 1) = i
         UINDEX(2, 1) = i
         error = Getelt(fds, 'DATAGRP_TEST_1A', '*',&
         &UINDEX, 1, 4, buffer)
         if (error /= 0) goto 9999
         if (nint(buffer) /= i) print *, 'error, i= ', i, buffer&
         &, nint(buffer)
      end do
      call clock(cpu2)
      write (file_unit, '(''DATAGRP_TEST_1A read    in [sec]'',1PE13.5)')&
      &cpu2 - cpu1
!
      call clock(cpu1)
      do i = 1, imax
         UINDEX(1, 1) = i
         UINDEX(2, 1) = i
         error = Getelt(fds, 'DATAGRP_TEST_1B', '*',&
         &UINDEX, 1, 4, buffer)
         if (error /= 0) goto 9999
         if (nint(buffer) /= -1 * i) print *, 'error, i= ', i, buffer&
         &, nint(buffer)
      end do
      call clock(cpu2)
      write (file_unit, '(''DATAGRP_TEST_1B read    in [sec]'',1PE13.5)')&
      &cpu2 - cpu1

      call clock(cpu1)
      do i = 1, imax
         UINDEX(1, 1) = i
         UINDEX(2, 1) = i
         error = Getelt(fds, 'DATAGRP_TEST_1C', '*',&
         &UINDEX, 1, 20, strdata)
         if (error /= 0) goto 9999
         if (strdata /= 'ABCDEFGHIJKLMNOPQRST') print *, 'error, i= ', i, strdata&
         &, strdata
      end do
      call clock(cpu2)
      write (file_unit, '(''DATAGRP_TEST_1C read    in [sec]'',1PE13.5)')&
      &cpu2 - cpu1
      write (file_unit, *)

      error = Clsdat(fds)
      error = Clsdef(fds)
!
9999  continue
!
      error = Neferr(0, errstr)
      write (file_unit, '(a)') trim(errstr)
      close (file_unit)
      !
      call compare_text_files(filename1, filename2, skiplines)

   end subroutine test_04
   !$f90tw)

end module m_nefis_test_04
