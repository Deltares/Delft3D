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

module m_nefis_test_09
   use assertions_gtest
   use tests_nefis_helper
   implicit none

   character(len=30), dimension(3) :: skiplines = ["Version", "version", "-----"]

contains

   !$f90tw TESTCODE(TEST, nefis_tests, test_09, test_09,
   subroutine test_09() bind(C)
      integer * 4 fds, datfds
      integer START, stop, INCR
      parameter(START=1, stop=2, INCR=3)
      integer Opndef,&
      &Defelm,&
      &Defgrp,&
      &Opndat,&
      &Credat,&
      &getnfv,&
      &Putelt,&
      &Defcel,&
      &Clsdat,&
      &Clsdef
      integer Getelt
      integer Neferr
      integer error,&
      &idum,&
      &i,&
      &UINDEX(3, 1)
      real cpu1,&
      &cpu2
      complex * 16 val
      character coding * 1
      character ERRSTR * 1024
      character * 255 version
      character(len=30) :: filename1
      character(len=30) :: filename2
      integer file_unit

      ! delete previous output files if they exist
      filename1 = 'test.out'
      filename2 = 'test.scr'
      call delete_file(filename1)
      call delete_file('data_c09.def')
      call delete_file('data_c09.dat')

      open (newunit=file_unit, file=filename1)

      error = getnfv(version)
      write (file_unit, *) '-----------------------------------------------'
      write (file_unit, '(a)') 'Version: '//trim(version(5:))
      write (file_unit, *) '-----------------------------------------------'

      write (file_unit, '(''Maak file met Complexe getallen'')')

      coding = 'N'
      call clock(cpu1)
!
      error = Opndef(fds, 'data_c09.def', coding)
      if (error /= 0) goto 9999

      error = Defelm(fds, 'ELEM_R_4', 'COMPLEX', 16,&
      &'GROOTHEID 1', 'eenheid 1', 'Beschrijving 1',&
      &0, idum)
      if (error /= 0) goto 9999

      error = Defcel(fds, 'CEL_TEST_1', 1, 'ELEM_R_4')
      if (error /= 0) goto 9999

      error = Defgrp(fds, 'GRP_TEST_1', 'CEL_TEST_1', 1, 1000, 1)
      if (error /= 0) goto 9999

      error = Opndat(datfds, 'data_c09.dat', coding)
      if (error /= 0) goto 9999

      error = Credat(fds, 'DATAGRP_TEST_1A', 'GRP_TEST_1')
      if (error /= 0) goto 9999

      error = Credat(fds, 'DATAGRP_TEST_1B', 'GRP_TEST_1')
      if (error /= 0) goto 9999
!
      call clock(cpu2)
      write (file_unit, '(''Initialisation NEFIS files [sec]'',1PE13.5)')&
      &cpu2 - cpu1

      write (file_unit, '(''Schrijf elementen'')')
      call clock(cpu1)

      UINDEX(incr, 1) = 1
      do i = 1, 1000
         UINDEX(start, 1) = i
         UINDEX(stop, 1) = i
         val = (10.0, 15.0)
         error = Putelt(fds, 'DATAGRP_TEST_1A', 'ELEM_R_4',&
         &UINDEX, 1, val)
         if (error /= 0) goto 9999
      end do
      call clock(cpu2)
      write (file_unit, '(''DATAGRP_TEST_1A written in [sec]'',1PE13.5)')&
      &cpu2 - cpu1
!
      call clock(cpu1)
      do i = 1000, 1, -1
         UINDEX(start, 1) = i
         UINDEX(stop, 1) = i
         val = (1.0, 1.0)
         error = Putelt(fds, 'DATAGRP_TEST_1B', 'ELEM_R_4',&
         &UINDEX, 1, val)
         if (error /= 0) goto 9999
      end do
!
      call clock(cpu2)
      write (file_unit, '(''DATAGRP_TEST_1B written in [sec]'',1PE13.5)')&
      &cpu2 - cpu1
!
      write (file_unit, '(''Lees elementen'')')
!
      call clock(cpu2)
      do i = 1000, 1, -1
         UINDEX(start, 1) = i
         UINDEX(stop, 1) = i
         error = Getelt(fds, 'DATAGRP_TEST_1A', 'ELEM_R_4',&
         &UINDEX, 1, 16, val)
         if (error /= 0) goto 9999
      end do
!
      call clock(cpu2)
      write (file_unit, '(''DATAGRP_TEST_1A read    in [sec]'',1PE13.5)')&
      &cpu2 - cpu1
!
      call clock(cpu1)
      do i = 1, 1000
         UINDEX(start, 1) = i
         UINDEX(stop, 1) = i
         error = Getelt(fds, 'DATAGRP_TEST_1B', 'ELEM_R_4',&
         &UINDEX, 1, 16, val)
         if (error /= 0) goto 9999
      end do
      call clock(cpu2)
      write (file_unit, '(''DATAGRP_TEST_1B read    in [sec]'',1PE13.5)')&
      &cpu2 - cpu1
!

9999  continue

      if (error /= 0) error = neferr(1, errstr)

      error = Clsdat(fds)
      error = Clsdef(fds)
      error = neferr(0, errstr)
      write (file_unit, '(a)') trim(errstr)

      close (file_unit)
      !
      call compare_text_files(filename1, filename2, skiplines)

   end subroutine test_09
   !$f90tw)

end module m_nefis_test_09
