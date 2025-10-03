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

module m_nefis_test_05
   use assertions_gtest
   use tests_nefis_helper
   implicit none

   character(len=30), dimension(3) :: skiplines = ["Version", "version", "-----"]

contains

   !$f90tw TESTCODE(TEST, nefis_tests, test_05, test_05,
   subroutine test_05() bind(C)
      integer start, stop, incr
      parameter(start=1, stop=2, incr=3)
      integer clsdat,&
      &clsdef,&
      &credat,&
      &defcel,&
      &defelm,&
      &defgrp,&
      &flsdat,&
      &flsdef,&
      &getnfv,&
      &getelt
      integer neferr,&
      &opndat,&
      &opndef,&
      &putelt
      integer error,&
      &idum,&
      &i, j,&
      &elmdms(5),&
      &UINDEX(3, 1),&
      &fds
      real buffer(748)
      character names(3) * 14, coding * 1
      character ERRSTR * 1024
      character * 255 version
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

      error = getnfv(version)
      write (file_unit, *) '-----------------------------------------------'
      write (file_unit, '(a)') 'Version: '//trim(version(5:))
      write (file_unit, *) '-----------------------------------------------'

      coding = ' '
      error = Opndef(fds, 'nefis_ex.def', coding)
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
!
      error = Opndat(fds, 'nefis_ex.dat', coding)
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
!
      error = Defelm(fds, 'ELEM_R_4_DIM_1', 'REAL', 4,&
      &'GROOTHEID 2', 'eenheid 2', 'Beschrijving 2',&
      &1, 3)
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
!
      elmdms(1) = 5
      elmdms(2) = 5
      error = Defelm(fds, 'ELEM_R_4_DIM_2', 'REAL', 4,&
      &'GROOTHEID 3', 'eenheid 3', 'Beschrijving 3',&
      &2, elmdms)
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
!
      elmdms(1) = 2
      elmdms(2) = 3
      elmdms(3) = 4
      elmdms(4) = 5
      elmdms(5) = 6
      error = Defelm(fds, 'ELEM_R_4_DIM_5', 'REAL', 4,&
      &'GROOTHEID 4', 'eenheid 4', 'Beschrijving 4',&
      &5, elmdms)
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
!
      names(1) = 'ELEM_R_4_DIM_1'
      names(2) = 'ELEM_R_4_DIM_2'
      names(3) = 'ELEM_R_4_DIM_5'
      error = Defcel(fds, 'CEL_TEST_2', 3, names)
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
!
      error = Defgrp(fds, 'GRP_TEST_2A', 'CEL_TEST_2', 0, idum, idum)
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
!
      error = Defgrp(fds, 'GRP_TEST_2B', 'CEL_TEST_2', 1, 100, 1)
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
!
      error = Credat(fds, 'DATAGRP_TEST_2A', 'GRP_TEST_2A')
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
!
      error = Credat(fds, 'DATAGRP_TEST_2B', 'GRP_TEST_2B')
      if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
!
      do 10 i = 1, 748
         buffer(i) = i
10       continue
!
         write (file_unit, '(''schrijf DATAGRP_TEST_2A'')')
         UINDEX(start, 1) = 1
         UINDEX(stop, 1) = 1
         UINDEX(incr, 1) = 1
         error = Putelt(fds, 'DATAGRP_TEST_2A', '*',&
         &UINDEX, 1, buffer)
         if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
!
         write (file_unit, '(''schrijf DATAGRP_TEST_2B'')')
         do 30 i = 1, 100
            UINDEX(start, 1) = i
            UINDEX(stop, 1) = i
            UINDEX(incr, 1) = 1
            do 20 j = 1, 748
               buffer(j) = real(i) * real(j)
20             continue
               error = Putelt(fds, 'DATAGRP_TEST_2B', '*',&
               &UINDEX, 1, buffer)
               if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
30             continue
               error = flsdat(fds)
               error = flsdef(fds)
!
               write (file_unit, '(''lees DATAGRP_TEST_2B'')')
               do 50 i = 100, 1, -1
                  UINDEX(start, 1) = i
                  UINDEX(stop, 1) = i
                  error = Getelt(fds, 'DATAGRP_TEST_2B', '*',&
                  &UINDEX, 1, 748 * 4, buffer)
                  if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                  do 40 j = 1, 748
                     if (int(buffer(j) / real(i) - j) /= 0)&
                     &write (file_unit, '(''error, i='',i3)') i
40                   continue
50                   continue
!
                     write (file_unit, '(''lees DATAGRP_TEST_2A'')')
                     UINDEX(start, 1) = 1
                     UINDEX(stop, 1) = 1
                     error = Getelt(fds, 'DATAGRP_TEST_2A', '*',&
                     &UINDEX, 1, 748 * 4, buffer)
                     if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
                     do 60 j = 1, 748
!      PRINT *, buffer(j),j, INT(buffer(j)-j)
                        if (int(buffer(j) - j) /= 0) print *, 'error, i= ', i
60                      continue

                        write (file_unit, *)
                        error = Clsdat(fds)
                        if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
!
                        error = Clsdef(fds)
                        if (ERROR /= 0) ERROR = NEFERR(1, ERRSTR)
!
                        ERROR = NEFERR(0, ERRSTR)
                        write (file_unit, '(a)') trim(trim_line_endings(errstr))
                        write (file_unit, *) '-----------------------------------------------'

                        close (file_unit)
                        !
                        call compare_text_files(filename1, filename2, skiplines)

                        end subroutine test_05
                        !$f90tw)

                        end module m_nefis_test_05
