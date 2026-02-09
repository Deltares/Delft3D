!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2026.
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

module m_nefis_tests
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
      &Neferr,&
      &Reserr
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
      filename1 = 'test04.out'
      filename2 = 'test04.scr'
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
      error = reserr()
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

9999  continue

      error = Clsdat(fds)
      error = Clsdef(fds)
      error = Neferr(0, errstr)
      write (file_unit, '(a)') trim(errstr)
      close (file_unit)
      !
      call compare_text_files(filename1, filename2, skiplines)

   end subroutine test_04
   !$f90tw)

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
      &getelt,&
      &reserr
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
      filename1 = 'test05.out'
      filename2 = 'test05.scr'
      call delete_file(filename1)
      call delete_file('nefis_ex.def')
      call delete_file('nefis_ex.dat')

      open (newunit=file_unit, file=filename1)

      error = reserr()
      error = getnfv(version)
      write (file_unit, *) '-----------------------------------------------'
      write (file_unit, '(a)') 'Version: '//trim(version(5:))
      write (file_unit, *) '-----------------------------------------------'

      coding = ' '
      error = Opndef(fds, 'nefis_ex.def', coding)
      if (error /= 0) goto 9999
!
      error = Opndat(fds, 'nefis_ex.dat', coding)
      if (error /= 0) goto 9999
!
      error = Defelm(fds, 'ELEM_R_4_DIM_1', 'REAL', 4,&
      &'GROOTHEID 2', 'eenheid 2', 'Beschrijving 2',&
      &1, 3)
      if (error /= 0) goto 9999
!
      elmdms(1) = 5
      elmdms(2) = 5
      error = Defelm(fds, 'ELEM_R_4_DIM_2', 'REAL', 4,&
      &'GROOTHEID 3', 'eenheid 3', 'Beschrijving 3',&
      &2, elmdms)
      if (error /= 0) goto 9999
!
      elmdms(1) = 2
      elmdms(2) = 3
      elmdms(3) = 4
      elmdms(4) = 5
      elmdms(5) = 6
      error = Defelm(fds, 'ELEM_R_4_DIM_5', 'REAL', 4,&
      &'GROOTHEID 4', 'eenheid 4', 'Beschrijving 4',&
      &5, elmdms)
      if (error /= 0) goto 9999
!
      names(1) = 'ELEM_R_4_DIM_1'
      names(2) = 'ELEM_R_4_DIM_2'
      names(3) = 'ELEM_R_4_DIM_5'
      error = Defcel(fds, 'CEL_TEST_2', 3, names)
      if (error /= 0) goto 9999
!
      error = Defgrp(fds, 'GRP_TEST_2A', 'CEL_TEST_2', 0, idum, idum)
      if (error /= 0) goto 9999
!
      error = Defgrp(fds, 'GRP_TEST_2B', 'CEL_TEST_2', 1, 100, 1)
      if (error /= 0) goto 9999
!
      error = Credat(fds, 'DATAGRP_TEST_2A', 'GRP_TEST_2A')
      if (error /= 0) goto 9999
!
      error = Credat(fds, 'DATAGRP_TEST_2B', 'GRP_TEST_2B')
      if (error /= 0) goto 9999
!
      do i = 1, 748
         buffer(i) = i
      end do
!
      write (file_unit, '(''schrijf DATAGRP_TEST_2A'')')
      UINDEX(start, 1) = 1
      UINDEX(stop, 1) = 1
      UINDEX(incr, 1) = 1
      error = Putelt(fds, 'DATAGRP_TEST_2A', '*',&
      &UINDEX, 1, buffer)
      if (error /= 0) goto 9999
!
      write (file_unit, '(''schrijf DATAGRP_TEST_2B'')')
      do i = 1, 100
         UINDEX(start, 1) = i
         UINDEX(stop, 1) = i
         UINDEX(incr, 1) = 1
         do j = 1, 748
            buffer(j) = real(i) * real(j)
         end do
         error = Putelt(fds, 'DATAGRP_TEST_2B', '*',&
         &UINDEX, 1, buffer)
         if (error /= 0) goto 9999
      end do
      error = flsdat(fds)
      error = flsdef(fds)
!
      write (file_unit, '(''lees DATAGRP_TEST_2B'')')
      do i = 100, 1, -1
         UINDEX(start, 1) = i
         UINDEX(stop, 1) = i
         error = Getelt(fds, 'DATAGRP_TEST_2B', '*',&
         &UINDEX, 1, 748 * 4, buffer)
         if (error /= 0) goto 9999
         do j = 1, 748
            if (int(buffer(j) / real(i) - j) /= 0)&
            &write (file_unit, '(''error, i='',i3)') i
         end do
      end do
!
      write (file_unit, '(''lees DATAGRP_TEST_2A'')')
      UINDEX(start, 1) = 1
      UINDEX(stop, 1) = 1
      error = Getelt(fds, 'DATAGRP_TEST_2A', '*',&
      &UINDEX, 1, 748 * 4, buffer)
      if (error /= 0) goto 9999
      do j = 1, 748
!      PRINT *, buffer(j),j, INT(buffer(j)-j)
         if (int(buffer(j) - j) /= 0) print *, 'error, i= ', i
      end do

      write (file_unit, *)
9999  continue
      error = Clsdat(fds)
      error = Clsdef(fds)
      ERROR = NEFERR(0, ERRSTR)
      write (file_unit, '(a)') trim(trim_line_endings(errstr))
      write (file_unit, *) '-----------------------------------------------'

      close (file_unit)
      !
      call compare_text_files(filename1, filename2, skiplines)

   end subroutine test_05
   !$f90tw)

   !$f90tw TESTCODE(TEST, nefis_tests, test_07, test_07,
   subroutine test_07() bind(C)
      integer * 4 fds
      integer clsdat,&
      &clsdef,&
      &getnfv,&
      &getiat,&
      &getrat
      integer getsat,&
      &opndat,&
      &opndef,&
      &putiat,&
      &putrat,&
      &putsat,&
      &neferr,&
      &reserr
      integer error, ival
      character attrib * 16, attval * 16, coding * 1
      real rval
      character ERRSTR * 1024
      character * 255 version
      character(len=30) :: filename1
      character(len=30) :: filename2
      integer file_unit

      ! delete previous output files if they exist
      filename1 = 'test07.out'
      filename2 = 'test07.scr'
      call delete_file(filename1)

      open (newunit=file_unit, file=filename1)

      error = reserr()
      error = getnfv(version)
      write (file_unit, *) '-----------------------------------------------'
      write (file_unit, '(a)') 'Version: '//trim(version(5:))
      write (file_unit, *) '-----------------------------------------------'

      coding = ' '
      error = Opndef(fds, 'nefis_ex.def', coding)
      if (error /= 0) goto 9999

      error = Opndat(fds, 'nefis_ex.dat', coding)
      if (error /= 0) goto 9999

      error = Putiat(fds, 'DATAGRP_TEST_3A',&
      &'INTEGER ATTRIB 1', 101)
      if (error /= 0) goto 9999

      error = Putiat(fds, 'DATAGRP_TEST_3A',&
      &'INTEGER ATTRIB 2', 102)
      if (error /= 0) goto 9999

      error = Putiat(fds, 'DATAGRP_TEST_3A',&
      &'INTEGER ATTRIB 3', 103)
      if (error /= 0) goto 9999

      error = Putiat(fds, 'DATAGRP_TEST_3A',&
      &'INTEGER ATTRIB 4', 104)
      if (error /= 0) goto 9999

      error = Putiat(fds, 'DATAGRP_TEST_3A',&
      &'INTEGER ATTRIB 5', 105)
      if (error /= 0) goto 9999

      error = Putrat(fds, 'DATAGRP_TEST_3B',&
      &'REAL ATTRIBUUT 1', 201.)
      if (error /= 0) goto 9999

      error = Putrat(fds, 'DATAGRP_TEST_3B',&
      &'REAL ATTRIBUUT 2', 202.)
      if (error /= 0) goto 9999

      error = Putrat(fds, 'DATAGRP_TEST_3B',&
      &'REAL ATTRIBUUT 3', 203.)
      if (error /= 0) goto 9999

      error = Putrat(fds, 'DATAGRP_TEST_3B',&
      &'REAL ATTRIBUUT 4', 204.)
      if (error /= 0) goto 9999

      error = Putrat(fds, 'DATAGRP_TEST_3B',&
      &'REAL ATTRIBUUT 5', 205.)
      if (error /= 0) goto 9999

      error = Putsat(fds, 'DATAGRP_TEST_3C',&
      &'TEXT ATTRIBUUT 1', 'ATR1')
      if (error /= 0) goto 9999

      error = Putsat(fds, 'DATAGRP_TEST_3C',&
      &'TEXT ATTRIBUUT 2', 'ATR2')
      if (error /= 0) goto 9999

      error = Putsat(fds, 'DATAGRP_TEST_3C',&
      &'TEXT ATTRIBUUT 3', 'ATR3')
      if (error /= 0) goto 9999

      error = Putsat(fds, 'DATAGRP_TEST_3C',&
      &'TEXT ATTRIBUUT 4', 'ATR4')
      if (error /= 0) goto 9999

      error = Putsat(fds, 'DATAGRP_TEST_3C',&
      &'TEXT ATTRIBUUT 5', 'ATR5')
      if (error /= 0) goto 9999

      error = Putsat(fds, 'DATAGRP_TEST_3A',&
      &'TEXT ATTRIBUUT 1', 'DATAGRP_TEST_3C')
      if (error /= 0) goto 9999
!
!     Get  text attributes
!
      error = Getsat(fds, 'DATAGRP_TEST_3A',&
      &'TEXT ATTRIBUUT 1', attrib)
      if (attrib /= 'DATAGRP_TEST_3C')&
      &write (file_unit, *) 'Attribute value (=DATA_GRP_TEST_3C): ', attrib
      if (error /= 0) goto 9999

      error = Getsat(fds, attrib,&
      &'TEXT ATTRIBUUT 3', attval)
      if (attval /= 'ATR3')&
      &write (file_unit, *) 'Attribute value (=ATR3): ', attval
      if (error /= 0) goto 9999
!
!     Get  integer attributes
!
      error = Getiat(fds, 'DATAGRP_TEST_3A',&
      &'INTEGER ATTRIB 1', ival)
      if (ival /= 101)&
      &write (file_unit, *) 'Attribute value (=101): ', ival
      if (error /= 0) goto 9999

      error = Getiat(fds, 'DATAGRP_TEST_3A',&
      &'INTEGER ATTRIB 2', ival)
      if (ival /= 102)&
      &write (file_unit, *) 'Attribute value (=102): ', ival
      if (error /= 0) goto 9999
!
!     Put integer attributes
!
      error = Putiat(fds, 'DATAGRP_TEST_3B',&
      &'INTEGER ATTRIB 1', 1000)
      if (error /= 0) goto 9999

      error = Putiat(fds, 'DATAGRP_TEST_3C',&
      &'INTEGER ATTRIB 1', 1001)
      if (error /= 0) goto 9999
!
!     Get integer attributes
!
      error = Getiat(fds, 'DATAGRP_TEST_3B',&
      &'INTEGER ATTRIB 1', ival)
      if (ival /= 1000)&
      &write (file_unit, *) 'Attribute value (=1000): ', ival
      if (error /= 0) goto 9999

      error = Getiat(fds, 'DATAGRP_TEST_3C',&
      &'INTEGER ATTRIB 1', ival)
      if (ival /= 1001)&
      &write (file_unit, *) 'Attribute value (=1001): ', ival
      if (error /= 0) goto 9999
!
!     Get  real attributes
!
      error = Getrat(fds, 'DATAGRP_TEST_3B',&
      &'REAL ATTRIBUUT 1', rval)
      if (rval /= 201.)&
      &write (file_unit, *) 'Attribute value (=201.): ', rval
      if (error /= 0) goto 9999

      error = Getrat(fds, 'DATAGRP_TEST_3B',&
      &'REAL ATTRIBUUT 2', rval)
      if (rval /= 202.)&
      &write (file_unit, *) 'Attribute value (=202.): ', rval
      if (error /= 0) goto 9999

      error = Getrat(fds, 'DATAGRP_TEST_3B',&
      &'REAL ATTRIBUUT 5', rval)
      if (rval /= 205.)&
      &write (file_unit, *) 'Attribute value (=205.): ', rval
      if (error /= 0) goto 9999

      error = Clsdat(fds)
      if (error /= 0) goto 9999

      error = Clsdef(fds)
      if (error /= 0) goto 9999

      goto 8888

9999  continue
      write (file_unit, *) ' Error detected in program Test7'
8888  continue

      error = neferr(0, errstr)
      write (file_unit, *)
      write (file_unit, '(a)') trim(errstr)

      close (file_unit)
      !
      call compare_text_files(filename1, filename2, skiplines)

   end subroutine test_07
   !$f90tw)

   !$f90tw TESTCODE(TEST, nefis_tests, test_08, test_08,
   subroutine test_08() bind(C)
      integer START, stop, INCR

      character CODING * 1, ELMTYP * 16, ELMQTY * 16, ELMUNT * 16,&
      &ELMDES * 64, ELMNMS(5) * 16, CELNAM * 16,&
      &GRPNAM * 16, GRPDEF * 16

      integer NBYTSG, I, NELEMS, J, K
      integer ERROR, ELMNDM, ELMDMS(5), GRPNDM, UINDEX(3, 1),&
      &GRPDMS(142), GRPORD(5), IARRIN(142), IARROU(142)

      real ARRAY(142, 65), ARROUT(142, 65)
      character ERRSTR * 1024

      integer FDS

      integer OPNDAT, OPNDEF, DEFELM, CLSDEF, CLSDAT, DEFCEL,&
      &DEFGRP, INQELM, INQCEL, INQGRP, FLSDAT, FLSDEF,&
      &CREDAT, PUTIAT, PUTSAT, PUTELT, GETELT, PUTRAT,&
      &INQFST, INQNXT, NEFERR, GETNFV, RESERR

      character * 255 version
      character(len=30) :: filename1
      character(len=30) :: filename2
      integer file_unit

      ! delete previous output files if they exist
      filename1 = 'test08.out'
      filename2 = 'test08.scr'
      call delete_file(filename1)
      call delete_file('data_c08.def')
      call delete_file('data_c08.dat')

      open (newunit=file_unit, file=filename1)
      error = reserr()
      error = getnfv(version)
      write (file_unit, *) '-----------------------------------------------'
      write (file_unit, '(a)') 'Version: '//trim(version(5:))
      write (file_unit, *) '-----------------------------------------------'

      start = 1
      stop = 2
      incr = 3
      CODING = 'N'
      ELMNDM = 5
      NELEMS = 5
      GRPNDM = 5
      NBYTSG = 0
      errstr = ' '

      write (file_unit, '(''TEST8: Open files'')')
      ERROR = OPNDAT(FDS, 'data_c08.dat', CODING)
      if (error /= 0) write (file_unit, *) ' OPNDAT:', error
      if (error /= 0) goto 9999

      ERROR = OPNDEF(FDS, 'data_c08.def', CODING)
      if (error /= 0) write (file_unit, *) ' OPNDEF:', error
      if (error /= 0) goto 9999

      ELMDMS(1) = 142
      ELMDMS(2) = 65

      write (file_unit, '(''TEST8: Define elements'')')
      ERROR = DEFELM(FDS, 'Elmnam', 'ReaL', 4, 'Elmqty',&
      &'Elmunt', 'Elmdes', 2, ELMDMS)
      if (error /= 0) write (file_unit, *) ' DEFELM: Elmnam'
      if (error /= 0) goto 9999

      ERROR = DEFELM(FDS, 'ElmInt', 'IntEgeR', 4, 'Elmqty',&
      &'Elmunt', 'Elmdes', 1, 142)
      if (error /= 0) write (file_unit, *) ' DEFELM: ElmInt'
      if (error /= 0) goto 9999

      write (file_unit, '(''TEST8: Define cells'')')
      ELMNMS(1) = 'Elmnam'
      ELMNMS(2) = 'ElmInt'
      ERROR = DEFCEL(FDS, 'Celnam', 2, ELMNMS)
      if (error /= 0) write (file_unit, *) ' DEFCEL: Celnam'
      if (error /= 0) goto 9999

      write (file_unit, '(''TEST8: Define groups'')')
!     ** Variable dimensie **
      ERROR = DEFGRP(FDS, 'Grpdef', 'Celnam', 1, 0, 1)
      if (error /= 0) write (file_unit, *) ' DEFGRP: Grpdef'
      if (error /= 0) goto 9999

      ERROR = FLSDEF(FDS)
      if (error /= 0) write (file_unit, *) ' FLSDEF'
      if (error /= 0) goto 9999

      write (file_unit, '(''TEST8: Inquire element'')')
      ERROR = INQELM(FDS, 'Elmnam', ELMTYP, NBYTSG, ELMQTY,&
      &ELMUNT, ELMDES, ELMNDM, ELMDMS)
      if (error /= 0)&
      &write (file_unit, *) ' INQELM:', error, elmtyp, nbytsg, elmqty, elmunt&
      &, elmdes, elmndm, (elmdms(i), i=1, elmndm)
      if (error /= 0) goto 9999

      write (file_unit, '(''TEST8: Inquire cell'')')
      NELEMS = 2
      ERROR = INQCEL(FDS, 'Celnam', NELEMS, ELMNMS)
      if (error /= 0)&
      &write (file_unit, *) ' INQCEL:', error, nelems, (elmnms(i), i=1, nelems)
      if (error /= 0) goto 9999

      write (file_unit, '(''TEST8: Inquire group'')')
      ERROR = INQGRP(FDS, 'Grpdef', CELNAM, GRPNDM, GRPDMS, GRPORD)
      if (error /= 0) write (file_unit, *) ' INQGRP:', error, celnam, grpndm,&
      &(grpdms(i), i=1, grpndm), (grpord(i), i=1, grpndm)
      if (error /= 0) goto 9999

      write (file_unit, '(''TEST8: Create group on data file'')')
      ERROR = CREDAT(FDS, 'Grpnam', 'Grpdef')
      if (error /= 0) write (file_unit, *) ' Credat: Grpnam'
      if (error /= 0) goto 9999
      ERROR = CREDAT(FDS, 'aaaabbbb', 'Grpdef')
      if (error /= 0) write (file_unit, *) ' Credat: aaaabbbb'
      if (error /= 0) goto 9999
      ERROR = CREDAT(FDS, 'bbbbaaaa', 'Grpdef')
      if (error /= 0) write (file_unit, *) ' Credat: bbbbaaaa'
      if (error /= 0) goto 9999
      ERROR = CREDAT(FDS, 'babaabab', 'Grpdef')
      if (error /= 0) write (file_unit, *) ' Credat: babaabab'
      if (error /= 0) goto 9999
      ERROR = CREDAT(FDS, 'Grpnam  Grpnam', 'Grpdef')
      if (error /= 0) write (file_unit, *) ' Credat: Grpnam  Grpnam'
      if (error /= 0) goto 9999

!     ERROR = PUTIAT (FDS, 'Grpnam', 'IAttrib_1', -5432)
!     ERROR = PUTIAT (FDS, 'Grpnam', 'IAttrib_2', 12345)
!     ERROR = PUTIAT (FDS, 'Grpnam', 'IAttrib_3', 3)
!     ERROR = PUTIAT (FDS, 'Grpnam', 'IAttrib_4', -4)
!     ERROR = PUTIAT (FDS, 'Grpnam', 'IAttrib_5', 5)
!     ERROR = PUTIAT (FDS, 'Grpnam', 'IAttrib_1', 1)
!
!     length attribute name equal to 19, first 16 are equal
!     so all attributes are the same and the value will be
!     overwritten
!
      write (file_unit, '(''TEST8: Put attributes'')')
      ERROR = PUTIAT(FDS, 'Grpnam',&
      &'INTEGER ATTRIBUUT 1', 1)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, '(a)') trim(errstr)
      end if
      ERROR = PUTIAT(FDS, 'Grpnam',&
      &'INTEGER ATTRIBUUT 2', 2)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, '(a)') trim(errstr)
      end if
      ERROR = PUTIAT(FDS, 'Grpnam',&
      &'INTEGER ATTRIBUUT 3', 3)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, '(a)') trim(errstr)
      end if
      ERROR = PUTIAT(FDS, 'Grpnam',&
      &'INTEGER ATTRIBUUT 4', 4)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, '(a)') trim(errstr)
      end if
      ERROR = PUTIAT(FDS, 'Grpnam',&
      &'INTEGER ATTRIBUUT 5', 5)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, '(a)') trim(errstr)
      end if
      ERROR = PUTIAT(FDS, 'Grpnam',&
      &'INTEGER ATTRIBUU', 5)
      if (error /= 0) write (file_unit, *) ' PUTIAT 5:', error
      if (error /= 0) goto 9999

      ERROR = PUTRAT(FDS, 'Grpnam', 'RAttrib_1', 12.345)
      if (error /= 0) goto 9999
      ERROR = PUTRAT(FDS, 'Grpnam', 'RAttrib_2', -2.2)
      if (error /= 0) goto 9999
      ERROR = PUTRAT(FDS, 'Grpnam', 'RAttrib_3', 3.3)
      if (error /= 0) goto 9999
      ERROR = PUTRAT(FDS, 'Grpnam', 'RAttrib_4', -4.4)
      if (error /= 0) goto 9999
      ERROR = PUTRAT(FDS, 'Grpnam', 'RAttrib_5', 5.5)
      if (error /= 0) goto 9999

      ERROR = PUTSAT(FDS, 'Grpnam', 'SAttrib_1', 'String 1')
      if (error /= 0) goto 9999
      ERROR = PUTSAT(FDS, 'Grpnam', 'SAttrib_2', 'String 2')
      if (error /= 0) goto 9999
      ERROR = PUTSAT(FDS, 'Grpnam', 'SAttrib_3', 'String 3')
      if (error /= 0) goto 9999
      ERROR = PUTSAT(FDS, 'Grpnam', 'SAttrib_4', 'String 4')
      if (error /= 0) goto 9999
      ERROR = PUTSAT(FDS, 'Grpnam', 'SAttrib_5', 'String 5')
      if (error /= 0) goto 9999

      ERROR = PUTRAT(FDS, 'bbbbaaaa', 'RAttrib_1', 12.347)
      if (error /= 0) goto 9999
      ERROR = PUTRAT(FDS, 'aaaabbbb', 'RAttrib_1', 12.346)
      if (error /= 0) goto 9999
      ERROR = PUTRAT(FDS, 'babaabab', 'RAttrib_1', 12.348)
      if (error /= 0) goto 9999
      ERROR = PUTRAT(FDS, 'Grpnam  Grpnam', 'RAttrib_1', 12.349)
      if (error /= 0) goto 9999
      ERROR = PUTSAT(FDS, 'aaaabbbb', 'SAttrib_1', 'String 6')
      if (error /= 0) goto 9999
      ERROR = PUTSAT(FDS, 'bbbbaaaa', 'SAttrib_1', 'String 11')
      if (error /= 0) goto 9999
      ERROR = PUTSAT(FDS, 'babaabab', 'SAttrib_1', 'String 16')
      if (error /= 0) goto 9999
      ERROR = PUTSAT(FDS, 'Grpnam  Grpnam', 'SAttrib_1', 'String 21')
      if (error /= 0) goto 9999
      ERROR = PUTIAT(FDS, 'aaaabbbb', 'IAttrib_1', -6)
      if (error /= 0) goto 9999
      ERROR = PUTIAT(FDS, 'bbbbaaaa', 'IAttrib_1', -11)
      if (error /= 0) goto 9999
      ERROR = PUTIAT(FDS, 'babaabab', 'IAttrib_1', -16)
      if (error /= 0) goto 9999
      ERROR = PUTIAT(FDS, 'Grpnam  Grpnam', 'IAttrib_1', -21)
      if (error /= 0) goto 9999

!     *** GeT All Attributes of all Groups of the Datafile
      write (file_unit, '(''TEST8: Get attributes'')')
      call GTALAT(FDS, file_unit)

      error = neferr(0, errstr)
      write (file_unit, '(a)') trim(errstr)

      write (file_unit, '(''TEST8: Put real and integer array'')')
      UINDEX(INCR, 1) = 1
      do J = 1, 10
         UINDEX(START, 1) = J
         UINDEX(stop, 1) = J
         do I = 1, 142
            do K = 1, 65
               ARRAY(I, K) = J * 1000.+I + K / 1000.
            end do !end do
            IARRIN(I) = J * 1000 + I
         end do

         ERROR = PUTELT(FDS, 'Grpnam',&
         &'Elmnam', UINDEX, 1, ARRAY)
         if (error /= 0) write (file_unit, *) ' PUTELT:', error
         if (error /= 0) goto 9999

         ERROR = PUTELT(FDS, 'Grpnam',&
         &'ElmInt', UINDEX, 1, IARRIN)
         if (error /= 0) write (file_unit, *) ' PUTELT:', error
         if (error /= 0) goto 9999

         ERROR = FLSDAT(FDS)
         if (error /= 0) write (file_unit, *) ' FLSDAT:', error
         if (error /= 0) goto 9999

      end do

      write (file_unit, '(''TEST8: Get real and integer array'')')
      do J = 1, 10
         UINDEX(START, 1) = J
         UINDEX(stop, 1) = J
         ERROR = GETELT(FDS, 'Grpnam',&
         &'Elmnam', UINDEX, 1, 142 * 65 * 4, ARROUT)
         if (error /= 0) write (file_unit, '('' GETELT:'',i4,1x,3(1pe14.6,1x))')&
         &error, arrout(142, 65), arrout(142, 64), arrout(141, 65)
         if (error /= 0) goto 9999

         ERROR = GETELT(FDS, 'Grpnam',&
         &'ElmInt', UINDEX, 1, 142 * 4, IARROU)
         if (error /= 0)&
         &write (file_unit, *) ' GETELT:', error, iarrou(1), iarrou(142)
         if (error /= 0) goto 9999

      end do

      write (file_unit, '(''TEST8: Flush memory to files'')')

      ERROR = FLSDAT(FDS)
      if (error /= 0) write (file_unit, *) ' FLSDAT:', error
      if (error /= 0) goto 9999

      ERROR = FLSDEF(FDS)
      if (error /= 0) write (file_unit, *) ' FLSDEF:', error
      if (error /= 0) goto 9999

      write (file_unit, '(''TEST8: Loop all groups'')')

      ERROR = INQFST(FDS, GRPNAM, GRPDEF)
      if (error /= 0) write (file_unit, *) ' INQFST:', error, grpnam, grpdef

      ERROR = INQNXT(FDS, GRPNAM, GRPDEF)
      if (error /= 0) write (file_unit, *) ' INQNXT:', error, grpnam, grpdef

      ERROR = INQNXT(FDS, GRPNAM, GRPDEF)
      if (error /= 0) write (file_unit, *) ' INQNXT:', error, grpnam, grpdef

      ERROR = INQNXT(FDS, GRPNAM, GRPDEF)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, '(a)') trim(errstr)
      end if

      ERROR = INQNXT(FDS, GRPNAM, GRPDEF)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, '(a)') trim(errstr)
      end if

      ERROR = INQNXT(FDS, GRPNAM, GRPDEF)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, '(a)') trim(errstr)
      end if

!     write(file_unit,'('' SO FAR SO GOOD'
      goto 8888
9999  continue
      error = neferr(0, errstr)
      write (file_unit, '(a)') trim(errstr)
      write (file_unit, '('' NOT SO GOOD'')')
8888  continue

      ERROR = CLSDAT(FDS)
      if (error /= 0) write (file_unit, *) ' CLSDAT:', error

      ERROR = CLSDEF(FDS)
      if (error /= 0) write (file_unit, *) ' CLSDEF:', error

      error = neferr(0, errstr)
      write (file_unit, '(a)') trim(errstr)

      close (file_unit)
      !
      call compare_text_files(filename1, filename2, skiplines)

   end subroutine test_08
   !$f90tw)

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
      &Clsdef,&
      &Reserr
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
      filename1 = 'test09.out'
      filename2 = 'test09.scr'
      call delete_file(filename1)
      call delete_file('data_c09.def')
      call delete_file('data_c09.dat')

      open (newunit=file_unit, file=filename1)

      error = reserr()
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

      error = Clsdat(fds)
      error = Clsdef(fds)
      error = neferr(0, errstr)
      write (file_unit, '(a)') trim(errstr)

      close (file_unit)
      !
      call compare_text_files(filename1, filename2, skiplines)

   end subroutine test_09
   !$f90tw)

   !$f90tw TESTCODE(TEST, nefis_tests, test_12, test_12,
   subroutine test_12() bind(C)
      integer * 4 fds_a,&
      &fds_b,&
      &fds_c
      integer clsdat,&
      &clsdef,&
      &getnfv,&
      &NEFERR,&
      &reserr
      integer error
      character ERRSTR * 1024
      character * 255 version
      character(len=30) :: filename1
      character(len=30) :: filename2
      integer file_unit

      ! delete previous output files if they exist
      filename1 = 'test12.out'
      filename2 = 'test12.scr'
      call delete_file(filename1)
      call delete_file('data_c12a.def')
      call delete_file('data_c12a.dat')
      call delete_file('data_c12b.def')
      call delete_file('data_c12b.dat')
      call delete_file('data_c12c.def')
      call delete_file('data_c12c.dat')
      open (newunit=file_unit, file=filename1)

      error = reserr()
      error = getnfv(version)
      write (file_unit, *) '-----------------------------------------------'
      write (file_unit, '(a)') 'Version: '//trim(version(5:))
      write (file_unit, *) '-----------------------------------------------'

      call WriteFile('data_c12a', fds_a, 33, file_unit)
      call WriteFile('data_c12b', fds_b, 39, file_unit)
      call WriteFile('data_c12c', fds_c, 78, file_unit)

      call ReadFile(fds_a, 33, file_unit)
      call ReadFile(fds_b, 39, file_unit)
      call ReadFile(fds_c, 78, file_unit)

      error = Clsdat(fds_a)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, *) trim(errstr)
      end if

      error = Clsdat(fds_b)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, *) trim(errstr)
      end if

      error = Clsdat(fds_c)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, *) trim(errstr)
      end if

      error = Clsdef(fds_a)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, *) trim(errstr)
      end if

      error = Clsdef(fds_b)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, *) trim(errstr)
      end if

      error = Clsdef(fds_c)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, *) trim(errstr)
      end if

      if (error == 0) then
         error = neferr(0, errstr)
         write (file_unit, *)
         write (file_unit, '(a)') trim(errstr)
      end if

      close (file_unit)
      !
      call compare_text_files(filename1, filename2, skiplines)

   end subroutine test_12
   !$f90tw)

   !$f90tw TESTCODE(TEST, nefis_tests, test_15, test_15,
   subroutine test_15() bind(C)
      integer ntimes, num_columns, num_rows, num_layers_grid, nsrc
      parameter(ntimes=3,&
      &num_columns=9,&
      &num_rows=7,&
      &num_layers_grid=5,&
      &nsrc=6&
      &)

      integer * 4 fds
      integer Clsnef,&
      &Clsdat,&
      &Clsdef,&
      &Credat,&
      &Defcel,&
      &Defelm,&
      &Defgrp,&
      &getnfv,&
      &Crenef,&
      &Putelt,&
      &Neferr,&
      &reserr
      integer Getelt
      integer error,&
      &idum,&
      &i, m, n, k, nt,&
      &UINDEX(3, 1),&
      &usrord(5)
      real rbuff3(num_columns, num_rows, num_layers_grid)
      real rbuff2(num_columns, num_rows)
      real rbuff1(num_columns)
      integer ibuff2(7, nsrc)
      integer ibuff1(nsrc)

      character * 8 elmtps(8)
      character * 16 elmnms(8), elmunt(8), elmqty(8)
      integer elmsiz(8), elmndm(8), elmdms(3, 8)
      character * 64 elmdes(8)
      integer grpdms(5), grpord(5)

      character * 1024 errstr
      character * 255 version
      character(len=30) :: filename1
      character(len=30) :: filename2
      integer file_unit

      ! delete previous output files if they exist
      filename1 = 'test15.out'
      filename2 = 'test15.scr'
      call delete_file(filename1)

      open (newunit=file_unit, file=filename1)

      error = reserr()
      error = getnfv(version)
      write (file_unit, *) '-----------------------------------------------'
      write (file_unit, '(a)') 'Version: '//trim(version(5:))
      write (file_unit, *) '-----------------------------------------------'

!
! Test to check cells with different element types
! (cell according dwqtim on comm. file)
!
!
!     Define element names
!
      elmnms(1) = 'TIMCUR'
      elmnms(2) = 'RSAL'
      elmnms(3) = 'RTEM'
      elmnms(4) = 'DICUV'
      elmnms(5) = 'DICWW'
      elmnms(6) = 'DISCUM'
      elmnms(7) = 'MNKSRC'
      elmnms(8) = 'TAUMAX'
!
!     Define element qty
!
      elmqty(1) = '1'
      elmqty(2) = '2'
      elmqty(3) = '3'
      elmqty(4) = '4'
      elmqty(5) = '5'
      elmqty(6) = '6'
      elmqty(7) = '7'
      elmqty(8) = '8'
!
!     Define element type
!
      elmtps(1) = 'INTEGER'
      elmtps(2) = 'REAL'
      elmtps(3) = 'REAL'
      elmtps(4) = 'REAL'
      elmtps(5) = 'REAL'
      elmtps(6) = 'REAL'
      elmtps(7) = 'INTEGER'
      elmtps(8) = 'REAL'
!
!     Define size of element one item
!
      elmsiz(1) = 4
      elmsiz(2) = 4
      elmsiz(3) = 4
      elmsiz(4) = 4
      elmsiz(5) = 4
      elmsiz(6) = 4
      elmsiz(7) = 4
      elmsiz(8) = 4
!
!     Define element description
!
      elmdes(1) = 'Time'
      elmdes(2) = 'Salinity'
      elmdes(3) = 'Temperature'
      elmdes(4) = 'Eddy viscosity, horizontal'
      elmdes(5) = 'Eddy diffusivity, horizontal'
      elmdes(6) = 'Cumm disachrge'
      elmdes(7) = 'Discharge location'
      elmdes(8) = 'Schuifspanning (maximale)'
!
!     Define element unity
!
      elmunt(1) = '[tscale]'
      elmunt(2) = '[ppt]'
      elmunt(3) = '[Degree]'
      elmunt(4) = '[m2/s]'
      elmunt(5) = '[m2/s]'
      elmunt(6) = '[m3]'
      elmunt(7) = '[-]'
      elmunt(8) = '[-]'
!
!     fill dimension of element
!
      elmndm(1) = 1
      elmdms(1, 1) = 1

      elmndm(2) = 3
      elmdms(1, 2) = num_columns
      elmdms(2, 2) = num_rows
      elmdms(3, 2) = num_layers_grid

      elmndm(3) = 3
      elmdms(1, 3) = num_columns
      elmdms(2, 3) = num_rows
      elmdms(3, 3) = num_layers_grid

      elmndm(4) = 3
      elmdms(1, 4) = num_columns
      elmdms(2, 4) = num_rows
      elmdms(3, 4) = num_layers_grid

      elmndm(5) = 3
      elmdms(1, 5) = num_columns
      elmdms(2, 5) = num_rows
      elmdms(3, 5) = num_layers_grid

      elmndm(6) = 1
      elmdms(1, 6) = nsrc

      elmndm(7) = 2
      elmdms(1, 7) = 7
      elmdms(2, 7) = nsrc

      elmndm(8) = 2
      elmdms(1, 8) = num_columns
      elmdms(2, 8) = num_rows
!
!     group dimensions
!
      grpdms(1) = ntimes
      grpdms(1) = 0
      grpdms(2) = 1
      grpdms(3) = 1
      grpdms(4) = 1
      grpdms(5) = 1
!
!     group order
!
      grpord(1) = 1
      grpord(2) = 2
      grpord(3) = 3
      grpord(4) = 4
      grpord(5) = 5
!
!     user order
!
      usrord(1) = 1
      usrord(2) = 2
      usrord(3) = 3
      usrord(4) = 4
      usrord(5) = 5
!
!------------------------------------------------------------------
!     openen nefis files
!
      error = crenef(fds, 'data_c15.dat', 'data_c15.def', 'B', 'C')
!
!     define elements on definition file
!
      do i = 1, 8
         error = Defelm(fds, elmnms(i), elmtps(i), elmsiz(i),&
         &elmqty(i), elmunt(i), elmdes(i),&
         &elmndm(i), elmdms(1, i))
         if (error /= 0) goto 9999
      end do
!
      error = Defcel(fds, 'cel_1', 7, elmnms(1))
      if (error /= 0) goto 9999
!
      error = Defcel(fds, 'cel_2', 1, elmnms(8))
      if (error /= 0) goto 9999
!
      error = Defgrp(fds, 'grp_1', 'cel_1', 1, grpdms, grpord)
      if (error /= 0) goto 9999
!
      error = Defgrp(fds, 'grp_2', 'cel_2', 1, grpdms, grpord)
      if (error /= 0) goto 9999
!
      error = Credat(fds, 'dat_grp_1', 'grp_1')
      if (error /= 0) goto 9999
!
      error = Credat(fds, 'dat_grp_2', 'grp_2')
      if (error /= 0) goto 9999

      error = Clsnef(fds)
!------------------------------------------------------------------
!     write data
!
      UINDEX(3, 1) = 1
      do nt = 1, ntimes
         UINDEX(1, 1) = nt
         UINDEX(2, 1) = nt
!
         error = crenef(fds, 'data_c15.dat', 'data_c15.def', ' ', 'u')
         if (error /= 0) goto 9999

!------------------------------------------------------------------
         write (file_unit, *) elmnms(1)
!      'TIMCUR'
         ibuff1(1) = 10 * nt + 1.
         error = Putelt(fds, 'dat_grp_1', elmnms(1),&
         &UINDEX, usrord, ibuff1)
         if (error /= 0) goto 9999

!------------------------------------------------------------------
         write (file_unit, *) elmnms(1)
!      'TIMCUR'
         ibuff1(1) = 10 * nt + 2.
         error = Putelt(fds, 'dat_grp_1', elmnms(1),&
         &UINDEX, usrord, ibuff1)
         if (error /= 0) goto 9999

!------------------------------------------------------------------
         write (file_unit, *) elmnms(2)
!     'RSAL'
         do m = 1, num_columns
            do n = 1, num_rows
               do k = 1, num_layers_grid
                  rbuff3(m, n, k) =&
                  &1000.*real(m) + 100.*real(n) + 10.*real(k) + real(nt)
               end do
            end do
         end do
         error = Putelt(fds, 'dat_grp_1', elmnms(2),&
         &UINDEX, usrord, rbuff3)
         if (error /= 0) goto 9999

!------------------------------------------------------------------
         write (file_unit, *) elmnms(3)
!     elmnms(3) =
         do m = 1, num_columns
            do n = 1, num_rows
               do k = 1, num_layers_grid
                  rbuff3(m, n, k) =&
                  &1000.*real(m) + 100.*real(n) + 10.*real(k) + real(nt)
               end do
            end do
         end do
         error = Putelt(fds, 'dat_grp_1', elmnms(3),&
         &UINDEX, usrord, rbuff3)
         if (error /= 0) goto 9999

!------------------------------------------------------------------
         write (file_unit, *) elmnms(4)
!     'DICUV'
         do m = 1, num_columns
            do n = 1, num_rows
               do k = 1, num_layers_grid
                  rbuff3(m, n, k) =&
                  &1000.*real(m) + 100.*real(n) + 10.*real(k) + real(nt)
               end do
            end do
         end do
         error = Putelt(fds, 'dat_grp_1', elmnms(4),&
         &UINDEX, usrord, rbuff3)
         if (error /= 0) goto 9999

!------------------------------------------------------------------
         write (file_unit, *) elmnms(5)
!     'DICWW'
         do m = 1, num_columns
            do n = 1, num_rows
               do k = 1, num_layers_grid
                  rbuff3(m, n, k) =&
                  &1000.*real(m) + 100.*real(n) + 10.*real(k) + real(nt)
               end do
            end do
         end do
         error = Putelt(fds, 'dat_grp_1', elmnms(5),&
         &UINDEX, usrord, rbuff3)
         if (error /= 0) goto 9999

!------------------------------------------------------------------
         write (file_unit, *) elmnms(6)
         do m = 1, nsrc
            rbuff1(m) = 10.*real(m) + real(nt)
         end do
!     'DISCUM'
         error = Putelt(fds, 'dat_grp_1', elmnms(6),&
         &UINDEX, usrord, rbuff1)
         if (error /= 0) goto 9999

!------------------------------------------------------------------
         write (file_unit, *) elmnms(7)
!     'MNKSRC'
         do m = 1, 7
            do n = 1, nsrc
               ibuff2(m, n) = 100 * m + 10 * n + nt
            end do
         end do
         error = Putelt(fds, 'dat_grp_1', elmnms(7),&
         &UINDEX, usrord, ibuff2)
         if (error /= 0) goto 9999

!------------------------------------------------------------------
         write (file_unit, *) elmnms(8)
!     'TAUMAX'
         do m = 1, num_columns
            do n = 1, num_rows
               rbuff2(m, n) = 0.0
               rbuff2(m, n) =&
               &1000.*real(m) + 100.*real(n) + real(nt)
            end do
         end do
         error = Putelt(fds, 'dat_grp_2', elmnms(8),&
         &UINDEX, usrord, rbuff2)
         if (error /= 0) goto 9999
!------------------------------------------------------------------
         error = Clsnef(fds)
      end do

!====================================================================
9999  continue
!
      error = Neferr(0, errstr)
      write (file_unit, '(a)') trim(errstr)

      error = Clsdat(fds)
      error = Clsdef(fds)

      close (file_unit)
      !
      call compare_text_files(filename1, filename2, skiplines)

   end subroutine test_15
   !$f90tw)

end module m_nefis_tests
