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

module m_nefis_test_08
   use assertions_gtest
   use tests_nefis_helper
   implicit none

   character(len=30), dimension(3) :: skiplines = [ "Version", "version", "-----" ]
   
contains

   !$f90tw TESTCODE(TEST, nefis_tests, test_08, test_08,
   subroutine test_08() bind(C)
   INTEGER   START, STOP, INCR

   CHARACTER CODING*1, ELMTYP*16, ELMQTY*16, ELMUNT*16,&
   &ELMDES*64, ELMNMS(5)*16, CELNAM*16,&
   &GRPNAM*16, GRPDEF*16

   INTEGER   NBYTSG, I, NELEMS, J, K
   INTEGER   ERROR, ELMNDM, ELMDMS(5), GRPNDM, UINDEX(3,1),&
   &GRPDMS(142), GRPORD(5), IARRIN(142), IARROU(142)

   REAL      ARRAY(142,65), ARROUT(142,65)
   CHARACTER ERRSTR*1024

   INTEGER   FDS

   INTEGER OPNDAT, OPNDEF, DEFELM, CLSDEF, CLSDAT, DEFCEL,&
   &DEFGRP, INQELM, INQCEL, INQGRP, FLSDAT, FLSDEF,&
   &CREDAT, PUTIAT, PUTSAT, PUTELT, GETELT, PUTRAT,&
   &INQFST, INQNXT, NEFERR, GETNFV

   CHARACTER*255  version
   character(len=30) :: filename1
   character(len=30) :: filename2
   integer file_unit
   
   ! delete previous output files if they exist
   filename1='test.out'
   filename2='test.scr'
   call delete_file(filename1) 
   call delete_file('data_c08.def')
   call delete_file('data_c08.dat')
   
   open(newunit=file_unit,file=filename1)

   error = getnfv(version)
   write(file_unit,*) '-----------------------------------------------'
   write(file_unit,*) 'Version: '//trim(version(5:))
   write(file_unit,*) '-----------------------------------------------'

   start = 1
   stop  = 2
   incr  = 3
   CODING = 'N'
   ELMNDM = 5
   NELEMS = 5
   GRPNDM = 5
   NBYTSG = 0
   errstr = ' '

   write(file_unit,'(''TEST8: Open files'')')
   ERROR = OPNDAT (FDS, 'data_c08.dat', CODING)
   if (error .ne. 0) write(file_unit,*) ' OPNDAT:', error
   if (error .ne. 0) goto 9999

   ERROR = OPNDEF (FDS, 'data_c08.def', CODING)
   if (error .ne. 0) write(file_unit,*) ' OPNDEF:', error
   if (error .ne. 0) goto 9999

   ELMDMS(1) = 142
   ELMDMS(2) = 65

   write(file_unit,'(''TEST8: Define elements'')')
   ERROR = DEFELM (FDS, 'Elmnam', 'ReaL', 4, 'Elmqty',&
   &'Elmunt', 'Elmdes', 2, ELMDMS)
   if (error .ne. 0) write(file_unit,*) ' DEFELM: Elmnam'
   if (error .ne. 0) goto 9999

   ERROR = DEFELM (FDS, 'ElmInt', 'IntEgeR', 4, 'Elmqty',&
   &'Elmunt', 'Elmdes', 1, 142)
   if (error .ne. 0) write(file_unit,*) ' DEFELM: ElmInt'
   if (error .ne. 0) goto 9999

   write(file_unit,'(''TEST8: Define cells'')')
   ELMNMS(1) = 'Elmnam'
   ELMNMS(2) = 'ElmInt'
   ERROR = DEFCEL (FDS, 'Celnam', 2, ELMNMS)
   if (error .ne. 0) write(file_unit,*) ' DEFCEL: Celnam'
   if (error .ne. 0) goto 9999

   write(file_unit,'(''TEST8: Define groups'')')
!     ** Variable dimensie **
   ERROR = DEFGRP (FDS, 'Grpdef', 'Celnam', 1, 0, 1)
   if (error .ne. 0) write(file_unit,*) ' DEFGRP: Grpdef'
   if (error .ne. 0) goto 9999

   ERROR = FLSDEF (FDS)
   if (error .ne. 0) write(file_unit,*) ' FLSDEF'
   if (error .ne. 0) goto 9999

   write(file_unit,'(''TEST8: Inquire element'')')
   ERROR = INQELM (FDS, 'Elmnam', ELMTYP, NBYTSG, ELMQTY,&
   &ELMUNT, ELMDES, ELMNDM, ELMDMS)
   if (error .ne. 0)&
   &write(file_unit,*) ' INQELM:', error, elmtyp, nbytsg, elmqty, elmunt&
   &, elmdes, elmndm, (elmdms(i),i=1,elmndm)
   if (error .ne. 0) goto 9999

   write(file_unit,'(''TEST8: Inquire cell'')')
   NELEMS = 2
   ERROR = INQCEL (FDS, 'Celnam', NELEMS, ELMNMS)
   if (error .ne. 0)&
   &write(file_unit,*) ' INQCEL:', error, nelems, (elmnms(i),i=1,nelems)
   if (error .ne. 0) goto 9999

   write(file_unit,'(''TEST8: Inquire group'')')
   ERROR = INQGRP (FDS, 'Grpdef', CELNAM, GRPNDM, GRPDMS, GRPORD)
   if (error .ne. 0) write(file_unit,*) ' INQGRP:', error, celnam, grpndm,&
   &(grpdms(i),i=1,grpndm), (grpord(i),i=1,grpndm)
   if (error .ne. 0) goto 9999

   write(file_unit,'(''TEST8: Create group on data file'')')
   ERROR = CREDAT (FDS, 'Grpnam', 'Grpdef')
   if (error .ne. 0) write(file_unit,*) ' Credat: Grpnam'
   if (error .ne. 0) goto 9999
   ERROR = CREDAT (FDS, 'aaaabbbb', 'Grpdef')
   if (error .ne. 0) write(file_unit,*) ' Credat: aaaabbbb'
   if (error .ne. 0) goto 9999
   ERROR = CREDAT (FDS, 'bbbbaaaa', 'Grpdef')
   if (error .ne. 0) write(file_unit,*) ' Credat: bbbbaaaa'
   if (error .ne. 0) goto 9999
   ERROR = CREDAT (FDS, 'babaabab', 'Grpdef')
   if (error .ne. 0) write(file_unit,*) ' Credat: babaabab'
   if (error .ne. 0) goto 9999
   ERROR = CREDAT (FDS, 'Grpnam  Grpnam', 'Grpdef')
   if (error .ne. 0) write(file_unit,*) ' Credat: Grpnam  Grpnam'
   if (error .ne. 0) goto 9999

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
   write(file_unit,'(''TEST8: Put attributes'')')
   ERROR = PUTIAT (FDS, 'Grpnam',&
   &'INTEGER ATTRIBUUT 1', 1)
   if (error .ne. 0) then
      error = neferr( 0, errstr)
      write(file_unit,'(a)') trim(errstr)
   endif
   ERROR = PUTIAT (FDS, 'Grpnam',&
   &'INTEGER ATTRIBUUT 2', 2)
   if (error .ne. 0) then
      error = neferr( 0, errstr)
      write(file_unit,'(a)') trim(errstr)
   endif
   ERROR = PUTIAT (FDS, 'Grpnam',&
   &'INTEGER ATTRIBUUT 3', 3)
   if (error .ne. 0) then
      error = neferr( 0, errstr)
      write(file_unit,'(a)') trim(errstr)
   endif
   ERROR = PUTIAT (FDS, 'Grpnam',&
   &'INTEGER ATTRIBUUT 4', 4)
   if (error .ne. 0) then
      error = neferr( 0, errstr)
      write(file_unit,'(a)') trim(errstr)
   endif
   ERROR = PUTIAT (FDS, 'Grpnam',&
   &'INTEGER ATTRIBUUT 5', 5)
   if (error .ne. 0) then
      error = neferr( 0, errstr)
      write(file_unit,'(a)') trim(errstr)
   endif
   ERROR = PUTIAT (FDS, 'Grpnam',&
   &'INTEGER ATTRIBUU', 5)
   if (error .ne. 0) write(file_unit,*) ' PUTIAT 5:', error
   if (error .ne. 0) goto 9999

   ERROR = PUTRAT (FDS, 'Grpnam', 'RAttrib_1', 12.345)
   if (error .ne. 0) goto 9999
   ERROR = PUTRAT (FDS, 'Grpnam', 'RAttrib_2', -2.2)
   if (error .ne. 0) goto 9999
   ERROR = PUTRAT (FDS, 'Grpnam', 'RAttrib_3', 3.3)
   if (error .ne. 0) goto 9999
   ERROR = PUTRAT (FDS, 'Grpnam', 'RAttrib_4', -4.4)
   if (error .ne. 0) goto 9999
   ERROR = PUTRAT (FDS, 'Grpnam', 'RAttrib_5', 5.5)
   if (error .ne. 0) goto 9999

   ERROR = PUTSAT (FDS, 'Grpnam', 'SAttrib_1', 'String 1')
   if (error .ne. 0) goto 9999
   ERROR = PUTSAT (FDS, 'Grpnam', 'SAttrib_2', 'String 2')
   if (error .ne. 0) goto 9999
   ERROR = PUTSAT (FDS, 'Grpnam', 'SAttrib_3', 'String 3')
   if (error .ne. 0) goto 9999
   ERROR = PUTSAT (FDS, 'Grpnam', 'SAttrib_4', 'String 4')
   if (error .ne. 0) goto 9999
   ERROR = PUTSAT (FDS, 'Grpnam', 'SAttrib_5', 'String 5')
   if (error .ne. 0) goto 9999

   ERROR = PUTRAT(FDS, 'bbbbaaaa', 'RAttrib_1', 12.347)
   if (error .ne. 0) goto 9999
   ERROR = PUTRAT(FDS, 'aaaabbbb', 'RAttrib_1', 12.346)
   if (error .ne. 0) goto 9999
   ERROR = PUTRAT(FDS, 'babaabab', 'RAttrib_1', 12.348)
   if (error .ne. 0) goto 9999
   ERROR = PUTRAT(FDS, 'Grpnam  Grpnam', 'RAttrib_1', 12.349)
   if (error .ne. 0) goto 9999
   ERROR = PUTSAT(FDS, 'aaaabbbb', 'SAttrib_1', 'String 6')
   if (error .ne. 0) goto 9999
   ERROR = PUTSAT(FDS, 'bbbbaaaa', 'SAttrib_1', 'String 11')
   if (error .ne. 0) goto 9999
   ERROR = PUTSAT(FDS, 'babaabab', 'SAttrib_1', 'String 16')
   if (error .ne. 0) goto 9999
   ERROR = PUTSAT(FDS, 'Grpnam  Grpnam', 'SAttrib_1', 'String 21')
   if (error .ne. 0) goto 9999
   ERROR = PUTIAT(FDS, 'aaaabbbb', 'IAttrib_1', -6)
   if (error .ne. 0) goto 9999
   ERROR = PUTIAT(FDS, 'bbbbaaaa', 'IAttrib_1', -11)
   if (error .ne. 0) goto 9999
   ERROR = PUTIAT(FDS, 'babaabab', 'IAttrib_1', -16)
   if (error .ne. 0) goto 9999
   ERROR = PUTIAT(FDS, 'Grpnam  Grpnam', 'IAttrib_1', -21)
   if (error .ne. 0) goto 9999

!     *** GeT All Attributes of all Groups of the Datafile
   write(file_unit,'(''TEST8: Get attributes'')')
   CALL GTALAT (FDS, file_unit)

   error = neferr( 0, errstr)
   write(file_unit,'(a)') trim(errstr)

   write(file_unit,'(''TEST8: Put real and integer array'')')
   UINDEX(INCR,1) = 1
   DO 20 J = 1,10
      UINDEX(START,1) = J
      UINDEX(STOP ,1) = J
      DO 10 I = 1,142
         DO 5 K = 1,65
            ARRAY(I,K) = J*1000.+I+K/1000.
5        CONTINUE
         IARRIN(I) = J*1000+I
10    CONTINUE

      ERROR = PUTELT (FDS, 'Grpnam',&
      &'Elmnam', UINDEX, 1, ARRAY)
      if (error .ne. 0) write(file_unit,*) ' PUTELT:', error
      if (error .ne. 0) goto 9999

      ERROR = PUTELT (FDS, 'Grpnam',&
      &'ElmInt', UINDEX, 1, IARRIN)
      if (error .ne. 0) write(file_unit,*) ' PUTELT:', error
      if (error .ne. 0) goto 9999

      ERROR = FLSDAT (FDS)
      if (error .ne. 0) write(file_unit,*) ' FLSDAT:', error
      if (error .ne. 0) goto 9999

20 CONTINUE

   write(file_unit,'(''TEST8: Get real and integer array'')')
   DO 30 J = 1,10
      UINDEX(START,1) = J
      UINDEX(STOP ,1) = J
      ERROR = GETELT (FDS, 'Grpnam',&
      &'Elmnam', UINDEX, 1, 142*65*4, ARROUT)
      if (error .ne. 0) write(file_unit,'('' GETELT:'',i4,1x,3(1pe14.6,1x))')&
      &error , arrout(142,65),arrout(142,64),arrout(141,65)
      if (error .ne. 0) goto 9999

      ERROR = GETELT (FDS, 'Grpnam',&
      &'ElmInt', UINDEX, 1, 142*4, IARROU)
      if (error .ne. 0)&
      &write(file_unit,*) ' GETELT:', error, iarrou(1), iarrou(142)
      if (error .ne. 0) goto 9999

30 CONTINUE

   write(file_unit,'(''TEST8: Flush memory to files'')')

   ERROR = FLSDAT (FDS)
   if (error .ne. 0) write(file_unit,*) ' FLSDAT:', error
   if (error .ne. 0) goto 9999

   ERROR = FLSDEF (FDS)
   if (error .ne. 0) write(file_unit,*) ' FLSDEF:', error
   if (error .ne. 0) goto 9999

   write(file_unit,'(''TEST8: Loop all groups'')')

   ERROR = INQFST (FDS, GRPNAM, GRPDEF)
   if (error .ne. 0) write(file_unit,*) ' INQFST:', error, grpnam, grpdef

   ERROR = INQNXT (FDS, GRPNAM, GRPDEF)
   if (error .ne. 0) write(file_unit,*) ' INQNXT:', error, grpnam, grpdef

   ERROR = INQNXT (FDS, GRPNAM, GRPDEF)
   if (error .ne. 0) write(file_unit,*) ' INQNXT:', error, grpnam, grpdef

   ERROR = INQNXT (FDS, GRPNAM, GRPDEF)
   if (error .ne. 0) then
      error = neferr( 0, errstr)
      write(file_unit,'(a)') trim(errstr)
   endif

   ERROR = INQNXT (FDS, GRPNAM, GRPDEF)
   if (error .ne. 0) then
      error = neferr( 0, errstr)
      write(file_unit,'(a)') trim(errstr)
   endif

   ERROR = INQNXT (FDS, GRPNAM, GRPDEF)
   if (error .ne. 0) then
      error = neferr( 0, errstr)
      write(file_unit,'(a)') trim(errstr)
   endif

!     write(file_unit,'('' SO FAR SO GOOD'
   goto 8888
9999 continue
   error = neferr( 0, errstr)
   write(file_unit,'(a)') trim(errstr)
   write(file_unit,'('' NOT SO GOOD'')')
8888 continue

   ERROR = CLSDAT (FDS)
   if (error .ne. 0) write(file_unit,*) ' CLSDAT:', error

   ERROR = CLSDEF (FDS)
   if (error .ne. 0) write(file_unit,*) ' CLSDEF:', error

   error = neferr( 0, errstr)
   write(file_unit,'(a)') trim(errstr)

   close(file_unit)
   !
   call compare_text_files(filename1, filename2, skiplines)   
   
   end subroutine test_08
   !$f90tw)
   
end module m_nefis_test_08
