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

module m_nefis_test_14
   use assertions_gtest
   use tests_nefis_helper
   implicit none

   character(len=30), dimension(3) :: skiplines = [ "Version", "version", "-----" ]
   
contains

   !$f90tw TESTCODE(TEST, nefis_tests, test_14, test_14,
   subroutine test_14() bind(C)

   INTEGER NTIMES, BUFSIZ
!
! size of nefis file: 4.800 Mbyte = 4xNTIMESxBUFSIZ: NTIMES=600, BUFSIZ=2000000
! size of nefis file: 3.200 Mbyte = 4xNTIMESxBUFSIZ: NTIMES=400, BUFSIZ=2000000
!
!      PARAMETER (NTIMES=600, BUFSIZ=2000000)  ! 4.8 Gbyte
   PARAMETER (NTIMES=400, BUFSIZ=2000000) ! 3.2 Gbyte
!      PARAMETER (NTIMES=400000, BUFSIZ=2000) ! 3.2 Gbyte
!      PARAMETER (NTIMES=20, BUFSIZ=200)

   INTEGER START, stp, INCR
   PARAMETER (START=1, stp=2, INCR=3)
   INTEGER fds
   INTEGER clsdat,&
   &clsdef,&
   &credat,&
   &defelm,&
   &defcel,&
   &defgrp,&
   &getnfv,&
   &getelt
   INTEGER crenef,&
   &putelt,&
   &clsnef,&
   &neferr
   INTEGER error, ierror,&
   &i, j,&
   &grpdms(1),&
   &grpord(1),&
   &usrord(1),&
   &UINDEX(3,5)
   INTEGER buffer(BUFSIZ)
   CHARACTER names*14, coding*1
   CHARACTER ERRSTR*1024
   character*16 dat_name, def_name
   REAL  cpu1, cpu2
   real  elap_r, elap_w
   CHARACTER*255  version
   character(len=30) :: filename1
   character(len=30) :: filename2
   integer file_unit
   
   ! delete previous output files if they exist
   filename1='test.out'
   filename2='test.scr'
   call delete_file(filename1) 
   
   open(newunit=file_unit,file=filename1)

   error = getnfv(version)
   write(file_unit,*) '-----------------------------------------------'
   write(file_unit,'(a)') 'Version: '//trim(version(5:))
   write(file_unit,*) '-----------------------------------------------'

   elap_w=0
   elap_r=0
   call clock(cpu1)
   if ( 0 .eq. 0) then
      coding = 'B'
      dat_name = 'data_c14.dat'
      def_name = 'data_c14.def'
      error= crenef( fds, dat_name, def_name, coding, 'C')

      if (error .ne. 0) then
         ierror = neferr( 0, errstr)
         write(file_unit,*)
         write(file_unit,'(a)') trim(errstr)
         goto 9999
      endif

      error= Defelm( fds, 'ELEM_R_4_DIM_1', 'INTEGER', 4,&
      &'GROOTHEID 2', 'eenheid 2','Beschrijving 2',&
      &1, BUFSIZ)
      if (error .ne. 0) then
         ierror = neferr( 0, errstr)
         write(file_unit,*)
         write(file_unit,'(a)') trim(errstr)
         goto 9999
      endif

      names= 'ELEM_R_4_DIM_1'
      error= Defcel( fds, 'CEL_TEST_3', 1, names)
      if (error .ne. 0) then
         ierror = neferr( 0, errstr)
         write(file_unit,*)
         write(file_unit,'(a)') trim(errstr)
         goto 9999
      endif

      grpdms(1) = 0
      grpord(1) = 1
      error= Defgrp( fds, 'GRP_TEST_3D', 'CEL_TEST_3', 1,&
      &grpdms, grpord)
      if (error .ne. 0) then
         ierror = neferr( 0, errstr)
         write(file_unit,*)
         write(file_unit,'(a)') trim(errstr)
         goto 9999
      endif
!---------------------------------------------------------------------
      error= Credat( fds, 'DATAGRP_TEST_3D', 'GRP_TEST_3D')
      if (error .ne. 0) then
         ierror = neferr( 0, errstr)
         write(file_unit,*)
         write(file_unit,'(a)') trim(errstr)
         goto 9999
      endif
!---------------------------------------------------------------------
      call clock(cpu2)
      write(file_unit,'(''Initialisation (real time) [sec]:'',1PE13.5)')&
      &cpu2-cpu1
      write(file_unit,*)

      usrord(1)= 1
      UINDEX(incr,1) = 1

      write(file_unit,&
      &'(I5,'' schrijfopdrachten van '',I9,'' bytes'')')NTIMES,BUFSIZ*4
      DO 20 j=1,NTIMES
         DO 10 i= 1, BUFSIZ
            buffer(i)= 1000*i+j
10       CONTINUE
         if (ntimes>1000) then
            if (mod(j,100)==1)&
            &write(file_unit,'(''opdracht '', i3, '' van '', i3)') j,ntimes
         elseif (ntimes>100) then
            if (mod(j,10)==1)&
            &write(file_unit,'(''opdracht '', i3, '' van '', i3)') j,ntimes
         elseif (ntimes>10) then
            write(file_unit,'(''opdracht '', i3, '' van '', i3)') j,ntimes
         endif
         UINDEX(start,1) = j
         UINDEX(stp ,1) = j
         call clock(cpu1)
         if (j.eq.265) then
            write(file_unit,*)
         endif

         error= Putelt( fds, 'DATAGRP_TEST_3D',&
         &'ELEM_R_4_DIM_1', UINDEX, usrord, buffer)
         call clock(cpu2)
         elap_w = elap_w+cpu2-cpu1
         if (error .ne. 0) then
            ierror = neferr( 0, errstr)
            write(file_unit,*)
            write(file_unit,'(a)') trim(errstr)
            goto 9999
         endif
20    CONTINUE
      write(file_unit,'(''Writing (real time) [sec]:'',1PE13.5)') elap_w
      write(file_unit,*)
      error= Clsnef( fds)
   endif

   coding = ' '
   error= crenef( fds, dat_name, def_name, coding, 'R')
   if (error .ne. 0) then
      ierror = neferr( 0, errstr)
      write(file_unit,*)
      write(file_unit,'(a)') trim(errstr)
      goto 9999
   endif
   write(file_unit,&
   &'(''Lees '', I5, '' keer '', I9, '' bytes'')') NTIMES, BUFSIZ*4
   DO 40 j=NTIMES-9, NTIMES+1
!      DO 40 j=NTIMES, NTIMES+1
      write(file_unit,'(''opdracht '', I3)') j
      UINDEX(start,1) = j
      UINDEX(stp  ,1) = j
      UINDEX(incr ,1) = 1
      usrord(1)= 1
      call clock(cpu1)
      error= Getelt( fds, 'DATAGRP_TEST_3D',&
      &'ELEM_R_4_DIM_1', UINDEX, usrord, BUFSIZ*4,&
      &buffer)
      call clock(cpu2)
      elap_r = elap_r + cpu2 - cpu1
      if (error .ne. 0) then
         ierror = neferr( 0, errstr)
         write(file_unit,*)
         write(file_unit,'(a)') trim(errstr)
         goto 9999
      else
         do 30 i= 1, BUFSIZ
            IF ( (buffer(i)- (1000*i+j) ).NE. 0) then
               print *,'error, i= ', i, buffer(i), 1000*i+j
            endif
30       continue
      endif
40 continue
   write(file_unit,'(''Writing (real time) [sec]:'',1PE13.5)') elap_w
   write(file_unit,'(''Reading (real time) [sec]:'',1PE13.5)') elap_r

9999 continue

   error= Clsdat( fds)
   if (error .ne. 0) then
      ierror = neferr( 0, errstr)
      write(file_unit,*)
      write(file_unit,'(a)') trim(errstr)
   endif

   error= Clsdef( fds)
   if (error .ne. 0) then
      ierror = neferr( 0, errstr)
      write(file_unit,*)
      write(file_unit,'(a)') trim(errstr)
   endif

   ierror = neferr( 0, errstr)
   write(file_unit,*)
   write(file_unit,'(a)') trim(errstr)

   close(file_unit)
   !
   call compare_text_files(filename1, filename2, skiplines)   
   
   end subroutine test_14
   !$f90tw)
   
end module m_nefis_test_14
