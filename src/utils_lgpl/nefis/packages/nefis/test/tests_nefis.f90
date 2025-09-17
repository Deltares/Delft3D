module tests_nefis
   use assertions_gtest
   use tests_nefis_helper
   implicit none

   character(len=30), dimension(3) :: skiplines = [ "Version", "version", "-----" ]
   
contains

   !$f90tw TESTCODE(TEST, tests_nefis, test_succeeds, test_succeeds,
   subroutine test_succeeds() bind(C)
      integer, parameter :: PATH_LENGTH = 2
      real, parameter :: tolerance = 1e-8
      real, dimension(PATH_LENGTH) :: x, y

      x = [0.0, 1.0]
      y = [0.01, 1.0]

      call f90_expect_near(1.0, 1.2, 0.3, "values should be close")
      call f90_expect_near(x, y, 0.3, "list of values should be close")
      !call f90_assert_streq("aaaaa", "aaaaa", "strings are not the same")
      
   end subroutine test_succeeds
   !$f90tw)

!$f90tw TESTCODE(TEST, tests_nefis, test4, test4,
   subroutine test4() bind(C)
   INTEGER*4 fds
   INTEGER ::&
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
   INTEGER Getelt
   INTEGER error,&
   &idum,&
   &i,&
   &imax,&
   &start,&
   &UINDEX(3,1)
   REAL    buffer,&
   &cpu1,&
   &cpu2
   CHARACTER coding*1
   CHARACTER*1024 errstr
   CHARACTER*255  version
   CHARACTER*20  strdata
   character(len=30) :: filename1
   character(len=30) :: filename2
   
   integer file_unit
   
   cpu1   = 0.0
   cpu2   = 0.0
   idum   = 0
   coding = 'N'
   imax   = 1000
   start  = 1
!
   call clock(cpu1)
   error = getnfv(version)
   
   ! delete previous output files if they exist
   filename1='test4.out'
   filename2='../test4.result'
   call delete_file(filename1) 
   call delete_file('nefis_ex.def')
   call delete_file('nefis_ex.dat')
   
   open(newunit=file_unit,file=filename1)
   
   write(file_unit,*) '-----------------------------------------------'
   write(file_unit,*) 'Version: '//trim(version(5:))
   write(file_unit,*) '-----------------------------------------------'

   error= Opndef( fds, 'nefis_ex.def', coding)
   if (error .ne. 0) goto 9999
!
   error= Defelm( fds, 'ELEM_R_4', 'REAL', 4,&
   &'GROOTHEID 1', 'eenheid 1','Beschrijving 1',&
   &0, idum)
   if (error .ne. 0) goto 9999

   error= Defelm( fds, 'ELEM_STR', 'CHARACTE', 20,&
   &'GROOTHEID 2', 'eenheid 2','Beschrijving 2',&
   &0, idum)
   if (error .ne. 0) goto 9999
!
   error= Defcel( fds, 'CEL_TEST_1', 1, 'ELEM_R_4')
   if (error .ne. 0) goto 9999

   error= Defcel( fds, 'CEL_TEST_2', 1, 'ELEM_STR')
   if (error .ne. 0) goto 9999
!
   error= Defgrp( fds, 'GRP_TEST_1', 'CEL_TEST_1', 1, imax, 1)
   if (error .ne. 0) goto 9999
!
   error= Defgrp( fds, 'GRP_TEST_2', 'CEL_TEST_2', 1, imax, 1)
   if (error .ne. 0) goto 9999
!==========================================================
   error= Defgrp( fds, 'GRP_TEMP', 'CEL_TEST_1', 1, 1, 1)
   if (error .ne. 0) goto 9999
!==========================================================
!
   error= Opndat( fds, 'nefis_ex.dat', coding)
   if (error .ne. 0) goto 9999
!
   error= Credat( fds, 'DATAGRP_TEST_1A', 'GRP_TEST_1')
   if (error .ne. 0) goto 9999
!
   error= Credat( fds, 'DATAGRP_TEST_1B', 'GRP_TEST_1')
   if (error .ne. 0) goto 9999
!
   error= Credat( fds, 'DATAGRP_TEST_1C', 'GRP_TEST_2')
   if (error .ne. 0) goto 9999
!   
   call clock(cpu2)
   write(file_unit,'(''Initialisation NEFIS files [sec]'',1PE13.5)')&
   &cpu2-cpu1
!
   write(file_unit,*)
   write(file_unit,'(''Schrijf elementen'')')
   write(file_unit,*)
!
   call clock(cpu1)
   UINDEX (3,1) = 1
   DO 10 i= 1, imax
      UINDEX (1,1) = i
      UINDEX (2,1) = i
      error= Putelt( fds, 'DATAGRP_TEST_1A', '*',&
      &UINDEX, 1, real(i))
      if (error .ne. 0) goto 9999
10 CONTINUE
   call clock(cpu2)
   write(file_unit,'(''DATAGRP_TEST_1A written in [sec]'',1PE13.5)')&
   &cpu2-cpu1

   call clock(cpu1)
   DO 20 i= imax, 1, -1
      UINDEX (1,1) = i
      UINDEX (2,1) = i
      error= Putelt( fds, 'DATAGRP_TEST_1B', '*',&
      &UINDEX, 1,-1.*real(i))
      if (error .ne. 0) goto 9999
20 CONTINUE
   call clock(cpu2)
   write(file_unit,'(''DATAGRP_TEST_1B written in [sec]'',1PE13.5)')&
   &cpu2-cpu1

   call clock(cpu1)
   DO 21 i= imax, 1, -1
      UINDEX (1,1) = i
      UINDEX (2,1) = i
      error= Putelt( fds, 'DATAGRP_TEST_1C', '*',&
      &UINDEX, 1, 'ABCDEFGHIJKLMNOPQRST')
      if (error .ne. 0) goto 9999
21 CONTINUE

   call clock(cpu2)
   write(file_unit,'(''DATAGRP_TEST_1C written in [sec]'',1PE13.5)')&
   &cpu2-cpu1

!
!=====================================================================
   write(file_unit,*)
   write(file_unit,'(''Lees elementen'')')
   write(file_unit,*)
!
   call clock(cpu1)
   DO 30 i= imax, 1, -1
      UINDEX (1,1) = i
      UINDEX (2,1) = i
      error= Getelt( fds, 'DATAGRP_TEST_1A', '*'       ,&
      &UINDEX, 1, 4, buffer)
      if (error .ne. 0) goto 9999
      IF (NINT(buffer).NE. i) PRINT *,'error, i= ', i, buffer&
      &,NINT(buffer)
30 CONTINUE
   call clock(cpu2)
   write(file_unit,'(''DATAGRP_TEST_1A read    in [sec]'',1PE13.5)')&
   &cpu2-cpu1
!
   call clock(cpu1)
   DO 40 i= 1, imax
      UINDEX (1,1) = i
      UINDEX (2,1) = i
      error= Getelt( fds, 'DATAGRP_TEST_1B', '*'       ,&
      &UINDEX, 1, 4, buffer)
      if (error .ne. 0) goto 9999
      IF (NINT(buffer).NE. -1*i) PRINT *,'error, i= ', i, buffer&
      &,NINT(buffer)
40 CONTINUE
   call clock(cpu2)
   write(file_unit,'(''DATAGRP_TEST_1B read    in [sec]'',1PE13.5)')&
   &cpu2-cpu1

   call clock(cpu1)
   DO 50 i= 1, imax
      UINDEX (1,1) = i
      UINDEX (2,1) = i
      error= Getelt( fds, 'DATAGRP_TEST_1C', '*'       ,&
      &UINDEX, 1, 20, strdata)
      if (error .ne. 0) goto 9999
      IF (strdata /= 'ABCDEFGHIJKLMNOPQRST') PRINT *,'error, i= ', i, strdata&
      &,strdata
50 CONTINUE
   call clock(cpu2)
   write(file_unit,'(''DATAGRP_TEST_1C read    in [sec]'',1PE13.5)')&
   &cpu2-cpu1
   write(file_unit,*)

   error= Clsdat( fds)
   error= Clsdef( fds)
!
9999 continue
!
   error = Neferr( 0, errstr)
   write(file_unit,'(a)') trim(errstr)
   close(file_unit)
!
call compare_text_files(filename1, filename2, skiplines)   
   
   end subroutine test4
   !$f90tw)
   
!$f90tw TESTCODE(TEST, tests_nefis, test5, test5,
   subroutine test5() bind(C)
   INTEGER start, stop, incr
   PARAMETER (start=1, stop=2, incr=3)
   INTEGER clsdat,&
   &clsdef,&
   &credat,&
   &defcel,&
   &defelm,&
   &defgrp,&
   &flsdat,&
   &flsdef,&
   &getnfv,&
   &getelt
   INTEGER neferr,&
   &opndat,&
   &opndef,&
   &putelt
   INTEGER error,&
   &idum,&
   &i, j,&
   &elmdms(5),&
   &UINDEX(3,1),&
   &fds
   REAL buffer(748)
   CHARACTER names(3)*14, coding*1
   CHARACTER ERRSTR*1024
   CHARACTER*255  version
   character(len=30) :: filename1
   character(len=30) :: filename2
   integer file_unit
   
   character(len=80), dimension(8) :: test_result = [ &
"schrijf DATAGRP_TEST_2A" , &
"schrijf DATAGRP_TEST_2B" , &
"lees DATAGRP_TEST_2B" , &
"lees DATAGRP_TEST_2A" , &
"lees DATAGRP_TEST_1A" , &
"lees DATAGRP_TEST_1B" , &
" " , &
"No NEFIS errors encountered" & 
   ]
   
!
   error = getnfv(version)

   filename1='test5.out'
   filename2='../test5.result'
   
   call delete_file('nefis_ex.def')
   call delete_file('nefis_ex.dat')
   call delete_file(filename1) 
   
   open(newunit=file_unit,file=filename1)

   write(file_unit,*) '-----------------------------------------------'
   write(file_unit,*) 'Version: '//trim(version(5:))
   write(file_unit,*) '-----------------------------------------------'

   coding=' '
   error= Opndef( fds, 'nefis_ex.def', coding)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
!
   error= Opndat( fds, 'nefis_ex.dat', coding)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
!
   error= Defelm( fds, 'ELEM_R_4_DIM_1', 'REAL', 4,&
   &'GROOTHEID 2', 'eenheid 2','Beschrijving 2',&
   &1, 3)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
!
   elmdms(1)= 5
   elmdms(2)= 5
   error= Defelm( fds, 'ELEM_R_4_DIM_2', 'REAL', 4,&
   &'GROOTHEID 3', 'eenheid 3','Beschrijving 3',&
   &2, elmdms)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
!
   elmdms(1)= 2
   elmdms(2)= 3
   elmdms(3)= 4
   elmdms(4)= 5
   elmdms(5)= 6
   error= Defelm( fds, 'ELEM_R_4_DIM_5', 'REAL', 4,&
   &'GROOTHEID 4', 'eenheid 4','Beschrijving 4',&
   &5, elmdms)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
!
   names(1)= 'ELEM_R_4_DIM_1'
   names(2)= 'ELEM_R_4_DIM_2'
   names(3)= 'ELEM_R_4_DIM_5'
   error= Defcel( fds, 'CEL_TEST_2', 3, names)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
!
   error= Defgrp( fds, 'GRP_TEST_2A', 'CEL_TEST_2', 0, idum, idum)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
!
   error= Defgrp( fds, 'GRP_TEST_2B', 'CEL_TEST_2', 1, 100, 1)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
!
   error= Credat( fds, 'DATAGRP_TEST_2A', 'GRP_TEST_2A')
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
!
   error= Credat( fds, 'DATAGRP_TEST_2B', 'GRP_TEST_2B')
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
!
   DO 10 i= 1, 748
      buffer(i)= i
10 CONTINUE
!
   write(file_unit,'(''schrijf DATAGRP_TEST_2A'')')
   UINDEX(start ,1) = 1
   UINDEX(stop  ,1) = 1
   UINDEX(incr  ,1) = 1
   error = Putelt( fds, 'DATAGRP_TEST_2A', '*',&
   &UINDEX, 1, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
!
   write(file_unit,'(''schrijf DATAGRP_TEST_2B'')')
   DO 30 i= 1, 100
      UINDEX(start,1) = i
      UINDEX(stop ,1) = i
      UINDEX(incr ,1) = 1
      DO 20 j= 1, 748
         buffer(j)= REAL(i)* REAL(j)
20    CONTINUE
      error = Putelt( fds, 'DATAGRP_TEST_2B', '*',&
      &UINDEX, 1, buffer)
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
30 CONTINUE
   error = flsdat ( fds )
   error = flsdef ( fds )
!
   write(file_unit,'(''lees DATAGRP_TEST_2B'')')
   DO 50 i= 100, 1, -1
      UINDEX(start,1) = i
      UINDEX(stop ,1) = i
      error= Getelt( fds, 'DATAGRP_TEST_2B', '*',&
      &UINDEX, 1, 748*4, buffer)
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
      DO 40 j= 1, 748
         IF (INT( buffer(j)/ REAL(i)-j).NE.0)&
         &write(file_unit,'(''error, i='',i3)') i
40    CONTINUE
50 CONTINUE
!
   write(file_unit,'(''lees DATAGRP_TEST_2A'')')
   UINDEX(start,1) = 1
   UINDEX(stop ,1) = 1
   error= Getelt( fds, 'DATAGRP_TEST_2A', '*',&
   &UINDEX, 1, 748*4, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   DO 60 j= 1, 748
!      PRINT *, buffer(j),j, INT(buffer(j)-j)
      IF (INT( buffer(j)-j).NE. 0) PRINT *,'error, i= ', i
60 CONTINUE

   write(file_unit,'(''lees DATAGRP_TEST_1A'')')
   DO 70 i= 1000, 1, -1
      UINDEX(start,1) = i
      UINDEX(stop ,1) = i
      error= Getelt( fds, 'DATAGRP_TEST_1A', '*' ,&
      &UINDEX, 1, 4, buffer(1))
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
!     write(file_unit,'(1x', buffer(1), i, int(buffer(1)-i),error
      IF (INT( buffer(1)-i).NE. 0)&
      &write(file_unit,'(''error, i= '',i3)') i
70 CONTINUE
!
   write(file_unit,'(''lees DATAGRP_TEST_1B'')')
   DO 80 i= 1, 1000
      UINDEX(start,1) = i
      UINDEX(stop ,1) = i
      error= Getelt( fds, 'DATAGRP_TEST_1B', '*',&
      &UINDEX, 1, 4, buffer(1))
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
      IF (INT( buffer(1)+1*i).NE. 0)&
      &write(file_unit,'(''error, i= '',i3)') i
80 CONTINUE
!
   write(file_unit,*)
   error= Clsdat( fds)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
!
   error= Clsdef( fds)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
!
   ERROR = NEFERR( 0, ERRSTR)
   write(file_unit,'(a)') trim(errstr)
!
   close(file_unit)
!
call compare_text_files(filename1, filename2, skiplines)   

   end subroutine test5
!$f90tw)

end module tests_nefis
