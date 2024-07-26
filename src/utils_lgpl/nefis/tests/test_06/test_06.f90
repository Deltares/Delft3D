!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
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
program test6
   INTEGER START, STOP, INCR
   PARAMETER (START=1, STOP=2, INCR=3)
   INTEGER*4 fds
   INTEGER clsdat,&
   &clsdef,&
   &credat,&
   &defcel,&
   &defgrp,&
   &getelt,&
   &getnfv,&
   &neferr
   INTEGER opndat,&
   &opndef,&
   &putelt
   INTEGER error,&
   &i, j, k, l, m, n, im,&
   &grpdms(5),&
   &grpord(5),&
   &usrord(5),&
   &UINDEX(3,5)
   REAL buffer(26880)
   CHARACTER names(2)*14, coding*1
   CHARACTER ERRSTR*1024
   REAL  cpu1, cpu2, elap_w, elap_r
   CHARACTER*255  version
!
   error = getnfv(version)
   write(*,*)
   write(*,*) trim(version(5:))
   write(*,*)

   call clock(cpu1)
   coding=' '
   error= Opndef( fds, 'nefis_ex.def', coding)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)

   error= Opndat( fds, 'nefis_ex.dat', coding)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)

   names(1)= 'ELEM_R_4_DIM_1'
   names(2)= 'ELEM_R_4'
   error= Defcel( fds, 'CEL_TEST_3', 2, names)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)

   grpdms(1)= 4
   grpdms(2)= 5
   grpdms(3)= 6
   grpdms(4)= 7
   grpdms(5)= 8

   grpord(1)= 1
   grpord(2)= 2
   grpord(3)= 3
   grpord(4)= 4
   grpord(5)= 5
   error= Defgrp( fds, 'GRP_TEST_3A', 'CEL_TEST_3', 5,&
   &grpdms, grpord)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)

   grpord(1)= 5
   grpord(2)= 4
   grpord(3)= 3
   grpord(4)= 2
   grpord(5)= 1
   error= Defgrp( fds, 'GRP_TEST_3B', 'CEL_TEST_3', 5,&
   &grpdms, grpord)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)

   grpord(1)= 5
   grpord(2)= 3
   grpord(3)= 1
   grpord(4)= 2
   grpord(5)= 4
   error= Defgrp( fds, 'GRP_TEST_3C', 'CEL_TEST_3', 5,&
   &grpdms, grpord)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)

   grpdms(1)= 0

   grpord(1)= 5
   grpord(2)= 4
   grpord(3)= 3
   grpord(4)= 2
   grpord(5)= 1

   error= Defgrp( fds, 'GRP_TEST_3D', 'CEL_TEST_3', 5,&
   &grpdms, grpord)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
!---------------------------------------------------------------------
   error= Credat( fds, 'DATAGRP_TEST_3A', 'GRP_TEST_3A')
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)

   error= Credat( fds, 'DATAGRP_TEST_3B', 'GRP_TEST_3B')
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)

   error= Credat( fds, 'DATAGRP_TEST_3C', 'GRP_TEST_3C')
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)

   error= Credat( fds, 'DATAGRP_TEST_3D', 'GRP_TEST_3D')
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
!---------------------------------------------------------------------
   call clock(cpu2)
   write(*,'(''Initialisation NEFIS files [sec]'',1PE13.5)')&
   &cpu2-cpu1

   UINDEX(incr,1) = 1
   UINDEX(incr,2) = 1
   UINDEX(incr,3) = 1
   UINDEX(incr,4) = 1
   UINDEX(incr,5) = 1
   usrord(1) = 1
   usrord(2) = 2
   usrord(3) = 3
   usrord(4) = 4
   usrord(5) = 5

   call clock(cpu1)
   write(*,*)
   write(*,'(&
   &  ''6720 schrijfopdrachten (6720 cellen) van 16 bytes elk'')')
   DO 60 i= 1, 8
      UINDEX(start,5) = I
      UINDEX(stop ,5) = I
      DO 50 j= 1, 7
         UINDEX(start,4) = J
         UINDEX(stop ,4) = J
         DO 40 k= 1, 6
            UINDEX(start,3) = K
            UINDEX(stop ,3) = K
            DO 30 l= 1, 5
               UINDEX(start,2) = L
               UINDEX(stop ,2) = L
               DO 20 m= 1, 4
                  UINDEX(start,1) = M
                  UINDEX(stop ,1) = M
                  DO 10 n= 1, 4
                     buffer(n)= REAL(i)* REAL(j)* REAL(k)* REAL(l)*&
                     &REAL(m)* REAL(n)
10                CONTINUE
                  error= Putelt( fds, 'DATAGRP_TEST_3A',&
                  &'*', UINDEX, usrord, buffer)
                  IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
20             CONTINUE
30          CONTINUE
40       CONTINUE
50    CONTINUE
60 CONTINUE
   call clock(cpu2)
   elap_w = cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3A'',&
   &          '' written in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(&
   &  ''6720 schrijfopdrachten (6720 cellen) van 16 bytes elk'')')
   call clock(cpu1)
   DO 120 i= 1, 8
      UINDEX(start,5) = I
      UINDEX(stop ,5) = I
      DO 110 j= 1, 7
         UINDEX(start,4) = J
         UINDEX(stop ,4) = J
         DO 100 k= 1, 6
            UINDEX(start,3) = K
            UINDEX(stop ,3) = K
            DO 90 l= 1, 5
               UINDEX(start,2) = L
               UINDEX(stop ,2) = L
               DO 80 m= 1, 4
                  UINDEX(start,1) = M
                  UINDEX(stop ,1) = M
                  DO 70 n= 1, 4
                     buffer(n)= REAL(i)* REAL(j)* REAL(k)* REAL(l)*&
                     &REAL(m)* REAL(n)* 2.
70                CONTINUE
                  error= Putelt( fds, 'DATAGRP_TEST_3B',&
                  &'*', UINDEX, usrord, buffer)
                  IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
80             CONTINUE
90          CONTINUE
100      CONTINUE
110   CONTINUE
120 CONTINUE
   call clock(cpu2)
   elap_w = elap_w + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3B'',&
   &          '' written in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(&
   &  ''6720 schrijfopdrachten (6720 cellen) van 16 bytes elk'')')
   call clock(cpu1)
   DO 180 i= 1, 8
      UINDEX(start,5) = I
      UINDEX(stop ,5) = I
      DO 170 j= 1, 7
         UINDEX(start,4) = J
         UINDEX(stop ,4) = J
         DO 160 k= 1, 6
            UINDEX(start,3) = K
            UINDEX(stop ,3) = K
            DO 150 l= 1, 5
               UINDEX(start,2) = L
               UINDEX(stop ,2) = L
               DO 140 m= 1, 4
                  UINDEX(start,1) = M
                  UINDEX(stop ,1) = M
                  DO 130 n= 1, 4
                     buffer(n)= REAL(i)* REAL(j)* REAL(k)* REAL(l)*&
                     &REAL(m)* REAL(n)* 3.
130               CONTINUE
                  error= Putelt( fds, 'DATAGRP_TEST_3C',&
                  &'*', UINDEX, usrord, buffer)
                  IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
140            CONTINUE
150         CONTINUE
160      CONTINUE
170   CONTINUE
180 CONTINUE
   call clock(cpu2)
   elap_w = elap_w + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3C'',&
   &          '' written in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(&
   &  ''6720 schrijfopdrachten (6720 cellen) van 16 bytes elk'')')
   call clock(cpu1)
   DO 4120 i= 1, 8
      UINDEX(start,5) = I
      UINDEX(stop ,5) = I
      DO 4110 j= 1, 7
         UINDEX(start,4) = J
         UINDEX(stop ,4) = J
         DO 4100 k= 1, 6
            UINDEX(start,3) = K
            UINDEX(stop ,3) = K
            DO 4090 l= 1, 5
               UINDEX(start,2) = L
               UINDEX(stop ,2) = L
               DO 4080 m= 1, 4
                  UINDEX(start,1) = M
                  UINDEX(stop ,1) = M
                  DO 4070 n= 1, 4
                     buffer(n)= REAL(i)* REAL(j)* REAL(k)* REAL(l)*&
                     &REAL(m)* REAL(n)* 2.
4070              CONTINUE
                  error= Putelt( fds, 'DATAGRP_TEST_3D',&
                  &'*', UINDEX, usrord, buffer)
                  IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
4080           CONTINUE
4090        CONTINUE
4100     CONTINUE
4110  CONTINUE
4120 CONTINUE
   call clock(cpu2)
   elap_w = elap_w + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3D'',&
   &          '' written in [sec]'',1PE13.5)') cpu2-cpu1

   DO 185 i=1,26880
      buffer(i) = 0.0
185 CONTINUE

   write(*,*)
   write(*,'(&
   &  ''6720 leesopdrachten (6720 cellen) van 16 bytes elk'')')
   call clock(cpu1)
   DO 240 i= 1, 4
      UINDEX(start,1) = I
      UINDEX(stop ,1) = I
      DO 230 j= 1, 5
         UINDEX(start,2) = J
         UINDEX(stop ,2) = J
         DO 220 k= 1, 6
            UINDEX(start,3) = K
            UINDEX(stop ,3) = K
            DO 210 l= 1, 7
               UINDEX(start,4) = L
               UINDEX(stop ,4) = L
               DO 200 m= 1, 8
                  UINDEX(start,5) = M
                  UINDEX(stop ,5) = M
                  error= Getelt( fds, 'DATAGRP_TEST_3A',&
                  &'*', UINDEX, usrord, 4*4, buffer)
                  IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
                  DO 190 n= 1, 4
                     IF (INT( buffer(n)/ REAL(i)/REAL(j)/REAL(k)/REAL(l)/&
                     &REAL(m)-n).NE. 0) PRINT *,'error, i= ', i
190               CONTINUE
200            CONTINUE
210         CONTINUE
220      CONTINUE
230   CONTINUE
240 CONTINUE
   call clock(cpu2)
   elap_r = cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3A'',&
   &          '' read in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(&
   &  ''6720 leesopdrachten (6720 cellen) van 16 bytes elk'')')
   call clock(cpu1)
   DO 300 i= 1, 4
      UINDEX(start,1) = I
      UINDEX(stop ,1) = I
      DO 290 j= 1, 5
         UINDEX(start,2) = J
         UINDEX(stop ,2) = J
         DO 280 k= 1, 6
            UINDEX(start,3) = K
            UINDEX(stop ,3) = K
            DO 270 l= 1, 7
               UINDEX(start,4) = L
               UINDEX(stop ,4) = L
               DO 260 m= 1, 8
                  UINDEX(start,5) = M
                  UINDEX(stop ,5) = M
                  error= Getelt( fds, 'DATAGRP_TEST_3B',&
                  &'*', UINDEX, usrord, 4*4, buffer)
                  IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
                  DO 250 n= 1, 4
                     IF (INT( buffer(n)/ REAL(i)/REAL(j)/REAL(k)/REAL(l)/&
                     &REAL(m)-2*n).NE. 0) PRINT *,'error, i= ', i
250               CONTINUE
260            CONTINUE
270         CONTINUE
280      CONTINUE
290   CONTINUE
300 CONTINUE
   call clock(cpu2)
   elap_r = elap_r + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3B'',&
   &          '' read in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(&
   &  ''6720 leesopdrachten (6720 cellen) van 16 bytes elk'')')
   call clock(cpu1)
   DO 360 i= 1, 4
      UINDEX(start,1) = I
      UINDEX(stop ,1) = I
      DO 350 j= 1, 5
         UINDEX(start,2) = J
         UINDEX(stop ,2) = J
         DO 340 k= 1, 6
            UINDEX(start,3) = K
            UINDEX(stop ,3) = K
            DO 330 l= 1, 7
               UINDEX(start,4) = L
               UINDEX(stop ,4) = L
               DO 320 m= 1, 8
                  UINDEX(start,5) = M
                  UINDEX(stop ,5) = M
                  error= Getelt( fds, 'DATAGRP_TEST_3C',&
                  &'*', UINDEX, usrord, 4*4, buffer)
                  IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
                  DO 310 n= 1, 4
                     IF (INT( buffer(n)/ REAL(i)/REAL(j)/REAL(k)/REAL(l)/&
                     &REAL(m)-3*n).NE. 0) PRINT *,'error, i= ', i
310               CONTINUE
320            CONTINUE
330         CONTINUE
340      CONTINUE
350   CONTINUE
360 CONTINUE
   call clock(cpu2)
   elap_r = elap_r + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3C'',&
   &          '' read in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(&
   &  ''6720 leesopdrachten (6720 cellen) van 16 bytes elk'')')
   call clock(cpu1)
   DO 4300 i= 1, 4
      UINDEX(start,1) = I
      UINDEX(stop ,1) = I
      DO 4290 j= 1, 5
         UINDEX(start,2) = J
         UINDEX(stop ,2) = J
         DO 4280 k= 1, 6
            UINDEX(start,3) = K
            UINDEX(stop ,3) = K
            DO 4270 l= 1, 7
               UINDEX(start,4) = L
               UINDEX(stop ,4) = L
               DO 4260 m= 1, 8
                  UINDEX(start,5) = M
                  UINDEX(stop ,5) = M
                  error= Getelt( fds, 'DATAGRP_TEST_3D',&
                  &'*', UINDEX, usrord, 4*4, buffer)
                  IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
                  DO 4250 n= 1, 4
                     IF (INT( buffer(n)/ REAL(i)/REAL(j)/REAL(k)/REAL(l)/&
                     &REAL(m)-2*n).NE. 0) PRINT *,'error, i= ', i
4250              CONTINUE
4260           CONTINUE
4270        CONTINUE
4280     CONTINUE
4290  CONTINUE
4300 CONTINUE
   call clock(cpu2)
   elap_r = elap_r + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3D'',&
   &          '' read in [sec]'',1PE13.5)') cpu2-cpu1

   usrord(1)= 5
   usrord(2)= 4
   usrord(3)= 3
   usrord(4)= 2
   usrord(5)= 1
   UINDEX(start,1) = 1
   UINDEX(start,2) = 1
   UINDEX(start,3) = 1
   UINDEX(start,4) = 1
   UINDEX(start,5) = 1
   UINDEX(stop ,1) = 8
   UINDEX(stop ,2) = 7
   UINDEX(stop ,3) = 6
   UINDEX(stop ,4) = 5
   UINDEX(stop ,5) = 4

   write(*,*)
   write(*,'(&
   &  ''1 schrijfopdracht van 6720 elementen van 4 bytes'')')
   call clock(cpu1)
   n= 0
   DO 375 i= 1, 4
      DO 374 j= 1, 5
         DO 373 k= 1, 6
            DO 372 l= 1, 7
               DO 371 m= 1, 8
                  n= n+ 1
                  buffer(n)= REAL(i)* REAL(j)* REAL(k)* REAL(l)* REAL(m)
371            CONTINUE
372         CONTINUE
373      CONTINUE
374   CONTINUE
375 CONTINUE
   error= Putelt( fds, 'DATAGRP_TEST_3A',&
   &'ELEM_R_4', UINDEX, usrord, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   call clock(cpu2)
   elap_w = elap_w + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3A'',&
   &          '' written in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(&
   &  ''1 schrijfopdracht van 6720 elementen van 4 bytes'')')
   call clock(cpu1)
   n= 0
   DO 385 i= 1, 4
      DO 384 j= 1, 5
         DO 383 k= 1, 6
            DO 382 l= 1, 7
               DO 381 m= 1, 8
                  n= n+ 1
                  buffer(n)= REAL(i)* REAL(j)* REAL(k)* REAL(l)* REAL(m)* 2.
381            CONTINUE
382         CONTINUE
383      CONTINUE
384   CONTINUE
385 CONTINUE
   error= Putelt( fds, 'DATAGRP_TEST_3B',&
   &'ELEM_R_4', UINDEX, usrord, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   call clock(cpu2)
   elap_w = elap_w + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3B'',&
   &          '' written in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(&
   &  ''1 schrijfopdracht van 6720 elementen van 4 bytes'')')
   call clock(cpu1)
   n= 0
   DO 395 i= 1, 4
      DO 394 j= 1, 5
         DO 393 k= 1, 6
            DO 392 l= 1, 7
               DO 391 m= 1, 8
                  n= n+ 1
                  buffer(n)= REAL(i)* REAL(j)* REAL(k)* REAL(l)* REAL(m)* 3.
391            CONTINUE
392         CONTINUE
393      CONTINUE
394   CONTINUE
395 CONTINUE
   error= Putelt( fds, 'DATAGRP_TEST_3C',&
   &'ELEM_R_4', UINDEX, usrord, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   call clock(cpu2)
   elap_w = elap_w + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3C'',&
   &          '' written in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(&
   &  ''1 schrijfopdracht van 6720 elementen van 4 bytes'')')
   call clock(cpu1)
   n= 0
   DO 4385 i= 1, 4
      DO 4384 j= 1, 5
         DO 4383 k= 1, 6
            DO 4382 l= 1, 7
               DO 4381 m= 1, 8
                  n= n+ 1
                  buffer(n)= REAL(i)* REAL(j)* REAL(k)* REAL(l)* REAL(m)* 4.
4381           CONTINUE
4382        CONTINUE
4383     CONTINUE
4384  CONTINUE
4385 CONTINUE
   error= Putelt( fds, 'DATAGRP_TEST_3D',&
   &'ELEM_R_4', UINDEX, usrord, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   call clock(cpu2)
   elap_w = elap_w + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3D'',&
   &          '' written in [sec]'',1PE13.5)') cpu2-cpu1

   usrord(1)= 1
   usrord(2)= 2
   usrord(3)= 3
   usrord(4)= 4
   usrord(5)= 5
   UINDEX(stop ,1) = 4
   UINDEX(stop ,2) = 5
   UINDEX(stop ,3) = 6
   UINDEX(stop ,4) = 7
   UINDEX(stop ,5) = 8

   write(*,*)
   write(*,'(&
   &  ''1 schrijfopdracht van 6720 elementen van 12 bytes'')')
   call clock(cpu1)
   n= 0
   DO 406 i= 1, 8
      DO 405 j= 1, 7
         DO 404 k= 1, 6
            DO 403 l= 1, 5
               DO 402 m= 1, 4
                  DO 401 im= 1, 3
                     n= n+ 1
                     buffer(n)= REAL(i)* REAL(j)* REAL(k)* REAL(l)* REAL(m)*&
                     &REAL(im)
401               CONTINUE
402            CONTINUE
403         CONTINUE
404      CONTINUE
405   CONTINUE
406 CONTINUE
   error= Putelt( fds, 'DATAGRP_TEST_3A',&
   &'ELEM_R_4_DIM_1', UINDEX, usrord, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   call clock(cpu2)
   elap_w = elap_w + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3A'',&
   &          '' written in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(&
   &  ''1 schrijfopdracht van 6720 elementen van 12 bytes'')')
   call clock(cpu1)
   n= 0
   DO 416 i= 1, 8
      DO 415 j= 1, 7
         DO 414 k= 1, 6
            DO 413 l= 1, 5
               DO 412 m= 1, 4
                  DO 411 im= 1, 3
                     n= n+ 1
                     buffer(n)= REAL(i)* REAL(j)* REAL(k)* REAL(l)* REAL(m)*&
                     &REAL(im)* 2.
411               CONTINUE
412            CONTINUE
413         CONTINUE
414      CONTINUE
415   CONTINUE
416 CONTINUE
   error= Putelt( fds, 'DATAGRP_TEST_3B',&
   &'ELEM_R_4_DIM_1', UINDEX, usrord, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   call clock(cpu2)
   elap_w = elap_w + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3B'',&
   &          '' written in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(&
   &  ''1 schrijfopdracht van 6720 elementen van 12 bytes'')')
   call clock(cpu1)
   n= 0
   DO 426 i= 1, 8
      DO 425 j= 1, 7
         DO 424 k= 1, 6
            DO 423 l= 1, 5
               DO 422 m= 1, 4
                  DO 421 im= 1, 3
                     n= n+ 1
                     buffer(n)= REAL(i)* REAL(j)* REAL(k)* REAL(l)* REAL(m)*&
                     &REAL(im)* 3.
421               CONTINUE
422            CONTINUE
423         CONTINUE
424      CONTINUE
425   CONTINUE
426 CONTINUE
   error= Putelt( fds, 'DATAGRP_TEST_3C',&
   &'ELEM_R_4_DIM_1', UINDEX, usrord, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   call clock(cpu2)
   elap_w = elap_w + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3C'',&
   &          '' written in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(&
   &  ''1 schrijfopdracht van 6720 elementen van 12 bytes'')')
   call clock(cpu1)
   n= 0
   DO 4416 i= 1, 8
      DO 4415 j= 1, 7
         DO 4414 k= 1, 6
            DO 4413 l= 1, 5
               DO 4412 m= 1, 4
                  DO 4411 im= 1, 3
                     n= n+ 1
                     buffer(n)= REAL(i)* REAL(j)* REAL(k)* REAL(l)* REAL(m)*&
                     &REAL(im)* 4.
4411              CONTINUE
4412           CONTINUE
4413        CONTINUE
4414     CONTINUE
4415  CONTINUE
4416 CONTINUE
   error= Putelt( fds, 'DATAGRP_TEST_3D',&
   &'ELEM_R_4_DIM_1', UINDEX, usrord, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   call clock(cpu2)
   elap_w = elap_w + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3D'',&
   &          '' written in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,*)
   write(*,'(''Lees een cel van 16 bytes'')')
   call clock(cpu1)
   UINDEX(start,1) = 2
   UINDEX(start,2) = 2
   UINDEX(start,3) = 2
   UINDEX(start,4) = 2
   UINDEX(start,5) = 2
   UINDEX(stop ,1) = 2
   UINDEX(stop ,2) = 2
   UINDEX(stop ,3) = 2
   UINDEX(stop ,4) = 2
   UINDEX(stop ,5) = 2
   error= Getelt( fds, 'DATAGRP_TEST_3A',&
   &'*', UINDEX, usrord, 26880*4, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   DO 427 i= 1,3
      IF (INT(buffer(i)-(2*2*2*2*2*i)).NE.0)PRINT *,'error, i= ', i
427 CONTINUE
   IF (INT(buffer(4)-(2*2*2*2*2)).NE. 0) PRINT *,'error, i= ', i
   call clock(cpu2)
   elap_r = elap_r + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3A'',&
   &          '' read in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(''Lees een cel van 16 bytes'')')
   call clock(cpu1)
   UINDEX(start,1) = 3
   UINDEX(start,2) = 3
   UINDEX(start,3) = 3
   UINDEX(start,4) = 3
   UINDEX(start,5) = 3
   UINDEX(stop ,1) = 3
   UINDEX(stop ,2) = 3
   UINDEX(stop ,3) = 3
   UINDEX(stop ,4) = 3
   UINDEX(stop ,5) = 3
   error= Getelt( fds, 'DATAGRP_TEST_3B',&
   &'*', UINDEX, usrord, 26880*4, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   DO 428 i= 1,3
      IF (INT(buffer(i)-(3*3*3*3*3*2*i)).NE.0)PRINT*,'error, i= ', i
428 CONTINUE
   IF (INT(buffer(4)-(3*3*3*3*3*2)).NE.0)PRINT *,'error, i= ', i
   call clock(cpu2)
   elap_r = elap_r + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3B'',&
   &          '' read in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(''Lees een cel van 16 bytes'')')
   call clock(cpu1)
   UINDEX(start,1) = 4
   UINDEX(start,2) = 4
   UINDEX(start,3) = 4
   UINDEX(start,4) = 4
   UINDEX(start,5) = 4
   UINDEX(stop ,1) = 4
   UINDEX(stop ,2) = 4
   UINDEX(stop ,3) = 4
   UINDEX(stop ,4) = 4
   UINDEX(stop ,5) = 4
   error= Getelt( fds, 'DATAGRP_TEST_3C',&
   &'*', UINDEX, usrord, 26880*4, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   DO 429 i= 1,3
      IF (INT(buffer(i)-(4*4*4*4*4*3*i)).NE.0)PRINT *,'error, i= ', i
429 CONTINUE
   IF (INT(buffer(4)-(4*4*4*4*4*3)).NE.0) PRINT *,'error, i= ', i
   call clock(cpu2)
   elap_r = elap_r + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3C'',&
   &          '' read in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(''Lees een cel van 16 bytes'')')
   call clock(cpu1)
   UINDEX(start,1) = 3
   UINDEX(start,2) = 3
   UINDEX(start,3) = 3
   UINDEX(start,4) = 3
   UINDEX(start,5) = 3
   UINDEX(stop ,1) = 3
   UINDEX(stop ,2) = 3
   UINDEX(stop ,3) = 3
   UINDEX(stop ,4) = 3
   UINDEX(stop ,5) = 3
   error= Getelt( fds, 'DATAGRP_TEST_3D',&
   &'*', UINDEX, usrord, 26880*4, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   DO 4428 i= 1,3
      IF (INT(buffer(i)-(3*3*3*3*3*4*i)).NE.0) then
         PRINT*,'error, i=', i,' buffer=', NINT(buffer(i))
      endif
4428 CONTINUE
   IF (INT(buffer(4)-(3*3*3*3*3*4)).NE.0)&
   &PRINT *,'error, i=', i, ' buffer=', NINT(buffer(4))
   call clock(cpu2)
   elap_r = elap_r + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3D'',&
   &          '' read in [sec]'',1PE13.5)') cpu2-cpu1

   usrord(1)= 5
   usrord(2)= 3
   usrord(3)= 1
   usrord(4)= 2
   usrord(5)= 4
   UINDEX(start,1) = 1
   UINDEX(start,2) = 1
   UINDEX(start,3) = 1
   UINDEX(start,4) = 1
   UINDEX(start,5) = 1
   UINDEX(stop ,1) = 8
   UINDEX(stop ,2) = 6
   UINDEX(stop ,3) = 4
   UINDEX(stop ,4) = 5
   UINDEX(stop ,5) = 7

   write(*,*)
   write(*,'(&
   &  ''Lees 6720 elementen van 4 bytes in een (1) opdracht'')')
   call clock(cpu1)
   error= Getelt( fds, 'DATAGRP_TEST_3A',&
   &'ELEM_R_4', UINDEX, usrord, 26880*4, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   n= 0
   DO 435 i= 1, 7
      DO 434 j= 1, 5
         DO 433 k= 1, 4
            DO 432 l= 1, 6
               DO 431 m= 1, 8
                  n= n+ 1
                  IF (INT(buffer(n)/(REAL(i)* REAL(j)* REAL(k)* REAL(l)*&
                  &REAL(m))-1).NE. 0) PRINT *,'error, i= ', i
431            CONTINUE
432         CONTINUE
433      CONTINUE
434   CONTINUE
435 CONTINUE
   call clock(cpu2)
   elap_r = elap_r + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3A'',&
   &          '' read in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(&
   &  ''Lees 6720 elementen van 4 bytes in een (1) opdracht'')')
   call clock(cpu1)
   error= Getelt( fds, 'DATAGRP_TEST_3B',&
   &'ELEM_R_4', UINDEX, usrord, 26880*4, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   n= 0
   DO 445 i= 1, 7
      DO 444 j= 1, 5
         DO 443 k= 1, 4
            DO 442 l= 1, 6
               DO 441 m= 1, 8
                  n= n+ 1
                  IF (INT(buffer(n)/(REAL(i)* REAL(j)* REAL(k)* REAL(l)*&
                  &REAL(m))-2).NE. 0) PRINT *,'error, i= ', i
441            CONTINUE
442         CONTINUE
443      CONTINUE
444   CONTINUE
445 CONTINUE
   call clock(cpu2)
   elap_r = elap_r + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3B'',&
   &          '' read in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(&
   &  ''Lees 6720 elementen van 4 bytes in een (1) opdracht'')')
   call clock(cpu1)
   error= Getelt( fds, 'DATAGRP_TEST_3C',&
   &'ELEM_R_4', UINDEX, usrord, 26880*4, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   n= 0
   DO 455 i= 1, 7
      DO 454 j= 1, 5
         DO 453 k= 1, 4
            DO 452 l= 1, 6
               DO 451 m= 1, 8
                  n= n+ 1
                  IF (INT(buffer(n)/(REAL(i)* REAL(j)* REAL(k)* REAL(l)*&
                  &REAL(m))-3).NE. 0) PRINT *,'error, i= ', i
451            CONTINUE
452         CONTINUE
453      CONTINUE
454   CONTINUE
455 CONTINUE
   call clock(cpu2)
   elap_r = elap_r + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3C'',&
   &          '' read in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(&
   &  ''Lees 6720 elementen van 4 bytes in een (1) opdracht'')')
   call clock(cpu1)
   error= Getelt( fds, 'DATAGRP_TEST_3D',&
   &'ELEM_R_4', UINDEX, usrord, 26880*4, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   n= 0
   DO 4445 i= 1, 7
      DO 4444 j= 1, 5
         DO 4443 k= 1, 4
            DO 4442 l= 1, 6
               DO 4441 m= 1, 8
                  n= n+ 1
                  IF (INT(buffer(n)/(REAL(i)* REAL(j)* REAL(k)* REAL(l)*&
                  &REAL(m))-4).NE. 0) PRINT *,'error, i= ', i
4441           CONTINUE
4442        CONTINUE
4443     CONTINUE
4444  CONTINUE
4445 CONTINUE
   call clock(cpu2)
   elap_r = elap_r + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3D'',&
   &          '' read in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,*)
   write(*,'(&
   &  ''Lees 6720 elementen van 12 bytes in een (1) opdracht'')')
!        call clock(cpu1)
   error= Getelt( fds, 'DATAGRP_TEST_3A',&
   &'ELEM_R_4_DIM_1', UINDEX, usrord, 26880*4, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   n= 0
   DO 486 i= 1, 7
      DO 485 j= 1, 5
         DO 484 k= 1, 4
            DO 483 l= 1, 6
               DO 482 m= 1, 8
                  DO 481 im= 1, 3
                     n= n+ 1
                     IF (INT(buffer(n)/(REAL(i)* REAL(j)* REAL(k)* REAL(l)*&
                     &REAL(m)* REAL(im))-1).NE. 0) PRINT *,'error, i= ', i
481               CONTINUE
482            CONTINUE
483         CONTINUE
484      CONTINUE
485   CONTINUE
486 CONTINUE
   call clock(cpu2)
   elap_r = elap_r + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3A'',&
   &          '' read in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(&
   &  ''Lees 6720 elementen van 12 bytes in een (1) opdracht'')')
   call clock(cpu1)
   error= Getelt( fds, 'DATAGRP_TEST_3B',&
   &'ELEM_R_4_DIM_1', UINDEX, usrord,26880*4, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   n= 0
   DO 466 i= 1, 7
      DO 465 j= 1, 5
         DO 464 k= 1, 4
            DO 463 l= 1, 6
               DO 462 m= 1, 8
                  DO 461 im= 1, 3
                     n= n+ 1
                     IF (INT(buffer(n)/(REAL(i)* REAL(j)* REAL(k)* REAL(l)*&
                     &REAL(m)* REAL(im))-2).NE. 0) PRINT *,'error, i= ', i
461               CONTINUE
462            CONTINUE
463         CONTINUE
464      CONTINUE
465   CONTINUE
466 CONTINUE
   call clock(cpu2)
   elap_r = elap_r + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3B'',&
   &          '' read in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(&
   &  ''Lees 6720 elementen van 12 bytes in een (1) opdracht'')')
   call clock(cpu1)
   error= Getelt( fds, 'DATAGRP_TEST_3C',&
   &'ELEM_R_4_DIM_1', UINDEX, usrord,26880*4, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   n= 0
   DO 476 i= 1, 7
      DO 475 j= 1, 5
         DO 474 k= 1, 4
            DO 473 l= 1, 6
               DO 472 m= 1, 8
                  DO 471 im= 1, 3
                     n= n+ 1
                     IF (INT(buffer(n)/(REAL(i)* REAL(j)* REAL(k)* REAL(l)*&
                     &REAL(m)* REAL(im))-3).NE. 0) PRINT *,'error, i= ', i
471               CONTINUE
472            CONTINUE
473         CONTINUE
474      CONTINUE
475   CONTINUE
476 CONTINUE
   call clock(cpu2)
   elap_r = elap_r + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3C'',&
   &          '' read in [sec]'',1PE13.5)') cpu2-cpu1

   write(*,'(&
   &  ''Lees 6720 elementen van 12 bytes in een (1) opdracht'')')
   call clock(cpu1)
   error= Getelt( fds, 'DATAGRP_TEST_3D',&
   &'ELEM_R_4_DIM_1', UINDEX, usrord,26880*4, buffer)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
   n= 0
   DO 4466 i= 1, 7
      DO 4465 j= 1, 5
         DO 4464 k= 1, 4
            DO 4463 l= 1, 6
               DO 4462 m= 1, 8
                  DO 4461 im= 1, 3
                     n= n+ 1
                     IF (INT(buffer(n)/(REAL(i)* REAL(j)* REAL(k)* REAL(l)*&
                     &REAL(m)* REAL(im))-4).NE. 0) PRINT *,'error, i= ', i
4461              CONTINUE
4462           CONTINUE
4463        CONTINUE
4464     CONTINUE
4465  CONTINUE
4466 CONTINUE
   call clock(cpu2)
   elap_r = elap_r + cpu2-cpu1
   write(*,'(''DATAGRP_TEST_3D'',&
   &          '' read in [sec]'',1PE13.5)') cpu2-cpu1


   error= Clsdat( fds)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)

   error= Clsdef( fds)
   IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)

   ERROR =NEFERR( 0, ERRSTR)
   write(*,*)
   write(*,'(a)') trim(errstr)
   write(*,'(''Total elapsed write time [sec]: '', 1PE13.5)') elap_w
   write(*,'(''Total elapsed read time [sec] : '', 1PE13.5)') elap_r

END
!====================================================================
!     Convert clock time to seconds
!
subroutine clock( cpu )

   integer ihr, imin, isec, i100th
   real cpu

   ihr = 0
   imin = 0
   isec = 0
   i100th = 0
   cpu = 0.
!      CALL Gettim(ihr, imin, isec, i100th)
!      cpu = ihr*3600.0 + imin*60.0 + isec + i100th/100.0
!      call system_clock(ihr,imin)
!      cpu = ihr/real(imin)

end
