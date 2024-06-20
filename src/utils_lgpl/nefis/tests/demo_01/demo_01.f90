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
PROGRAM DEMO01

!     Company name                    : Deltares
!                                       P.O.Box 177
!                                       2600 MH Delft
!                                       The Netherlands
!     DESCRIPTION :
!
!     This program demonstrates how the NEFIS functions can be used
!     to write data to a NEFIS file, and how this data can be
!     retrieved again from the same file.
!
!=======================================================================
!     ..
!     .. Local Scalars ..
   CHARACTER*1024 ERRSTR
!                 .. character string to catch the NEFIS error message
   CHARACTER CODING*1
!                 .. indicates Y/N neutral representation of data
   INTEGER   DUMMY&
!                 .. dummy parameter in function calls
   &,ERROR&
!                 .. contains return-value of NEFIS functions
   &,I&
!                 .. loop control variabel, array index
   &,J&
!                 .. loop control variabel, array index
   &,K&
!                 .. loop control variabel, array index
   &,OBSFIL
!                 .. unitnumber of user's observation file
!     ..
!     .. Local Arrays ..
   CHARACTER ELMNMS(2)*14
!                 .. cell element names
   INTEGER   FDS&
!                 .. NEFIS FILE DESCRIPTOR
   &,GRPDMS(5)&
!                 .. DIMENSIONS OF DATA GROUP TO DEFINE
   &,GRPORD(5)&
!                 .. ORDER INFORMATION OF DATA GROUP TO DEFINE
   &,USRORD(5)&
!                 .. ORDERING INFORMATION OF USER DATA
   &,USRIND(3,5)
!                 .. ORDERING INFORMATION OF USER DATA
   REAL      VLOCTY(3,4)&
!                 .. ARRAY TO CONTAIN VELOCITY-ELEMENT INFO OF 4 POINTS
   &,DEPTH(12)&
!                 .. ARRAY TO CONTAIN DEPTHS
   &,OBSDAT(4,100,10)
!                 .. ARRAY TO CONTAIN OBSERVED DATA
!     ..
!     .. EXTERNAL FUNCTIONS ..
   INTEGER   OPNDAT&
!                 .. NEFIS-FUNCTION: OPEN A DATA FILE
   &,OPNDEF&
!                 .. NEFIS-FUNCTION: OPEN A DEFINITION FILE
   &,DEFELM&
!                 .. NEFIS-FUNCTION: DEFINE AN ELEMENT
   &,DEFCEL&
!                 .. NEFIS-FUNCTION: DEFINE A CELL
   &,DEFGRP&
!                 .. NEFIS-FUNCTION: DEFINE A GROUP
   &,CREDAT&
!                 .. NEFIS-FUNCTION: CREATE SPACE FOR DATA ON DATA FILE
   &,PUTELT&
!                 .. NEFIS-FUNCTION: WRITE DATA OF 1 OR MORE ELEMENTS
!                 ..                 TO DATA FILE
   &,GETELT&
!                 .. NEFIS-FUNCTION: RETRIEVE DATA OF 1 OR MORE
!                 ..                 ELEMENTS FROM DATA FILE
   &,CLSDAT&
!                 .. NEFIS-FUNCTION: CLOSE A DATA FILE
   &,CLSDEF&
!                 .. NEFIS-FUNCTION: CLOSE A DEFINITION FILE
   &,NEFERR
!                 .. NEFIS-FUNCTION: RETRIEVE ERROR STRING
!=======================================================================
!                 ..
!                 .. LET US WRITE THE DATA IN NEUTRAL REPRESENTATION
   CODING = 'N'
!                 ..
!                 .. OPEN THE NEFIS DATA FILE AND DEFINITION FILE
   write(*,'(''Demo1: Open NEFIS data file'')')
   ERROR = OPNDAT(FDS, 'data_d01.dat', CODING)
   IF (ERROR .NE. 0) GOTO 9999
   write(*,'(''Demo1: Open NEFIS definition file'')')
   ERROR = OPNDEF(FDS, 'data_d01.def', CODING)
   IF (ERROR .NE. 0) GOTO 9999
!=======================================================================
!                 ..
!                 .. FIRST, LET'S DEFINE SOME ELEMENTS
!                 ..
!                 .. DEFINE A 1 DIMENSIONAL ELEMENT OF 3 REALS,
!                    NAMED: MEAN VELOCITY
   write(*,'(''Demo1: Define ELEMENT: MEAN VELOCITY (3)'')')
   ERROR = DEFELM (FDS, 'MEAN VELOCITY', 'REAL', 4,&
   &'VELOCITY', '[M3/S]',&
   &'Mean velocity in centre of river at ' //&
   &'3 different levels', 1, 3)
   IF (ERROR .NE. 0) GOTO 9999
!                 ..
!                 .. DEFINE A (0 DIMENSIONAL) ELEMENT OF 1 REAL,
!                 .. NAMED: WATERDEPTH
   write(*,'(''Demo1: Define ELEMENT: WATERDEPTH'')')
   DUMMY = 1
   ERROR = DEFELM (FDS, 'WATERDEPTH', 'REAL', 4, 'DEPTH',&
   &'[M]', 'DEPTH AT CENTRE OF RIVER', 1, DUMMY)
   IF (ERROR .NE. 0) GOTO 9999
!                 ..
!                 .. LET'S DEFINE A CELL TO CONTAIN OBSERVATIONS AT A
!                 .. CERTAIN POINT AND A CERTAIN PLACE,
!                 .. NAMED: OBSERVATION
!                 ..
   ELMNMS(1) = 'MEAN VELOCITY'
   ELMNMS(2) = 'WATERDEPTH'
   write(*,'(''Demo1: Define CELL: '',&
   &             ''OBSERVATION = MEAN VELOCITY + WATERDEPTH'')')
   ERROR = DEFCEL(FDS, 'OBSERVATION', 2, ELMNMS)
   IF (ERROR .NE. 0) GOTO 9999
!                 ..
!                 .. DEFINE A GROUP FOR 10 DIFFERENT LOCATIONS,
!                 .. ABLE TO CONTAIN 100 OBSERVATIONS (TIME SERIES)
!                 .. FOR EACH LOCATION, NAMED: RIVER DATA
!                 ..
   GRPDMS(1) = 100
!                 .. MAX. 100 OBSERVATIONS FOR EACH LOCATION
   GRPDMS(2) = 10
!                 .. MAX. 10 LOCATIONS
   GRPORD(1) = 2
   GRPORD(2) = 1
!                 .. CELLS WILL BE STORED IN THE FILE IN THE ORDER:
!                 .. (1,1), (1,2) ..... (1,10), (2,1), (2,2).... ETC.
   write(*,'(''Demo1: Define GROUP:'',&
   &             '' RIVERDATA = OBSERVATION (100,10)'')')
   ERROR = DEFGRP (FDS, 'RIVERDATA', 'OBSERVATION', 2, GRPDMS,&
   &GRPORD)
   IF (ERROR .NE. 0) GOTO 9999
!                 .. END OF DEFINITION PART
!=======================================================================
!                 ..
!                 .. NOW, LET'S CREATE SPACE ON THE DATA FILE FOR
!                 .. RED RIVER DATA
   write(*,'(''Demo1: Create space for data labelled: RED RIVER,'',&
   &             '' using THE RIVERDATA GROUP DEFINITION'')')
   ERROR = CREDAT (FDS, 'RED RIVER', 'RIVERDATA')
   IF (ERROR .NE. 0) GOTO 9999
!                 ..
!                 .. NOW, READ ALL FIELD OBSERVATIONS FROM A FILE
   write(*,'(''Demo1: Read observation data from input file'',&
   &             '' (not a NEFIS action)'')')
   OPEN (NEWUNIT=OBSFIL,FILE='observ.inp')
   DO 10 I = 1, 10
      READ (OBSFIL,*)
      DO 20 J = 1, 100
         READ (OBSFIL,*) (OBSDAT(K,J,I), K=1,4)
!                 .. VELOCITIES AND WATERDEPTH AT LOCATION I, TIME J
20    CONTINUE
10 CONTINUE
   CLOSE (OBSFIL)
!=======================================================================
!                 ..
!                 .. OBSERVATIONS CAN BE WRITTEN TO THE NEFIS DATA FILE
!                 .. FOR EXAMPLE CELL AFTER CELL
   USRORD(1) = 1
   USRORD(2) = 2
!                 .. THIS IS THE FORTRAN ORDER, IE.:
!            (1,1), (2,1) .. (100,1), (1,2), (2,2) .. ETC.
!     write(*,'(''Demo1: Write DATA to NEFIS file, ONE cell at a time'')')
!     DO 40 I = 1, 100
!        DO 30 J = 1, 10
!           USRIND(1,1) = I
!           USRIND(2,1) = I
!           USRIND(3,1) = 1
!           USRIND(1,2) = J
!           USRIND(2,2) = J
!           USRIND(3,2) = 1
!           USRIND(1,3) = 3
!           USRIND(2,3) = 3
!           USRIND(3,3) = 3
!           USRIND(1,4) = 4
!           USRIND(2,4) = 4
!           USRIND(3,4) = 4
!           USRIND(1,5) = 5
!           USRIND(2,5) = 5
!           USRIND(3,5) = 5
!           write(*,'(''Demo1: Data: '',i8)') OBSDAT(4,I,J)
!           ERROR = PUTELT (FDS, 'RED RIVER',
!    +                      'WATERDEPTH', USRIND, USRORD, OBSDAT(4,I,J))
!           IF (ERROR .NE. 0) GOTO 9999
!  30    CONTINUE
!  40 CONTINUE
!     WRITE(*,'(''Demo1: RED RIVER written in [sec]'',1PE13.5)')
!    *        cpu2-cpu1
!                 ..
!                 .. OR ALL CELLS TOGETHER (10*100 CELLS)
   USRIND(1,1) = 1
   USRIND(2,1) = 100
   USRIND(3,1) = 1
   USRIND(1,2) = 1
   USRIND(2,2) = 10
   USRIND(3,2) = 1
!                 .. INDEX OF FIRST CELL TO STORE INFORMATION TO
   USRORD(1) = 1
   USRORD(2) = 2
!                 .. THIS IS THE FORTRAN ORDER, IE.:
!            (1,1), (2,1) .. (100,1), (1,2), (2,2) .. ETC.
   write(*,'(''Demo1: Write the same DATA, '',&
   &             ''but now all cells at ONE go'')')
   ERROR = PUTELT (FDS, 'RED RIVER', '*',&
   &USRIND, USRORD, OBSDAT)
   IF (ERROR .NE. 0) GOTO 9999
!                 .. ALL DATA IS NOW STORED ON THE NEFIS DATA FILE
!=======================================================================
!                 ..
!                 .. LET'S DO SOME RETRIEVAL
!                 ..
!                 .. LET'S RETRIEVE THE VELOCITIES FROM LOCATIONS
!                 .. 6-9 AT TIME 54, IE. FROM
!                 .. CELLS (54,6), (54,7), (54,8) AND (54,9).
!                 .. THE PERFORMANCE WILL BE RATHER GOOD, BECAUSE THE
!                 .. DATA ON THE NEFIS DATA FILE IS WRITTEN IN THIS
!                 .. ORDER (SEE DEFGRP).
   USRIND(1,1) = 6
   USRIND(2,1) = 9
   USRIND(3,1) = 1
   USRIND(1,2) = 54
   USRIND(2,2) = 54
   USRIND(3,2) = 1

   USRORD(1) = 2
   USRORD(2) = 1
!                 .. MEANS: FROM CELL (54,6), (54,7), (54,8) AND (54,9)
   write(*,'(''Demo1: Start retrieval'')')
   ERROR = GETELT (FDS, 'RED RIVER','MEAN VELOCITY',&
   &USRIND, USRORD, 48         , VLOCTY        )
   IF (ERROR .NE. 0) GOTO 9999
!                 ..
   write(*,'(''Demo1: Velocities at time 54'')')
   DO 50 I = 1, 4
      write (*,'(A,I2,'':'',3F8.1)') '  Location ', I+5,&
      &(VLOCTY(J,I), J=1,3)
50 CONTINUE
!                 ..
!                 .. NOW, RETRIEVE AT LOCATION 7 THE WATERDEPTHS FROM
!                 .. TIME 35-46
   USRIND(1,1) = 7
   USRIND(2,1) = 7
   USRIND(3,1) = 1
   USRIND(1,2) = 35
   USRIND(2,2) = 46
   USRIND(3,2) = 1

   USRORD(1) = 2
   USRORD(2) = 1
!                 .. MEANS: FROM CELL (35,7), (36,7), (37,7) .... (46,7)
   ERROR = GETELT (FDS, 'RED RIVER', 'WATERDEPTH',&
   &USRIND, USRORD, 48, DEPTH)
   IF (ERROR .NE. 0) GOTO 9999
!                 ..
   write (*,'(''Demo1: Waterdepths at location 7'')')
   DO 60 I = 1, 12
      write (*,'(A,I2,'':'',F8.1)') '  Time ', I+34, DEPTH(I)
60 CONTINUE
!=======================================================================
!
!                 .. close the NEFIS files
   write(*,'(''Demo1: Close the NEFIS files'')')
   ERROR = CLSDAT (FDS)
   if (error .ne. 0) goto 9999
   ERROR = CLSDEF (FDS)
9999 continue

   ERROR = NEFERR( 0, ERRSTR)
   write(*,'(a)') trim(errstr)

   write(*,'(''Demo1: End of demonstration'')')
END
