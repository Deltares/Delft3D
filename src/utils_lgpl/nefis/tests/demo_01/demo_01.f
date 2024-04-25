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

C     Company name                    : Deltares
C                                       P.O.Box 177
C                                       2600 MH Delft
C                                       The Netherlands
C     DESCRIPTION :
C
C     This program demonstrates how the NEFIS functions can be used
C     to write data to a NEFIS file, and how this data can be
C     retrieved again from the same file.
C
C=======================================================================
C     ..
C     .. Local Scalars ..
      CHARACTER*1024 ERRSTR
C                 .. character string to catch the NEFIS error message
      CHARACTER CODING*1
C                 .. indicates Y/N neutral representation of data
      INTEGER   DUMMY
C                 .. dummy parameter in function calls
     +         ,ERROR
C                 .. contains return-value of NEFIS functions
     +         ,I
C                 .. loop control variabel, array index
     +         ,J
C                 .. loop control variabel, array index
     +         ,K
C                 .. loop control variabel, array index
     +         ,OBSFIL
C                 .. unitnumber of user's observation file
C     ..
C     .. Local Arrays ..
      CHARACTER ELMNMS(2)*14
C                 .. cell element names
      INTEGER   FDS
C                 .. NEFIS FILE DESCRIPTOR
     +         ,GRPDMS(5)
C                 .. DIMENSIONS OF DATA GROUP TO DEFINE
     +         ,GRPORD(5)
C                 .. ORDER INFORMATION OF DATA GROUP TO DEFINE
     +         ,USRORD(5)
C                 .. ORDERING INFORMATION OF USER DATA
     +         ,USRIND(3,5)
C                 .. ORDERING INFORMATION OF USER DATA
      REAL      VLOCTY(3,4)
C                 .. ARRAY TO CONTAIN VELOCITY-ELEMENT INFO OF 4 POINTS
     +         ,DEPTH(12)
C                 .. ARRAY TO CONTAIN DEPTHS
     +         ,OBSDAT(4,100,10)
C                 .. ARRAY TO CONTAIN OBSERVED DATA
C     ..
C     .. EXTERNAL FUNCTIONS ..
      INTEGER   OPNDAT
C                 .. NEFIS-FUNCTION: OPEN A DATA FILE
     +         ,OPNDEF
C                 .. NEFIS-FUNCTION: OPEN A DEFINITION FILE
     +         ,DEFELM
C                 .. NEFIS-FUNCTION: DEFINE AN ELEMENT
     +         ,DEFCEL
C                 .. NEFIS-FUNCTION: DEFINE A CELL
     +         ,DEFGRP
C                 .. NEFIS-FUNCTION: DEFINE A GROUP
     +         ,CREDAT
C                 .. NEFIS-FUNCTION: CREATE SPACE FOR DATA ON DATA FILE
     +         ,PUTELT
C                 .. NEFIS-FUNCTION: WRITE DATA OF 1 OR MORE ELEMENTS
C                 ..                 TO DATA FILE
     +         ,GETELT
C                 .. NEFIS-FUNCTION: RETRIEVE DATA OF 1 OR MORE
C                 ..                 ELEMENTS FROM DATA FILE
     +         ,CLSDAT
C                 .. NEFIS-FUNCTION: CLOSE A DATA FILE
     +         ,CLSDEF
C                 .. NEFIS-FUNCTION: CLOSE A DEFINITION FILE
     +         ,NEFERR
C                 .. NEFIS-FUNCTION: RETRIEVE ERROR STRING
C=======================================================================
C                 ..
C                 .. LET US WRITE THE DATA IN NEUTRAL REPRESENTATION
      CODING = 'N'
C                 ..
C                 .. OPEN THE NEFIS DATA FILE AND DEFINITION FILE
      write(*,'(''Demo1: Open NEFIS data file'')')
      ERROR = OPNDAT(FDS, 'data_d01.dat', CODING)
      IF (ERROR .NE. 0) GOTO 9999
      write(*,'(''Demo1: Open NEFIS definition file'')')
      ERROR = OPNDEF(FDS, 'data_d01.def', CODING)
      IF (ERROR .NE. 0) GOTO 9999
C=======================================================================
C                 ..
C                 .. FIRST, LET'S DEFINE SOME ELEMENTS
C                 ..
C                 .. DEFINE A 1 DIMENSIONAL ELEMENT OF 3 REALS,
C                    NAMED: MEAN VELOCITY
      write(*,'(''Demo1: Define ELEMENT: MEAN VELOCITY (3)'')')
      ERROR = DEFELM (FDS, 'MEAN VELOCITY', 'REAL', 4,
     +                'VELOCITY', '[M3/S]',
     +                'Mean velocity in centre of river at ' //
     +                '3 different levels', 1, 3)
      IF (ERROR .NE. 0) GOTO 9999
C                 ..
C                 .. DEFINE A (0 DIMENSIONAL) ELEMENT OF 1 REAL,
C                 .. NAMED: WATERDEPTH
      write(*,'(''Demo1: Define ELEMENT: WATERDEPTH'')')
      DUMMY = 1
      ERROR = DEFELM (FDS, 'WATERDEPTH', 'REAL', 4, 'DEPTH',
     +               '[M]', 'DEPTH AT CENTRE OF RIVER', 1, DUMMY)
      IF (ERROR .NE. 0) GOTO 9999
C                 ..
C                 .. LET'S DEFINE A CELL TO CONTAIN OBSERVATIONS AT A
C                 .. CERTAIN POINT AND A CERTAIN PLACE,
C                 .. NAMED: OBSERVATION
C                 ..
      ELMNMS(1) = 'MEAN VELOCITY'
      ELMNMS(2) = 'WATERDEPTH'
      write(*,'(''Demo1: Define CELL: '',
     *             ''OBSERVATION = MEAN VELOCITY + WATERDEPTH'')')
      ERROR = DEFCEL(FDS, 'OBSERVATION', 2, ELMNMS)
      IF (ERROR .NE. 0) GOTO 9999
C                 ..
C                 .. DEFINE A GROUP FOR 10 DIFFERENT LOCATIONS,
C                 .. ABLE TO CONTAIN 100 OBSERVATIONS (TIME SERIES)
C                 .. FOR EACH LOCATION, NAMED: RIVER DATA
C                 ..
      GRPDMS(1) = 100
C                 .. MAX. 100 OBSERVATIONS FOR EACH LOCATION
      GRPDMS(2) = 10
C                 .. MAX. 10 LOCATIONS
      GRPORD(1) = 2
      GRPORD(2) = 1
C                 .. CELLS WILL BE STORED IN THE FILE IN THE ORDER:
C                 .. (1,1), (1,2) ..... (1,10), (2,1), (2,2).... ETC.
      write(*,'(''Demo1: Define GROUP:'',
     *             '' RIVERDATA = OBSERVATION (100,10)'')')
      ERROR = DEFGRP (FDS, 'RIVERDATA', 'OBSERVATION', 2, GRPDMS,
     +                GRPORD)
      IF (ERROR .NE. 0) GOTO 9999
C                 .. END OF DEFINITION PART
C=======================================================================
C                 ..
C                 .. NOW, LET'S CREATE SPACE ON THE DATA FILE FOR
C                 .. RED RIVER DATA
      write(*,'(''Demo1: Create space for data labelled: RED RIVER,'',
     *             '' using THE RIVERDATA GROUP DEFINITION'')')
      ERROR = CREDAT (FDS, 'RED RIVER', 'RIVERDATA')
      IF (ERROR .NE. 0) GOTO 9999
C                 ..
C                 .. NOW, READ ALL FIELD OBSERVATIONS FROM A FILE
      write(*,'(''Demo1: Read observation data from input file'',
     *             '' (not a NEFIS action)'')')
      OPEN (NEWUNIT=OBSFIL,FILE='observ.inp')
      DO 10 I = 1, 10
         READ (OBSFIL,*)
         DO 20 J = 1, 100
            READ (OBSFIL,*) (OBSDAT(K,J,I), K=1,4)
C                 .. VELOCITIES AND WATERDEPTH AT LOCATION I, TIME J
   20    CONTINUE
   10 CONTINUE
      CLOSE (OBSFIL)
C=======================================================================
C                 ..
C                 .. OBSERVATIONS CAN BE WRITTEN TO THE NEFIS DATA FILE
C                 .. FOR EXAMPLE CELL AFTER CELL
      USRORD(1) = 1
      USRORD(2) = 2
C                 .. THIS IS THE FORTRAN ORDER, IE.:
C            (1,1), (2,1) .. (100,1), (1,2), (2,2) .. ETC.
C     write(*,'(''Demo1: Write DATA to NEFIS file, ONE cell at a time'')')
C     DO 40 I = 1, 100
C        DO 30 J = 1, 10
C           USRIND(1,1) = I
C           USRIND(2,1) = I
C           USRIND(3,1) = 1
C           USRIND(1,2) = J
C           USRIND(2,2) = J
C           USRIND(3,2) = 1
C           USRIND(1,3) = 3
C           USRIND(2,3) = 3
C           USRIND(3,3) = 3
C           USRIND(1,4) = 4
C           USRIND(2,4) = 4
C           USRIND(3,4) = 4
C           USRIND(1,5) = 5
C           USRIND(2,5) = 5
C           USRIND(3,5) = 5
C           write(*,'(''Demo1: Data: '',i8)') OBSDAT(4,I,J)
C           ERROR = PUTELT (FDS, 'RED RIVER',
C    +                      'WATERDEPTH', USRIND, USRORD, OBSDAT(4,I,J))
C           IF (ERROR .NE. 0) GOTO 9999
C  30    CONTINUE
C  40 CONTINUE
C     WRITE(*,'(''Demo1: RED RIVER written in [sec]'',1PE13.5)')
C    *        cpu2-cpu1
C                 ..
C                 .. OR ALL CELLS TOGETHER (10*100 CELLS)
      USRIND(1,1) = 1
      USRIND(2,1) = 100
      USRIND(3,1) = 1
      USRIND(1,2) = 1
      USRIND(2,2) = 10
      USRIND(3,2) = 1
C                 .. INDEX OF FIRST CELL TO STORE INFORMATION TO
      USRORD(1) = 1
      USRORD(2) = 2
C                 .. THIS IS THE FORTRAN ORDER, IE.:
C            (1,1), (2,1) .. (100,1), (1,2), (2,2) .. ETC.
      write(*,'(''Demo1: Write the same DATA, '',
     *             ''but now all cells at ONE go'')')
      ERROR = PUTELT (FDS, 'RED RIVER', '*',
     +                USRIND, USRORD, OBSDAT)
      IF (ERROR .NE. 0) GOTO 9999
C                 .. ALL DATA IS NOW STORED ON THE NEFIS DATA FILE
C=======================================================================
C                 ..
C                 .. LET'S DO SOME RETRIEVAL
C                 ..
C                 .. LET'S RETRIEVE THE VELOCITIES FROM LOCATIONS
C                 .. 6-9 AT TIME 54, IE. FROM
C                 .. CELLS (54,6), (54,7), (54,8) AND (54,9).
C                 .. THE PERFORMANCE WILL BE RATHER GOOD, BECAUSE THE
C                 .. DATA ON THE NEFIS DATA FILE IS WRITTEN IN THIS
C                 .. ORDER (SEE DEFGRP).
      USRIND(1,1) = 6
      USRIND(2,1) = 9
      USRIND(3,1) = 1
      USRIND(1,2) = 54
      USRIND(2,2) = 54
      USRIND(3,2) = 1

      USRORD(1) = 2
      USRORD(2) = 1
C                 .. MEANS: FROM CELL (54,6), (54,7), (54,8) AND (54,9)
      write(*,'(''Demo1: Start retrieval'')')
      ERROR = GETELT (FDS, 'RED RIVER','MEAN VELOCITY',
     +                USRIND, USRORD, 48         , VLOCTY        )
      IF (ERROR .NE. 0) GOTO 9999
C                 ..
      write(*,'(''Demo1: Velocities at time 54'')')
      DO 50 I = 1, 4
         write (*,'(A,I2,'':'',3F8.1)') '  Location ', I+5,
     +                                  (VLOCTY(J,I), J=1,3)
   50 CONTINUE
C                 ..
C                 .. NOW, RETRIEVE AT LOCATION 7 THE WATERDEPTHS FROM
C                 .. TIME 35-46
      USRIND(1,1) = 7
      USRIND(2,1) = 7
      USRIND(3,1) = 1
      USRIND(1,2) = 35
      USRIND(2,2) = 46
      USRIND(3,2) = 1

      USRORD(1) = 2
      USRORD(2) = 1
C                 .. MEANS: FROM CELL (35,7), (36,7), (37,7) .... (46,7)
      ERROR = GETELT (FDS, 'RED RIVER', 'WATERDEPTH',
     +                USRIND, USRORD, 48, DEPTH)
      IF (ERROR .NE. 0) GOTO 9999
C                 ..
      write (*,'(''Demo1: Waterdepths at location 7'')')
      DO 60 I = 1, 12
        write (*,'(A,I2,'':'',F8.1)') '  Time ', I+34, DEPTH(I)
   60 CONTINUE
C=======================================================================
C
C                 .. close the NEFIS files
      write(*,'(''Demo1: Close the NEFIS files'')')
      ERROR = CLSDAT (FDS)
      if (error .ne. 0) goto 9999
      ERROR = CLSDEF (FDS)
 9999 continue

      ERROR = NEFERR( 0, ERRSTR)
      write(*,'(a)') trim(errstr)

      write(*,'(''Demo1: End of demonstration'')')
      END
