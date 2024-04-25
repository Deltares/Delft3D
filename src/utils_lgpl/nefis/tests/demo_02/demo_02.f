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
      PROGRAM DEMO2
C
C     Company name                    : Deltares
C                                       P.O.Box 177
C                                       2600 MH Delft
C                                       The Netherlands
C--------------------------------------------------------------------------------
C     System: NEFIS
C
C     $Header: /delft3d/libraries/nefis/demo/demo_02/demo_02.f 2     10/03/06 9:56 Mooiman $
C--------------------------------------------------------------------------------
C     Programmer                      : A. Hoekstra
C     Project                         : NEutral FIle Structure
C--------------------------------------------------------------------------------
C      * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
C
C     - This demo-program demonstrates the use of NEFIS store-
C       and retrieval functions. Special is the use of a
C       datagroup with a variable dimension.
C
C       This program performs the following tasks:
C       - create an element, cel and a 3-d group defintion
C       - create a data group
C       - store data in this group
C       - retrieve data from this group, using a
C         different view
C       - retrieve data using a filter
C
C     Note: the error-return code from the NEFIS-functions is
C           not checked
C--------------------------------------------------------------------------------
C     ..
C     .. Scalars
      CHARACTER*1024 ERRSTR
C                 .. character string to catch the NEFIS error message
      CHARACTER CODING*1
C                 .. indicates Y/N neutral representation of data
      INTEGER   ERROR
C                 .. contains return-value of NEFIS-functions
C     ..
C     .. Arrays
      INTEGER   FDS
C                 .. nefis file descriptor
C     ..
C     .. Declarations of NEFIS-functions
      INTEGER   CLSDAT
     +         ,CLSDEF
     +         ,CREDAT
     +         ,FLSDAT
     +         ,OPNDAT
     +         ,OPNDEF
     +         ,NEFERR
C
      EXTERNAL  CLSDAT
     +         ,CLSDEF
     +         ,CREDAT
     +         ,FLSDAT
     +         ,OPNDAT
     +         ,OPNDEF
     +         ,NEFERR
C     ..
C     .. Executable statements
C
C                 ..
C                 .. Open a definition file
      CODING = 'N'
      ERROR = OPNDEF (FDS, 'data_d02.def', CODING)
      IF (ERROR.NE.0) goto 9999
C                 ..
C                 .. Define element, cel, and group-definition
      CALL DEFINE (FDS)
C                 ..
C                 .. Open a data file
      CODING = 'N'
      ERROR = OPNDAT (FDS, 'data_d02.dat', CODING)
      IF (ERROR.NE.0) goto 9999
C                 ..
C                 .. Create space for data
      ERROR = CREDAT (FDS, 'GrpNaam', 'Groep')
      IF (ERROR.NE.0) goto 9999
C
      ERROR = FLSDAT (FDS)
      IF (ERROR.NE.0) goto 9999
C                 ..
C                 .. Write data to file
      CALL PUTDAT (FDS)
C                 ..
C                 .. Retrieve data, using a different view
      CALL DTVIEW (FDS)
C                 ..
C                 .. Retrieve a part of the data
      CALL FILTER (FDS)
C                 ..
C                 .. Close the files
 9999 continue

      if (error.eq.0) ERROR = CLSDEF (FDS)
      if (error.eq.0) ERROR = CLSDAT (FDS)

      ERROR = NEFERR( 1, ERRSTR)
C
      END
C================================================================================
      SUBROUTINE DEFINE (FDS)
C
      INTEGER   FDS
C
      INTEGER       ERROR
      CHARACTER*134 ERRSTR

      INTEGER   GRPDMS(5)
     +         ,GRPORD(5)
C
      INTEGER   DEFCEL
     +         ,DEFELM
     +         ,DEFGRP
     +         ,FLSDEF
     +         ,NEFERR
      EXTERNAL  DEFCEL
     +         ,DEFELM
     +         ,DEFGRP
     +         ,FLSDEF
     +         ,NEFERR
C     ..
C     .. Executable statements
C
C                 ..
C                 .. Define a simple element, type Real*4
      ERROR = DEFELM (FDS, 'ElmName', 'Integer',  4,
     +                'ElmQuantity', 'ElmUnity', 'ElmDescription',
     +                1, 1)
      IF (ERROR.NE.0) goto 9999
C                 ..
C                 .. Define a cel with only one real value
      ERROR = DEFCEL (FDS, 'Cell', 1, 'ElmName')
      IF (ERROR.NE.0) goto 9999
C                 ..
C                 .. Define a 3-d group of dimension (3,5,0),
C                 .. so a group with a variable dimension
      GRPDMS(1) = 3
      GRPDMS(2) = 5
      GRPDMS(3) = 0
      GRPORD(1) = 1
      GRPORD(2) = 3
      GRPORD(3) = 2
      ERROR = DEFGRP (FDS, 'Groep', 'Cell', 3, GRPDMS, GRPORD)
      IF (ERROR.NE.0) goto 9999
C                 ..
C                 .. Flush buffers to file
      ERROR = FLSDEF (FDS)
      IF (ERROR.NE.0) goto 9999
C
 9999 continue
      ERROR = NEFERR( 1, ERRSTR)
      END
C================================================================================
      SUBROUTINE PUTDAT (FDS)
C
      CHARACTER*1024 ERRSTR
C
      INTEGER   FDS
C
      INTEGER   START, STOP, INCR
      PARAMETER (START=1, STOP=2, INCR=3)
      equivalence(AARRAY,ARRAY)
C
      CHARACTER SPACE*7
      INTEGER   COL
     +         ,ERROR
     +         ,PLANE
     +         ,ROW
      INTEGER   UINDEX(3,5)
     +         ,USRORD(5)
      INTEGER   ARRAY (3,5,7)
      INTEGER   AARRAY (105)
C
      INTEGER   FLSDAT
     +         ,PUTELT
     +         ,NEFERR
      EXTERNAL  FLSDAT
     +         ,PUTELT
     +         ,NEFERR
C     ..
C     .. Executable statements
C
      SPACE = '       '
C                 ..
C                 .. Set view to (3,5,*)
      USRORD (1) = 1
      USRORD (2) = 2
      USRORD (3) = 3
C                 ..
C                 .. Define indices for each dimension
      UINDEX (START,1) = 1
      UINDEX (STOP ,1) = 3
      UINDEX (INCR ,1) = 1
      UINDEX (START,2) = 1
      UINDEX (STOP ,2) = 5
      UINDEX (INCR ,2) = 1
      UINDEX (START,3) = 1
      UINDEX (STOP ,3) = 7
      UINDEX (INCR ,3) = 1
C                 ..
C                 .. Fill array with values
      DO 30 PLANE = 1,7
         DO 20 COL = 1,5
            DO 10 ROW = 1,3
               ARRAY (ROW, COL, PLANE) = ROW*1000+COL*100+PLANE
   10       CONTINUE
   20    CONTINUE
   30 CONTINUE
C                 ..
C                 .. Write data to file
      ERROR = PUTELT (FDS, 'GrpNaam', '*'
c     ERROR = PUTELT (FDS, 'GrpNaam', 'ElmName'
     +               ,UINDEX, USRORD, ARRAY)
      IF (ERROR.NE.0) goto 9999
C                 ..
C                 .. Flush the buffers
      ERROR = FLSDAT (FDS)
      IF (ERROR.NE.0) goto 9999
C                 ..
C                 .. Output data to screen
c     write(*,'('' ARRAY(105) written to file:'')')
c     DO 11 PLANE = 1,105
c       WRITE (*,'(  I10)') AARRAY(PLANE)
c  11 CONTINUE
      write(*,'('' ARRAY(3,5,7) written to file:'')')
      DO 50 PLANE = 1,7
         DO 40 COL = 1,5
C           WRITE (*,'(  3F10.2)')
            WRITE (*,'(  3I10)')
     +          (ARRAY(ROW,COL,PLANE),ROW=1,3)
   40    CONTINUE
         WRITE (*,*)
   50 CONTINUE
C
 9999 continue
      ERROR = NEFERR( 1, ERRSTR)
      END
C================================================================================
      SUBROUTINE DTVIEW (FDS)
C
      CHARACTER*1024 ERRSTR
C
      INTEGER   FDS
C
      INTEGER   START, STOP, INCR
      PARAMETER (START=1, STOP=2, INCR=3)
C
      CHARACTER SPACE*7
      INTEGER   COL
     +         ,ERROR
     +         ,PLANE
     +         ,ROW
      INTEGER   UINDEX(3,5)
     +         ,USRORD(3)
      INTEGER   ARRAY (7,3,5)
C
      INTEGER   GETELT
     +         ,NEFERR
      EXTERNAL  GETELT
     +         ,NEFERR
C     ..
C     .. Executable statements
C
      SPACE = '       '
C                 ..
C                 .. Change view to (*,3,5)
      USRORD (1) = 3
      USRORD (2) = 1
      USRORD (3) = 2
C                 ..
C                 .. Define indices for each dimension
      UINDEX (START,1) = 1
      UINDEX (STOP ,1) = 7
      UINDEX (INCR ,1) = 1
      UINDEX (START,2) = 1
      UINDEX (STOP ,2) = 3
      UINDEX (INCR ,2) = 1
      UINDEX (START,3) = 1
      UINDEX (STOP ,3) = 5
      UINDEX (INCR ,3) = 1
C                 ..
C                 .. Retrieve data
      ERROR = GETELT (FDS, 'GrpNaam', '*'
     +               ,UINDEX, USRORD, 7*3*5*4, ARRAY)
      IF (ERROR.NE.0) goto 9999
C                 ..
C                 .. Output data to screen
      write(*,'('' Same values now retrieved in ARRAY(7,3,5)'')')
      DO 20 PLANE = 1,5
         DO 10 COL = 1,3
c           WRITE (*,'(  7F10.2)')
            WRITE (*,'(  7I10  )')
     +          (ARRAY(ROW,COL,PLANE),ROW=1,7)
   10    CONTINUE
         WRITE (*,*)
   20 CONTINUE
c
 9999 continue
      ERROR = NEFERR( 1, ERRSTR)
      END
C================================================================================
      SUBROUTINE FILTER (FDS)
C
      CHARACTER*1024 ERRSTR
C
      INTEGER   FDS
C
      INTEGER   START, STOP, INCR
      PARAMETER (START=1, STOP=2, INCR=3)
C
      CHARACTER SPACE*7
      INTEGER   COL
     +         ,ERROR
     +         ,PLANE
     +         ,ROW
      INTEGER   UINDEX(3,5)
     +         ,USRORD(3)
      INTEGER   ARRAY (4,2,3)
C
      INTEGER   GETELT
     +         ,NEFERR
      EXTERNAL  GETELT
     +         ,NEFERR
C     ..
C     .. Executable statements
C
      SPACE = '       '
C                 ..
C                 .. Change view to (*,3,5)
      USRORD (1) = 3
      USRORD (2) = 1
      USRORD (3) = 2
C                 ..
C                 .. Define indices and step for each dimension
C                 .. The stepsize of 2 creates a filter
      UINDEX (START,1) = 1
      UINDEX (STOP ,1) = 7
      UINDEX (INCR ,1) = 2
      UINDEX (START,2) = 1
      UINDEX (STOP ,2) = 3
      UINDEX (INCR ,2) = 2
      UINDEX (START,3) = 1
      UINDEX (STOP ,3) = 5
      UINDEX (INCR ,3) = 2
C                 ..
C                 .. Retrieve data
      ERROR = GETELT (FDS, 'GrpNaam', '*'
     +               ,UINDEX, USRORD, 4*2*3*4, ARRAY)
      IF (ERROR.NE.0) goto 9999
C                 ..
C                 .. Output data to screen
      write(*,'('' Every other value retrieved in ARRAY(4,2,3)'')')
      DO 20 PLANE = 1,3
         DO 10 COL = 1,2
c           WRITE (*,'(  4F10.2)')
            WRITE (*,'(  4I10  )')
     +          (ARRAY(ROW,COL,PLANE),ROW=1,4)
   10    CONTINUE
         WRITE (*,*)
   20 CONTINUE
C
 9999 continue
      ERROR = NEFERR( 0, ERRSTR)
      write(*,'(a)') trim(errstr)
      END
C
