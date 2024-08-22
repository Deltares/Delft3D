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
!
!     Company name                    : Deltares
!                                       P.O.Box 177
!                                       2600 MH Delft
!                                       The Netherlands
!--------------------------------------------------------------------------------
!     System: NEFIS
!
!     $Header: /delft3d/libraries/nefis/demo/demo_02/demo_02.f 2     10/03/06 9:56 Mooiman $
!--------------------------------------------------------------------------------
!     Programmer                      : A. Hoekstra
!     Project                         : NEutral FIle Structure
!--------------------------------------------------------------------------------
!      * * * * * * * * * * * * * DESCRIPTION * * * * * * * * * * * * *
!
!     - This demo-program demonstrates the use of NEFIS store-
!       and retrieval functions. Special is the use of a
!       datagroup with a variable dimension.
!
!       This program performs the following tasks:
!       - create an element, cel and a 3-d group defintion
!       - create a data group
!       - store data in this group
!       - retrieve data from this group, using a
!         different view
!       - retrieve data using a filter
!
!     Note: the error-return code from the NEFIS-functions is
!           not checked
!--------------------------------------------------------------------------------
!     ..
!     .. Scalars
   CHARACTER*1024 ERRSTR
!                 .. character string to catch the NEFIS error message
   CHARACTER CODING*1
!                 .. indicates Y/N neutral representation of data
   INTEGER   ERROR
!                 .. contains return-value of NEFIS-functions
!     ..
!     .. Arrays
   INTEGER   FDS
!                 .. nefis file descriptor
!     ..
!     .. Declarations of NEFIS-functions
   INTEGER   CLSDAT&
   &,CLSDEF&
   &,CREDAT&
   &,FLSDAT&
   &,OPNDAT&
   &,OPNDEF&
   &,NEFERR
!
   EXTERNAL  CLSDAT&
   &,CLSDEF&
   &,CREDAT&
   &,FLSDAT&
   &,OPNDAT&
   &,OPNDEF&
   &,NEFERR
!     ..
!     .. Executable statements
!
!                 ..
!                 .. Open a definition file
   CODING = 'N'
   ERROR = OPNDEF (FDS, 'data_d02.def', CODING)
   IF (ERROR.NE.0) goto 9999
!                 ..
!                 .. Define element, cel, and group-definition
   CALL DEFINE (FDS)
!                 ..
!                 .. Open a data file
   CODING = 'N'
   ERROR = OPNDAT (FDS, 'data_d02.dat', CODING)
   IF (ERROR.NE.0) goto 9999
!                 ..
!                 .. Create space for data
   ERROR = CREDAT (FDS, 'GrpNaam', 'Groep')
   IF (ERROR.NE.0) goto 9999
!
   ERROR = FLSDAT (FDS)
   IF (ERROR.NE.0) goto 9999
!                 ..
!                 .. Write data to file
   CALL PUTDAT (FDS)
!                 ..
!                 .. Retrieve data, using a different view
   CALL DTVIEW (FDS)
!                 ..
!                 .. Retrieve a part of the data
   CALL FILTER (FDS)
!                 ..
!                 .. Close the files
9999 continue

   if (error.eq.0) ERROR = CLSDEF (FDS)
   if (error.eq.0) ERROR = CLSDAT (FDS)

   ERROR = NEFERR( 1, ERRSTR)
!
END
!================================================================================
SUBROUTINE DEFINE (FDS)
!
   INTEGER   FDS
!
   INTEGER       ERROR
   CHARACTER*134 ERRSTR

   INTEGER   GRPDMS(5)&
   &,GRPORD(5)
!
   INTEGER   DEFCEL&
   &,DEFELM&
   &,DEFGRP&
   &,FLSDEF&
   &,NEFERR
   EXTERNAL  DEFCEL&
   &,DEFELM&
   &,DEFGRP&
   &,FLSDEF&
   &,NEFERR
!     ..
!     .. Executable statements
!
!                 ..
!                 .. Define a simple element, type Real*4
   ERROR = DEFELM (FDS, 'ElmName', 'Integer',  4,&
   &'ElmQuantity', 'ElmUnity', 'ElmDescription',&
   &1, 1)
   IF (ERROR.NE.0) goto 9999
!                 ..
!                 .. Define a cel with only one real value
   ERROR = DEFCEL (FDS, 'Cell', 1, 'ElmName')
   IF (ERROR.NE.0) goto 9999
!                 ..
!                 .. Define a 3-d group of dimension (3,5,0),
!                 .. so a group with a variable dimension
   GRPDMS(1) = 3
   GRPDMS(2) = 5
   GRPDMS(3) = 0
   GRPORD(1) = 1
   GRPORD(2) = 3
   GRPORD(3) = 2
   ERROR = DEFGRP (FDS, 'Groep', 'Cell', 3, GRPDMS, GRPORD)
   IF (ERROR.NE.0) goto 9999
!                 ..
!                 .. Flush buffers to file
   ERROR = FLSDEF (FDS)
   IF (ERROR.NE.0) goto 9999
!
9999 continue
   ERROR = NEFERR( 1, ERRSTR)
END
!================================================================================
SUBROUTINE PUTDAT (FDS)
!
   CHARACTER*1024 ERRSTR
!
   INTEGER   FDS
!
   INTEGER   START, STOP, INCR
   PARAMETER (START=1, STOP=2, INCR=3)
   equivalence(AARRAY,ARRAY)
!
   CHARACTER SPACE*7
   INTEGER   COL&
   &,ERROR&
   &,PLANE&
   &,ROW
   INTEGER   UINDEX(3,5)&
   &,USRORD(5)
   INTEGER   ARRAY (3,5,7)
   INTEGER   AARRAY (105)
!
   INTEGER   FLSDAT&
   &,PUTELT&
   &,NEFERR
   EXTERNAL  FLSDAT&
   &,PUTELT&
   &,NEFERR
!     ..
!     .. Executable statements
!
   SPACE = '       '
!                 ..
!                 .. Set view to (3,5,*)
   USRORD (1) = 1
   USRORD (2) = 2
   USRORD (3) = 3
!                 ..
!                 .. Define indices for each dimension
   UINDEX (START,1) = 1
   UINDEX (STOP ,1) = 3
   UINDEX (INCR ,1) = 1
   UINDEX (START,2) = 1
   UINDEX (STOP ,2) = 5
   UINDEX (INCR ,2) = 1
   UINDEX (START,3) = 1
   UINDEX (STOP ,3) = 7
   UINDEX (INCR ,3) = 1
!                 ..
!                 .. Fill array with values
   DO PLANE = 1,7
      DO COL = 1,5
         DO ROW = 1,3
            ARRAY (ROW, COL, PLANE) = ROW*1000+COL*100+PLANE
         END DO
      END DO
   END DO
!                 ..
!                 .. Write data to file
!  ERROR = PUTELT (FDS, 'GrpNaam', 'ElmName'
   ERROR = PUTELT (FDS, 'GrpNaam', '*'&
                  ,UINDEX, USRORD, ARRAY)
   IF (ERROR.NE.0) goto 9999
!                 ..
!                 .. Flush the buffers
   ERROR = FLSDAT (FDS)
   IF (ERROR.NE.0) goto 9999
!                 ..
!                 .. Output data to screen
!     write(*,'('' ARRAY(105) written to file:'')')
!     DO PLANE = 1,105
!       WRITE (*,'(  I10)') AARRAY(PLANE)
!     END DO
   write(*,'('' ARRAY(3,5,7) written to file:'')')
   DO PLANE = 1,7
      DO COL = 1,5
!           WRITE (*,'(  3F10.2)')
         WRITE (*,'(  3I10)')&
         &(ARRAY(ROW,COL,PLANE),ROW=1,3)
      END DO
      WRITE (*,*)
   END DO
!
9999 continue
   ERROR = NEFERR( 1, ERRSTR)
END
!================================================================================
SUBROUTINE DTVIEW (FDS)
!
   CHARACTER*1024 ERRSTR
!
   INTEGER   FDS
!
   INTEGER   START, STOP, INCR
   PARAMETER (START=1, STOP=2, INCR=3)
!
   CHARACTER SPACE*7
   INTEGER   COL&
   &,ERROR&
   &,PLANE&
   &,ROW
   INTEGER   UINDEX(3,5)&
   &,USRORD(3)
   INTEGER   ARRAY (7,3,5)
!
   INTEGER   GETELT&
   &,NEFERR
   EXTERNAL  GETELT&
   &,NEFERR
!     ..
!     .. Executable statements
!
   SPACE = '       '
!                 ..
!                 .. Change view to (*,3,5)
   USRORD (1) = 3
   USRORD (2) = 1
   USRORD (3) = 2
!                 ..
!                 .. Define indices for each dimension
   UINDEX (START,1) = 1
   UINDEX (STOP ,1) = 7
   UINDEX (INCR ,1) = 1
   UINDEX (START,2) = 1
   UINDEX (STOP ,2) = 3
   UINDEX (INCR ,2) = 1
   UINDEX (START,3) = 1
   UINDEX (STOP ,3) = 5
   UINDEX (INCR ,3) = 1
!                 ..
!                 .. Retrieve data
   ERROR = GETELT (FDS, 'GrpNaam', '*'&
   &,UINDEX, USRORD, 7*3*5*4, ARRAY)
   IF (ERROR.NE.0) goto 9999
!                 ..
!                 .. Output data to screen
   write(*,'('' Same values now retrieved in ARRAY(7,3,5)'')')
   DO PLANE = 1,5
      DO COL = 1,3
!           WRITE (*,'(  7F10.2)')
         WRITE (*,'(  7I10  )')&
         &(ARRAY(ROW,COL,PLANE),ROW=1,7)
      END DO
      WRITE (*,*)
   END DO
!
9999 continue
   ERROR = NEFERR( 1, ERRSTR)
END
!================================================================================
SUBROUTINE FILTER (FDS)
!
   CHARACTER*1024 ERRSTR
!
   INTEGER   FDS
!
   INTEGER   START, STOP, INCR
   PARAMETER (START=1, STOP=2, INCR=3)
!
   CHARACTER SPACE*7
   INTEGER   COL&
   &,ERROR&
   &,PLANE&
   &,ROW
   INTEGER   UINDEX(3,5)&
   &,USRORD(3)
   INTEGER   ARRAY (4,2,3)
!
   INTEGER   GETELT&
   &,NEFERR
   EXTERNAL  GETELT&
   &,NEFERR
!     ..
!     .. Executable statements
!
   SPACE = '       '
!                 ..
!                 .. Change view to (*,3,5)
   USRORD (1) = 3
   USRORD (2) = 1
   USRORD (3) = 2
!                 ..
!                 .. Define indices and step for each dimension
!                 .. The stepsize of 2 creates a filter
   UINDEX (START,1) = 1
   UINDEX (STOP ,1) = 7
   UINDEX (INCR ,1) = 2
   UINDEX (START,2) = 1
   UINDEX (STOP ,2) = 3
   UINDEX (INCR ,2) = 2
   UINDEX (START,3) = 1
   UINDEX (STOP ,3) = 5
   UINDEX (INCR ,3) = 2
!                 ..
!                 .. Retrieve data
   ERROR = GETELT (FDS, 'GrpNaam', '*'&
   &,UINDEX, USRORD, 4*2*3*4, ARRAY)
   IF (ERROR.NE.0) goto 9999
!                 ..
!                 .. Output data to screen
   write(*,'('' Every other value retrieved in ARRAY(4,2,3)'')')
   DO PLANE = 1,3
      DO COL = 1,2
!           WRITE (*,'(  4F10.2)')
         WRITE (*,'(  4I10  )')&
         &(ARRAY(ROW,COL,PLANE),ROW=1,4)
      END DO
      WRITE (*,*)
   END DO
!
9999 continue
   ERROR = NEFERR( 0, ERRSTR)
   write(*,'(a)') trim(errstr)
END
!
