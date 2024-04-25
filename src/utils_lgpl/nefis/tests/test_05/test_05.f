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
      PROGRAM test5
      INTEGER start, stop, incr
      PARAMETER (start=1, stop=2, incr=3)
      INTEGER clsdat,
     *        clsdef,
     *        credat,
     *        defcel,
     *        defelm,
     *        defgrp,
     *        flsdat,
     *        flsdef,
     *        getnfv,
     *        getelt
      INTEGER neferr,
     *        opndat,
     *        opndef,
     *        putelt
      INTEGER error,
     *        idum,
     *        i, j,
     *        elmdms(5),
     *        UINDEX(3,1),
     *        fds
      REAL buffer(748)
      CHARACTER names(3)*14, coding*1
      CHARACTER ERRSTR*1024
      CHARACTER*255  version
c
      error = getnfv(version)
      write(*,*)
      write(*,*) trim(version(5:))
      write(*,*)

      coding=' '
      error= Opndef( fds, 'nefis_ex.def', coding)
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
c
      error= Opndat( fds, 'nefis_ex.dat', coding)
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
c
      error= Defelm( fds, 'ELEM_R_4_DIM_1', 'REAL', 4,
     &              'GROOTHEID 2', 'eenheid 2','Beschrijving 2',
     &               1, 3)
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
c
      elmdms(1)= 5
      elmdms(2)= 5
      error= Defelm( fds, 'ELEM_R_4_DIM_2', 'REAL', 4,
     &              'GROOTHEID 3', 'eenheid 3','Beschrijving 3',
     &               2, elmdms)
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
c
      elmdms(1)= 2
      elmdms(2)= 3
      elmdms(3)= 4
      elmdms(4)= 5
      elmdms(5)= 6
      error= Defelm( fds, 'ELEM_R_4_DIM_5', 'REAL', 4,
     &              'GROOTHEID 4', 'eenheid 4','Beschrijving 4',
     &               5, elmdms)
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
c
      names(1)= 'ELEM_R_4_DIM_1'
      names(2)= 'ELEM_R_4_DIM_2'
      names(3)= 'ELEM_R_4_DIM_5'
      error= Defcel( fds, 'CEL_TEST_2', 3, names)
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
c
      error= Defgrp( fds, 'GRP_TEST_2A', 'CEL_TEST_2', 0, idum, idum)
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
c
      error= Defgrp( fds, 'GRP_TEST_2B', 'CEL_TEST_2', 1, 100, 1)
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
c
      error= Credat( fds, 'DATAGRP_TEST_2A', 'GRP_TEST_2A')
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
c
      error= Credat( fds, 'DATAGRP_TEST_2B', 'GRP_TEST_2B')
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
c
      DO 10 i= 1, 748
        buffer(i)= i
   10 CONTINUE
c
      write(*,'(''schrijf DATAGRP_TEST_2A'')')
      UINDEX(start ,1) = 1
      UINDEX(stop  ,1) = 1
      UINDEX(incr  ,1) = 1
      error = Putelt( fds, 'DATAGRP_TEST_2A', '*',
     +                UINDEX, 1, buffer)
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
c
      write(*,'(''schrijf DATAGRP_TEST_2B'')')
      DO 30 i= 1, 100
        UINDEX(start,1) = i
        UINDEX(stop ,1) = i
        UINDEX(incr ,1) = 1
        DO 20 j= 1, 748
          buffer(j)= REAL(i)* REAL(j)
   20   CONTINUE
        error = Putelt( fds, 'DATAGRP_TEST_2B', '*',
     +                  UINDEX, 1, buffer)
        IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
  30  CONTINUE
      error = flsdat ( fds )
      error = flsdef ( fds )
c
      write(*,'(''lees DATAGRP_TEST_2B'')')
      DO 50 i= 100, 1, -1
         UINDEX(start,1) = i
         UINDEX(stop ,1) = i
        error= Getelt( fds, 'DATAGRP_TEST_2B', '*',
     +                UINDEX, 1, 748*4, buffer)
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
        DO 40 j= 1, 748
          IF (INT( buffer(j)/ REAL(i)-j).NE.0)
     *      write(*,'(''error, i='',i3)') i
   40   CONTINUE
   50 CONTINUE
c
      write(*,'(''lees DATAGRP_TEST_2A'')')
      UINDEX(start,1) = 1
      UINDEX(stop ,1) = 1
      error= Getelt( fds, 'DATAGRP_TEST_2A', '*',
     +              UINDEX, 1, 748*4, buffer)
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
      DO 60 j= 1, 748
C      PRINT *, buffer(j),j, INT(buffer(j)-j)
        IF (INT( buffer(j)-j).NE. 0) PRINT *,'error, i= ', i
   60 CONTINUE

      write(*,'(''lees DATAGRP_TEST_1A'')')
      DO 70 i= 1000, 1, -1
        UINDEX(start,1) = i
        UINDEX(stop ,1) = i
        error= Getelt( fds, 'DATAGRP_TEST_1A', '*' ,
     +                UINDEX, 1, 4, buffer(1))
        IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
C     write(*,'(1x', buffer(1), i, int(buffer(1)-i),error
        IF (INT( buffer(1)-i).NE. 0)
     *    write(*,'(''error, i= '',i3)') i
   70 CONTINUE
c
      write(*,'(''lees DATAGRP_TEST_1B'')')
      DO 80 i= 1, 1000
        UINDEX(start,1) = i
        UINDEX(stop ,1) = i
        error= Getelt( fds, 'DATAGRP_TEST_1B', '*',
     +                UINDEX, 1, 4, buffer(1))
        IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
        IF (INT( buffer(1)+1*i).NE. 0)
     *    write(*,'(''error, i= '',i3)') i
   80 CONTINUE
c
      write(*,*)
      error= Clsdat( fds)
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
c
      error= Clsdef( fds)
      IF (ERROR.NE.0) ERROR = NEFERR( 1, ERRSTR)
c
      ERROR = NEFERR( 0, ERRSTR)
      write(*,'(a)') trim(errstr)
c
      END
