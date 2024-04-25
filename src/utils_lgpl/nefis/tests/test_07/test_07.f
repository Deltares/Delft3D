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
      program test7
      INTEGER*4 fds
      INTEGER clsdat,
     *        clsdef,
     *        getnfv,
     *        getiat,
     *        getrat
      INTEGER getsat,
     *        opndat,
     *        opndef,
     *        putiat,
     *        putrat,
     *        putsat,
     *        neferr
      INTEGER error, ival
      CHARACTER attrib*16, attval*16, coding*1
      REAL    rval
      CHARACTER ERRSTR*1024
      CHARACTER*255  version

      error = getnfv(version)
      write(*,*)
      write(*,*) trim(version(5:))
      write(*,*)

      coding = ' '
      error= Opndef( fds, 'nefis_ex.def', coding)
      IF (error .NE. 0) goto 9999

      error= Opndat( fds, 'nefis_ex.dat', coding)
      IF (error .NE. 0) goto 9999

      error= Putiat( fds, 'DATAGRP_TEST_3A',
     *               'INTEGER ATTRIB 1', 101)
      IF (error .NE. 0) goto 9999

      error= Putiat( fds, 'DATAGRP_TEST_3A',
     *               'INTEGER ATTRIB 2', 102)
      IF (error .NE. 0) goto 9999

      error= Putiat( fds, 'DATAGRP_TEST_3A',
     *               'INTEGER ATTRIB 3', 103)
      IF (error .NE. 0) goto 9999

      error= Putiat( fds, 'DATAGRP_TEST_3A',
     *               'INTEGER ATTRIB 4', 104)
      IF (error .NE. 0) goto 9999

      error= Putiat( fds, 'DATAGRP_TEST_3A',
     *               'INTEGER ATTRIB 5', 105)
      IF (error .NE. 0) goto 9999

      error= Putrat( fds, 'DATAGRP_TEST_3B',
     *               'REAL ATTRIBUUT 1', 201.)
      IF (error .NE. 0) goto 9999

      error= Putrat( fds, 'DATAGRP_TEST_3B',
     *               'REAL ATTRIBUUT 2', 202.)
      IF (error .NE. 0) goto 9999

      error= Putrat( fds, 'DATAGRP_TEST_3B',
     *               'REAL ATTRIBUUT 3', 203.)
      IF (error .NE. 0) goto 9999

      error= Putrat( fds, 'DATAGRP_TEST_3B',
     *               'REAL ATTRIBUUT 4', 204.)
      IF (error .NE. 0) goto 9999

      error= Putrat( fds, 'DATAGRP_TEST_3B',
     *               'REAL ATTRIBUUT 5', 205.)
      IF (error .NE. 0) goto 9999

      error= Putsat( fds, 'DATAGRP_TEST_3C',
     *               'TEXT ATTRIBUUT 1', 'ATR1')
      IF (error .NE. 0) goto 9999

      error= Putsat( fds, 'DATAGRP_TEST_3C',
     *               'TEXT ATTRIBUUT 2', 'ATR2')
      IF (error .NE. 0) goto 9999

      error= Putsat( fds, 'DATAGRP_TEST_3C',
     *               'TEXT ATTRIBUUT 3', 'ATR3')
      IF (error .NE. 0) goto 9999

      error= Putsat( fds, 'DATAGRP_TEST_3C',
     *               'TEXT ATTRIBUUT 4', 'ATR4')
      IF (error .NE. 0) goto 9999

      error= Putsat( fds, 'DATAGRP_TEST_3C',
     *               'TEXT ATTRIBUUT 5', 'ATR5')
      IF (error .NE. 0) goto 9999

      error= Putsat( fds, 'DATAGRP_TEST_3A',
     *               'TEXT ATTRIBUUT 1', 'DATAGRP_TEST_3C')
      IF (error .NE. 0) goto 9999
c
c     Get  text attributes
c
      error= Getsat( fds, 'DATAGRP_TEST_3A',
     *               'TEXT ATTRIBUUT 1', attrib)
      IF (attrib .NE. 'DATAGRP_TEST_3C')
     *  write(*,*) 'Attribute value (=DATA_GRP_TEST_3C): ', attrib
      IF (error .NE. 0) goto 9999

      error= Getsat( fds, attrib,
     *               'TEXT ATTRIBUUT 3', attval)
      IF (attval .NE. 'ATR3')
     *  write(*,*) 'Attribute value (=ATR3): ', attval
      IF (error .NE. 0) goto 9999
c
c     Get  integer attributes
c
      error= Getiat( fds, 'DATAGRP_TEST_3A',
     *               'INTEGER ATTRIB 1', ival)
      IF (ival .NE. 101)
     *  write(*,*) 'Attribute value (=101): ', ival
      IF (error .NE. 0) goto 9999

      error= Getiat( fds, 'DATAGRP_TEST_3A',
     *               'INTEGER ATTRIB 2', ival)
      IF (ival .NE. 102)
     *  write(*,*) 'Attribute value (=102): ', ival
      IF (error .NE. 0) goto 9999
c
c     Put integer attributes
c
      error= Putiat( fds, 'DATAGRP_TEST_3B',
     *               'INTEGER ATTRIB 1', 1000)
      IF (error .NE. 0) goto 9999

      error= Putiat( fds, 'DATAGRP_TEST_3C',
     *               'INTEGER ATTRIB 1', 1001)
      IF (error .NE. 0) goto 9999
c
c     Get integer attributes
c
      error= Getiat( fds, 'DATAGRP_TEST_3B',
     *               'INTEGER ATTRIB 1', ival)
      IF (ival .NE. 1000)
     *  write(*,*) 'Attribute value (=1000): ', ival
      IF (error .NE. 0) goto 9999

      error= Getiat( fds, 'DATAGRP_TEST_3C',
     *               'INTEGER ATTRIB 1', ival)
      IF (ival .NE. 1001)
     *  write(*,*) 'Attribute value (=1001): ', ival
      IF (error .NE. 0) goto 9999
c
c     Get  real attributes
c
      error= Getrat( fds, 'DATAGRP_TEST_3B',
     *               'REAL ATTRIBUUT 1', rval)
      IF (rval .NE. 201.)
     *  write(*,*) 'Attribute value (=201.): ', rval
      IF (error .NE. 0) goto 9999

      error= Getrat( fds, 'DATAGRP_TEST_3B',
     *               'REAL ATTRIBUUT 2', rval)
      IF (rval .NE. 202.)
     *  write(*,*) 'Attribute value (=202.): ', rval
      IF (error .NE. 0) goto 9999

      error= Getrat( fds, 'DATAGRP_TEST_3B',
     *               'REAL ATTRIBUUT 5', rval)
      IF (rval .NE. 205.)
     *  write(*,*) 'Attribute value (=205.): ', rval
      IF (error .NE. 0) goto 9999

      error= Clsdat( fds)
      IF (error .NE. 0) goto 9999

      error= Clsdef( fds)
      IF (error .NE. 0) goto 9999

      goto 8888

 9999 continue
      write(*,*) ' Error detected in program Test7'
 8888 continue

      error = neferr( 0, errstr)
      write(*,*)
      write(*,'(a)') trim(errstr)

      END
