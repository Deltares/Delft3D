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
      program test9
      INTEGER*4 fds,datfds
      INTEGER START, STOP, INCR
      PARAMETER (START=1, STOP=2, INCR=3)
      INTEGER Opndef,
     *        Defelm,
     *        Defgrp,
     *        Opndat,
     *        Credat,
     *        getnfv,
     *        Putelt,
     *        Defcel,
     *        Clsdat,
     *        Clsdef
      INTEGER Getelt
      INTEGER Neferr
      INTEGER error,
     *        idum ,
     *        i    ,
     *        UINDEX(3,1)
      REAL    cpu1,
     *        cpu2
      COMPLEX*16 val
      CHARACTER coding*1
      CHARACTER ERRSTR*1024
      CHARACTER*255  version

      error = getnfv(version)
      write(*,*)
      write(*,*) trim(version(5:))
      write(*,*)

      write(*,'(''Maak file met Complexe getallen'')')

      coding = 'N'
      call clock(cpu1)
c
      error= Opndef( fds, 'data_c09.def', coding)
      IF (error.NE. 0) goto 9999

      error= Defelm( fds, 'ELEM_R_4', 'COMPLEX', 16,
     &              'GROOTHEID 1', 'eenheid 1','Beschrijving 1',
     &               0, idum)
      IF (error.NE. 0) goto 9999

      error= Defcel( fds, 'CEL_TEST_1', 1, 'ELEM_R_4')
      IF (error.NE. 0) goto 9999

      error= Defgrp( fds, 'GRP_TEST_1', 'CEL_TEST_1', 1, 1000, 1)
      IF (error.NE. 0) goto 9999

      error= Opndat( datfds, 'data_c09.dat', coding)
      IF (error.NE. 0) goto 9999

      error= Credat( fds, 'DATAGRP_TEST_1A', 'GRP_TEST_1')
      IF (error.NE. 0) goto 9999

      error= Credat( fds, 'DATAGRP_TEST_1B', 'GRP_TEST_1')
      IF (error.NE. 0) goto 9999
c
      call clock(cpu2)
      write(*,'(''Initialisation NEFIS files [sec]'',1PE13.5)')
     *        cpu2-cpu1

      write(*,'(''Schrijf elementen'')')
      call clock(cpu1)

      UINDEX (incr ,1) = 1
      DO 10 i= 1, 1000
        UINDEX (start,1) = i
        UINDEX (stop ,1) = i
        val = ( 10.0, 15.0 )
        error= Putelt( fds, 'DATAGRP_TEST_1A', 'ELEM_R_4',
     +                 UINDEX, 1, val )
        IF (error.NE. 0) goto 9999
   10 CONTINUE
      call clock(cpu2)
      WRITE(*,'(''DATAGRP_TEST_1A written in [sec]'',1PE13.5)')
     *        cpu2-cpu1
c
      call clock(cpu1)
      DO 20 i= 1000, 1, -1
        UINDEX (start,1) = i
        UINDEX (stop ,1) = i
        val = (1.0, 1.0 )
        error= Putelt( fds, 'DATAGRP_TEST_1B', 'ELEM_R_4',
     +                 UINDEX, 1,val )
        IF (error.NE. 0) goto 9999
   20 CONTINUE
c   
      call clock(cpu2)
      WRITE(*,'(''DATAGRP_TEST_1B written in [sec]'',1PE13.5)')
     *        cpu2-cpu1
c
      write(*,'(''Lees elementen'')')
c
      call clock(cpu2)
      DO 30 i= 1000, 1, -1
        UINDEX (start,1) = i
        UINDEX (stop ,1) = i
        error= Getelt( fds, 'DATAGRP_TEST_1A', 'ELEM_R_4',
     +                 UINDEX, 1, 16, val)
        IF (error.NE. 0) goto 9999
   30 CONTINUE
c
      call clock(cpu2)
      WRITE(*,'(''DATAGRP_TEST_1A read    in [sec]'',1PE13.5)')
     *        cpu2-cpu1
c
      call clock(cpu1)
      DO 40 i= 1, 1000
        UINDEX (start,1) = i
        UINDEX (stop ,1) = i
        error= Getelt( fds, 'DATAGRP_TEST_1B', 'ELEM_R_4',
     +                 UINDEX, 1, 16, val)
        IF (error.NE. 0) goto 9999
   40 CONTINUE
      call clock(cpu2)
      WRITE(*,'(''DATAGRP_TEST_1B read    in [sec]'',1PE13.5)')
     *        cpu2-cpu1
c

 9999 continue

      if ( error .ne. 0 ) error = neferr( 1, errstr)

      error = Clsdat( fds)
      error = Clsdef( fds)
      error = neferr( 0, errstr)
      write(*,'(a)') trim(errstr)

      END
c====================================================================
c     Convert clock time to seconds
c
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
