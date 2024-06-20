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
program test4
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
!
   cpu1   = 0.0
   cpu2   = 0.0
   idum   = 0
   coding = 'N'
   imax   = 1000
   start  = 1
!
   call clock(cpu1)
   error = getnfv(version)
   write(*,*)
   write(*,*) trim(version(5:))
   write(*,*)

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
   call clock(cpu2)
   WRITE(*,'(''Initialisation NEFIS files [sec]'',1PE13.5)')&
   &cpu2-cpu1
!
   write(*,*)
   write(*,'(''Schrijf elementen'')')
   write(*,*)
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
   WRITE(*,'(''DATAGRP_TEST_1A written in [sec]'',1PE13.5)')&
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
   WRITE(*,'(''DATAGRP_TEST_1B written in [sec]'',1PE13.5)')&
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
   WRITE(*,'(''DATAGRP_TEST_1C written in [sec]'',1PE13.5)')&
   &cpu2-cpu1

!
!=====================================================================
   write(*,*)
   write(*,'(''Lees elementen'')')
   write(*,*)
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
   WRITE(*,'(''DATAGRP_TEST_1A read    in [sec]'',1PE13.5)')&
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
   WRITE(*,'(''DATAGRP_TEST_1B read    in [sec]'',1PE13.5)')&
   &cpu2-cpu1
   write(*,*)
!
   error= Clsdat( fds)
   error= Clsdef( fds)
!
9999 continue
!
   error = Neferr( 0, errstr)
   write(*,'(a)') trim(errstr)
!
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
