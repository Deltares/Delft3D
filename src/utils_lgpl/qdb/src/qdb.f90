!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2026.
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
program qdb
   !
   use qdb_version_module, only: getfullversionstring_qdb, getbranch_qdb
   use string_module, only: str_toupper
   !=============================================================
   ! DEFINE VARIABLES
   !-------------------------------------------------------------
   !
   implicit none
   !
   ! CHARACTER variables
   !
   character*16 :: grpnam
   character*16 :: filetype ! filetype currently COM or TRIM
   character*64 :: simfil ! name of simulation file com- or trim-
   character*64 :: filinp
   character*64 :: command
   character*64 :: dbfil ! name of database file
   character*64 :: sizefil ! file with the intended size.
   character*64 :: errmsg
   character*80 :: msgbuf
   !
   ! INTEGER variables
   !
   integer :: ios
   integer :: lundia
   integer :: iuni
   integer :: simidx
   integer :: tgtidx
   integer :: mlim(2)
   integer :: nlim(2)
   !
   ! LOGICAL variables
   !
   logical :: fout
   logical :: freematch
   logical :: store
   logical :: dblist
   logical :: overwr
   logical :: sizeexists
   logical :: big
   !
   ! REAL variables
   !
   real q
   !
   !-------------------------------------------------------------
   ! end of DEFINE VARIABLES
   !=============================================================
   !
   !=============================================================
   ! OPEN DIAGNOSTIC OUTPUT FILE
   !-------------------------------------------------------------
   !
   fout = .false.
   filinp = 'qdb.log'
   !
   ! open diagnostics file
   !
   open (newunit=lundia, file=filinp, form='formatted', status='unknown')
   !
   !-------------------------------------------------------------
   ! end of OPEN DIAGNOSTIC OUTPUT FILE
   !=============================================================
   !
   call getfullversionstring_qdb(msgbuf)
   write (lundia, '(a)') trim(msgbuf)
   call getbranch_qdb(msgbuf)
   write (lundia, '(a)') 'Source: '//trim(msgbuf)
   !=============================================================
   ! READ INPUT FILE
   !-------------------------------------------------------------
   !
   filinp = 'qdb.cmd'
   !
   ! open the command file
   !
   errmsg = 'Error opening qdb.cmd'
   open (newunit=iuni, file=filinp, form='formatted', status='old', err=9998)
   !
   ! read from file: command
   !
   store = .true.
   dblist = .true.
   overwr = .true.
   freematch = .false.
   sizeexists = .true.
   big = .false.
   grpnam = ' '
   !
   errmsg = 'Error reading command from qdb.cmd'
   read (iuni, '(A)', iostat=ios) command
   if (ios /= 0) goto 9998
   command = str_toupper(command)
   !
   ! parse command
   !
   if (index(command, 'RETRIEVE') /= 0) then
      !
      ! get data from database
      !
      store = .false.
      dblist = .false.
   elseif (index(command, 'STORE1') /= 0) then
      !
      ! 'STORE1' should be checked before 'STORE' (substring!!)
      !
      ! store data in database when not yet stored
      !
      store = .true.
      dblist = .false.
      overwr = .false.
   elseif (index(command, 'STORE') /= 0) then
      !
      ! store data in database (overwrite any existing data)
      !
      store = .true.
      dblist = .false.
      overwr = .true.
   elseif (index(command, 'COPY') /= 0) then
      !
      ! copy groups between files (overwrite any existing data)
      !
      store = .true.
      dblist = .false.
      overwr = .true.
      errmsg = 'Error reading copy group name from qdb.cmd'
      read (iuni, *, iostat=ios) grpnam
      if (ios /= 0) goto 9998
   elseif (index(command, 'LIST') /= 0) then
      !
      ! list database contents
      !
   else
      !
      ! unknown command: print error!
      !
      errmsg = 'Invalid command in qdb.cmd. RETRIEVE,'// &
            & 'STORE1, STORE, COPY or LIST expected'
      goto 9998
   end if
   !
   if (index(command, 'DON''T OVERWRITE') /= 0) then
      !
      ! do not overwrite any existing data in database
      !
      overwr = .false.
   elseif (index(command, 'OVERWRITE') /= 0) then
      !
      ! overwrite any existing data in database
      !
      overwr = .true.
   end if
   !
   if (index(command, 'FREE MATCH') /= 0) then
      !
      ! free match: search for nearest discharge if not
      ! exact match
      !
      freematch = .true.
   end if
   !
   write (lundia, *) trim(command), ': ', trim(grpnam)
   if (.not. dblist) then
      write (lundia, *) 'STORE=', store
      if (store) then
         write (lundia, *) 'OVERWRITE=', overwr
      else
         write (lundia, *) 'FREE MATCH=', freematch
      end if
   end if
   !
   ! read user index
   !
   simidx = -999999
   tgtidx = -999999
   if (grpnam /= ' ') then
      !
      ! read from file: source and target indices
      !
      errmsg = 'Error reading source index number from qdb.cmd'
      read (iuni, *, iostat=ios) simidx
      if (ios /= 0) goto 9998
      write (lundia, *) 'SOURCE INDEX=', simidx
      errmsg = 'Error reading target index number from qdb.cmd'
      read (iuni, *, iostat=ios) tgtidx
      if (ios /= 0) goto 9998
      write (lundia, *) 'TARGET INDEX=', tgtidx
   else
      !
      ! read from file: user index
      !
      if (index(command, 'INDEX') /= 0) then
         errmsg = 'Error reading index number from qdb.cmd'
         read (iuni, *, iostat=ios) simidx
         if (ios /= 0) goto 9998
         write (lundia, *) 'INDEX=', simidx
      end if
      !
      ! read from file: discharge
      !
      errmsg = 'Error reading discharge from qdb.cmd'
      read (iuni, *, iostat=ios) q
      if (ios /= 0) goto 9998
      write (lundia, *) 'DISCHARGE=', q
   end if
   !
   ! read from file: simfil
   ! simfil contains the name of the simulation file to be used
   ! this can be either a COMMUNICATION FILE (hence the name)
   ! or a TRISULA MAP FILE.
   !
   errmsg = 'Error reading simfil from qdb.cmd'
   read (iuni, '(A)', iostat=ios) simfil
   if (ios /= 0) goto 9998
   write (lundia, *) 'SIMFIL=', simfil
   !
   ! read from file: dbfilbig
   ! dbfil contains the name of the database file to be used
   !
   errmsg = 'Error reading dbfil from qdb.cmd'
   read (iuni, '(A)', iostat=ios) dbfil
   if (ios /= 0) goto 9998
   write (lundia, *) 'DBFIL=', dbfil
   !
   ! read from file: dbfilsmall
   ! dbfil contains the name of the database file to be used
   !
   errmsg = 'Error reading sizefil from qdb.cmd'
   read (iuni, '(A)', iostat=ios) sizefil
   if (ios /= 0) sizeexists = .false.
   if (sizeexists) then
      write (lundia, *) 'SIZEFIL=', sizefil
      errmsg = 'Error reading simfile size from qdb.cmd'
      read (iuni, '(A)', iostat=ios) command
      if (ios /= 0) goto 9998
      command = str_toupper(command)
      !
      ! parse command
      !
      if (index(command, 'BIG') /= 0) then
         big = .true.
         write (lundia, *) 'SIZEFILE=BIG'
      end if
      if (index(command, 'SMALL') /= 0) then
         big = .false.
         write (lundia, *) 'SIZEFILE=SMALL'
      end if
      read (iuni, *, iostat=ios) mlim
      write (lundia, *) 'MLIM=', mlim
      read (iuni, *, iostat=ios) nlim
      write (lundia, *) 'NLIM=', nlim
   end if
   !
   ! close the command file
   !
   close (iuni)
   write (lundia, *)
   !
   !-------------------------------------------------------------
   ! end of READ INPUT FILE
   !=============================================================
   !
   call getfiletype(simfil, lundia, fout, filetype)
   if (fout) goto 9999
   !
   if (filetype == 'UNKNOWN') then
      call getfiletype(dbfil, lundia, fout, filetype)
      if (fout) goto 9999
      !
      if (grpnam /= ' ' .and. filetype == 'UNKNOWN') then
         errmsg = 'Unable to determine type of NEFIS file'
         goto 9998
      end if
   end if
   !
   !=============================================================
   ! CALL SUBFUNCTION FOR THE REAL WORK
   !-------------------------------------------------------------
   !
   if (dblist) then
      write (lundia, *) 'DATABASE'
      call listdb(dbfil, lundia, fout)
   else
      if (.not. sizeexists) then
         call qdb_fcn(simfil, dbfil, store, q, lundia, &
                    & overwr, freematch, filetype, fout, &
                    & grpnam, simidx, tgtidx)
      else
         call qdb_fcnbs(simfil, dbfil, sizefil, store, q, lundia, &
                  & overwr, freematch, filetype, fout, big, &
                  & grpnam, simidx, tgtidx, mlim, nlim)
      end if
   end if
   !
   !-------------------------------------------------------------
   ! end of CALL SUBFUNCTION FOR THE REAL WORK
   !=============================================================
   !
   ! ERROR HANDLING
   !
   if (fout) goto 9999
   errmsg = 'Normal end'
9998 continue
   write (lundia, '(1X,A)') errmsg
9999 continue
   !
   ! close diagnostic file
   !
   write (lundia, *)
   write (lundia, *) 'END'
   close (lundia)
   !
end program qdb
