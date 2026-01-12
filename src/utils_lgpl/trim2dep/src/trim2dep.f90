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
program trim2dep
   !
   use trim2dep_version_module, only: getfullversionstring_trim2dep, getbranch_trim2dep
   !
   !=============================================================
   ! DEFINE VARIABLES
   !-------------------------------------------------------------
   !
   implicit none
   !
   ! CHARACTER variables
   !
   character*16 :: grpnam
   character*16 :: elmnam
   character*16 :: filetype ! filetype currently COM or TRIM
   character*64 :: simfil ! name of simulation file com- or trim-
   character*64 :: depfil ! name of simulation file com- or trim-
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
   real nanval
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
   filinp = 'trim2dep.log'
   !
   ! open diagnostics file
   !
   open (newunit=lundia, file=filinp, form='formatted', status='unknown')
   !
   !-------------------------------------------------------------
   ! end of OPEN DIAGNOSTIC OUTPUT FILE
   !=============================================================
   !
   call getfullversionstring_trim2dep(msgbuf)
   write (lundia, '(a)') trim(msgbuf)
   call getbranch_trim2dep(msgbuf)
   write (lundia, '(a)') 'Source: '//trim(msgbuf)
   !=============================================================
   ! READ INPUT FILE
   !-------------------------------------------------------------
   !
   filinp = 'trim2dep.cmd'
   !
   ! open the command file
   !
   errmsg = 'Error opening trim2dep.cmd'
   open (newunit=iuni, file=filinp, form='formatted', status='old', err=9998)

   write (lundia, *) trim(grpnam), trim(elmnam)
   !
   ! read from file: simfil
   ! simfil contains the name of the simulation file to be used
   ! this can be either a COMMUNICATION FILE (hence the name)
   ! or a TRISULA MAP FILE.
   !
   errmsg = 'Error reading simfil from trim2dep.cmd'
   read (iuni, '(A)', iostat=ios) simfil
   if (ios /= 0) goto 9998
   write (lundia, *) 'SIMFIL=', simfil
   !
   ! read from file: depfil
   ! depfil contains the name of the depth file for output
   !
   errmsg = 'Error reading .dep file from trim2dep.cmd'
   read (iuni, '(A)', iostat=ios) depfil
   if (ios /= 0) goto 9998
   write (lundia, *) 'DEPFIL=', depfil
   !
   ! read group name: such 'map-series'
   !
   errmsg = 'Error reading group name from trim2dep.cmd'
   read (iuni, '(A)', iostat=ios) grpnam
   if (ios /= 0) goto 9998
   write (lundia, *) 'GROUP=', grpnam
   !
   ! read element name: such 'DPS'
   !
   errmsg = 'Error reading element name from trim2dep.cmd'
   read (iuni, '(A)', iostat=ios) elmnam
   if (ios /= 0) goto 9998
   write (lundia, *) 'ELEMENT=', elmnam
   if (elmnam(1:3) == 'DPS') then
      write (lundia, *) 'WARNING: When using DPS.. remember to set Dpsopt= #DP#, Dpuopt= #MIN# in mdf-file'
   end if
   !
   ! read user index
   !
   simidx = -999999
   !
   ! read from file: source and target indices
   !
   errmsg = 'Error reading source index number from trim2dep.cmd'
   read (iuni, *, iostat=ios) simidx
   if (ios /= 0) goto 9998
   write (lundia, *) 'SOURCE INDEX=', simidx
   !
   ! read inactive value -999 or 999;
   !
   errmsg = 'Error reading source index number from trim2dep.cmd'
   read (iuni, *, iostat=ios) nanval
   if (ios /= 0) goto 9998
   write (lundia, *) 'INACTIVE VALUE=', nanval
   !
   ! close the command file
   !
   close (iuni)
   !write(*,*) 'Close unit', iuni
   write (lundia, *)
   !
   !-------------------------------------------------------------
   ! end of READ INPUT FILE
   !=============================================================
   !
   call getfiletype(simfil, lundia, fout, filetype)
   if (fout) goto 9999
   !
   if (filetype == 'UNKNOWN' .and. grpnam /= ' ') then
      errmsg = 'Unable to determine type of NEFIS file'
      goto 9998
   end if
   !
   !=============================================================
   ! CALL SUBFUNCTION FOR THE REAL WORK
   !-------------------------------------------------------------
   !
   call trim2dep_fcn(simfil, depfil, lundia, filetype, fout, &
       & grpnam, elmnam, simidx, nanval)
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
   write (lundia, '(1X,A)')
   write (lundia, '(1X,A)') '---Input File should look like: (without #...)-------------------------'
   write (lundia, '(1X,A)') 'trimfile/trim-br1    # TRIM or COM file'
   write (lundia, '(1X,A)') 'refplane.dep         # depth-file'
   write (lundia, '(1X,A)') 'map-const            # group name (see Delft3D\VSI.exe)'
   write (lundia, '(1X,A)') 'DPS0                 # element name'
   write (lundia, '(1X,A)') '1                    # Which record from trim/com file'
   write (lundia, '(1X,A)') '-999                 # Inactive cell value (mapconst:KCS) (Only for Reals)'
   write (lundia, '(1X,A)') '----------------------------------------------------------------------------'
9999 continue
   !
   ! close diagnostic file
   !
   write (lundia, *)
   write (lundia, *) 'END'
   close (lundia)
   !write(*,*) 'Close unit', lundia
   !
end program trim2dep
