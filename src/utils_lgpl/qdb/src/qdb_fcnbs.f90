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
subroutine qdb_fcnbs(simfil, dbfil, sizefil, store, q, lundia, &
                   & overwr, freematch, filetype, fout, big,  &
                   & grpnam, simidx, tgtidx, mlim, nlim)
   !
   !=============================================================
   ! DEFINE VARIABLES
   !-------------------------------------------------------------
   !
   implicit none
   !
   ! External INTEGER functions
   !
   integer, external :: OPEN_DATDEF
   integer, external :: CLSNEF
   !
   ! CHARACTER variables
   !
   character*(*) :: filetype
   character*(*) :: grpnam
   character*(*) :: simfil
   character*(*) :: dbfil
   character*(*) :: sizefil
   !
   ! INTEGER variables
   !
   integer :: idx
   integer :: nidx
   integer :: idx1
   integer :: idx2
   integer :: simidx
   integer :: tgtidx
   integer :: lundia
   integer :: ierr
   integer :: fds1
   integer :: fds2
   integer :: fds3
   integer :: mlim(2)
   integer :: nlim(2)
   !
   ! LOGICAL variables
   !
   logical :: fout
   logical :: freematch
   logical :: store
   logical :: overwr
   logical :: big
   logical :: smallexists
   !
   ! REAL variables
   !
   real :: q
   real :: qfound
   !
   !-------------------------------------------------------------
   ! end of DEFINE VARIABLES
   !=============================================================
   !
   ! beginning of actual commands
   !
   qfound = -1.0
   if (grpnam /= ' ') then
      !
      !===========================================================
      ! COPY GROUP FROM FILE 1 TO FILE 2
      !-----------------------------------------------------------
      !
      ierr = open_datdef(simfil, fds1)
      if (ierr /= 0) goto 9999
      ierr = open_datdef(dbfil, fds2)
      if (ierr /= 0) goto 9999
      ierr = open_datdef(sizefil, fds3)
      if (ierr /= 0) goto 9999

      if (big) then
         call copygroupbs(fds1, fds2, fds3, grpnam, simidx, tgtidx, 1, fout, mlim, nlim)
         if (fout) goto 9999
      else
         call copygroupsb(fds1, fds2, fds3, grpnam, simidx, tgtidx, 1, fout, mlim, nlim)
         if (fout) goto 9999
      end if
      !
      ierr = clsnef(fds1)
      ierr = clsnef(fds2)
      ierr = clsnef(fds3)
      !
      !-----------------------------------------------------------
      ! end of COPY GROUP FROM FILE 1 TO FILE 2
      !===========================================================
      !
   else
      if (store) then
         !
         !===========================================================
         ! STORE IN DATABASE
         !-----------------------------------------------------------
         !
         ! find discharge in database
         !
         call finddb(dbfil, lundia, fout, &
                       & q, qfound, idx, &
                       & nidx)
         if (q /= qfound) then
            !
            ! if discharge not yet included
            ! write DISCHARGE to database
            !
            idx = nidx + 1
            qfound = q
            call wrq(dbfil, lundia, fout, &
                        & idx, q)
            if (fout) goto 9999
         end if
         !
         ! do not overwrite data in the database in case of the
         ! STORE1 command (overwr logical set to .false.)
         !
         if (q == qfound .and. .not. overwr) then
            write (lundia, *) 'Database contains already data for this'
            write (lundia, *) 'discharge. Will not overwrite the data!'
            goto 9999
         end if
         !
         if (filetype == 'COM') then
            !
            ! communication file: copy first=last field
            !
            if (simidx == -999999) simidx = 1
         elseif (filetype == 'TRIM') then
            !
            ! flow map file: copy last field
            !
            if (simidx == -999999) simidx = -1
         end if
         idx1 = simidx
         ierr = open_datdef(simfil, fds1)
         if (ierr /= 0) goto 9999
         idx2 = idx
         ierr = open_datdef(dbfil, fds2)
         if (ierr /= 0) goto 9999
         ierr = open_datdef(sizefil, fds3)
         if (ierr /= 0) goto 9999
         !
         !-----------------------------------------------------------
         ! end of STORE IN DATABASE
         !===========================================================
         !
      else
         !
         !===========================================================
         ! RETRIEVE FROM DATABASE
         !-----------------------------------------------------------
         !
         ! find discharge in database
         !
         call finddb(dbfil, lundia, fout, &
                       & q, qfound, idx, &
                       & nidx)
         if (fout) goto 9999
         if (q /= qfound) then
            write (lundia, *) 'Discharge ', q, ' not found in database'
            if (idx == 0 .or. .not. freematch) then
               fout = .true.
               goto 9999
            end if
            write (lundia, *) 'Providing ', qfound, ' instead'
         end if
         !
         if (filetype == 'COM') then
            !
            ! communication file: overwrite first=last field
            !
            if (simidx == -999999) simidx = 1
         elseif (filetype == 'TRIM') then
            !
            ! flow map file: append a new field
            !
            if (simidx == -999999) simidx = 0
         end if
         idx1 = idx
         ierr = open_datdef(dbfil, fds1)
         if (ierr /= 0) goto 9999
         idx2 = simidx
         ierr = open_datdef(simfil, fds2)
         if (ierr /= 0) goto 9999
         ierr = open_datdef(sizefil, fds3)
         if (ierr /= 0) goto 9999
         !
         !-----------------------------------------------------------
         ! end of RETRIEVE FROM DATABASE
         !===========================================================
         !
      end if
      !
      ! Now copy the data ...
      !
      if (filetype == 'COM') then
         !
         ! copy relevant groups if not present
         !
         call autocreate(fds1, fds2, 'com-version', fout)
         if (fout) goto 9999
         call autocreate(fds1, fds2, 'PARAMS', fout)
         if (fout) goto 9999
         call autocreate(fds1, fds2, 'GRID', fout)
         if (fout) goto 9999
         call autocreate(fds1, fds2, 'KENMCNST', fout)
         if (fout) goto 9999
         call autocreate(fds1, fds2, 'TEMPOUT', fout)
         if (fout) goto 9999
         !
         ! copy data of CURTIM and KENMTIM groups
         !
         call copygroup(fds1, fds2, 'CURTIM', idx1, idx2, fout)
         if (fout) goto 9999
         call copygroup(fds1, fds2, 'KENMTIM', idx1, idx2, fout)
         if (fout) goto 9999
      elseif (filetype == 'TRIM') then
         !
         ! copy relevant groups if not present
         !
!        call autocreate(fds1,fds2,'map-version',fout)
!        if (fout) goto 9999
!        call autocreate(fds1,fds2,'map-const',fout)
!        if (fout) goto 9999
!        call autocreate(fds1,fds2,'TEMPOUT',fout)
!        if (fout) goto 9999
         !
         ! copy data of map-series and map-info-series group
         !
!        call copygroup(fds1,fds2,'map-series',idx1,idx2,fout)
!        if (fout) goto 9999
!        call copygroup(fds1,fds2,'map-info-series',idx1,idx2,fout)
!        if (fout) goto 9999
         if (big) then
            call autocreatebs(fds1, fds2, fds3, 'map-const', fout, mlim, nlim)
            if (fout) goto 9999
            call autocreatebs(fds1, fds2, fds3, 'map-version', fout, mlim, nlim)
            if (fout) goto 9999
            call autocreatebs(fds1, fds2, fds3, 'TEMPOUT', fout, mlim, nlim)
            if (fout) goto 9999
            call copygroupbs(fds1, fds2, fds3, 'map-series', idx1, idx2, 1, fout, mlim, nlim)
            if (fout) goto 9999
            call copygroupbs(fds1, fds2, fds3, 'map-info-series', idx1, idx2, 1, fout, mlim, nlim)
            if (fout) goto 9999
         else
            call autocreatesb(fds1, fds2, fds3, 'map-const', fout, mlim, nlim)
            if (fout) goto 9999
            call autocreatesb(fds1, fds2, fds3, 'map-version', fout, mlim, nlim)
            if (fout) goto 9999
            call autocreatesb(fds1, fds2, fds3, 'TEMPOUT', fout, mlim, nlim)
            if (fout) goto 9999
            call copygroupsb(fds1, fds2, fds3, 'map-series', idx1, idx2, 1, fout, mlim, nlim)
            if (fout) goto 9999
            call copygroupsb(fds1, fds2, fds3, 'map-info-series', idx1, idx2, 1, fout, mlim, nlim)
            if (fout) goto 9999
         end if
      else
         write (lundia, '(A,A,A)') 'ERROR: File type "', trim(filetype), '" not yet implemented.'
         goto 9999
      end if
      !
      ierr = clsnef(fds1)
      ierr = clsnef(fds2)
      ierr = clsnef(fds3)
      !
      if (q /= qfound) then
         fout = .true.
         goto 9999
      end if
   end if
   write (lundia, *) 'Operations completed successfully'
   !
   ! ERROR HANDLING
   !
9999 continue
end subroutine qdb_fcnbs
