subroutine getdim(lundia,lunbnd,fout  ,extnef,notims,nostat,&
&kmax  ,lstci ,nobnd                      )
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
!***********************************************************************
! Deltares                         marine and coastal management
!
!     Function: Gets dimensions necessary for pointersfrom the
!               (NEFIS) history file and boundary definition file
!
!               Theo van der Kaaij
!               September 1997
!
!***********************************************************************
!
   integer*4     fdNef,&
   &uindex(     3),&
   &buflen,&
   &usrord,&
   &grpdms(     5), grpndm,&
   &grpord(     5)
   integer*4     error
   integer*4     getelt, crenef, clsnef, inqgrp, inqmxi
!
   character*1   coding, access
   character*(*) extnef
   character*256 fildat, fildef
   character*16  elmnam, celnam, grpnam
!
   logical       fout
!
!              ****************************************
!              *     read dimensions from NEFIS files *
!              ****************************************
!
!     +-----------------------------------------------------------------
!     | Initialize
!     +-----------------------------------------------------------------
   coding     = 'N'
   access     = 'R'
   uindex (1) = 1
   uindex (2) = 1
   uindex (3) = 1
   usrord     = 1
   fout       = .false.
!     +-----------------------------------------------------------------
!     | Ophalen dimensies datagroep 'his-info-series'
!     +-----------------------------------------------------------------
   fildef = 'trih-' // trim(extnef) // '.def'
!
   fildat = 'trih-' // trim(extnef) // '.dat'
   error  = CRENEF (fdNef, fildat, fildef, coding, access)
   if (error  .ne. 0) then
      write(lundia,'(a)')&
      &'***ERROR: opening Delft3D-FLOW his file(s)'
      fout   = .true.
      goto 999
   endif
!
   celnam = 'his-info-series'
   grpnam = 'his-info-series'
   grpndm = 5
   error  = INQGRP (fdNef, grpnam, celnam, grpndm, grpdms, grpord)
   if (error  .ne. 0) then
      write(lundia,'(a)')&
      &'***ERROR: reading Delft3D-FLOW his definition file'
      fout   = .true.
      goto 999
   endif
!
   notims     = grpdms (1)
!     notims  = 768

!-----------------------------------------------------------------------
!-----Test value of notims if notims = 0 then get notims with INQMXI
!-----------------------------------------------------------------------
   if (notims .eq. 0) then
      error    = INQMXI(fdNef    ,grpnam    ,notims    )
      if (error  .ne. 0) then
         write(lundia,'(a)')&
         &'***ERROR: reading Delft3D-FLOW his definition file'
         fout   = .true.
         goto 999
      endif
      if (notims  .eq. 0) then
         write(lundia,'(a,a)')&
         &'***ERROR: No data available on Delft3D-FLOW',&
         &' his data file'
         fout   = .true.
         goto 999
      endif
   endif
!----------------------------------------------------------------------
!     get nostat from nefis files
!----------------------------------------------------------------------
   grpnam     = 'his-const'
   elmnam    = 'NOSTAT'
   buflen    = 4
   uindex(2) = 1
   error     = GETELT(fdNef,grpnam,elmnam,uindex,&
   &usrord,buflen,nostat              )
   if (error  .ne. 0) then
      write(lundia,'(a)')&
      &'***ERROR: reading Delft3D-FLOW his data file (NOSTAT)'
      fout   = .true.
      goto 999
   endif
!
   elmnam    = 'KMAX'
   buflen    = 4
   uindex(2) = 1
   error     = GETELT(fdNef,grpnam,elmnam,uindex,&
   &usrord,buflen,kmax                )
   if (error  .ne. 0) then
      write(lundia,'(a)')&
      &'***ERROR: reading Delft3D-FLOW his data file (KMAX)'
      fout   = .true.
      goto 999
   endif

   elmnam     = 'LSTCI'
   buflen    = 4
   uindex(2) = 1
   error     = GETELT(fdNef    ,grpnam    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,LSTCI     )
   if (error  .ne. 0) then
      write(lundia,'(a)')&
      &'***ERROR: reading Delft3D-FLOW his data file (LSTCI)'
      fout   = .true.
      goto 999
   endif
!
!-----determine number of time series boundaries from bnddef file
!
   call dimbnd (lunbnd,nobnd )
!
!-----close NEFIS files
!
999 continue

   error     = CLSNEF(fdNef)
!
!----
!
   return

end
