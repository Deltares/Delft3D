subroutine dimgrd(lun, lunadm, mmax , nmax,name)
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
! wl|Deltares         marine and coastal management
!
! subroutine         : dimgrd
! version            : v1.0
! date               : October 2000
! programmer         : Antoon Koster
!
! function           : Get dimensions for model grid
! limitations        :
! subroutines called : -
!***********************************************************************

   integer      lun, lunadm, mmax, nmax
   character*15 name

   integer       irec, leng
   logical       kw_keyword, kw_found
   character*132 rec

!     reading grid dimensions on grid file

   irec = 0
!
10 kw_keyword = .false.
   irec = irec + 1
   read(lun,'(a)', iostat = ios) rec
   if (ios .ne.0) go to 900
   if (rec(1:1) == '*') goto 10

   L = INDEX(REC,'Coordinate System')
   if (L /= 0) then
      kw_found = .true.
   endif

   l = index(rec,'Missing Value')
   if (l .ne. 0) then
      kw_found = .true.
   endif

   if (kw_found) then
      kw_found = .false.
      goto 10
   endif
!
   read  (rec,*, iostat = ios) mmax, nmax
   if (ios .ne.0) go to 900
   rewind lun
   return
!
900 write(lunadm,'(//a     )') 'Fatal error detected'
   write(lunadm,'(a,i4,a)')&
   &'Error detected while reading record ',irec,&
   &' on grid file '//name
   write(lunadm,'(  a//   )') 'Delft3D-NESTHD1 Aborted'
   stop ' Abnormal end - Check administration-file'
end

