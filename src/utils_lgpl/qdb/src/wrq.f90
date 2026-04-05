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
subroutine wrq(dbfil, lundia, fout, idx, q)
   !
   ! DEFINE VARIABLES
   !
   implicit none
   !
   ! External INTEGER functions
   !
   integer, external :: open_datdef
   integer, external :: clsnef
   integer, external :: inqdat
   integer, external :: neferr
   !
   integer, external :: defelm
   integer, external :: defcel
   integer, external :: defgrp
   integer, external :: credat
   integer, external :: putelt
   !
   character*16 :: grpnam
   character*16 :: grpdef
   character*16 :: elmnms(1)
   character*(*) :: dbfil
   character*1024 :: error_string
   !
   integer :: ierr
   integer :: lundia
   integer :: uindex(3, 5)
   integer :: grpdms(5)
   integer :: order(5)
   !
   logical :: fout
   !
   integer :: i
   integer :: idx
   integer :: fds
   !
   real :: q
   real :: rdummy(1)
   !
   !-------------------------------------------------------------------
   !
   grpdms = 1
   grpdms(1) = 0
   do i = 1, 5
      order(i) = i
   end do
   grpnam = 'CURDIS'
   !
   ! Initialize local variables
   ierr = open_datdef(dbfil, fds)
   if (ierr /= 0) goto 9999
   !
   ierr = inqdat(fds, grpnam, grpdef)
   if (ierr /= 0) then
      !
      ! group does not yet exist, so define it.
      !
      ierr = defelm(fds, 'Q', 'REAL', 4, ' ', '[ M3/S  ]', 'Discharge', 1, (1))
      if (ierr /= 0) goto 9999
      !
      ! Define the cell.
      !
      elmnms(1) = 'Q'
      ierr = defcel(fds, grpnam, 1, elmnms)
      if (ierr /= 0) goto 9999
      !
      ! Define the group definition.
      !
      ierr = defgrp(fds, grpnam, grpnam, 1, grpdms, order)
      if (ierr /= 0) goto 9999
      !
      ! Create the group.
      !
      ierr = credat(fds, grpnam, grpnam)
      if (ierr /= 0) goto 9999
      !
   end if
   !
   ! write Q for group CURDIS (cel number IDX)
   !
   uindex = 1
   uindex(1, 1) = idx
   uindex(2, 1) = idx
   !
   rdummy(1) = q
   ierr = putelt(fds, grpnam, 'Q', uindex, order, rdummy)
   if (ierr /= 0) goto 9999
   !
   ! ERROR HANDLING
   !
9999 continue
   if (ierr /= 0) then
      write (lundia, *) 'Nefis error:', ierr, ' encountered in wrq'
      ierr = neferr(0, error_string)
      write (lundia, *) trim(error_string)
      fout = .true.
   end if
   !
   ! Close NEFIS file when opened
   !
   ierr = clsnef(fds)
   !
end subroutine wrq
