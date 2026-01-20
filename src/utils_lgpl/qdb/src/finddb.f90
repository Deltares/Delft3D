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
subroutine finddb(dbfil, lundia, fout, q, qfound, idx, nidx)
   !
   !===================================================================
   !     DEFINE VARIABLES
   !-------------------------------------------------------------------
   implicit none
   !
   ! External INTEGER functions
   !
   integer, external :: open_datdef
   integer, external :: clsnef
   integer, external :: inqmxi
   integer, external :: getelt
   integer, external :: neferr
   !
   character*16 :: grpnam
   character*(*), intent(in) :: dbfil
   character*1024 :: error_string
   !
   integer :: ierr
   integer, intent(in) :: lundia
   !
   logical, intent(out) :: fout
   !
   integer, intent(out) :: idx
   integer, intent(out) :: nidx
   integer :: i
   integer :: fds
   integer :: uindex(3, 5)
   !
   real, pointer :: qread(:)
   real, intent(in) :: q
   real, intent(out) :: qfound
   real :: qnear
   !
   !-------------------------------------------------------------------
   !
   fout = .false.
   grpnam = 'CURDIS'
   qfound = -1.0
   !
   idx = 0
   nidx = 0
   ierr = open_datdef(dbfil, fds)
   if (ierr /= 0) goto 9999
   !
   ierr = inqmxi(fds, grpnam, nidx)
   if (ierr == 6004) then
      !
      ! Group CURDIS does not exist
      !
      ierr = 0
      !
      ! Print discharges in diagnostic file
      !
      if (q < 0) then
         write (lundia, *) 'No discharges stored'
      end if
      goto 9999
   elseif (ierr /= 0) then
      goto 9999
   end if
   !
   ! search Q element in CURDIS for matching Q
   !
   allocate (qread(nidx))
   !
   uindex(1, 1) = 1 ! start index
   uindex(2, 1) = nidx ! end index
   uindex(3, 1) = 1 ! increment in time
   !
   ierr = getelt(fds, grpnam, 'Q', uindex, 1, 4 * nidx, qread)
   if (ierr /= 0) goto 9998
   !
   ! Search an exact match
   !
   do i = 1, nidx
      !
      ! Print discharges in diagnostic file (LIST)
      !
      if (q < 0) write (lundia, *) 'STORED DISCHARGE ', i, ' =', qread
      !
      ! Search the discharge (FIND)
      !
      if (q == qread(i)) then
         idx = i
         qfound = q
         goto 9998
      end if
   end do
   if (q < 0) goto 9998
   !
   ! Search the largest dicharge smaller than the requested discharge
   !
   qfound = 0.0
   do i = 1, nidx
      if (qfound < qread(i) .and. qread(i) < q) then
         idx = i
         qfound = qread(i)
      end if
   end do
   if (qfound > 0.0) goto 9998
   !
   ! Search the smallest dicharge bigger than the requested discharge
   !
   qfound = 1.01e30
   do i = 1, nidx
      if (q < qread(i) .and. qread(i) < qfound) then
         idx = i
         qfound = qread(i)
      end if
   end do
   if (qfound > 1.0e30) qfound = -1.0
   !
9998 continue
   deallocate (qread)
   !
   ! ERROR HANDLING
   !
9999 continue
   if (ierr /= 0) then
      write (lundia, *) 'Nefis error:', ierr, ' encountered in finddb'
      ierr = neferr(0, error_string)
      write (lundia, *) trim(error_string)
      fout = .true.
   end if
   !
   ! Close NEFIS file when opened
   !
   ierr = clsnef(fds)
   !
end subroutine finddb
