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
subroutine readmn(simfil, lundia, fout, filetype, &
                & mmax, nmax, kmax)
   !
   !===================================================================
   ! DEFINE VARIABLES
   !-------------------------------------------------------------------
   !
   implicit none
   !
   ! External INTEGER functions
   !
   integer, external :: clsnef
   integer, external :: getelt
   integer, external :: inqdat
   integer, external :: neferr
   integer, external :: open_datdef
   !
   ! CHARACTER variables
   !
   character*6 :: errmsg
   character*(*) :: simfil
   character(16) :: filetype
   character(16) :: celnam
   character(16) :: grpnam
   character(1024) :: error_string
   !
   ! INTEGER variables
   !
   integer :: fds
   integer :: ierr
   integer :: kmax
   integer :: lundia
   integer :: mmax
   integer :: nmax
   integer, dimension(1) :: idummy
   integer, dimension(3, 5) :: uindex
   !
   ! LOGICAL variables
   !
   logical :: fout
   !
   !-------------------------------------------------------------------
   ! end of DEFINE VARIABLES
   !===================================================================
   !
   ! Initialize local variables
   !
   ierr = 0
   uindex(1, 1) = 1 ! start index
   uindex(2, 1) = 1 ! end index
   uindex(3, 1) = 1 ! increment in time
   !
   ierr = open_datdef(simfil, fds)
   if (ierr /= 0) goto 9999
   !
   ! first try reading the file as a COM file
   !
   filetype = 'COM'
   grpnam = 'GRID'
   ierr = inqdat(fds, grpnam, celnam)
   if (ierr == 6004) then
      !
      ! now try reading the file as a TRIM file
      !
      filetype = 'TRIM'
      grpnam = 'map-const'
      ierr = inqdat(fds, grpnam, celnam)
   end if
   if (ierr /= 0) goto 9999
   !
   !------------------------------------------------------------------
   !
   ! element  MMAX
   !
   ! write(lundia,*) 'mmax ...'
   !
   ierr = getelt(fds, grpnam, 'MMAX', uindex, 1, 4, idummy)
   if (ierr /= 0) goto 9999
   mmax = idummy(1)
   !
   ! write(lundia,*) mmax
   !
   !------------------------------------------------------------------
   !
   ! element  NMAX(US)
   !
   ! write(lundia,*) 'nmax ...'
   !
   ierr = getelt(fds, grpnam, 'NMAX', uindex, 1, 4, idummy)
   if (ierr /= 0) goto 9999
   nmax = idummy(1)
   !
   ! write(lundia,*) nmax
   !
   !------------------------------------------------------------------
   !
   ! element  KMAX
   !
   ! write(lundia,*) 'kmax ...'
   !
   ierr = getelt(fds, grpnam, 'KMAX', uindex, 1, 4, idummy)
   if (ierr /= 0) goto 9999
   kmax = idummy(1)
   !
   ! write(lundia,*) kmax
   !
   !------------------------------------------------------------------
   !
   ierr = clsnef(fds)
   !
   !------------------------------------------------------------------
   !
9999 continue
   if (ierr /= 0) then
      !
      ! If error occured then write error message to diagnostic file
      !
      write (errmsg, '(i6)') ierr
      write (lundia, '(1X,A,A,A)') 'Nefis error ', errmsg, ' in readmn'
      ierr = neferr(0, error_string)
      write (lundia, '(1X,A)') trim(error_string)
      fout = .true.
   end if
end subroutine readmn
