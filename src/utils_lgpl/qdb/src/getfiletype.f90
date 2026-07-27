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
subroutine getfiletype(simfil, lundia, fout, filetype)
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
   integer :: lundia
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
      if (ierr == 6004) then
         filetype = 'UNKNOWN'
         ierr = 0
      end if
   end if
   if (ierr /= 0) goto 9999
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
      write (lundia, '(1X,A,A,A)') 'Nefis error ', errmsg, ' in getfiletype'
      ierr = neferr(0, error_string)
      write (lundia, '(1X,A)') trim(error_string)
      fout = .true.
   end if
end subroutine getfiletype
