!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

subroutine wricom(tim)
   use m_flow
   use m_flowtimes
   use m_observations
   use unstruc_netcdf
   use unstruc_model
   use unstruc_files, only: defaultFilename
   implicit none
   double precision, intent(in) :: tim

   ! locals

   type(t_unc_mapids), save :: comids
   integer :: ierr
   character(len=256), save :: filnam
   character(len=256) :: msg
   logical :: file_exists

   ! When leaving netcdf-file open and using nf90_sync:
   ! Data did not appear during debugging
   ! This problem was solved by closing/opening the file everytime
   !
   if (comids%ncid /= 0 .and. jawave == 3) then
      !
      ! Existing/ongoing communication via com file:
      ! com file already exists
      !
      ierr = nf90_open(filnam, NF90_WRITE, comids%ncid)
   elseif (comids%ncid == 0 .and. jawave == 3) then
      !
      ! No communication yet via com file:
      ! Check whether com file exists
      !
      filnam = defaultFilename('com')
      md_wavefile = filnam
      inquire (file=filnam, exist=file_exists)
      if (file_exists) then
         write (msg, '(3a)') "File '", trim(filnam), "' already exists. Assuming that it contains valid WAVE information. FLOW data will be added."
         call mess(LEVEL_WARN, trim(msg))
         ierr = nf90_open(filnam, NF90_WRITE, comids%ncid)
      else
         ! No com file yet. Create a new one and write FLOW parameters
         !
         ierr = unc_create(filnam, 0, comids%ncid)
         if (ierr /= nf90_noerr) then
            call mess(LEVEL_WARN, 'Could not create com file.')
            comids%ncid = 0
         end if
      end if
   end if

   if (comids%ncid /= 0) then
      call unc_write_map_filepointer(comids%ncid, tim, 2)
   end if

   ierr = nf90_close(comids%ncid) ! Flush file
end subroutine wricom
