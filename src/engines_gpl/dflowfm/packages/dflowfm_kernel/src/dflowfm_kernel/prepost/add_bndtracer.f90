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

!> add tracer boundary
subroutine add_bndtracer(tracnam, tracunit, itrac, janew)
   use fm_external_forcings_data
   use m_alloc
   use m_missing
   use m_fm_wq_processes
   use unstruc_messages
   use m_find_name, only: find_name

   implicit none

   character(len=*), intent(in) :: tracnam
   character(len=20), intent(in) :: tracunit
   integer, intent(out) :: itrac
   integer, intent(out) :: janew
   integer :: iwqbot

   if (.not. allocated(trnames)) then
      allocate (trnames(0))
   end if
   if (.not. allocated(wqbotnames)) then
      allocate (wqbotnames(0))
   end if

   itrac = find_name(trnames, tracnam)
   iwqbot = find_name(wqbotnames, tracnam)

   if (iwqbot /= 0) then
      call mess(LEVEL_ERROR, 'add_bndtracer: tracer named '''//trim(tracnam)//''' already exists as a water quality bottom variable')
   end if

   janew = 0
   if (itrac == 0) then
      janew = 1
!     add tracer

      numtracers = numtracers + 1
!     realloc
      call realloc(nbndtr, numtracers, keepExisting=.true., fill=0)
      call realloc(trnames, numtracers, keepExisting=.true., fill='')
      call realloc(trunits, numtracers, keepExisting=.true., fill='')
      call realloc(wstracers, numtracers, keepExisting=.true., fill=0d0)
      call realloc(decaytimetracers, numtracers, keepExisting=.true., fill=0d0)
      if (transformcoef(24) /= DMISS) then
         wstracers(numtracers) = transformcoef(24)
      end if
      if (transformcoef(25) /= dmiss .and. transformcoef(25) /= 0d0) then
         jadecaytracers = 1
         decaytimetracers(numtracers) = transformcoef(25)
      end if

      trnames(numtracers) = trim(tracnam)
      itrac = numtracers
   else
      if (transformcoef(24) /= dmiss .and. transformcoef(24) /= 0d0 .and. transformcoef(24) /= wstracers(itrac)) then
         write (msgbuf, '(a,e12.5,a,e12.5,a)') 'add_bndtracer: tracer '''//trim(tracnam)//''' already has a fall velocity (', &
            wstracers(itrac), '). Ignoring different value (', transformcoef(24), ').'
         call warn_flush()
      end if
      if (transformcoef(25) /= dmiss .and. transformcoef(25) /= 0d0 .and. transformcoef(25) /= decaytimetracers(itrac)) then
         write (msgbuf, '(a,e12.5,a,e12.5,a)') 'add_bndtracer: tracer '''//trim(tracnam)//''' already has a decay time (', &
            decaytimetracers(itrac), '). Ignoring different value (', transformcoef(25), ').'
         call warn_flush()
      end if
   end if

   trunits(itrac) = tracunit
end subroutine add_bndtracer
