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
   use m_flowexternalforcings
   use m_alloc
   use m_missing
   use m_fm_wq_processes
   use unstruc_messages

   implicit none

   character(len=*), intent(in)  :: tracnam
   character(len=20), intent(in) :: tracunit
   integer,          intent(out) :: itrac
   integer,          intent(out) :: janew

   integer,          external    :: findname
   integer                       :: iwqbot

   if ( .not. allocated(trnames) ) then
       allocate( trnames(0) )
   endif
   if ( .not. allocated(wqbotnames) ) then
       allocate( wqbotnames(0) )
   endif

   itrac = findname(numtracers, trnames, tracnam)
   iwqbot = findname(numwqbots, wqbotnames, tracnam)

   if ( iwqbot.ne.0 ) then
      call mess(LEVEL_ERROR, 'add_bndtracer: tracer named '''//trim(tracnam)//''' already exists as a water quality bottom variable')
   endif

   janew = 0
   if ( itrac.eq.0 ) then
      janew = 1
!     add tracer

      numtracers = numtracers+1
!     realloc
      call realloc(nbndtr, numtracers, keepExisting=.true., fill=0 )
      call realloc(trnames, numtracers, keepExisting=.true., fill='')
      call realloc(trunits, numtracers, keepExisting=.true., fill='')
      call realloc(wstracers, numtracers, keepExisting=.true., fill=0d0)
      call realloc(decaytimetracers, numtracers, keepExisting=.true., fill=0d0)
      if ( transformcoef(4).ne.DMISS ) then
         wstracers(numtracers) = transformcoef(4)
      endif
      if ( transformcoef(5) /= dmiss .and. transformcoef(5) /= 0d0 ) then
          jadecaytracers = 1
          decaytimetracers(numtracers) = transformcoef(5)
      endif

      trnames(numtracers) = trim(tracnam)
      itrac = numtracers
   else
      if (transformcoef(4) /= dmiss .and. transformcoef(4) /= 0d0 .and. transformcoef(4) /= wstracers(itrac)) then
         write (msgbuf, '(a,e10.5,a,e10.5,a)') 'add_bndtracer: tracer '''//trim(tracnam)//''' already has a fall velocity (', wstracers(itrac), &
                                               '). Ignoring different value (', transformcoef(4),').'
         call warn_flush()
      end if
      if (transformcoef(5) /= dmiss .and. transformcoef(5) /= 0d0 .and. transformcoef(5) /= decaytimetracers(itrac)) then
         write (msgbuf, '(a,e10.5,a,e10.5,a)') 'add_bndtracer: tracer '''//trim(tracnam)//''' already has a decay time (', decaytimetracers(itrac), &
                                               '). Ignoring different value (', transformcoef(5),').'
         call warn_flush()
      end if
   end if

   trunits(itrac) = tracunit
end subroutine add_bndtracer
