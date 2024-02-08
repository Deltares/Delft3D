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

!> add tracer to constituents, or get constituents number if tracer already exists
subroutine add_tracer(tracer_name, iconst)
   use m_transport
   use unstruc_messages
   use m_meteo, only: numtracers, trnames
   implicit none

   character(len=*), intent(in)  :: tracer_name  !< tracer name, or '' for default name
   integer,          intent(out) :: iconst       !< constituent number

   character(len=8)              :: str

   integer                       :: ierror, i, itrac

   integer, external             :: findname

   ierror = 1

!  check if tracer already exists, based on tracer name
   iconst = 0
   if ( ITRA1.gt.0 .and. trim(tracer_name).ne.'') then
      iconst = findname(NUMCONST, const_names, tracer_name)

      if ( iconst.ge.ITRA1 .and. iconst.le.ITRAN ) then  ! existing tracer found
         call mess(LEVEL_INFO, 'add_tracer: tracer ' // trim(tracer_name) // ' already exists. No update required.')
         goto 1234
      end if
   end if

!  append tracer
   if ( ITRA1.eq.0) then
      NUMCONST = NUMCONST+1
      ITRA1    = NUMCONST
      ITRAN    = NUMCONST
   else
!     check if tracers are at the back
      if ( iTRAN.ne.NUMCONST ) then
         call mess(LEVEL_ERROR, 'add_tracer: tracer(s) not at the back of the constituents array')
         goto 1234
      end if

      NUMCONST = NUMCONST+1
      ITRAN    = NUMCONST
   end if

!  output tracer number
   iconst = ITRAN

!  reallocate arrays
   call alloc_transport(.true.)

!  set name
   if ( trim(tracer_name).ne.'' ) then
      const_names(ITRAN) = trim(tracer_name)
   else
      write(str,"(I0)") ITRAN-ITRA1+1
      const_names(ITRAN) = 'tracer_'//trim(str)
   end if

   if ( numtracers.gt.0 ) then   ! number of tracers with boundary conditions
!     generate tracer (boundary condition) to constituent
      itrac = findname(numtracers,trnames,trim(tracer_name))
      if ( itrac.gt.0 ) then
         itrac2const(itrac) = iconst
      end if
   end if

   ierror = 0

1234 continue

   return
end subroutine add_tracer
