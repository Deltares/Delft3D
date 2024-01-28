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

!> Runs flow steps for a certain period (do computational flowsteps for as long as timeinterval dtrange).
subroutine flow_run_sometimesteps(dtrange, iresult)                   ! do computational flowsteps for as long as timeinterval dtrange
   use m_flowtimes
   use unstruc_messages
   use m_partitioninfo
   use unstruc_display, only: jaGUI
   use dfm_error
   implicit none
   double precision, intent(in)  :: dtrange
   integer,          intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.
   integer                       :: key

   double precision :: timetarget

   iresult = DFM_GENERICERROR
   if (dtrange < 0) then
      timetarget = time1 + epsilon(1d0) ! dtrange < 0 means: auto pick a *single* timestep. Enforce this with a target time *just* larger than current time.
   else
      timetarget = time1 + dtrange
   end if

   timetarget = min(timetarget, tstop_user)


 do while (time1 < timetarget)                        ! nb, outside flow_singletimestep, time0=time1 !

    !! INIT only in case of new user timestep
    if (time1 >= time_user) then
       call flow_init_usertimestep(iresult)

       if (iresult /= DFM_NOERR) then
          goto 888
       end if
    end if

    !! RUN actual SINGLE computational timestep
    call flow_single_timestep(key, iresult)
    if (iresult /= DFM_NOERR) then
       goto 888
    end if

    !! FINALIZE only when a time_user is finished
    if (time1 >= time_user) then
       call flow_finalize_usertimestep(iresult)

       if (iresult /= DFM_NOERR) then
          goto 888
       end if
    end if

 enddo

   iresult = DFM_NOERR
   return ! Return with success.

888 continue
end subroutine flow_run_sometimesteps
