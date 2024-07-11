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

!> Initializes a new user-timestep (advances user time, sets new meteo forcing)
!!
!! Should be followed by a flow_run_usertimestep and a flow_finalize_usertimestep.
subroutine flow_init_usertimestep(iresult)
   use m_flowtimes
   use dfm_error
   use MessageHandling
   use m_flowparameters,     only: janudge
   use m_partitioninfo,      only: jampi, abort_all
   use fm_external_forcings

   implicit none
   integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

 iresult = DFM_GENERICERROR

 call inctime_user()

 tim1fld = max(time_user, tim1fld )
 if ( janudge.eq.1 ) call setzcs()
 call set_external_forcings(tim1fld ,.false. , iresult)    ! set field oriented forcings. boundary oriented forcings are in
 if (iresult /= DFM_NOERR) then
    goto 888
 end if

 ! call flow_externalinput (time_user)                 ! receive signals etc
   iresult = DFM_NOERR
   return ! Return with success.

888 continue

 if (iresult /= DFM_NOERR) then
    write (msgbuf,*) ' Error found in EC-module ' ; call err_flush()
    if (jampi == 1) then
       write(msgbuf,*) 'Error occurs on one or more processes when setting external forcings on boundaries at time=', tim1bnd;
       call err_flush()
       ! Terminate all MPI processes
       call abort_all()
    endif
    goto 888
 end if

end subroutine flow_init_usertimestep
