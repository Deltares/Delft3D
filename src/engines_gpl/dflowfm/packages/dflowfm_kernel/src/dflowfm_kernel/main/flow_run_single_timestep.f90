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

!> Performs a single computational timestep, but not the init and finalize of the timestep.
subroutine flow_run_single_timestep(key, iresult)                ! do only 1 flow timestep
use m_flow
use timers
use m_flowgeom
use m_flowtimes
use unstruc_netcdf
use m_timer
use unstruc_display, only : jaGUI
use dfm_error

implicit none

integer :: key
integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful. DFM_TIMESETBACK if succesful, but with timestep setbacks.

integer :: N, L

 iresult = DFM_GENERICERROR

 if (itstep >= 2) then
    call timstrt('step_reduce', handle_extra(51)) ! step_reduce
    select case (flow_solver)
        case (FLOW_SOLVER_FM)
            call step_reduce_hydro(key)                            ! set a computational timestep implicit, reduce, elim conj grad substi
        case (FLOW_SOLVER_SRE)
            !time step advanced in <flow_single_timestep>.
            call flow_initialize_fm1dimp_timestep(iresult,time1) !in kernel, can access everything
            call SOFLOW_wrap(time1) !in module, only accesses SRE variables
            call flow_finalize_fm1dimp_timestep() !in kernel, can access everything
    end select
    call step_reduce_transport_morpho(key)
    
    call timstop(handle_extra(51)) ! step_reduce

    if (dsetb > 0) then
       iresult = DFM_TIMESETBACK ! Warning about setbacks, but don't return directly, continue function normally
    end if
 else
    call velocities_explicit()                       ! progress without pressure coupling
    call transport()                                 ! progress without pressure coupling
    time1  = time0 + dts                             ! progress without pressure coupling
 endif

 if (jaeverydt > 0) then
    if ((comparereal(time1, ti_maps, eps10) >= 0) .and. (comparereal(time1, ti_mape, eps10) <= 0)) then
       if (jamapFlowAnalysis > 0) then
          ! update the cumulative flow analysis parameters, and also compute the right CFL numbers
          call updateFlowAnalysisParameters()
       endif
         
       call wrimap(time1)
       
       if (jamapFlowAnalysis > 0) then
          ! Reset the interval related flow analysis arrays
          negativeDepths = 0
          noiterations = 0
          limitingTimestepEstimation = 0
          flowCourantNumber = 0d0
       endif
    
    end if
 end if
   ! Finalize timestep code used to be here, now flow_finalize_single_timestep()

   if (iresult /= DFM_TIMESETBACK) then
      iresult = DFM_NOERR
   end if

   return ! Return with success

888 continue
   ! Error

end subroutine flow_run_single_timestep
