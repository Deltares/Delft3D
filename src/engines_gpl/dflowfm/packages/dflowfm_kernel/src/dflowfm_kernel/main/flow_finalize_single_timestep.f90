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

!> Finalizes a single time step, should be called directly after flow_run_single_timestep
subroutine flow_finalize_single_timestep(iresult)
use m_flow
use m_flowgeom
use m_flowtimes
use unstruc_model, only : md_fou_step
use unstruc_netcdf
use timers
use m_timer
use unstruc_display, only : jaGUI
use dfm_error
use dfm_signals
use m_mass_balance_areas, only: jamba
use m_partitioninfo, only: jampi, my_rank
use m_integralstats, is_is_numndvals=>is_numndvals
use m_oned_functions, only: updateTimeWetOnGround, updateTotalInflow1d2d, updateTotalInflowLat, &
                            updateFreeboard, updateDepthOnGround, updateVolOnGround
use unstruc_channel_flow, only : network
use m_sedtrails_stats, st_is_numndvals=>is_numndvals
use m_update_fourier, only : update_fourier
use mass_balance_areas_routines, only : comp_horflowmba
use m_lateral, only : numlatsg

implicit none
integer, intent(out) :: iresult

   ! Timestep has been performed, now finalize it.

   if (ti_waqproc < 0d0) then
      if ( jatimer.eq.1 ) call starttimer(IFMWAQ)
      call fm_wq_processes_step(dts,time1)
      if ( jatimer.eq.1 ) call stoptimer (IFMWAQ)
   endif

   if (jamba > 0) then  ! at moment, this function is only required for the mass balance areas
      call comp_horflowmba()
   endif

 call flow_f0isf1()                                  ! mass balance and vol0 = vol1

 ! Update water depth at pressure points (for output).
 ! TODO: UNST-3415: investigate if this statement can be moved to step_reduce.
 hs = s1 - bl

 ! The subroutine below is called in every time step.
 ! TODO: consider to treat it as the cross section, that the mpi reduction is made at his-output time step.
 call structure_parameters()

 dnt    = dnt + 1
 time0  = time1                                      ! idem
 dtprev = dts                                        ! save previous timestep

 if ( jatimer.eq.1 ) then ! TODO: AvD: consider moving timers to flow_perform_*
   call stoptimer(ITIMESTEP)
   numtsteps = numtsteps + 1
 end if

 ! call wriinc(time1)

  if (jaQext > 0) then
     call updateCumulativeInflow(dts)
  end if

  call updateValuesOnCrossSections(time1)             ! Compute sum values across cross sections.
  call updateValuesOnRunupGauges()
 if (jampi == 0 .or. (jampi == 1 .and. my_rank==0)) then
    if (numsrc > 0) then
       call updateValuesonSourceSinks(time1)         ! Compute discharge and volume on sources and sinks
    endif
 endif

 if (jahislateral > 0 .and. numlatsg > 0 .and. ti_his > 0) then
    call updateValuesOnLaterals(time1, dts)
 end if


 ! for 1D only
 if (network%loaded .and. ndxi-ndx2d > 0) then
    if (jamapTimeWetOnGround > 0) then
       call updateTimeWetOnGround(dts)
    end if
    if (jamapTotalInflow1d2d > 0) then
       call updateTotalInflow1d2d(dts)
    end if
    if (jamapTotalInflowLat > 0) then
       call updateTotalInflowLat(dts)
    end if
 end if
 ! note updateValuesOnObservationStations() in flow_usertimestep

 ! Time-integral statistics on all flow nodes.
 if (is_is_numndvals > 0) then
    call update_integralstats()
 end if
 
 if (jasedtrails>0) then
    if (st_is_numndvals > 0) then
       call update_sedtrails_stats()
    end if
 endif   

 if ( jaGUI.eq.1 ) then
    call TEXTFLOW()
 end if

 call timstop(handle_steps)
 iresult = dfm_check_signals()                      ! Abort when Ctrl-C was pressed
 if (iresult /= DFM_NOERR) goto 888

 if (validateon) then
    call flow_validatestate(iresult)                ! abort when the solution becomes unphysical
 endif
 validateon = .true.
 if (iresult /= DFM_NOERR) goto 888

888 continue

   if (md_fou_step == 1) then
      call update_fourier(dts)
   end if

end subroutine flow_finalize_single_timestep
