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

!> Finalizes a single time step, should be called directly after flow_run_single_timestep
subroutine flow_finalize_single_timestep(iresult)
   use m_flow
   use m_flowgeom
   use m_flowtimes
   use unstruc_model, only: md_fou_step
   use unstruc_netcdf
   use timers
   use m_timer
   use m_gui
   use dfm_error
   use dfm_signals
   use m_mass_balance_areas, only: jamba
   use m_partitioninfo, only: jampi, my_rank
   use m_integralstats, is_is_numndvals => is_numndvals
   use m_oned_functions, only: updateTimeWetOnGround, updateTotalInflow1d2d, updateTotalInflowLat, &
                               updateFreeboard, updateDepthOnGround, updateVolOnGround
   use unstruc_channel_flow, only: network
   use m_sedtrails_stats, st_is_numndvals => is_numndvals
   use fm_statistical_output
   use m_statistical_output, only: update_statistical_output, update_source_input
   use m_update_fourier, only: update_fourier
   use mass_balance_areas_routines, only: comp_horflowmba
   use m_laterals, only: numlatsg
   use m_update_values_on_cross_sections, only: update_values_on_cross_sections
   use m_structure_parameters
   use m_flow_f0isf1

   implicit none
   integer, intent(out) :: iresult

   ! Timestep has been performed, now finalize it.

   if (ti_waqproc < 0d0) then
      if (jatimer == 1) call starttimer(IFMWAQ)
      call fm_wq_processes_step(dts, time1)
      if (jatimer == 1) call stoptimer(IFMWAQ)
   end if

   if (jamba > 0) then ! at moment, this function is only required for the mass balance areas
      call comp_horflowmba()
   end if

   call flow_f0isf1() ! mass balance and vol0 = vol1

   ! Update water depth at pressure points (for output).
   hs = s1 - bl
   call structure_parameters()

   if (jaQext > 0) then
      call updateCumulativeInflow(dts)
   end if

!       only update values at the observation stations when necessary
!          alternative: move this to flow_externaloutput
   call timstrt('update HIS data DtUser', handle_extra(75))
   if (ti_his > 0) then

      call updateValuesOnObservationStations()

      if (comparereal(time1, time_his, eps10) >= 0) then
         !do_fourier = do_fourier .or. (md_fou_step == 2)
         if (jampi == 1) then
            call updateValuesOnRunupGauges_mpi()
            !call reduce_particles()
         end if
         if (jahisbal > 0) then ! Update WaterBalances etc.
            call updateBalance()
         end if
         if (jacheckmonitor == 1) then
            !compute "checkerboard" monitor
            call comp_checkmonitor()
         end if
      end if
   end if
   call timstop(handle_extra(75))

   call update_values_on_cross_sections
   call updateValuesOnRunupGauges()
   if (jampi == 0 .or. (jampi == 1 .and. my_rank == 0)) then
      if (numsrc > 0) then
         call updateValuesonSourceSinks(time1) ! Compute discharge and volume on sources and sinks
      end if
   end if

   if (jahislateral > 0 .and. numlatsg > 0 .and. ti_his > 0) then
      call updateValuesOnLaterals(time1, dts)
   end if

   ! for 1D only
   if (network%loaded .and. ndxi - ndx2d > 0) then
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

   ! Time-integral statistics on all flow nodes.
   if (is_is_numndvals > 0) then
      call update_integralstats()
   end if

   if (jasedtrails > 0) then
      if (st_is_numndvals > 0) then
         call update_sedtrails_stats()
      end if
   end if

   if (jaGUI == 1) then
      call TEXTFLOW()
   end if

   call update_source_input(out_variable_set_his)
   call update_source_input(out_variable_set_map)
   call update_source_input(out_variable_set_clm)

   if (out_variable_set_his%count > 0) then
      call update_statistical_output(out_variable_set_his%statout, dts)
   end if

!call update_statistical_output(out_variable_set_map%configs,dts)
!call update_statistical_output(out_variable_set_clm%configs,dts)

   dnt = dnt + 1
   time0 = time1 ! idem
   dtprev = dts ! save previous timestep

   if (jatimer == 1) then ! TODO: AvD: consider moving timers to flow_perform_*
      call stoptimer(ITIMESTEP)
      numtsteps = numtsteps + 1
   end if

   call timstop(handle_steps)
   iresult = dfm_check_signals() ! Abort when Ctrl-C was pressed
   if (iresult /= DFM_NOERR) goto 888

   if (validateon) then
      call flow_validatestate(iresult) ! abort when the solution becomes unphysical
   end if
   validateon = .true.
   if (iresult /= DFM_NOERR) goto 888

888 continue

   if (md_fou_step == 1) then
      call update_fourier(dts)
   end if

end subroutine flow_finalize_single_timestep
