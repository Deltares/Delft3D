!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

module m_step_reduce_transport_morpho

   use m_subsupl_update_s1, only: subsupl_update_s1, subsupl_update_constituents
   use m_setucxucy_mor, only: setucxucy_mor
   use m_fm_flocculate, only: fm_flocculate
   use m_fm_fallve, only: fm_fallve
   use m_apply_subsupl, only: apply_subsupl
   use m_update_s_explicit, only: update_s_explicit
   use m_u1q1, only: u1q1
   use m_transport_sub, only: transport

   use precision, only: dp
   implicit none

   private

   public :: step_reduce_transport_morpho

contains

   subroutine step_reduce_transport_morpho()
      use m_equili_spiralintensity
      use m_flow
      use m_laterals, only: qlatwaq, qlatwaq0
      use m_flowgeom
      use m_sediment, only: stm_included
      use Timers
      use m_flowtimes
      use m_sferic
      use m_wind
      use m_reduce
      use m_ship
      use m_partitioninfo
      use m_timer
      use m_xbeach_data
      use MessageHandling
      use m_sobekdfm
      use unstruc_display
      use m_waves
      use m_subsidence, only: jasubsupl
      use m_fm_bott3d, only: fm_bott3d
      use m_fm_erosed, only: ti_sedtrans
      use m_curvature, only: get_curvature
      use m_xbeach_netcdf, only: xbeach_mombalance
      use mass_balance_areas_routines, only: comp_bedload_fluxmba, comp_fixedbed_fluxmba, comp_bedflowmba
      use m_mass_balance_area_data, only: jamba
      use m_set_kbot_ktop
      use m_fm_erosed_sub, only: fm_erosed
      use m_source_sink, only: source_sinks

      numnodneg = 0
      if (wrwaqon) then
         ! store current cumulative source_sink_water_discharge and qlat for waq at the beginning of this time step
         if (allocated(source_sinks%cumulative_discharge_waq)) then
            source_sinks%cumulative_discharge_waq_previous = source_sinks%cumulative_discharge_waq
         end if
         if (allocated(qlatwaq)) then
            qlatwaq0 = qlatwaq
         end if
      end if

      !-----------------------------------------------------------------------------------------------
      ! TODO: AvD: consider moving everything below to flow_finalize single_timestep?
      call set_kbot_ktop(jazws0=0) ! bottom and top layer indices and new sigma distribution

      if (flow_solver == FLOW_SOLVER_FM) then
         call u1q1() ! the vertical flux qw depends on new sigma => after set_kbot_ktop
         call compute_q_total_1d2d()
      end if

      !if ( jacheckmonitor.eq.1 ) then
      !   call comp_checkmonitor()
      !end if

      if (itstep == 4) then ! explicit time-step
         call update_s_explicit()
      end if
      hs = s1 - bl
      hs = max(hs, 0.0_dp)

      if (jased > 0 .and. stm_included) then
         if (time1 >= tstart_user + ti_sedtrans * tfac) then
            if (jatimer == 1) then
               call starttimer(IEROSED)
            end if
            !
            call setucxucy_mor(u1)
            call fm_flocculate() ! fraction transitions due to flocculation

            call timstrt('Settling velocity   ', handle_extra(87))
            call fm_fallve() ! update fall velocities
            call timstop(handle_extra(87))

            call timstrt('Erosed_call         ', handle_extra(88))
            call fm_erosed() ! source/sink, bedload/total load
            call timstop(handle_extra(88))

            if (jatimer == 1) then
               call stoptimer(IEROSED)
            end if
         end if
      end if

      ! secondary flow
      if (jasecflow > 0 .and. kmx == 0) then
         call get_curvature()
         if (jaequili > 0) then
            call equili_spiralintensity()
         end if
      end if

      !SPvdP: timestep is now based on u0, q0
      !       transport is with u1,q1 with timestep based on u0,q0
      if (jatimer == 1) then
         call starttimer(ITRANSPORT)
      end if
      call transport()
      if (jatimer == 1) then
         call stoptimer(ITRANSPORT)
      end if

      if (jamba > 0 .and. ((jased > 0 .and. stm_included) .or. jasubsupl > 0)) then
         call comp_bedflowmba(before_update=.true.)
      end if
      if (jased > 0 .and. stm_included) then
         if (jamba > 0) then
            call comp_fixedbed_fluxmba()
         end if
         call fm_bott3d() ! bottom update
         ! Account for the corrected bedload used by this step's composition update.
         call comp_bedload_fluxmba()
      end if

      if (jasubsupl > 0) then
         if (jased > 0 .and. stm_included) then
            ! Complete the morphology volume change before saving the subsidence volumes.
            call update_bed_geometry(adjust_water_level=.false.)
         end if
         call apply_subsupl()
      end if

      if ((jased > 0 .and. stm_included) .or. (jasubsupl > 0)) then
         call update_bed_geometry(adjust_water_level=jasubsupl > 0)
         if (jasubsupl > 0) then
            call subsupl_update_constituents()
         end if
         if (jamba > 0) then
            call comp_bedflowmba(before_update=.false.)
         end if
      end if

   end subroutine step_reduce_transport_morpho

   !> Rebuild cell volumes after bed movement, including all vertical layers.
   subroutine update_bed_geometry(adjust_water_level)
      use m_set_bobs, only: setbobs
      use m_volsur, only: volsur
      use m_set_kbot_ktop, only: set_kbot_ktop
      use m_flow, only: kmx

      logical, intent(in) :: adjust_water_level !< Apply the configured subsidence water-level adjustment.

      call setbobs()
      if (adjust_water_level) then
         call subsupl_update_s1()
      end if
      call volsur()
      if (kmx > 0) then
         call set_kbot_ktop(jazws0=0)
      end if
   end subroutine update_bed_geometry

end module m_step_reduce_transport_morpho
