!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2022.
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
   use m_subsupl_update_s1, only: subsupl_update_s1
   use m_setucxucy_mor, only: setucxucy_mor
   use m_fm_flocculate, only: fm_flocculate
   use m_fm_fallve, only: fm_fallve
   use m_apply_subsupl, only: apply_subsupl
   use m_update_s_explicit, only: update_s_explicit
   use m_u1q1, only: u1q1
   use m_transport_sub, only: transport

   implicit none

   private

   public :: step_reduce_transport_morpho

contains

   subroutine step_reduce_transport_morpho()
      use m_equili_spiralintensity
      use m_flow
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
      use mass_balance_areas_routines, only: comp_bedload_fluxmba
      use m_set_kbot_ktop
      use m_volsur
      use m_set_bobs
      use m_fm_erosed_sub, only: fm_erosed

      numnodneg = 0
      if (wrwaqon .and. allocated(qsrcwaq)) then
         qsrcwaq0 = qsrcwaq ! store current cumulative qsrc for waq at the beginning of this time step
      end if

!-----------------------------------------------------------------------------------------------
! TODO: AvD: consider moving everything below to flow_finalize single_timestep?
      call setkbotktop(0) ! bottom and top layer indices and new sigma distribution

      if (flow_solver == FLOW_SOLVER_FM) then
         call u1q1() ! the vertical flux qw depends on new sigma => after setkbotktop
         call compute_q_total_1d2d()
      end if

      !if ( jacheckmonitor.eq.1 ) then
      !   call comp_checkmonitor()
      !end if

      if (itstep == 4) then ! explicit time-step
         call update_s_explicit()
      end if
      hs = s1 - bl
      hs = max(hs, 0d0)

      if (jased > 0 .and. stm_included) then
         if (time1 >= tstart_user + ti_sedtrans * tfac) then
            if (jatimer == 1) call starttimer(IEROSED)
            !
            call setucxucy_mor(u1)
            call fm_flocculate() ! fraction transitions due to flocculation

            call timstrt('Settling velocity   ', handle_extra(87))
            call fm_fallve() ! update fall velocities
            call timstop(handle_extra(87))

            call timstrt('Erosed_call         ', handle_extra(88))
            call fm_erosed() ! source/sink, bedload/total load
            call timstop(handle_extra(88))

            call comp_bedload_fluxmba()
            if (jatimer == 1) call stoptimer(IEROSED)
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
      if (jatimer == 1) call starttimer(ITRANSPORT)
      call transport()
      if (jatimer == 1) call stoptimer(ITRANSPORT)

      if (jased > 0 .and. stm_included) then
         call fm_bott3d() ! bottom update
      end if

      if (jasubsupl > 0) then
         call apply_subsupl()
      end if

      if ((jased > 0 .and. stm_included) .or. (jasubsupl > 0)) then
         call setbobs() ! adjust administration - This option only works for ibedlevtyp = 1, otherwise original bed level [bl] is overwritten to original value
         if (jasubsupl > 0) then
            call subsupl_update_s1()
         end if
         call volsur() ! update volumes 2d
         if (kmx > 0) then
            call setkbotktop(0) ! and 3D for cell volumes
         end if
      end if

      ! Moved to flow_finalize_single_timestep: call flow_f0isf1()                                  ! mass balance and vol0 = vol1

      if (layertype > 1 .and. kmx > 0) then

         ! ln = ln0 ! was ok.

      end if

   end subroutine step_reduce_transport_morpho

end module m_step_reduce_transport_morpho
