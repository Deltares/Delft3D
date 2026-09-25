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

module m_flow_single_timestep

   use precision, only: dp
   implicit none

   private

   public :: flow_single_timestep

contains

   !> A complete single computational time step (init-perform-finalize).
   subroutine flow_single_timestep(key, iresult) ! do only 1 flow timestep
      use m_flow_run_single_timestep, only: flow_run_single_timestep
      use m_flow_init_single_timestep, only: flow_init_single_timestep
      use m_flow_finalize_single_timestep, only: flow_finalize_single_timestep
      use m_sedmor_write_stats, only: sedmor_write_stats
      use m_flow
      use m_flowgeom
      use m_flowtimes
      use m_flowparameters, only: solver_sequence, solver_period_index, FLOW_SOLVER_FM
      use messagehandling, only: mess, LEVEL_INFO
      use unstruc_netcdf
      use m_xbeach_netcdf
      use m_timer
      use dfm_error
      use m_sedtrails_netcdf, only: sedtrails_write_stats

      integer :: key
      integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.
      character(len=128) :: switch_message

      iresult = DFM_GENERICERROR

      if (allocated(solver_sequence)) then
         if (solver_period_index < size(solver_sequence)) then
            if (time0 + 1.0e-8_dp >= solver_sequence(solver_period_index + 1)%tstart) then
               solver_period_index = solver_period_index + 1
               flow_solver = solver_sequence(solver_period_index)%solver
               if (flow_solver == FLOW_SOLVER_FM) then
                  write(switch_message, '(a,i0,a)') 'Solver sequence: starting period ', solver_period_index, ' with generic1d2d3d.'
               else
                  write(switch_message, '(a,i0,a)') 'Solver sequence: starting period ', solver_period_index, ' with frozen1d2d.'
               end if
               call mess(LEVEL_INFO, trim(switch_message))
               if (len_trim(solver_sequence(solver_period_index)%restart_file) > 0) then
                  call load_sequence_restart(iresult)
                  if (iresult /= DFM_NOERR) return
               end if
            end if
         end if
      end if

!V: At this moment we are at time <t>. When using the regular solver (i.e., <flow_solver>=1),
!the time step is advanced in <flow_run_single_timestep>. This means that the boundary conditions
!(constructed when calling <flow_init_single_timestep>) are at time <t>.  When using the
!implicit 1D solver, the boundary conditions are expected at time <t+1>. Hence, we advance
!the time here such that the boundary conditions are at <t+1>. This could be done somewhere
!else in the code, e.g., <flow_initimestep>. I think that here it is clearer.
      if (flow_solver == FLOW_SOLVER_SRE) then
         !V: During model initialization, the time advances 1 s. This is very annoying when using
         !an implicit solver with fixed time step. Here we take it out considering the case in
         !which the time step is set to 1 s. This should be done in a better way (not sure how).
         if ((time0 == 1.0_dp) .and. (dts /= 1.0_dp)) then
            time0 = 0.0_dp
         end if
         time1 = time0 + dts
      end if

      call flow_init_single_timestep(iresult)
      if (iresult /= DFM_NOERR) then
         goto 888
      end if

      call flow_run_single_timestep(key, iresult)
      if (iresult /= DFM_NOERR .and. iresult /= DFM_TIMESETBACK) then
         goto 888
      end if

      call flow_finalize_single_timestep(iresult)
      if (iresult /= DFM_NOERR) then
         goto 888
      end if

      ! JRE avoid annoying dt_user interference
      ! This may induce slight inaccuracies when dts is relatively large
      call xbeach_write_stats(time1)
      call sedmor_write_stats(time1)
      if (jasedtrails > 0) then
         call sedtrails_write_stats(time1)
      end if
      iresult = DFM_NOERR
      return ! Return with success

888   continue
      ! Error
   end subroutine flow_single_timestep

   subroutine load_sequence_restart(iresult)
      use m_flow_flowinit, only: load_restart_file
      use m_flowparameters, only: solver_sequence, solver_period_index
      use m_flowtimes, only: restart_date_time, time0, time1, time_user
      use unstruc_model, only: md_restartfile
      use messagehandling, only: mess, LEVEL_INFO, LEVEL_ERROR
      use dfm_error, only: DFM_NOERR, DFM_GENERICERROR

      integer, intent(out) :: iresult
      character(len=len(md_restartfile)) :: original_file
      character(len=len(restart_date_time)) :: original_date_time
      real(kind=dp) :: saved_time0, saved_time1, saved_time_user
      logical :: file_exist

      original_file = md_restartfile
      original_date_time = restart_date_time
      saved_time0 = time0
      saved_time1 = time1
      saved_time_user = time_user
      md_restartfile = solver_sequence(solver_period_index)%restart_file
      restart_date_time = solver_sequence(solver_period_index)%restart_date_time
      call load_restart_file(file_exist, iresult)
      md_restartfile = original_file
      restart_date_time = original_date_time
      time0 = saved_time0
      time1 = saved_time1
      time_user = saved_time_user
      if (iresult /= DFM_NOERR .or. .not. file_exist) then
         call mess(LEVEL_ERROR, 'Solver sequence: failed to read restart file.')
         iresult = DFM_GENERICERROR
      else
         call mess(LEVEL_INFO, 'Solver sequence: read restart file '//trim(solver_sequence(solver_period_index)%restart_file))
      end if
   end subroutine load_sequence_restart

end module m_flow_single_timestep
