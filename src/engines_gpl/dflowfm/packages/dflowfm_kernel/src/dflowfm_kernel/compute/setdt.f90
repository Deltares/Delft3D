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

module m_setdt
   use m_setdtorg, only: setdtorg
   use m_timestepanalysis, only: timestepanalysis
   use m_xbeachwaves, only: xbeach_wave_maxtimestep
   use m_fm_mor_maxtimestep, only: fm_mor_maxtimestep
   use m_setdtmaxavalan, only: setdtmaxavalan

   implicit none

   private

   public :: setdt

contains

   subroutine setdt()
      use precision, only: dp
      use m_tekcflmx
      use m_partitioninfo
      use m_flowparameters, only: jawave, flow_solver, FLOW_SOLVER_SRE
      use m_xbeach_data, only: swave, instat
      use m_flowtimes
      use m_flow, only: kkcflmx
      use m_timer
      use m_gui
      use m_sediment, only: jased, stm_included, stmpar, jamorcfl, jamormergedtuser
      use m_fm_erosed, only: duneavalan
      use m_mormerge
      implicit none

      real(kind=dp) :: dtsc_loc
      integer :: nsteps
      integer :: jareduced

!  compute CFL-based maximum time step and limiting flownode/time step, per subomdain
      call setdtorg(jareduced) ! 7.1 2031

      ! morphological timestep reduction
      if (stm_included .and. jamorcfl > 0) then
         if (time1 > tstart_user + stmpar%morpar%tmor * tfac) then
            call fm_mor_maxtimestep()
         end if
      end if

      if (jawave == 4 .and. swave == 1) then
         if (.not. (trim(instat) == 'stat' .or. trim(instat) == 'stat_table')) then
            call xbeach_wave_maxtimestep()
         end if
      end if

      if (jased == 4 .and. stm_included) then
         if (duneavalan) then
            call setdtmaxavalan(dts)
         end if
      end if

      dtsc_loc = dtsc

      !  globally reduce time step
      if (jampi == 1 .and. jareduced == 0) then
         !     globally reduce dts (dtsc may now be larger)
         if (jatimer == 1) call starttimer(IMPIREDUCE)
         call reduce_double_min(dts)
         if (jatimer == 1) call stoptimer(IMPIREDUCE)
      end if

      dtsc = dts

!  account for user time step
      if (ja_timestep_auto >= 1) then
         if (dts > dt_fac_max * dtprev) then
            dts = dt_fac_max * dtprev
            nsteps = ceiling((time_user - time0) / dts)
            ! New timestep dts would be rounded down to same dtprev (undesired, so use nsteps-1)
            if (1000 * dtprev > time_user - time0) then
               nsteps = ceiling((time_user - time0) / dts)
               if (nsteps == ceiling((time_user - time0) / dtprev)) then
                  nsteps = max(1, nsteps - 1)
               end if
               dts = (time_user - time0) / dble(nsteps)
               ! dtmax is always leading.
               if (dts > dt_max .or. dts > dtsc) then ! Fall back to smaller step anyway.
                  dts = (time_user - time0) / dble(nsteps + 1)
               end if
            end if

         else
            ! dts = min (dts, dt_max) ! hk: commented out, already done this 15 lines above

            ! Fit timestep dts so that we will exactly reach time_user in future steps.
            !if ( time0+dts.ge.time_user ) then
            !   dts = min(dts, time_user-time0)
            !else
            ! NOTE: when the model has an extremely small timestep, nsteps gets an integer overflow,
            ! then becomes negative, so the max below sets nsteps=1, violating the dtmax requirement. (UNST-1926)
            nsteps = max(1, ceiling((time_user - time0) / dts))
            dts = (time_user - time0) / dble(nsteps)
            !end if
         end if
      else
         dts = dt_max
         dtsc = 0d0 ! SPvdP: safety, was undefined but could be used later
         kkcflmx = 0 ! SPvdP: safety, was undefined but could be used later
      end if

      if (stm_included .and. jased > 0) then
         if (stmpar%morpar%multi .and. jamormergedtuser == 0) then
            if (jampi == 0) then
               call put_get_time_step(stmpar%morpar%mergehandle, dts)
            else
               call reduce_double_min(dts)
               if (my_rank == 0) then
                  call put_get_time_step(stmpar%morpar%mergehandle, dts)
               end if
               call reduce_double_min(dts)
            end if
         end if
      end if

      if (flow_solver == FLOW_SOLVER_SRE) then
         dts = dt_max
      end if

      call timestepanalysis(dtsc_loc)

      dti = 1d0 / dts

      if (jaGUI == 1) then
         call tekcflmx()
      end if

   end subroutine setdt

end module m_setdt
