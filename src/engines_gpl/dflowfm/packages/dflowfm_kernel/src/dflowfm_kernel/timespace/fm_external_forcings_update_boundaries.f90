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

submodule(fm_external_forcings) fm_external_forcings_update_boundaries

   implicit none

contains

!> set boundary conditions
   module subroutine set_external_forcings_boundaries(time, iresult)
      use m_setzminmax, only: setzminmax
      use precision, only: dp
      use m_update_dambreak_breach, only: update_dambreak_breach
      use m_setsigmabnds, only: setsigmabnds
      use m_fm_thahbc
      use timers
      use m_flowtimes
      use m_flowgeom
      use m_flow
      use m_sferic
      use timespace
      use m_ship
      use m_observations, only: numobs, nummovobs, updateobservationxy
      use m_timer
      use m_partitioninfo
      use m_meteo
      use m_ec_parameters
      use dfm_error
      use m_sobekdfm
      use unstruc_channel_flow
      use m_oned_functions
      use m_obs_on_flowgeom, only: obs_on_flowgeom

      implicit none

      real(kind=dp), intent(in) :: time !< Current simulation time (s)
      integer, intent(out) :: iresult !< Integer error status

      integer :: i, n, k2, kb, L, itrac, isf
      real(kind=dp) :: dQ

      iresult = DFM_EXTFORCERROR
      call timstrt('External forcings boundaries', handle_extbnd)

      call setzminmax() ! our side of preparation for 3D ec module
      call setsigmabnds() ! our side of preparation for 3D ec module

      if (nzbnd > nqhbnd) then
         success = ec_gettimespacevalue(ecInstancePtr, item_waterlevelbnd, irefdate, tzone, tunit, time)
         if (.not. success) then
            goto 888
         end if
      end if

      if (nqhbnd > 0) then
         ! loop over nqhbnd (per pli)
         do i = 1, nqhbnd
            !    prepare qtot array
            atqh_all(i) = 0d0
            do n = L1qhbnd(i), L2qhbnd(i)
               kb = kbndz(1, n)
               k2 = kbndz(2, n)
               L = kbndz(3, n)
               if (jampi == 0) then
                  atqh_all(i) = atqh_all(i) - q1(L) ! flow link always directed inwards
               else
                  ! exclude ghost cells
                  if (idomain(k2) == my_rank) then
                     atqh_all(i) = atqh_all(i) - q1(L) ! flow link always directed inwards
                  end if
               end if
            end do
         end do

         ! do communication between domains
         if (jampi == 1) then
            if (jatimer == 1) call starttimer(IMPIREDUCE)
            call reduce_atqh_all()
            if (jatimer == 1) call stoptimer(IMPIREDUCE)
         end if

         ! First step calculate the water level, using the QH-relation for a outflowing discharge + dQ
         do i = 1, nqhbnd
            q_org(i) = atqh_all(i)
            atqh_all(i) = q_org(i) + max(min(0.001d0 * abs(q_org(i)), 1d0), 0.001d0)
         end do
         success = ec_gettimespacevalue(ecInstancePtr, item_qhbnd, irefdate, tzone, tunit, time)
         if (.not. success) then
            goto 888
         end if
         qhbndz_plus = qhbndz

         ! Second step calculate the water level, using the QH-relation for a outflowing discharge - dQ
         do i = 1, nqhbnd
            atqh_all(i) = q_org(i) - max(min(0.001d0 * abs(q_org(i)), 1d0), 0.001d0)
         end do
         success = ec_gettimespacevalue(ecInstancePtr, item_qhbnd, irefdate, tzone, tunit, time)
         if (.not. success) then
            goto 888
         end if
         qhbndz_min = qhbndz

         ! Step 3 now estimate the slope of the QH-relation at the given discharge
         do i = 1, nqhbnd
            dQ = max(min(0.001d0 * abs(q_org(i)), 1d0), 0.001d0)
            if (comparereal(qhbndz_plus(i), qhbndz_min(i)) == 0) then
               qh_gamma(i) = 0d0
            else
               qh_gamma(i) = 2 * dQ / (qhbndz_plus(i) - qhbndz_min(i))
            end if
            atqh_all(i) = q_org(i)
         end do

         success = ec_gettimespacevalue(ecInstancePtr, item_qhbnd, irefdate, tzone, tunit, time)
         if (.not. success) then
            goto 888
         end if

         ! vind bijbehorende zbndz punten
         do i = 1, nqhbnd
            do n = L1qhbnd(i), L2qhbnd(i)
               zbndz(n) = qhbndz(i)
            end do
         end do
      end if

      if (item_velocitybnd /= ec_undef_int) then
         success = ec_gettimespacevalue(ecInstancePtr, item_velocitybnd, irefdate, tzone, tunit, time)
         if (.not. success) then
            goto 888
         end if
      end if

      if (item_dischargebnd /= ec_undef_int) then
         success = ec_gettimespacevalue(ecInstancePtr, item_dischargebnd, irefdate, tzone, tunit, time)
         if (.not. success) then
            goto 888
         end if
      end if

      if (nbnds > 0) then
         success = ec_gettimespacevalue(ecInstancePtr, item_salinitybnd, irefdate, tzone, tunit, time)
         if (.not. success) then
            goto 888
         end if
      end if

      if (nbndTM > 0) then
         success = ec_gettimespacevalue(ecInstancePtr, item_temperaturebnd, irefdate, tzone, tunit, time)
         if (.not. success) then
            goto 888
         end if
      end if

      if (nbndsd > 0) then
         success = ec_gettimespacevalue(ecInstancePtr, item_sedimentbnd, irefdate, tzone, tunit, time)
         if (.not. success) then
            goto 888
         end if
      end if

      do itrac = 1, numtracers
         if (nbndtr(itrac) > 0) then
            success = ec_gettimespacevalue(ecInstancePtr, item_tracerbnd(itrac), irefdate, tzone, tunit, time)
            if (.not. success) then
               goto 888
            end if
         end if
      end do

      if (stm_included) then
         do isf = 1, numfracs ! numfracs okay, is number of fractions with bc
            if (nbndsf(isf) > 0) then
               success = ec_gettimespacevalue(ecInstancePtr, item_sedfracbnd(isf), irefdate, tzone, tunit, time)
               if (.not. success) then
                  goto 888
               end if
            end if
         end do
      end if

      if (nbndt > 0) then
         success = ec_gettimespacevalue(ecInstancePtr, item_tangentialvelocitybnd, irefdate, tzone, tunit, time)
         if (.not. success) then
            goto 888
         end if
      end if

      if (nbnduxy > 0) then
         success = ec_gettimespacevalue(ecInstancePtr, item_uxuyadvectionvelocitybnd, irefdate, tzone, tunit, time)
         if (.not. success) then
            goto 888
         end if
      end if

      if (nbndn > 0) then
         success = ec_gettimespacevalue(ecInstancePtr, item_normalvelocitybnd, irefdate, tzone, tunit, time)
         if (.not. success) then
            goto 888
         end if
      end if

      if (nbnd1d2d > 0) then
         ! NOTE: no gettimespacevalue is needed here: zbnd1d2d should be filled via BMI (forcing is REALTIME by coupler program)
      end if

      if (nshiptxy > 0) then
         success = ec_gettimespacevalue(ecInstancePtr, item_shiptxy, irefdate, tzone, tunit, time)
         if (.not. success) then
            goto 888
         end if
      end if

      if (nummovobs > 0) then
         success = ec_gettimespacevalue(ecInstancePtr, item_movingstationtxy, irefdate, tzone, tunit, time)
         if (success) then
            do i = 1, nummovobs
               call updateObservationXY(numobs + i, xyobs(2 * (i - 1) + 1), xyobs(2 * (i - 1) + 2))
            end do
            call obs_on_flowgeom(1)
         else
            goto 888
         end if
      end if

      if (allocated(threttim)) then
         call fm_thahbc()
      end if

      if (ngatesg > 0) then
         success = success .and. ec_gettimespacevalue(ecInstancePtr, item_gateloweredgelevel, irefdate, tzone, tunit, time, zgate)
      end if

      !dambreak
      if (ndambreaksignals > 0) then
         ! Variable ndambreaksignals is >0 for all partitions if there is a dambreak, even if it is outside of a partition.
         ! In a parallel simulation, we need to call this subroutine even in a special situation that there is no dambreak
         ! on the current subdomain (i.e. ndambreaklinks == 0), because this subroutine calls function
         ! getAverageQuantityFromLinks, which involves mpi communication among all subdomains. However, in this special situation,
         ! all the necessary variables will be set to 0 and will not participate the dambreak related computation in this subroutine.
         call update_dambreak_breach(time, dts)
      end if

      if (network%rgs%timeseries_defined) then
         if (times_update_roughness(2) == tstart_user) then
            ! First time: the roughness values for tstart are not set yet
            success = success .and. ec_gettimespacevalue(ecInstancePtr, item_frcutim, irefdate, tzone, tunit, times_update_roughness(1))
            call reCalculateConveyanceTables(network)
         end if
         if (time >= times_update_roughness(2)) then
            ! Shift the time dependent roughness values and collect the values for the new time instance
            times_update_roughness(1) = times_update_roughness(2)
            times_update_roughness(2) = times_update_roughness(2) + dt_UpdateRoughness ! (e.g., 86400 s)
            call shiftTimeDependentRoughnessValues(network%rgs)
            ! The next gettimespace call will automatically read and fill new values in prgh%timeDepValues(:,2).
            success = success .and. ec_gettimespacevalue(ecInstancePtr, item_frcutim, irefdate, tzone, tunit, times_update_roughness(2))
            ! update conveyance tables
            call reCalculateConveyanceTables(network)
         end if
         call interpolateRoughnessParameters(network%rgs, times_update_roughness, time)
      end if

      iresult = DFM_NOERR

      goto 999 ! Return with success

      ! Error handling:
888   continue
      msgbuf = dumpECMessageStack(LEVEL_WARN, callback_msg)
      iresult = DFM_EXTFORCERROR
      write (msgbuf, '(a,f13.3)') 'Error while updating boundary forcing at time=', time
      call mess(LEVEL_WARN, trim(msgbuf))

999   continue
      call timstop(handle_extbnd)
      return

   end subroutine set_external_forcings_boundaries

end submodule fm_external_forcings_update_boundaries
