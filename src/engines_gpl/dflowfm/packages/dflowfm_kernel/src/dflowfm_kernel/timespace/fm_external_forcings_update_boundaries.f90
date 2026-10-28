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

module fm_external_forcings_update_boundaries

   implicit none(type, external)

   private

   public :: set_external_forcings_boundaries
   public :: addtimespacerelation_boundaries

contains

   !> set boundary conditions
   subroutine set_external_forcings_boundaries(time, iresult)
      use m_setzminmax, only: setzminmax
      use precision, only: dp
      use m_dambreak_breach, only: update_dambreak_breach
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
      use unstruc_messages, only: callback_msg

      ! Arguments
      real(kind=dp), intent(in) :: time !< Current simulation time (s)
      integer, intent(out) :: iresult !< Integer error status

      ! Local variables
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
            atqh_all(i) = 0.0_dp
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
            if (jatimer == 1) then
               call starttimer(IMPIREDUCE)
            end if
            call reduce_atqh_all()
            if (jatimer == 1) then
               call stoptimer(IMPIREDUCE)
            end if
         end if

         ! First step calculate the water level, using the QH-relation for a outflowing discharge + dQ
         do i = 1, nqhbnd
            q_org(i) = atqh_all(i)
            atqh_all(i) = q_org(i) + max(min(0.001_dp * abs(q_org(i)), 1.0_dp), 0.001_dp)
         end do
         success = ec_gettimespacevalue(ecInstancePtr, item_qhbnd, irefdate, tzone, tunit, time)
         if (.not. success) then
            goto 888
         end if
         qhbndz_plus = qhbndz

         ! Second step calculate the water level, using the QH-relation for a outflowing discharge - dQ
         do i = 1, nqhbnd
            atqh_all(i) = q_org(i) - max(min(0.001_dp * abs(q_org(i)), 1.0_dp), 0.001_dp)
         end do
         success = ec_gettimespacevalue(ecInstancePtr, item_qhbnd, irefdate, tzone, tunit, time)
         if (.not. success) then
            goto 888
         end if
         qhbndz_min = qhbndz

         ! Step 3 now estimate the slope of the QH-relation at the given discharge
         do i = 1, nqhbnd
            dQ = max(min(0.001_dp * abs(q_org(i)), 1.0_dp), 0.001_dp)
            if (comparereal(qhbndz_plus(i), qhbndz_min(i)) == 0) then
               qh_gamma(i) = 0.0_dp
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

      if (update_dambreak_breach(time, dts) /= 0) then
         success = .false.
         goto 888
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
            times_update_roughness(2) = times_update_roughness(2) + dt_update_roughness ! (e.g., 86400 s)
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
      msgbuf = dump_ec_message_stack(LEVEL_WARN, callback_msg)
      iresult = DFM_EXTFORCERROR
      write (msgbuf, '(a,f13.3)') 'Error while updating boundary forcing at time=', time
      call mess(LEVEL_WARN, trim(msgbuf))

999   continue
      call timstop(handle_extbnd)
      return

   end subroutine set_external_forcings_boundaries

   !> Calls the ec_addtimespacerelation with all proper unstruc-specific target arrays and element set masks.
   function addtimespacerelation_boundaries(qid, filename, filetype, method, operand, forcing_file, targetindex) result(success)
      use fm_external_forcings_data, no1 => qid, no2 => filetype, no3 => operand, no4 => success
      use fm_external_forcings_utils, only: check_keyword_zerozbndinflowadvection, get_tracername, get_sedfracname
      use m_meteo, no5 => qid, no6 => filetype, no7 => operand, no8 => success
      use m_flowparameters, only: jawave
      use m_flowtimes, only: dt_nodal
      use m_qnerror
      use m_find_name, only: find_name
      use messagehandling, only: LEVEL_WARN, mess

      ! Arguments
      character(len=*), intent(inout) :: qid !< Identifier of current quantity (i.e., 'waterlevelbnd')
      character(len=*), intent(in) :: filename !< Name of data file for current quantity.
      integer, intent(in) :: filetype !< File type of current quantity.
      integer, intent(in) :: method !< Time-interpolation method for current quantity.
      integer, intent(in) :: operand !< Operand w.r.t. previous data
      character(len=*), optional, intent(in) :: forcing_file !< Optional forcings file, if it differs from the filename (i.e., if filename=*.pli, and forcing_file=*.bc)
      integer, optional, intent(in) :: targetIndex !< target position or rank of (complete!) vector in target array

      ! Local variables
      logical :: success
      character(len=256) :: tracnam, sfnam, qidnam
      integer :: itrac, isf
      real(kind=dp), dimension(:), pointer :: pzmin, pzmax

      success = .true. ! initialization

      ! Special forcingsfile: if name equals 'REALTIME', do not do an ec_addtimespacerelation, just leave it to the external caller to fill zbnd* target value array.
      ! TODO: AVD: we now leave it to caller to fill array with length(zbnd*),
      ! instead of the number of polyline points. Cleaner alternative is to create
      ! a poly_tim provider, with the *underlying* point child providers being REALTIME.
      if (present(forcing_file)) then
         if (trim(forcing_file) == 'REALTIME') then
            call mess(LEVEL_DEBUG, 'addtimespacerelation_boundaries: leave empty timespacerelation for '''//trim(qid)//''' from locationFile '''//trim(filename)//''' (REALTIME data).')
            return
         end if
      end if

      kx = 1
      if (nbndz > 0 .and. (qid == 'waterlevelbnd' .or. qid == 'neumannbnd' .or. qid == 'riemannbnd' .or. qid == 'outflowbnd')) then
         success = ec_addtimespacerelation(qid, xbndz, ybndz, kdz, kx, filename, filetype, method, operand, xy2bndz, forcingfile=forcing_file, dtnodal=dt_nodal, targetindex=targetindex)

      else if (nbndz > 0 .and. nqhbnd > 0 .and. (qid == 'qhbnd')) then
         success = ec_addtimespacerelation(qid, xbndz, ybndz, kdz, kx, filename, filetype, method, operand, xy2bndz, forcingfile=forcing_file, targetindex=targetindex)

      else if (nbndu > 0 .and. (qid == 'dischargebnd' .or. qid == 'criticaloutflowbnd' .or. qid == 'weiroutflowbnd' .or. qid == 'absgenbnd')) then
         if (qid == 'absgenbnd') then
            jawave = 4
         end if
         success = ec_addtimespacerelation(qid, xbndu, ybndu, kdu, kx, filename, filetype, method, operand, xy2bndu, forcingfile=forcing_file, targetindex=targetindex)

      else if (nbndu > 0 .and. qid == 'velocitybnd') then
         pzmin => zminmaxu(1:nbndu)
         pzmax => zminmaxu(nbndu + 1:2 * nbndu)
         success = ec_addtimespacerelation(qid, xbndu, ybndu, kdu, kx, filename, filetype, method, operand, &
                                           xy2bndu, z=sigmabndu, pzmin=pzmin, pzmax=pzmax, forcingfile=forcing_file, targetindex=targetindex)

      else if (nbnds > 0 .and. qid == 'salinitybnd') then ! 2D
         pzmin => zminmaxs(1:nbnds)
         pzmax => zminmaxs(nbnds + 1:2 * nbnds)
         success = ec_addtimespacerelation(qid, xbnds, ybnds, kds, kx, filename, filetype, method, operand, xy2bnds, &
                                           z=sigmabnds, pzmin=pzmin, pzmax=pzmax, forcingfile=forcing_file, targetindex=targetindex)

      else if (nbndTM > 0 .and. qid == 'temperaturebnd') then
         pzmin => zminmaxtm(1:nbndTM)
         pzmax => zminmaxtm(nbndTM + 1:2 * nbndTM)
         success = ec_addtimespacerelation(qid, xbndTM, ybndTM, kdtm, kx, filename, filetype, method, operand, xy2bndtm, &
                                           z=sigmabndtm, pzmin=pzmin, pzmax=pzmax, forcingfile=forcing_file, targetindex=targetindex)

      else if (nbndsd > 0 .and. (qid == 'sedimentbnd')) then
         pzmin => zminmaxsd(1:nbndsd)
         pzmax => zminmaxsd(nbndsd + 1:2 * nbndsd)
         success = ec_addtimespacerelation(qid, xbndsd, ybndsd, kdsd, kx, filename, filetype, method, operand, xy2bndsd, &
                                           z=sigmabndsd, pzmin=pzmin, pzmax=pzmax, forcingfile=forcing_file, targetindex=targetindex)

      else if (numtracers > 0 .and. istarts_with(qid, 'tracerbnd')) then
         ! get tracer boundary condition number
         call get_tracername(qid, tracnam, qidnam)
         itrac = find_name(trnames, tracnam)

         ! for parallel runs, we always need to add the tracer, even if this subdomain has no tracer boundary conditions defined
         ! call add_tracer(tracnam, iconst)
         ! update: all tracers are counted first and allocated later

         if (nbndtr(itrac) > 0) then
            pzmin => bndtr(itrac)%zminmax(1:nbndtr(itrac))
            pzmax => bndtr(itrac)%zminmax(nbndtr(itrac) + 1:2 * nbndtr(itrac))
            success = ec_addtimespacerelation(qid, bndtr(itrac)%x, bndtr(itrac)%y, bndtr(itrac)%kd, kx, filename, filetype, method, operand, bndtr(itrac)%xy2, &
                                              z=bndtr(itrac)%sigma, forcingfile=forcing_file, pzmin=pzmin, pzmax=pzmax, targetindex=targetindex)
         else
            success = .true.
         end if

      else if (numfracs > 0 .and. istarts_with(qid, 'sedfracbnd') .and. stm_included) then

         call get_sedfracname(qid, sfnam, qidnam)
         isf = find_name(sfnames, sfnam)

         if (isf > 0) then
            if (nbndsf(isf) > 0) then
               pzmin => bndsf(isf)%zminmax(1:nbndsf(isf))
               pzmax => bndsf(isf)%zminmax(nbndsf(isf) + 1:2 * nbndsf(isf))
               success = ec_addtimespacerelation(qid, bndsf(isf)%x, bndsf(isf)%y, bndsf(isf)%kd, kx, filename, filetype, method, operand, bndsf(isf)%xy2, &
                                                 z=bndsf(isf)%sigma, forcingfile=forcing_file, pzmin=pzmin, pzmax=pzmax, targetindex=targetindex)
            else
               success = .true.
            end if
         else
            call mess(LEVEL_WARN, 'Initializing boundary block for file '''//trim(filename)//''', getting unknown sediment fraction '''//trim(sfnam)//''' from QUANTITY '''//trim(qid)//'''.')
            call qnerror('Initializing boundary block for file '''//trim(filename)//''', getting unknown sediment fraction '''//trim(sfnam)//''' from QUANTITY '''//trim(qid)//'''.', ' ', ' ')
         end if

      else if (nbndt > 0 .and. (qid == 'tangentialvelocitybnd')) then
         success = ec_addtimespacerelation(qid, xbndt, ybndt, kdt, kx, filename, filetype, method, operand, xy2bndt, forcingfile=forcing_file, targetindex=targetindex)

      else if (nbnduxy > 0 .and. (qid == 'uxuyadvectionvelocitybnd')) then
         kx = 2
         pzmin => zminmaxuxy(1:nbnduxy)
         pzmax => zminmaxuxy(nbnduxy + 1:2 * nbnduxy)
         success = ec_addtimespacerelation(qid, xbnduxy, ybnduxy, kduxy, kx, filename, filetype, method, operand, xy2bnduxy, &
                                           z=sigmabnduxy, pzmin=pzmin, pzmax=pzmax, forcingfile=forcing_file)
         success = success .and. check_keyword_zerozbndinflowadvection()

      else if (nbndn > 0 .and. (qid == 'normalvelocitybnd')) then
         success = ec_addtimespacerelation(qid, xbndn, ybndn, kdn, kx, filename, filetype, method, operand, xy2bndn, forcingfile=forcing_file, targetindex=targetindex)

      else ! There is some boundary that is not detected or recognized
      end if
   end function addtimespacerelation_boundaries

end module fm_external_forcings_update_boundaries
