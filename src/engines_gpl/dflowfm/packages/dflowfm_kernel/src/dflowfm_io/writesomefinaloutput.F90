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

 subroutine writesomefinaloutput()
    use m_sferic
    use timers
    use unstruc_model
    use m_flow
    use m_flowgeom
    use m_flowtimes
    use unstruc_messages
    use m_timer
    use m_netw
    use m_partitioninfo
    use m_monitoring_crosssections
    use m_observations, only: mxls
    use unstruc_files, only: defaultFilename
    use m_sediment, only: stm_included
    use m_transport, only: maserrsed
    use mass_balance_areas_routines, only: mba_final
    use m_datum
#ifdef _OPENMP
    use omp_lib
#endif

    implicit none

    integer :: k, i
    double precision :: frac, tot, dtav
    double precision :: f
    double precision :: tstop
    double precision :: tcpustep
    double precision :: time_cpu
    double precision :: tcpusol
    double precision :: totalcomp
    double precision :: timeloop

    if (ndx == 0) then
       write (msgbuf, '(a)') 'Empty model, no flow cells found. No statistics to report.'; call msg_flush()
       return
    end if

    tcpustep = tim_get_wallclock(handle_steps)
    tcpusol = tim_get_wallclock(handle_sol)

    frac = tcpusol / max(1d-10, tcpustep)
    call timstop(handle_all)

    tot = tim_get_wallclock(handle_all) / max(1d0, ndx * (dnt - 1))
    dtav = (time1 - tstart_user) / max(1d0, dnt)

    do k = 1, 3
       msgbuf = ' '; call msg_flush()
    end do

    do i = 1, size(handle_extra)
       if (handle_extra(i) > 0) then
          time_cpu = tim_get_wallclock(handle_extra(i))
          if (time_cpu > 0.01d0) then ! only the relevant
             write (msgbuf, '(a,a,F25.10)') 'extra timer:', tim_get_label(handle_extra(i)), time_cpu; call msg_flush()
          end if
       end if
    end do

! use current time instead of tstop_user in statistics
    tstop = time1
    if (tstop /= tstop_user) then
       write (msgbuf, '(a,I25)') 'Simulation did not reach stop time'; call msg_flush()
    end if
    msgbuf = ' '; call msg_flush()
    msgbuf = ' '; call msg_flush()
    msgbuf = ' '; call msg_flush()

    write (msgbuf, '(a,F25.10)') 'nr of timesteps        ( )  :', dnt - 1; call msg_flush()
    write (msgbuf, '(a,F25.10)') 'average timestep       (s)  :', dtav; call msg_flush()
    write (msgbuf, '(a,F25.10)') 'nr of setbacks         ( )  :', dsetb; call msg_flush()

    msgbuf = ' '; call msg_flush()
    msgbuf = ' '; call msg_flush()

    f = 24d0 * 3600d0
    totalcomp = tim_get_wallclock(handle_all)
    timeloop = tim_get_wallclock_inc(handle_all)
    write (msgbuf, '(a,F25.10)') 'simulation period      (d)  :', (tstop - tstart_user) / f; call msg_flush()
    write (msgbuf, '(a,F25.10)') 'total computation time (d)  :', (totalcomp) / f; call msg_flush()
    write (msgbuf, '(a,F25.10)') 'time modelinit         (d)  :', (totalcomp - timeloop) / f; call msg_flush()
    write (msgbuf, '(a,F25.10)') 'time steps (+ plots)   (d)  :', (timeloop) / f; call msg_flush()

    msgbuf = ' '; call msg_flush()

    f = 3600d0
    write (msgbuf, '(a,F25.10)') 'simulation period      (h)  :', (tstop - tstart_user) / f; call msg_flush()
    write (msgbuf, '(a,F25.10)') 'total computation time (h)  :', (totalcomp) / f; call msg_flush()
    write (msgbuf, '(a,F25.10)') 'time modelinit         (h)  :', (totalcomp - timeloop) / f; call msg_flush()
    write (msgbuf, '(a,F25.10)') 'time steps (+ plots)   (h)  :', (timeloop) / f; call msg_flush()

    msgbuf = ' '; call msg_flush()

    write (msgbuf, '(a,F25.10)') 'simulation period      (s)  :', tstop - tstart_user; call msg_flush()
    write (msgbuf, '(a,F25.10)') 'total computation time (s)  :', (totalcomp); call msg_flush()
    write (msgbuf, '(a,F25.10)') 'time modelinit         (s)  :', (totalcomp - timeloop); call msg_flush()
    write (msgbuf, '(a,F25.10)') 'time steps (+ plots)   (s)  :', (timeloop); call msg_flush()

    msgbuf = ' '; call msg_flush()
    msgbuf = ' '; call msg_flush()

    write (msgbuf, '(a,F25.10)') 'time iniexternalforc.  (s)  :', tim_get_wallclock(handle_iniext); call msg_flush()

    msgbuf = ' '; call msg_flush()

    write (msgbuf, '(a,F25.10)') 'time inistep           (s)  :', tim_get_wallclock(handle_inistep); call msg_flush()
    write (msgbuf, '(a,F25.10)') 'time setumod           (s)  :', tim_get_wallclock(handle_umod); call msg_flush()
    write (msgbuf, '(a,F25.10)') 'time furu              (s)  :', tim_get_wallclock(handle_furu); call msg_flush()
    write (msgbuf, '(a,F25.10)') 'time solve             (s)  :', tim_get_wallclock(handle_sol); call msg_flush()

    write (msgbuf, '(a,F25.10)') 'time gausselimination  (s)  :', t(3, igaussel); call msg_flush()
    write (msgbuf, '(a,F25.10)') 'time gausssubstitution (s)  :', t(3, igausssu); call msg_flush()
    write (msgbuf, '(a,F25.10)') 'time totalsolve        (s)  :', t(3, itotalsol); call msg_flush()

    write (msgbuf, '(a,F25.10)') 'time setexternalforc.  (s)  :', tim_get_wallclock(handle_ext); call msg_flush()
    write (msgbuf, '(a,F25.10)') 'time setext.forc.fetch (s)  :', tim_get_wallclock(handle_fetch); call msg_flush()
    write (msgbuf, '(a,F25.10)') 'time setexternalfbnd.  (s)  :', tim_get_wallclock(handle_extbnd); call msg_flush()
    write (msgbuf, '(a,F25.10)') 'time steps             (s)  :', tim_get_wallclock(handle_steps); call msg_flush()
    write (msgbuf, '(a,F25.10)') 'fraction solve/steps   ( )  :', frac; call msg_flush()
    write (msgbuf, '(a,F25.10)') 'total/(dnt*ndx)        (s)  :', tot; call msg_flush()
    write (msgbuf, '(a,F25.10)') 'av nr of cont. it s1it ( )  :', dnums1it / max(dnt, 1d-8); call msg_flush()

    if (jatimer == 1) then
       write (msgbuf, '(a,F25.10)') 'time transport         (s)  :', gettimer(1, ITRANSPORT)
       call msg_flush()
       if (ti_waqproc /= 0) then
          write (msgbuf, '(a,F25.10)') 'time processes         (s)  :', gettimer(1, IFMWAQ)
          call msg_flush()
       end if
       if (idebug > 0) then
          write (msgbuf, '(a,F25.10)') 'time debug            (s)  :', gettimer(1, IDEBUG)
          call msg_flush()
       end if
       if (jafilter > 0) then
          write (msgbuf, '(a,F25.10)') 'time filter coeff.     (s)  :', gettimer(1, IFILT_COEF)
          call msg_flush()
          write (msgbuf, '(a,F25.10)') 'time filter solve      (s)  :', gettimer(1, IFILT_SOLV)
          call msg_flush()
          write (msgbuf, '(a,F25.10)') 'time filter cnstr. mat.(s)  :', gettimer(1, IFILT_MAT)
          call msg_flush()
          write (msgbuf, '(a,F25.10)') 'time filter copy back  (s)  :', gettimer(1, IFILT_COPYBACK)
          call msg_flush()
          write (msgbuf, '(a,F25.10)') 'time filter other      (s)  :', gettimer(1, IFILT_OTHER)
          call msg_flush()
          write (msgbuf, '(a,F25.10)') 'time filter            (s)  :', gettimer(1, IFILT)
          call msg_flush()
       end if
       if (jased > 0 .and. stm_included) then
          write (msgbuf, '(a,F25.10)') 'time erosed            (s)  :', gettimer(1, IEROSED)
          call msg_flush()
          write (msgbuf, '(a,F25.3)') 'mass error from ssc limitation (10^6 kg)  :', maserrsed / 1d6
          call msg_flush()
       end if
    end if

    if (number_steps_limited_visc_flux_links > 0) then
       msgbuf = ' '; call msg_flush()
       write (msgbuf, '(a)') 'Viscosity coefficient/Horizontal transport flux were limited on some links in the course of computation.'
       call msg_flush()
    end if

    do k = 1, 3
       msgbuf = ' '; call msg_flush()
    end do

    write (msgbuf, '(a,a)') 'Computation started  at: ', rundat0; call msg_flush()
    call datum(rundat0)
    write (msgbuf, '(a,a)') 'Computation finished at: ', rundat0; call msg_flush()
    msgbuf = ' '; call msg_flush()

    write (msgbuf, '(a,F25.10)') 'simulation period      (h)  :', (tstop - tstart_user) / 3600d0; call msg_flush()
    write (msgbuf, '(a,F25.10)') 'total time in timeloop (h)  :', (timeloop) / 3600d0; call msg_flush()

#ifdef HAVE_MPI
    if (jampi == 1) then
       write (msgbuf, '(a,i0,a,i0)') 'MPI    : yes.         #processes   : ', numranks, ', my_rank: ', my_rank; call msg_flush()
    else
       write (msgbuf, '(a)') 'MPI    : no.'; call msg_flush()
    end if
#else
    write (msgbuf, '(a)') 'MPI    : unavailable.'; call msg_flush()
#endif

#ifdef _OPENMP
    write (msgbuf, '(a,i0)') 'OpenMP : yes.         #threads max : ', omp_get_max_threads(); call msg_flush()
#else
    write (msgbuf, '(a)') 'OpenMP : unavailable.'; call msg_flush()
#endif

    do k = 1, 3
       msgbuf = ' '; call msg_flush()
    end do

    ! if (ti_xls > 0) then
    ! call wrirstfileold(time1)                     ! schrijf aan het einde     een ascii.rst-file weg
    ! call wrinumlimdt()                                 ! number of limitating timesteps per node
    ! endif
    !call unc_write_his(time1)                         ! schrijf aan het einde ook een .his-file weg
    !call wrimap(time1)                                ! schrijf aan het einde ook een .map-file weg

!   call mba_final(time_user)
    if (ti_mba > 0) then
       call mba_final(time_user)
    end if

    msgbuf = ' '; call msg_flush()

    if (mxls /= 0 .and. ncrs > 0) then
       write (msgbuf, '(a)') 'crosssection discharges (m3/s) : '; call msg_flush()
       do i = 1, ncrs
          write (msgbuf, '(F14.3)') crs(i)%sumvalcur(1); call msg_flush()
       end do
       write (msgbuf, '(a)') 'crosssection areas (m2) : '; call msg_flush()
       do i = 1, ncrs
          write (msgbuf, '(F14.3)') crs(i)%sumvalcur(2); call msg_flush()
       end do
    end if

    if (jawriteDetailedTimers > 0) then
       call timdump(trim(defaultFilename('timers')), .true.)
    end if

    call timstrt('All', handle_all)
 end subroutine writesomefinaloutput
