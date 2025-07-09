!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

module m_partfm
    use m_part17
    use m_part15
    use m_part08
    use m_fm_write_unc

    implicit none
contains

    subroutine partfm(lunpr)

        use m_fm_update
        use m_fm_read_hydro
        use m_fm_particles_in_grid
        use m_fm_handle_mesh
        use m_rdpart
        use m_waq_precision
        use MessageHandling
        use partmem
        use m_part_mesh
        use m_particles, only: NopartTot, Nrpart, trpart, xrpart, yrpart, zrpart, mrpart, irpart
        use part10fm_mod
        use fm_oildsp_mod
        use fm_vert_disp_mod
        use m_partfm_decay
        use alloc_mod
        use m_alloc
        use fileinfo, lun => lunit    ! logical unit numbers for files
        use m_part_times
        use timers
        use m_sferic_part, only: ptref
        use m_sferic, only: jsferic
        use geometry_module, only: Cart3Dtospher, sphertocart3D
        use physicalconsts, only: earth_radius
        use mathconsts, only: raddeg_hp, pi
        use random_generator

        implicit none

        integer(int_wp), intent(in) :: lunpr
        integer(int_wp) :: ierror, ntimes, ictimes, npload, ipartload
        integer(int_wp) :: lunmem, istat, itime, ilay, Ldot, ipart, iload, idye, iinsti, ndpart
        double precision :: dtcontp, totcload
        double precision :: rseed = 0.5d0
        double precision :: dpangle, dxp, dyp, dradius, xx, yy
        integer(4) ithndl              ! handle to time this subroutine
        logical :: mapfil  ! true if map file extension
        logical :: trkfil   ! true if track file extension
        data ithndl / 0 /
        if (timon) call timstrt("partfm", ithndl)

        call SetMessageHandling(lunMessages = lunpr)
        ! initialize the allocation system
        call init_alloc(lunmem, lunpr)

        if (hyd%num_layers /= 1) then
            write (lunpr, *) ' WARNING: 3D hydrodynamics is not yet supported for unstructured grids!'
            write (*, *) ' WARNING: 3D hydrodynamics is not yet supported for unstructured grids!'
        endif
        !dts   = real(hyd%cnv_step_sec, 8)  !idelt in seconds taken from the hyd file (conversion timestep)
        tzone = 0.0_dp
        refdat = hyd%HYD_REF(1:8)
        call setTUDUnitString()

        !
        ! For a model based on z-layers we need extra administration
        ! but that is not part of the current implementation yet
        !
        allocate(laytop(0, 0), laytopp(0, 0))

        !
        ! Read the grid information and the actual input file
        !
        call ini_part_grid(hyd)

        call rdpart (lun(1), lun(2), fname(1))

        dts = real(idelt, kind = kind(dts))
        noparttot = npmax + npmax / 100
        nopart = 0
        allocate(nplay(hyd%num_layers))

        !  part08 - calculates total released mass and mass/particle

        call part08 (lun(2), nodye, nocont, ictmax, amassd, &
                ictime, amassc, aconc, tmass, tmassc, &
                nosubs, ndprt, tmassu, ftime, linear, &
                substi, nmdyer, nmconr)


        !  calculate dump-sites in the grids

        call part06fm (lun(2), nodye, nocont, xwaste, &
                ywaste, zwaste, nwaste, mwaste)

        tstart_user = itstrtp
        tstop_user = itstopp
        Ldot = index(fname(1), '.', .true.) - 1
        filebase = ' '
        filebase = fname(1)(1:Ldot)


        !! AM NpartTot = npmax
        !! AM Nrpart = npmax !npmax is the number of particles released due to the instantaneous and continuous discharges

        call ini_part(partinifile, partrelfile, tstart_user, dts, 0)

        call realloc(xrpart, npmax)
        call realloc(yrpart, npmax)
        call realloc(zrpart, npmax)

        call realloc_particles(npmax, .true., ierror)
        irpart = 1
        ptref = 0.0D0

        if (notrak > 0) call unc_init_trk()
        call unc_init_map(hyd%crs, hyd%waqgeom, hyd%nosegl, noslay)

        time0 = tstart_user
        time1 = time0
        istat = -1 ! skip copying of data durin the first stime stap
        call part_readhydstep(hyd, itime, istat)

        if (istat == 99) then
            write (lunpr, *) ' Timing mismatch between input and actual data:', time0, itime
            write (*, *) ' Timing mismatch between input and actual data:', time0, itime
            goto 1234
        else if (istat /= 0) then
            write (lunpr, *) ' Error during reading of the hydrodynamic time step data', time0, itime
            write (*, *) ' Error during reading of the hydrodynamic time step data', time0, itime
            goto 1234
        end if

        do while (istat == 0)
            !     determine if map and track files must be produced

            mapfil = .true.
            trkfil = .true.
            if (notrak == 0) trkfil = .false.
            if (icwste                     < 1) mapfil = .false.
            if (itime                      < icwsta) mapfil = .false.
            if (itime - idelt              >=  icwsto) mapfil = .false.
            if (mod(itime - icwsta, icwste)  >=  idelt) mapfil = .false.

            if (trkfil) then
                if (mod(itime, notrak * idelt) >= idelt) then
                    trkfil = .false.
                endif
            endif
            if (trkfil) call unc_write_trk()
            if (mapfil) call unc_write_map()

            call report_progress(lunpr, int(time0), itstrtp, itstopp, nopart, npmax)

            if (time1 >= tstop_user) then
                exit
            endif
            time0 = time1
            time1 = min(tstop_user, time0 + dts)
            call part_readhydstep(hyd, itime, istat)
            if (idtset > 0)                                                    &
                    call part17 (itime, nosubs, idtset, idtime, decay, &
                            decays)
            call partfm_decay()

            !     interpolation for wind speed/direction in the wind table
            call part15 (lun(2), itime, spawnd, numcells, nowind, &
                    iwndtm, wveloa, wdira, wvelo, wdir)

            !     transport (advection, dispersion, winddrag)
            !      jsfer_old = jsferic
            !      jsferic = 0 ! everything in part10fm is in meters
            call update_part(itime)
            call part10fm()
            !      jsferic = jsfer_old ! back to what it should be
            call oildspfm(itime)
            call fm_vert_disp(lun(2), itime)
            !     interpolation for wind speed/direction in the wind table
        end do

        1234 continue

        call unc_close_trk()
        call unc_close_map()

        if (timon) call timstop (ithndl)

        return

    contains

        subroutine report_progress(lunpr, itime, itstrtp, itstopp, nopart, npmax)
            integer, intent(in) :: lunpr, itime, itstrtp, itstopp, nopart, npmax

            real :: pctprogress

            pctprogress = 100.0 * (real(itime, 4) - real(itstrtp, 4)) / (real(itstopp, 4) - real(itstrtp, 4)) ! percentage progress

            write (*, 1020) itime / 86400, mod(itime, 86400) / 3600, mod(itime, 3600) / 60, mod(itime, 60), &
                    itstopp / 86400, mod(itstopp, 86400) / 3600, mod(itstopp, 3600) / 60, mod(itstopp, 60), &
                    pctprogress, nopart, npmax
            write (lunpr, 1020) itime / 86400, mod(itime, 86400) / 3600, mod(itime, 3600) / 60, mod(itime, 60), &
                    itstopp / 86400, mod(itstopp, 86400) / 3600, mod(itstopp, 3600) / 60, mod(itstopp, 60), &
                    pctprogress, nopart, npmax

            1020 format(/, '  Time ', i6.4, 'D-', i2.2, 'H-', i2.2, 'M-', i2.2, 'S.', ' Stop time ', &
                    i6.4, 'D-', i2.2, 'H-', i2.2, 'M-', i2.2, 'S. (', f5.1, '% completed) ', &
                    i11, ' part. (of', i11, ')')

        end subroutine report_progress
    end subroutine partfm

end module m_partfm
