!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

!> main steering module for the 3d discrete particle model.
module m_delpar

    use m_wrttrk
    use m_plotgr
    use m_partzp
    use m_partvs
    use m_part17
    use m_part15
    use m_part08
    use m_part06
    use m_part01
    use m_inipart_asc
    use m_inipart

    implicit none

contains

    !> main subroutine for the 3d discrete particle model.
    subroutine delpar(ifnam)
        !
        !  module declarations
        !
        use m_stop_exit
        use m_part11
        use m_report_date_time
        use m_rdpart
        use m_rdlgri
        use m_rdfnam
        use m_rdccol
        use m_getdps
        use m_logger_helper
        use m_waq_precision                  ! single/double precision
        use timers
        use fileinfo, lun => lunit    ! logical unit numbers for files
        use spec_feat_par
        use normal_mod
        use m_partfm
        !
        !  module procedure(s)
        !
        use openfl_mod
        use delete_file_mod            ! explicit interface
        use oildsp_mod                 ! explicit interface
        use part03_mod                 ! explicit interface
        use part09_mod                 ! explicit interface
        use part10_mod                 ! explicit interface
        use grid_search_mod            ! explicit interface
        use part12_mod                 ! explicit interface
        use part13_mod                 ! explicit interface
        use part14_mod                 ! explicit interface
        use part18_mod                 ! explicit interface
        use part21_mod                 ! explicit interface
        use parths_mod                 ! explicit interface
        use partur_mod                 ! explicit interface
        use partwq_mod                 ! explicit interface
        use grid_search_mod            ! explicit interface
        use rdhydr_mod                 ! explicit interface
        use writrk_mod                 ! explicit interface
        use partmem
        use m_part_regular
        use alloc_mod
        use abm_mod
        use omp_lib

        implicit none
        save

        integer(int_wp) :: itime, lunout, lunfil, lunini
        integer(int_wp) :: nosubud, noth
        integer(int_wp) :: ilp, isp, iext, nores, noras
        real(sp) :: dtstep, pctprogress
        logical :: update
        character(len = *) :: ifnam

        integer :: iniday  ! day number for initial condition

        real     (dp) :: rseed = 0.5d0
        real     (sp) :: rnorm

        integer(4) ithndl              ! handle to time this subroutine
        data ithndl / 0 /
        call timini ()
        timon = .true.
        if (timon) call timstrt("delpar", ithndl)

        ! initialize normal distribution generator
        call norm_init()

        ! set switch for stand alone delpar run to true
        alone = .true.

        ! read unit-numbers and file-names
        call rdfnam (lun, ifnam, fname, nfiles, 2, 1, alone, hyd)
        lunout = lun(2)
        call set_log_unit_number(lunout)



        call report_date_time (lunout)
        noth = OMP_GET_MAX_THREADS()
        write (lunout, 2020) noth
        write (*, 2020) noth

        zmodel = hyd%layer_type == HYD_LAYERS_Z
        fmmodel = hyd%geometry == HYD_GEOM_UNSTRUC
        if (fmmodel) then
            call partfm(lunout)
            goto 999
        end if

        ! rdlgri also calculates tcktot ! Data is put in the partmem module
        call rdlgri (nfiles, lun, fname)

        ! Read curved grid
        call rdccol (nmaxp, mmaxp, lun(5), fname(5), &
                lgrid2, xb, yb, lunout)

        if((maxval(xb) <= 180.0).and.(minval(xb) >= -180.0).and.&
                (maxval(yb) <= 90.0).and.(minval(yb) >= -90.0)) then
            write (lunout, 2030)
            write (*, 2030)
        end if

        ! Calculate distances and angles, and depth in the grid
        call part01 (lgrid, lgrid2, xb, yb, dx, &
                dy, area, angle, nmaxp, mmaxp)

        ! From nmax, mmax get dimension of flow array
        nolayp = layt

        ! Initialize hydrodynamics reading
        ihdel = -999
        itime = -999
        call rdhydr (nmaxp, mmaxp, mnmaxk, nflow, nosegp, &
                noqp, itime, itstrtp, ihdel, volumep, &
                vdiff, area, flow, vol1, vol2, &
                flow1, flow2m, vdiff1, update, cellpntp, flowpntp, &
                tau, tau1, caltau, salin, salin1, &
                temper, temper1, nfiles, lun, fname, &
                flow2, rhowatc)

        ! Read the whole input file ! Data is put in the partmem module !
        call rdpart (lun(1), lunout, fname(1))

        ! Calculate mapping function between plot grid and curvilinear grid
        write (*, '(/a)', advance = 'no') '  Preparing initial stage ...'
        call plotgrp(npgrid, pg, nmaxp, mmaxp, lgrid, &
                lgrid2, xb, yb)

        ! part08 - calculates total released mass and mass/particle
        call part08 (lunout, nodye, nocont, ictmax, amassd, &
                ictime, amassc, aconc, tmass, tmassc, &
                nosubs, ndprt, tmassu, ftime, linear, &
                substi, nmdyer, nmconr)

        ! Calculate dump-sites in the grids
        call part06 (lunout, lgrid, lgrid2, nmaxp, mmaxp, &
                xb, yb, nodye, nocont, xwaste, &
                ywaste, nwaste, mwaste)

        ! Initialize number of active particles and other essentials
        nopart = 0
        npwndw = 1
        npwndn = 0

        ! some fixed intializations..
        acomp = .false.
        accrjv = 1.0e-9_sp
        ltrack = notrak  /=  0
        oil = modtyp == model_oil
        oil2dh = oil .and. layt == 1
        oil3d = oil .and. layt  > 1

        ! deflection angle for coriolis effect.(3d oil module only)
        if (oil3d) then
            defang = const(noconsp)
        end if
        !3d
        !3d.. modtyp = model_two_layer_temp is still a 2 layer option, it assumes 2 layers
        !3d.. nolay etc. specifies the total number of layers
        !3d.. layt specifies the number of layers for the hydrodynamics
        !3d
        !3d.. pblay specifies the thickness where the particles will be flipped
        !3d
        if ((modtyp == model_tracers).or.(modtyp >= model_red_tide)) then
            pblay = 0.0
        elseif(modtyp==model_two_layer_temp) then
            !..
            !.. set pblay equal to some value that differs from zero
            !.. the true value will be set by the user later
            !..
            pblay = 0.7
            !3d
            !3d
        else
            write(*, *) 'This model type has not been implemented yet '
            call stop_exit(1)
        end if
        ptlay = 1.0 - pblay
        !
        ! check maximal number of time steps for the psf's
        nstep = 1 + (itstopp - itstrtp) / idelt

        ! for particle tracking:
        ! ittrkc : current time step for writing to track file
        ! itraki : time step increment for writing to track file

        ! in this version, tracks will be written from begin to end
        ! time and each time step. all particles will be written.
        ! particles not yet released will be written as default (999.999)
        itrakc = 0
        itraki = notrak  ! timestep for writing trackinformation to the track file, if notrack =0 then no track file

        ! Get bathymetry depths (w.r.t. reference level)
        call getdps (lunout, lun(17), fname(17), nmaxp, mmaxp, &
                noseglp, dpsp, cellpntp, ltrack)

        ! Compute velocities and depth
        call part03 (lgrid, volumep, flow, dx, dy, &
                nmaxp, mmaxp, mnmaxk, lgrid2, velo, &
                layt, area, depth, dpsp, locdep, &
                zlevel, zmodel, laytop, laytopp, laybot, &
                pagrid, aagrid, tcktot, ltrack, flow2m, &
                lgrid3, vol1, vol2, vel1, vel2)

        ! Initiate particle track file(s)
        if (ltrack) then
            ! Write initial information to track file(s)
            dtstep = real(idelt)
            nstept = 1 + ((itstopp - itstrtp) / idelt) / itraki
            call writrk (lunout, fout, fname(16), nopart, title(4), &
                    dtstep, nstept, ibuff, rbuff, cbuff, &
                    track, npmax)
        end if
        write(*, '(a//)') '  Ready'

        ! Set initial conditions of particles (only oil module)
        iniday = 0
        if (ini_opt == 1 .and. oil) then
            call inipart(lgrid, lgrid2, nmaxp, mmaxp, xb, &
                    yb, nopart, nosubs, substi, ini_file, &
                    xpol, ypol, npolmax, wpart, xpart, &
                    ypart, zpart, npart, mpart, kpart, &
                    iptime, npmax, nrowsmax, lunout)
        elseif (ini_opt == 2 .and. oil) then
            call inipart_asc(lgrid, lgrid2, nmaxp, mmaxp, xb, &
                    yb, nopart, nosubs, substi, ini_file, &
                    xpol, ypol, wpart, xpart, conc2, &
                    ypart, zpart, npart, mpart, kpart, &
                    iptime, npmax, nrowsmax, lunout)
        end if
        if (idp_file /= ' ' .and. modtyp /= model_abm) then
            if (modtyp /= model_prob_dens_settling) then
                write (lunout, *) ' Opening initial particles file:', idp_file(1:len_trim(idp_file))
                call openfl (lunini, idp_file, 0)
                read (lunini) ilp, nopart, nosubs
                do ilp = 1, nopart
                    read(lunini) npart(ilp), mpart(ilp), kpart(ilp), xpart(ilp), ypart(ilp), zpart(ilp), wpart(1:nosubs, ilp), iptime(ilp), track(1, ilp), track(2, ilp), track(3, ilp), &
                            track(4, ilp), track(5, ilp), track(6, ilp), track(7, ilp)
                end do
                close (lunini)
            else
                write (lunout, *) ' Opening initial particles file:', idp_file(1:len_trim(idp_file))
                call openfl (lunini, idp_file, 0)
                read (lunini) ilp, nopart, nosubs
                do ilp = 1, nopart
                    read(lunini) npart(ilp), mpart(ilp), kpart(ilp), xpart(ilp), ypart(ilp), zpart(ilp), wpart(1:nosubs, ilp), &
                            spart(1:nosubs, ilp), iptime(ilp), track(1, ilp), track(2, ilp), track(3, ilp), &
                            track(4, ilp), track(5, ilp), track(6, ilp), track(7, ilp)
                end do
                do ilp = 1, nopart
                    do isp = 1, nosubs
                        if (modtyp == model_prob_dens_settling) then
                            rhopart(isp, ilp) = pldensity(isp)
                        end if
                    end do
                end do
                close (lunini)
            end if
        end if

        ! Draw random log normal distributed particle sizes for non-restart particles
        if (modtyp == model_prob_dens_settling) then
            do ilp = 1, npmax
                rnorm = normal(rseed)
                if (ilp > nopart_res) then
                    do isp = 1, nosubs
                        spart(isp, ilp) = exp(plmusize(isp) + plsigmasize(isp) * rnorm)
                    end do
                end if
            end do
            if (pldebug) then
                size_file = fname(1)
                iext = len_trim(size_file) - 3
                size_file(iext + 1:iext + 5) = 'size'    !dump file for drawn plastic sizes
                open  (newunit = lunfil, file = size_file, form = 'formatted')
                write(lunfil, '(A10,100A20)') 'particle', (trim(substi(isp)), isp = 1, nosubs)
                do ilp = 1, npmax
                    write(lunfil, '(I10,100E20.7)') ilp, spart(1:nosubs, ilp)
                end do
                close(lunfil)
            end if
        end if

        ! Echo start- and stop-time to screen
        write (*, 1010) itstrtp / 86400, mod(itstrtp, 86400) / 3600, mod(itstrtp, 3600) / 60, mod(itstrtp, 60), &
                itstopp / 86400, mod(itstopp, 86400) / 3600, mod(itstopp, 3600) / 60, mod(itstopp, 60)

        ! Begin of main time-cycle loop
        do itime = itstrtp, itstopp, idelt

            !----------------------------------------------------------------------
            ! From here we should keep delpar01.f90 in sync with delpar.F90.
            ! Could this be moved to a separate subroutine called from both?
            !----------------------------------------------------------------------

            ! Echo actual time to screen
            pctprogress = 100.0 * (real(itime) - real(itstrtp)) / (real(itstopp) - real(itstrtp)) ! percentage progress

            write (*, 1020) itime / 86400, mod(itime, 86400) / 3600, mod(itime, 3600) / 60, mod(itime, 60), &
                    itstopp / 86400, mod(itstopp, 86400) / 3600, mod(itstopp, 3600) / 60, mod(itstopp, 60), &
                    pctprogress, nopart - npwndw + 1, npmax
            write (lunout, '(/a)')&
                    '----------------------------------------------------------------------------------'
            write (lunout, 1020) itime / 86400, mod(itime, 86400) / 3600, mod(itime, 3600) / 60, mod(itime, 60), &
                    itstopp / 86400, mod(itstopp, 86400) / 3600, mod(itstopp, 3600) / 60, mod(itstopp, 60), &
                    pctprogress, nopart - npwndw + 1, npmax

            ! Part15 adapts wind and direction for actual time
            call part15(lunout, itime, spawnd, mnmax2, nowind, iwndtm, wveloa, wdira, wvelo, wdir)

            ! Rdhydr reads hydrodynamic water-flow
            call rdhydr (nmaxp, mmaxp, mnmaxk, nflow, nosegp, &
                    noqp, itime, itstrtp, ihdel, volumep, &
                    vdiff, area, flow, vol1, vol2, &
                    flow1, flow2m, vdiff1, update, cellpntp, flowpntp, &
                    tau, tau1, caltau, salin, salin1, &
                    temper, temper1, nfiles, lun, fname, &
                    flow2, rhowatc)

            ! Part03 computes velocities and depth (immediately after reading the new hydro)
            call part03(lgrid, volumep, flow, dx, dy, &
                    nmaxp, mmaxp, mnmaxk, lgrid2, velo, &
                    layt, area, depth, dpsp, locdep, &
                    zlevel, zmodel, laytop, laytopp, laybot, &
                    pagrid, aagrid, tcktot, ltrack, flow2m, &
                    lgrid3, vol1, vol2, vel1, vel2)

            if (zmodel) then
                call partzp(lunout, nopart, nmaxp, mmaxp, mnmax2, nolayp, mpart, npart, &
                        kpart, zpart, lgrid, laytopp, laytop, locdepp, locdep, itime, itstrtp)
            end if

            ! Part12 makes .map files, binary and Nefis versions
            call part12(lun(8), fname(8), lunout, title, subst2, &
                    lgrid, lgrid2, lgrid3, nmaxp, mmaxp, &
                    concp, volumep, npart, mpart, wpart, &
                    nopart, itime, idelt, icwsta, icwsto, &
                    icwste, atotal, npwndw, kpart, pblay, &
                    iptime, npwndn, modtyp, nosubs, noslay, &
                    iyear, imonth, iofset, pg(1), rbuffr, &
                    nosta, mnmax2, noseglp, isfile, mapsub, &
                    layt, area, nfract, use_settling, mstick, &
                    elt_names, elt_types, elt_dims, elt_bytes, locdep, &
                    nosub_max, bufsize)

            ! Part13 makes 3d detail plot grids corrected for recovery rate
            call part13(lun(9), fname(9), lunout, title, subst2, &
                    lgrid2, nmaxp, volumep, area, npart, &
                    mpart, xpart, ypart, wpart, nopart, &
                    itime, idelt, ipset, iptset, xa, &
                    ya, xb, yb, pg(1), recovr, &
                    atotal, iyear, imonth, iofset, npwndw, &
                    lgrid, pblay, modtyp, apeak, adepth, &
                    noslay, nosubs, rbuffr, kpart, itrack, &
                    nplot, mapsub, ntrack, isfile, mmaxp, &
                    nfract, use_settling, mstick, elt_names, elt_types, &
                    elt_dims, elt_bytes, locdep, zpart, za, &
                    dpsp, tcktot, nosub_max, bufsize)

            ! Parths makes 2D averaged time histories every ihstep
            call parths(lun(13), lunout, title, subst, mmaxp, &
                    lgrid2, nmaxp, volumep, area, npart, &
                    mpart, xpart, ypart, wpart, nopart, &
                    itime, idelt, xa, npwndw, lgrid, &
                    ya, xb, yb, pg(1), pblay, &
                    modtyp, noslay, nosubs, concp, chismp, &
                    chispl, nosta, nmstat, xstat, ystat, &
                    nstat, mstat, nplsta, mplsta, ihstrtp, &
                    ihstopp, ihstepp, ihplot, fname(13), kpart, &
                    mnmax2, noseglp, nfract, use_settling, mstick, &
                    elt_names, elt_types, elt_dims, elt_bytes, rbuffr, &
                    zpart, za, locdep, dpsp, tcktot, &
                    lgrid3)

            ! Write particle tracks
            if (ltrack .and. itime==(itstrtp + idelt * itrakc)) then
                ! Get the absolute x,y,z's of the particles
                call part11 (lgrid, xb, yb, nmaxp, npart, &
                        mpart, xpart, ypart, xa, ya, &
                        nopart, npwndw, lgrid2, kpart, zpart, &
                        za, locdep, dpsp, layt, mmaxp, &
                        tcktot)
                ! Write actual particle tracks (file #16)
                call wrttrk(lunout, fout, fname(16), itrakc, nopart, &
                        npmax, xa, ya, za, xyztrk, &
                        nosubs, wpart, track)
                itrakc = itrakc + itraki
            end if

            if (itime >= itstopp) then
                exit ! <=== here the simulation loop ends
            end if

            ! This section does water quality processes
            select case (modtyp)

            case (1)
                ! Conservative tracer model

            case (2, 5)
                ! Temperature model
                call partwq (lgrid, nmaxp, concp, volumep, area, &
                        npart, mpart, wpart, radius, nodye, &
                        npwndw, nopart, idelt, velo, wvelo, &
                        const, noconsp, ptlay, lunout, nosubs, &
                        nolayp, lgrid2, mmaxp, xb, yb, &
                        t0cf, acf, nwaste, mwaste, kpart, &
                        mapsub, layt, mnmaxk)

            case (3)
                ! Obsolete

            case (4)
                ! Oil model
                call oildsp (lgrid, nmaxp, concp, volumep, area, &
                        npart, mpart, wpart, radius, nodye, &
                        npwndw, nopart, itime, idelt, wvelo, &
                        const, lunout, nosubs, noslay, lgrid2, &
                        lgrid3, &
                        mmaxp, xb, yb, kpart, mapsub, &
                        isfile, nfract, mstick, nstick, fstick, &
                        xa, ya, pg(1), use_settling, xpart, &
                        ypart, zpart, za, locdep, dpsp, &
                        tcktot, substi, npmax, rhow, &
                        amassd, ioptrad, ndisapp, idisset, tydisp, &
                        efdisp, xpoldis, ypoldis, nrowsdis, wpartini, &
                        iptime)

            case (7)
                ! ABM model
                if (mod(itime, 86400) == 0) then
                    call part11 (lgrid, xb, yb, nmaxp, npart, &
                            mpart, xpart, ypart, xa, ya, &
                            nopart, npwndw, lgrid2, kpart, zpart, &
                            za, locdep, dpsp, layt, mmaxp, &
                            tcktot)
                end if
                call abm(lunout, itime, idelt, nmaxp, mmaxp, &
                        layt, noseglp, nolayp, mnmaxk, lgrid, &
                        lgrid2, lgrid3, nopart, npwndw, nosubs, &
                        npart, mpart, kpart, xpart, ypart, &
                        zpart, wpart, iptime, wsettl, locdep, &
                        noconsp, const, concp, xa, ya, &
                        angle, vol1, vol2, flow, depth, &
                        vdiff1, salin1, temper1, v_swim, d_swim, &
                        itstrtp, vel1, vel2, abmmt, abmsd, &
                        chronrev, selstage, zmodel, laybot, laytop)

            end select

            ! Two-layer system with stratification
            if (modtyp == model_two_layer_temp) then
                call part18 (lgrid, velo, concp, flres, volumep, &
                        area, mnmaxk, npart, mpart, wpart, &
                        zpart, nopart, idelt, nolayp, npwndw, &
                        vdiff, pblay, ptlay, const, noconsp, &
                        lunout, nosubs, layt, kpart, mapsub(1), &
                        wvelo, alpha, nosubc, mapsub(2))
            end if

            ! Add dye release
            if (nodye > 0) then
                call part09 (lunout, itime, nodye, nwaste, mwaste, &
                        xwaste, ywaste, iwtime, amassd, aconc, &
                        npart, mpart, xpart, ypart, zpart, &
                        wpart, iptime, nopart, radius, nrowswaste, &
                        xpolwaste, ypolwaste, lgrid, &
                        lgrid2, nmaxp, mmaxp, xb, yb, &
                        dx, dy, ndprt, nosubs, kpart, &
                        layt, tcktot, zmodel, laytop, laybot, &
                        nplay, kwaste, nolayp, &
                        modtyp, zwaste, track, nmdyer, substi, &
                        rhopart)
            end if

            ! Add continuous release
            if (nocont > 0) then
                call part14 (itime, idelt, nodye, nocont, ictime, &
                        ictmax, nwaste, mwaste, xwaste, ywaste, &
                        zwaste, aconc, rem, npart, ndprt, &
                        mpart, xpart, ypart, zpart, wpart, &
                        iptime, nopart, pblay, radius, nrowswaste, &
                        xpolwaste, ypolwaste, lgrid, &
                        lgrid2, nmaxp, mmaxp, xb, yb, &
                        dx, dy, ftime, tmassu, nosubs, &
                        ncheck, t0buoy, modtyp, abuoy, t0cf, &
                        acf, lunout, kpart, layt, tcktot, &
                        zmodel, laytop, laybot, nplay, kwaste, &
                        nolayp, linear, track, &
                        nmconr, spart, rhopart, noconsp, const)
            end if

            if (noudef > 0)  then
                ! Add release in a way defined by the user array isub contains references to substances
                call partur (itime, noudef, iutime, mpart, npart, &
                        kpart, xpart, ypart, zpart, wpart, &
                        iptime, nopart, lgrid, nmaxp, mmaxp, &
                        tmasud, ipntp, substi, nosubs, nolayp, &
                        nocont, ndprt, nodye, lunout, rbuffr, &
                        volumep, aconud, uscal, isub, finud, &
                        iftime, ifopt, nosyss, isfud, nosubud, &
                        subsud)
            end if

            ! Calculate the settling velocities on a refined grid (NB: this routine is NOT part of any test in the testbench)
            if (anfac /= 0.0) then
                call part21 (lunout, lgrid, lgrid2, xb, yb, &
                        area, volumep, nmaxp, mmaxp, noslay, &
                        nosubs, nopart, npart, mpart, kpart, &
                        xpart, ypart, zpart, wpart, npwndw, &
                        pg(1), amapsett, xa, ya, za, &
                        atotal, apeak, adepth, imap, nplay, &
                        wsettl, irfac, anfac, use_settling, locdep, &
                        tcktot, dpsp)
            else
                if (modtyp/=model_abm) then
                    wsettl = 1.0  ! Whole array assignment
                end if
            end if

            if (modtyp/=model_abm) then
                call partvs (lunout, itime, nosubs, nopart, ivtset, &
                        ivtime, vsfour, vsfact, wpart, wsettl, &
                        modtyp, nmaxp, mmaxp, lgrid3, noslay, &
                        npart, mpart, kpart, nosegp, noseglp, &
                        rhopart, rhowatc, spart, iptime)
            end if

            ! Calculate actual decaycoefficient
            if (idtset > 0) then
                call part17 (itime, nosubs, idtset, idtime, decay, decays)
            end if

            ! Calculate actual displacement  3d version
            call part10 (lgrid, volumep, flow, dx, dy, &
                    area, angle, nmaxp, mnmaxk, idelt, &
                    nopart, npart, mpart, xpart, ypart, &
                    zpart, iptime, rough, drand, lgrid2, &
                    zmodel, laytop, laybot, &
                    wvelo, wdir, decays, wpart, pblay, &
                    npwndw, vdiff, nosubs, dfact, modtyp, &
                    t0buoy, abuoy, kpart, mmaxp, layt, &
                    wsettl, depth, ldiffz, ldiffh, &
                    acomp, accrjv, xb, yb, &
                    tcktot, lunout, alpha, mapsub, nfract, &
                    taucs, tauce, chezy, rhow, use_settling, &
                    mstick, nstick, ioptdv, cdisp, dminim, &
                    fstick, defang, floil, xpart0, ypart0, &
                    xa0, ya0, xa, ya, npart0, &
                    mpart0, za, locdep, dpsp, nolayp, &
                    vrtdsp, stickdf, subst, nbmax, nconn, &
                    conn, tau, caltau, nboomint, iboomset, &
                    tyboom, efboom, xpolboom, ypolboom, nrowsboom, &
                    itime, v_swim, d_swim)

            !----------------------------------------------------------------------
            ! Until here we should keep delpar01.f90 in sync with delpar.F90
            !----------------------------------------------------------------------

        end do

        call exit_alloc (nstep)

        call delete_file ("particle.wrk", ierror)

        if (write_restart_file) then
            ! First to calculate the number of particles in the restart files
            nores = 0
            noras = 0
            do ilp = 1, nopart
                if (npart(ilp)>1.and.mpart(ilp)>1) then
                    ! Only for the active particles
                    if (lgrid(npart(ilp), mpart(ilp)) >= 1) then
                        ! Only for the active particles
                        nores = nores + 1
                        if (max_restart_age > 0 .and. iptime(ilp) < max_restart_age) then
                            ! If max_restart_age is a positve and the particles' age is less then max_restart_age
                            noras = noras + 1
                        end if
                    end if
                end if
            end do

            res_file = fname(1)
            iext = len_trim(res_file) - 3
            if (max_restart_age < 0) then
                ! Write the restart file with all active paritcles
                if (modtyp==model_prob_dens_settling)then
                    ! Limited number of particles (for 'plastics' modeltype 6 restart, as 'ras' but including settling values)
                    res_file(iext + 1:iext + 4) = 'ses'
                    write (lunout, *) ' Including particle dependent settling velocity'
                else
                    ! All results, except those that are inactive (outside model)
                    res_file(iext + 1:iext + 4) = 'res'
                end if
                write (lunout, *) ' Opening restart particles file:', idp_file(1:len_trim(res_file))
                call openfl (lunfil, res_file, 1)
                write (lunfil) 0, nores, nosubs

                do ilp = 1, nopart
                    if (npart(ilp)>1.and.mpart(ilp)>1) then
                        if (lgrid(npart(ilp), mpart(ilp)) >= 1) then  !only for the active particles
                            if (modtyp /= model_prob_dens_settling) then
                                write (lunfil) npart(ilp), mpart(ilp), kpart(ilp), xpart(ilp), ypart(ilp), zpart(ilp), &
                                        wpart(1:nosubs, ilp), iptime(ilp), track(1:7, ilp)
                            else
                                write (lunfil) npart(ilp), mpart(ilp), kpart(ilp), xpart(ilp), ypart(ilp), zpart(ilp), &
                                        wpart(1:nosubs, ilp), spart(1:nosubs, ilp), iptime(ilp), track(1:7, ilp)
                            end if
                        end if
                    end if
                end do
                write (lunout, *) ' Number of active particles in the restart file: ', nores
                close (lunfil)
            else
                ! Write the restart file with all active paritcles below a certain age
                if (modtyp==model_prob_dens_settling)then
                    ! Limited number of particles (for 'plastics' modeltype 6 restart, as 'ras' but including settling values)
                    res_file(iext + 1:iext + 4) = 'sas'
                    write (lunout, *) ' Including particle dependent settling velocity'
                else
                    ! Limited number of particles (remove particles older than a certain age or inactive)
                    res_file(iext + 1:iext + 4) = 'ras'
                end if
                write (lunout, *) ' Opening restart particles file:', idp_file(1:len_trim(res_file))
                write (lunout, *) ' Particles older than ', max_restart_age, ' seconds are removed'
                call openfl (lunfil, res_file, 1)
                write (lunfil) 0, noras, nosubs

                do ilp = 1, nopart
                    if (npart(ilp)>1.and.mpart(ilp)>1) then
                        if (lgrid(npart(ilp), mpart(ilp)) <= 1 .and. (iptime(ilp) < max_restart_age)) then
                            ! Only when the particles' age less than max_restart_age, time in seconds
                            if (modtyp /= model_prob_dens_settling) then
                                write (lunfil) npart(ilp), mpart(ilp), kpart(ilp), xpart(ilp), ypart(ilp), zpart(ilp), &
                                        wpart(1:nosubs, ilp), iptime(ilp), track(1:7, ilp)
                            else
                                write (lunfil) npart(ilp), mpart(ilp), kpart(ilp), xpart(ilp), ypart(ilp), zpart(ilp), &
                                        wpart(1:nosubs, ilp), spart(1:nosubs, ilp), iptime(ilp), track(1:7, ilp)
                            end if
                        end if
                    end if
                end do
                write (lunout, *) ' Number of active particles in the restart file below maximum age: ', noras
                close (lunfil)
            end if
        end if

        999 call report_date_time(lunout)
        write (*, '(//a)') ' Normal end of PART simulation'
        write (lunout, '(//a)') ' Normal end of PART simulation'

        if (timon) then
            call timstop (ithndl)
            call timdump (fname(1)(1:index(fname(1), ".", .true.) - 1) // '-timers.out')
        end if

        ! Formats
        1010 format('  Start  time :', i6.4, 'D-', i2.2, 'H-', i2.2, 'M-', i2.2, 'S.'/&
                '  Stop   time :', i6.4, 'D-', i2.2, 'H-', i2.2, 'M-', i2.2, 'S.'//)
        1020 format('  Time ', i6.4, 'D-', i2.2, 'H-', i2.2, 'M-', i2.2, 'S.', ' Stop time ', &
                i6.4, 'D-', i2.2, 'H-', i2.2, 'M-', i2.2, 'S. (', f5.1, '% completed) ', &
                i11, ' part. (of', i11, ')')

        2020 format (/'  Parallel processing with ', i3, ' processor(s)'/)
        2030 format (/'  WARNING: Your x-coordinates are in the range [-180,180] and your'&
                /'           y-coordinates are in the range [-90,90]. You might have'&
                /'           a spherical grid. This is not yet supported by PART.'//)

    end subroutine delpar

end module m_delpar
