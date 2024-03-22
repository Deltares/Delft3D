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
module m_delpar00
    use m_waq_precision
    use m_string_utils
    use m_wrttrk
    use m_plotgr
    use m_part08
    use m_part06
    use m_part01
    use m_inipart_asc
    use m_inipart
    use m_part11
    use m_report_date_time
    use m_rdpart
    use m_rdlgri
    use m_rdfnam
    use m_rdccol
    use m_getdps

    implicit none

contains


    subroutine delpar00 (mdpfile, noseg, noq, dwqvol, dwqflo, &
            nosfun, sfname, segfun)

        use m_srstop
        use partmem      !   for PARTicle tracking
        use alloc_mod    !   for PARTicle tracking
        use writrk_mod   !   for PARTicle tracking
        use timers
        use spec_feat_par
        use openfl_mod            ! explicit interface
        use normal_mod
        use part03_mod                 ! explicit interface
        use partini_mod
        use m_part_regular
        use m_part_modeltypes
        implicit none

        !         Particle tracking


        !     Arguments

        !     kind            function         name                      description

        character(*), intent(in) :: mdpfile                 !< file name mdp-file
        integer  (kind = int_wp), intent(in) :: noseg                   !< delwaq noseg
        integer  (kind = int_wp), intent(in) :: noq                     !< delwaq noq
        real     (kind = real_wp), intent(in) :: dwqvol (noseg)          !< delwaq volumes
        real     (kind = real_wp), intent(in) :: dwqflo (noq)            !< delwaq flows
        integer  (kind = int_wp), intent(in) :: nosfun                  !< number of segment functions
        character(20), intent(in) :: sfname (nosfun)         !< names of segment functions
        real(kind = real_wp), intent(in) :: segfun (noseg, nosfun)   !< segment function values
        integer(kind = int_wp), save :: iniday               ! release of initial condition, not used here

        !     Locals

        integer(kind = int_wp) i, i2, itime, indx, ierr
        integer(kind = int_wp) :: lunut          ! report file
        integer(kind = int_wp) :: ilp, isp, ids, ide, iext, nores, noras, nosubs_idp
        real   (kind = real_wp) :: depmin
        real   (kind = real_wp) :: dtstep
        real   (kind = real_wp) :: rnorm
        real   (kind = dp) :: rseed = 0.5d0 ! AM: some strange quirk makes this necessary
        logical update
        integer(kind = int_wp) :: luini

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("delpar00", ithandl)

        alone = .true.
        if (mdpfile /= ' ') then
            call norm_init()
            alone = .false.
            call rdfnam (lunitp, mdpfile, fnamep, nfilesp, 2, &
                    1, .false.)
            lunut = lunitp(2)
            call report_date_time   (lunut)
            call rdlgri (nfilesp, lunitp, fnamep)
            call rdccol (nmaxp, mmaxp, lunitp(5), fnamep(5), &
                    lgrid2, xb, yb, lunut)
            call part01 (lgrid, lgrid2, xb, yb, dx, &
                    dy, area, angle, nmaxp, mmaxp)
            nolayp = layt

            !           this replaces the call to rdhydr

            if (lsettl .or. layt > 1) then
                indx = index_in_array('TAU       ', sfname)
                if (indx > 0) then
                    do i = 1, nosegp
                        tau(cellpntp(i)) = segfun(i, indx)
                    enddo
                    caltau = .false.
                else if (lunitp(21) > 0) then
                    if (fnamep(21)(1:4) /= 'none') then
                        write (lunut, *) ' Opening the tau    file:', fnamep(21)(1:len_trim(fnamep(21)))
                        open (lunitp(21), file = fnamep(21), form = 'unformatted', access = 'stream', status = 'old', iostat = ierr)
                        if (ierr /= 0) then
                            write (lunut, *) ' Warning: could not open the tau file! Tau will be computed.'
                            lunitp(21) = 0
                        endif
                    else
                        lunitp(21) = 0
                    endif
                    caltau = .false.
                    if (lunitp(21) == 0) caltau = .true.
                endif
                indx = index_in_array('VERTDISP  ', sfname)
                if (indx > 0) then
                    do i = 1, nosegp
                        vdiff(cellpntp(i)) = segfun(i, indx)
                    enddo
                else if (lunitp(20) > 0) then
                    if (fnamep(20)(1:4) /= 'none') then
                        write (lunut, *) ' Opening the vdf    file:', fnamep(20)(1:len_trim(fnamep(20)))
                        open (lunitp(20), file = fnamep(20), form = 'unformatted', access = 'stream', status = 'old', iostat = ierr)
                        if (ierr /= 0) then
                            write (lunut, *) ' Warning: could not open the vdf file! vdf will be set to zero!'
                            lunitp(20) = 0
                        endif
                    else
                        lunitp(20) = 0
                    endif
                endif
                indx = index_in_array('SALINITY  ', sfname)
                if (indx > 0) then
                    do i = 1, nosegp
                        salin(cellpntp(i)) = segfun(i, indx)
                    enddo
                else if (lunitp(22) > 0) then
                    write (lunut, *) ' Opening the salinity file:', fnamep(22)(1:len_trim(fnamep(22)))
                    open (lunitp(22), file = fnamep(22), form = 'unformatted', access = 'stream', status = 'old', iostat = ierr)
                    if (ierr /= 0) then
                        write (lunut, *) ' Warning: could not open the sal file! sal will be set to zero!'
                        lunitp(22) = 0
                    endif
                else
                    lunitp(22) = 0
                endif

                !.. temperature

                indx = index_in_array('TEMP      ', sfname)
                if (indx > 0) then
                    do i = 1, nosegp
                        temper(cellpntp(i)) = segfun(i, indx)
                    enddo
                else if (lunitp(23) > 0) then
                    write (lunut, *) ' Opening the temperature file:', fnamep(23)(1:len_trim(fnamep(23)))
                    open (lunitp(23), file = fnamep(23), form = 'unformatted', access = 'stream', status = 'old', iostat = ierr)
                    if (ierr /= 0) then
                        write (lunut, *) ' Warning: could not open the tem file! temp will be set to zero!'
                        lunitp(23) = 0
                    endif
                else
                    lunitp(23) = 0
                endif
            endif

            do i = 1, nosegp
                volumep(cellpntp(i)) = dwqvol(i)
            enddo

            flow = 0.0
            do i = 1, noqp
                if (flowpntp(i, 1) > 0) flow(flowpntp(i, 1)) = flow(flowpntp(i, 1)) + dwqflo(i)
                if (flowpntp(i, 2) > 0) flow(flowpntp(i, 2)) = flow(flowpntp(i, 2)) + dwqflo(i)
            enddo
            depmin = (0.05 * nmaxp * mmaxp) / mnmaxk
            depmin = max(depmin, 0.001)
            do i = 1, mnmaxk        !       limit volume to depmin
                i2 = mod(i - 1, nmaxp * mmaxp) + 1
                volumep(i) = max(volumep(i), area(i2) * depmin)
            enddo

            call rdpart (lunitp(1), lunut, fnamep(1))
            call plotgrp(npgrid, pg, nmaxp, mmaxp, lgrid, &
                    lgrid2, xb, yb)
            call part08 (lunut, nodye, nocont, ictmax, amassd, &
                    ictime, amassc, aconc, tmass, tmassc, &
                    nosubs, ndprt, tmassu, ftime, linear, &
                    substi, nmdyer, nmconr)
            call part06 (lunut, lgrid, lgrid2, nmaxp, mmaxp, &
                    xb, yb, nodye, nocont, xwaste, &
                    ywaste, nwaste, mwaste)
            nopart = 0
            npwndw = 1
            npwndn = 0
            acomp = .false.
            accrjv = 1.0e-9 !_sp
            oil2dh = oil .and. layt == 1
            oil3d = oil .and. layt  > 1
            ltrack = notrak  /=  0
            call getdps (lunut, lunitp(17), fnamep(17), nmaxp, mmaxp, &
                    noseglp, dpsp, cellpntp, ltrack)
            call part03 (lgrid, volumep, flow, dx, dy, &
                    nmaxp, mmaxp, mnmaxk, lgrid2, velo, &
                    layt, area, depth, dpsp, locdep, &
                    zlevel, zmodel, laytop, laytopp, laybot, &
                    pagrid, aagrid, tcktot, ltrack, flow2m, &
                    lgrid3, vol1, vol2, vel1, vel2)

            if (ltrack) then
                call part11 (lgrid, xb, yb, nmaxp, npart, &
                        mpart, xpart, ypart, xa, ya, &
                        nopart, npwndw, lgrid2, kpart, zpart, &
                        za, locdep, dpsp, layt, mmaxp, &
                        tcktot)
                call wrttrk (lunut, fout, fnamep(16), itrakc, nopart, &
                        npmax, xa, ya, za, xyztrk, &
                        nosubs, wpart, track)
            endif
            oil = modtyp == model_oil
            if (ini_opt == 1 .and. oil) then
                call inipart(lgrid, lgrid2, nmaxp, mmaxp, xb, &
                        yb, nopart, nosubs, substi(1), ini_file, &
                        xpol, ypol, npolmax, wpart, xpart, &
                        ypart, zpart, npart, mpart, kpart, &
                        iptime, npmax, nrowsmax, lunut)
            elseif (ini_opt == 2 .and. oil) then
                call inipart_asc(lgrid, lgrid2, nmaxp, mmaxp, xb, &
                        yb, nopart, nosubs, substi(1), ini_file, &
                        xpol, ypol, wpart, xpart, conc2, &
                        ypart, zpart, npart, mpart, kpart, &
                        iptime, npmax, nrowsmax, lunut)
            endif
            if (modtyp /= model_abm .and. idp_file /= ' ') then
                if (modtyp /= model_prob_dens_settling) then
                    write (lunut, *) ' Opening initial particles file:', idp_file(1:len_trim(idp_file))
                    call openfl (luini, idp_file, 0)
                    read (50) ilp, nopart, nosubs_idp
                    if (nosubs_idp/=nosubs) then
                        write (lunut, *) ' Error: number of substances in the ini-file   : ', nosubs_idp
                        write (lunut, *) '        number of substances in the model setup: ', nosubs
                        write (*, *) ' Error: number of substances in the ini-file   : ', nosubs_idp
                        write (*, *) '        number of substances in the model setup: ', nosubs
                        call srstop(1)
                    endif
                    do ilp = 1, nopart
                        read(luini) npart(ilp), mpart(ilp), kpart(ilp), xpart(ilp), ypart(ilp), zpart(ilp), wpart(1:nosubs, ilp), &
                                iptime(ilp)
                    enddo
                    close (luini)
                else
                    write (lunut, *) ' Opening initial particles file:', idp_file(1:len_trim(idp_file))
                    call openfl (luini, idp_file, 0)
                    read (luini) ilp, nopart, nosubs_idp
                    if (nosubs_idp/=nosubs) then
                        write (lunut, *) ' Error: number of substances in the ini-file   : ', nosubs_idp
                        write (lunut, *) '        number of substances in the model setup: ', nosubs
                        write (*, *) ' Error: number of substances in the ini-file   : ', nosubs_idp
                        write (*, *) '        number of substances in the model setup: ', nosubs
                        call srstop(1)
                    endif
                    do ilp = 1, nopart
                        read(luini) npart(ilp), mpart(ilp), kpart(ilp), xpart(ilp), ypart(ilp), zpart(ilp), wpart(1:nosubs, ilp), &
                                spart(1:nosubs, ilp), iptime(ilp)
                    enddo
                    do ilp = 1, nopart
                        do isp = 1, nosubs
                            if (modtyp == model_prob_dens_settling) then
                                rhopart(isp, ilp) = pldensity(isp)
                            endif
                        enddo
                    enddo
                    close (luini)
                end if
            elseif (idp_file /= ' ') then
                write (lunut, *) ' Opening initial particles file:', idp_file(1:len_trim(idp_file))
                open(newunit = luini, file = idp_file, form = 'FORMATTED', status = 'old')
                read (luini, *) nopart_res, nosubs_idp
                close (luini)
                npmax = nopart_res
            endif
            !     Draw random log normal distributed particle sizes for non-restart particles
            if (modtyp == model_prob_dens_settling) then
                do ilp = 1, npmax
                    rnorm = normal(rseed)
                    if (ilp > nopart_res) then
                        do isp = 1, nosubs
                            spart(isp, ilp) = exp(plmusize(isp) + plsigmasize(isp) * rnorm)
                        enddo
                    endif
                enddo
                if (pldebug) then
                    size_file = mdpfile
                    iext = len_trim(size_file) - 3
                    size_file(iext + 1:iext + 5) = 'size'    !dump file for drawn plastic sizes
                    open  (newunit = luini, file = size_file, form = 'formatted')
                    write(luini, '(A10,100A20)') 'particle', (trim(substi(isp)), isp = 1, nosubs)
                    do ilp = 1, npmax
                        write(luini, '(I10,100E20.7)') ilp, spart(1:nosubs, ilp)
                    enddo
                    close(luini)
                endif
            end if

            if (oil2dh) hmin = const(noconsp) ! 2dh: last par        =hmin
            if (oil3d) then
                hmin = const(noconsp - 1)
                defang = const(noconsp)
            endif
            if ((modtyp == model_tracers).or.(modtyp >= model_red_tide)) then
                pblay = 0.0
            elseif(modtyp==model_two_layer_temp) then
                pblay = 0.7
            else
                write(*, *) 'This model type has not been implemented yet '
                call srstop(1)
            endif
            ptlay = 1.0 - pblay
            nstep = 1 + (itstopp - itstrtp) / idelt
            itrakc = 0
            itraki = notrak  ! timestep for writing trackinformation to the track file, if notrack =0 then no track file
            if (ltrack) itrakc = itrakc + itraki
            itraki = notrak  ! timestep for writing trackinformation to the track file, if notrack =0 then no track file
            if (ltrack) then
                !     write initial information to track file
                dtstep = float(idelt)
                nstept = 1 + ((itstopp - itstrtp) / idelt) / itraki
                call writrk (lunut, fout, fnamep(16), nopart, title(4), &
                        dtstep, nstept, ibuff, rbuff, cbuff, &
                        track, npmax)
                call part11 (lgrid, xb, yb, nmaxp, npart, &
                        mpart, xpart, ypart, xa, ya, &
                        nopart, npwndw, lgrid2, kpart, zpart, &
                        za, locdep, dpsp, layt, mmaxp, &
                        tcktot)
                call wrttrk (lunut, fout, fnamep(16), itrakc, nopart, &
                        npmax, xa, ya, za, xyztrk, &
                        nosubs, wpart, track)
                itrakc = itrakc + itraki
            endif
            call exit_alloc (i2)
        endif

        if (timon) call timstop (ithandl)
        return
    end

end module m_delpar00
