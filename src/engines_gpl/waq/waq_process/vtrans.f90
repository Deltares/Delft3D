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
module m_vtrans
    use m_waq_precision

    implicit none

contains


    subroutine vtrans (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Vertical distribution after a longer time span to correct 3D-BLOOM

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! NOLAY   I*4 1 I     number of layers
        !

        use m_logger_helper, only : get_log_unit_number, stop_with_error
        use m_cli_utils, only : get_command_argument_by_name
        use m_dhnoseg
        use m_dhnolay
        use m_dhltim
        use m_evaluate_waq_attribute
        use bloom_data_vtrans

        implicit none

        real(kind = real_wp) :: pmsa  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(*), increm(*), noseg, noflux, &
                iexpnt(4, *), iknmrk(*), noq1, noq2, noq3, noq4
        !
        !     local declarations
        !
        integer(kind = int_wp) :: ierr_alloc, lunrep
        integer(kind = int_wp) :: ip1, ip2, ip3, ip4, ip5, &
                ip6, ip7, ip8, ip9, ip10, &
                ip11, ip12, ip13, ip14, ip15
        integer(kind = int_wp) :: in1, in2, in3, in4, in5, &
                in6, in7, in8, in9, in10, &
                in11, in12, in13, in14, in15
        integer(kind = int_wp) :: idt, nolay, nosegl, noq, noq12, &
                ilay, isegl, iseg, ifrom, ito, &
                nosub, isub, iq, iqtest, ikmrk1, &
                nosegw, ikmrk2, itime
        real(kind = real_wp) :: disp, area, lenfr, lento, al, &
                e, diag, codiag, rhs, volume, &
                delt, period

        logical :: parsing_error
        logical :: l_initial
        logical, save :: l_restart
        character(:), allocatable :: file_initial
        character(:), allocatable, save :: file_restart
        integer(kind = int_wp) :: ilun
        integer(kind = int_wp) :: nosegi
        integer(kind = int_wp) :: nolayi

        !
        ip1 = ipoint(1)
        ip2 = ipoint(2)
        ip3 = ipoint(3)
        ip5 = ipoint(5)
        ip6 = ipoint(6)
        ip7 = ipoint(7)
        ip8 = ipoint(8)
        ip14 = ipoint(14)
        !
        idt = nint(pmsa(ip1))
        delt = pmsa(ip2)
        period = pmsa(ip3) / 24.
        itime = nint(pmsa(ip5))

        if (.not.fm_vtrans) then
            call dhnoseg(nosegw)
            call dhnolay(nolay)
        else
            nolay = nolayfm
            nosegw = noseg
        endif

        !     initialise and allocate memory in module bloom_data_vtrans
        if (.not. init_vtrans) then
            init_vtrans = .true.
            call get_log_unit_number(lunrep)
            if (noq3 > 0) then
                if (nolay/=0) then
                    nosegl = nosegw / nolay
                else
                    nosegw = -1
                endif
                active_vtrans = .true.
                pmsa(ip14) = 1.0
                if (fm_vtrans) then
                    allocate(fmlayer(noseg), fmktop(noseg), fmkbot(noseg), stat = ierr_alloc)
                    if (ierr_alloc /= 0) then
                        write (lunrep, 1000) ierr_alloc
                        write (lunrep, 1001) noseg
                    endif
                    do iseg = 1, noseg
                        fmlayer(iseg) = pmsa(ip6)
                        fmktop(iseg) = pmsa(ip7)
                        fmkbot(iseg) = pmsa(ip8)
                        ip6 = ip6 + increm(6)
                        ip7 = ip7 + increm(7)
                        ip8 = ip8 + increm(8)
                    enddo
                    nolayfm = maxval(fmlayer)
                    nolay = nolayfm
                    nosegw = noseg
                else if(nosegl * nolay /= nosegw) then
                    write(lunrep, *) ' WARNING unstructured 3D application'
                    write(lunrep, *) ' Vertical distribution routine VTRANS not possible'
                    nolay = 1
                    active_vtrans = .false.
                    pmsa(ip14) = 0.0
                endif
            else
                write(lunrep, *) ' WARNING 2D application'
                write(lunrep, *) ' Vertical distribution routine VTRANS not possible'
                nolay = 1
                active_vtrans = .false.
                pmsa(ip14) = 0.0
            endif
            if (active_vtrans) then
                nolaylocal = nolay
                noseglocal = noseg
                allocate(concv(nolay, noseg), timev(nolay, noseg), fracv(nolay, noseg), dervv(nolay, noseg), stat = ierr_alloc)
                if (ierr_alloc /= 0) then
                    write (lunrep, 1000) ierr_alloc
                    write (lunrep, 1001) noseg
                    write (lunrep, 1002) nolay
                    call stop_with_error()
                endif

                ! read initial file?

                if(.not.fm_vtrans) then
                    l_initial = get_command_argument_by_name('-vtrans_initial', file_initial, parsing_error)
                    l_restart = get_command_argument_by_name('-vtrans_restart', file_restart, parsing_error)
                else
                    l_initial = .false.
                    l_restart = .false.
                endif
                if (l_initial) then
                    write(lunrep, *) 'vtrans using initial condition from file:', trim(file_initial)
                    open(newunit = ilun, file = file_initial, form = 'unformatted', access = 'stream')
                    read(ilun) nosegi, nolayi
                    read(ilun) timtot
                    read(ilun) concv
                    read(ilun) timev
                    read(ilun) fracv
                    close(ilun)
                else

                    ! initialise concentration level 1.0 in the specific layer, 0.0 rest layers, init timev 0.0

                    concv = 0.0
                    timev = 0.0
                    fracv = 0.0
                    timtot = 0.0
                    if(.not.fm_vtrans) then
                        do ilay = 1, nolay
                            isegl = (ilay - 1) * nosegl + 1
                            concv(ilay, isegl:isegl + nosegl - 1) = 1.0
                        enddo
                    else
                        do iseg = 1, noseg
                            ilay = fmlayer(iseg)
                            if(ilay>0) then
                                concv(ilay, iseg) = 1.0
                            endif
                        enddo
                    endif
                endif
                if (l_restart) then
                    write(lunrep, *) 'vtrans will write restart condition to file:', trim(file_restart)
                endif
            endif
        else
            l_initial = .false.
        endif
        !
        if (.not. active_vtrans) return
        !
        nolay = nolaylocal
        nosegl = nosegw / nolay
        nosub = nolay
        noq = noq1 + noq2 + noq3
        noq12 = noq1 + noq2

        ! not the first time if initialised to prevent double step

        if (.not. l_initial) then
            !
            !        make masses , volumes on diagonal + test for active exchange for z model
            !
            in4 = increm(4)
            ip4 = ipoint(4)
            do iseg = 1, nosegw
                volume = pmsa(ip4) + tiny(1.0)
                do ilay = 1, nolay
                    concv(ilay, iseg) = concv(ilay, iseg) * volume
                    dervv(ilay, iseg) = volume
                enddo
                ip4 = ip4 + in4
            enddo
            !
            !        do a transport step in the vertical, dispersion only, double sweep see also DLWQD1
            !
            in9 = increm(9)
            in10 = increm(10)
            in11 = increm(11)
            in12 = increm(12)
            in13 = increm(13)
            ip9 = ipoint(9) + noq12 * in9
            ip10 = ipoint(10) + noq12 * in10
            ip11 = ipoint(11) + noq12 * in11
            ip12 = ipoint(12) + noq12 * in12
            ip13 = ipoint(13) + noq12 * in13
            do iq = noq12 + 1, noq
                ifrom = iexpnt(1, iq)
                ito = iexpnt(2, iq)
                if (ifrom > 0 .and. ito > 0) then
                    call extract_waq_attribute(1, iknmrk(ito), ikmrk1)
                    if (ikmrk1==1) then
                        disp = pmsa(ip9) + pmsa(ip13)
                    else
                        disp = 0.0
                    endif
                    area = pmsa(ip10)
                    lenfr = pmsa(ip11)
                    lento = pmsa(ip12)
                    !
                    al = max(tiny(1.0), lenfr + lento)
                    e = idt * disp * area / al
                    do isub = 1, nosub
                        !
                        !                 row of the 'from' segment
                        !
                        diag = dervv(isub, ifrom) + e
                        codiag = -e / diag
                        rhs = concv(isub, ifrom) / diag
                        dervv(isub, ifrom) = codiag
                        concv(isub, ifrom) = rhs
                        !                 row of the 'to  ' segment
                        dervv(isub, ito) = dervv(isub, ito) + (e + e * codiag)
                        concv(isub, ito) = concv(isub, ito) + e * rhs
                    enddo
                endif
                ip9 = ip9 + in9
                ip10 = ip10 + in10
                ip11 = ip11 + in11
                ip12 = ip12 + in12
                ip13 = ip13 + in13
            enddo
            !
            !            Loop over exchanges, single sweep backward
            !
            do iq = noq, noq12 + 1, -1
                ifrom = iexpnt(1, iq)
                ito = iexpnt(2, iq)
                if (ifrom > 0 .and. ito > 0) then
                    do isub = 1, nosub
                        codiag = dervv(isub, ifrom)
                        diag = dervv(isub, ito)
                        rhs = concv(isub, ito) / diag
                        concv(isub, ifrom) = concv(isub, ifrom) - codiag * rhs
                        concv(isub, ito) = rhs
                        dervv(isub, ifrom) = 1.0
                        dervv(isub, ito) = 1.0
                    enddo
                endif
            enddo

            do iseg = 1, nosegw      !  for if some diagonal entries are not 1.0
                do ilay = 1, nolay
                    if (dervv(ilay, iseg) /= 0.0) then
                        concv(ilay, iseg) = concv(ilay, iseg) / dervv(ilay, iseg)
                    endif
                    dervv(ilay, iseg) = 1.0
                enddo
            enddo
            !
            !        cummulate time
            !
            timev = timev + concv * delt
            timtot = timtot + delt
            !
            !        if accumulated time equal or greater then accumulation period then calculate fraction of time
            !        and reset the distribution
            !
            if (timtot >= (period - delt * 0.5)) then
                fracv = timev / timtot
                concv = 0.0
                timev = 0.0
                timtot = 0.0
                if(.not.fm_vtrans) then
                    do ilay = 1, nolay
                        isegl = (ilay - 1) * nosegl + 1
                        concv(ilay, isegl:isegl + nosegl - 1) = 1.0
                    enddo
                else
                    do iseg = 1, noseg
                        ilay = fmlayer(iseg)
                        if(ilay>0) then
                            concv(ilay, iseg) = 1.0
                        endif
                    enddo
                endif
            endif
        endif
        !
        !     output IP14 is switch for the PLCT/BLOOM
        !     furthermore there is a max of 100 output, the fraction of time
        !
        do iseg = 1, noseg
            do ilay = 1, min(100, nolay)
                ip15 = ipoint(14 + ilay) + (iseg - 1) * increm(14 + ilay)
                pmsa(ip15) = fracv(ilay, iseg)
            enddo
        enddo

        ! write restart file? at last time

        if (l_restart) then
            if (dhltim(itime, idt)) then
                open(newunit = ilun, file = file_restart, form = 'unformatted', access = 'stream')
                write(ilun) noseg, nolay
                write(ilun) timtot
                write(ilun) concv
                write(ilun) timev
                write(ilun) fracv
                close(ilun)
            endif
        endif
        !
        return
        1000 format(' ERROR: allocating memory in process ''vtrans'' :', I10)
        1001 format(' noseg = ', I10)
        1002 format(' nolay = ', I10)
    end

end module m_vtrans
