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
module m_intoou
    use m_waq_precision

    implicit none

contains


    subroutine intoou (procesdef, nproc, nflux, nsvar, pronam, &
            iflux, ipmsa, ipssa, nipmsa, ioffx, &
            nocons, nopa, nofun, nosfun, notot, &
            nodisp, novelo, nodef, noloc, ndspx, &
            nvelx, nlocx, nopred, prvvar, prvtyp, &
            novar, progrd, prondt)

        ! maps input structure to output structure

        use processet
        use timers       !   performance timers

        implicit none

        ! declaration of arguments

        type(procespropcoll) :: procesdef       ! all processes
        integer(kind = int_wp) :: nproc           ! number of processes (active)
        integer(kind = int_wp) :: nflux           ! number of fluxes
        integer(kind = int_wp) :: nsvar(*)        ! number of variables
        character(len = *) :: pronam(*)       ! routine name
        integer(kind = int_wp) :: iflux(*)        ! index in flux array
        integer(kind = int_wp) :: ipmsa(*)        ! index in pmsa array
        integer(kind = int_wp) :: ipssa(*)        ! index in ssa  array
        integer(kind = int_wp) :: nipmsa          ! length pmsa array
        integer(kind = int_wp) :: ioff            ! offset in pointer for segment items
        integer(kind = int_wp) :: ioffx           ! offset in pointer for exchange items
        integer(kind = int_wp) :: nocons          ! number of constants
        integer(kind = int_wp) :: nopa            ! number of parameters
        integer(kind = int_wp) :: nofun           ! number of functions
        integer(kind = int_wp) :: nosfun          ! number of segment functions
        integer(kind = int_wp) :: notot           ! number of substances
        integer(kind = int_wp) :: nodisp          ! number of dispersions
        integer(kind = int_wp) :: novelo          ! number of velocities
        integer(kind = int_wp) :: nodef           ! number of default
        integer(kind = int_wp) :: noloc           ! number of local values
        integer(kind = int_wp) :: ndspx           ! number of dispersions
        integer(kind = int_wp) :: nvelx           ! number of velocities
        integer(kind = int_wp) :: nlocx           ! number of local values on exchanges
        integer(kind = int_wp) :: nopred          ! number of predfined values
        integer(kind = int_wp) :: prvvar(*)       ! variable pointer
        integer(kind = int_wp) :: prvtyp(*)       ! type of variable
        integer(kind = int_wp) :: novar           ! number of variables
        integer(kind = int_wp) :: progrd(*)       ! process grid
        integer(kind = int_wp) :: prondt(*)       ! process ndt

        ! local decalarations

        integer(kind = int_wp) :: nproctot        ! number of processes
        integer(kind = int_wp) :: iproc           ! loop counter processes
        integer(kind = int_wp) :: iproc_act       ! index active processes
        type(procesprop), pointer :: proc            ! process description
        integer(kind = int_wp) :: i_input         ! index input item
        integer(kind = int_wp) :: ioutput         ! index output item
        integer(kind = int_wp) :: ibfl            ! index flux
        integer(kind = int_wp) :: itel            ! counter
        integer(kind = int_wp) :: itel0           ! counter
        integer(kind = int_wp) :: itel1           ! counter
        character(len = 80) :: line            ! output buffer
        integer(kind = int_wp) :: ivvol           ! pointer index to array
        integer(kind = int_wp) :: ivare           ! pointer index to array
        integer(kind = int_wp) :: ivflo           ! pointer index to array
        integer(kind = int_wp) :: ivlen           ! pointer index to array
        integer(kind = int_wp) :: ivcns           ! pointer index to array
        integer(kind = int_wp) :: ivpar           ! pointer index to array
        integer(kind = int_wp) :: ivfun           ! pointer index to array
        integer(kind = int_wp) :: ivsfu           ! pointer index to array
        integer(kind = int_wp) :: ivcnc           ! pointer index to array
        integer(kind = int_wp) :: ivmas           ! pointer index to array
        integer(kind = int_wp) :: ivder           ! pointer index to array
        integer(kind = int_wp) :: ivdsp           ! pointer index to array
        integer(kind = int_wp) :: ivvel           ! pointer index to array
        integer(kind = int_wp) :: ivdef           ! pointer index to array
        integer(kind = int_wp) :: ivloc           ! pointer index to array
        integer(kind = int_wp) :: ivdsx           ! pointer index to array
        integer(kind = int_wp) :: ivvlx           ! pointer index to array
        integer(kind = int_wp) :: ivlcx           ! pointer index to array
        integer(kind = int_wp) :: ivflx           ! pointer index to array
        integer(kind = int_wp) :: ioffx2          ! pointer index to array
        integer(kind = int_wp) :: nfluxx          ! second counter nflux
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("intoou", ithndl)

        ! first calculate nproc, nflux

        nproc = 0
        nflux = 0
        nproctot = procesdef%cursize
        do iproc = 1, nproctot
            proc => procesdef%procesprops(iproc)
            if (proc%active) then
                nproc = nproc + 1
                nflux = nflux + proc%no_fluxoutput
            endif
        enddo
        ioffx2 = ioffx + nflux

        ! set pointer index to the delwaq arrays

        ivvol = 1
        ivare = ivvol + 1
        ivflo = ivare + 1
        ivlen = ivflo + 1
        ivcns = ivlen + 2
        ivpar = ivcns + nocons
        ivfun = ivpar + nopa
        ivsfu = ivfun + nofun
        ivcnc = ivsfu + nosfun
        ivmas = ivcnc + notot
        ivder = ivmas + notot
        ivdsp = ivder + notot
        ivvel = ivdsp + nodisp
        ivdef = ivvel + novelo
        ivloc = ivdef + nodef
        ivdsx = ivloc + noloc
        ivvlx = ivdsx + ndspx
        ivlcx = ivvlx + nvelx
        ivflx = ivlcx + nlocx
        novar = ivflx + nflux - 1

        ! initialize totals , including nflux again (principally the same)

        itel = 0
        nfluxx = 0

        ! loop over processes

        nproctot = procesdef%cursize
        iproc_act = 0
        do iproc = 1, nproctot
            proc => procesdef%procesprops(iproc)
            if (proc%active) then
                iproc_act = iproc_act + 1
                nsvar(iproc_act) = proc%no_input + proc%no_output + proc%no_fluxoutput
                pronam(iproc_act) = proc%routine
                progrd(iproc_act) = proc%grid
                prondt(iproc_act) = proc%ndt
                iflux(iproc_act) = nfluxx + 1

                ! map in- and output pointers on ipmsa

                itel0 = itel
                do i_input = 1, proc%no_input
                    itel = itel + 1
                    if (proc%input_item(i_input)%indx > 0) then
                        itel1 = itel0 + proc%input_item(i_input)%indx
                    else
                        itel1 = itel
                        write(*, *) 'geen volgorde voor proces:', proc%name
                    endif
                    if (proc%input_item(i_input)%type == IOTYPE_SEGMENT_INPUT .or. &
                            proc%input_item(i_input)%type == IOTYPE_SEGMENT_WORK) then
                        if (proc%input_item(i_input)%ip_val == 3) then
                            ! idt, voor fractional step aparte ingang in default array
                            ioff = nopred + nocons + nopa + nofun + nosfun + notot + noloc
                            proc%input_item(i_input)%ip_val = ioff + nodef - 2 * nproc + iproc_act
                        endif
                        if (proc%input_item(i_input)%ip_val == 4) then
                            ! delt, voor fractional step aparte ingang in default array
                            ioff = nopred + nocons + nopa + nofun + nosfun + notot + noloc
                            proc%input_item(i_input)%ip_val = ioff + nodef - nproc + iproc_act
                        endif
                        ipmsa(itel1) = proc%input_item(i_input)%ip_val
                        prvtyp(itel1) = 1
                    else
                        if (proc%input_item(i_input)%ip_val /= 0) then
                            ipmsa(itel1) = ioffx2 + proc%input_item(i_input)%ip_val
                        else
                            ipmsa(itel1) = 0
                        endif
                        prvtyp(itel1) = 2
                    endif
                    ipssa(itel1) = 0
                    call ip2var &
                            (ipmsa(itel1), prvvar(itel1), nocons, nopa, nofun, &
                            nosfun, notot, nodisp, novelo, nodef, &
                            noloc, ndspx, nvelx, nlocx, nflux, &
                            nopred)
                enddo

                itel0 = itel
                do ioutput = 1, proc%no_output
                    itel = itel + 1
                    if (proc%output_item(ioutput)%indx > 0) then
                        itel1 = itel0 + proc%output_item(ioutput)%indx
                    else
                        itel1 = itel
                        write(*, *) 'geen volgorde voor proces:', proc%name
                    endif
                    ipmsa(itel1) = 0
                    if (proc%output_item(ioutput)%type == IOTYPE_SEGMENT_OUTPUT .or. &
                            proc%output_item(ioutput)%type == IOTYPE_SEGMENT_WORK) then
                        ipssa(itel1) = proc%output_item(ioutput)%ip_val
                        prvtyp(itel1) = 3
                    else
                        if (proc%output_item(ioutput)%ip_val /= 0) then
                            ipssa(itel1) = ioffx2 + proc%output_item(ioutput)%ip_val
                        else
                            ipssa(itel1) = 0
                        endif
                        prvtyp(itel1) = 4
                    endif
                    call ip2var &
                            (ipssa(itel1), prvvar(itel1), nocons, nopa, nofun, &
                            nosfun, notot, nodisp, novelo, nodef, &
                            noloc, ndspx, nvelx, nlocx, nflux, &
                            nopred)
                enddo

                do ibfl = 1, proc%no_fluxoutput
                    itel = itel + 1
                    ipmsa(itel) = 0
                    ipssa(itel) = 0
                    prvvar(itel) = ivflx + nfluxx + ibfl - 1
                    prvtyp(itel) = 5
                enddo
                nfluxx = nfluxx + proc%no_fluxoutput
            endif
        enddo

        nipmsa = itel

        if (timon) call timstop(ithndl)
        return
    end

    subroutine ip2var (ipin, ivar, nocons, nopa, nofun, &
            nosfun, notot, nodisp, novelo, nodef, &
            noloc, ndspx, nvelx, nlocx, nflux, &
            nopred)

        use timers       !   performance timers
        integer(kind = int_wp) :: nocons, nopa, nofun, nosfun, notot, nodisp, novelo, nodef, &
                noloc, ndspx, nvelx, nlocx, nflux, nopred

        integer(kind = int_wp) :: ithndl = 0
        integer(kind = int_wp) :: ipin, ivar

        !     local
        integer(kind = int_wp) :: ivvol, ivare, ivflo, ivlen, ivcns, ivpar, ivfun
        integer(kind = int_wp) :: ivsfu, ivcnc, ivmas, ivder, ivdsp, ivvel, ivdef
        integer(kind = int_wp) :: ivloc, ivdsx, ivvlx, ivlcx, ivflx, ip, ioff

        if (timon) call timstrt("ip2var", ithndl)

        ! a bit the other way around but set now the new variable pointers
        ! later we will do this different

        ivvol = 1
        ivare = ivvol + 1
        ivflo = ivare + 1
        ivlen = ivflo + 1
        ivcns = ivlen + 2
        ivpar = ivcns + nocons
        ivfun = ivpar + nopa
        ivsfu = ivfun + nofun
        ivcnc = ivsfu + nosfun
        ivmas = ivcnc + notot
        ivder = ivmas + notot
        ivdsp = ivder + notot
        ivvel = ivdsp + nodisp
        ivdef = ivvel + novelo
        ivloc = ivdef + nodef
        ivdsx = ivloc + noloc
        ivvlx = ivdsx + ndspx
        ivlcx = ivvlx + nvelx
        ivflx = ivlcx + nlocx

        ip = ipin
        if (ip == 1) then

            ! volume

            ivar = ivvol
            goto 65
        endif
        if (ip <= nopred) then

            ! default, pre-defined

            if (ip == 0) ip = 1
            ivar = ivdef + ip - 1
            goto 65
        endif
        ioff = nopred
        if (ip <= nocons + ioff) then

            ! constant

            ivar = ivcns + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + nocons
        if (ip <= nopa + ioff) then

            ! parameter

            ivar = ivpar + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + nopa
        if (ip <= nofun + ioff) then

            ! function

            ivar = ivfun + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + nofun
        if (ip <= nosfun + ioff) then

            ! segment function

            ivar = ivsfu + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + nosfun
        if (ip <= notot + ioff) then

            ! concentration

            ivar = ivcnc + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + notot
        if (ip <= noloc + ioff) then

            ! local

            ivar = ivloc + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + noloc
        if (ip <= nodef + ioff) then

            ! default

            ivar = ivdef + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + nodef
        if (ip <= nflux + ioff) then

            ! flux

            ivar = ivflx + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + nflux
        if (ip <= 1 + ioff) then

            ! flow

            ivar = ivflo + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + 1
        if (ip <= 1 + ioff) then

            ! area

            ivar = ivare + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + 1
        if (ip <= 2 + ioff) then

            ! length

            ivar = ivlen + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + 2
        if (ip <= nodisp + ioff) then

            ! dispersion

            ivar = ivdsp + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + nodisp
        if (ip <= novelo + ioff) then

            ! velocity

            ivar = ivvel + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + novelo
        if (ip <= nofun + ioff) then

            ! function

            ivar = ivfun + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + nofun
        if (ip <= nocons + ioff) then

            ! constant

            ivar = ivcns + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + nocons
        if (ip <= ndspx + ioff) then

            ! extra dispersion

            ivar = ivdsx + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + ndspx
        if (ip <= nvelx + ioff) then

            ! extra velocity

            ivar = ivvlx + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + nvelx
        if (ip <= nlocx + ioff) then

            ! local on exchange

            ivar = ivlcx + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + nlocx
        if (ip > ioff) then

            ! local on exchange

            ivar = ivdef + ip - ioff - 1
            goto 65
        endif
        65 continue

        if (timon) call timstop(ithndl)
        return
    end

end module m_intoou
