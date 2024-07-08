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


    subroutine intoou (procesdef, num_processes_activated, num_fluxes, nsvar, pronam, &
            iflux, process_space_int, ipssa, process_space_int_len, ioffx, &
            num_constants, num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, num_substances_total, &
            num_dispersion_arrays, num_velocity_arrays, num_defaults, num_local_vars, num_dispersion_arrays_extra, &
            num_velocity_arrays_extra, num_local_vars_exchange, nopred, prvvar, prvtyp, &
            num_vars, progrd, prondt)

        ! maps input structure to output structure

        use processet
        use timers       !   performance timers

        implicit none

        ! declaration of arguments

        type(procespropcoll) :: procesdef       ! all processes
        integer(kind = int_wp) :: num_processes_activated           ! number of processes (active)
        integer(kind = int_wp) :: num_fluxes           ! number of fluxes
        integer(kind = int_wp) :: nsvar(*)        ! number of variables
        character(len = *) :: pronam(*)       ! routine name
        integer(kind = int_wp) :: iflux(*)        ! index in flux array
        integer(kind = int_wp) :: process_space_int(*)        ! index in process_space_real array
        integer(kind = int_wp) :: ipssa(*)        ! index in ssa  array
        integer(kind = int_wp) :: process_space_int_len          ! length process_space_real array
        integer(kind = int_wp) :: ioff            ! offset in pointer for segment items
        integer(kind = int_wp) :: ioffx           ! offset in pointer for exchange items
        integer(kind = int_wp) :: num_constants          ! number of constants
        integer(kind = int_wp) :: num_spatial_parameters            ! number of parameters
        integer(kind = int_wp) :: num_time_functions           ! number of functions
        integer(kind = int_wp) :: num_spatial_time_fuctions          ! number of segment functions
        integer(kind = int_wp) :: num_substances_total           ! number of substances
        integer(kind = int_wp) :: num_dispersion_arrays          ! number of dispersions
        integer(kind = int_wp) :: num_velocity_arrays          ! number of velocities
        integer(kind = int_wp) :: num_defaults           ! number of default
        integer(kind = int_wp) :: num_local_vars           ! number of local values
        integer(kind = int_wp) :: num_dispersion_arrays_extra           ! number of dispersions
        integer(kind = int_wp) :: num_velocity_arrays_extra           ! number of velocities
        integer(kind = int_wp) :: num_local_vars_exchange
        integer(kind = int_wp) :: nopred          ! number of predfined values
        integer(kind = int_wp) :: prvvar(*)       ! variable pointer
        integer(kind = int_wp) :: prvtyp(*)       ! type of variable
        integer(kind = int_wp) :: num_vars           ! number of variables
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
        integer(kind = int_wp) :: nfluxx          ! second counter num_fluxes
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("intoou", ithndl)

        ! first calculate num_processes_activated, num_fluxes

        num_processes_activated = 0
        num_fluxes = 0
        nproctot = procesdef%current_size
        do iproc = 1, nproctot
            proc => procesdef%procesprops(iproc)
            if (proc%active) then
                num_processes_activated = num_processes_activated + 1
                num_fluxes = num_fluxes + proc%no_fluxoutput
            endif
        enddo
        ioffx2 = ioffx + num_fluxes

        ! set pointer index to the delwaq arrays

        ivvol = 1
        ivare = ivvol + 1
        ivflo = ivare + 1
        ivlen = ivflo + 1
        ivcns = ivlen + 2
        ivpar = ivcns + num_constants
        ivfun = ivpar + num_spatial_parameters
        ivsfu = ivfun + num_time_functions
        ivcnc = ivsfu + num_spatial_time_fuctions
        ivmas = ivcnc + num_substances_total
        ivder = ivmas + num_substances_total
        ivdsp = ivder + num_substances_total
        ivvel = ivdsp + num_dispersion_arrays
        ivdef = ivvel + num_velocity_arrays
        ivloc = ivdef + num_defaults
        ivdsx = ivloc + num_local_vars
        ivvlx = ivdsx + num_dispersion_arrays_extra
        ivlcx = ivvlx + num_velocity_arrays_extra
        ivflx = ivlcx + num_local_vars_exchange
        num_vars = ivflx + num_fluxes - 1

        ! initialize totals , including num_fluxes again (principally the same)

        itel = 0
        nfluxx = 0

        ! loop over processes

        nproctot = procesdef%current_size
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

                ! map in- and output pointers on process_space_int

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
                            ioff = nopred + num_constants + num_spatial_parameters + num_time_functions + num_spatial_time_fuctions + num_substances_total + num_local_vars
                            proc%input_item(i_input)%ip_val = ioff + num_defaults - 2 * num_processes_activated + iproc_act
                        endif
                        if (proc%input_item(i_input)%ip_val == 4) then
                            ! delt, voor fractional step aparte ingang in default array
                            ioff = nopred + num_constants + num_spatial_parameters + num_time_functions + num_spatial_time_fuctions + num_substances_total + num_local_vars
                            proc%input_item(i_input)%ip_val = ioff + num_defaults - num_processes_activated + iproc_act
                        endif
                        process_space_int(itel1) = proc%input_item(i_input)%ip_val
                        prvtyp(itel1) = 1
                    else
                        if (proc%input_item(i_input)%ip_val /= 0) then
                            process_space_int(itel1) = ioffx2 + proc%input_item(i_input)%ip_val
                        else
                            process_space_int(itel1) = 0
                        endif
                        prvtyp(itel1) = 2
                    endif
                    ipssa(itel1) = 0
                    call ip2var &
                            (process_space_int(itel1), prvvar(itel1), num_constants, num_spatial_parameters, num_time_functions, &
                            num_spatial_time_fuctions, num_substances_total, num_dispersion_arrays, num_velocity_arrays, num_defaults, &
                            num_local_vars, num_dispersion_arrays_extra, num_velocity_arrays_extra, num_local_vars_exchange, num_fluxes, &
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
                    process_space_int(itel1) = 0
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
                            (ipssa(itel1), prvvar(itel1), num_constants, num_spatial_parameters, num_time_functions, &
                            num_spatial_time_fuctions, num_substances_total, num_dispersion_arrays, num_velocity_arrays, num_defaults, &
                            num_local_vars, num_dispersion_arrays_extra, num_velocity_arrays_extra, num_local_vars_exchange, num_fluxes, &
                            nopred)
                enddo

                do ibfl = 1, proc%no_fluxoutput
                    itel = itel + 1
                    process_space_int(itel) = 0
                    ipssa(itel) = 0
                    prvvar(itel) = ivflx + nfluxx + ibfl - 1
                    prvtyp(itel) = 5
                enddo
                nfluxx = nfluxx + proc%no_fluxoutput
            endif
        enddo

        process_space_int_len = itel

        if (timon) call timstop(ithndl)
        return
    end

    subroutine ip2var (ipin, ivar, num_constants, num_spatial_parameters, num_time_functions, &
            num_spatial_time_fuctions, num_substances_total, num_dispersion_arrays, num_velocity_arrays, num_defaults, &
            num_local_vars, num_dispersion_arrays_extra, num_velocity_arrays_extra, num_local_vars_exchange, num_fluxes, &
            nopred)

        use timers       !   performance timers
        integer(kind = int_wp) :: num_constants, num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, num_substances_total, num_dispersion_arrays, num_velocity_arrays, num_defaults, &
                num_local_vars, num_dispersion_arrays_extra, num_velocity_arrays_extra, num_local_vars_exchange, num_fluxes, nopred

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
        ivpar = ivcns + num_constants
        ivfun = ivpar + num_spatial_parameters
        ivsfu = ivfun + num_time_functions
        ivcnc = ivsfu + num_spatial_time_fuctions
        ivmas = ivcnc + num_substances_total
        ivder = ivmas + num_substances_total
        ivdsp = ivder + num_substances_total
        ivvel = ivdsp + num_dispersion_arrays
        ivdef = ivvel + num_velocity_arrays
        ivloc = ivdef + num_defaults
        ivdsx = ivloc + num_local_vars
        ivvlx = ivdsx + num_dispersion_arrays_extra
        ivlcx = ivvlx + num_velocity_arrays_extra
        ivflx = ivlcx + num_local_vars_exchange

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
        if (ip <= num_constants + ioff) then

            ! constant

            ivar = ivcns + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + num_constants
        if (ip <= num_spatial_parameters + ioff) then

            ! parameter

            ivar = ivpar + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + num_spatial_parameters
        if (ip <= num_time_functions + ioff) then

            ! function

            ivar = ivfun + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + num_time_functions
        if (ip <= num_spatial_time_fuctions + ioff) then

            ! segment function

            ivar = ivsfu + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + num_spatial_time_fuctions
        if (ip <= num_substances_total + ioff) then

            ! concentration

            ivar = ivcnc + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + num_substances_total
        if (ip <= num_local_vars + ioff) then

            ! local

            ivar = ivloc + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + num_local_vars
        if (ip <= num_defaults + ioff) then

            ! default

            ivar = ivdef + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + num_defaults
        if (ip <= num_fluxes + ioff) then

            ! flux

            ivar = ivflx + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + num_fluxes
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
        if (ip <= num_dispersion_arrays + ioff) then

            ! dispersion

            ivar = ivdsp + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + num_dispersion_arrays
        if (ip <= num_velocity_arrays + ioff) then

            ! velocity

            ivar = ivvel + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + num_velocity_arrays
        if (ip <= num_time_functions + ioff) then

            ! function

            ivar = ivfun + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + num_time_functions
        if (ip <= num_constants + ioff) then

            ! constant

            ivar = ivcns + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + num_constants
        if (ip <= num_dispersion_arrays_extra + ioff) then

            ! extra dispersion

            ivar = ivdsx + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + num_dispersion_arrays_extra
        if (ip <= num_velocity_arrays_extra + ioff) then

            ! extra velocity

            ivar = ivvlx + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + num_velocity_arrays_extra
        if (ip <= num_local_vars_exchange + ioff) then

            ! local on exchange

            ivar = ivlcx + ip - ioff - 1
            goto 65
        endif
        ioff = ioff + num_local_vars_exchange
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
