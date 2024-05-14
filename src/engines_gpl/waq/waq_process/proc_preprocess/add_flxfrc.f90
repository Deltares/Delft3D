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
module m_add_flxfrc
    use m_waq_precision
    use m_string_utils

    implicit none

contains


    subroutine add_flxfrc(lunrep, procesdef, allitems, sfracs, no_act, &
            actlst, nbpr)

        ! add the fluxes to the fractions by adding a distribution process

        use m_logger, only : terminate_execution
        use ProcesSet
        use timers       !   performance timers

        implicit none

        ! decalaration of arguments

        integer(kind = int_wp) :: lunrep          ! report file
        type(procespropcoll) :: procesdef       ! the process definition
        type(itempropcoll) :: allitems        ! all items of the proces system
        type(sfracsprop) :: sfracs          ! substance fraction properties
        integer(kind = int_wp) :: no_act          ! number of active processes
        character(len = *) :: actlst(*)       ! active processes names
        integer(kind = int_wp) :: nbpr            ! number of processes

        ! local declaration

        type(procesprop), pointer :: proc            ! single process
        type(procesprop) :: procn           ! process to be added
        type(itemprop) :: item            ! one item
        integer(kind = int_wp) :: nproc           ! number of processes
        integer(kind = int_wp) :: iproc           ! loop counter processes
        integer(kind = int_wp) :: iproc_new       ! index inserted process
        integer(kind = int_wp) :: isfrac          ! index substance fractions
        integer(kind = int_wp) :: isfrac_2        ! index substance fractions
        integer(kind = int_wp) :: nfrac           ! number fractions in substance fraction
        integer(kind = int_wp) :: nlink           ! number of linked substance fractions
        integer(kind = int_wp) :: ilink           ! index of linked substance fractions
        integer(kind = int_wp) :: linklst(sfracs%nsfrac) ! index linked substance fractions
        character(len = 20) :: basnam          ! base name substance fractions
        integer(kind = int_wp) :: istochi         ! index stochi
        integer(kind = int_wp) :: istochi_2       ! index stochi
        integer(kind = int_wp) :: isfrac_positive ! fraction to be used if flux is positive
        integer(kind = int_wp) :: isfrac_negative ! fraction to be used if flux is negative
        integer(kind = int_wp) :: iret            ! index in collection
        integer(kind = int_wp) :: ifrac           ! fraction number
        character(len = 3) :: suffix          ! suffix
        integer(kind = int_wp) :: ierr_alloc      ! error indication
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("add_flxfrc", ithndl)

        ! loop over the processes

        nproc = procesdef%current_size
        do iproc = 1, nproc

            proc => procesdef%procesprops(iproc)

            ! check for fluxes on substances with fractions

            do isfrac = 1, sfracs%nsfrac

                nfrac = sfracs%nfrac(isfrac)
                basnam = sfracs%name(isfrac)
                nlink = 0
                if (sfracs%linked(isfrac) /= 0) then
                    do isfrac_2 = 1, sfracs%nsfrac
                        if (isfrac_2 /= isfrac) then
                            if (sfracs%linked(isfrac_2) == sfracs%linked(isfrac)) then
                                nlink = nlink + 1
                                linklst(nlink) = isfrac_2
                            endif
                        endif
                    enddo
                endif

                do istochi = 1, proc%no_fluxstochi

                    ! skip dummy rules, factor equal zero

                    if (abs(proc%fluxstochi(istochi)%scale) > 1e-10) then

                        if (string_equals(basnam(1:10), proc%fluxstochi(istochi)%substance)) then

                            ! flux found, check if flux has to be split

                            if (procesdef%procesprops(iproc)%sfrac_type == SFRAC_SPLITFLUX) then

                                ! check which fraction is used for the split

                                if (proc%fluxstochi(istochi)%scale > 0.0) then

                                    ! for positive flux, check if the fraction is linked with flux with negative stochi on the same flux, best is equal scale, all this is speculation

                                    isfrac_negative = isfrac
                                    isfrac_positive = 0
                                    do istochi_2 = 1, proc%no_fluxstochi
                                        if (istochi_2 == istochi) cycle
                                        if (proc%fluxstochi(istochi_2)%ioitem ==  proc%fluxstochi(istochi)%ioitem) then
                                            do ilink = 1, nlink
                                                if (string_equals(sfracs%name(linklst(ilink))(1:10), proc%fluxstochi(istochi_2)%substance)) then
                                                    if (abs(proc%fluxstochi(istochi_2)%scale + proc%fluxstochi(istochi)%scale) < 1e-20) then

                                                        ! same flux same -scale, set fraction and exit link loop

                                                        isfrac_positive = linklst(ilink)
                                                        goto 10

                                                    elseif (proc%fluxstochi(istochi_2)%scale < -1e-20) then

                                                        ! same flux other scale set fraction if not already set and keep looking

                                                        if (isfrac_positive == 0) then
                                                            isfrac_positive = linklst(ilink)
                                                        endif

                                                    endif
                                                endif
                                            enddo
                                        endif
                                    enddo
                                    10                      continue

                                    ! if not found look for any flux with linked negative stochi

                                    if (isfrac_positive == 0) then
                                        do istochi_2 = 1, proc%no_fluxstochi
                                            if (istochi_2 == istochi) cycle
                                            do ilink = 1, nlink
                                                if (string_equals(sfracs%name(linklst(ilink))(1:10), proc%fluxstochi(istochi_2)%substance)) then
                                                    if (proc%fluxstochi(istochi_2)%scale < -1e-20) then

                                                        isfrac_positive = linklst(ilink)
                                                        goto 20

                                                    endif
                                                endif
                                            enddo
                                        enddo
                                        20                         continue
                                    endif

                                    if (isfrac_positive == 0) then
                                        isfrac_positive = isfrac
                                    endif

                                elseif (proc%fluxstochi(istochi)%scale < 0.0) then

                                    ! negative flux, check if the fraction is linked with flux with negative stochi

                                    isfrac_positive = isfrac
                                    isfrac_negative = 0
                                    do istochi_2 = 1, proc%no_fluxstochi
                                        if (istochi_2 == istochi) cycle
                                        if (proc%fluxstochi(istochi_2)%ioitem ==  proc%fluxstochi(istochi)%ioitem) then
                                            do ilink = 1, nlink
                                                if (string_equals(sfracs%name(linklst(ilink))(1:10), proc%fluxstochi(istochi_2)%substance)) then
                                                    if (abs(proc%fluxstochi(istochi_2)%scale + proc%fluxstochi(istochi)%scale) < 1e-20) then

                                                        ! same flux same -scale, set fraction and exit link loop

                                                        isfrac_negative = linklst(ilink)
                                                        goto 30

                                                    elseif (proc%fluxstochi(istochi_2)%scale > 1e-20) then

                                                        ! same flux other scale set fraction if not already set and keep looking

                                                        if (isfrac_negative == 0) then
                                                            isfrac_negative = linklst(ilink)
                                                        endif

                                                    endif
                                                endif
                                            enddo
                                        endif
                                    enddo
                                    30                      continue

                                    ! if not found look for any flux with linked negative stochi

                                    if (isfrac_negative == 0) then
                                        do istochi_2 = 1, proc%no_fluxstochi
                                            if (istochi_2 == istochi) cycle
                                            do ilink = 1, nlink
                                                if (string_equals(sfracs%name(linklst(ilink))(1:10), proc%fluxstochi(istochi_2)%substance)) then
                                                    if (proc%fluxstochi(istochi_2)%scale > 1e-20) then

                                                        isfrac_negative = linklst(ilink)
                                                        goto 40

                                                    endif
                                                endif
                                            enddo
                                        enddo
                                        40                         continue
                                    endif

                                    if (isfrac_negative == 0) then
                                        isfrac_negative = isfrac
                                    endif

                                endif

                                ! construct split process

                                procn%name = 'FRC_' // trim(proc%fluxstochi(istochi)%ioitem) // '*' // basnam
                                procn%routine = 'FLXFRC'
                                procn%text = 'split a flux into fractions'
                                procn%swtransp = 123
                                procn%type = PROCESTYPE_FLUX
                                procn%sfrac_type = 0
                                procn%no_input = 2 + 2 * nfrac
                                procn%no_output = 0
                                procn%no_fluxoutput = nfrac
                                procn%no_fluxstochi = nfrac
                                procn%no_dispstochi = 0
                                procn%no_velostochi = 0
                                allocate(procn%input_item(procn%no_input), procn%fluxoutput(procn%no_fluxoutput), &
                                        procn%fluxstochi(procn%no_fluxstochi), stat = ierr_alloc)
                                if (ierr_alloc /= 0) then
                                    write(lunrep, *) 'error allocating work array in routine add_flxfrc:', ierr_alloc
                                    write(lunrep, *) 'array length:', procn%no_input, procn%no_fluxoutput, procn%no_fluxstochi
                                    write(*, *) 'error allocating array:', ierr_alloc
                                    call terminate_execution(1)
                                endif

                                ! input on segments

                                procn%input_item(1)%name = 'nfrac_' // basnam
                                procn%input_item(1)%type = iotype_segment_input
                                procn%input_item(1)%actdef = nfrac
                                procn%input_item(1)%indx = 1
                                procn%input_item(1)%ip_val = 0
                                item%name = procn%input_item(1)%name
                                iret = itempropcollfind(allitems, item)
                                if (iret <= 0) then
                                    item%text = procn%input_item(1)%name
                                    item%default = nfrac
                                    item%waqtype = waqtype_none
                                    iret = itempropcolladd(allitems, item)
                                endif
                                procn%input_item(1)%item => allitems%itemproppnts(iret)%pnt

                                procn%input_item(2)%name = proc%fluxstochi(istochi)%ioitem
                                procn%input_item(2)%type = iotype_segment_input
                                procn%input_item(2)%actdef = -999.0
                                procn%input_item(2)%indx = 2
                                procn%input_item(2)%ip_val = 0
                                item%name = procn%input_item(2)%name
                                iret = itempropcollfind(allitems, item)
                                if (iret <= 0) then
                                    item%text = procn%input_item(2)%name
                                    item%default = -999.0
                                    item%waqtype = waqtype_none
                                    iret = itempropcolladd(allitems, item)
                                endif
                                procn%input_item(2)%item => allitems%itemproppnts(iret)%pnt

                                do ifrac = 1, nfrac
                                    if (ifrac < 100) then
                                        write(suffix, '(i2.2)') ifrac
                                    else
                                        write(suffix, '(i3.3)') ifrac
                                    endif
                                    procn%input_item(2 + ifrac)%name = 'fr' // trim(sfracs%name(isfrac_positive)) // suffix
                                    procn%input_item(2 + ifrac)%type = iotype_segment_input
                                    procn%input_item(2 + ifrac)%actdef = 0.0
                                    procn%input_item(2 + ifrac)%indx = 2 + ifrac
                                    procn%input_item(2 + ifrac)%ip_val = 0
                                    item%name = procn%input_item(2 + ifrac)%name
                                    iret = itempropcollfind(allitems, item)
                                    if (iret <= 0) then
                                        item%text = procn%input_item(2 + ifrac)%name
                                        item%default = 0.0
                                        item%waqtype = waqtype_none
                                        iret = itempropcolladd(allitems, item)
                                    endif
                                    procn%input_item(2 + ifrac)%item => allitems%itemproppnts(iret)%pnt
                                enddo

                                do ifrac = 1, nfrac
                                    if (ifrac < 100) then
                                        write(suffix, '(i2.2)') ifrac
                                    else
                                        write(suffix, '(i3.3)') ifrac
                                    endif
                                    procn%input_item(2 + nfrac + ifrac)%name = 'fr' // trim(sfracs%name(isfrac_negative)) // suffix
                                    procn%input_item(2 + nfrac + ifrac)%type = iotype_segment_input
                                    procn%input_item(2 + nfrac + ifrac)%actdef = 0.0
                                    procn%input_item(2 + nfrac + ifrac)%indx = 2 + nfrac + ifrac
                                    procn%input_item(2 + nfrac + ifrac)%ip_val = 0
                                    item%name = procn%input_item(2 + nfrac + ifrac)%name
                                    iret = itempropcollfind(allitems, item)
                                    if (iret <= 0) then
                                        item%text = procn%input_item(2 + nfrac + ifrac)%name
                                        item%default = 0.0
                                        item%waqtype = waqtype_none
                                        iret = itempropcolladd(allitems, item)
                                    endif
                                    procn%input_item(2 + nfrac + ifrac)%item => allitems%itemproppnts(iret)%pnt
                                enddo

                                ! split fluxes and stochi

                                do ifrac = 1, nfrac
                                    if (ifrac < 100) then
                                        write(suffix, '(i2.2)') ifrac
                                    else
                                        write(suffix, '(i3.3)') ifrac
                                    endif
                                    procn%fluxoutput(ifrac)%name = trim(proc%fluxstochi(istochi)%ioitem) // '*' // trim(basnam) // suffix
                                    procn%fluxoutput(ifrac)%type = iotype_segment_output
                                    procn%fluxoutput(ifrac)%actdef = 0.0
                                    procn%fluxoutput(ifrac)%indx = ifrac
                                    procn%fluxoutput(ifrac)%ip_val = 0
                                    item%name = procn%fluxoutput(ifrac)%name
                                    iret = itempropcollfind(allitems, item)
                                    if (iret <= 0) then
                                        item%text = procn%fluxoutput(ifrac)%name
                                        item%default = -999.
                                        item%waqtype = waqtype_none
                                        iret = itempropcolladd(allitems, item)
                                    endif
                                    procn%fluxoutput(ifrac)%item => allitems%itemproppnts(iret)%pnt

                                    procn%fluxstochi(ifrac)%type = STOCHITYPE_FLUX
                                    procn%fluxstochi(ifrac)%ioitem = procn%fluxoutput(ifrac)%name
                                    procn%fluxstochi(ifrac)%substance = trim(basnam) // suffix
                                    procn%fluxstochi(ifrac)%subindx = 0
                                    procn%fluxstochi(ifrac)%scale = proc%fluxstochi(istochi)%scale

                                enddo

                                ! insert process at the end

                                iproc_new = procespropcolladd(procesdef, procn)
                                no_act = no_act + 1
                                actlst(no_act) = procn%name
                                nbpr = nbpr + 1
                                write(lunrep, 2000) ' adding splitflux process [', procn%name, ']'

                                ! repointer proc, the procespropcolladd makes this necessary

                                proc => procesdef%procesprops(iproc)

                            endif
                        endif
                    endif
                enddo
            enddo
        enddo

        if (timon) call timstop(ithndl)
        return
        2000 format (3a)
    end

end module m_add_flxfrc
