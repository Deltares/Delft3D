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
module m_expand_frc
    use m_waq_precision
    use m_string_utils

    implicit none

contains


    subroutine expand_frc (lunrep, procesdef, allitems, sfracs)

        ! expand processes per fractions

        use processet
        use timers       !   performance timers

        implicit none

        ! declaration of arguments

        integer(kind = int_wp) :: lunrep          ! report file
        type(procespropcoll) :: procesdef       ! the process definition
        type(itempropcoll) :: allitems        ! all items of the proces system
        type(sfracsprop) :: sfracs          ! substance fraction properties

        ! local decalarations

        type(procesprop), pointer :: proc            ! single process
        type(ioitemprop), pointer :: new_item(:)     ! expanded list of items
        type(stochiprop), pointer :: new_stochi(:)   ! expanded list of items
        type(itemprop) :: item            ! one item
        integer(kind = int_wp) :: nproc           ! number of processes
        integer(kind = int_wp) :: iproc           ! loop counter processes
        integer(kind = int_wp) :: isfrac          ! index substance fractions
        integer(kind = int_wp) :: isfrac2         ! index substance fractions
        integer(kind = int_wp) :: nfrac           ! number fractions in substance fraction
        character(len = 20) :: basnam          ! base name substance fractions
        character(len = 20) :: fracnam         ! name of substance fraction
        integer(kind = int_wp) :: i_item          ! index input item
        integer(kind = int_wp) :: i_flux          ! index flux item
        integer(kind = int_wp) :: i_stochi        ! index stochi
        integer(kind = int_wp) :: indx            ! index in list
        integer(kind = int_wp) :: iret            ! index in collection
        integer(kind = int_wp) :: ifrac           ! fraction number
        character(len = 3) :: suffix          ! suffix
        integer(kind = int_wp) :: ierr_alloc      ! error indication

        logical :: l_expand              ! expand item list
        logical :: l_frac(sfracs%nsfrac) ! fraction involved in process

        integer(kind = int_wp) :: no_input_new          ! new number of items
        integer(kind = int_wp) :: no_output_new         ! new number of items
        integer(kind = int_wp) :: no_fluxoutput_new     ! new number of items
        integer(kind = int_wp) :: no_fluxstochi_new     ! new number of items
        integer(kind = int_wp) :: no_velostochi_new     ! new number of items
        integer(kind = int_wp) :: no_dispstochi_new     ! new number of items
        integer(kind = int_wp) :: i_new                 ! index in new items
        integer(kind = int_wp) :: isfrac_found          ! index substance fractions
        integer(kind = int_wp) :: i_star                ! index of * in name
        integer(kind = int_wp) :: nzoek                 ! nzoek
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("expand_frc", ithndl)

        ! loop over the processes

        nproc = procesdef%current_size
        do iproc = 1, nproc

            proc => procesdef%procesprops(iproc)

            ! check for expansion and for the fractions involved

            l_expand = .false.
            l_frac = .false.
            do i_item = 1, proc%no_input
                if (proc%input_item(i_item)%name(1:1) == '*') then
                    do isfrac = 1, sfracs%nsfrac
                        basnam = sfracs%name(isfrac)
                        if (string_equals(basnam(1:19), proc%input_item(i_item)%name(2:))) then
                            l_expand = .true.
                            l_frac(isfrac) = .true.
                            exit
                        endif
                    enddo
                endif
            enddo

            if (l_expand) then

                ! count the dimensions of the expanded process

                no_input_new = proc%no_input
                do i_item = 1, proc%no_input

                    ! check for the fractions itself

                    do isfrac = 1, sfracs%nsfrac
                        if (l_frac(isfrac)) then
                            if (string_equals(sfracs%name(isfrac), proc%input_item(i_item)%name)) then
                                no_input_new = no_input_new + sfracs%nfrac(isfrac) - 1
                                exit
                            endif
                        endif
                    enddo

                    ! check for input which needs to be expanded

                    i_star = index(proc%input_item(i_item)%name, '*')
                    if (i_star > 1) then
                        do isfrac = 1, sfracs%nsfrac
                            nzoek = 20 - i_star
                            if (string_equals(sfracs%name(isfrac)(1:nzoek), proc%input_item(i_item)%name(i_star + 1:))) then
                                no_input_new = no_input_new + sfracs%nfrac(isfrac) - 1
                                exit
                            endif
                        enddo
                    endif

                enddo

                no_output_new = proc%no_output
                do i_item = 1, proc%no_output
                    i_star = index(proc%output_item(i_item)%name, '*')
                    if (i_star > 0) then
                        do isfrac = 1, sfracs%nsfrac
                            nzoek = 20 - i_star
                            if (string_equals(sfracs%name(isfrac)(1:nzoek), proc%output_item(i_item)%name(i_star + 1:))) then
                                no_output_new = no_output_new + sfracs%nfrac(isfrac) - 1
                                exit
                            endif
                        enddo
                    endif
                enddo

                no_fluxoutput_new = proc%no_fluxoutput
                do i_flux = 1, proc%no_fluxoutput
                    i_star = index(proc%fluxoutput(i_flux)%name, '*')
                    if (i_star > 0) then
                        do isfrac = 1, sfracs%nsfrac
                            nzoek = 20 - i_star
                            if (string_equals(sfracs%name(isfrac)(1:nzoek), proc%fluxoutput(i_flux)%name(i_star + 1:))) then
                                no_fluxoutput_new = no_fluxoutput_new + sfracs%nfrac(isfrac) - 1
                                exit
                            endif
                        enddo
                    endif
                enddo

                no_fluxstochi_new = proc%no_fluxstochi
                do i_stochi = 1, proc%no_fluxstochi
                    i_star = index(proc%fluxstochi(i_stochi)%ioitem, '*')
                    if (i_star > 0) then
                        do isfrac = 1, sfracs%nsfrac
                            nfrac = sfracs%nfrac(isfrac)
                            basnam = sfracs%name(isfrac)
                            nzoek = 20 - i_star
                            if (string_equals(basnam(1:nzoek), proc%fluxstochi(i_stochi)%ioitem(i_star + 1:))) then
                                no_fluxstochi_new = no_fluxstochi_new + nfrac - 1
                                exit
                            endif
                        enddo
                    endif
                enddo

                no_velostochi_new = proc%no_velostochi
                do i_item = 1, proc%no_velostochi
                    i_star = index(proc%velostochi(i_item)%ioitem, '*')
                    if (i_star > 0) then
                        do isfrac = 1, sfracs%nsfrac
                            nzoek = 20 - i_star
                            if (string_equals(sfracs%name(isfrac)(1:nzoek), proc%velostochi(i_item)%ioitem(i_star + 1:))) then
                                no_velostochi_new = no_velostochi_new + sfracs%nfrac(isfrac) - 1
                                exit
                            endif
                        enddo
                    endif
                enddo

                no_dispstochi_new = proc%no_dispstochi
                do i_item = 1, proc%no_dispstochi
                    i_star = index(proc%dispstochi(i_item)%ioitem, '*')
                    if (i_star > 0) then
                        do isfrac = 1, sfracs%nsfrac
                            nzoek = 20 - i_star
                            if (string_equals(sfracs%name(isfrac)(1:nzoek), proc%dispstochi(i_item)%ioitem(i_star + 1:))) then
                                no_dispstochi_new = no_dispstochi_new + sfracs%nfrac(isfrac) - 1
                                exit
                            endif
                        enddo
                    endif
                enddo

                ! then expand

                write(lunrep, 2000) ' expanding fraction input for process [', proc%name, ']'

                allocate(new_item(no_input_new))
                i_new = 0
                do i_item = 1, proc%no_input

                    ! check for fraction (i_star = 0 then the substance fraction itself, i_star > 1 then process parameter)

                    isfrac_found = 0
                    i_star = index(proc%input_item(i_item)%name, '*')
                    do isfrac = 1, sfracs%nsfrac
                        if (l_frac(isfrac)) then
                            nzoek = 20 - i_star
                            if (string_equals(sfracs%name(isfrac)(1:nzoek), proc%input_item(i_item)%name(i_star + 1:))) then
                                isfrac_found = isfrac
                                exit
                            endif
                        endif
                    enddo

                    if (i_star == 1) then

                        ! number of fractions

                        i_new = i_new + 1
                        new_item(i_new) = proc%input_item(i_item)
                        new_item(i_new)%indx = i_new
                        if (isfrac_found > 0) then
                            new_item(i_new)%actdef = sfracs%nfrac(isfrac_found)
                        endif

                    elseif (isfrac_found > 0) then

                        ! expand input

                        nfrac = sfracs%nfrac(isfrac_found)
                        basnam = sfracs%name(isfrac_found)
                        do ifrac = 1, nfrac
                            if (ifrac < 100) then
                                write(suffix, '(i2.2)') ifrac
                            else
                                write(suffix, '(i3.3)') ifrac
                            endif
                            fracnam = trim(basnam) // trim(suffix)

                            i_new = i_new + 1
                            new_item(i_new) = proc%input_item(i_item)
                            new_item(i_new)%indx = i_new
                            if (i_star == 0) then
                                new_item(i_new)%name = fracnam
                            else
                                new_item(i_new)%name = trim(proc%input_item(i_item)%name(1:i_star)) // fracnam
                            endif
                            item%name = new_item(i_new)%name
                            iret = itempropcollfind(allitems, item)
                            if (iret <= 0) then
                                item%text = proc%input_item(i_item)%item%text
                                item%default = proc%input_item(i_item)%item%default
                                item%waqtype = proc%input_item(i_item)%item%waqtype
                                iret = itempropcolladd(allitems, item)
                            endif
                            new_item(i_new)%item => allitems%itemproppnts(iret)%pnt
                        enddo
                    else

                        ! just copy input

                        i_new = i_new + 1
                        new_item(i_new) = proc%input_item(i_item)
                        new_item(i_new)%indx = i_new

                    endif

                enddo
                if (proc%no_input > 0) deallocate(proc%input_item)
                proc%input_item => new_item
                proc%no_input = no_input_new

                ! output

                i_new = 0
                allocate(new_item(no_output_new))
                do i_item = 1, proc%no_output
                    isfrac_found = 0
                    i_star = index(proc%output_item(i_item)%name, '*')
                    if (i_star > 0) then
                        do isfrac = 1, sfracs%nsfrac
                            nzoek = 20 - i_star
                            if (string_equals(sfracs%name(isfrac)(1:nzoek), proc%output_item(i_item)%name(i_star + 1:))) then
                                isfrac_found = isfrac
                                exit
                            endif
                        enddo
                    endif
                    if (isfrac_found > 0) then
                        nfrac = sfracs%nfrac(isfrac_found)
                        basnam = sfracs%name(isfrac_found)
                        do ifrac = 1, nfrac
                            if (ifrac < 100) then
                                write(suffix, '(i2.2)') ifrac
                            else
                                write(suffix, '(i3.3)') ifrac
                            endif
                            fracnam = trim(basnam) // trim(suffix)
                            i_new = i_new + 1
                            new_item(i_new) = proc%output_item(i_item)
                            new_item(i_new)%indx = i_new
                            new_item(i_new)%name = trim(proc%output_item(i_item)%name(1:i_star)) // fracnam
                            item%name = new_item(i_new)%name
                            iret = itempropcollfind(allitems, item)
                            if (iret <= 0) then
                                item%text = proc%output_item(i_item)%item%text
                                item%default = proc%output_item(i_item)%item%default
                                item%waqtype = proc%output_item(i_item)%item%waqtype
                                iret = itempropcolladd(allitems, item)
                            endif
                            new_item(i_new)%item => allitems%itemproppnts(iret)%pnt
                        enddo
                    else
                        i_new = i_new + 1
                        new_item(i_new) = proc%output_item(i_item)
                        new_item(i_new)%indx = i_new
                    endif
                enddo
                if (proc%no_output > 0) deallocate(proc%output_item)
                proc%output_item => new_item
                proc%no_output = no_output_new

                ! fluxes

                i_new = 0
                allocate(new_item(no_fluxoutput_new))
                do i_item = 1, proc%no_fluxoutput
                    isfrac_found = 0
                    i_star = index(proc%fluxoutput(i_item)%name, '*')
                    if (i_star > 0) then
                        do isfrac = 1, sfracs%nsfrac
                            nzoek = 20 - i_star
                            if (string_equals(sfracs%name(isfrac)(1:nzoek), proc%fluxoutput(i_item)%name(i_star + 1:))) then
                                isfrac_found = isfrac
                                exit
                            endif
                        enddo
                    endif
                    if (isfrac_found > 0) then
                        nfrac = sfracs%nfrac(isfrac_found)
                        basnam = sfracs%name(isfrac_found)
                        do ifrac = 1, nfrac
                            if (ifrac < 100) then
                                write(suffix, '(i2.2)') ifrac
                            else
                                write(suffix, '(i3.3)') ifrac
                            endif
                            fracnam = trim(basnam) // trim(suffix)
                            i_new = i_new + 1
                            new_item(i_new) = proc%fluxoutput(i_item)
                            new_item(i_new)%indx = i_new
                            new_item(i_new)%name = trim(proc%fluxoutput(i_item)%name(1:i_star)) // fracnam
                            item%name = new_item(i_new)%name
                            iret = itempropcollfind(allitems, item)
                            if (iret <= 0) then
                                item%text = proc%fluxoutput(i_item)%item%text
                                item%default = proc%fluxoutput(i_item)%item%default
                                item%waqtype = proc%fluxoutput(i_item)%item%waqtype
                                iret = itempropcolladd(allitems, item)
                            endif
                            new_item(i_new)%item => allitems%itemproppnts(iret)%pnt
                        enddo
                    else
                        i_new = i_new + 1
                        new_item(i_new) = proc%fluxoutput(i_item)
                        new_item(i_new)%indx = i_new
                    endif
                enddo
                if (proc%no_fluxoutput > 0) deallocate(proc%fluxoutput)
                proc%fluxoutput => new_item
                proc%no_fluxoutput = no_fluxoutput_new

                ! flux stochi

                i_new = 0
                allocate(new_stochi(no_fluxstochi_new))
                do i_stochi = 1, proc%no_fluxstochi
                    i_star = index(proc%fluxstochi(i_stochi)%ioitem, '*')
                    if (i_star > 0) then
                        do isfrac = 1, sfracs%nsfrac
                            nfrac = sfracs%nfrac(isfrac)
                            basnam = sfracs%name(isfrac)
                            nzoek = 20 - i_star
                            if (string_equals(basnam(1:nzoek), proc%fluxstochi(i_stochi)%ioitem(i_star + 1:))) then
                                isfrac_found = isfrac
                                exit
                            endif
                        enddo
                        if (isfrac_found > 0) then
                            do ifrac = 1, nfrac
                                if (ifrac < 100) then
                                    write(suffix, '(i2.2)') ifrac
                                else
                                    write(suffix, '(i3.3)') ifrac
                                endif
                                fracnam = trim(basnam) // trim(suffix)
                                i_new = i_new + 1
                                new_stochi(i_new) = proc%fluxstochi(i_stochi)
                                new_stochi(i_new)%ioitem = trim(proc%fluxstochi(i_stochi)%ioitem(1:i_star)) // fracnam
                                if (string_equals(basnam, proc%fluxstochi(i_stochi)%substance)) then
                                    new_stochi(i_new)%substance = fracnam
                                else
                                    ! look for linked substance (if not found the substance name stays the same
                                    do isfrac2 = 1, nfrac
                                        if (string_equals(sfracs%name(isfrac2), proc%fluxstochi(i_stochi)%substance) &
                                                .and. sfracs%linklist(isfrac, isfrac2) == 1) then
                                            new_stochi(i_new)%substance = trim(sfracs%name(isfrac2)) // suffix
                                            exit
                                        endif
                                    enddo
                                endif
                            enddo
                            isfrac_found = 0
                        else
                            i_new = i_new + 1
                            new_stochi(i_new) = proc%fluxstochi(i_stochi)
                        endif
                    else
                        i_new = i_new + 1
                        new_stochi(i_new) = proc%fluxstochi(i_stochi)
                    endif
                enddo
                if (proc%no_fluxstochi > 0) deallocate(proc%fluxstochi)
                proc%fluxstochi => new_stochi
                proc%no_fluxstochi = no_fluxstochi_new

                ! disp stochi

                i_new = 0
                allocate(new_stochi(no_dispstochi_new))
                do i_stochi = 1, proc%no_dispstochi
                    i_star = index(proc%dispstochi(i_stochi)%ioitem, '*')
                    if (i_star > 0) then
                        do isfrac = 1, sfracs%nsfrac
                            nfrac = sfracs%nfrac(isfrac)
                            basnam = sfracs%name(isfrac)
                            nzoek = 20 - i_star
                            if (string_equals(basnam(1:nzoek), proc%dispstochi(i_stochi)%ioitem(i_star + 1:))) then
                                isfrac_found = isfrac
                                exit
                            endif
                        enddo
                        if (isfrac_found > 0) then
                            do ifrac = 1, nfrac
                                if (ifrac < 100) then
                                    write(suffix, '(i2.2)') ifrac
                                else
                                    write(suffix, '(i3.3)') ifrac
                                endif
                                fracnam = trim(basnam) // trim(suffix)
                                i_new = i_new + 1
                                new_stochi(i_new) = proc%dispstochi(i_stochi)
                                new_stochi(i_new)%ioitem = trim(proc%dispstochi(i_stochi)%ioitem(1:i_star)) // fracnam
                                if (string_equals(basnam, proc%dispstochi(i_stochi)%substance)) then
                                    new_stochi(i_new)%substance = fracnam
                                else
                                    ! look for linked substance (if not found the substance name stays the same
                                    do isfrac2 = 1, nfrac
                                        if (string_equals(sfracs%name(isfrac2), proc%dispstochi(i_stochi)%substance) &
                                                .and. sfracs%linklist(isfrac, isfrac2) == 1) then
                                            new_stochi(i_new)%substance = trim(sfracs%name(isfrac2)) // suffix
                                            exit
                                        endif
                                    enddo
                                endif
                            enddo
                            isfrac_found = 0
                        else
                            i_new = i_new + 1
                            new_stochi(i_new) = proc%dispstochi(i_stochi)
                        endif
                    else
                        i_new = i_new + 1
                        new_stochi(i_new) = proc%dispstochi(i_stochi)
                    endif
                enddo
                if (proc%no_dispstochi > 0) deallocate(proc%dispstochi)
                proc%dispstochi => new_stochi
                proc%no_dispstochi = no_dispstochi_new

                ! velo stochi

                i_new = 0
                allocate(new_stochi(no_velostochi_new))
                do i_stochi = 1, proc%no_velostochi
                    i_star = index(proc%velostochi(i_stochi)%ioitem, '*')
                    if (i_star > 0) then
                        do isfrac = 1, sfracs%nsfrac
                            nfrac = sfracs%nfrac(isfrac)
                            basnam = sfracs%name(isfrac)
                            nzoek = 20 - i_star
                            if (string_equals(basnam(1:nzoek), proc%velostochi(i_stochi)%ioitem(i_star + 1:))) then
                                isfrac_found = isfrac
                                exit
                            endif
                        enddo
                        if (isfrac_found > 0) then
                            do ifrac = 1, nfrac
                                if (ifrac < 100) then
                                    write(suffix, '(i2.2)') ifrac
                                else
                                    write(suffix, '(i3.3)') ifrac
                                endif
                                fracnam = trim(basnam) // trim(suffix)
                                i_new = i_new + 1
                                new_stochi(i_new) = proc%velostochi(i_stochi)
                                new_stochi(i_new)%ioitem = trim(proc%velostochi(i_stochi)%ioitem(1:i_star)) // fracnam
                                if (string_equals(basnam, proc%velostochi(i_stochi)%substance)) then
                                    new_stochi(i_new)%substance = fracnam
                                else
                                    ! look for linked substance (if not found the substance name stays the same
                                    do isfrac2 = 1, nfrac
                                        if (string_equals(sfracs%name(isfrac2), proc%velostochi(i_stochi)%substance) &
                                                .and. sfracs%linklist(isfrac, isfrac2) == 1) then
                                            new_stochi(i_new)%substance = trim(sfracs%name(isfrac2)) // suffix
                                            exit
                                        endif
                                    enddo
                                endif
                            enddo
                            isfrac_found = 0
                        else
                            i_new = i_new + 1
                            new_stochi(i_new) = proc%velostochi(i_stochi)
                        endif
                    else
                        i_new = i_new + 1
                        new_stochi(i_new) = proc%velostochi(i_stochi)
                    endif
                enddo
                if (proc%no_velostochi > 0) deallocate(proc%velostochi)
                proc%velostochi => new_stochi
                proc%no_velostochi = no_velostochi_new

            endif

        enddo

        if (timon) call timstop(ithndl)
        return
        2000 format (3a)
    end

end module m_expand_frc
