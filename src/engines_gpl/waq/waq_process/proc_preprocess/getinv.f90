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
module m_getinv
    use m_waq_precision
    use m_string_utils
    use m_vxlpoi
    use m_valpoi

    implicit none

contains


    subroutine getinv (procesdef, notot, syname, nocons, constants, &
            nopa, paname, nofun, funame, nosfun, &
            sfname, nodisp, diname, novelo, vename, &
            nmis, defaul, noloc, nodef, dename, outputs, &
            ndspx, nvelx, nlocx, locnam, refday)

        ! sets the i/o pointers for every proces
        ! if nessacary turns on secondary processes
        ! fills defaults in defaul array

        use m_monsys
        use m_array_manipulation, only : is_missing
        use timers       !   performance timers
        use dlwq_hyd_data
        use processet
        use results, only : OutputPointers
        implicit none

        ! declaration of arguments

        type(procespropcoll) :: procesdef       ! all processes
        integer(kind = int_wp) :: notot           ! number of substances
        character(len = *) :: syname(*)       ! substance name
        integer(kind = int_wp) :: nocons          ! number of constants
        type(t_dlwq_item), intent(inout) :: constants       !< delwaq constants list
        integer(kind = int_wp) :: nopa            ! number of parameters
        character(len = *) :: paname(*)       ! parameter names
        integer(kind = int_wp) :: nofun           ! number of functions
        character(len = *) :: funame(*)       ! function names
        integer(kind = int_wp) :: nosfun          ! number of segment functions
        character(len = *) :: sfname(*)       ! segment function names
        integer(kind = int_wp) :: nodisp          ! number of dispersions
        character(len = *) :: diname(*)       ! dispersion names
        integer(kind = int_wp) :: novelo          ! number of velocities
        character(len = *) :: vename(*)       ! velocity names
        integer(kind = int_wp) :: nmis            ! number of missing inpu items
        real(kind = real_wp) :: defaul(*)       ! default values array
        integer(kind = int_wp) :: noloc           ! number of local values
        integer(kind = int_wp) :: nodef           ! number of default
        character(len = *) :: dename(*)       ! default names
        type(OutputPointers) :: outputs         ! output structure
        integer(kind = int_wp) :: ndspx           ! number of dispersions
        integer(kind = int_wp) :: nvelx           ! number of velocities
        integer(kind = int_wp) :: nlocx           ! number of local values on exchanges
        character(len = *) :: locnam(*)       ! local values names
        integer(kind = int_wp), intent(in) :: refday          ! reference day, varying from 1 till 365

        ! local decalarations

        integer(kind = int_wp) :: nproc           ! number of processes
        integer(kind = int_wp) :: iproc           ! loop counter processes
        integer(kind = int_wp) :: iproc2          ! second loop counter processes
        type(procesprop), pointer :: proc1           ! process description
        type(procesprop), pointer :: proc2           ! description second process
        integer(kind = int_wp) :: i_input         ! index input item
        integer(kind = int_wp) :: ioutput         ! index output item
        integer(kind = int_wp) :: iou             ! index output item
        integer(kind = int_wp) :: ioux            ! index output item
        integer(kind = int_wp) :: nrout           ! number of outputs
        integer(kind = int_wp) :: iflux           ! index flux
        integer(kind = int_wp) :: ivalip          ! index variable in pmsa
        integer(kind = int_wp) :: ioff            ! offset for values in pmsa
        integer(kind = int_wp) :: ioffx           ! offset for values in pmsa
        integer(kind = int_wp) :: idef            ! index defualt
        integer(kind = int_wp) :: nfl             ! flux counter
        character(len = 20) :: valnam          ! variable name
        character(len = 50) :: valtxt          ! variable description
        character(len = 100) :: line            ! output buffer
        character(len = 100) :: line1           ! output buffer
        character(len = 100) :: line2           ! output buffer
        integer(kind = int_wp), parameter :: nopred = 6      ! number of predefined defaults
        real(kind = real_wp), parameter :: rmis0 = -888.  ! missing but no matter (set to 0.0)
        integer(kind = int_wp) :: i_star          ! index of * in name
        integer(kind = int_wp) :: ithndl = 0      ! handle for performance timer

        if (timon) call timstrt("getinv", ithndl)

        ! some init

        ioff = nopred + nocons + nopa + nofun + nosfun + notot
        ioffx = 4 + nodisp + novelo + nofun + nocons + ndspx + nvelx
        line2 = ' '

        write (line, '(a)') '# determining the input for the processes (in reversed order)'
        call monsys(line, 2)
        line = ' '
        call monsys(line, 2)

        ! loop over all possible processes

        nproc = procesdef%cursize
        do iproc = nproc, 1, -1
            proc1 => procesdef%procesprops(iproc)
            if (proc1%active) then
                write (line, '(4a)') ' Input for [', proc1%name, '] ', proc1%text(1:50)
                call monsys(line, 4)

                ! loop over the number of input items for this process

                do i_input = 1, proc1%no_input

                    if (proc1%input_item(i_input)%type == IOTYPE_SEGMENT_INPUT .or. &
                            proc1%input_item(i_input)%type == IOTYPE_SEGMENT_WORK) then

                        valnam = proc1%input_item(i_input)%name
                        valtxt = proc1%input_item(i_input)%item%text
                        write(line, '(4a)') '       [', valnam, '] ', valtxt

                        ! specified in input?

                        10             continue

                        call valpoi (notot, nopa, nosfun, syname, nocons, &
                                nofun, constants, paname, funame, sfname, &
                                valnam, ivalip, line1)

                        ! output earlier in process ? , is this switched on , switch this on

                        if (ivalip == -1) then

                            do iproc2 = 1, iproc

                                proc2 => procesdef%procesprops(iproc2)

                                ! flux van proces

                                call zoekio (valnam, proc2%no_fluxoutput, proc2%fluxoutput, 20, iflux)
                                if (iflux > 0) then
                                    if (proc2%linvok) then
                                        write (line1, '(3a)') '       Using flux from proces [', proc2%name, ']'
                                        if (.not. proc2%active) then
                                            proc2%active = .true.
                                            write (line2, '(3a)') '       switching [', proc2%name, '] on'
                                        endif
                                        ivalip = -2

                                        ! pointers to flux not absolute yet set them later

                                        exit
                                    else
                                        write (line1, '(3a)') '  NOT  Using flux from proces [', proc2%name, ']'
                                    endif
                                endif

                                ! output variable van proces

                                call zoekio (valnam, proc2%no_output, proc2%output_item, 20, ioutput, IOTYPE_SEGMENT_OUTPUT)
                                if (ioutput > 0) then
                                    if (proc2%linvok) then
                                        write (line1, '(3a)') '       Using output from proces [', proc2%name, ']'
                                        if (.not. proc2%active) then
                                            proc2%active = .true.
                                            write (line2, '(3a)') '       switching [', proc2%name, '] on'
                                        endif

                                        ! is output already present in ssa?

                                        if (proc2%output_item(ioutput)%ip_val /= 0) then
                                            ivalip = proc2%output_item(ioutput)%ip_val
                                        else

                                            ! reserve local spot

                                            noloc = noloc + 1
                                            locnam(noloc) = valnam
                                            ivalip = ioff + noloc
                                            proc2%output_item(ioutput)%ip_val = ivalip
                                        endif
                                        exit
                                    else
                                        write(line1, '(3a)') '  NOT  Using output from proces [', proc2%name, ']'
                                    endif
                                endif

                            enddo

                        endif

                        ! work array from this process only

                        if (ivalip == -1 .and. proc1%input_item(i_input)%type == IOTYPE_SEGMENT_WORK) then
                            call zoekio (valnam, proc1%no_output, proc1%output_item, 20, ioutput, IOTYPE_SEGMENT_WORK)
                            if (ioutput > 0) then
                                write (line1, '(3a)') '       Using output from proces [', proc1%name, ']'

                                ! is output already present in ssa?

                                if (proc1%output_item(ioutput)%ip_val /= 0) then
                                    ivalip = proc1%output_item(ioutput)%ip_val
                                else

                                    ! reserve local spot

                                    noloc = noloc + 1
                                    locnam(noloc) = valnam
                                    ivalip = ioff + noloc
                                    proc1%output_item(ioutput)%ip_val = ivalip
                                endif
                            endif
                        endif

                        if (ivalip == -1) then

                            ! if this is a fraction input then first look for the generic name

                            i_star = index(valnam, '*')
                            if (i_star > 1) then
                                valnam(i_star:) = ' '
                                write(line, '(4a)') '       [', valnam, '] ', valtxt
                                goto 10
                            endif

                        endif

                        if (ivalip == -1) then
                            if (is_missing(proc1%input_item(i_input)%actdef))then
                                nmis = nmis + 1
                                write (line1, '(a)')  'error: not in input'
                            else
                                nodef = nodef + 1
                                dename(nodef) = valnam
                                ivalip = -3
                                if (string_equals('RefDay', valnam)) then
                                    defaul(nodef) = real(refday)
                                    write(line1, '(a,g13.6)') '       based on T0-string:', real(refday)
                                else
                                    if (abs(proc1%input_item(i_input)%actdef - rmis0) < 1.e-20)then
                                        line = ' '
                                        defaul(nodef) = 0.0
                                    else
                                        defaul(nodef) = proc1%input_item(i_input)%actdef
                                        write(line1, '(a,g13.6)') '       using default value:', proc1%input_item(i_input)%actdef
                                    endif
                                endif
                            endif
                        endif
                        proc1%input_item(i_input)%ip_val = ivalip
                        if (line  /= ' ') call monsys(line, 4)
                        if (line1 /= ' ') call monsys(line1, 4)
                        if (line2 /= ' ') call monsys(line2, 4)
                        line = ' '
                        line1 = ' '
                        line2 = ' '
                    endif

                enddo


                ! on exchange level, seperate loop, then report file stays compatible

                do i_input = 1, proc1%no_input

                    if (proc1%input_item(i_input)%type == IOTYPE_EXCHANG_INPUT) then

                        valnam = proc1%input_item(i_input)%name
                        valtxt = proc1%input_item(i_input)%item%text
                        write(line, '(4a)') '       [', valnam, '] ', valtxt

                        20             continue

                        ! specified in input?

                        call vxlpoi (nocons, nofun, nodisp, novelo, constants, &
                                funame, diname, vename, valnam, ivalip, &
                                line1)

                        ! output earlier in process ? , is this switched on , switch this on

                        if (ivalip == -1) then

                            do iproc2 = 1, iproc

                                proc2 => procesdef%procesprops(iproc2)

                                ! xoutput variable of process

                                call zoekio (valnam, proc2%no_output, proc2%output_item, 20, ioutput, IOTYPE_EXCHANG_OUTPUT)
                                if (ioutput > 0) then
                                    if (proc2%linvok) then
                                        write (line1, '(3a)') '       Using output from proces [', proc2%name, ']'
                                        if (.not. proc2%active) then
                                            proc2%active = .true.
                                            write (line2, '(3a)') '       switching [', proc2%name, '] on'
                                        endif

                                        ! is output already present in ssa?

                                        if (proc2%output_item(ioutput)%ip_val /= 0) then
                                            ivalip = proc2%output_item(ioutput)%ip_val
                                        else

                                            ! reserve local spot

                                            nlocx = nlocx + 1
                                            ivalip = ioffx + nlocx
                                            proc2%output_item(ioutput)%ip_val = ivalip
                                        endif
                                        exit
                                    else
                                        write(line1, '(3a)') '  NOT  Using output from proces [', proc2%name, ']'
                                    endif
                                endif

                            enddo
                        endif

                        if (ivalip == -1) then

                            ! if this is a fraction input then first look for the generic name

                            i_star = index(valnam, '*')
                            if (i_star > 1) then
                                valnam(i_star:) = ' '
                                write(line, '(4a)') '       [', valnam, '] ', valtxt
                                goto 20
                            endif

                        endif

                        if (ivalip == -1) then
                            if (is_missing(proc1%input_item(i_input)%actdef))then
                                nmis = nmis + 1
                                write (line1, '(a)')  'error: not in input'
                            else
                                nodef = nodef + 1
                                dename(nodef) = valnam
                                ivalip = -3
                                if (abs(proc1%input_item(i_input)%actdef - rmis0) < 1.e-20)then
                                    line = ' '
                                    defaul(nodef) = 0.0
                                else
                                    defaul(nodef) = proc1%input_item(i_input)%actdef
                                    write(line1, '(a,g13.6)') '       using default value:', proc1%input_item(i_input)%actdef
                                endif
                            endif
                        endif
                        proc1%input_item(i_input)%ip_val = ivalip
                        if (line  /= ' ') call monsys(line, 4)
                        if (line1 /= ' ') call monsys(line1, 4)
                        if (line2 /= ' ') call monsys(line2, 4)
                        line = ' '
                        line1 = ' '
                        line2 = ' '

                    endif
                enddo

                ! set output variables used in output system in the local array

                do ioutput = 1, proc1%no_output
                    if (proc1%output_item(ioutput)%type == IOTYPE_SEGMENT_OUTPUT) then
                        valnam = proc1%output_item(ioutput)%name
                        ioux = 0
                        350             continue
                        nrout = outputs%cursize - ioux
                        iou = index_in_array(valnam, outputs%names(ioux + 1:nrout))
                        if (iou > 0) then
                            iou = iou + ioux
                            if (outputs%pointers(iou) == -1) then
                                if (proc1%output_item(ioutput)%ip_val /= 0) then

                                    ! already present in local array, set pointer

                                    outputs%pointers(iou) = proc1%output_item(ioutput)%ip_val
                                else

                                    ! reserve local spot

                                    noloc = noloc + 1
                                    locnam(noloc) = valnam
                                    ivalip = ioff + noloc
                                    proc1%output_item(ioutput)%ip_val = ivalip
                                    outputs%pointers(iou) = ivalip
                                endif
                            endif
                            ioux = iou
                            if (ioux < outputs%cursize) goto 350
                        endif
                    endif
                enddo

                call monsys(line, 2)
            endif
        enddo

        ! reserve in the default array idt and delt per process, delwaq2 will set value

        nodef = nodef + 2 * nproc

        ! all processes turned on , so set the pointers to the fluxes

        idef = nopred
        do iproc = nproc, 1, -1
            proc1 => procesdef%procesprops(iproc)
            if (proc1%active) then

                do i_input = 1, proc1%no_input
                    if (proc1%input_item(i_input)%type == IOTYPE_SEGMENT_INPUT) then
                        if (proc1%input_item(i_input)%ip_val == -2) then
                            valnam = proc1%input_item(i_input)%name
                            nfl = 0
                            do iproc2 = 1, iproc - 1

                                proc2 => procesdef%procesprops(iproc2)
                                if (proc2%active) then
                                    call zoekio (valnam, proc2%no_fluxoutput, proc2%fluxoutput, 20, iflux)
                                    if (iflux > 0) then
                                        ivalip = ioff + noloc + nodef + nfl + iflux
                                        exit
                                    endif
                                    nfl = nfl + proc2%no_fluxoutput
                                endif
                            enddo
                            proc1%input_item(i_input)%ip_val = ivalip
                        elseif (proc1%input_item(i_input)%ip_val == -3) then
                            idef = idef + 1
                            ivalip = ioff + noloc + idef
                            proc1%input_item(i_input)%ip_val = ivalip
                        endif
                    endif
                enddo

                do i_input = 1, proc1%no_input
                    if (proc1%input_item(i_input)%type == IOTYPE_EXCHANG_INPUT) then
                        if (proc1%input_item(i_input)%ip_val == -3) then
                            idef = idef + 1
                            ivalip = ioffx + nlocx + idef
                            proc1%input_item(i_input)%ip_val = ivalip
                        endif
                    endif
                enddo

            endif
        enddo

        if (timon) call timstop(ithndl)
        return
    end

end module m_getinv
