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
module m_wrstoc
    use m_waq_precision

    implicit none

contains


    subroutine wrstoc (procesdef, luout, num_substances_total, syname, stoch2, &
            num_output_files, ioutps, outputs, ndmpar, output_buffer_len, &
            intopt)

        ! writes the stochi file, sets stoch2 array
        ! set output structure for new balance file

        use timers         !< performance timers
        use m_array_manipulation, only : resize_integer_array, resize_character_array
        use processet
        use results, only : OutputPointers, iba2, iba3
        implicit none

        ! declaration of arguments

        type(procespropcoll) :: procesdef       ! all processes
        integer(kind = int_wp) :: luout           ! stochi file
        integer(kind = int_wp) :: num_substances_total           ! number of substances
        character(len = *) :: syname(num_substances_total)   ! name of substances
        real(kind = real_wp) :: stoch2(num_substances_total, *) ! delwaq stochi array
        integer(kind = int_wp) :: num_output_files           ! number of output variables
        integer(kind = int_wp) :: ioutps(7, num_output_files) ! output structure
        type(OutputPointers) :: outputs         ! output structure
        integer(kind = int_wp) :: ndmpar          ! number of stations
        integer(kind = int_wp) :: output_buffer_len
        integer(kind = int_wp) :: intopt          ! integration option

        character(len = 80) :: line            ! output buffer
        integer(kind = int_wp) :: noflx           ! number of fluxes
        integer(kind = int_wp) :: nproctot        ! number of processes
        integer(kind = int_wp) :: iproc           ! index process
        type(procesprop), pointer :: proc            ! process description
        integer(kind = int_wp) :: i               ! loop counter
        integer(kind = int_wp) :: j               ! loop counter
        integer(kind = int_wp) :: niflx           ! number of fluxes
        integer(kind = int_wp) :: nflx            ! number of fluxes
        integer(kind = int_wp) :: iflx            ! index flux
        integer(kind = int_wp) :: iflux           ! index flux
        integer(kind = int_wp) :: istochi         ! index flux
        integer(kind = int_wp) :: isys            ! index substance
        real(kind = real_wp) :: scale           ! stochi factor
        character(len = 20) :: flxnam          ! output buffer
        integer(kind = int_wp) :: nrvar           ! counter
        integer(kind = int_wp) :: nrvarn          ! counter
        integer(kind = int_wp) :: nrvaro          ! counter
        integer(kind = int_wp) :: ioutp           ! index output variable
        integer(kind = int_wp) :: ivar            ! index output variable
        integer(kind = int_wp) :: isrtou          ! type of output
        integer(kind = int_wp) :: nobalt          ! number of balances
        integer(kind = int_wp) :: nocel           ! number of cells
        integer(kind = int_wp) :: nbufou          ! buffer used
        type(OutputPointers) :: outputl         ! local output structure
        integer(kind = int_wp) :: nrvarm          ! size of local output structure
        integer(kind = int_wp) :: ithndl = 0        ! handle for performance timer
        if (timon) call timstrt("wrstoc", ithndl)

        ! calculate noflx

        noflx = 0
        nproctot = procesdef%current_size
        do iproc = 1, nproctot
            proc => procesdef%procesprops(iproc)
            if (proc%active) then
                noflx = noflx + proc%no_fluxoutput
            endif
        enddo

        ! zero stoch2 array

        do j = 1, noflx
            do i = 1, num_substances_total
                stoch2(i, j) = 0.0
            enddo
        enddo

        ! write stochiometry output file


        ! fluxes

        if (btest(intopt, 3) .and. .not. btest(intopt, 4)) then
            write (luout, '(i5)') noflx
            do iproc = 1, nproctot
                proc => procesdef%procesprops(iproc)
                if (proc%active) then
                    niflx = proc%no_fluxoutput
                    do iflx = 1, niflx
                        write (luout, '(a20)') proc%fluxoutput(iflx)%name
                    enddo
                endif
            enddo
        endif

        ! stochimetry, write and set stoch2 = complete stochiometry

        nflx = 0
        do iproc = 1, nproctot
            proc => procesdef%procesprops(iproc)
            if (proc%active) then
                do istochi = 1, proc%no_fluxstochi

                    flxnam = proc%fluxstochi(istochi)%ioitem
                    isys = proc%fluxstochi(istochi)%subindx
                    scale = proc%fluxstochi(istochi)%scale

                    if (isys>0 .and. abs(scale)>1e-10) then
                        call zoekio (flxnam, proc%no_fluxoutput, proc%fluxoutput, 20, iflux)
                        iflx = nflx + iflux
                        stoch2(isys, iflx) = scale
                        if (btest(intopt, 3) .and. .not. btest(intopt, 4)) then
                            write (luout, '(a20,2x,a20,2x,f10.3)') syname(isys)(1:10), flxnam, scale
                        endif
                    endif
                enddo
                nflx = nflx + proc%no_fluxoutput
            endif
        enddo

        ! set variables for output balance file new style

        nrvarm = outputs%current_size * 2
        outputl%current_size = nrvarm
        allocate(outputl%names(nrvarm))
        allocate(outputl%std_var_name(nrvarm))
        allocate(outputl%pointers(nrvarm))
        allocate(outputl%units(nrvarm))
        allocate(outputl%description(nrvarm))
        nrvarn = 0
        nrvaro = 0
        do ioutp = 1, num_output_files
            isrtou = ioutps(5, ioutp)
            if (isrtou == iba2) then
                nobalt = 0
                do isys = 1, num_substances_total
                    if (nrvarn + nobalt + 4 > nrvarm) then
                        outputl%current_size = (nrvarn + nobalt + 4) * 2
                        call resize_integer_array(outputl%pointers, outputl%current_size, nrvarm)
                        call resize_character_array(outputl%names, outputl%current_size, nrvarm)
                        nrvarm = outputl%current_size
                    endif
                    outputl%names(nrvarn + nobalt + 1)(1:10) = syname(isys)
                    outputl%names(nrvarn + nobalt + 1)(11:20) = 'Loads in'
                    outputl%names(nrvarn + nobalt + 2)(1:10) = syname(isys)
                    outputl%names(nrvarn + nobalt + 2)(11:20) = 'Loads out'
                    outputl%names(nrvarn + nobalt + 3)(1:10) = syname(isys)
                    outputl%names(nrvarn + nobalt + 3)(11:20) = 'Transp in'
                    outputl%names(nrvarn + nobalt + 4)(1:10) = syname(isys)
                    outputl%names(nrvarn + nobalt + 4)(11:20) = 'Transp out'
                    outputl%pointers(nrvarn + nobalt + 1) = 0
                    outputl%pointers(nrvarn + nobalt + 2) = 0
                    outputl%pointers(nrvarn + nobalt + 3) = 0
                    outputl%pointers(nrvarn + nobalt + 4) = 0
                    nobalt = nobalt + 4
                    nflx = 0
                    do iproc = 1, nproctot
                        proc => procesdef%procesprops(iproc)
                        if (proc%active) then
                            niflx = proc%no_fluxoutput
                            do iflx = 1, niflx
                                scale = stoch2(isys, nflx + iflx)
                                if (abs(scale) > 1.e-10) then
                                    nobalt = nobalt + 1
                                    if (nrvarn + nobalt > nrvarm) then
                                        outputl%current_size = (nrvarn + nobalt) * 2
                                        call resize_integer_array(outputl%pointers, outputl%current_size, nrvarm)
                                        call resize_character_array(outputl%names, outputl%current_size, nrvarm)
                                        nrvarm = outputl%current_size
                                    endif
                                    outputl%names(nrvarn + nobalt)(1:10) = syname(isys)
                                    outputl%names(nrvarn + nobalt)(11:20) = proc%fluxoutput(iflx)%name
                                    outputl%pointers(nrvarn + nobalt) = 0
                                endif
                            enddo
                            nflx = nflx + niflx
                        endif
                    enddo
                enddo
                nrvar = ioutps(4, ioutp)
                ioutps(4, ioutp) = nobalt
                nrvaro = nrvaro + nrvar
                nrvarn = nrvarn + nobalt

                nocel = ndmpar
                nbufou = nocel * nobalt
                output_buffer_len = max (output_buffer_len, nbufou)

            elseif (isrtou == iba3) then

                if (nrvarn + noflx > nrvarm) then
                    outputl%current_size = (nrvarn + noflx) * 2
                    call resize_integer_array(outputl%pointers, outputl%current_size, nrvarm)
                    call resize_character_array(outputl%names, outputl%current_size, nrvarm)
                    nrvarm = outputl%current_size
                endif

                iflux = 0
                do iproc = 1, nproctot
                    proc => procesdef%procesprops(iproc)
                    if (proc%active) then
                        niflx = proc%no_fluxoutput
                        do iflx = 1, niflx
                            iflux = iflux + 1
                            outputl%names(nrvarn + iflux) = proc%fluxoutput(iflx)%name
                            outputl%pointers(nrvarn + iflux) = 0
                        enddo
                    endif
                enddo

                nrvar = ioutps(4, ioutp)
                ioutps(4, ioutp) = noflx
                nrvaro = nrvaro + nrvar
                nrvarn = nrvarn + noflx

            else
                nrvar = ioutps(4, ioutp)
                if (nrvarn + nrvar > nrvarm) then
                    outputl%current_size = (nrvarn + nrvar) * 2
                    call resize_integer_array(outputl%pointers, outputl%current_size, nrvarm)
                    call resize_character_array(outputl%names, outputl%current_size, nrvarm)
                    nrvarm = outputl%current_size
                endif
                do ivar = 1, nrvar
                    outputl%names(nrvarn + ivar) = outputs%names(nrvaro + ivar)
                    outputl%pointers(nrvarn + ivar) = outputs%pointers(nrvaro + ivar)
                enddo
                nrvaro = nrvaro + nrvar
                nrvarn = nrvarn + nrvar
            endif
        enddo

        ! copy local output structure to argument

        deallocate(outputs%names)
        deallocate(outputs%std_var_name)
        deallocate(outputs%pointers)
        deallocate(outputs%units)
        deallocate(outputs%description)
        allocate(outputs%names(nrvarn))
        allocate(outputs%std_var_name(nrvarn))
        allocate(outputs%pointers(nrvarn))
        allocate(outputs%units(nrvarn))
        allocate(outputs%description(nrvarn))
        outputs%current_size = nrvarn
        outputs%names(1:nrvarn) = outputl%names(1:nrvarn)
        outputs%std_var_name(1:nrvarn) = ' ' ! outputl%std_var_name(1:nrvarn)
        outputs%units(1:nrvarn) = ' ' ! outputl%units(1:nrvarn)
        outputs%description(1:nrvarn) = ' ' ! outputl%description(1:nrvarn)
        outputs%pointers(1:nrvarn) = outputl%pointers(1:nrvarn)

        ! deallocate local output structure

        deallocate(outputl%names)
        deallocate(outputl%std_var_name)
        deallocate(outputl%pointers)
        deallocate(outputl%units)
        deallocate(outputl%description)

        if (timon) call timstop(ithndl)
        return
    end

end module m_wrstoc
