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

module partable
    use m_waq_precision
    use m_valpoi
    use m_logger_helper
    use m_string_utils


    ! NB This is a module, because the subroutine partab allocates the array proref
    !                      that is used outside of partab, through dlwqp1 in wripro.
    !                      The alternative is to construct an explicit interface to
    !                      the routine partab in dlwqp1. This method seems simpler.

contains

    subroutine partab (procesdef, num_substances_total, syname, num_constants, constants, &
            &                    num_spatial_parameters, paname, num_time_functions, funame, num_spatial_time_fuctions, &
            &                    sfname, proref, num_input_ref, status, nothread, &
            &                    nopred, num_local_vars, num_defaults)

        !     Deltares Software Department

        !     Created   : March 2010 by Leo Postma

        !     Function  : Makes a parallel processing reference table to ensure
        !                 integrity at runtime of required process input.
        !                 Procesdef is sorted to contain all processes in optimal
        !                 calling order, the inactive processes at the end.
        !                 Proref(num_input_ref,num_processes_activated) contains the reference information
        !                 for the active processes only, so num_processes_activated < procesdef%current_size

        !     Modified  :

        use m_waq_data_structure
        use ProcesSet
        use timers       !   performance timers
        use m_error_status

        implicit none

        !     Arguments           :

        !     Kind                  Function         Name                  Description

        type(ProcesPropColl), intent(inout) :: procesdef       ! all processes
        integer(kind = int_wp), intent(in) :: num_substances_total           ! number of substances
        character(20), intent(in) :: syname(num_substances_total)   ! substance names
        integer(kind = int_wp), intent(in) :: num_constants          ! number of constants
        type(t_waq_item), intent(inout) :: constants       !< delwaq constants list
        integer(kind = int_wp), intent(in) :: num_spatial_parameters            ! number of parameters
        character(20), intent(in) :: paname(num_spatial_parameters)    ! parameter names
        integer(kind = int_wp), intent(in) :: num_time_functions           ! number of functions
        character(20), intent(in) :: funame(num_time_functions)   ! function names
        integer(kind = int_wp), intent(in) :: num_spatial_time_fuctions          ! number of segment functions
        character(20), intent(in) :: sfname(num_spatial_time_fuctions)  ! segment function names
        integer(kind = int_wp), pointer, intent(out) :: proref(:, :)     ! input items to be resolved for each process
        integer(kind = int_wp), intent(out) :: num_input_ref           ! maximum nr of references to be resolved
        integer(kind = int_wp), intent(inout) :: nothread        ! number of threads to be used
        integer(kind = int_wp), intent(in) :: nopred          ! number of predefined items
        integer(kind = int_wp), intent(in) :: num_local_vars           ! number of items in local array
        integer(kind = int_wp), intent(in) :: num_defaults           ! number of items in default array

        type(error_status), intent(inout) :: status !< current error status

        !     Local declarations

        integer(kind = int_wp) :: noproc          ! nr of processes ( = ProcesDef%current_size )
        integer(kind = int_wp) :: iproc1, iproc2  ! process loop counters
        type(ProcesProp), pointer :: proc1           ! the process with sequence nr iproc1
        type(ProcesProp), pointer :: proc2           ! the process with sequence nr iproc2
        integer(kind = int_wp) :: iout            ! loop counter for process outputs
        integer(kind = int_wp) :: iin             ! loop counter for process inputs
        integer(kind = int_wp) :: nitem           ! number of needed items
        integer(kind = int_wp) :: ioff            ! offset to flux array
        integer(kind = int_wp) :: nfl             ! number of fluxes till this process
        character(100)                         line            ! output buffer
        integer(kind = int_wp) :: iprocs          ! counter of the saved ordered processes
        integer(kind = int_wp) :: num_processes_activated           ! incremental start value or process ordering
        integer(kind = int_wp) :: naproc          ! nr of active processes
        integer(kind = int_wp) :: ifound          ! result of search routine >0 if found
        integer(kind = int_wp) :: k               ! help variable
        integer(kind = int_wp), allocatable :: profreq(:)      ! nr of used forward references per process
        integer(kind = int_wp), allocatable :: prorder(:)      ! final order of execution of processes
        integer(kind = int_wp), allocatable :: needed (:)      ! nr of needer backward references pewr process
        integer(kind = int_wp), allocatable :: work   (:)      ! work array to sort the processes
        type(ProcesProp), allocatable :: cProces(:)      ! work array to rearrange the processes
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("partab", ithndl)

        !         initial allocations

        noproc = ProcesDef%current_size
        allocate (prorder(noproc), profreq(noproc))
        allocate (work   (noproc), needed (noproc))
        allocate (cProces(noproc))

        !         make an array with the number of used forward references per process

        profreq = 0
        naproc = 0
        do iproc1 = 1, noproc
            proc1 => procesdef%procesprops(iproc1)
            if (proc1%active) then
                naproc = naproc + 1
                nitem = 0
                do iout = 1, proc1%no_output
                    do iproc2 = iproc1 + 1, noproc                        ! they are already sorted before
                        proc2 => procesdef%procesprops(iproc2)
                        if (proc2%active) then
                            do iin = 1, proc2%no_input
                                if (isinput(num_substances_total, syname, num_constants, constants, num_spatial_parameters, &
                                        &                                paname, num_time_functions, funame, num_spatial_time_fuctions, sfname, &
                                        &                                proc2%input_item(iin)%name, &
                                        &                                proc1%output_item(iout)%name) > 0) then
                                    nitem = nitem + 1
                                endif
                            enddo
                        endif
                    enddo
                enddo
                do iout = 1, proc1%no_fluxoutput
                    do iproc2 = iproc1 + 1, noproc                        ! they are already sorted before
                        proc2 => procesdef%procesprops(iproc2)
                        if (proc2%active) then
                            do iin = 1, proc2%no_input
                                if (isinput(num_substances_total, syname, num_constants, constants, num_spatial_parameters, &
                                        &                                paname, num_time_functions, funame, num_spatial_time_fuctions, sfname, &
                                        &                                proc2%input_item(iin)%name, &
                                        &                                proc1%fluxoutput(iout)%name) > 0) then
                                    nitem = nitem + 1
                                endif
                            enddo
                        endif
                    enddo
                enddo
                profreq(iproc1) = nitem
            endif
        enddo

        !         determine cyclicly which processes need no further input

        num_processes_activated = 1
        iprocs = 0
        needed = 0
        num_input_ref = 0
        do while (num_processes_activated <= noproc)
            do iproc1 = 1, noproc                                     ! count backward references
                proc1 => procesdef%procesprops(iproc1)
                if (proc1%active) then
                    if (needed(iproc1) == -1) cycle                  ! this process is already ordered
                    nitem = 0
                    do iin = 1, proc1%no_input
                        do iproc2 = 1, iproc1 - 1                          ! only the previous processes can provide
                            proc2 => procesdef%procesprops(iproc2)
                            if (proc2%active) then
                                if (needed(iproc2) == -1) cycle
                                do iout = 1, proc2%no_output
                                    if (isinput(num_substances_total, syname, num_constants, constants, num_spatial_parameters, &
                                            &                                   paname, num_time_functions, funame, num_spatial_time_fuctions, sfname, &
                                            &                                   proc1%input_item(iin)%name, &
                                            &                                   proc2%output_item(iout)%name) > 0) then
                                        nitem = nitem + 1
                                    endif
                                enddo
                                do iout = 1, proc2%no_fluxoutput
                                    if (isinput(num_substances_total, syname, num_constants, constants, num_spatial_parameters, &
                                            &                                   paname, num_time_functions, funame, num_spatial_time_fuctions, sfname, &
                                            &                                   proc1%input_item(iin)%name, &
                                            &                                   proc2%fluxoutput(iout)%name) > 0) then
                                        nitem = nitem + 1
                                    endif
                                enddo
                            endif
                        enddo
                    enddo
                    if (nitem == 0) then                             ! save the processes that are able to run
                        iprocs = iprocs + 1                              ! in separate arrays
                        prorder(iprocs) = iproc1
                        work   (iprocs) = profreq(iproc1)
                    else
                        num_input_ref = max (num_input_ref, nitem)                      ! determine the maximum of dependencies
                    endif
                endif
            enddo
            if (num_processes_activated == iprocs + 1) exit                            ! no further progress made
            do iproc1 = num_processes_activated, iprocs                                 ! simple bubblesort of those that can run
                do iproc2 = iprocs, iproc1 + 1, -1                        ! to get those with hihest forward refs first
                    if (work(iproc2) > work(iproc2 - 1)) then
                        iin = work   (iproc2 - 1)
                        work   (iproc2 - 1) = work   (iproc2)
                        work   (iproc2) = iin
                        iin = prorder(iproc2 - 1)
                        prorder(iproc2 - 1) = prorder(iproc2)
                        prorder(iproc2) = iin
                    endif
                enddo
            enddo
            iin = 0
            iout = 0
            do iproc1 = num_processes_activated, iprocs
                needed(prorder(iproc1)) = -1                            ! mark these processes dealt with
                if (profreq(prorder(iproc1)) == 0) then             ! take care that at the end there are
                    iout = iout + 1                                        ! at least 5 without forward reference
                    if (iout == 5) then                              ! Is 5 enough to prevent the running of a
                        iin = iproc1                                      ! process that has not got its input resolved
                        exit                                              ! yet during parallel processing ?
                    endif                                                ! You can take a larger value here, but then
                endif                                                   ! you might run out of separators later on.
            enddo                                                      ! There will at least be a check at runtime
            if (iin /= 0) iprocs = iin
            num_processes_activated = iprocs + 1
        enddo
        if (nothread > 1) then

            !         if at the end there are still processes left, then add them at the end with a warning

            do iproc1 = 1, noproc
                proc1 => procesdef%procesprops(iproc1)
                if (proc1%active) then
                    if (needed(iproc1) == -1) cycle
                    iprocs = iprocs + 1
                    prorder(iprocs) = iproc1
                    write(line, '(a,a)') ' WARNING: possibly unresolved input for process: ', &
                            &                                                      ProcesDef%ProcesProps(iproc1)%name
                    call write_log_message(line)
                    call status%increase_warning_count()
                endif
            enddo

            !         add the inactive processes after that

            iin = iprocs
            num_processes_activated = 0
            do iproc1 = 1, noproc
                proc1 => procesdef%procesprops(iproc1)
                if (proc1%active) then
                    num_processes_activated = num_processes_activated + 1
                else
                    iprocs = iprocs + 1
                    prorder(iprocs) = iproc1
                endif
            enddo
            if (num_processes_activated /= iin) then
                write(line, '(a,2i5)') ' ERROR: no match in number of active processes: ', num_processes_activated, iin
                call write_log_message(line)
            endif
        else
            iprocs = 0
            do iproc1 = 1, noproc
                proc1 => procesdef%procesprops(iproc1)
                if (proc1%active) then
                    iprocs = iprocs + 1
                    prorder(iprocs) = iproc1
                endif
            enddo
            do iproc1 = 1, noproc
                proc1 => procesdef%procesprops(iproc1)
                if (.not. proc1%active) then
                    iprocs = iprocs + 1
                    prorder(iprocs) = iproc1
                endif
            enddo
        endif

        !         put the processes in the right order

        do iproc1 = 1, noproc
            cProces(iproc1) = ProcesDef%ProcesProps(prorder(iproc1))
        enddo
        do iproc1 = 1, noproc
            ProcesDef%ProcesProps(iproc1) = cProces(iproc1)
        enddo

        !         make the refrence table: proref

        if (num_input_ref == 0) num_input_ref = 1
        allocate (proref(num_input_ref, naproc))
        proref = 0
        do iproc1 = 1, naproc
            proc1 => procesdef%procesprops(iproc1)
            if (proc1%active) then
                nitem = 0
                do iin = 1, proc1%no_input
                    do iproc2 = 1, noproc
                        if (iproc2 == iproc1) cycle
                        proc2 => procesdef%procesprops(iproc2)
                        if (proc2%active) then
                            do iout = 1, proc2%no_output
                                if (isinput(num_substances_total, syname, num_constants, constants, num_spatial_parameters, &
                                        &                                paname, num_time_functions, funame, num_spatial_time_fuctions, sfname, &
                                        &                                proc1%input_item (iin)%name, &
                                        &                                proc2%output_item(iout)%name) > 0) then
                                    nitem = nitem + 1
                                    proref(nitem, iproc1) = iproc2
                                endif
                            enddo
                            do iout = 1, proc2%no_fluxoutput
                                if (isinput(num_substances_total, syname, num_constants, constants, num_spatial_parameters, &
                                        &                                paname, num_time_functions, funame, num_spatial_time_fuctions, sfname, &
                                        &                                proc1%input_item(iin)%name, &
                                        &                                proc2%fluxoutput(iout)%name) > 0) then
                                    nitem = nitem + 1
                                    proref(nitem, iproc1) = iproc2
                                endif
                            enddo
                        endif
                    enddo
                enddo
            endif
        enddo

        !         update flux pointers to new order

        ioff = nopred + num_constants + num_spatial_parameters + num_time_functions + num_spatial_time_fuctions + num_substances_total + num_local_vars + num_defaults
        do iproc1 = 1, naproc
            proc1 => procesdef%procesprops(iproc1)
            if (proc1%active) then
                do iin = 1, proc1%no_input
                    nfl = 0
                    do iproc2 = 1, noproc
                        proc2 => procesdef%procesprops(iproc2)
                        if (proc2%active) then
                            if (iproc2 /= iproc1) then
                                do iout = 1, proc2%no_fluxoutput
                                    if (isinput(num_substances_total, syname, num_constants, constants, num_spatial_parameters, &
                                            &                                   paname, num_time_functions, funame, num_spatial_time_fuctions, sfname, &
                                            &                                   proc1%input_item(iin)%name, &
                                            &                                   proc2%fluxoutput(iout)%name) > 0) then
                                        proc1%input_item(iin)%ip_val = ioff + nfl + iout
                                    endif
                                enddo
                            endif
                            nfl = nfl + proc2%no_fluxoutput
                        endif
                    enddo
                enddo
            endif
        enddo

        if (timon) call timstop(ithndl)
        return
    end subroutine partab

    integer function isinput(num_substances_total, syname, num_constants, constants, num_spatial_parameters, &
            &                          paname, num_time_functions, funame, num_spatial_time_fuctions, sfname, &
            &                          valnam, input)

        use m_waq_data_structure

        character(20), intent(in) :: valnam
        character(20), intent(in) :: input

        integer(kind = int_wp), intent(in) :: num_substances_total           ! number of substances
        character(20), intent(in) :: syname(num_substances_total)   ! substance names
        integer(kind = int_wp), intent(in) :: num_constants          ! number of constants
        type(t_waq_item), intent(inout) :: constants       ! delwaq constants list
        integer(kind = int_wp), intent(in) :: num_spatial_parameters            ! number of parameters
        character(20), intent(in) :: paname(num_spatial_parameters)    ! parameter names
        integer(kind = int_wp), intent(in) :: num_time_functions           ! number of functions
        character(20), intent(in) :: funame(num_time_functions)   ! function names
        integer(kind = int_wp), intent(in) :: num_spatial_time_fuctions          ! number of segment functions
        character(20), intent(in) :: sfname(num_spatial_time_fuctions)  ! segment function names
        !
        integer(kind = int_wp) :: status          ! value to be returned by function
        integer(kind = int_wp) :: ifound          ! result of search routine >0 if found
        integer(kind = int_wp) :: ivalip          ! >0 if valnam was a valid item in user input
        character(20)                          locnam          ! local copy of valnam
        character(100)                         line            ! output buffer
        !
        locnam = valnam
        status = -1
        do while (status == -1)
            call valpoi (num_substances_total, num_spatial_parameters, num_spatial_time_fuctions, syname, num_constants, &
                    &                 num_time_functions, constants, paname, funame, sfname, &
                    &                 locnam, ivalip, line)
            if (ivalip /= -1) then
                status = 0
            else
                if (string_equals(locnam, input)) then
                    status = 1
                else
                    ! if name contains star, remove it
                    i_star = index(locnam, '*')
                    if (i_star > 1) then
                        locnam(i_star:) = ' '
                    else
                        status = 0
                    endif
                endif
            endif
        enddo
        isinput = status
        return
    end function isinput

end module partable
