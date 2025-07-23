!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_add_dspfrc
    use m_waq_precision
    use m_string_utils

    implicit none

contains


    subroutine add_dspfrc(lunrep, procesdef, sfracs)

        ! add the dispersion and velocity stochi for fractions

        use m_logger_helper, only : stop_with_error
        use ProcesSet
        use timers       !   performance timers

        implicit none

        ! decalaration of arguments

        integer(kind = int_wp) :: lunrep          ! report file
        type(procespropcoll) :: procesdef       ! the process definition
        type(sfracsprop) :: sfracs          ! substance fraction properties

        ! local declaration

        type(stochiprop), pointer :: new_dispstochi(:) ! list with added stochies
        type(stochiprop), pointer :: new_velostochi(:) ! list with added stochies
        type(procesprop), pointer :: proc              ! single process
        integer(kind = int_wp) :: num_processes_activated             ! number of processes
        integer(kind = int_wp) :: iproc             ! loop counter processes
        integer(kind = int_wp) :: isfrac            ! index substance fractions
        integer(kind = int_wp) :: nfrac             ! number fractions in substance fraction
        character(len = 20) :: basnam            ! base name substance fractions
        integer(kind = int_wp) :: nstochi           ! number of original stochies
        integer(kind = int_wp) :: istochi           ! index stochi
        integer(kind = int_wp) :: n_velo_stochi     ! number of original stochies
        integer(kind = int_wp) :: ifrac             ! fraction number
        character(len = 3) :: suffix            ! suffix
        integer(kind = int_wp) :: indx              ! index in list
        integer(kind = int_wp) :: ierr_alloc        ! error indication
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("add_dspfrc", ithndl)

        ! loop over the processes

        num_processes_activated = procesdef%current_size
        do iproc = 1, num_processes_activated

            proc => procesdef%procesprops(iproc)
            nstochi = proc%no_dispstochi
            n_velo_stochi = proc%no_velostochi

            ! only for processes which are split up

            if (procesdef%procesprops(iproc)%sfrac_type == SFRAC_SPLITFLUX) then

                ! check for dispersion stochi with fractions

                do isfrac = 1, sfracs%nsfrac

                    nfrac = sfracs%nfrac(isfrac)
                    basnam = sfracs%name(isfrac)

                    do istochi = 1, nstochi

                        ! skip dummy rules, factor equal zero

                        if (abs(proc%dispstochi(istochi)%scale) > 1e-10) then

                            if (string_equals(basnam(1:10), proc%dispstochi(istochi)%substance)) then

                                ! dispersion found add the fractions with the same dipersion name and the same factor

                                allocate(new_dispstochi(proc%no_dispstochi + nfrac), stat = ierr_alloc)
                                if (ierr_alloc /= 0) then
                                    write(lunrep, *) 'error allocating work array in routine add_dspfrc:', ierr_alloc
                                    write(lunrep, *) 'array length:', proc%no_dispstochi + nfrac
                                    write(*, *) 'error allocating array:', ierr_alloc
                                    call stop_with_error()
                                endif

                                ! copy the existing stochis

                                new_dispstochi(1:proc%no_dispstochi) = proc%dispstochi(1:proc%no_dispstochi)

                                ! add the new ones

                                do ifrac = 1, nfrac
                                    if (ifrac < 100) then
                                        write(suffix, '(i2.2)') ifrac
                                    else
                                        write(suffix, '(i3.3)') ifrac
                                    endif
                                    new_dispstochi(proc%no_dispstochi + ifrac)%substance = trim(basnam) // suffix
                                    new_dispstochi(proc%no_dispstochi + ifrac)%ioitem = proc%dispstochi(istochi)%ioitem
                                    new_dispstochi(proc%no_dispstochi + ifrac)%scale = proc%dispstochi(istochi)%scale
                                enddo

                                ! attach new array to porcess defintion

                                proc%no_dispstochi = proc%no_dispstochi + nfrac
                                deallocate(proc%dispstochi)
                                proc%dispstochi => new_dispstochi
                                write(lunrep, 2000) ' adding dispersion [', trim(proc%dispstochi(istochi)%ioitem), &
                                        '] for fractions of [', trim(basnam), ']'

                                ! no_sto = no_sto + nfrac ! Should something like this also happen here?

                            endif
                        endif
                    enddo

                    ! the velocities

                    do istochi = 1, n_velo_stochi

                        ! skip dummy rules, factor equal zero

                        if (abs(proc%velostochi(istochi)%scale) > 1e-10) then

                            if (string_equals(basnam(1:10), proc%velostochi(istochi)%substance)) then

                                ! velocity found add the fractions with the same velocity name and the same factor

                                allocate(new_velostochi(proc%no_velostochi + nfrac), stat = ierr_alloc)
                                if (ierr_alloc /= 0) then
                                    write(lunrep, *) 'error allocating work array in routine add_dspfrc:', ierr_alloc
                                    write(lunrep, *) 'array length:', proc%no_velostochi + nfrac
                                    write(*, *) 'error allocating array:', ierr_alloc
                                    call stop_with_error()
                                endif

                                ! copy the existing stochis

                                new_velostochi(1:proc%no_velostochi) = proc%velostochi(1:proc%no_velostochi)

                                ! add the new ones

                                do ifrac = 1, nfrac
                                    if (ifrac < 100) then
                                        write(suffix, '(i2.2)') ifrac
                                    else
                                        write(suffix, '(i3.3)') ifrac
                                    endif
                                    new_velostochi(proc%no_velostochi + ifrac)%substance = trim(basnam) // suffix
                                    new_velostochi(proc%no_velostochi + ifrac)%ioitem = proc%velostochi(istochi)%ioitem
                                    new_velostochi(proc%no_velostochi + ifrac)%scale = proc%velostochi(istochi)%scale
                                enddo

                                ! attach new array to porcess defintion

                                proc%no_velostochi = proc%no_velostochi + nfrac
                                deallocate(proc%velostochi)
                                proc%velostochi => new_velostochi
                                write(lunrep, 2000) ' adding velocity [', trim(proc%velostochi(istochi)%ioitem), &
                                        '] for fractions of [', trim(basnam), ']'

                                ! no_sto = no_sto + nfrac ! Should something like this also happen here?

                            endif
                        endif
                    enddo
                enddo
            endif
        enddo

        if (timon) call timstop(ithndl)
        return
        2000 format (5a)
    end

end module m_add_dspfrc
