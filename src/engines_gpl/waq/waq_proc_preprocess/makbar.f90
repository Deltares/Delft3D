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
module m_makbar
    use m_waq_precision
    use m_string_utils
    use m_vxlpoi
    use m_valpoi
    use m_error_status

    implicit none

contains


    subroutine makbar (procesdef, num_substances_total, syname, num_constants, constants, &
            num_spatial_parameters, paname, num_time_functions, funame, num_spatial_time_fuctions, &
            sfname, num_dispersion_arrays, diname, num_velocity_arrays, vename, &
            num_exchanges_z_dir, laswi, no_act, actlst, &
            status)

        ! Checks which processes can be activated

        use m_logger_helper
        use waq_attribute_utils, only : evaluate_dimension_match
        use m_array_manipulation, only : is_missing
        use m_waq_data_structure
        use processet
        use timers       !   performance timers

        implicit none

        ! declaration of arguments

        character(len = *), dimension(*) :: syname ! substance name
        character(len = *), dimension(*) :: paname ! parameter names
        character(len = *), dimension(*) :: funame ! function names
        character(len = *), dimension(*) :: sfname ! segment function names
        character(len = *), dimension(*) :: diname ! dispersion names
        character(len = *), dimension(*) :: vename ! velocity names
        character(len = *), dimension(*) :: actlst ! active processes names

        integer(kind = int_wp) :: num_substances_total  ! number of substances
        integer(kind = int_wp) :: num_constants ! number of constants
        integer(kind = int_wp) :: num_spatial_parameters   ! number of parameters
        integer(kind = int_wp) :: num_time_functions  ! number of functions
        integer(kind = int_wp) :: num_spatial_time_fuctions ! number of segment functions
        integer(kind = int_wp) :: num_dispersion_arrays ! number of dispersions
        integer(kind = int_wp) :: num_velocity_arrays ! number of velocities
        integer(kind = int_wp) :: num_exchanges_z_dir   ! number of exhcanges in third direction
        integer(kind = int_wp) :: no_act ! number of active processes

        logical :: laswi ! active only switch

        type(procespropcoll) :: procesdef              !< all processes
        type(t_waq_item), intent(inout) :: constants !< delwaq constants list
        type(error_status), intent(inout) :: status    !< current error status

        ! local decalarations
        integer(kind = int_wp), parameter :: mismax = 50 ! maximum number of missing variables per process

        character(len = 20) :: valnam ! variable name
        character(len = 50) :: valtxt ! variable description
        character(len = 100) :: line   ! line buffer for output
        character(len = 20), dimension(mismax) :: misnam ! name missing variables
        character(len = 50), dimension(mismax) :: mistxt ! description missing variables

        integer(kind = int_wp) :: num_processes_activated   ! number of processes
        integer(kind = int_wp) :: iproc   ! loop counter processes
        integer(kind = int_wp) :: iproc2  ! second loop counter processes
        integer(kind = int_wp) :: ivalip  ! index variable in process_space_real
        integer(kind = int_wp) :: iflux   ! index flux
        integer(kind = int_wp) :: i_input ! index input item
        integer(kind = int_wp) :: ioutput ! index output item
        integer(kind = int_wp) :: iact    ! index in active list
        integer(kind = int_wp) :: nmis    ! actual number of missing variables
        integer(kind = int_wp) :: imis    ! index number of missing variables
        integer(kind = int_wp) :: i_star  ! index of * in name
        integer(kind = int_wp) :: ithndl = 0

        logical :: iok ! indicates if its ok

        type(procesprop), pointer :: proc1 ! process description
        type(procesprop), pointer :: proc2 ! description second process

        if (timon) call timstrt("makbar", ithndl)

        write (line, '(a)') '# Determining which processes can be switched on'
        call write_log_message(line)
        line = ' '
        call write_log_message(line)

        num_processes_activated = procesdef%current_size
        do iproc = 1, num_processes_activated

            proc1 => procesdef%procesprops(iproc)
            if (proc1%sfrac_type == SFRAC_DUPLICATED_ORIGINAL) then
                !
                ! prevent the original version of duplicated processes from showing up in
                ! warning list
                !
                proc1%linvok = .false.
                cycle
            endif

            nmis = 0
            call evaluate_dimension_match(proc1%swtransp, num_exchanges_z_dir, iok)
            if (.not. iok) then
                write (line, '(4a)') ' Input for [', proc1%name, '] ', proc1%text(1:50)
                call write_log_message(line)
                write (line, '(a)') ' process for different model dimensions'
                call write_log_message(line)
                proc1%linvok = .false.
                goto 550
            endif

            write (line, '(4a)') ' Input for [', proc1%name, '] ', proc1%text(1:50)
            call write_log_message(line)
            proc1%linvok = .true.

            ! check input items

            do i_input = 1, proc1%no_input

                if (proc1%input_item(i_input)%type == IOTYPE_SEGMENT_INPUT) then

                    if (.not.is_missing(proc1%input_item(i_input)%actdef)) cycle
                    valnam = proc1%input_item(i_input)%name
                    valtxt = proc1%input_item(i_input)%item%text
                    write(line, '(4a)') '       [', valnam, '] ', valtxt
                    call write_log_message(line)

                    10          continue

                    ! specified in input?

                    call valpoi (num_substances_total, num_spatial_parameters, num_spatial_time_fuctions, syname, num_constants, &
                            num_time_functions, constants, paname, funame, sfname, &
                            valnam, ivalip, line)

                    ! output of previous proces ? , is this switched on , switch it on

                    if (ivalip == -1) then

                        ! output of previous proces ? , including own pre-workspace

                        do iproc2 = 1, iproc

                            proc2 => procesdef%procesprops(iproc2)

                            ! flux of process

                            call zoekio (valnam, proc2%no_fluxoutput, proc2%fluxoutput, 20, iflux)
                            if (iflux > 0) then
                                if (proc2%linvok) then
                                    write (line, '(3a)') '       Using flux from proces [', proc2%name, ']'
                                    ivalip = -2
                                    exit
                                else
                                    write (line, '(3a)') '  NOT  Using flux from proces [', proc2%name, ']'
                                endif
                            endif

                            ! output variable of process

                            call zoekio (valnam, proc2%no_output, proc2%output_item, 20, ioutput, IOTYPE_SEGMENT_OUTPUT)
                            if (ioutput > 0) then
                                if (proc2%linvok) then
                                    write (line, '(3a)') '       Using output from proces [', proc2%name, ']'
                                    ivalip = -3
                                    exit
                                else
                                    write(line, '(3a)') '  NOT  Using output from proces [', proc2%name, ']'
                                endif
                            endif

                        enddo
                    endif
                    if (ivalip == -1) then

                        ! if this is a fraction input then first look for the generic name

                        i_star = index(valnam, '*')
                        if (i_star > 1) then
                            valnam(i_star:) = ' '
                            write(line, '(a)') '       fraction specific input not found, trying generic name'
                            call write_log_message(line)
                            write(line, '(4a)') '       [', valnam, '] ', valtxt
                            call write_log_message(line)
                            goto 10
                        endif

                        if (is_missing(proc1%input_item(i_input)%actdef))then
                            write(line, '(a)') '       not found'
                            proc1%linvok = .false.
                            nmis = nmis + 1
                            if (nmis <= mismax) then
                                misnam(nmis) = valnam
                                mistxt(nmis) = valtxt
                            endif
                        else
                            write(line, '(a)') '       can use default value'
                        endif
                    endif
                    call write_log_message(line)

                endif
            enddo

            ! on exchange level, seperate loop, then report file stays compatible

            do i_input = 1, proc1%no_input

                if (proc1%input_item(i_input)%type == IOTYPE_EXCHANG_INPUT) then

                    if (.not.is_missing(proc1%input_item(i_input)%actdef)) cycle
                    valnam = proc1%input_item(i_input)%name
                    valtxt = proc1%input_item(i_input)%item%text
                    write(line, '(4a)') '       [', valnam, '] ', valtxt
                    call write_log_message(line)

                    20          continue

                    ! specified in input?

                    call vxlpoi (num_constants, num_time_functions, num_dispersion_arrays, num_velocity_arrays, constants, &
                            funame, diname, vename, valnam, ivalip, &
                            line)

                    ! output of previous proces ? , is this switched on , switch it on

                    if (ivalip == -1) then

                        ! output of previous proces ? , including own pre-workspace

                        do iproc2 = 1, iproc

                            proc2 => procesdef%procesprops(iproc2)

                            ! xoutput variable of process

                            call zoekio (valnam, proc2%no_output, proc2%output_item, 20, ioutput, IOTYPE_EXCHANG_OUTPUT)
                            if (ioutput > 0) then
                                if (proc2%linvok) then
                                    write (line, '(3a)') '       Using output from proces [', proc2%name, ']'
                                    ivalip = -3
                                    exit
                                else
                                    write(line, '(3a)') '  NOT  Using output from proces [', proc2%name, ']'
                                endif
                            endif

                        enddo
                    endif
                    if (ivalip == -1) then

                        ! if this is a fraction input then first look for the generic name

                        i_star = index(valnam, '*')
                        if (i_star > 1) then
                            valnam(i_star:) = ' '
                            write(line, '(a)') '       fraction specific input not found, trying generic name'
                            call write_log_message(line)
                            write(line, '(4a)') '       [', valnam, '] ', valtxt
                            call write_log_message(line)
                            goto 20
                        endif

                        if (is_missing(proc1%input_item(i_input)%actdef))then
                            write(line, '(a)') '       not found'
                            proc1%linvok = .false.
                            nmis = nmis + 1
                            if (nmis <= mismax) then
                                misnam(nmis) = valnam
                                mistxt(nmis) = valtxt
                            endif
                        else
                            write(line, '(a)') '       can use default value'
                        endif
                    endif
                    call write_log_message(line)
                endif
            enddo

            550    continue
            if (laswi) then
                iact = index_in_array(proc1%name, actlst(:no_act))
                if (iact > 0) then
                    if (proc1%linvok) then
                        proc1%active = .true.
                        write(line, '(a)') '   Process is activated'
                        call write_log_message(line)
                        write(line, '(a,a)') '   Process subroutine: ', proc1%routine
                        call write_log_message(line)
                    else
                        if (proc1%name(1:8)=='VertDisp') then
                            call status%increase_warning_count()
                            write(line, '(a)') '   WARNING : VertDisp can NOT be switched on, is this a 2D model?'
                        else
                            call status%increase_error_count()
                            write(line, '(a)') '   ERROR : activated process can NOT be switched on'
                        end if
                        call write_log_message(line)
                        do imis = 1, min(nmis, mismax)
                            write(line, '(4a)') '   Not found:[', misnam(imis), '] ', mistxt(imis)
                            call write_log_message(line)
                        enddo
                        if (nmis > mismax) then
                            write(line, '(a)') '   and more ...'
                            call write_log_message(line)
                        endif
                    endif
                else
                    proc1%linvok = .false.
                    write(line, '(a)') '   Process is not activated'
                    call write_log_message(line)
                endif
            else
                if (proc1%linvok) then
                    write(line, '(a)') '   Proces can be switched on'
                else
                    write(line, '(a)') '   Proces can NOT be switched on'
                endif
                call write_log_message(line)
            endif
            line = ' '
            call write_log_message(line)
        enddo
        call write_log_message(line)

        if (timon) call timstop(ithndl)
        return
    end

end module m_makbar
