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
module m_repuse
    use m_waq_precision
    use m_string_utils

    implicit none

contains


    subroutine repuse (procesdef, nocons, coname, nopa, paname, &
            nofun, funame, nosfun, sfname, noinfo)

        ! report on the use of the delwaq input

        use m_monsys
        use processet
        use timers       !   performance timers
        use string_module

        implicit none

        ! declaration of arguments

        type(procespropcoll) :: procesdef       ! all processes
        integer(kind = int_wp) :: nocons          ! number of constants
        character(len = *) :: coname(*)       ! constant names
        integer(kind = int_wp) :: nopa            ! number of parameters
        character(len = *) :: paname(*)       ! parameter names
        integer(kind = int_wp) :: nofun           ! number of functions
        character(len = *) :: funame(*)       ! function names
        integer(kind = int_wp) :: nosfun          ! number of segment functions
        character(len = *) :: sfname(*)       ! segment function names
        integer(kind = int_wp) :: noinfo          ! number of informative messages

        ! local declarations

        integer(kind = int_wp), parameter :: nopred = 6       ! number of predefined defaults
        integer(kind = int_wp) :: nproc            ! number of processes
        integer(kind = int_wp) :: iproc            ! loop counter processes
        type(procesprop), pointer :: proc             ! process description
        character(len = 80) :: line             ! output buffer
        integer(kind = int_wp) :: icons            ! index constants
        integer(kind = int_wp) :: ipcons           ! pointer to constant in delwaq data space
        integer(kind = int_wp) :: ipa              ! index parameters
        integer(kind = int_wp) :: ippa             ! pointer to parameter in delwaq data space
        integer(kind = int_wp) :: ifun             ! index fun
        integer(kind = int_wp) :: ipfun            ! pointer to fun in delwaq data space
        integer(kind = int_wp) :: isfun            ! index sfun
        integer(kind = int_wp) :: ipsfun           ! pointer to sfun in delwaq data space
        integer(kind = int_wp) :: i_input          ! index
        integer(kind = int_wp) :: ioutp            ! index
        logical :: variable_is_used ! indicates if variable is used
        integer(kind = int_wp) :: iused            ! index
        integer(kind = int_wp) :: ithndl = 0
        character(len = 24) :: spcl_const_print ! special constants not used printed in brackets for lsp file

        character(len = 17), dimension(21), parameter :: special_constants = & ! names of special constants
                     (/ 'SURF             ', 'SWSCALE          ', &
                        'CLOSE_ERR        ', 'MIN_VOLUME       ', &
                        'THETA            ', 'MIN_AREA         ', &
                        'TIMMULTBL        ', 'ZTHRESHOLD       ', &
                        'NOVEC            ', 'Z_THRESH         ', &
                        'TOLERANCE        ', 'ONLY_ACTIVE      ', &
                        'ACTIVE           ', 'NOTHREADS        ', &
                        'ITERATION        ', 'NUMBER_OF_BUCKETS', &
                        'LENGTH           ', 'DRY_TRESH        ', &
                        'SWPRECOND        ', 'ITERATION REPORT ', &
                        'MAXITER          '/)

        if (timon) call timstrt("repuse", ithndl)

        ! write header report output block

        write (line, '(a)') '# determining the use of the delwaq input'
        call monsys(line, 4)
        line = ' '
        call monsys(line, 4)

        nproc = procesdef%current_size

        ! loop over the constants

        do icons = 1, nocons

            variable_is_used = .false.
            ipcons = nopred + icons

            ! loop over processes

            do iproc = nproc, 1, -1
                proc => procesdef%procesprops(iproc)
                if (proc%active) then

                    ! check if constant is used as input in this process

                    do i_input = 1, proc%no_input
                        if (proc%input_item(i_input)%type == IOTYPE_SEGMENT_INPUT) then
                            if (ipcons == proc%input_item(i_input)%ip_val) then
                                variable_is_used = .true.
                                exit
                            endif
                        endif
                    enddo
                    if (variable_is_used) exit
                endif
            enddo

            ! check if special constants are used

            variable_is_used = ANY(special_constants == str_toupper(coname(icons)))

            ! report if not used

            if (.not. variable_is_used) then
                noinfo = noinfo + 1
                write(spcl_const_print, '(3a)') '[', trim(coname(icons)), ']'
                write (line, '(3a)') ' info: constant ', spcl_const_print, ' is not used by the process system'
                call monsys(line, 4)
            endif
        enddo

        ! loop over the parameters

        do ipa = 1, nopa

            variable_is_used = .false.
            ippa = nopred + nocons + ipa

            ! loop over processes

            do iproc = nproc, 1, -1
                proc => procesdef%procesprops(iproc)
                if (proc%active) then

                    ! check if constant is used as input in this process

                    do i_input = 1, proc%no_input
                        if (ippa == proc%input_item(i_input)%ip_val) then
                            variable_is_used = .true.
                            exit
                        endif
                    enddo
                    if (variable_is_used) exit
                endif
            enddo

            ! report if not used and not an output parameter

            if (.not. variable_is_used .and. string_equals('output    ', paname(ipa))) then
                noinfo = noinfo + 1
                write (line, '(3a)') ' info: parameter [', paname(ipa)(1:10), '] is not used by the process system'
                call monsys(line, 4)
            endif
        enddo

        ! loop over the functions

        do ifun = 1, nofun

            variable_is_used = .false.
            ipfun = nopred + nocons + nopa + ifun

            ! loop over processes

            do iproc = nproc, 1, -1
                proc => procesdef%procesprops(iproc)
                if (proc%active) then

                    ! check if constant is used as input in this process

                    do i_input = 1, proc%no_input
                        if (ipfun == proc%input_item(i_input)%ip_val) then
                            variable_is_used = .true.
                            exit
                        endif
                    enddo
                    if (variable_is_used) exit
                endif
            enddo

            ! report if not used

            if (.not. variable_is_used) then
                noinfo = noinfo + 1
                write (line, '(3a)') ' info: function [', funame(ifun)(1:10), '] is not used by the process system'
                call monsys(line, 4)
            endif
        enddo

        ! loop over the segment functions

        do isfun = 1, nosfun

            variable_is_used = .false.
            ipsfun = nopred + nocons + nopa + nofun + isfun

            ! loop over processes

            do iproc = nproc, 1, -1
                proc => procesdef%procesprops(iproc)
                if (proc%active) then

                    ! check if constant is used as input in this process

                    do i_input = 1, proc%no_input
                        if (ipsfun == proc%input_item(i_input)%ip_val) then
                            variable_is_used = .true.
                            exit
                        endif
                    enddo
                    if (variable_is_used) exit
                endif
            enddo

            ! report if not used

            if (.not. variable_is_used) then
                noinfo = noinfo + 1
                write (line, '(3a)') ' info: segment function [', sfname(isfun)(1:10), '] is not used by the process system'
                call monsys(line, 4)
            endif
        enddo

        line = ' '
        call monsys(line, 4)

        if (timon) call timstop(ithndl)
        return
    end

end module m_repuse
