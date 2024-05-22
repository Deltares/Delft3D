module m_set_active
    use m_waq_precision
    use m_string_utils

    implicit none

contains

    !----- GPL ---------------------------------------------------------------------
    !
    !  Copyright (C)  Stichting Deltares, 2011-2024.
    !
    !  This program is free software: you can redistribute it and/or modify
    !  it under the terms of the GNU General Public License as published by
    !  the Free Software Foundation version 3.
    !
    !  This program is distributed in the hope that it will be useful,
    !  but WITHOUT ANY WARRANTY; without even the implied warranty of
    !  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    !  GNU General Public License for more details.
    !
    !  You should have received a copy of the GNU General Public License
    !  along with this program.  If not, see <http://www.gnu.org/licenses/>.
    !
    !  contact: delft3d.support@deltares.nl
    !  Stichting Deltares
    !  P.O. Box 177
    !  2600 MH Delft, The Netherlands
    !
    !  All indications and logos of, and references to, "Delft3D" and "Deltares"
    !  are registered trademarks of Stichting Deltares, and remain the property of
    !  Stichting Deltares. All rights reserved.
    !
    !-------------------------------------------------------------------------------
    !
    !

    subroutine set_active(constants, no_act_max, no_act, actlst)

        !     Deltares Software Centre

        !>/File
        !>                makes list of active processes

        use m_logger_helper, only : stop_with_error, write_log_message
        use timers         !< performance timers
        use m_waq_data_structure      !< data definitions
        use processet      !< use processet definitions
        implicit none

        ! arguments

        type(t_waq_item), intent(inout) :: constants              !< delwaq constants list
        integer(kind = int_wp), intent(in) :: no_act_max             !< number of activated processes max
        integer(kind = int_wp), intent(inout) :: no_act                 !< number of activated processes
        character(len = *), intent(inout) :: actlst(*)              !< list of activated processes

        ! local declarations

        integer(kind = int_wp) :: nocons                 !  number of constants
        integer(kind = int_wp) :: ico                    !  loop counter constants
        integer(kind = int_wp) :: i_act                  !  loop counter active
        integer(kind = int_wp) :: ix_act                 !  index active
        integer(kind = int_wp) :: ix_dbl                 !  index double
        character(len = 10) :: name10                 !  local process name
        character(len = 80) :: line                   !  line buffer for report file
        integer(kind = int_wp) :: i_old_item             !  loopcounter old_items
        integer(kind = int_wp) :: ithndl = 0             !  handle for performance timer
        if (timon) call timstrt("set_active", ithndl)

        ! check the actives in the constant names

        nocons = constants%no_item
        do ico = 1, nocons
            if (string_equals('active', constants%name(ico))) then

                ! check if double in the list

                name10 = constants%name(ico)(8:17)
                ix_dbl = index_in_array(name10, actlst(:no_act))
                if (ix_dbl <= 0) then
                    no_act = no_act + 1
                    if (no_act > no_act_max) then
                        write(line, 2130)
                        call write_log_message(line)
                        write(line, 2110) no_act, no_act_max
                        call write_log_message(line)
                        call stop_with_error()
                    endif
                    actlst(no_act) = name10
                endif
            endif
        enddo

        ! if bloom then also phy_blo

        name10 = 'bloom'
        ix_dbl = index_in_array(name10, actlst(:no_act))
        if (ix_dbl > 0) then
            name10 = 'phy_blo'
            ix_dbl = index_in_array(name10, actlst(:no_act))
            if (ix_dbl <= 0) then
                write(line, 2140)
                call write_log_message(line)
                no_act = no_act + 1
                if (no_act > no_act_max) then
                    write(line, 2130)
                    call write_log_message(line)
                    write(line, 2110) no_act, no_act_max
                    call write_log_message(line)
                    call stop_with_error()
                endif
                actlst(no_act) = name10
            endif
        endif

        if (timon) call timstop(ithndl)
        return
        2110 format (' in input :', I6, ' maximum :', I6)
        2130 format (' ERROR: Local dimension to small for active processes')
        2140 format (' Automatic activation of BLOOM ouput process Phy_Blo')
    end

end module m_set_active
