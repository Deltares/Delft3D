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
module m_add_atrfrc
    use m_waq_precision

    implicit none

contains


    subroutine add_atrfrc(lunrep, procesdef, sfracs)

        ! add attributes to processes from file

        use data_processing, only : extract_value_from_group
        use m_cli_utils, only : get_command_argument_by_name
        use m_string_manipulation, only : upper_case
        use processet
        use timers       !   performance timers

        implicit none

        ! decalaration of arguments

        integer(kind = int_wp) :: lunrep          ! report file
        type(procespropcoll) :: procesdef       ! the process definition
        type(sfracsprop) :: sfracs          ! substance fraction properties

        ! local declaration

        type(procesprop), pointer :: proc              ! single process
        integer(kind = int_wp) :: num_processes_activated             ! number of processes
        integer(kind = int_wp) :: iproc             ! loop counter processes
        character(:), allocatable :: patrfil        ! process attributes file
        integer(kind = int_wp) :: lun_patr          ! unit number
        character(len = 256) :: type              ! sfrac_type from file
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("add_atrfrc", ithndl)

        if (get_command_argument_by_name('-sfrac', patrfil)) then
            open(newunit = lun_patr, file = patrfil)

            ! loop over the processes

            num_processes_activated = procesdef%current_size
            do iproc = 1, num_processes_activated

                proc => procesdef%procesprops(iproc)
                call extract_value_from_group(lun_patr, proc%name, 'sfrac_type', type)
                call upper_case(type, type, len(type))
                if (type == 'SPLITFLUX') then
                    proc%sfrac_type = SFRAC_SPLITFLUX
                elseif (type == 'DUPLICATE') then
                    proc%sfrac_type = SFRAC_DUPLICATE
                elseif (type == 'EXPAND') then
                    proc%sfrac_type = SFRAC_EXPAND
                endif

            enddo

            close(lun_patr)
        endif

        if (timon) call timstop(ithndl)
        return
    end

end module m_add_atrfrc
