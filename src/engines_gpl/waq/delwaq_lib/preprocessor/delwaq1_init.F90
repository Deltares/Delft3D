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
module m_delwaq1_init

    use m_working_files, only : create_work_file_one
    use m_waq_precision

    implicit none

contains

    subroutine delwaq1_init(argv)
        !< initializes timer and values

        use m_delwaq1_data
        use m_cli_utils, only : get_argument_from_list, store_command_arguments, get_number_of_arguments

        character(len = *), dimension(:), intent(in) :: argv

        integer(kind = int_wp) :: arg_index

        !     Special system init

        call timini()                          ! initializes timer

        call store_command_arguments(argv)

        narg = get_number_of_arguments()            ! but timer is switched 'off' by default
        if (narg == 0) narg = command_argument_count() + 1
        do arg_index = 1, narg
            call get_argument_from_list(arg_index, arg)
            if (arg == "timer" .or. arg == "TIMER") then
                timon = .true.                     ! optionally switch it 'on'
                exit
            end if
        end do
        if (timon) call timstrt("delwaq1", ithndl)

        !        initialise values

        lunrep = file_unit_list(29)
        num_file_units = num_files
        filtype = 0
        noitem = noitm
        noutp = nooutp
        nharms = 0
        niharm = 0
        nlines = 0
        npoins = 0
        newrsp = 0
        newisp = 0
        ivflag = 0
        itflag = 0
        ncbufm = 0
        novar = 0
        noarr = iasize + ijsize + icsize
        nufil = 0
        do i = 1, noitem
            nrftot(i) = 0
            nrharm(i) = 0
        end do
        StatProcesDef%maxsize = 0
        StatProcesDef%current_size = 0
        AllItems%maxsize = 0
        AllItems%current_size = 0
        GridPs%current_size = 0
        GridPs%maxsize = 0

        call create_work_file_one(file_unit_list, file_name_list, num_file_units, runid)

    end subroutine delwaq1_init
end module m_delwaq1_init
