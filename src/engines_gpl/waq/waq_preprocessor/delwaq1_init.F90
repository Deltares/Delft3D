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

    subroutine delwaq1_init()
        !< initializes timer and values

        use m_delwaq1_data
        use m_cli_utils, only : is_command_arg_specified

        integer(kind = int_wp) :: arg_index

        !     Special system init

        call timini() ! initializes timer

        timon = is_command_arg_specified("-timer")

        if (timon) call timstrt("delwaq1", ithndl)

        !        initialise values

        lunrep = file_unit_list(29)
        num_file_units = num_files
        filtype = 0
        num_items_time_fn = noitm
        num_output_files = nooutp
        harmonics_arr_len = 0
        num_harmonics = 0
        nlines = 0
        num_indices = 0
        newrsp = 0
        newisp = 0
        ivflag = 0
        itflag = 0
        char_arr_buffer_len = 0
        num_vars = 0
        num_arrays = iasize + ijsize + icsize
        num_unformat_files = 0
        do i = 1, num_items_time_fn
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
