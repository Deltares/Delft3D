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
module m_write_restart_map_file
    use m_waq_precision

    implicit none

    private
    public :: write_restart_map_file

contains


    !> Writes the concentrations of all substances for all cells or segments to a restart map file
    subroutine write_restart_map_file(file_unit_list, file_name_list, concentration_values, time_clock_unit, &
            model_name, substances_names, num_systems, num_cells)

        use m_open_waq_files
        use timers

        integer(kind = int_wp), intent(inout) :: file_unit_list(*) !< Array containing all file unit numbers
        integer(kind = int_wp), intent(in   ) :: num_cells      !< Number of cells or segments
        integer(kind = int_wp), intent(in   ) :: num_systems       !< Number of substances
        integer(kind = int_wp), intent(in   ) :: time_clock_unit   !< present time in clock units

        real(kind = real_wp),intent(inout) :: concentration_values(num_systems, num_cells) !< Concentrations of all substances in all cells or segments

        character(len = 20),  intent(in   ) :: substances_names(*) !< Names of substances
        character(len = 40),  intent(in   ) :: model_name(*)       !< Name of the model
        character(len = *),   intent(in   ) :: file_name_list(*)   !< Names of all output files

        ! Local variables
        integer(kind = int_wp) :: i, j, k
        integer(kind = int_wp) :: nan_count, ierr, ithandl = 0

        character(len = 255) :: file_name

        if (timon) call timstrt ("write_restart_map_file", ithandl)

        ! check for NaNs
        nan_count = 0
        do j = 1, num_cells
            do i = 1, num_systems
                if (concentration_values(i, j) /= concentration_values(i, j)) then
                    concentration_values(i, j) = 0.0
                    nan_count = nan_count + 1
                endif
            enddo
        enddo

        if (nan_count /= 0) then
            write (file_unit_list(19), *) ' Corrected concentrations as written to the restart file:'
            write (file_unit_list(19), *) ' Number of values reset from NaN to zero: ', nan_count
            write (file_unit_list(19), *) ' Total amount of numbers in the array: ', num_systems * num_cells
            write (file_unit_list(19), *) ' This may indicate that the computation was unstable'
        endif

        ! write restart file in .map format
        file_name = ' '
        file_name(1:248) = file_name_list(23)(1:248)
        DO I = 248, 1, -1
            IF (file_name(I:I) == '.') THEN
                file_name(I:I + 7) = "_res.map"
                GOTO 20
            ENDIF
        end do

        write (*, *) ' Invalid name of restart MAP file !'
        write (*, *) ' Restart file written to restart_temporary.map !'
        write (file_unit_list(19), *) ' Invalid name of restart MAP file !'
        write (file_unit_list(19), *) ' Restart file written to restart_temporary.map !'
        file_name = 'restart_temporary.map'

        20 call open_waq_files (file_unit_list(23), file_name, 23, 1, ierr)
        write (file_unit_list(23)) (model_name(k), k = 1, 4)
        write (file_unit_list(23))   num_systems, num_cells
        write (file_unit_list(23)) (substances_names(k), k = 1, num_systems)
        write (file_unit_list(23)) time_clock_unit, concentration_values
        close (file_unit_list(23))

        if (timon) call timstop (ithandl)

    end subroutine write_restart_map_file
end module m_write_restart_map_file
