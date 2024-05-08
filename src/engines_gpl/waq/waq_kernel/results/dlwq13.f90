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

contains


    SUBROUTINE write_restart_map_file(file_unit_list, file_name_list, concentration_values, time_clock_unit, &
            model_name, substances_names, num_systems, num_segments)
        ! gives a complete system dump

        !     concentration_values    REAL     num_systems*?     INPUT   concentration values
        !     time_clock_unit   INTEGER  1           INPUT   present time in clock units
        !     model_name   CHAR*40  4           INPUT   model identhification
        !     substances_names   CHAR*20  num_systems       INPUT   names of substances
        !     num_systems   INTEGER  1           INPUT   total number of systems
        !     num_segments   INTEGER  1           INPUT   total number of segments

        use m_open_waq_files
        use timers

        real(kind = real_wp) :: concentration_values(num_systems, num_segments)
        integer(kind = int_wp), intent(in) :: num_segments, num_systems, time_clock_unit
        character(len = 20), intent(in) :: substances_names(*)
        character(len = 40) :: model_name(*)
        character(len = *) :: file_name_list(*)
        character(len = 255) :: file_name
        integer(kind = int_wp) :: file_unit_list(*)

        integer(kind = int_wp) :: i, j, k

        integer(kind = int_wp) :: nonan, ierr, ithandl = 0

        if (timon) call timstrt ("write_restart_map_file", ithandl)

        ! check for NaNs
        nonan = 0
        do j = 1, num_segments
            do i = 1, num_systems
                if (concentration_values(i, j) /= concentration_values(i, j)) then
                    concentration_values(i, j) = 0.0
                    nonan = nonan + 1
                endif
            enddo
        enddo

        if (nonan /= 0) then
            write (file_unit_list(19), *) ' Corrected concentrations as written to the restart file:'
            write (file_unit_list(19), *) ' Number of values reset from NaN to zero: ', nonan
            write (file_unit_list(19), *) ' Total amount of numbers in the array: ', num_systems * num_segments
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

        WRITE (*, *) ' Invalid name of restart MAP file !'
        write (*, *) ' Restart file written to restart_temporary.map !'
        WRITE (file_unit_list(19), *) ' Invalid name of restart MAP file !'
        write (file_unit_list(19), *) ' Restart file written to restart_temporary.map !'
        file_name = 'restart_temporary.map'

        20 CALL open_waq_files (file_unit_list(23), file_name, 23, 1, IERR)
        WRITE (file_unit_list(23)) (model_name(K), K = 1, 4)
        WRITE (file_unit_list(23))   num_systems, num_segments
        WRITE (file_unit_list(23)) (substances_names(K), K = 1, num_systems)
        WRITE (file_unit_list(23)) time_clock_unit, concentration_values
        CLOSE (file_unit_list(23))

        if (timon) call timstop (ithandl)

    END SUBROUTINE write_restart_map_file

end module m_write_restart_map_file
