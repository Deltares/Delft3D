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
module m_read_hydfile
    use m_waq_precision
    use m_error_status

    implicit none

contains



    subroutine read_hydfile(lunout, hydfile, file_name_list, num_cells, nexch, status)
        use waq_file_utils_external, only : get_filepath_and_pathlen

        !> Reads the hyd-file and extracts relevant information

        integer(kind = int_wp), intent(in) :: lunout       !< unit number for reporting
        character(len = *), intent(in) :: hydfile      !< name of the hyd-file to read
        character(len = *), intent(inout) :: file_name_list(*)     !< filenames
        integer(kind = int_wp), intent(out) :: num_cells        !< number of segments
        integer(kind = int_wp), dimension(*), intent(out) :: nexch        !< number of exchanges

        type(error_status), intent(inout) :: status !< current error status

        character(len = 400) :: line
        character(len = 400) :: path
        character(len = 20) :: cdummy
        character(len = 20), dimension(10) :: keyword
        integer(kind = int_wp), dimension(10) :: fileno
        integer(kind = int_wp) :: i, ierr2, input_file, idxlga, idxgeom, pathlen

        integer(kind = int_wp) :: num_cells_u_dir, num_cells_v_dir, nosegl, num_layers, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir
        character(len = 4) :: identifier
        character(len = len(file_name_list)) :: grid_file

        ! Read the various file names
        ! Note:
        ! The subroutine validate_simulation_time_steps checks the times in the files and we do not know the
        ! number of items yet. So either we need to make that routine more complex
        ! or exploit the simple structure of the hyd-file.
        keyword(1:8) = ['volumes-file        ', 'areas-file          ', 'flows-file          ', 'pointers-file       ', &
                'lengths-file        ', 'attributes-file     ', 'grid-indices-file   ', 'waqgeom-file        ']
        fileno(1:8) = [7, 10, 11, 44, 13, 40, 6, 46]

        idxlga = -1
        idxgeom = -1

        call get_filepath_and_pathlen(hydfile, path, pathlen)

        open(newunit = input_file, file = hydfile, status = 'old', iostat = status%ierr)
        if (status%ierr /= 0) then
            write(lunout, '(a,a)') 'ERROR: Hyd-file does not exist or could not be opened - ', trim(hydfile)
            return
        endif

        do
            read(input_file, '(a)', iostat = ierr2) line

            if (ierr2 < 0) then
                exit
            endif
            if (ierr2 > 0) then
                status%ierr = ierr2
                write(lunout, '(a,a)') 'ERROR: Reading hyd-file failed - ', trim(hydfile)
                return
            endif

            do i = 1, 8
                if (index(line, keyword(i)) > 0) then
                    read(line, *, iostat = ierr2) cdummy, file_name_list(fileno(i))
                    if (ierr2 > 0) then
                        status%ierr = ierr2
                        write(lunout, '(a,a)') 'ERROR: Reading hyd-file failed - ', trim(hydfile)
                        write(lunout, '(a,a)') '       Line: ', trim(line)
                        return
                    endif

                    file_name_list(fileno(i)) = path(1:pathlen) // file_name_list(fileno(i))

                    if (i == 7) then  ! LGA file
                        idxlga = fileno(i)
                    endif
                    if (i == 8) then  ! WAQGEOM file
                        idxgeom = fileno(i)
                    endif

                    exit
                endif
            enddo

            if (index(line, 'number-horizontal-exchanges') > 0) then
                read(line, *) cdummy, nexch(1)
            endif
            if (index(line, 'number-vertical-exchanges') > 0) then
                read(line, *) cdummy, nexch(3)
            endif
            if (index(line, 'number-water-quality-segments-per-layer') > 0) then
                read(line, *) cdummy, nosegl
            endif
            if (index(line, 'number-water-quality-layers') > 0) then
                read(line, *) cdummy, num_layers
            endif
            if (index(line, 'grid-coordinates-file') > 0) then
                read(line, *) cdummy, grid_file
            endif
        enddo

        num_cells = nosegl * num_layers
        close(input_file)

        ! Read the number of grid cells:
        ! - LGRID file
        ! - WAQGEOM file
        if (idxgeom > 0) then
            !
            ! Retrieved via keywords
            !
            num_cells = nosegl * num_layers
        elseif (idxlga > 0) then
            open(newunit = input_file, file = file_name_list(idxlga), access = 'stream', iostat = ierr2)

            if (ierr2 /= 0) then
                call status%increase_error_count()
                write(lunout, '(a,a)') 'ERROR: LGA-file does not exist or could not be opened - ', trim(file_name_list(8))
                return
            endif

            ! Check that it is not a NetCDF 3/4 file -- UNTRIM
            read(input_file, iostat = ierr2) identifier
            if (identifier(1:3) == 'CDF' .or. identifier(2:4) == 'HDF') then

                ! We have a hyd-file from UNTRIM, so use the other file name
                idxgeom = fileno(8)
                file_name_list(idxgeom) = grid_file
            else
                rewind(input_file)
                read(input_file, iostat = ierr2) num_cells_u_dir, num_cells_v_dir, nosegl, num_layers, nexch(1), nexch(2), nexch(3)

                if (ierr2 /= 0) then
                    call status%increase_error_count()
                    write(lunout, '(a,a)') 'ERROR: Header of LGA-file could not be read - ', trim(file_name_list(8))
                    return
                endif
            endif

            num_cells = nosegl * num_layers
            close(input_file)
        else
            close(input_file)
            write(lunout, '(a,a)') 'ERROR: Hyd-file does not contain the name for a grid file - ', trim(hydfile)
            call status%increase_error_count()
            return
        endif
    end subroutine read_hydfile

end module m_read_hydfile
