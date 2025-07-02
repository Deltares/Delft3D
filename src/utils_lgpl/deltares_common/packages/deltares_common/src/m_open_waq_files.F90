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
module m_open_waq_files
    use m_logger_helper, only : stop_with_error

    implicit none
    private
    public :: open_waq_files

contains

    subroutine open_waq_files(file_unit, file_name, file_id, mode, ierr)
        !! Opens all Delwaq files
        !! SHARED is used for those files which are optionally prepared at runtime by another programme
        !! running on-line. To enable this, the reading is synchronised (DLWQI0, DLWQT4)
        !!
        !! Files opened with this option are:
        !!      volumes, flows, areas, lengths, segment functions and harmonics (see local comments)
        integer, intent(inout) :: file_unit       !< unit number of file to be opened
        character(len=*), intent(in) :: file_name     !< name of the file to be opened
        integer, intent(in) :: file_id    !< Delwaq number of the file to be opened
        integer, intent(in) :: mode    !< Indicator how file must be opened
        integer, intent(inout) :: ierr      !< Error flag

        ierr = 0

        ! get the correct open statement
        select case (file_id)
            ! 1 = common-block file
        case (1)
            select case (mode)
            case (1)
                call open_unformatted_stream(file_unit, file_name, mode, ierr, file_id)
            case (2)
                open (newunit = file_unit, file = file_name, err = 910, form = 'unformatted', access = 'stream', &
                        status = 'old')
            case (3)
                open (file_unit, file = file_name, err = 910, form = 'unformatted', access = 'stream')
                close(file_unit, status = 'delete', err = 910)
            case default
                ierr = 3
            end select

            !  2 = system file;        4 = pointers functions;      5 = time steps file;   6 = grid layout;
            !  9 = dispersion file;   12 = velocities file;        14 = boundaries file;  15 = waste loads file;
            ! 16 = functions file;    18 = initial conditions file;24 = process work file;25 = output work file
        case (2, 4, 5, 6, 9, 12, 14, 15, 16, 18, 24, 25)
            call open_unformatted_stream(file_unit, file_name, mode, ierr, file_id, .true.)

            !  3 = harmonic functions; 7 = volumes file
            ! 10 = areas file;        11 = flows        ; 13 = length file; 17 = segment functions file
            ! 44 = pointer file
        case (3, 7, 10, 11, 13, 17, 44)
            call open_unformatted(file_unit, file_name, mode, ierr, file_id, support_old_status = .false.)

            !  8 = to-/from-pointers file;
            ! 40 = Binary segment attribute file
        case (8, 40)
            call open_unformatted(file_unit, file_name, mode, ierr, file_id, replace = .true.)

            ! 19 = DELWAQ2 monitoring file (.mon);
            ! 20 = dump file
        case (19, 20)
            select case (mode)
            case (1)
                open (newunit = file_unit, file = file_name, err = 910)
            case default
                ierr = 3
            end select

            ! 21 = history file; 22 = map file; 23 = restart file
        case (21, 22, 23)
            select case (mode)
            case (1)
                open (newunit = file_unit, file = file_name, err = 910, form = 'unformatted', access = 'stream', &
                        status = 'replace')
            case default
                ierr = 3
            end select

            ! 26 = input file
        case (26)
            select case (mode)
            case (1)
                open (file_unit, file = file_name, err = 900, status = 'old')
            case default
                ierr = 3
            end select

            ! 27 = stripped input file; 28 = aux. stripped input file
        case (27, 28)
            select case (mode)
            case (1)
                open (file_unit, status = 'scratch')
            case default
                ierr = 3
            end select

            ! 29 = DELWAQ1 input report file (.lst)
        case (29)
            select case (mode)
            case (1)
                open (file_unit, file = file_name, err = 910)
            case (2)
                open (file_unit, file = file_name, err = 900, status = 'new')
            case default
                ierr = 3
            end select

            ! 30 = dimensioning include file
        case (30)
            select case (mode)
            case (1)
                open (file_unit, file = file_name, err = 910)
            case (2)
                open (file_unit, file = file_name, err = 900, status = 'old')
            case default
                ierr = 3
            end select

            ! 31 = scratch file time functions 1; 32 = scratch file time functions 2
        case (31, 32)
            select case (mode)
            case (1)
                open (file_unit, err = 910, form = 'unformatted', access = 'stream', status = 'scratch')
            case default
                ierr = 3
            end select

            ! 33 = auxiliary input file
        case (33)
            select case (mode)
            case (1)
                open (file_unit, file = file_name, err = 900, status = 'old')
            case (2)
                open (file_unit, file = file_name, err = 900, form = 'unformatted', access = 'stream', status = 'old')
            case default
                ierr = 3
            end select

            ! 34 = proces definition file
        case (34)
            select case (mode)
            case (1)
                open (file_unit, file = file_name, err = 910, status = 'old')
            case (2)
                open (file_unit, file = file_name, err = 910, form = 'unformatted', access = 'stream', status = 'old')
            case (3)

                open (file_unit, file = file_name, err = 910, form = 'unformatted', access = 'stream', &
                        status = 'replace')
            case default
                ierr = 3
            end select

            ! 35 = DELWAQ1 proceses report file (.lsp); 36 = Proces stochi file
        case (35, 36)
            select case (mode)
            case (1)
                open (file_unit, file = file_name, err = 910)
            case default
                ierr = 3
            end select

            ! 37 = bal file
        case (37)
            select case (mode)
            case (1)
                open (newunit = file_unit, file = file_name, err = 910, form = 'unformatted', access = 'stream')
            case default
                ierr = 3
            end select

            ! 41 = ASCII file with filenames of binary files
        case (41)
            select case (mode)
            case (1)
                open (file_unit, file = file_name, err = 910)
            case (2)
                open (file_unit, file = file_name, err = 910, status = 'old')
            case default
                ierr = 3
            end select

            ! 42 = domain names configuration file, online dd
        case (42)
            select case (mode)
            case (1)
                open (file_unit, file = file_name, err = 900, status = 'old')
            case default
                ierr = 3
            end select

            ! no valid number present
        case default
            ierr = 2
        end select
        return

        ! error while opening with return value
        900 ierr = 1
        return

        ! error while opening STOP with message
        910 call report_error_and_stop(file_unit, file_id, file_name)
    end subroutine open_waq_files

    subroutine report_error_and_stop(file_unit, file_id, file_name)
        use cwd, only : getCWD

        integer, intent(in) :: file_unit       !< unit number of file to be opened
        character(len=*), intent(in) :: file_name     !< name of the file to be opened
        integer, intent(in) :: file_id    !< Delwaq number of the file to be opened

        integer ierr_cwd  !< Error flag for obtaining current working directory
        character(256) wd_path   !< Current working directory path

        wd_path = ''
        ierr_cwd = getCWD(wd_path)
        if (ierr_cwd /= 0) then
            wd_path = 'Current working directory not found!'
        endif

        write (*, 2000) file_id, file_unit, trim(file_name), trim(wd_path)
        call stop_with_error()


        2000 format (' ERROR opening file number:', I3, ' on unit:', I3, &
                /, ' Filename is: ', A, &
                /, ' Searching in working directory: ', /, '  ' A)
    end subroutine report_error_and_stop

    subroutine open_unformatted(file_unit, file_name, mode, ierr, file_id, support_old_status, replace)
        integer, intent(in) :: file_unit         !< unit number of file to be opened
        character(len=*), intent(in) :: file_name       !< name of the file to be opened
        integer, intent(in) :: file_id      !< Delwaq number of the file to be opened
        integer, intent(in) :: mode      !< Indicator how file must be opened
        integer, intent(inout) :: ierr     !< Error flag
        logical, intent(in), optional :: support_old_status !< use old status if applicable
        logical, intent(in), optional :: replace !< use replace status for mode 1

        logical :: old_supported
        integer :: stat

        old_supported = .true.
        stat = 0
        if (present(support_old_status)) then
            old_supported = support_old_status
        end if

        select case (mode)
        case (1, 2)
            call open_unformatted_stream(file_unit, file_name, mode, ierr, file_id)
            return
        case (11, 12)
            if (old_supported .and. mode == 12) then
                open (file_unit, file = file_name, iostat = stat, form = 'unformatted', status = 'old')
            else
                open (file_unit, file = file_name, iostat = stat, form = 'unformatted')
            end if
        case (21)
            open (file_unit, file = file_name, iostat = stat, form = 'unformatted', access = 'stream', &
                    convert = 'big_endian')
        case (22)
            open (file_unit, file = file_name, iostat = stat, form = 'unformatted', access = 'stream', status = 'old', &
                    convert = 'big_endian')
        case (31)
            open (file_unit, file = file_name, iostat = stat, form = 'unformatted', &
                    convert = 'big_endian')
        case (32)
            open (file_unit, file = file_name, iostat = stat, form = 'unformatted', status = 'old', &
                    convert = 'big_endian')
        case default
            ierr = 3
        end select

        if (stat /= 0) then
            call report_error_and_stop(file_unit, file_id, file_name)
        end if

    end subroutine open_unformatted

    subroutine open_unformatted_stream(file_unit, file_name, mode, ierr, file_id, replace)
        integer, intent(in) :: file_unit       !< unit number of file to be opened
        character(len=*), intent(in) :: file_name     !< name of the file to be opened
        integer, intent(in) :: file_id    !< Delwaq number of the file to be opened
        integer, intent(in) :: mode    !< Indicator how file must be opened
        integer, intent(inout) :: ierr      !< Error flag
        logical, intent(in), optional :: replace !< use replace status for mode 1

        logical :: replace_
        integer :: stat

        stat = 0
        replace_ = .false.
        if (present(replace)) then
            replace_ = replace
        endif

        ierr = 0
        select case (mode)
        case (1)
            if (replace_) then
                open (file_unit, file = file_name, iostat = stat, form = 'unformatted', access = 'stream', &
                        status = 'replace')
            else
                ! using unknown status
                open (file_unit, file = file_name, iostat = stat, form = 'unformatted', access = 'stream')
            end if
        case (2)
            open (file_unit, file = file_name, iostat = stat, form = 'unformatted', access = 'stream', status = 'old')
        case default
            ierr = 3
        end select
        if (stat /= 0) then
            call report_error_and_stop(file_unit, file_id, file_name)
        end if

    end subroutine open_unformatted_stream
end module m_open_waq_files
