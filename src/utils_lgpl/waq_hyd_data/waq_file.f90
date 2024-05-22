!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
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

module m_waq_file
    !! module contains everything for the files
    use m_logger, only : terminate_execution, get_log_unit_number

    implicit none

    integer, parameter, private :: FILE_NAME_SIZE = 256          ! length filenames
    integer, parameter, private :: NAME_SIZE = 20          ! size of descriptive names
    integer, parameter, private :: TEXT_SIZE = 40          ! descriptive text size

    ! platform types
    integer, parameter :: PL_DOS = 1          ! DOS kind of platform
    integer, parameter :: PL_UNX = 2          ! UNIX kind of platform

    ! file system types
    integer, parameter :: FS_DOS = 1          ! DOS kind of files
    integer, parameter :: FS_UNX = 2          ! UNIX kind of files
    integer, parameter :: FS_ASC = 3          ! ASCII kind of files

    ! file types
    integer, parameter :: FT_ASC = 1          ! ASCII kind of file
    integer, parameter :: FT_UNF = 2          ! UNFORMATTED kind of file
    integer, parameter :: FT_BIN = 3          ! BINARY kind of file
    integer, parameter :: FT_SDS = 4          ! SIMONA kind of file
    integer, parameter :: FT_NEF = 5          ! ASCII kind of file

    ! file status
    integer, parameter :: FILE_STAT_UNOPENED = 0          ! file not opened
    integer, parameter :: FILE_STAT_OPENED = 1          ! file openend
    integer, parameter :: FILE_STAT_INIT = 2          ! file initialised (header)
    integer, parameter :: FILE_STAT_CLOSED = 3          ! file closed

    ! data type to define a single file
    type t_file
        character(len = FILE_NAME_SIZE) :: name                   ! name of file
        character(len = TEXT_SIZE) :: description            ! description of file
        integer :: unit                ! unit number
        integer :: type                ! file type to be used
        integer :: status              ! status

    contains
        procedure :: open => open_waq_file
        procedure :: close => close_waq_file
    end type t_file

    private
    public :: t_file
    public :: which_operating_system
    public :: FILE_STAT_UNOPENED, FILE_STAT_OPENED, FILE_STAT_INIT, FILE_STAT_CLOSED
    public :: FT_ASC, FT_UNF, FT_BIN, FT_SDS, FT_NEF, FS_DOS, FS_UNX, FS_ASC, PL_DOS, PL_UNX

contains

    subroutine open_waq_file(self)
        !! if successful, the file is opened and the status is set to 1

        class(t_file), intent(inout) :: self               ! the file to be opened

        integer :: io_error                  ! error indicator
        character(len = 1000) :: message      ! error message
        integer :: file_unit                 ! unit number report file

        if (self%status == 0) then
            if (self%type == FT_ASC) then
                open(newunit = self%unit, file = self%name, status = 'unknown', iostat = io_error, &
                        iomsg = message)
            elseif (self%type == FT_BIN) then
                open(newunit = self%unit, file = self%name, status = 'unknown', access = 'stream', &
                        iostat = io_error, iomsg = message)
            elseif (self%type == FT_UNF) then
                open(newunit = self%unit, file = self%name, status = 'unknown', form = 'unformatted', &
                        iostat = io_error, iomsg = message)
            else
                call get_log_unit_number(file_unit)
                write(*, *) 'ERROR opening file:', trim(self%name)
                write(file_unit, *) 'ERROR opening file:', trim(self%name)
                write(*, *) 'unknown filetype:', self%type
                write(file_unit, *) 'unknown filetype:', self%type
                call terminate_execution(1)
            endif
            if (io_error /= 0) then
                call get_log_unit_number(file_unit)
                write(*, *) 'ERROR opening file:', trim(self%name)
                write(file_unit, *) 'ERROR opening file:', trim(self%name)
                write(*, *) 'ERROR message: ', trim(message)
                write(file_unit, *) 'ERROR message: ', trim(message)
                call terminate_execution(1)
            endif
            self%status = 1
        endif
    end subroutine open_waq_file

    subroutine close_waq_file(self)
        !! if successful, the file is closed and the status is set to 0
        class(t_file) :: self               ! the file to be closed

        if (self%status /= 0) then
            close(self%unit)
            self%status = 0
        endif

    end subroutine close_waq_file

    function which_operating_system() result(os)
        integer :: os               ! result os type
        os = PL_DOS
    end function which_operating_system

end module m_waq_file
