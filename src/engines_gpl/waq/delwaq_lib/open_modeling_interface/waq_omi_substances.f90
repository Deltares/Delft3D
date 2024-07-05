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

!> @file
!! Interface routines for dealing with the "predefined sets".
!! The routines must be accessible from C/C++/C#/Java
!!
!! TODO: log any errors
!!<

!> Implementation of the routines that read the substances file
!! and the parameters file.
!!
!! Motivation:
!! \li The classical substances file does not contain information about whether an item is a constant or a parameter.
!!     This is by design: that way the user can determine the character.
!! \li It is also necessary to add "hidden" items to the list - these items facilitate the computation without
!!     burdening the user with their existence. For example: CLOSE_ERR to control the closure error correction.
!! \li The parameters file introduced here makes sure that we can control these two aspects from "outside"
!!
module waq_omi_substances
    use m_waq_precision

    !use waq_omi_utils

    implicit none
    private

    public :: loadSubstancesFile
    public :: readParametersFile   ! Public for testing purposes
    public :: readSubstancesFile   ! Public for testing purposes
    public :: writeItems           ! Public for testing purposes
    public :: openSubstancesReport ! Public for testing purposes
    public :: testSetLunumbers     ! Public for testing purposes

    integer(kind = int_wp), parameter :: type_substance = 1
    integer(kind = int_wp), parameter :: type_inactive = 2
    integer(kind = int_wp), parameter :: type_constant = 3
    integer(kind = int_wp), parameter :: type_parameter = 4
    integer(kind = int_wp), parameter :: type_output = 5
    integer(kind = int_wp), parameter :: type_process = 6

    type itemInfo
        integer(kind = int_wp) :: type
        logical :: private
        real(kind = real_wp) :: value
        character(len = 20) :: name
        character(len = 20) :: unit
        character(len = 80) :: description
    end type itemInfo

    type(itemInfo), dimension(:), allocatable :: item

    integer(kind = int_wp), private :: lunlst = 0, lunsub, lunpar

contains

    !> Set LU-numbers, specifically for test purposes
    subroutine testSetLunumbers(testsub, testpar)
        integer(kind = int_wp), intent(in) :: testsub, testpar

        lunsub = testsub
        lunpar = testpar
    end subroutine testSetLunumbers

    !> Open a file for reporting what substances were read
    subroutine openSubstancesReport

        if (lunlst == 0) then
            open (newunit = lunlst, file = 'waq_omi.report')
        end if

    end subroutine openSubstancesReport

    !> Read the substances and the parameters file and store the information
    subroutine loadSubstancesFile(substances_file, parameters_file, success)

        character(len = *), intent(in) :: substances_file
        character(len = *), intent(in) :: parameters_file
        logical, intent(out) :: success

        integer(kind = int_wp) :: ierr

        success = .true.

        call openSubstancesReport

        open (newunit = lunsub, file = substances_file, status = 'old', iostat = ierr)

        if (ierr /= 0) then
            write (lunlst, '(2a)') 'Error opening substances file: ', trim(substances_file)
            return
        end if

        open (newunit = lunpar, file = parameters_file, status = 'old', iostat = ierr)

        if (ierr /= 0) then
            write (lunlst, '(2a)') 'Error opening parameters file: ', trim(parameters_file)
            return
        end if

        if (allocated(item)) then
            deallocate (item)
        end if

        write (lunlst, '(2a)') 'Reading information on substances and processes:'
        write (lunlst, '(2a)') '    Parameters file: ', trim(parameters_file)
        write (lunlst, '(2a)') '    Substances file: ', trim(substances_file)

        call readParametersFile(success)
        call readSubstancesFile(success)

        if (success) then
            call registerSubstances(success)
        end if

        close (lunsub)
        close (lunpar)
        close (lunlst)

    end subroutine LoadSubstancesFile

    !> Read the parameters file - store the information
    subroutine readParametersFile(success)

        logical, intent(inout) :: success !< Whether this step was successful or not

        integer(kind = int_wp) :: i
        integer(kind = int_wp) :: k
        integer(kind = int_wp) :: lineno
        integer(kind = int_wp) :: ierr
        character(len = 80) :: string
        character(len = 80) :: prev_string
        character(len = 20) :: type
        character(len = 20) :: name
        real(kind = real_wp) :: value
        type(itemInfo), dimension(:), pointer :: newItems
        type(itemInfo), dimension(:), pointer :: prevItems

        allocate (newItems(10))

        i = 0
        lineno = 0
        string = '(Start of file)'

        readLoop : &
                do
            prev_string = string
            read (lunpar, '(a)', iostat = ierr) string
            lineno = lineno + 1

            if (ierr < 0) then
                exit
            end if

            if (ierr > 0) then
                success = .false.
                write (lunlst, '(a,i0)') 'Error reading parameters file at line ', lineno
                write (lunlst, '(a,a)') '    Last line read: ', trim(prev_string)
                exit readLoop
            end if

            k = index(string, '#')
            if (k > 0) then
                string = string(1:k - 1)
            end if

            if (string == '  ') then
                cycle
            end if

            read (string, *, iostat = ierr) type, name, value
            if (ierr /= 0) then
                write (lunlst, '(a,i0)') 'Error reading parameters file at line ', lineno
                write (lunlst, '(a,a)') '    Line read: ', trim(string)
                write (lunlst, '(a,a)') '    (Should have three items: type, name and value)'
                success = .false.
                exit readLoop
            end if

            i = i + 1

            if (i > size(newItems)) then
                prevItems => newItems
                allocate (newItems(2 * size(prevItems)))
                newItems(1:size(prevItems)) = prevItems
                deallocate (prevItems)
            end if

            newItems(i)%name = name
            newItems(i)%unit = '?'
            newItems(i)%value = value
            newItems(i)%private = .true.
            newItems(i)%description = 'Private item - ' // name

            select case (type)
            case ('constant')
                newItems(i)%type = type_constant
            case ('parameter')
                newItems(i)%type = type_parameter
            case ('process')
                newItems(i)%type = type_process
            case default
                write (lunlst, '(a,i0)') 'Unknown keyword in parameters file at line ', lineno
                write (lunlst, '(a,a)') '    Line: ', trim(string)
                success = .false.
                exit readLoop
            end select
        end do &
                readLoop

        if (success) then
            if (allocated(item)) then
                deallocate (item)
            end if

            allocate (item(1:i))

            item = newItems(1:i)

            deallocate (newItems)
        end if
    end subroutine readParametersFile

    !> Read the substances file - store the information
    subroutine readSubstancesFile(success)

        logical, intent(inout) :: success !< Whether this step was successful or not

        integer(kind = int_wp) :: i
        integer(kind = int_wp) :: idx
        integer(kind = int_wp) :: k
        integer(kind = int_wp) :: lineno
        integer(kind = int_wp) :: ierr
        integer(kind = int_wp) :: mode
        integer(kind = int_wp) :: itemType
        integer(kind = int_wp) :: maxidx
        logical :: createItem
        logical :: isNew
        character(len = 80) :: string
        character(len = 80) :: prev_string
        character(len = 20) :: type
        character(len = 20) :: name
        character(len = 20) :: activeKeyword
        character(len = 80) :: description
        type(itemInfo), dimension(:), pointer :: newItems
        type(itemInfo), dimension(:), pointer :: prevItems

        integer(kind = int_wp), parameter :: mode_general = 0
        integer(kind = int_wp), parameter :: mode_substance = 1
        integer(kind = int_wp), parameter :: mode_parameter = 2
        integer(kind = int_wp), parameter :: mode_output = 3
        integer(kind = int_wp), parameter :: mode_process = 4

        if (.not. allocated(item)) then
            allocate (item(1))
            item(1)%name = '?'
        end if

        allocate (newItems(size(item)))
        newItems = item

        i = 0
        lineno = 0
        string = '(Start of file)'
        mode = 0

        readLoop : &
                do
            prev_string = string
            read (lunsub, '(a)', iostat = ierr) string
            lineno = lineno + 1

            if (ierr < 0) then
                exit
            end if

            if (ierr > 0) then
                success = .false.
                write (lunlst, '(a,i0)') 'Error reading substances file at line ', lineno
                write (lunlst, '(a,a)') '    Last line read: ', trim(prev_string)
                exit readLoop
            end if

            k = index(string, '#')
            if (k > 0) then
                string = string(1:k - 1)
            end if

            if (string == '  ') then
                cycle
            end if

            if (mode == mode_general) then
                read (string, *, iostat = ierr) type
                if (ierr /= 0) then
                    write (lunlst, '(a,i0)') 'Error reading substances file at line ', lineno
                    write (lunlst, '(a,a)') '    Line read: ', trim(string)
                    success = .false.
                    exit readLoop
                end if

                !
                ! Handle the first line of each block
                !
                select case (type)
                case ('substance')
                    createItem = .true.
                    mode = mode_substance
                    itemType = type_substance

                    read (string, *, iostat = ierr) type, name, activeKeyword

                    if (ierr /= 0) then
                        write (lunlst, '(a,i0)') 'Error reading substances file at line ', lineno
                        write (lunlst, '(a,a)') '    Line read: ', trim(string)
                        write (lunlst, '(a,a)') '    The line should contain the keyword "substance", the name and "active" or "inactive"'
                        success = .false.
                        exit readLoop
                    end if

                    if (activeKeyword == 'active') then
                        itemType = type_substance
                    elseif (activeKeyword == 'inactive') then
                        itemType = type_inactive
                    else
                        write (lunlst, '(a,i0)') 'Error reading substances file at line ', lineno
                        write (lunlst, '(a,a)') '    Line read: ', trim(string)
                        write (lunlst, '(a,a)') '    Unknown keyword: ', activeKeyword
                        success = .false.
                        exit readLoop
                    end if

                case ('parameter', 'output')
                    createItem = .true.
                    if (type == 'parameter') then
                        mode = mode_parameter
                        itemType = type_parameter
                    else
                        mode = mode_output
                        itemType = type_output
                    end if

                    read (string, *, iostat = ierr) type, name

                    if (ierr /= 0) then
                        write (lunlst, '(a,i0)') 'Error reading substances file at line ', lineno
                        write (lunlst, '(a,a)') '    Line read: ', trim(string)
                        write (lunlst, '(a,a)') '    The line should contain the keyword "parameter" or "output" and the name'
                        success = .false.
                        exit readLoop
                    end if

                case ('active-processes')
                    createItem = .false.
                    mode = mode_process
                    itemType = type_process

                case default
                    write (lunlst, '(a,i0)') 'Error reading substances file at line ', lineno
                    write (lunlst, '(a,a)') '    Line read: ', trim(string)
                    write (lunlst, '(a,a)') '    Unknown keyword: ', type
                    success = .false.
                    exit readLoop
                end select

                if (createItem) then
                    call createOrFindItem(name, idx, isNew)

                    if (isNew) then
                        newItems(idx)%name = name
                        newItems(idx)%unit = '?'
                        newItems(idx)%value = 0.0
                        newItems(idx)%type = itemType
                        newItems(idx)%description = 'NO DESCRIPTION!'
                    end if
                    newItems(idx)%private = .false. ! Always public!
                end if

            elseif (mode /= mode_process) then

                read (string, *, iostat = ierr) type

                success = .true.
                select case (type)
                case ('description')
                    call readSecondString(string, newItems(idx)%description, success)

                case ('unit', 'concentration-unit')
                    call readSecondString(string, newItems(idx)%unit, success)

                case ('waste-load-unit')
                    ! Ignore - no useful information

                case ('value')
                    call readValue(string, newItems(idx)%value, success)

                case ('end-substance', 'end-parameter', 'end-output')
                    mode = mode_general

                case default
                    write (lunlst, '(a,i0)') 'Unknown keyword in substances file at line ', lineno
                    write (lunlst, '(a,a)') '    Line: ', trim(string)
                    success = .false.
                    exit readLoop
                end select

                if (.not. success) then
                    exit readLoop
                end if

            elseif (mode == mode_process) then

                read (string, *, iostat = ierr) type
                if (ierr /= 0) then
                    write (lunlst, '(a,i0)') 'Error reading substances file at line ', lineno
                    write (lunlst, '(a,a)') '    Line read: ', trim(string)
                    write (lunlst, '(a,a)') '    Cause unclear'
                    success = .false.
                    exit readLoop
                end if

                if (type == 'end-active-processes') then
                    mode = mode_general
                    cycle
                end if

                read (string, *, iostat = ierr) type, name, description

                if (ierr /= 0) then
                    write (lunlst, '(a,i0)') 'Error reading substances file at line ', lineno
                    write (lunlst, '(a,a)') '    Line read: ', trim(string)
                    write (lunlst, '(a,a)') '    The line should contain the keyword "name", the name of the process and the description'
                    success = .false.
                    exit readLoop
                end if

                call createOrFindItem('ACTIVE_' // name, idx, isNew)

                newItems(idx)%type = type_process
                newItems(idx)%name = 'ACTIVE_' // name
                newItems(idx)%unit = '?'
                newItems(idx)%value = 1.0
                newItems(idx)%description = description
                newItems(idx)%private = .false.
            end if

            maxidx = max(maxidx, idx)

        end do &
                readLoop

        if (success) then
            deallocate (item)
            allocate (item(1:maxidx))

            item = newItems(1:maxidx)

            deallocate (newItems)
        end if

    contains

        !> Create a new item if necessary (internal routine)
        subroutine createOrFindItem(name, idx, isNew)
            character(len = *), intent(in) :: name
            integer(kind = int_wp), intent(out) :: idx
            logical, intent(out) :: isNew

            integer(kind = int_wp) :: i
            integer(kind = int_wp) :: maxused

            idx = -1
            isNew = .true.
            maxused = size(newItems)

            do i = 1, size(newItems)
                if (newItems(i)%name == name) then
                    idx = i
                    isNew = .false.
                    exit
                end if
                if (newItems(i)%name /= '?') then
                    maxused = i
                end if
            end do

            if (idx == -1) then
                idx = maxused + 1
                if (idx > size(newItems)) then
                    prevItems => newItems
                    allocate (newItems(2 * size(prevItems)))
                    newItems(1:size(prevItems)) = prevItems

                    newItems(size(prevItems) + 1:)%name = '?'

                    deallocate (prevItems)
                end if

            end if

        end subroutine createOrFindItem

        !> Read the second string from the line
        subroutine readSecondString(string, secondString, success)
            character(len = *), intent(in) :: string
            character(len = *), intent(out) :: secondString
            logical, intent(out) :: success

            character(len = 1) :: dummy
            integer(kind = int_wp) :: ierr

            success = .true.
            read (string, *, iostat = ierr) dummy, secondString

            if (ierr /= 0) then
                write (lunlst, '(a,i0)') 'Error reading substances file at line ', lineno
                write (lunlst, '(a,a)') '    Line read: ', trim(string)
                write (lunlst, '(a,a)') '    The line should contain a keyword and a string value (possibly enclosed in quotes)'
                success = .false.
            end if

        end subroutine readSecondString

        !> Read the value as the second item from the line
        subroutine readValue(string, value, success)
            character(len = *), intent(in) :: string
            real(kind = real_wp), intent(out) :: value
            logical, intent(out) :: success

            character(len = 1) :: dummy
            integer(kind = int_wp) :: ierr

            success = .true.
            read (string, *, iostat = ierr) dummy, value

            if (ierr /= 0) then
                write (lunlst, '(a,i0)') 'Error reading substances file at line ', lineno
                write (lunlst, '(a,a)') '    Line read: ', trim(string)
                write (lunlst, '(a,a)') '    The line should contain a keyword and a real value'
                success = .false.
            end if

        end subroutine readValue

    end subroutine readSubstancesFile

    !> Register the substances, process parameters and processes with DELWAQ
    subroutine registerSubstances(success)

        use waq_omi_interface

        logical, intent(inout) :: success !< Whether this step was successful or not

        !
        ! The information in the array "item" gets distributed
        !
        integer(kind = int_wp) :: number_substances
        integer(kind = int_wp) :: number_transported
        integer(kind = int_wp) :: number_parameters
        integer(kind = int_wp) :: number_processes

        integer(kind = int_wp) :: i

        character(len = 20), dimension(:), allocatable :: substance
        character(len = 20), dimension(:), allocatable :: process_parameter
        character(len = 20), dimension(:), allocatable :: process
        real(kind = real_wp), dimension(:), allocatable :: value

        number_substances = count(item%type == type_substance) + count(item%type == type_inactive)
        number_transported = count(item%type == type_substance)
        number_parameters = count(item%type == type_constant) + count(item%type == type_parameter)
        number_processes = count(item%type == type_process)

        allocate (substance(1:number_substances))
        allocate (process_parameter(1:number_parameters))
        allocate (value(1:number_parameters))
        allocate (process(1:number_processes))

        substance = (/pack(item%name, item%type == type_substance), pack(item%name, item%type == type_inactive)/)
        process_parameter = (/pack(item%name, item%type == type_constant), pack(item%name, item%type == type_parameter)/)
        value = (/pack(item%value, item%type == type_constant), pack(item%value, item%type == type_parameter)/)
        process = pack(item%name(8:), item%type == type_process)

        success = DefineWQProcesses(substance, number_substances, number_transported, &
                process_parameter, number_parameters, &
                process, number_processes)

        do i = 1, number_parameters
            success = SetCurrentValueScalarRun(process_parameter(i), value(i))
        end do

    end subroutine registerSubstances

    !> Subroutine for testing - write out the contents
    subroutine writeItems(file_unit_list)

        integer(kind = int_wp), intent(in) :: file_unit_list

        integer(kind = int_wp) :: i

        do i = 1, size(item)
            write (file_unit_list, '(3a,i5,l5,e15.6)') &
                    item(i)%name, item(i)%description, item(i)%unit, item(i)%type, item(i)%private, item(i)%value
        end do

    end subroutine writeItems

end module waq_omi_substances
