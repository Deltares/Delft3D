    !!  Copyright (C)  Stichting Deltares, 2012-2023.
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
    !!    Run the computation step by step
    !!

    !> Main program steering the computation
    program delwaq_test_dimr
    use iso_c_binding

    implicit none
    interface
    integer(c_int) function set_var(c_key, c_value) bind(C, name="set_var")
    use iso_c_binding
    character,intent(in)    :: c_key
    character,intent(in)    :: c_value
    end function  set_var

    integer(c_int) function initialize(c_config_file) bind(C, name="initialize")
    use iso_c_binding
    character,intent(in)    :: c_config_file
    end function  initialize

    subroutine get_version_string(c_version_string) bind(C, name="get_version_string")
    character, intent(out) :: c_version_string
    end subroutine get_version_string

    integer function update(dt) bind(C, name="update")
    use iso_c_binding, only: c_double
    real(c_double), value, intent(in) :: dt
    end function update

    integer function finalize() bind(C, name="finalize")
    end function finalize

    subroutine get_start_time(t) bind(C, name="get_start_time")
    use iso_c_binding, only: c_double
    real(c_double), intent(out) :: t
    end subroutine get_start_time

    subroutine get_end_time(t) bind(C, name="get_end_time")
    use iso_c_binding, only: c_double
    real(c_double), intent(out) :: t
    end subroutine get_end_time

    subroutine get_time_step(dt) bind(C, name="get_time_step")
    use iso_c_binding, only: c_double
    end subroutine get_time_step

    subroutine get_current_time(t) bind(C, name="get_current_time")
    use iso_c_binding, only: c_double
    real(c_double), intent(out) :: t
    end subroutine get_current_time
    end interface

    integer :: dummy

    character(len=1023)     :: version_string
    character(len=1023)     :: key
    character(len=1023)     :: value
    character(len=1023)     :: runid
    character(len=1023)     :: resfile
    integer                :: itimestamp
    real(kind=kind(1.0d0)) :: startTime, stopTime, currentTime
    integer                :: i ,status, found, lunlog
    logical                :: log = .false.

    if (log) open(newunit=lunlog, file='dimr_test.log',status='replace')

    version_string = ' '
    call get_version_string(version_string)
    if (log) write(lunlog,'(A)') 'dll version string:'
    if (log) write(lunlog,'(A200)') trim(version_string)

    key = '-waq'
    value = ' '
    dummy = set_var( key, value)
    key = '-p'
    !   value = 'c:\Program Files\Deltares\Delft3D 4.02.00.01\win64\waq\default\proc_def.dat'
    value = '..\..\bin\win64\waq\default\proc_def.dat'
    dummy = set_var( key, value)

    runid = ' '
    call get_command_argument(1,runid,status)
    if ( runid == ' ' ) then
        if (log) write(lunlog,'(A)') 'Please specify the run-ID!'
        stop
    endif
    if (log) write(lunlog,'(A)') 'run id:'
    if (log) write(lunlog,'(A200)') trim(runid)

    dummy = initialize(runid)

    call get_start_time(startTime)
    if (log) write(lunlog,'(A)') 'run id:'
    if (log) write(lunlog,'(A)') runid

    call get_end_time(stopTime)
    if (log) write(lunlog,'(A)') 'run id:'
    if (log) write(lunlog,'(A)') runid

    call get_current_time(currentTime)
    if (log) write(lunlog,'(A)') 'currentTime:'
    if (log) write(lunlog,'(E17.6)') currentTime

    dummy = update(stopTime-startTime)
    call get_current_time(currentTime)
    if (log) write(lunlog,'(A)') 'currentTime:'
    if (log) write(lunlog,'(E17.6)') currentTime

    dummy = finalize()

    stop(0)
    end program delwaq_test_dimr
