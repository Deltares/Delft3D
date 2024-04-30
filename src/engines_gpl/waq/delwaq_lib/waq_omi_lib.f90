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

module waq_omi_utils
    !! Interface routines for Delta-Shell
    !!
    !! Note:
    !! Not all routines can be in a module, they have to
    !! be callable from outside Fortran.

    use m_waq_precision
    use monitoring_areas, only : create_write_monitoring_area_array
    use integration_options, only : check_integration_option
    use inputs_block_9
    use m_delwaq2_main
    use m_dlwqp1
    use m_open_waq_files

    implicit None

    integer(kind = int_wp), parameter :: LEVEL_FATAL = 1
    integer(kind = int_wp), parameter :: LEVEL_ERROR = 2
    integer(kind = int_wp), parameter :: LEVEL_WARNING = 3
    integer(kind = int_wp), parameter :: LEVEL_INFO = 4
    logical, save :: reporting = .false.
    integer(kind = int_wp), save :: lunlst

    integer(kind = int_wp), save :: msg_level = LEVEL_INFO
    character(len = 200), save :: msg_text = 'No message'


contains

    !> Find a name in a list of names
    subroutine find_index(name, list_names, idx)
        character(len = *) :: name           !< Name to be found
        character(len = *), dimension(:) :: list_names     !< List of names to be searched
        integer(kind = int_wp) :: idx             !< Index (-1 if name unknown)

        integer(kind = int_wp) :: i

        idx = -1
        do i = 1, size(list_names)
            if (name == list_names(i)) then
                idx = i
                exit
            end if
        end do

    end subroutine find_index

    !> Set a subparameter for the integration option (via a DELWAQ core routine)
    !! Used here only
    subroutine set_intopt(option, keyword_true, keyword_false)

        use m_sysi          ! Timer characteristics

        logical :: option                                  !< Selected value of the option
        character(len = *) :: keyword_true                   !< Keyword describing "true" value for the option
        character(len = *) :: keyword_false                  !< Keyword describing "false" value for the option

        integer(kind = int_wp) :: lunut, ierr2

        lunut = 10
        if (option) then
            call check_integration_option(keyword_true, intopt, lunut, ierr2)
        else
            call check_integration_option(keyword_false, intopt, lunut, ierr2)
        end if
    end subroutine set_intopt

    !> Store an error message
    !! Used here only
    subroutine SetMessage(level, text)
        integer(kind = int_wp) :: level
        character(*) :: text

        msg_level = level
        msg_text = text

    end subroutine SetMessage

    !> Write a one-dimensional, constant array to a work file
    subroutine write_array_const(name, suffix, value, size)
        character(len = *) :: name                         !< Name of the work files
        character(len = *) :: suffix                       !< Suffix for this particular file
        real(kind = real_wp) :: value                         !< Constant value to be written
        integer(kind = int_wp) :: size                          !< Number of times the value must be repeated

        integer(kind = int_wp) :: i, lunwrk
        integer(kind = int_wp) :: time_dummy

        time_dummy = 0

        open (newunit = lunwrk, file = trim(name) // '-' // trim(suffix) // '.wrk', form = 'unformatted', access = 'stream')
        write (lunwrk) time_dummy, (value, i = 1, size)
        close (lunwrk)

    end subroutine write_array_const

end module waq_omi_utils


module waq_omi_interface
    use m_waq_precision

    implicit none

contains

    !! Return the last known message
    logical function GetLastMessage(level, text)

        !DEC$ ATTRIBUTES DLLEXPORT::GetLastMessage
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETLASTMESSAGE' :: GetLastMessage

        use waq_omi_utils

        integer(kind = int_wp), intent(out) :: level
        character(*), intent(out) :: text

        GetLastMessage = .true.
        level = msg_level
        text = msg_text

    end function GetLastMessage

    logical function GetWQDimensions(notot, noseg)

        !DEC$ ATTRIBUTES DLLEXPORT::GetWQDimensions
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETWQDIMENSIONS' :: GetWQDimensions

        use delwaq2_global_data
        use m_sysi          ! Timer characteristics

        integer(kind = int_wp), intent(out) :: notot          !< Number of substances
        integer(kind = int_wp), intent(out) :: noseg          !< Number of segments

        notot = size_dlwq_state%notot
        noseg = size_dlwq_state%noseg

        GetWQDimensions = .true.

    end function GetWQDimensions

    !> Set the times for the simulation
    logical function SetSimulationTimes(startTime, endTime, timeStep)

        !DEC$ ATTRIBUTES DLLEXPORT::SetSimulationTimes
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETSIMULATIONTIMES' :: SetSimulationTimes

        use delwaq2_global_data
        use m_sysi          ! Timer characteristics

        integer(kind = int_wp), intent(in) :: startTime        !< Start time in seconds since the reference date/time
        integer(kind = int_wp), intent(in) :: endTime          !< Stop time in seconds since the reference date/time
        integer(kind = int_wp), intent(in) :: timeStep         !< Time step in seconds

        itstrt = startTime
        itstop = endTime
        idt = timeStep

        isflag = 1

        SetSimulationTimes = .true.

    end function SetSimulationTimes

    !> Retrieve the times for the simulation
    logical function GetSimulationTimes(startTime, endTime, timeStep)

        !DEC$ ATTRIBUTES DLLEXPORT::GetSimulationTimes
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETSIMULATIONTIMES' :: GetSimulationTimes

        use delwaq2_global_data
        use m_sysi          ! Timer characteristics

        integer(kind = int_wp), intent(out) :: startTime        !< Start time in seconds since the reference date/time
        integer(kind = int_wp), intent(out) :: endTime          !< Stop time in seconds since the reference date/time
        integer(kind = int_wp), intent(out) :: timeStep         !< Time step in seconds

        startTime = itstrt
        endTime = itstop
        timeStep = idt

        GetSimulationTimes = .true.

    end function GetSimulationTimes

    !> Set time format in monitoring file
    logical function SetTimeFormat(timeFormat)

        use m_sysi          ! Timer characteristics

        !DEC$ ATTRIBUTES DLLEXPORT::SetTimeFormat
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETTIMEFORMAT' :: SetTimeFormat

        integer(kind = int_wp), intent(in) :: timeFormat  !< Time format
        ! (0 = integer, 1 = dd:hh:mm:ss, 2 = yy:ddd:hh:mm:ss)

        isflag = timeFormat

        SetTimeFormat = .true.

    end function SetTimeFormat

    !> Set the reference date (the so-called T0-string)
    logical function SetReferenceDate(year_in, month_in, day_in, hour_in, minute_in, second_in)

        !DEC$ ATTRIBUTES DLLEXPORT::SetReferenceDate
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETREFERENCEDATE' :: SetReferenceDate

        use delwaq2_global_data

        integer(kind = int_wp), intent(in) :: year_in
        integer(kind = int_wp), intent(in) :: month_in
        integer(kind = int_wp), intent(in) :: day_in
        integer(kind = int_wp), intent(in) :: hour_in
        integer(kind = int_wp), intent(in) :: minute_in
        integer(kind = int_wp), intent(in) :: second_in

        ref_year = year_in
        ref_month = month_in
        ref_day = day_in
        ref_hour = hour_in
        ref_minute = minute_in
        ref_second = second_in

        SetReferenceDate = .true.

    end function SetReferenceDate

    ! SetOutputTimers --
    !     Set the timers for the output:
    !     type: defines what type of output (1 = monitor, 2 = history, 3 = map)
    logical function SetOutputTimers(type, startTime, endTime, timeStep)

        !DEC$ ATTRIBUTES DLLEXPORT::SetOutputTimers
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETOUTPUTTIMERS' :: SetOutputTimers

        use delwaq2_global_data
        use m_sysn
        use m_sysi

        integer(kind = int_wp), intent(in) :: type
        integer(kind = int_wp), intent(in) :: startTime
        integer(kind = int_wp), intent(in) :: endTime
        integer(kind = int_wp), intent(in) :: timeStep

        select case (type)
        case (1)    !monitor
            imstrt = startTime
            imstop = endTime
            imstep = timeStep
        case (2)    !history
            ihstrt = startTime
            ihstop = endTime
            ihstep = timeStep
        case (3)    !map
            idstrt = startTime
            idstop = endTime
            idstep = timeStep
        case default
        end select

        noutp = 9
        SetOutputTimers = .true.

    end function SetOutputTimers

    ! SetAttributeInit --
    !     Set an attribute
    !
    !     Note: use before ModelInitialize!
    logical function SetAttributeInit(idx, ivalue)

        !DEC$ ATTRIBUTES DLLEXPORT::SetAttributeInit
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETATTRIBUTEINIT' :: SetAttributeInit

        use waq_omi_utils
        use delwaq2_global_data
        use m_sysn

        integer(kind = int_wp), intent(in) :: idx
        integer(kind = int_wp), dimension(*), intent(in) :: ivalue

        integer(kind = int_wp) :: iseg  !< segment number
        integer(kind = int_wp) :: ilow  !< divisor of this attribute
        integer(kind = int_wp) :: iup   !< divisor of attributes with higher index
        integer(kind = int_wp) :: i1    !< value of attributes with higher index
        integer(kind = int_wp) :: i2    !< previous value of this attribute
        integer(kind = int_wp) :: i3    !< value of attributes wih lower index

        SetAttributeInit = .false.

        if (idx <= 0 .or. idx > 10) then
            return
        end if

        ilow = 10**(idx - 1)
        iup = 10**idx

        do iseg = 1, noseg ! + nseg2 (segments in bed)
            i1 = (iknmrk(iseg) / iup) * iup
            i2 = ((iknmrk(iseg) - i1) / ilow) * ilow
            i3 = iknmrk(iseg) - i1 - i2
            iknmrk(iseg) = i1 + ivalue(iseg) * ilow + i3
        end do

        if (reporting) then
            write (lunlst, '(a25,i5)') 'Values set for attribute ', idx
        end if

        SetAttributeInit = .true.

    end function SetAttributeInit

    logical function SetCurrentValueScalarInit(name, value)
        ! SetCurrentValueScalarInit --
        !     Set the current value of a substance or process parameter
        !
        !     Note: use before ModelInitialize!
        !

        !DEC$ ATTRIBUTES DLLEXPORT::SetCurrentValueScalarInit
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETCURRENTVALUESCALARINIT' :: SetCurrentValueScalarInit

        use waq_omi_utils
        use delwaq2_global_data

        character(len = *), intent(in) :: name
        real(kind = real_wp), intent(in) :: value

        integer(kind = int_wp) :: idx

        SetCurrentValueScalarInit = .false.

        call find_index(name, substance_name, idx)
        if (idx > 0) then
            substance_conc(idx, :) = value
        else
            call find_index(name, procparam_const, idx)
            if (idx > 0) then
                procparam_const_value(idx) = value
            else
                call find_index(name, procparam_param, idx)
                if (idx > 0) then
                    procparam_param_value(idx, :) = value
                else
                    call SetMessage(LEVEL_ERROR, &
                            'Name not found (not a substance or process parameter): ' // name)
                    return
                end if
            end if
        end if

        if (reporting) then
            write (lunlst, '(3a,g14.5,a)') 'Initial value for substance ', trim(name), ' set to: ', value
        end if

        SetCurrentValueScalarInit = .true.

    end function SetCurrentValueScalarInit

    logical function SetCurrentValueFieldInit(name, value)
        ! SetCurrentValueFieldInit --
        !     Set the current value of a process parameter (which varies per segment)
        !
        !     Note: use before ModelInitialize!
        !

        !DEC$ ATTRIBUTES DLLEXPORT::SetCurrentValueFieldInit
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETCURRENTVALUEFIELDINIT' :: SetCurrentValueFieldInit

        use waq_omi_utils
        use delwaq2_global_data
        use m_sysn          ! System characteristics
        use m_sysa          ! Pointers in real array workspace

        character(len = *), intent(in) :: name
        real(kind = real_wp), dimension(*), intent(in) :: value

        integer(kind = int_wp) :: idx

        SetCurrentValueFieldInit = .false.

        call find_index(name, substance_name, idx)
        if (idx > 0) then
            substance_conc(idx, 1:noseg) = value(1:noseg)
        else
            call find_index(name, procparam_param, idx)
            if (idx > 0) then
                procparam_param_value(idx, 1:noseg) = value(1:noseg)
            else
                call SetMessage(LEVEL_ERROR, &
                        'Name not found (not a substance or process parameter): ' // name)
                return
            end if
        end if

        if (reporting) then
            write (lunlst, '(3a,g14.5,a)') 'Initial value for parameter ', trim(name), ' set (varying values)'
        end if

        SetCurrentValueFieldInit = .true.

    end function SetCurrentValueFieldInit

    ! SetCurrentValueScalarRun --
    !     Set the current value of a process parameter
    !
    !     Note: use after ModelInitialize!
    logical function SetCurrentValueScalarRun(name, value)

        !DEC$ ATTRIBUTES DLLEXPORT::SetCurrentValueScalarRun
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETCURRENTVALUESCALARRUN' :: SetCurrentValueScalarRun

        use waq_omi_utils
        use delwaq2_global_data
        use m_sysn          ! System characteristics
        use m_sysa          ! Pointers in real array workspace

        character(len = *), intent(in) :: name
        real(kind = real_wp), intent(in) :: value

        integer(kind = int_wp) :: idx

        SetCurrentValueScalarRun = .false.

        call find_index(name, procparam_const, idx)
        if (idx > 0) then
            dlwqd%buffer%rbuf(icons + idx - 1) = value
        else
            call find_index(name, procparam_param, idx)
            if (idx > 0) then
                dlwqd%buffer%rbuf(iparm + idx - 1:iparm + idx - 1 + nopa * noseg - 1:nopa) = value
            else
                call SetMessage(LEVEL_ERROR, &
                        'Name not found (not a process parameter): ' // name)
                return
            end if
        end if

        if (reporting) then
            write (lunlst, '(3a,g14.5,a,i0)') 'Parameter ', trim(name), ' set to: ', value, ' at time = ', dlwqd%itime
        end if

        SetCurrentValueScalarRun = .true.

    end function SetCurrentValueScalarRun

    ! SetCurrentValueFieldRun --
    !     Set the current value of a process parameter (which varies per segment)
    !
    !     Note: use after ModelInitialize!
    logical function SetCurrentValueFieldRun(name, value)

        !DEC$ ATTRIBUTES DLLEXPORT::SetCurrentValueFieldRun
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETCURRENTVALUEFIELDRUN' :: SetCurrentValueFieldRun

        use waq_omi_utils
        use delwaq2_global_data
        use m_sysn          ! System characteristics
        use m_sysa          ! Pointers in real array workspace

        character(len = *), intent(in) :: name
        real(kind = real_wp), dimension(*), intent(in) :: value

        integer(kind = int_wp) :: idx

        SetCurrentValueFieldRun = .false.

        call find_index(name, procparam_param, idx)
        if (idx > 0) then
            dlwqd%buffer%rbuf(iparm + idx - 1:iparm + idx - 1 + nopa * noseg - 1:nopa) = value(1:noseg)
        else
            call SetMessage(LEVEL_ERROR, &
                    'Name not found (not a process parameter): ' // name)
            return
        end if

        if (reporting) then
            write (lunlst, '(3a,g14.5,a,i0)') 'Parameter ', trim(name), &
                    '(values vary over the model area) set at time = ', dlwqd%itime
        end if

        SetCurrentValueFieldRun = .true.

    end function SetCurrentValueFieldRun

    ! GetCurrentValue --
    !     Get the current value of a substance or process parameter
    !     for ALL segments. The array value is assumed to be large enough
    logical function GetCurrentValue(name, value)

        !DEC$ ATTRIBUTES DLLEXPORT::GetCurrentValue
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'GETCURRENTVALUE' :: GetCurrentValue

        use waq_omi_utils
        use delwaq2_global_data
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace

        character(len = *), intent(in) :: name
        real(kind = real_wp), dimension(*), intent(out) :: value

        integer(kind = int_wp) :: idx
        integer(kind = int_wp) :: i

        GetCurrentValue = .false.

        call find_index(name, substance_name, idx)
        if (idx > 0) then
            do i = 1, noseg
                value(i) = dlwqd%buffer%rbuf(iconc + idx - 1 + (i - 1) * notot)
            end do
        else
            call find_index(name, procparam_const, idx)
            if (idx > 0) then
                do i = 1, noseg
                    value(i) = dlwqd%buffer%rbuf(iconc + idx - 1 + (i - 1) * notot)
                end do
            else
                call SetMessage(LEVEL_ERROR, &
                        'Name not found (not a substance of process parameter): ' // name)
                return
            end if
        end if

        GetCurrentValue = .true.

    end function GetCurrentValue

    ! SetIntegrationOptions --
    !     Set the integration option (and all its subparameters)
    logical function SetIntegrationOptions(method, disp_flow_zero, disp_bound, first_order, forester, anticreep)
        !DEC$ ATTRIBUTES DLLEXPORT::SetIntegrationOptions
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETINTEGRATIONOPTIONS' :: SetIntegrationOptions

        use waq_omi_utils
        use delwaq2_global_data
        use m_sysi

        integer(kind = int_wp), intent(in) :: method
        logical, intent(in) :: disp_flow_zero
        logical, intent(in) :: disp_bound
        logical, intent(in) :: first_order
        logical, intent(in) :: forester
        logical, intent(in) :: anticreep

        intsrt = method
        intopt = 0

        call set_intopt(disp_flow_zero, 'DISP-AT-NOFLOW', 'NODISP-AT-NOFLOW')
        call set_intopt(disp_bound, 'DISP-AT-BOUND', 'NODISP-AT-BOUND')
        call set_intopt(first_order, 'LOWER-ORDER-AT-BOUND', 'HIGHER-ORDER-AT-BOUND')
        call set_intopt(forester, 'FORESTER', 'NO-FORESTER')
        call set_intopt(anticreep, 'ANTICREEP', 'NO-ANTICREEP')

        SetIntegrationOptions = .true.

    end function SetIntegrationOptions

    ! TODO: local theta!
    ! TODO: anti-diffusion, scheme 15 unstructured

    ! SetBalanceOutputOptions --
    !     Set the output options for balances
    logical function SetBalanceOutputOptions(type, lump_processes, lump_loads, lump_transport, suppress_space, &
            suppress_time, unit_type)
        !DEC$ ATTRIBUTES DLLEXPORT::SetBalanceOutputOptions
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETBALANCEOUTPUTOPTIONS' :: SetBalanceOutputOptions

        use waq_omi_utils
        use delwaq2_global_data
        use m_sysi

        integer(kind = int_wp), intent(in) :: type
        logical, intent(in) :: lump_processes
        logical, intent(in) :: lump_loads
        logical, intent(in) :: lump_transport
        logical, intent(in) :: suppress_space
        logical, intent(in) :: suppress_time
        integer(kind = int_wp), intent(in) :: unit_type

        select case (type)
        case (1, 2)
            intopt = intopt + 8 + 16
        case (3)
            intopt = intopt + 8 + 32
        case default
            ! Ignore - defaults to no balance
        end select

        call set_intopt(lump_processes, 'BAL_LUMPPROCESSES', 'BAL_NOLUMPPROCESSES')
        call set_intopt(lump_loads, 'BAL_LUMPLOADS', 'BAL_NOLUMPLOADS')
        call set_intopt(lump_transport, 'BAL_LUMPTRANSPORT', 'BAL_NOLUMPTRANSPORT')
        call set_intopt(suppress_space, 'BAL_SUPPRESSSPACE', 'BAL_NOSUPPRESSSPACE')
        call set_intopt(suppress_time, 'BAL_SUPPRESSTIME', 'BAL_NOSUPPRESSTIME')
        if (unit_type == 0) then
            ! This is the default - there is no keyword for it
        end if
        if (unit_type == 1) then
            call set_intopt(.true., 'BAL_UNITAREA', 'xxxxxxxxxxxxxxxxxx')
        end if
        if (unit_type == 2) then
            call set_intopt(.true., 'BAL_UNITVOLUME', 'xxxxxxxxxxxxxxxxxx')
        end if

        SetBalanceOutputOptions = .true.

    end function SetBalanceOutputOptions

    ! DefineWQSchematisation --
    !     Define the number of segments and the pointer table
    logical function DefineWQSchematisation(number_segments, pointer_table, number_exchanges)
        !DEC$ ATTRIBUTES DLLEXPORT::DefineWQSchematisation
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'DEFINEWQSCHEMATISATION' :: DefineWQSchematisation

        use delwaq2_global_data
        use m_sysn
        use matrix_utils, only : compute_matrix_size

        integer(kind = int_wp), intent(in) :: number_segments
        integer(kind = int_wp), dimension(4) :: number_exchanges
        integer, dimension(4, 1:sum(number_exchanges)), intent(in) :: pointer_table

        integer(kind = int_wp) :: number_layers
        integer(kind = int_wp) :: number_segments_per_layer
        integer(kind = int_wp) :: i, j
        integer(kind = int_wp) :: lunwrk

        number_layers = 1
        if (number_exchanges(3) > 0) then
            number_segments_per_layer = number_segments - number_exchanges(3)
            if (number_segments_per_layer > 0) then
                number_layers = number_segments / number_segments_per_layer
                if (number_layers * number_segments_per_layer /= number_segments) then
                    number_layers = 1
                end if
            end if
        end if

        open (newunit = lunwrk, file = trim(runid) // '-to_from.wrk', form = 'unformatted', access = 'stream')
        write (lunwrk) pointer_table(:, 1:sum(number_exchanges))
        close (lunwrk)

        noseg = number_segments
        noq = sum(number_exchanges)
        noq1 = number_exchanges(1)
        noq2 = number_exchanges(2)
        noq3 = number_exchanges(3)
        noq4 = number_exchanges(4)
        nolay = number_layers
        if (noq > 0) then
            nobnd = -minval(pointer_table(:, 1:noq))
        else
            nobnd = 0
        end if
        nobtyp = nobnd

        !
        ! determine nomat (actually only needed if intsrt in [15:18, 21, 22] but
        ! the numerical solver may not have been set and at a later time we don't
        ! have access to the pointer_table anymore)
        !
        call compute_matrix_size(noq1, noq2, noq3, noseg, pointer_table, nomat)

        if (allocated(ipoint)) deallocate (ipoint)
        if (allocated(iknmrk)) deallocate (iknmrk)
        if (allocated(boundary_id)) deallocate (boundary_id)
        if (allocated(boundary_name)) deallocate (boundary_name)
        if (allocated(boundary_type)) deallocate (boundary_type)
        if (allocated(ibpnt_array)) deallocate (ibpnt_array)

        allocate (ipoint(4, noq))
        ipoint = pointer_table(1:4, 1:noq)

        allocate (iknmrk(1:noseg)) ! actually noseg+nseg2 (segments in the bed)
        iknmrk = 0

        allocate (boundary_id(1:nobnd))
        allocate (boundary_name(1:nobnd))
        allocate (boundary_type(1:nobnd))
        allocate (ibpnt_array(4, nobnd))
        boundary_id = 'Dummy id'
        boundary_name = 'Dummy name'
        boundary_type = 'Dummy type'

        ibpnt_array = 0
        ibpnt_array(1, :) = 0                     ! Time lags

        do i = 1, noq
            if (pointer_table(1, i) < 0 .and. pointer_table(2, i) > 0) then
                j = -pointer_table(1, i)
                ibpnt_array(2, j) = i
                ibpnt_array(3, j) = pointer_table(2, i)
            end if
            if (pointer_table(1, i) > 0 .and. pointer_table(2, i) < 0) then
                j = -pointer_table(2, i)
                ibpnt_array(2, j) = -i
                ibpnt_array(3, j) = pointer_table(1, i)
            end if
        end do

        DefineWQSchematisation = .true.

    end function DefineWQSchematisation

    ! DefineWQDispersion --
    !     Define the dispersion coefficients and dispersion lengths
    !
    !     Note:
    !     Use after DefineWQSchematisation
    logical function DefineWQDispersion(dispc, length)
        !DEC$ ATTRIBUTES DLLEXPORT::DefineWQDispersion
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'DEFINEWQDISPERSION' :: DefineWQDispersion

        use delwaq2_global_data
        use m_sysn

        real(kind = real_wp), dimension(3), intent(in) :: dispc
        real(kind = real_wp), dimension(2, noq) :: length

        integer(kind = int_wp) :: time_dummy
        integer(kind = int_wp) :: lunwrk

        time_dummy = 0

        open (newunit = lunwrk, file = trim(runid) // '-lengthes.wrk', form = 'unformatted', access = 'stream')
        write (lunwrk) time_dummy, length
        close (lunwrk)

        disp = dispc

        DefineWQDispersion = .true.

    end function DefineWQDispersion

    ! SetWQProcessDefinition --
    !     Set the mode and the process definition file
    logical function SetProcessDefinition(mode, procdef_file)
        !DEC$ ATTRIBUTES DLLEXPORT::SetProcessDefinition
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETPROCESSDEFINITION' :: SetProcessDefinition

        character(len = *) :: mode
        character(len = *) :: procdef_file

        SetProcessDefinition = SetProcessDefinitionCore(mode, procdef_file, ' ')

    end function SetProcessDefinition

    ! SetWQProcessDefinitionX --
    !     Set the mode, the process definition file and sfrac options file
    logical function SetProcessDefinitionX(mode, procdef_file, sfrac_file)
        !DEC$ ATTRIBUTES DLLEXPORT::SetProcessDefinitionX
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETPROCESSDEFINITIONX' :: SetProcessDefinitionX

        character(len = *) :: mode
        character(len = *) :: procdef_file
        character(len = *) :: sfrac_file

        SetProcessDefinitionX = SetProcessDefinitionCore(mode, procdef_file, sfrac_file)

    end function SetProcessDefinitionX

    ! SetWQProcessDefinitionCore --
    !     Set the mode, the process definition file and sfrac options file
    logical function SetProcessDefinitionCore(mode, procdef_file, sfrac_file)

        use delwaq2_global_data

        character(len = *) :: mode
        character(len = *) :: procdef_file
        character(len = *) :: sfrac_file

        if (allocated(argv)) then
            deallocate (argv)
        end if

        if (sfrac_file == ' ') then
            allocate (argv(5))
        else
            allocate (argv(7))
            argv(6) = '-sfrac'
            argv(7) = sfrac_file
        end if

        argv(1) = 'dlwqlib.dll'
        argv(2) = runid
        argv(3) = mode
        argv(4) = '-p'
        argv(5) = procdef_file

        ! TODO: check the values

        SetProcessDefinitionCore = .true.

    end function SetProcessDefinitionCore

    ! DefineWQProcesses --
    !     Define the substances, process parameters etc.
    logical function DefineWQProcesses(substance, number_substances, number_transported, &
            process_parameter, number_parameters, &
            process, number_processes)
        !DEC$ ATTRIBUTES DLLEXPORT::DefineWQProcesses
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'DEFINEWQPROCESSES' :: DefineWQProcesses

        integer(kind = int_wp), intent(in) :: number_substances
        character(len = *), dimension(number_substances) :: substance
        integer(kind = int_wp), intent(in) :: number_transported
        integer(kind = int_wp), intent(in) :: number_parameters
        character(len = *), dimension(number_parameters) :: process_parameter
        integer(kind = int_wp), intent(in) :: number_processes
        character(len = *), dimension(number_processes) :: process

        integer(kind = int_wp), dimension(number_substances) :: substance_mult

        substance_mult = 1

        DefineWQProcesses = DefineWQProcessesCore(substance, substance_mult, &
                number_substances, number_transported, &
                process_parameter, number_parameters, &
                (/' '/), 0, &
                process, number_processes)

    end function DefineWQProcesses

    ! DefineWQProcessesX --
    !     Define the substances, multiplicity, process parameters etc.
    logical function DefineWQProcessesX(substance, substance_mult, &
            number_substances, number_transported, &
            process_parameter, number_parameters, &
            field_parameter, number_fields, &
            process, number_processes)
        !DEC$ ATTRIBUTES DLLEXPORT::DefineWQProcessesX
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'DEFINEWQPROCESSESX' :: DefineWQProcessesX

        character(len = *), dimension(*) :: substance
        integer(kind = int_wp), dimension(*) :: substance_mult
        integer(kind = int_wp), intent(in) :: number_substances
        integer(kind = int_wp), intent(in) :: number_transported
        character(len = *), dimension(*) :: process_parameter
        integer(kind = int_wp), intent(in) :: number_parameters
        character(len = *), dimension(*) :: field_parameter
        integer(kind = int_wp), intent(in) :: number_fields
        character(len = *), dimension(*) :: process
        integer(kind = int_wp), intent(in) :: number_processes

        DefineWQProcessesX = DefineWQProcessesCore(substance, substance_mult, &
                number_substances, number_transported, &
                process_parameter, number_parameters, &
                field_parameter, number_fields, &
                process, number_processes)

    end function DefineWQProcessesX

    ! DefineWQProcessesCore --
    !     Define the substances, multiplicity, process parameters etc.
    !
    !     TODO: DELWAQ parameters (now: everything is considered a constant)
    !           Output parameters
    logical function DefineWQProcessesCore(substance, substance_mult, &
            number_substances, number_transported, &
            process_parameter, number_parameters, &
            field_parameter, number_fields, &
            process, number_processes)

        use delwaq2_global_data
        use m_sysn

        integer(kind = int_wp), intent(in) :: number_substances
        integer(kind = int_wp), intent(in) :: number_parameters
        integer(kind = int_wp), intent(in) :: number_transported
        integer(kind = int_wp), intent(in) :: number_processes
        integer(kind = int_wp), intent(in) :: number_fields

        character(len = *), dimension(number_substances) :: substance
        character(len = *), dimension(number_processes) :: process
        character(len = *), dimension(number_parameters) :: process_parameter
        character(len = *), dimension(number_fields) :: field_parameter

        integer(kind = int_wp), dimension(number_substances) :: substance_mult

        integer(kind = int_wp) :: numsubstot
        integer(kind = int_wp) :: numsubsact
        integer(kind = int_wp) :: i
        character(len = 3) :: numstr
        integer(kind = int_wp) :: j

        if (allocated(substance_name)) deallocate (substance_name)
        if (allocated(mult)) deallocate (mult)
        if (allocated(substance_conc)) deallocate (substance_conc)
        if (allocated(procparam_const)) deallocate (procparam_const)
        if (allocated(procparam_param)) deallocate (procparam_param)
        if (allocated(procparam_const_value)) deallocate (procparam_const_value)
        if (allocated(procparam_param_value)) deallocate (procparam_param_value)

        numsubstot = 0
        numsubsact = 0
        nomult = 0
        do i = 1, number_substances
            if (substance_mult(i) > 1) then
                nomult = nomult + 1
            end if
            numsubstot = numsubstot + substance_mult(i)
            if (i <= number_transported) then
                numsubsact = numsubsact + substance_mult(i)
            end if
        end do

        allocate (substance_name(numsubstot))
        allocate (mult(2, nomult))
        allocate (substance_conc(numsubstot, noseg))
        allocate (procparam_const(number_parameters + number_processes + 1))
        allocate (procparam_const_value(number_parameters + number_processes + 1))
        allocate (procparam_param(number_fields))
        allocate (procparam_param_value(number_fields, noseg))

        numsubstot = 0
        nomult = 0
        do i = 1, number_substances
            if (substance_mult(i) == 1) then
                numsubstot = numsubstot + 1
                substance_name(numsubstot) = substance(i)
            else
                nomult = nomult + 1
                mult(1, nomult) = numsubstot + 1
                mult(2, nomult) = numsubstot + substance_mult(i)
                do j = 1, substance_mult(i)
                    write (numstr, '(I0.2)') j
                    substance_name(numsubstot + j) = trim(substance(i)) // numstr
                end do
                numsubstot = numsubstot + substance_mult(i)
            end if
        end do
        substance_conc = 0.0

        procparam_const(1:number_parameters) = process_parameter(1:number_parameters)
        procparam_const_value(1:number_parameters) = -999.0
        procparam_const(number_parameters + 1) = 'ONLY_ACTIVE'
        procparam_const(number_parameters + 2:) = (/('ACTIVE_' // process(i), i = 1, number_processes)/)
        procparam_const_value(number_parameters + 1:) = 1.0
        procparam_param = field_parameter(1:number_fields)
        procparam_param_value = 0.0

        nosys = numsubsact
        notot = numsubstot
        nototp = numsubstot        ! Particles not supported yet
        nocons = number_parameters + number_processes + 1
        nopa = number_fields
        nofun = 0
        nosfun = 0

        !   administrate state sizes for OpenDA use
        size_dlwq_state%notot = notot
        size_dlwq_state%noseg = noseg
        size_dlwq_state%conc = notot * noseg
        size_dlwq_state%other = 1 ! todo: set this to zero?
        size_dlwq_state%core = size_dlwq_state%conc + size_dlwq_state%other

        size_dlwq_state%rbuf = 0   !not known at this time. TODO: Will this cause problems???
        size_dlwq_state%mass = notot * noseg
        size_dlwq_state%names = notot
        size_dlwq_state%timeadmin = 3
        size_dlwq_state%pseudo = size_dlwq_state%rbuf + size_dlwq_state%mass + size_dlwq_state%names + size_dlwq_state%timeadmin

        size_dlwq_state%output = 9 + 7 * noutp
        size_dlwq_state%total = size_dlwq_state%core + size_dlwq_state%pseudo + size_dlwq_state%output

        write (*, *) 'state sizes have been set in DefineWQProcessesCore'

        DefineWQProcessesCore = .true.

    end function DefineWQProcessesCore

    ! DefineWQExtraOutputParameters --
    !     Define extra output parameters
    logical function DefineWQExtraOutputParameters(extra_output, number_output)
        !DEC$ ATTRIBUTES DLLEXPORT::DefineWQExtraOutputParameters
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'DEFINEWQEXTRAOUTPUTPARAMETERS' :: DefineWQEXTRAOUTPUTPARAMETERS

        use delwaq2_global_data

        integer(kind = int_wp) :: number_output
        character(len = *), dimension(number_output) :: extra_output

        if (allocated(output_param)) deallocate (output_param)

        allocate (output_param(1:number_output))

        output_param = extra_output(1:number_output)

        DefineWQExtraOutputParameters = .true.

    end function DefineWQExtraOutputParameters

    ! DefineDischargeLocations --
    !     Define the location of discharges
    !
    !     Note:
    !     For the moment there is no support for different types of waste loads
    !     or for names
    logical function DefineDischargeLocations(cell, number_loads)
        !DEC$ ATTRIBUTES DLLEXPORT::DefineDischargeLocations
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'DEFINEDISCHARGELOCATIONS' :: DefineDischargeLocations

        use waq_omi_utils
        use delwaq2_global_data
        use m_sysn

        integer(kind = int_wp), intent(in) :: number_loads
        integer(kind = int_wp), dimension(number_loads) :: cell

        integer(kind = int_wp) :: i
        character(len = 100) :: message

        DefineDischargeLocations = .false.

        do i = 1, number_loads
            if (cell(i) < 1 .or. cell(i) > noseg) then
                write (message, '(a,i0,a)') 'Discharge location out of range: ', cell(i), &
                        ' - should be between 1 and the number of segments'
                call SetMessage(LEVEL_ERROR, message)
                return
            end if
        end do

        if (allocated(load_name)) deallocate (load_name)
        if (allocated(load_cell)) deallocate (load_cell)
        if (allocated(load_type)) deallocate (load_type)
        if (allocated(load_type_def)) deallocate (load_type_def)

        allocate (load_name(number_loads))
        allocate (load_cell(number_loads))
        allocate (load_type(number_loads))
        allocate (load_type_def(1))

        load_type_def = 'Default'

        do i = 1, number_loads
            write (load_name(i), '(a,i0)') 'LOAD ', i
            load_cell(i) = cell(i)
            load_type(i) (1:20) = load_name(i) (1:20)
        end do

        nowst = number_loads
        nowtyp = 1

        DefineDischargeLocations = .true.

    end function DefineDischargeLocations

    ! DefineMonitoringLocations --
    !     Define the location of monitoring points and areas
    !
    ! Note:
    ! For the moment there is no support for monitoring areas or transects
    logical function DefineMonitoringLocations(cell, name, number_monitoring)
        !DEC$ ATTRIBUTES DLLEXPORT::DefineMonitoringLocations
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'DEFINEMONITORINGLOCATIONS' :: DefineMonitoringLocations

        use waq_omi_utils
        use delwaq2_global_data
        use m_sysn

        integer(kind = int_wp), intent(in) :: number_monitoring
        character(len = *), dimension(number_monitoring) :: name
        integer(kind = int_wp), dimension(number_monitoring) :: cell

        integer(kind = int_wp) :: i
        character(len = 100) :: message

        DefineMonitoringLocations = .false.

        do i = 1, number_monitoring
            if (cell(i) < 1 .or. cell(i) > noseg) then
                write (message, '(a,i0,a)') 'Monitoring location out of range: ', cell(i), &
                        ' - should be between 1 and the number of segments'
                call SetMessage(LEVEL_ERROR, message)
                return
            end if
        end do

        if (allocated(monitor_name)) deallocate (monitor_name)
        if (allocated(monitor_cell)) deallocate (monitor_cell)
        if (allocated(cells_per_monitor)) deallocate (cells_per_monitor)
        if (allocated(transect_name)) deallocate (transect_name)

        allocate (monitor_name(number_monitoring))
        allocate (monitor_cell(number_monitoring))
        allocate (cells_per_monitor(number_monitoring))
        allocate (transect_name(1))

        monitor_name = name(1:number_monitoring)
        monitor_cell = cell(1:number_monitoring)
        cells_per_monitor = 1

        nodump = 0
        ndmpar = number_monitoring

        DefineMonitoringLocations = .true.

    end function DefineMonitoringLocations

    ! SetInitialVolume
    !     Set the initial volume for all segments
    !
    !     Note: use before ModelInitialize
    logical function SetInitialVolume(volume)

        !DEC$ ATTRIBUTES DLLEXPORT::SetInitialVolume
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETINITIALVOLUME' :: SetInitialVolume

        use waq_omi_utils
        use delwaq2_global_data
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace

        real(kind = real_wp), dimension(noseg), intent(in) :: volume

        integer(kind = int_wp) :: time_dummy
        integer(kind = int_wp) :: lunwrk

        SetInitialVolume = .false.

        time_dummy = 0
        open (newunit = lunwrk, file = trim(runid) // '-volumes.wrk', form = 'unformatted', access = 'stream', err = 911)
        write (lunwrk) time_dummy, volume(1:noseg)
        close (lunwrk)

        SetInitialVolume = .true.
        return

        911   write (*, *) 'setinitialvolume: error!!! '

    end function SetInitialVolume

    ! SetFlowData --
    !     Set the current volumes, areas and flows
    !
    !     Note: use before ModelPerformTimeStep, after ModelInitialize
    logical function SetFlowData(volume, area, flow)

        !DEC$ ATTRIBUTES DLLEXPORT::SetFlowData
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETFLOWDATA' :: SetFlowData

        use waq_omi_utils
        use delwaq2_global_data
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace

        real(kind = real_wp), dimension(noseg), intent(in) :: volume
        real(kind = real_wp), dimension(noq), intent(in) :: area
        real(kind = real_wp), dimension(noq), intent(in) :: flow

        SetFlowData = .false.

        dlwqd%buffer%rbuf(ivol2:ivol2 + noseg - 1) = volume
        dlwqd%buffer%rbuf(iarea:iarea + noq - 1) = area
        dlwqd%buffer%rbuf(iflow:iflow + noq - 1) = flow

        SetFlowData = .true.

    end function SetFlowData

    ! SetFlowData --
    !     Set the current volumes only
    !
    !     Note: use before ModelPerformTimeStep, after ModelInitialize
    logical function SetFlowDataVolume(volume)

        !DEC$ ATTRIBUTES DLLEXPORT::SetFlowDataVolume
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETFLOWDATAVOLUME' :: SetFlowDataVolume

        use waq_omi_utils
        use delwaq2_global_data
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace

        real(kind = real_wp), dimension(noseg), intent(in) :: volume

        SetFlowDataVolume = .false.

        dlwqd%buffer%rbuf(ivol2:ivol2 + noseg - 1) = volume

        SetFlowDataVolume = .true.

    end function SetFlowDataVolume

    logical function SetFlowDataVelocity(velocity)

        !DEC$ ATTRIBUTES DLLEXPORT::SetFlowDataVelocity
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETFLOWDATAVELOCITY' :: SetFlowDataVelocity

        use waq_omi_utils
        use delwaq2_global_data
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace

        real(kind = real_wp), dimension(noq), intent(in) :: velocity

        SetFlowDataVelocity = .false.

        dlwqd%buffer%rbuf(ivelo:ivelo + noq - 1) = velocity

        SetFlowDataVelocity = .true.

    end function SetFlowDataVelocity

    ! CorrectVolumeSurface --
    !     Correct the mass for all substances via new volumes
    !     and horizontal surface areas
    !
    !     Note: use once after ModelInitialize or ModelInitialize_by_Id
    !
    !     Note: the initial volume and the horizontal surface area
    !           should NOT be zero
    integer function CorrectVolumeSurface(volume, surf, mass_per_m2)

        !DEC$ ATTRIBUTES DLLEXPORT::CorrectVolumeSurface
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'CORRECTVOLUMESURFACE' :: CorrectVolumeSurface

        use waq_omi_utils
        use delwaq2_global_data
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace

        real(kind = real_wp), dimension(noseg), intent(in) :: volume
        real(kind = real_wp), dimension(noseg), intent(in) :: surf
        integer(kind = int_wp) :: mass_per_m2

        integer(kind = int_wp) :: error_count
        integer(kind = int_wp) :: iseg, isys, isurf, ioff, ip
        integer(kind = int_wp) :: nosubs
        real(kind = real_wp) :: ratio

        CorrectVolumeSurface = 1

        !
        ! The option determines if the correction for inactive substances
        ! should be done via the volume or the surface area
        !
        if (mass_per_m2 /= 0) then
            nosubs = nosys
        else
            nosubs = notot
        end if

        error_count = 0

        call find_index('SURF      ', procparam_param, isurf)

        do iseg = 1, noseg
            ioff = imass + (iseg - 1) * notot - 1
            ip = iparm + (iseg - 1) * nopa + isurf - 1

            if (abs(dlwqd%buffer%rbuf(ivol + iseg - 1)) > 1.0e-20) then
                ratio = volume(iseg) / dlwqd%buffer%rbuf(ivol + iseg - 1)
                do isys = 1, nosubs
                    dlwqd%buffer%rbuf(ioff + isys) = dlwqd%buffer%rbuf(ioff + isys) * ratio
                end do
                dlwqd%buffer%rbuf(ivol + iseg - 1) = volume(iseg)
            else
                error_count = error_count + 1
            end if

            if (isurf > 0) then
                if (abs(dlwqd%buffer%rbuf(ip)) > 1.0e-20) then
                    ratio = surf(iseg) / dlwqd%buffer%rbuf(ip)
                    do isys = nosubs + 1, notot
                        dlwqd%buffer%rbuf(ioff + isys) = dlwqd%buffer%rbuf(ioff + isys) * ratio
                    end do
                else
                    error_count = error_count + 1
                end if
            end if
        end do

        if (error_count /= 0) then
            CorrectVolumeSurface = 0
            write (*, *) 'Number of segments with zero volume or surface area:', error_count
        else
            CorrectVolumeSurface = 1
        end if

    end function CorrectVolumeSurface

    ! SetWasteLoadValues --
    !     Set the current values for a single waste load
    !
    !     Note: use before ModelPerformTimeStep, after ModelInitialize
    logical function SetWasteLoadValues(idx, value)

        !DEC$ ATTRIBUTES DLLEXPORT::SetWasteLoadValues
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETWASTELOADVALUES' :: SetWasteLoadValues

        use waq_omi_utils
        use delwaq2_global_data
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace

        integer(kind = int_wp) :: idx
        real(kind = real_wp), dimension(notot + 1), intent(in) :: value
        character(len = 20) :: string
        integer(kind = int_wp) :: i
        integer(kind = int_wp) :: i2
        integer(kind = int_wp) :: j

        SetWasteLoadValues = .false.

        if (idx < 1 .or. idx > nowst) then
            write (string, '(i0)') idx
            call SetMessage(LEVEL_ERROR, &
                    'Waste load index out of range - index: ' // string)
            return
        end if

        dlwqd%buffer%rbuf(iwste + (idx - 1) * (notot + 1):iwste + idx * (notot + 1) - 1) = value

        if (reporting) then
            write (lunlst, '(a,i5,a,i10)') 'Waste loads for discharge ', idx, ' - at time: ', dlwqd%itime
            string = 'Flow rate'
            do i = 0, nosys, 5
                i2 = min(i + 4, nosys)
                if (i == 0) then
                    write (lunlst, '(5a20)') adjustr(string), (adjustr(substance_name(j)), j = i + 1, i2)
                else
                    write (lunlst, '(5a20)') (adjustr(substance_name(j)), j = i, i2)
                end if
                write (lunlst, '(5g20.5)') (value(j), j = i + 1, i2 + 1)
            end do
        end if

        SetWasteLoadValues = .true.

    end function SetWasteLoadValues

    ! SetBoundaryConditions --
    !     Set the current values for all boundary cells of a given type
    !
    !     Note: use before ModelPerformTimeStep, after ModelInitialize
    !
    !     TODO:
    !     Make the index work correctly
    logical function SetBoundaryConditions(idx, value)

        !DEC$ ATTRIBUTES DLLEXPORT::SetBoundaryConditions
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'SETBOUNDARYCONDITIONS' :: SetBoundaryConditions

        use waq_omi_utils
        use delwaq2_global_data
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace

        integer(kind = int_wp) :: idx
        real(kind = real_wp), dimension(nosys), intent(in) :: value
        character(len = 20) :: string
        integer(kind = int_wp) :: i
        integer(kind = int_wp) :: i2
        integer(kind = int_wp) :: j

        SetBoundaryConditions = .false.

        if (idx < 1 .or. idx > nobnd) then
            write (string, '(i0)') idx
            call SetMessage(LEVEL_ERROR, &
                    'Boundary index out of range - index: ' // string)
            return
        end if

        dlwqd%buffer%rbuf(ibset + (idx - 1) * nosys:ibset + idx * nosys - 1) = value

        if (reporting) then
            write (lunlst, '(a,i5,a,i10)') 'Conditions for boundary cell ', idx, ' - at time: ', dlwqd%itime
            do i = 1, nosys, 5
                i2 = min(i + 4, nosys)
                write (lunlst, '(5a20)') (adjustr(substance_name(j)), j = i, i2)
                write (lunlst, '(5g20.5)') (value(j), j = i, i2)
            end do
        end if

        SetBoundaryConditions = .true.

    end function SetBoundaryConditions

    ! ModelPerformTimeStep --
    !     Set a single time step
    integer function ModelPerformTimeStep()
        !DEC$ ATTRIBUTES DLLEXPORT::ModelPerformTimeStep
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'MODELPERFORMTIMESTEP' :: ModelPerformTimeStep

        use delwaq2_global_data
        use m_delwaq2_main
        use m_actions

        character(len = 20), dimension(0) :: argv_dummy

        call dlwqmain(ACTION_SINGLESTEP, 0, argv_dummy, dlwqd)

        ModelPerformTimeStep = 0

    end function ModelPerformTimeStep

    ! WriteRestartFileDefaultName --
    !     Write a restart file in .map format using default filename with current status of the model
    integer function WriteRestartFileDefaultName()
        !DEC$ ATTRIBUTES DLLEXPORT::WriteRestartFileDefaultName
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'WRITERESTARTFILEDEFAULTNAME' :: WriteRestartFileDefaultName

        use delwaq2_global_data

        character(len = 255) file_name
        integer(kind = int_wp) :: i, k, ierr, found

        file_name = file_name_list(23)
        found = 0
        do i = 248, 1, -1
            if (file_name(i:i) == '.' .and. found == 0) then
                file_name(i:i + 7) = "_res.map"
                found = 1
            end if
        end do
        if (found == 0) then
            write (*, *) ' Invalid name of restart MAP file !'
            write (file_unit_list(19), *) ' Invalid name of restart MAP file !'
            WriteRestartFileDefaultName = 1
        else
            WriteRestartFileDefaultName = WriteRestartFile(file_name)
        end if
    end function

    ! WriteRestartFile --
    !     Write a restart file in .map format with current status of the model
    integer function WriteRestartFile(file_name)
        !DEC$ ATTRIBUTES DLLEXPORT::WriteRestartFile
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'WRITERESTARTFILE' :: WriteRestartFile

        use delwaq2_global_data
        use m_open_waq_files
        use m_sysn          ! System characteristics
        use m_sysc          ! Pointers in character array workspace
        use m_sysa          ! Pointers in real array workspace

        character(len = *) file_name
        integer(kind = int_wp) :: i, k, ierr

        call open_waq_files(file_unit_list(23), file_name, 23, 1, ierr)
        if (ierr == 0) then
            write (file_unit_list(23)) (dlwqd%buffer%chbuf(imnam + k - 1), k = 1, 160)
            write (file_unit_list(23)) notot, noseg
            write (file_unit_list(23)) (substance_name(k), k = 1, notot)
            write (file_unit_list(23)) dlwqd%itime, ((dlwqd%buffer%rbuf(iconc + (k - 1) + (i - 1) * notot), k = 1, notot), i = 1, noseg)
            close (file_unit_list(23))
            WriteRestartFile = 0
        else
            write (*, *) ' Could not open restart MAP file !'
            write (file_unit_list(19), *) ' Could not open restart MAP file !'
            WriteRestartFile = 1
        end if
    end function

    ! ModelInitialize --
    !     Initialize the model run
    integer function ModelInitialize()
        !DEC$ ATTRIBUTES DLLEXPORT::ModelInitialize
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'MODELINITIALIZE' :: ModelInitialize

        use delwaq2_global_data
        use waq_omi_utils
        use m_cli_utils, only : store_command_arguments
        use m_waq_openda_exchange_items, only : openda_buffer_initialize
        use m_actions
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace
        use m_sysj          ! Pointers in integer array workspace

        type(t_waq_item) :: constants    !< delwaq constants list
        integer(kind = int_wp) :: lunrep, lunwrk
        integer(kind = int_wp) :: ierr

        !
        ! Arguments have already been initialised
        !
        call delwaq2_global_data_initialize(runid)

        if (.not. allocated(argv)) then
            allocate (argv(2))
            argv(1) = 'dlwqlib.dll'
            argv(2) = runid
        end if

        call store_command_arguments(argv)

        !
        ! Some parameters that still need to be set
        !
        lunrep = file_unit_list(19)
        ! VORTech: early, otherwise we get those fort.1 files
        call open_waq_files(lunrep, file_name_list(19), 19, 1, ierr)

        !
        ! Make sure the harmonic work file exists
        !

        open (newunit = lunwrk, file = file_name_list(3), form = 'unformatted', access = 'stream')
        close (lunwrk)

        num_file_units = 45      ! num_file_units has been declared in sysn_ff.inc
        !
        nothrd = 1       ! Set OpenMP threads to 1 (note: read_block_7_process_parameters code to overrule isn't called)
        nogrid = 1       ! No multiple grid option at the moment
        noitem = 11      ! Fixed number of items
        ilflag = 1       ! Always assume varying lengths
        itfact = 86400   ! Auxiliary timescale always 1 day
        newisp = 0
        newrsp = 0
        nlines = noseg * 2 + ((noq1 + noq2 + noq3) * 2) * 2 ! memory related to volumes, areas, flows?
        npoins = noseg + 3 + ((noq1 + noq2 + noq3) + 3) * 2 ! memory related to volumes, areas, flows?

        ! fill the constants structure for use in dlwqp1
        ierr = constants%initialize()
        ierr = constants%resize(nocons)
        constants%no_item = nocons
        constants%name(1:nocons) = procparam_const(1:nocons)
        constants%constant(1:nocons) = procparam_const_value(1:nocons)

        if (nocons > 0) then
            newisp = newisp + nocons + 5 + 2
            newrsp = newrsp + nocons
            nufil = 0
        end if
        if (nopa > 0) then
            newisp = newisp + nopa + noseg + 5 + 3
            newrsp = newrsp + noseg * nopa * 3
            nufil = 1
        end if

        call write_delwaq04(argv(2))
        call write_array_2d(argv(2), 'initials', substance_conc)
        if (nopa > 0) then
            call write_array_2d(argv(2), 'params', procparam_param_value)
            open (newunit = lunwrk, file = file_name_list(41))
            write (lunwrk, '(i5,a1,a256)') 0, ' ', trim(argv(2)) // '-params.wrk'
            close (lunwrk)
        end if

        !
        ! Require SetInitialVolume instead
        call write_array_const(argv(2), 'flows', 0.0, noq)
        call write_array_const(argv(2), 'areas', 1.0, noq)
        call write_array_const(argv(2), 'wastload', 0.0, nowst * (notot + 1))
        call write_array_const(argv(2), 'boundary', 0.0, nobnd * nosys)
        call write_functions(argv(2))

        call handle_output_requests(argv(2))
        call handle_processes(argv(2))

        !
        ! Now write the size information
        !
        call write_delwaq03(argv(2))

        !
        ! Echo the input (also during the computation)
        !
        reporting = .true.
        lunlst = 2
        open (lunlst, file = trim(argv(2)) // '.lst')
        call report_model_data(lunlst)
        !
        ! Everything has been prepared
        !
        call dlwqmain(ACTION_INITIALISATION, 2, argv, dlwqd)

        ! openDA buffer
        call openda_buffer_initialize

        ModelInitialize = 0

    contains

        ! Write the first DELWAQ system intermediate file
        subroutine write_delwaq03(name)
            use workspace, only : set_array_indexes
            use m_sysn          ! System characteristics
            use m_sysi          ! Timer characteristics

            character(len = *) :: name

            integer(kind = int_wp) :: imaxa, imaxi, imaxc
            type(waq_data_buffer) :: buffer
            integer(kind = int_wp) :: lunwrk

            !noutp = 0 ! TODO: requires additional information in delwaq03

            imaxa = 0
            imaxi = 0
            imaxc = 0

            call set_array_indexes(lunrep, .false., buffer%rbuf, buffer%ibuf, buffer%chbuf, imaxa, imaxi, imaxc)

            open (newunit = lunwrk, file = trim(name) // '-delwaq03.wrk', form = 'unformatted', access = 'stream')
            write (lunwrk) in
            write (lunwrk) ii
            write (lunwrk) imaxa, imaxi, imaxc

            write (lunwrk) file_unit_list(1:num_file_units)
            write (lunwrk) file_name_list(1:num_file_units)
            write (lunwrk) filtype(1:num_file_units)
            close (lunwrk)

        end subroutine write_delwaq03

        ! write_delwaq04 --
        !     Write the second DELWAQ system intermediate file
        !
        subroutine write_delwaq04(name)
            use m_grid_utils_external
            use m_sysn          ! System characteristics
            use m_sysi          ! Timer characteristics

            character(len = *) :: name

            integer(kind = int_wp) :: i
            integer(kind = int_wp) :: idummy
            integer(kind = int_wp) :: iref
            integer(kind = int_wp) :: load_kind
            integer(kind = int_wp) :: iseg
            integer(kind = int_wp) :: error
            integer(kind = int_wp) :: lunwrk

            type(t_grid) :: aGrid

            title(1) = 'Wrapper for DELWAQ-DLL'
            title(2) = '                      '
            title(3) = '                      '
            title(4) = '                      '

            write (title(4), '(a4,i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)') &
                    'T0: ', ref_year, '.', ref_month, '.', ref_day, ' ', &
                    ref_hour, ':', ref_minute, ':', ref_second, '  (scu=       1s)'

            open (newunit = lunwrk, file = trim(name) // '-delwaq04.wrk', form = 'unformatted', access = 'stream')
            write (lunwrk) title

            write (lunwrk) substance_name

            write (lunwrk) (monitor_name(i), i = 1, ndmpar)
            write (lunwrk) (monitor_cell(i), i = 1, ndmpar)

            ! Grid definitions - base grid only
            iref = 1
            write (lunwrk) noseg, iref, (i, i = 1, noseg)

            !
            ! Copied from grid.f
            aGrid%name = 'Base grid'
            aGrid%noseg = noseg
            aGrid%noseg_lay = noseg / nolay
            aGrid%iref = 1
            aGrid%name_ref = ' '
            aGrid%itype = BaseGrid
            aGrid%space_var_nolay = .FALSE.
            aGrid%nolay = nolay
            aGrid%nolay_var => null()
            allocate (aGrid%iarray(noseg))
            allocate (aGrid%finalpointer(noseg))
            do iseg = 1, noseg
                agrid%iarray(iseg) = iseg
                agrid%finalpointer(iseg) = iseg
            end do

            error = aGrid%write(10)

            !
            ! Now the rest ...
            !
            idummy = 1
            write (lunwrk) (idummy, i = 1, notot) ! SYSGRD in set_grid_all_processes
            write (lunwrk) (idummy, i = 1, notot) ! SYSNDT in set_grid_all_processes

            write (lunwrk) iknmrk

            if (nodisp > 0) write (lunwrk) diname
            if (novelo > 0) write (lunwrk) vename

            if (allocated(idpnt_array)) deallocate (idpnt_array)
            allocate (idpnt_array(1:nosys), ivpnt_array(1:nosys))
            idpnt_array = 0   ! For the moment
            ivpnt_array = 0
            write (lunwrk) idpnt_array
            write (lunwrk) ivpnt_array

            if (nobnd > 0) then
                write (lunwrk) ibpnt_array(2, :)
                write (lunwrk) ibpnt_array(3, :)
            end if

            ! Monitoring points, not monitoring areas
            ! No transect information yet
            call write_delwaq04_monitoring

            idummy = 0
            write (lunwrk) idummy, (disp(i), i = 1, 3)
            write (lunwrk) idummy, (aleng(i), i = 1, 3)

            if (nobnd > 0) then
                write (lunwrk) (boundary_id(i), boundary_name(i), i = 1, nobnd)
                write (lunwrk) boundary_type
                write (lunwrk) (/(i, i = 1, nobnd)/)  ! Type
                write (lunwrk) ibpnt_array(1, :)      ! Time lag
            end if

            if (nowst > 0) then
                ! TODO:
                ! Allow the waste load kind to be set
                load_kind = 0
                write (lunwrk) (load_cell(i), load_kind, load_type(i), load_name(i), i = 1, nowst)
                write (lunwrk) load_type_def
                write (lunwrk) (1, i = 1, nowst)
            end if

            write (lunwrk) procparam_const
            write (lunwrk) procparam_param ! No separate function/segment function

            ! Time function information

            if (allocated(nrftot)) deallocate (nrftot)
            if (allocated(nrharm)) deallocate (nrharm)
            allocate (nrftot(1:noitem), nrharm(1:noitem))
            nrftot = 0
            nrharm = 0
            write (lunwrk) nrftot
            write (lunwrk) nrharm

            close (lunwrk)

        end subroutine write_delwaq04

        subroutine write_delwaq04_monitoring
            use m_error_status

            integer(kind = int_wp) :: ndmpq
            integer(kind = int_wp) :: ndmps
            integer(kind = int_wp) :: noraai
            integer(kind = int_wp) :: ntraaq
            integer(kind = int_wp), dimension(1) :: nexcraai
            integer(kind = int_wp), dimension(1) :: iexcraai
            integer(kind = int_wp), dimension(1) :: ioptraai
            integer(kind = int_wp), dimension(ndmpar) :: nsegdmp
            integer(kind = int_wp), dimension(ndmpar) :: isegdmp

            type(error_status) :: status !< dummy error status

            ntdmps = ndmpar  ! For now
            nsegdmp = 1
            isegdmp = monitor_cell

            noraai = 0      ! For now
            ntraaq = 0      ! For now
            file_unit_list(2) = 10

            call create_write_monitoring_area_array(file_unit_list, ndmpar, ntdmps, noq, noseg, nobnd, ipoint, ntdmpq, ndmpq, ndmps, &
                    noraai, ntraaq, nsegdmp, isegdmp, nexcraai, iexcraai, ioptraai, &
                    status)

        end subroutine write_delwaq04_monitoring

        ! handle_output_requests --
        !     Write a small piece of the DELWAQ input file for handling
        !     the output options
        !
        subroutine handle_output_requests(name)
            use processet
            use results
            use m_sysn          ! System characteristics

            character(len = *) :: name

            integer(kind = int_wp) :: i
            integer(kind = int_wp) :: k
            integer(kind = int_wp) :: luninp

            open (newunit = luninp, file = trim(name) // '.inp')

            write (luninp, '(a)') '1 ; output information in this file'
            do k = 1, 4
                write (luninp, '(a)') '2 ; all substances and extra output'
                write (luninp, '(i5,a)') size(output_param), ' ; number of extra variables'
                do i = 1, size(output_param)
                    if (k == 1 .or. k == 3) then
                        write (luninp, '(a,1x,a)') output_param(i), ''' '''
                    else
                        write (luninp, '(a,1x,a)') output_param(i)
                    end if
                end do
            end do

            write (luninp, '(a)') ' 1 ; binary history file on'
            write (luninp, '(a)') ' 1 ; binary map     file on'
            write (luninp, '(a)') ' 1 ; nefis  history file on'
            write (luninp, '(a)') ' 1 ; nefis  map     file on'

            write (luninp, '(a)') ' #9 ; delimiter for the ninth block'
            write (luninp, '(a)') ' #10 ; delimiter for the tenth block'

            close (luninp)

        end subroutine handle_output_requests

        ! handle_processes --
        !     Handle all the process information
        !
        subroutine handle_processes(name)
            use processet
            use results, only : OutputPointers
            use rd_token
            use m_sysn          ! System characteristics

            character(len = *) :: name

            type(procespropcoll) :: statprocesdef   ! the statistical proces definition
            type(itempropcoll) :: allitems        ! all items of the proces system
            integer(kind = int_wp) :: ioutps(7, 10)     ! (old) output structure
            type(OutputPointers) :: outputs         ! output structure
            integer(kind = int_wp) :: org_noutp        ! Store the number of output files
            ! Pointers into DELWAQ arrays
            integer(kind = int_wp), parameter :: nopred = 6       ! Predefined parameters - fioutv.f
            integer(kind = int_wp) :: iocons           ! Constants
            integer(kind = int_wp) :: iopa             ! Parameters
            integer(kind = int_wp) :: iofun            ! Functions
            integer(kind = int_wp) :: iosfun           ! Segment functions
            integer(kind = int_wp) :: ioconc           ! Concentrations

            integer(kind = int_wp), parameter :: max_char_size = 2000
            integer(kind = int_wp), parameter :: max_int_size = 2000
            character(len = 20), dimension(max_char_size) :: char_arr
            integer(kind = int_wp), dimension(max_int_size) :: iar
            integer(kind = int_wp) :: iwidth
            integer(kind = int_wp) :: output_verbose_level  ! Dummy
            real :: version = 4.9
            integer(kind = int_wp) :: refday
            type(error_status) :: status

            StatProcesDef%maxsize = 0
            StatProcesDef%current_size = 0
            AllItems%maxsize = 0
            AllItems%current_size = 0

            org_noutp = noutp

            iocons = nopred + 1
            iopa = iocons + nocons
            iofun = iopa + nopa
            iosfun = iofun + nofun
            ioconc = iosfun + nosfun

            !
            ! For the moment: only output the substances, nothing extra
            !

            open (9, file = trim(name) // '.inp')
            open (file_unit_list(29), file = trim(name) // '.lstdummy')

            npos = 100
            cchar = ';'
            iwidth = 100
            output_verbose_level = 0
            call status%initialize(0, 0, 0)

            ilun(1) = 9
            lch(1) = trim(name) // '.inp'

            call read_block_9(file_unit_list, file_name_list, filtype, char_arr, iar, max_char_size, &
                    max_int_size, iwidth, &
                    output_verbose_level, ioutps, outputs, status)

            close (9) ! TODO: status = 'delete'
            close (11)

            call dlwqp1(file_unit_list, file_name_list, statprocesdef, allitems, &
                    ioutps, outputs, nomult, mult, constants, &
                    refday, status)

            noutp = org_noutp

        end subroutine handle_processes

        subroutine write_array_2d(name, suffix, array)

            character(len = *) :: name
            character(len = *) :: suffix
            real(kind = real_wp), dimension(:, :) :: array

            integer(kind = int_wp) :: time_dummy
            integer(kind = int_wp) :: file_unit

            time_dummy = 0

            open (newunit = file_unit, file = trim(name) // '-' // trim(suffix) // '.wrk', form = 'unformatted', &
                    access = 'stream')
            write (file_unit) time_dummy, array
            close (file_unit)

        end subroutine write_array_2d

        ! Write the file with constants, functions etc
        subroutine write_functions(name)
            character(len = *), intent(in) :: name

            integer(kind = int_wp), parameter :: FILE_NAME_SIZE = 256
            integer(kind = int_wp), parameter :: ITEM_NAME_SIZE = 20
            integer(kind = int_wp) :: k
            integer(kind = int_wp) :: i
            integer(kind = int_wp) :: file_unit = 10
            character(len = FILE_NAME_SIZE) :: filename
            character(len = ITEM_NAME_SIZE) :: loc

            open (file_unit, file = trim(name) // '-function.wrk', form = 'unformatted', access = 'stream')
            write (file_unit) ' 5.000PROCES'
            i = 0
            if (nocons > 0) then
                i = i + 1
            end if
            if (nopa > 0) then
                i = i + 1
            end if

            write (file_unit) i ! proc_pars%current_size
            if (nocons > 0) then
                write (file_unit) 10       ! subject SUBJECT_CONSTANT
                write (file_unit) nocons   ! no_param
                write (file_unit) 1        ! no_loc
                write (file_unit) 0        ! no_brk
                write (file_unit) 0        ! functype FUNCTYPE_CONSTANT
                write (file_unit) 1        ! igrid
                write (file_unit) .false.  ! extern
                write (file_unit) 0        ! filetype FILE_NONE
                filename = ''
                write (file_unit) filename
                write (file_unit) 2        ! iorder ORDER_LOC_PARAM
                write (file_unit) .true.   ! param_named
                write (file_unit) procparam_const
                write (file_unit) .true.   ! loc_named
                loc = 'constant'
                write (file_unit) loc
                write (file_unit) .true.   ! param_pointered
                write (file_unit) (i, i = 1, nocons)
                write (file_unit) .false.  ! loc_defaults
                write (file_unit) .false.  ! loc_pointered
                write (file_unit) .false.  ! scaled
                write (file_unit) 1.0_4    ! scale_factor
                write (file_unit) .false.  ! param_scaled
                write (file_unit) .false.  ! loc_scaled
                write (file_unit) procparam_const_value
            end if
            if (nopa > 0) then
                write (file_unit) 11       ! subject SUBJECT_PARAMETER
                write (file_unit) nopa     ! no_param
                write (file_unit) noseg    ! no_loc
                write (file_unit) 0        ! no_brk
                write (file_unit) 0        ! functype FUNCTYPE_CONSTANT
                write (file_unit) 1        ! igrid
                write (file_unit) .false.  ! extern
                write (file_unit) 0        ! filetype FILE_NONE
                filename = ''
                write (file_unit) filename
                write (file_unit) 2        ! iorder ORDER_LOC_PARAM
                write (file_unit) .true.   ! param_named
                write (file_unit) procparam_param
                write (file_unit) .true.   ! loc_named
                do i = 1, noseg
                    write (loc, '(A8,I8)') 'segment ', i
                    write (file_unit) loc
                end do
                write (file_unit) .true.   ! param_pointered
                write (file_unit) (i, i = 1, nopa)
                write (file_unit) .false.  ! loc_defaults
                write (file_unit) .false.  ! loc_pointered
                write (file_unit) .false.  ! scaled
                write (file_unit) 1.0_4    ! scale_factor
                write (file_unit) .false.  ! param_scaled
                write (file_unit) .false.  ! loc_scaled
                write (file_unit) transpose(procparam_param_value)
            end if

            close (file_unit)

        end subroutine write_functions

        ! Report the model input in the monitor file
        subroutine report_model_data(file_unit)
            integer(kind = int_wp), intent(in) :: file_unit
            integer(kind = int_wp) :: i

            write (file_unit, '(a)') 'Run:'
            write (file_unit, '(4x,a)') title
            write (file_unit, '(/,a)') 'Integration details:'
            write (file_unit, '(4x,a,i5)') 'Integration method: ', intsrt
            write (file_unit, '(4x,a,i5)') 'Secondary options:  ', intopt
            write (file_unit, '(/,a)') 'Schematisation:'
            write (file_unit, '(4x,a,i10)') 'Number of segments: ', noseg
            write (file_unit, '(4x,a,4i10)') 'Number of exchanges:', noq1, noq2, noq3, noq4
            write (file_unit, '(/,a)') 'Substances:'
            write (file_unit, '(i5,1x,a20)') (i, substance_name(i), i = 1, size(substance_name))
            write (file_unit, '(/,a)') 'Constants/functions:'
            if (size(procparam_const) > 0) then
                write (file_unit, '(i5,1x,a20,g14.5)') (i, procparam_const(i), procparam_const_value(i), i = 1, size(procparam_const))
            else
                write (file_unit, '(4xa)') 'None'
            end if
            write (file_unit, '(/,a)') 'Parameters/segment functions:'

            if (size(procparam_param) > 0) then
                write (file_unit, '(i5,1x,a20)') (i, procparam_param(i), i = 1, size(procparam_param))
            else
                write (file_unit, '(4xa)') 'None'
            end if
            write (file_unit, '(/,a)') 'Monitor points:'
            if (size(monitor_name) > 0) then
                write (file_unit, '(i5,1x,a20,i10)') (i, monitor_name(i), monitor_cell(i), i = 1, size(monitor_name))
            else
                write (file_unit, '(4xa)') 'None'
            end if
            write (file_unit, '(/,a)') 'Waste load points:'
            if (size(load_name) > 0) then
                write (file_unit, '(i5,1x,a20,i10,1x,a20)') (i, load_name(i), load_cell(i), load_type(i), i = 1, size(load_name))
            else
                write (file_unit, '(4xa)') 'None'
            end if
            write (file_unit, '(/,a)') 'Boundary cells:'
            if (size(boundary_name) > 0) then
                write(file_unit, '(i5,1x,a20,a20,1x,a20)') (i, boundary_name(i), boundary_id(i), boundary_type(i), i = 1, size(boundary_name))
            else
                write (file_unit, '(4xa)') 'None'
            end if

            write (file_unit, '(//)')
        end subroutine report_model_data

    end function ModelInitialize


    ! Conclude the model run - final output will be written
    integer function ModelFinalize()
        !DEC$ ATTRIBUTES DLLEXPORT::ModelFinalize
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'MODELFINALIZE' :: ModelFinalize

        use delwaq2_global_data
        use m_delwaq2_main
        use m_actions

        character(len = 20), dimension(0) :: argv_dummy
        integer(kind = int_wp) :: ierr

        call dlwqmain(ACTION_FINALISATION, 0, argv_dummy, dlwqd)

        write (*, *) 'Model has been finalized'

        ! now deallocate all arrays in this lib!
        call delwaq2_global_data_finalize

        ModelFinalize = 0

    end function ModelFinalize

    ! Create the model instance from an ID (currently: the name of a
    ! preprocessed input file)
    integer function ModelInitialize_By_Id(runid_given)
        !DEC$ ATTRIBUTES DLLEXPORT::ModelInitialize_By_id
        !DEC$ ATTRIBUTES DECORATE, ALIAS : 'MODELINITIALIZE_BY_ID' :: ModelInitialize_By_Id

        use delwaq2_global_data
        use waq_omi_utils
        use m_cli_utils, only : store_command_arguments
        use m_waq_openda_exchange_items, only : openda_buffer_initialize
        use m_actions
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace
        use m_sysj          ! Pointers in integer array workspace

        character(len = *), intent(in) :: runid_given

        ! TODO: it is probably not all that simple
        if (.not. allocated(argv)) then
            allocate (argv(3))
        end if
        argv(1) = 'dlwqlib.dll' ! argument 0 is the executable name on the command line
        argv(2) = runid_given
        argv(3) = '-waq'

        call store_command_arguments(argv)
        call delwaq2_global_data_initialize(runid_given)

        ! Leave everything to DELWAQ itself
        call dlwqmain(ACTION_INITIALISATION, 2, argv, dlwqd)

        ! Extract some data (mostly names) from the DLWQD data structure
        ! for later use
        !
        ! TODO

        call delwaq2_global_data_copy(dlwqd)

        ! openDA buffer
        call openda_buffer_initialize

        ModelInitialize_By_Id = 0

    end function ModelInitialize_By_Id
end module waq_omi_interface
