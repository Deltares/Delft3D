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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

! Note:
!     The routines in this module are based on the "BMI"-like interface
!     used by DIMR. This is not exactly BMI 2.0 and in some cases actually
!     clashes with that standard. The relationship with BMI 2.0 has
!     been added in the comments to each routine.
!
!     The authorative description of BMI 2.0 can be found here:
!     https://csdms.colorado.edu/wiki/BMI   (introduction)
!     https://bmi.readthedocs.io/en/stable/ (latest documentation)
!
!     To solve the above mentioned incompatibility the BMI 2.0 routines
!     that clash with the current (october 2023) version of DIMR are
!     prefixed with "BMI2_" in their name. This is a unique substring,
!     so once we can rely on BMI 2.0 to be useable, we can simply
!     remove that.
!

module bmi

    use m_waq_precision
    use m_string_utils
    use m_delwaq2_main
    use delwaq2_global_data
    use delwaq_loads
    use m_delwaq1
    use m_getidentification
    use iso_c_binding
    use iso_c_utils
    implicit none

    ! Define some global constants
    character(*), parameter :: PREFIX = "WAQ"
    !DEC$ ATTRIBUTES DLLEXPORT :: PREFIX
    integer(c_int) :: MAXNAMES = 100
    integer(c_int), BIND(C, name = "MAXSTRLEN") :: MAXSTRLEN = 1024
    !DEC$ ATTRIBUTES DLLEXPORT :: MAXSTRLEN
    integer(c_int), BIND(C, name = "MAXDIMS") :: MAXDIMS = 6
    !DEC$ ATTRIBUTES DLLEXPORT :: MAXDIMS

    ! Store the connections between DELWAQ and external components
    integer(kind = int_wp), parameter :: category_hydrodynamics = 1  ! Placeholder!
    integer(kind = int_wp), parameter :: category_boundary = 2
    integer(kind = int_wp), parameter :: category_wasteload = 3
    integer(kind = int_wp), parameter :: category_segment = 4
    integer(kind = int_wp), parameter :: category_monitorpoint = 5
    integer(kind = int_wp), parameter :: category_procparam = 6

    type :: connection_data
        logical :: incoming       ! Identifies the direction: if true, data from outside
        integer(kind = int_wp) :: category       ! Which category of data: concentrations, process parameters ...
        integer(kind = int_wp) :: buffer_idx     ! Index into the RBUF array
        integer(kind = int_wp) :: system_idx     ! Index of the substance or quantity in general
        real(kind = dp), pointer :: p_value        ! Copy of the value received/sent by DELWAQ - must be a pointer!
        character(len = 40) :: exchange_name  ! Name used by get_var to identify the item
    end type

    type(connection_data), allocatable, target, save :: connection(:)

contains

    integer(c_int) function set_var(c_key, xptr) bind(C, name = "set_var")
        ! set_var --
        !>    The set_var function lets the caller set a variable within DELWAQ.
        !!    Currently only used to manipulate key-value pairs that could appear
        !!    on the command line.
        !!    Always returns zero - there is no error condition
        !
        !     Note: defined by BMI 2.0
        !
        !DEC$ ATTRIBUTES DLLEXPORT::set_var
        character(kind = c_char), intent(in) :: c_key(MAXSTRLEN)  !< Incoming string, determines the variable to be set
        type(c_ptr), value, intent(in) :: xptr              !< C-pointer to the actual value to be picked up by DELWAQ
        !
        character(kind = c_char), dimension(:), pointer :: c_value => null()
        character(MAXSTRLEN) :: key_given
        character(MAXSTRLEN) :: value_given
        integer(kind = int_wp) :: argc
        integer(kind = int_wp) :: argnew
        integer(kind = int_wp) :: iarg
        integer(kind = int_wp) :: errorcode
        integer(kind = int_wp) :: i

        write (88, *) 'Set_var ...'
        flush (88)
        ! Store the key and value
        key_given = char_array_to_string(c_key)
        call c_f_pointer(xptr, c_value, [MAXSTRLEN])

        write (88, *) 'Set_var: ', key_given, ' -- ', c_value
        flush (88)

        value_given = " "
        if (associated(c_value)) then
            do i = 1, MAXSTRLEN
                if (c_value(i) == c_null_char) exit
                value_given(i:i) = c_value(i)
            end do
        end if
        !
        argnew = 2
        if (value_given(1:1) == ' ') argnew = 1
        if (key_given(1:1) == ' ') argnew = 0
        !
        if (argnew > 0) then
            ! Add new arguments to argv
            if (allocated(argv_tmp)) deallocate (argv_tmp)
            if (allocated(argv)) then
                argc = size(argv, 1)
                allocate (argv_tmp(argc))
                do iarg = 1, argc
                    argv_tmp(iarg) = argv(iarg)
                end do
                deallocate (argv)
            else
                argc = 0
            end if
            allocate (argv(argc + argnew))
            do iarg = 1, argc
                argv(iarg) = argv_tmp(iarg)
            end do
            argv(argc + 1) = key_given
            if (argnew == 2) then
                argv(argc + 2) = value_given
            end if
        end if
        set_var = 0
    end function set_var

    ! Control

    integer(c_int) function initialize(c_config_file) bind(C, name = "initialize")
        ! initialize --
        !>    The initialize() function accepts a string argument that
        !!    gives the name (and path) of its "main input file", called
        !!    a configuration file. This function should perform all tasks
        !!    that are to take place before entering the model's time loop.
        !!
        !!    Returns 0 if successful, 1 otherwise.
        !
        !     Note: defined by BMI 2.0
        !
        !DEC$ ATTRIBUTES DLLEXPORT::initialize
        use m_actions
        use m_sysi

        character(kind = c_char), intent(in) :: c_config_file(MAXSTRLEN)  !< Name of the DELWAQ input file

        character(len = strlen(c_config_file)) :: runid_given
        integer(kind = int_wp) :: argc
        integer(kind = int_wp) :: iarg

        ! local
        logical :: init_successful

        write (88, *) 'Initialise ...'
        flush (88)
        ! Store the name
        runid_given = char_array_to_string(c_config_file)

        write (88, *) 'Initialise: ', runid_given
        flush (88)

        ! Add runid_given before the current arguments list
        if (allocated(argv_tmp)) deallocate (argv_tmp)
        if (allocated(argv)) then
            argc = size(argv, 1)
            allocate (argv_tmp(argc))
            do iarg = 1, argc
                argv_tmp(iarg) = argv(iarg)
            end do
            deallocate (argv)
        else
            argc = 0
        end if
        allocate (argv(argc + 2))
        argv(1) = 'delwaq.dll' ! argument 0 is the executable name on the command line
        argv(2) = runid_given
        do iarg = 1, argc
            argv(iarg + 2) = argv_tmp(iarg)
        end do
        argc = argc + 2

        init_successful = delwaq1(argv)

        if (init_successful) then
            call delwaq2_global_data_initialize(runid_given)
            call dlwqmain(ACTION_INITIALISATION, argc, argv, dlwqd)
            call delwaq2_global_data_copy(dlwqd)
            initialize = 0
        else
            initialize = 1
        end if

        write (88, *) 'Initialise: ', dlwqd%otime, dlwqd%itime, dlwqd%tscale, idt
        flush (88)
    end function initialize

    subroutine get_version_string(c_version_string) bind(C, name = "get_version_string")
        ! get_version_string --
        !>    The subroutine get_version_string() returns the version of DELWAQ
        !!    that is being used.
        !
        !     Note: NOT defined by BMI 2.0
        !
        !DEC$ ATTRIBUTES DLLEXPORT :: get_version_string

        character(kind = c_char), intent(out) :: c_version_string(MAXSTRLEN)
        character(len = MAXSTRLEN) :: name
        character(len = 120) :: idstr

        write (88, *) 'Get_version_string ...'
        flush (88)

        call getidentification(idstr)
        name = trim(idstr)
        c_version_string = string_to_char_array(trim(name))
    end subroutine get_version_string

    subroutine get_attribute(c_att_name, c_att_value) bind(C, name = "get_attribute")
        ! get_attribute --
        !>    Returns a static attribute (i.e. an attribute that does not change
        !!    from one model application to the next) of the model (as a string)
        !!    When passed any attribute name from the following list:
        !!    * model_name
        !!    * version      (e.g. 2.0.1)
        !!    * author_name
        !!    * grid_type
        !!    * time_step_type
        !!    * step_method   (explicit, implicit, semi_implicit, iterative)
        !
        !     Note: NOT defined by BMI 2.0
        !
        !DEC$ ATTRIBUTES DLLEXPORT :: get_attribute
        use delwaq_version_module
        character(kind = c_char), intent(in) :: c_att_name(MAXSTRLEN)  !< Attribute name as C-delimited character string.
        character(kind = c_char), intent(out) :: c_att_value(MAXSTRLEN) !< Returned attribute value as C-delimited character string.

        character(len = strlen(c_att_name)) :: att_name
        character(len = MAXSTRLEN) :: att_value

        write (88, *) 'Get_attribute ...'
        flush (88)

        ! Store the name
        att_name = char_array_to_string(c_att_name)

        write (88, *) 'Get_attribute: ', att_name
        flush (88)

        select case (att_name)
        case ('model_name')
            att_value = component_name
        case ('version')
            att_value = major_minor_buildnr
        case ('author_name')
            att_value = company
        case default
            att_value = 'unknown attribute'
        end select

        c_att_value = string_to_char_array(trim(att_value))

        write (88, *) 'Get_attribute done -- ', trim(att_value)
        flush (88)
    end subroutine get_attribute

    integer function update(dt) bind(C, name = "update")
        ! update --
        !>    Run the model calculation over a given interval (dt)
        !!
        !!    Always returns 0
        !
        !     Note: BMI 2.0 defines a different interface
        !           update() (single step) and update_until (up to some end time)
        !           Neither exactly what update() is supposed to do according to DIMR.
        !

        !DEC$ ATTRIBUTES DLLEXPORT :: update
        use delwaq2_global_data
        use iso_c_binding, only : c_double

        real(c_double), value, intent(in) :: dt        !< Interval over which the calculation is to be done.
        !! May involve one or more internal timesteps

        real(c_double) :: tupdate

        tupdate = dlwqd%itime + dt

        update = update_until(tupdate)

    end function update

    integer function update_until(tupdate) bind(C, name = "update_until")
        ! update_until --
        !>    Run the model calculation up to a given model time
        !!
        !!    Always returns 0

        !DEC$ ATTRIBUTES DLLEXPORT :: update
        use delwaq2_global_data
        use messagehandling
        use iso_c_binding, only : c_double
        use m_actions
        use m_sysi

        real(c_double), value, intent(in) :: tupdate   !< Time until which the calculation is to be run.
        !! May involve one or more internal timesteps

        integer(kind = int_wp) :: update_steps, step
        character(len = 20), dimension(0) :: argv_dummy

        update_steps = nint(tupdate - dlwqd%itime) / idt

        write (88, *) 'Update ...', tupdate, update_steps, dlwqd%itime
        flush (88)

        if (intsrt == 2) then
            ! Correct update_steps for delwaq scheme 2, which does a double time step every call
            update_steps = (update_steps + 1) / 2
        end if

        call update_from_incoming_data(connection)

        do step = 1, update_steps
            call dlwqmain(ACTION_SINGLESTEP, 0, argv_dummy, dlwqd)
        end do
        update_until = 0

        write (88, *) 'Update done'
        flush (88)

    end function update_until

    integer function finalize() bind(C, name = "finalize")
        ! finalize --
        !>    Finish the model calculation (close files, clean up, etc.)
        !!
        !!    Always returns 0
        !
        !     Note: defined by BMI 2.0
        !

        !DEC$ ATTRIBUTES DLLEXPORT :: finalize
        use m_actions

        character(len = 20), dimension(0) :: argv_dummy

        write (88, *) 'Finalise ...', dlwqd%itime
        flush (88)

        write (88, *) 'Finalise ...', dlwqd%itime
        flush (88)

        call dlwqmain(ACTION_SINGLESTEP, 0, argv_dummy, dlwqd)
        call dlwqmain(ACTION_FINALISATION, 0, argv_dummy, dlwqd)
        call delwaq2_global_data_finalize()

        finalize = 0

        write (88, *) 'Finalise done'
        flush (88)

    end function finalize

    ! get_start_time --
    !>    Return the start time of the calculation
    !!
    !!    Always returns 0
    !
    !     Note 1: BMI 2.0 defines this as a function. As the name is defined by DIMR
    !             we cannot easily wrap it.
    !     Note 2: The time frame and unit are to the discretion of the components.
    !             Due to the implementation of DIMR
    !
    subroutine get_start_time(t) bind(C, name = "get_start_time")
        !DEC$ ATTRIBUTES DLLEXPORT :: get_start_time
        real(c_double), intent(out) :: t

        integer(kind = int_wp) :: rc

        rc = BMI2_get_start_time(t)

    end subroutine get_start_time

    ! get_end_time --
    !>    Return the end time of the calculation
    !!
    !!    Always returns 0
    !
    !     Note: See get_start_time
    !
    subroutine get_end_time(t) bind(C, name = "get_end_time")
        !DEC$ ATTRIBUTES DLLEXPORT :: get_end_time
        real(c_double), intent(out) :: t

        integer(kind = int_wp) :: rc

        rc = BMI2_get_end_time(t)

    end subroutine get_end_time

    ! get_time_step --
    !>    Return the end time of the calculation
    !!
    !!    Always returns 0
    !
    !     Note: See get_start_time
    !
    subroutine get_time_step(dt) bind(C, name = "get_time_step")
        !DEC$ ATTRIBUTES DLLEXPORT :: get_time_step
        real(c_double), intent(out) :: dt

        integer(kind = int_wp) :: rc

        rc = BMI2_get_time_step(dt)

    end subroutine get_time_step

    ! get_current_time --
    !>    Return the current time in the calculation
    !!
    !!    Always returns 0
    !
    !     Note: See get_start_time
    !
    subroutine get_current_time(t) bind(C, name = "get_current_time")
        !DEC$ ATTRIBUTES DLLEXPORT :: get_current_time
        real(c_double), intent(out) :: t
        integer(kind = int_wp) :: current

        integer(kind = int_wp) :: rc

        rc = BMI2_get_current_time(t)

    end subroutine get_current_time

    ! BMI2_get_start_time --
    !>    Return the start time of the calculation
    !!
    !!    Always returns 0
    !
    integer function BMI2_get_start_time(t) bind(C, name = "BMI2_get_start_time")
        !DEC$ ATTRIBUTES DLLEXPORT :: BMI2_get_start_time
        use m_sysi
        real(c_double), intent(out) :: t

        !t = real(dlwqd%otime,8) + real(itstrt,8) / real(dlwqd%tscale,8)
        t = itstrt

        write (88, *) 'Start time: ', t
        flush (88)

        BMI2_get_start_time = 0

    end function BMI2_get_start_time

    ! BMI2_get_end_time --
    !>    Return the end time of the calculation
    !!
    !!    Always returns 0
    !
    integer function BMI2_get_end_time(t) bind(C, name = "BMI2_get_end_time")
        !DEC$ ATTRIBUTES DLLEXPORT :: BMI2_get_end_time
        use m_sysi
        real(c_double), intent(out) :: t

        t = itstop

        write (88, *) 'Stop time: ', t
        flush (88)

        BMI2_get_end_time = 0

    end function BMI2_get_end_time

    ! BMI2_get_time_step --
    !>    Return the end time of the calculation
    !!
    !!    Always returns 0
    !
    integer function BMI2_get_time_step(dt) bind(C, name = "BMI2_get_time_step")
        !DEC$ ATTRIBUTES DLLEXPORT ::BMI2_get_time_step
        use m_sysi
        real(c_double), intent(out) :: dt

        dt = idt

        write (88, *) 'Time step: ', dt
        flush (88)

        BMI2_get_time_step = 0

    end function BMI2_get_time_step

    ! BMI2_get_current_time --
    !>    Return the current time in the calculation
    !!
    !!    Always returns 0
    !
    integer function BMI2_get_current_time(t) bind(C, name = "BMI2_get_current_time")
        !DEC$ ATTRIBUTES DLLEXPORT :: BMI2_get_current_time
        use m_sysi
        real(c_double), intent(out) :: t

        integer(kind = int_wp) :: current

        t = dlwqd%itime

        write (88, *) 'Current time: ', t, dlwqd%otime, dlwqd%itime, dlwqd%tscale
        flush (88)

        BMI2_get_current_time = 0

    end function BMI2_get_current_time

    ! get_var --
    !>    Thin layer to accomdate DIMR and BMI 2.0 - calls get_var_ptr, which is a BMI function
    !
    subroutine get_var(c_key, xptr) bind(C, name = "get_var")
        !DEC$ ATTRIBUTES DLLEXPORT :: get_var

        character(kind = c_char), intent(in) :: c_key(MAXSTRLEN)
        type(c_ptr), intent(inout) :: xptr

        integer(kind = int_wp) :: rc

        rc = get_value_ptr(c_key, xptr)

    end subroutine get_var

    ! get_value_ptr --
    !>    The get_value_ptr function returns a pointer to the actual variable in the model component,
    !!    so that the caller can read the current or set a new value.
    !!    The consequence is that we do not know whether a new value has been set or not.
    !!    Also noteworthy: DIMR only accepts double-precision numbers.
    !!
    !!    Returns 0 on success, 1 if the key was not recognised or an index was out of scope.
    !
    !     Note: defined by BMI 2.0
    !

    integer function get_value_ptr(c_key, xptr) bind(C, name = "get_value_ptr")
        !DEC$ ATTRIBUTES DLLEXPORT :: get_var

        use m_sysn
        use m_sysa

        character(kind = c_char), intent(in) :: c_key(MAXSTRLEN)
        type(c_ptr), intent(inout) :: xptr

        character(MAXSTRLEN) :: key_given

        integer(kind = int_wp) :: idx

        get_value_ptr = 0  ! Assume everyting will be okay

        write (88, *) 'Get_var ...'
        flush (88)

        key_given = char_array_to_string(c_key)

        write (88, *) 'Get_var: ', trim(key_given)
        flush (88)

        !
        ! * Format of the key:
        !   direction|category|item-name-or-number|substance-or-parameter-name
        ! * The part "direction" is meant to indicate whether the data are sent to DELWAQ
        !   or are to be sent from DELWAQ: TODLWQ and FROMDLWQ
        ! * Categories: BOUND, WASTE, CONST, OBSRV, SEGMN, MODEL
        ! * item-name-or-number: name of the waste load and the like or segment number, could also be "*"
        ! * substance-or-parameter-name: name or "*"
        !
        if (.not. allocated(connection)) then
            allocate(connection(0))
        endif

        idx = key_index(key_given, connection)

        if (idx <= 0) then
            call split_key(key_given, connection, idx)

            !
            ! If the connection string was not recognised or contained invalid information,
            ! notify the caller.
            !
            if (idx <= 0) then
                xptr = c_null_ptr
                get_value_ptr = 1
                return
            end if
        end if

        !
        ! If the connection is outgoing, copy the current value into the pointer,
        ! else leave it to the update routine.
        !
        if (.not. connection(idx)%incoming) then
            connection(idx)%p_value = dlwqd%buffer%rbuf(connection(idx)%buffer_idx)
        end if

        xptr = c_loc(connection(idx)%p_value)

        write (88, *) 'Get_var done ', idx, connection(idx)%p_value
        flush (88)

    contains

        ! key_index --
        !     Find the key in the registered connections and return its index
        !
        ! Result:
        !     Index into the connection array or 0 if not found
        !
        ! Note:
        !     Internal to get_var_ptr
        !
        integer function key_index(key_name, connection)
            character(len = *), intent(in) :: key_name    !< Connection key to find
            type(connection_data), dimension(:), intent(in) :: connection  !< Array storing the connection information

            integer(kind = int_wp) :: i

            key_index = 0
            do i = 1, size(connection)
                if (connection(i)%exchange_name == key_name) then
                    key_index = i
                    exit
                end if
            end do
        end function key_index

        ! split_key --
        !     Split a (new) connection key into its components and
        !     add to the array
        !
        ! Arguments:
        !     key_name           Connection key to find
        !     connection         Array storing the connection information
        !     newidx             Index into the connection array to be used
        !                        for the new connection
        ! Result:
        !     Index into the connection array and updated array
        !
        ! Note:
        !     Internal to get_var_ptr
        !
        subroutine split_key(key_name, connection, newidx)
            character(len = *), intent(in) :: key_name     !< Connection key to find
            type(connection_data), dimension(:), allocatable, intent(inout) :: connection   !< Array storing the connection information

            integer(kind = int_wp), intent(out) :: newidx        !< Index into the connection array to be used
            !! for the new connection

            type(connection_data) :: new_connection
            character(len = len(key_name)) :: copy_key, component, item_name, subst_param
            integer(kind = int_wp) :: i, k
            integer(kind = int_wp) :: iseg, isys, monidx, conidx
            integer(kind = int_wp) :: ierr
            logical :: error

            ! Analyse the connection string
            newidx = 0
            isys = -999 ! Just to be safe

            if (key_name(1:10) /= 'TO_DELWAQ|' .and. key_name(1:12) /= 'FROM_DELWAQ|') then
                return
            end if

            new_connection%exchange_name = key_name
            new_connection%incoming = key_name(1:10) == 'TO_DELWAQ|' ! Otherwise automatically outgoing!

            !
            ! We require four parts, separated by a vertical bar ("|")
            ! By requiring the position to be larger than 1 and removing spaces on the left
            ! we ensure that there is something between the bars.
            !
            error = .false.
            copy_key = adjustl(key_name)
            do i = 1, 3
                k = index(copy_key, '|')
                if (k > 1) then
                    copy_key = adjustl(copy_key(k + 1:))
                else
                    if (index(key_name, '|CONST|') == 0) then
                        error = .true.
                    endif
                    exit
                end if
            end do

            if (copy_key == ' ') then
                error = .true.
            end if

            if (error) then
                return
            end if

            copy_key = key_name

            !
            ! Strip off the first part
            !
            k = index(copy_key, '|')
            copy_key = copy_key(k + 1:)

            !
            ! The category
            !
            k = index(copy_key, '|')
            component = copy_key(1:k - 1)
            copy_key = copy_key(k + 1:)

            !
            ! The item name or index
            !
            k = index(copy_key, '|')
            item_name = copy_key(1:k - 1)
            copy_key = copy_key(k + 1:)

            !
            ! The substance name or parameter name
            ! (for constants this is the previous part of the connection string)
            !
            if (copy_key == ' ') then
                if (index(key_name, '|CONST|') /= 0) then
                    subst_param = item_name
                else
                    return
                endif
            else
                subst_param = copy_key
            endif

            !
            ! Create the connection
            !
            selection : &
                    block
                select case (component)
                case ('BOUND')
                    new_connection%category = category_boundary
                    ! TODO = boundary cell - to be worked out
                case ('WASTE')
                    new_connection%category = category_wasteload
                    !
                    ! Index would be index in the list, name is also allowed
                    !
                    read (item_name, *, iostat = ierr) iseg
                    if (ierr == 0) then
                        if (iseg < 1 .or. iseg > noseg) then
                            newidx = 0
                            exit selection
                        end if
                    else
                        iseg = index_in_array(item_name(:len(load_name)), load_name)

                        if (iseg < 1) then
                            newidx = 0
                            exit selection
                        end if
                    end if

                    !
                    ! The substance/parameter may be "FLOW": handle this carefully.
                    !
                    if (subst_param == 'FLOW') then
                        isys = 1
                    else
                        isys = index_in_array(subst_param(:len(substance_name)), substance_name)
                        if (isys <= 0) then
                            newidx = 0
                            exit selection
                        end if

                        isys = isys + 1
                    end if

                    !
                    ! If the waste load is to be set, we set the flow, hence store the index
                    ! of the waste load, not that of the individual value
                    !
                    if (new_connection%incoming) then
                        new_connection%buffer_idx = iseg
                    else
                        new_connection%buffer_idx = iwste - 1 + isys + (iseg - 1) * nowst * (notot + 1)
                    end if

                case ('SEGMN')
                    new_connection%category = category_segment
                    !
                    ! Segments are indicated by segment number. We also need the substance index
                    !
                    read (item_name, *, iostat = ierr) iseg
                    if (ierr == 0) then
                        if (iseg < 1 .or. iseg > noseg) then
                            newidx = 0
                            exit selection
                        end if
                    else
                        newidx = 1
                        exit selection
                    end if

                    isys = index_in_array(subst_param(:20), substance_name(:notot))
                    if (isys <= 0) then
                        newidx = 0
                        exit selection
                    end if

                    new_connection%buffer_idx = iconc - 1 + isys + (iseg - 1) * notot

                case('OBSRV')
                    new_connection%category = category_monitorpoint
                    !
                    ! Observation points
                    ! Index would be index in the list, name is also allowed
                    !
                    read(item_name, *, iostat = ierr) monidx
                    if (ierr == 0) then
                        if (monidx < 1 .or. monidx > size(monitor_name)) then
                            newidx = 0
                            exit selection
                        endif
                    else
                        monidx = index_in_array(item_name(:len(monitor_name)), monitor_name)
                        if (monidx < 1) then
                            newidx = 0
                            exit selection
                        endif
                    endif

                    iseg = monitor_cell(monidx)

                    isys = index_in_array(subst_param(:20), substance_name(:notot))
                    if (isys <= 0) then
                        newidx = 0
                        exit selection
                    endif
                    new_connection%buffer_idx = iconc - 1 + isys + (iseg - 1) * notot

                case('CONST')

                    ! NOTE: parameters/segment functions not supported yet

                    new_connection%category = category_procparam
                    !
                    ! Constant (timeseries) or parameter (segment function)
                    ! Index would be index in the list, name is also allowed
                    !

                    conidx = index_in_array(subst_param(:len(procparam_const)), procparam_const) ! procparam_const is the name of constants.
                    if (conidx < 1) then
                        newidx = 0
                        exit selection
                    endif
                    new_connection%buffer_idx = icons - 1 + conidx

                case ('HYDRO')
                    new_connection%category = category_hydrodynamics
                    ! TODO - get arrays like volume etc.
                end select

                ! Fill in the details
                ! Implementation note:
                ! The component p_value must be a pointer, as otherwise the automatic reallocation
                ! below would result in new memory locations that we cannot correct.
                ! Alternative implementations are possible, but short of a fixed size, they all
                ! involve pointers as far as I (AM) can tell. This seems the simplest one.
                !
                new_connection%system_idx = isys
                allocate(new_connection%p_value)

                connection = [connection, new_connection]
                newidx = size(connection)

            end block &
                    selection

        end subroutine split_key

    end function get_value_ptr

    ! update_from_incoming_data --
    !     Update the internal data in case we have incoming data
    !
    ! Note:
    !     Used by update()
    !
    subroutine update_from_incoming_data(connection)
        type(connection_data), dimension(:), intent(in) :: connection !< Information about the connections to
        !! external components

        integer(kind = int_wp) :: idx, isys, iwaste

        !
        ! Handle waste loads
        !
        if (allocated(wasteloads)) then
            do idx = 1, size(connection)
                if (connection(idx)%category == category_wasteload .and. connection(idx)%incoming) then
                    iwaste = connection(idx)%buffer_idx
                    if (.not. allocated(wasteloads(iwaste)%set_factor)) then
                        allocate(wasteloads(iwaste)%set_factor(1:size(substance_name) + 1))
                        wasteloads(iwaste)%set_factor = 1.0
                    endif

                    isys = connection(idx)%system_idx
                    wasteloads(iwaste)%set_factor(isys) = connection(idx)%p_value
                endif
            enddo
        endif

        !
        ! Handle constants (process parameters)
        !
        do idx = 1, size(connection)
            if (connection(idx)%category == category_procparam .and. connection(idx)%incoming) then
                dlwqd%buffer%rbuf(connection(idx)%buffer_idx) = connection(idx)%p_value
            endif
        enddo

    end subroutine update_from_incoming_data

end module bmi
