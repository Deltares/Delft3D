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

!> Module containing the supported external api calls
!! Used by adapters like BMI
module m_waq_external_access_layer

    ! system modules
    use iso_c_binding
    use iso_c_utils
    use m_waq_precision
    use m_getidentification

    ! delwaq data modules
    use delwaq2_global_data
    use delwaq_loads

    ! delwaq logic modules
    use m_delwaq1
    use m_delwaq2_main

    ! utility modules
    use m_string_utils

    ! logging modules
    use m_logger
    use m_logger_factory
    use m_log_level
    use m_logger_type

    ! sub modules
    use m_connection_manager
    use m_connection_data

    implicit none

    private

    ! time parameters
    public :: ext_get_start_time, ext_get_end_time, ext_get_time_step, ext_get_current_time

    ! control flow
    public :: ext_initialize, ext_update_until, ext_finalize

    ! data exchange
    public :: ext_set_var, ext_get_version_string, ext_get_attribute, ext_get_value_ptr, ext_get_var_shape

    ! Define some global constants
    character(*), parameter, public :: EXT_PREFIX = "WAQ"

    integer(c_int), parameter, public :: EXT_MAXSTRLEN = 1024
    integer(c_int), parameter, public :: EXT_MAXDIMS = 6

    type(connection_manager), public :: connections = connection_manager() !< Controls the exchanges between Delwaq and other external components
    class(logger), allocatable, save :: log !< logger to log towards

    logical, save                    :: init_mass = .false.

    integer, save                    :: luvol, luflow, luarea

contains

    !> The set_var function lets the caller set a variable within DELWAQ.
    function ext_set_var(c_key, xptr) result(error_code)
        use m_connection_data_mapping
        use m_connection_parser
        character(kind=c_char), intent(in) :: c_key(EXT_MAXSTRLEN)  !< Incoming string, determines the variable to be set
        type(c_ptr), value, intent(in)     :: xptr                  !< C-pointer to the actual value to be picked up by DELWAQ
        integer(c_int)                     :: error_code            !< Always returns zero - there is no error condition

        type(connection_data), pointer                :: con_data
        real(dp), dimension(:), pointer               :: incoming_data
        character(EXT_MAXSTRLEN)                      :: key_given
        logical                                       :: first_only = .true.

        call init_logger()
        call log%log_debug("ext_set_var started")

        ! Get the key and determine the required action
        key_given = char_array_to_string(c_key)

        if ( key_given(1:1) /= '-' ) then
            !
            ! Only continue processing if the key does not contain "_shape" - that
            ! is the way DIMR passes on the shape information
            !
            if ( index(key_given,'_shape') == 0 ) then

                ! Handle the time frame differently
                if ( key_given == 'TO_DELWAQ|HYDRO|timeframe' ) then
                    call set_coupling_timeframe( xptr, first_only )
                    first_only = .false.
                else

                    ! Get the connection
                    con_data => connections%get_or_create_connection(key_given)

                    if ( associated(con_data) ) then
                        !
                        ! Copy the data for later use
                        !
                        if ( size(con_data%p_value) > 1 ) then
                            call c_f_pointer(xptr, incoming_data, [size(con_data%p_value)])
                            con_data%p_value = incoming_data
                        endif
                    endif
                endif
            endif
        else
            ! Apparently, we are dealing with a key/value pair
            call extend_arg_list( c_key, xptr )
        endif

        error_code = 0
        call log%log_debug("ext_set_var ended")
    end function ext_set_var

    !> Auxiliary subroutine for extending the set of command-line arguments known to DELWAQ
    subroutine extend_arg_list(c_key, xptr)
        character(kind=c_char), intent(in) :: c_key(EXT_MAXSTRLEN)  !< Incoming string, determines the variable to be set
        type(c_ptr), value, intent(in)     :: xptr                  !< C-pointer to the actual value to be picked up by DELWAQ

        character(kind=c_char), dimension(:), pointer :: c_value => null()
        character(EXT_MAXSTRLEN)           :: key_given
        character(EXT_MAXSTRLEN)           :: value_given
        integer(kind=int_wp)               :: argc
        integer(kind=int_wp)               :: argnew
        integer(kind=int_wp)               :: iarg
        integer(kind=int_wp)               :: i

        ! Store the key and value
        key_given = char_array_to_string(c_key)
        call c_f_pointer(xptr, c_value, [EXT_MAXSTRLEN])

        call log%log_debug("Set_var: key = "//key_given)

        value_given = " "
        if (associated(c_value)) then
            do i = 1, EXT_MAXSTRLEN
                if (c_value(i) == c_null_char) exit
                value_given(i:i) = c_value(i)
            end do
        end if

        call log%log_debug("Set_var: value = "//value_given)
        write(88,*) "Set_var: key -- value = ", trim(key_given), ' -- ', trim(value_given)

        argnew = 2
        if (value_given(1:1) == ' ') argnew = 1
        if (key_given(1:1) == ' ') argnew = 0
        !
        if (argnew > 0) then
            ! Handle the argument -onlinehydrodynamics here - it will remain
            ! unknown to DELWAQ otherwise
            if (key_given == '-onlinehydrodynamics') then
                dlwqd%online_hydrodynamics = .true.
            else
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
        end if
    end subroutine extend_arg_List

    ! Control

    !> The initialize() function accepts a string argument that
    !! gives the name (and path) of its "main input file", called
    !! a configuration file. This function should perform all tasks
    !! that are to take place before entering the model's time loop.
    function ext_initialize(c_config_file) result(error_code)
        use m_actions
        use m_timer_variables
        use m_cli_utils, only: store_command_arguments

        character(kind=c_char), intent(in) :: c_config_file(EXT_MAXSTRLEN)  !< Name of the DELWAQ input file
        integer(c_int) :: error_code !< error code (0 if successful, 1 otherwise)

        character(len=strlen(c_config_file)) :: runid_given
        integer(kind=int_wp) :: argc
        integer(kind=int_wp) :: iarg

        call init_logger()
        call log%log_debug("ext_initialize started")

        ! Store the name
        runid_given = char_array_to_string(c_config_file)

        call log%log_debug("Initialise: "//runid_given)
        write(88,*) "Ext_initialize: ", trim(runid_given)

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
        allocate (argv(argc + 1))
        argv(1) = runid_given
        do iarg = 1, argc
            argv(iarg + 1) = argv_tmp(iarg)
        end do

        if (delwaq1(argv)) then
            init_mass = .true.
            call delwaq2_global_data_initialize(runid_given)
            call dlwqmain(ACTION_INITIALISATION, dlwqd)
            call delwaq2_global_data_copy(dlwqd)
            error_code = 0
        else
            error_code = 1
        end if

        call log%log_debug("Initialise: "// &
                           real_to_str(dlwqd%otime)//'--'// &
                           int_to_str(dlwqd%itime)//'--'// &
                           real_to_str(dlwqd%tscale)//'--'// &
                           int_to_str(idt))

        call log%log_debug("ext_initialize ended")

        !! Write out what we get ...
        open( newunit = luvol,  file = 'volumes.bin', access = 'stream', status = 'new' )
        open( newunit = luflow, file = 'flows.bin',   access = 'stream', status = 'new' )
        open( newunit = luarea, file = 'areas.bin',   access = 'stream', status = 'new' )

        write(88,*) "ext_initialize ended"

    end function ext_initialize

    !> The subroutine get_version_string() returns the version of DELWAQ
    !! that is being used.
    function ext_get_version_string() result(c_version_string)
        character(kind=c_char) :: c_version_string(EXT_MAXSTRLEN)

        character(len=EXT_MAXSTRLEN) :: name
        character(len=120) :: identification_text

        call init_logger()
        call log%log_debug("ext_get_version_string started")

        call getidentification(identification_text)
        name = trim(identification_text)
        c_version_string = string_to_char_array(trim(name))

        call log%log_debug("ext_get_version_string ended")
    end function ext_get_version_string

    !> Returns a static attribute (i.e. an attribute that does not change
    !! from one model application to the next) of the model (as a string)
    !! When passed any attribute name from the following list:
    !! * model_name
    !! * version      (e.g. 2.0.1)
    !! * author_name
    !! * grid_type
    !! * time_step_type
    !! * step_method   (explicit, implicit, semi_implicit, iterative)
    function ext_get_attribute(c_att_name) result(c_att_value)
        use delwaq_version_module
        character(kind=c_char), intent(in) :: c_att_name(EXT_MAXSTRLEN)  !< Attribute name as C-delimited character string.
        character(kind=c_char) :: c_att_value(EXT_MAXSTRLEN) !< Returned attribute value as C-delimited character string.

        character(len=strlen(c_att_name)) :: att_name
        character(len=EXT_MAXSTRLEN) :: att_value

        call init_logger()
        call log%log_debug("ext_get_attribute started")

        ! Store the name
        att_name = char_array_to_string(c_att_name)

        call log%log_debug('Get_attribute: '//att_name)

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

        call log%log_debug('Get_attribute done -- '//trim(att_value))
        call log%log_debug("ext_get_attribute ended")
    end function ext_get_attribute

    !> Run the model calculation up to a given model time
    !! Always returns 0
    function ext_update_until(tupdate) result(error_code)
        use messagehandling
        use m_waq_external_access_layer_utils
        use m_actions
        use m_timer_variables

        real(c_double), value, intent(in) :: tupdate   !< Time until which the calculation is to be run.
        integer :: error_code !< return error code

        !! May involve one or more internal timesteps
        integer(kind=int_wp) :: update_steps, step

        call init_logger()
        call log%log_debug("ext_update_until started")
        write(88,*) 'ext_update_until: ', dlwqd%itime, tupdate

        update_steps = nint(tupdate - dlwqd%itime) / idt

        call log%log_debug("Update "// &
                           c_real_to_str(tupdate)// &
                           int_to_str(update_steps)// &
                           int_to_str(dlwqd%itime))

        !
        ! Only run if the coupling from the hydrodynamic side has started.
        ! Otherwise it makes no sense.
        !
        if ( update_steps <= 0 ) then
            call log%log_debug("ext_update_until skipped")
            return
        endif

        if (intsrt == 2) then
            ! Correct update_steps for delwaq scheme 2, which does a double time step every call
            update_steps = (update_steps + 1) / 2
        end if

        !
        ! Update the hydrodynamic time information, if in online mode
        !
        if ( dlwqd%online_hydrodynamics ) then
            call set_coupling_timeframe( c_null_ptr, .false. )
        endif

        call update_from_incoming_data(connections)
        write(88,*) 'update_until: update_from_incoming_data', dlwqd%buffer%rbuf(iflow+100-1:iflow+109)

        !! Write out what we get ...
        write( luvol  ) dlwqd%itime, dlwqd%buffer%rbuf(ivol:ivol+num_cells-1)
        write( luflow ) dlwqd%itime, dlwqd%buffer%rbuf(iflow:iflow+num_exchanges-1)
        write( luarea ) dlwqd%itime, dlwqd%buffer%rbuf(iarea:iarea+num_exchanges-1)

        !
        ! If we are using the online coupling mode, then this is the moment that
        ! everything is ready from the side of the hydrodydnamic module.
        ! We re-calculate the masses from the initial concentrations and the
        ! volumes that we now know from the incoming data.
        !
        ! If we are using the standalone mode, the volumes were already known
        ! and this step is not needed.
        !
        if ( init_mass .and. dlwqd%online_hydrodynamics ) then
            write(88,*) 'ext_update_until: recalculate mass'
            init_mass = .false.
            call recalculate_masses
        endif

        write(88,*) 'ext_update_until: ', dlwqd%buffer%rbuf(ivol+2000-1), dlwqd%buffer%rbuf(ivol+3000-1)

        do step = 1, update_steps
            call dlwqmain(ACTION_SINGLESTEP, dlwqd)
        end do
        error_code = 0

        call log%log_debug("ext_update_until ended")

    end function ext_update_until

    !> Finish the model calculation (close files, clean up, etc.)
    function ext_finalize() result(error_code)
        use m_actions
        integer :: error_code

        call init_logger()
        call log%log_debug("ext_finalize started")

        call log%log_debug("Finalise "//int_to_str(dlwqd%itime))

        call dlwqmain(ACTION_SINGLESTEP, dlwqd)
        call dlwqmain(ACTION_FINALISATION, dlwqd)
        call delwaq2_global_data_finalize()

        error_code = 0

        call log%log_debug("ext_finalize ended")

    end function ext_finalize

    !> Return the start time of the calculation
    function ext_get_start_time(t) result(error_code)
        use m_timer_variables
        real(c_double), intent(out) :: t
        integer :: error_code

        call init_logger()
        call log%log_debug("ext_get_start_time started")

        t = itstrt

        call log%log_debug("Start time: "//c_real_to_str(t))

        error_code = 0
        call log%log_debug("ext_get_start_time ended")
    end function ext_get_start_time

    !> Return the end time of the calculation
    function ext_get_end_time(t) result(error_code)
        use m_timer_variables
        real(c_double), intent(out) :: t
        integer :: error_code

        call init_logger()
        call log%log_debug("ext_get_end_time started")

        t = itstop

        call log%log_debug("Stop time: "//c_real_to_str(t))

        error_code = 0
        call log%log_debug("ext_get_end_time ended")
    end function ext_get_end_time

    !> Return the end time of the calculation
    function ext_get_time_step(dt) result(error_code)
        use m_timer_variables
        real(c_double), intent(out) :: dt
        integer :: error_code

        call init_logger()
        call log%log_debug("ext_get_time_step started")

        dt = idt

        call log%log_debug("Time step: "//c_real_to_str(dt))

        error_code = 0
        call log%log_debug("ext_get_time_step ended")
    end function ext_get_time_step

    !> Return the current time in the calculation
    function ext_get_current_time(t) result(error_code)
        use m_timer_variables
        real(c_double), intent(out) :: t
        integer :: error_code

        integer(kind=int_wp) :: current

        call init_logger()
        call log%log_debug("ext_get_current_time started")

        t = dlwqd%itime

        call log%log_debug("Current time: "// &
                           c_real_to_str(t)// &
                           real_to_str(dlwqd%otime)// &
                           int_to_str(dlwqd%itime)// &
                           real_to_str(dlwqd%tscale))
        error_code = 0
        call log%log_debug("ext_get_current_time ended")
    end function ext_get_current_time

    !> The get_value_ptr function returns a pointer to the actual variable in the model component,
    !! so that the caller can read the current or set a new value.
    !! The consequence is that we do not know whether a new value has been set or not.
    !! Also noteworthy: DIMR only accepts double-precision numbers.
    function ext_get_value_ptr(c_key, xptr) result(error_code)
        use m_connection_parser
        use m_connection_data_mapping

        character(kind=c_char), intent(in) :: c_key(EXT_MAXSTRLEN) !< Incoming string, determines the variable to be set
        type(c_ptr), intent(inout) :: xptr !< Pointer to the actual value to be picked up by DELWAQ
        integer :: error_code !< 0 on success, 1 if the key was not recognised or an index was out of scope

        type(connection_data), pointer :: con_data !< current connection data object
        type(connection_data), allocatable :: new_con_data
        character(EXT_MAXSTRLEN) :: key_given !< key as string
        integer                  :: i, j

        call init_logger()
        call log%log_debug("ext_get_current_time started")

        error_code = 0  ! Assume everyting will be okay

        key_given = char_array_to_string(c_key)

        call log%log_debug("Get_var: "//trim(key_given))

        ! * Format of the key:
        !   direction|category|item-name-or-number|substance-or-parameter-name
        ! * The part "direction" is meant to indicate whether the data are sent to DELWAQ
        !   or are to be sent from DELWAQ: TODLWQ and FROMDLWQ
        ! * Categories: BOUND, WASTE, CONST, OBSRV, SEGMN, MODEL
        ! * item-name-or-number: name of the waste load and the like or segment number, could also be "*"
        ! * substance-or-parameter-name: name or "*"

        con_data => connections%get_connection_by_exchange_name(key_given)
        if (.not. associated(con_data)) then
            new_con_data = parse_connection_string(key_given)

            if (allocated(new_con_data)) then
                call set_connection_data(new_con_data)

                ! use added connection instance
                con_data => connections%add_connection(new_con_data)
            end if
        end if

        ! If the connection is outgoing, copy the current value into the pointer,
        ! else leave it to the update routine.
        if (.not. con_data%incoming) then
            do i = 1,size(con_data%p_value)
                j = con_data%buffer_idx + (i-1) * con_data%stride
                con_data%p_value(i) = dlwqd%buffer%rbuf(j)
            enddo
        end if

        xptr = c_loc(con_data%p_value)

        call log%log_debug("Get_var done "//int_to_str(con_data%buffer_idx)//real_to_str(con_data%p_value(1)))
        call log%log_debug("ext_get_current_time ended")
    end function ext_get_value_ptr

    integer function ext_get_var_shape(c_var_name, shape)
        character(kind=c_char), intent(in)    :: c_var_name(EXT_MAXSTRLEN)
        integer(c_int)        , intent(inout) :: shape(EXT_MAXDIMS)

        character(len=strlen(c_var_name))     :: var_name
        type(connection_data), pointer        :: con_data !< current connection data object

        var_name = char_array_to_string(c_var_name)
        shape    = 0


        con_data => connections%get_connection_by_exchange_name(var_name)

        shape(1) = size(con_data%p_value)

        ext_get_var_shape = 0
    end function ext_get_var_shape

    subroutine init_logger()
        if (.not. allocated(log)) then
            log = create_logger(file, info_level, "delwaq_coupling.log")
        end if
    end subroutine init_logger

    function int_to_str(int_value) result(str_value)
        integer(kind=int_wp) :: int_value !< value to convert
        character(:), allocatable :: str_value !< converted int value

        character(30) :: str_buffer

        write (str_buffer, *) int_value
        str_value = trim(str_buffer)
    end function int_to_str

    function real_to_str(real_value) result(str_value)
        real(kind=dp) :: real_value !< value to convert
        character(:), allocatable :: str_value !< converted real value

        character(30) :: str_buffer

        write (str_buffer, *) real_value
        str_value = trim(str_buffer)
    end function real_to_str

    function c_real_to_str(real_value) result(str_value)
        real(c_double) :: real_value !< value to convert
        character(:), allocatable :: str_value !< converted real value

        character(30) :: str_buffer

        write (str_buffer, *) real_value
        str_value = trim(str_buffer)
    end function c_real_to_str

    !> Adjust the computational time frame to match that of
    !! the output provided by the hydrodynamic module we
    !! are connecting to. Also update the information on the
    !! hydrodynamic data we are holding - volumes only.
    !!
    subroutine set_coupling_timeframe( xptr, first_only )
        use m_timer_variables
        type(c_ptr), value, intent(in)  :: xptr                  !< C-pointer to the actual value to be picked up by DELWAQ
        logical, intent(in)             :: first_only            !< Indicate whether some specific initial steps should be taken

        real(dp), dimension(:), pointer :: timeframe

        integer, parameter              :: ILUN  = 7             ! NOTE: this is a fixed LU-number, see DLWQT4.
        integer                         :: iColl
        integer, save                   :: coupling_step         ! Time step for coupling in seconds (!)

        ! TODO: correct the date for differences in reference date/time

        if ( first_only ) then
            call c_f_pointer(xptr, timeframe, [4])

            itstrt        = int( timeframe(1) )
            itstop        = int( timeframe(2) )
            coupling_step = int( timeframe(3) )

            dlwqd%itime   = int( timeframe(1) )
        else
            !
            ! Correct the hydrodynamic information - volumes only
            !
            if ( associated(dlwqd%CollColl%FileUseDefColls) ) then
                iColl = FileUseDefCollCollFind (dlwqd%CollColl, ILUN)

                if ( iColl > 0 ) then
                    dlwqd%CollColl%FileUseDefColls(iColl)%istart = dlwqd%itime
                    dlwqd%CollColl%FileUseDefColls(iColl)%istop  = dlwqd%itime + coupling_step
                endif
            endif
        endif

    end subroutine set_coupling_timeframe

    !> Recalculate the masses, as the initial volumes are now known
    !
    !  Note: this could be an opportunity for a smart use of ASSOCIATE
    !
    subroutine recalculate_masses
        use m_real_array_indices, only: imass, iconc
        integer       :: cell_i, i1
        real(real_wp) :: volume

        do cell_i = 0, dlwqd%nosss - 1
            volume = dlwqd%buffer%rbuf(ivol + cell_i)

            do i1 = cell_i * num_substances_total, cell_i * num_substances_total + num_substances_transported - 1
                dlwqd%buffer%rbuf(imass + i1) = dlwqd%buffer%rbuf(iconc + i1) * volume
                if ( cell_i == 2000 ) then
                    write(88,*) 'Volume, Concentration, Mass: ', volume, dlwqd%buffer%rbuf(imass + i1), dlwqd%buffer%rbuf(iconc + i1)
                endif
            enddo
        enddo
    end subroutine recalculate_masses

end module m_waq_external_access_layer
