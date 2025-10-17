module dsle
  use, intrinsic :: iso_c_binding, only : c_char, c_double, c_f_pointer, c_int, c_ptr, c_size_t
  implicit none

  type, bind(C) :: dsle_param_t
    real(c_double) :: lock_length
    real(c_double) :: lock_width
    real(c_double) :: lock_bottom
    real(c_double) :: num_cycles
    real(c_double) :: door_time_to_open
    real(c_double) :: leveling_time
    real(c_double) :: calibration_coefficient
    real(c_double) :: symmetry_coefficient
    real(c_double) :: ship_volume_sea_to_lake
    real(c_double) :: ship_volume_lake_to_sea
    real(c_double) :: salinity_lock
    real(c_double) :: head_sea
    real(c_double) :: salinity_sea
    real(c_double) :: temperature_sea
    real(c_double) :: head_lake
    real(c_double) :: salinity_lake
    real(c_double) :: temperature_lake
    real(c_double) :: flushing_discharge_high_tide
    real(c_double) :: flushing_discharge_low_tide
    real(c_double) :: density_current_factor_sea
    real(c_double) :: density_current_factor_lake
    real(c_double) :: distance_door_bubble_screen_sea
    real(c_double) :: distance_door_bubble_screen_lake
    real(c_double) :: sill_height_sea
    real(c_double) :: sill_height_lake
    real(c_double) :: rtol
    real(c_double) :: atol
  end type dsle_param_t

  type, bind(C) :: dsle_results_t
    real(c_double) :: mass_transport_lake
    real(c_double) :: salt_load_lake
    real(c_double) :: discharge_from_lake
    real(c_double) :: discharge_to_lake
    real(c_double) :: salinity_to_lake

    real(c_double) :: mass_transport_sea
    real(c_double) :: salt_load_sea
    real(c_double) :: discharge_from_sea
    real(c_double) :: discharge_to_sea
    real(c_double) :: salinity_to_sea
  end type dsle_results_t

  type, bind(C) :: dsle_phase_state_t
    real(c_double) :: salinity_lock
    real(c_double) :: saltmass_lock
    real(c_double) :: head_lock
    real(c_double) :: volume_ship_in_lock
  end type dsle_phase_state_t

  type, bind(C) :: dsle_phase_transports_t
    real(c_double) :: mass_transport_lake
    real(c_double) :: volume_from_lake
    real(c_double) :: volume_to_lake
    real(c_double) :: discharge_from_lake
    real(c_double) :: discharge_to_lake
    real(c_double) :: salinity_to_lake

    real(c_double) :: mass_transport_sea
    real(c_double) :: volume_from_sea
    real(c_double) :: volume_to_sea
    real(c_double) :: discharge_from_sea
    real(c_double) :: discharge_to_sea
    real(c_double) :: salinity_to_sea
  end type dsle_phase_transports_t

  type, bind(C) :: dsle_aux_results_t
    real(c_double) :: z_fraction
    real(c_double) :: dimensionless_door_open_time
    real(c_double) :: volume_to_lake
    real(c_double) :: volume_from_lake
    real(c_double) :: volume_to_sea
    real(c_double) :: volume_from_sea
    real(c_double) :: volume_lock_at_lake
    real(c_double) :: volume_lock_at_sea
    real(c_double) :: t_cycle
    real(c_double) :: t_open
    real(c_double) :: t_open_lake
    real(c_double) :: t_open_sea
    real(c_double) :: salinity_lock_1
    real(c_double) :: salinity_lock_2
    real(c_double) :: salinity_lock_3
    real(c_double) :: salinity_lock_4
    type(dsle_phase_transports_t) :: transports_phase_1
    type(dsle_phase_transports_t) :: transports_phase_2
    type(dsle_phase_transports_t) :: transports_phase_3
    type(dsle_phase_transports_t) :: transports_phase_4
  end type dsle_aux_results_t

  interface
    integer(c_int) function dsle_initialize_state(p, state, salinity_lock, head_lock) bind(C, name='dsle_initialize_state')
      import c_int, dsle_param_t, dsle_phase_state_t, c_double
      type(dsle_param_t), intent(in) :: p
      type(dsle_phase_state_t), intent(inout) :: state
      real(c_double), intent(in), value :: salinity_lock
      real(c_double), intent(in), value :: head_lock
    end function dsle_initialize_state

    integer(c_int) function dsle_step_phase_1(p, t_level, state, results) bind(C, name='dsle_step_phase_1')
      import c_int, dsle_param_t, c_double, dsle_phase_state_t, dsle_phase_transports_t
      type(dsle_param_t), intent(in) :: p
      real(c_double), intent(in), value :: t_level
      type(dsle_phase_state_t), intent(inout) :: state
      type(dsle_phase_transports_t), intent(inout) :: results
      end function dsle_step_phase_1

    integer(c_int) function dsle_step_phase_2(p, t_open_lake, state, results) bind(C, name='dsle_step_phase_2')
      import c_int, dsle_param_t, c_double, dsle_phase_state_t, dsle_phase_transports_t
      type(dsle_param_t), intent(in) :: p
      real(c_double), intent(in), value :: t_open_lake
      type(dsle_phase_state_t), intent(inout) :: state
      type(dsle_phase_transports_t), intent(inout) :: results
    end function dsle_step_phase_2

    integer(c_int) function dsle_step_phase_3(p, t_level, state, results) bind(C, name='dsle_step_phase_3')
      import c_int, dsle_param_t, c_double, dsle_phase_state_t, dsle_phase_transports_t
      type(dsle_param_t), intent(in) :: p
      real(c_double), intent(in), value :: t_level
      type(dsle_phase_state_t), intent(inout) :: state
      type(dsle_phase_transports_t), intent(inout) :: results
    end function dsle_step_phase_3

    integer(c_int) function dsle_step_phase_4(p, t_open_sea, state, results) bind(C, name='dsle_step_phase_4')
      import c_int, dsle_param_t, c_double, dsle_phase_state_t, dsle_phase_transports_t
      type(dsle_param_t), intent(in) :: p
      real(c_double), intent(in), value :: t_open_sea
      type(dsle_phase_state_t), intent(inout) :: state
      type(dsle_phase_transports_t), intent(inout) :: results
    end function dsle_step_phase_4

    integer(c_int) function dsle_step_flush_doors_closed(p, t_flushing, state, results) bind(C, name='dsle_step_flush_doors_closed')
      import c_int, dsle_param_t, c_double, dsle_phase_state_t, dsle_phase_transports_t
      type(dsle_param_t), intent(in) :: p
      real(c_double), intent(in), value :: t_flushing
      type(dsle_phase_state_t), intent(inout) :: state
      type(dsle_phase_transports_t), intent(inout) :: results
    end function dsle_step_flush_doors_closed

    subroutine dsle_param_default(p) bind(C, name='dsle_param_default')
      import dsle_param_t
      type(dsle_param_t), intent(inout) :: p
    end subroutine dsle_param_default

    integer(c_int) function dsle_calc_steady(p, results, aux_results) bind(C, name='dsle_calc_steady')
      import c_int, dsle_param_t, dsle_results_t, dsle_aux_results_t
      type(dsle_param_t), intent(in) :: p
      type(dsle_results_t), intent(inout) :: results
      type(dsle_aux_results_t), intent(inout) :: aux_results
    end function dsle_calc_steady

    type(c_ptr) function dsle_error_msg__raw(code) bind(C, name='dsle_error_msg')
      import c_int, c_ptr
      integer(c_int), intent(in), value :: code
    end function dsle_error_msg__raw

    type(c_ptr) function dsle_version__raw() bind(C, name='dsle_version')
      import c_ptr
    end function dsle_version__raw

    integer(c_size_t) function c_strlen(s) bind(c, name="strlen")
      import c_size_t, c_ptr
      type(c_ptr), intent(in), value :: s
    end function
  end interface

  contains

  ! See https://fortran-lang.discourse.group/t/iso-c-binding-interface-to-a-c-function-returning-a-string/527/14
  function dsle_version() result(str)
    character(:, c_char), allocatable :: str
    type(c_ptr) :: cstr
    integer(c_size_t) :: n

    cstr = dsle_version__raw()
    n = c_strlen(cstr)
    allocate(character(len=n, kind=c_char) :: str)
    block
      character(len=n, kind=c_char), pointer :: s
      call c_f_pointer(cstr, s)  ! Recovers a view of the C string
      str = s                    ! Copies the string contents
    end block
    ! Note that we do not have to free the returned string,
    ! as it was not dynamically allocated.
  end function dsle_version

  function dsle_error_msg(code) result(str)
    integer(c_int), intent(in) :: code
    character(:, c_char), allocatable :: str
    type(c_ptr) :: cstr
    integer(c_size_t) :: n

    cstr = dsle_error_msg__raw(code)
    n = c_strlen(cstr)
    allocate(character(len=n, kind=c_char) :: str)
    block
      character(len=n, kind=c_char), pointer :: s
      call c_f_pointer(cstr, s)  ! Recovers a view of the C string
      str = s                    ! Copies the string contents
    end block
    ! Note that we do not have to free the returned string,
    ! as it was not dynamically allocated.
  end function dsle_error_msg
end module
