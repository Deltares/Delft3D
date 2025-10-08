program test
  use, intrinsic :: iso_c_binding, only : c_int
  use zsf
  implicit none

  ! initialization
  type(zsf_param_t) :: p
  type(zsf_results_t) :: results
  type(zsf_aux_results_t) :: aux_results
  type(zsf_phase_state_t) :: state
  type(zsf_phase_transports_t) :: transports
  integer(c_int) :: err_code

  call zsf_param_default(p)
  p%lock_length = 240.0
  p%lock_width = 12.0
  p%lock_bottom = -4.0
  p%num_cycles = 24.0
  p%door_time_to_open = 300.0
  p%leveling_time = 300.0
  p%calibration_coefficient = 1.0
  p%symmetry_coefficient = 1.0
  p%ship_volume_sea_to_lake = 0.0
  p%ship_volume_lake_to_sea = 0.0
  p%head_sea = 0.0
  p%salinity_sea = 25.0
  p%temperature_sea = 15.0
  p%head_lake = 0.0
  p%salinity_lake = 5.0
  p%temperature_lake = 15.0
  p%flushing_discharge_high_tide = 0.0
  p%flushing_discharge_low_tide = 0.0
  p%density_current_factor_sea = 1.0
  p%density_current_factor_lake = 1.0

  ! Test is steady state works
  err_code = zsf_calc_steady(p, results, aux_results)

  if (err_code > 0) then
    write(*, *) 'zsf_calc_steady failed'
    write(*, *) zsf_error_msg(err_code)
    call exit(1)
  endif

  ! Salt load should be about -34.315
  write(*, *) 'Steady salt load: ', results%salt_load_lake

  if (results%salt_load_lake < -34.4 .or. results%salt_load_lake > -34.2) then
    write(*, *) 'zsf_calc_steady did not give correct results'
    call exit(1)
  endif

  ! Test is phase-wise calculation works
  p%lock_length = 148.0
  p%lock_width = 14.0
  p%lock_bottom = -4.4
  p%head_lake = 0.0
  p%salinity_lake = 5.0
  p%temperature_lake = 15.0
  p%head_sea = 2.0
  p%salinity_sea = 25.0
  p%temperature_sea = 15.0
  p%ship_volume_sea_to_lake = 1000.0
  p%ship_volume_lake_to_sea = 1000.0

  err_code = zsf_initialize_state(p, state, 15.0, 0.0)
  if (err_code > 0) then
    write(*, *) 'zsf_initialize_state failed'
    write(*, *) zsf_error_msg(err_code)
    call exit(1)
  endif

  err_code = zsf_step_phase_1(p, 300.0, state, transports)
  if (err_code > 0) then
    write(*, *) 'zsf_step_phase_1 failed'
    write(*, *) zsf_error_msg(err_code)
    call exit(1)
  endif

  err_code = zsf_step_phase_2(p, 840.0, state, transports)
  if (err_code > 0) then
    write(*, *) 'zsf_step_phase_2 failed'
    write(*, *) zsf_error_msg(err_code)
    call exit(1)
  endif

  err_code = zsf_step_phase_3(p, 300.0, state, transports)
  if (err_code > 0) then
    write(*, *) 'zsf_step_phase_3 failed'
    write(*, *) zsf_error_msg(err_code)
    call exit(1)
  endif

  p%ship_volume_sea_to_lake = 800.0
  err_code = zsf_step_phase_4(p, 840.0, state, transports)
  if (err_code > 0) then
    write(*, *) 'zsf_step_phase_4 failed'
    write(*, *) zsf_error_msg(err_code)
    call exit(1)
  endif

  write(*, *) ''
  write(*, *) 'Unsteady results: '
  write(*, *) 'head_lock = ', state%head_lock
  write(*, *) 'salinity_lock = ', state%salinity_lock
  write(*, *) 'saltmass_lock = ', state%saltmass_lock
  write(*, *) 'volume_ship_in_lock = ', state%volume_ship_in_lock

  if (state%salinity_lock < 22.5 .or. state%salinity_lock > 22.7) then
    write(*, *) 'Unsteady calculation did not give correct results'
    call exit(1)
  endif
end program test
