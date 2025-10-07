

#include "sealock.h"
#include "load_phase_wise.h"
#include "load_time_averaged.h"
#include "timestamp.h"

#include <assert.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void sealock_set_2d_defaults(dfm_volumes_t *volumes) {
  volumes->num_volumes = 1;
  volumes->volumes[0] = 1.0;
  volumes->first_active_cell = 0;
  volumes->num_active_cells = 1;
}

int sealock_defaults(sealock_state_t *lock) {
  // Init calculation parameters with defaults.
  lock->phase_args = PHASE_WISE_CLEAR_ARGS();
  zsf_param_default(&lock->parameters);
  lock->parameters.allowed_head_difference = 0.1; // 10 cm.
  // Set up default volumes/profile for '2D' case.
  sealock_set_2d_defaults(&lock->from_lake_volumes);
  sealock_set_2d_defaults(&lock->from_sea_volumes);
  sealock_set_2d_defaults(&lock->to_lake_volumes);
  sealock_set_2d_defaults(&lock->to_sea_volumes);
  return io_layer_init_2d(&lock->flow_profile);
}

int sealock_init(sealock_state_t *lock, time_t start_time, unsigned int max_num_z_layers) {
  int status = SEALOCK_OK;

  // Set number of DFM volumes.
  lock->from_lake_volumes.num_volumes = max_num_z_layers;
  lock->from_sea_volumes.num_volumes = max_num_z_layers;
  lock->to_lake_volumes.num_volumes = max_num_z_layers;
  lock->to_sea_volumes.num_volumes = max_num_z_layers;

  // Load timeseries data when required.
  if (status == SEALOCK_OK && lock->operational_parameters_file) {
    status = sealock_load_timeseries(lock, lock->operational_parameters_file);
    if (status == SEALOCK_OK) {
      if (lock->times[0] > start_time) {
        log_error("Timeseries of lock '%s' starts after start_time! (%lld > %lld)\n", lock->id,
                  lock->times[0], start_time);
        status = SEALOCK_ERROR;
      }
    }
  }

  if (status == SEALOCK_OK) {
    // Do one update to properly populate all parameters from timeseries for current time.
    status = sealock_set_parameters_for_time(lock, start_time);
  }

  if (status == SEALOCK_OK) {
    // Initialize parameters consistent with current and given settings.
    status = zsf_initialize_state(&lock->parameters, &lock->phase_state,
                                  lock->phase_state.salinity_lock, lock->phase_state.head_lock);
  }

  return status;
}

int sealock_load_timeseries(sealock_state_t *lock, char *filepath) {
  int status = SEALOCK_OK;
  double *time_column = NULL;
  size_t num_rows = 0;

  // read csv data
  switch (lock->computation_mode) {
  case cycle_average_mode:
    status = load_time_averaged_timeseries(&lock->timeseries_data, filepath) ? SEALOCK_ERROR
                                                                             : SEALOCK_OK;
    break;
  case phase_wise_mode:
    status =
        load_phase_wise_timeseries(&lock->timeseries_data, filepath) ? SEALOCK_ERROR : SEALOCK_OK;
    break;
  default:
    status = SEALOCK_ERROR;
    break;
  }

  if (status == SEALOCK_OK) {
    lock->current_row = NO_CURRENT_ROW;
    num_rows = get_csv_num_rows(&lock->timeseries_data);
    if (!num_rows)
      return SEALOCK_ERROR;
    time_column = malloc(num_rows * sizeof(double));
    if (time_column != NULL) {
      status = get_csv_column_data(&lock->timeseries_data, "time", time_column, num_rows);
      if (status == SEALOCK_OK) {
        lock->times = timestamp_array_to_times(time_column, num_rows);
        if (lock->times) {
          if (times_strictly_increasing(lock->times, num_rows)) {
            lock->times_len = num_rows;
          } else {
            free(lock->times);
            lock->times = NULL;
            status = SEALOCK_ERROR;
          }
        } else {
          status = SEALOCK_ERROR;
        }
      }
      free(time_column);
    }
  }

  return status;
}

// Returns 1 if we skipped to a new row.
static int sealock_update_current_row(sealock_state_t *lock, time_t time) {
  size_t previous_row = lock->current_row;
  size_t row = 0;
  while (time >= lock->times[row] && row < lock->times_len) {
    row++;
  }
  lock->current_row = row ? row - 1 : 0;
  log_debug("%s: time      = %lu (%12.0f)\n", __func__, time, time_to_timestamp(time));
  log_debug("%s: times[%2d] = %lu (%12.0f)\n", __func__, lock->current_row,
            lock->times[lock->current_row], time_to_timestamp(lock->times[lock->current_row]));
  log_debug("%s: returning %d\n", __func__, lock->current_row != previous_row);
  return lock->current_row != previous_row;
}

static int sealock_update_cycle_average_parameters(sealock_state_t *lock, time_t time) {
  sealock_update_current_row(lock, time);
  return get_csv_row_data(&lock->timeseries_data, lock->current_row, &lock->parameters) == CSV_OK
             ? SEALOCK_OK
             : SEALOCK_ERROR;
}

static int sealock_cycle_average_step(sealock_state_t *lock, time_t time) {
  int status = SEALOCK_OK;

  status = zsf_calc_steady(&lock->parameters, &lock->results, NULL);
  // Adjust result value signs to align with D-Flow FM conventions.
  if (status == SEALOCK_OK) {
    lock->results.discharge_from_lake = -lock->results.discharge_from_lake;
    lock->results.discharge_from_sea = -lock->results.discharge_from_sea;
  } else {
    log_error("zsf_calc_steady(..) returned %d: %s!\n", status, zsf_error_msg(status));
  }

  return status;
}

/* copy phase_results to general results */
static int sealock_phase_results_to_results(sealock_state_t *lock) {
  // Copy phase_results to equivelent results struct.
  // Also apply the sign convention for DIMR/BMI for discharge.
  lock->results.discharge_from_lake = -lock->phase_results.discharge_from_lake;
  lock->results.discharge_from_sea = -lock->phase_results.discharge_from_sea;
  lock->results.discharge_to_lake = lock->phase_results.discharge_to_lake;
  lock->results.discharge_to_sea = lock->phase_results.discharge_to_sea;
  lock->results.mass_transport_lake = lock->phase_results.mass_transport_lake;
  lock->results.mass_transport_sea = lock->phase_results.mass_transport_sea;
  lock->results.salinity_to_lake = lock->phase_results.salinity_to_lake;
  lock->results.salinity_to_sea = lock->phase_results.salinity_to_sea;
  return SEALOCK_OK;
}

/* Set a correction for the current phase results for possibly skipping a bit of the start. */
static int sealock_apply_phase_wise_result_correction(sealock_state_t *lock, time_t time) {
  // Note: We assume the timestepping from DIMR will be constant and non-zero.
  time_t delta_time = lock->phase_args.time_step;
  time_t phase_start = lock->times[lock->current_row];
  time_t phase_end = lock->phase_args.time_duration_end;
  time_t phase_duration = (time_t)lock->phase_args.duration;
  time_t skipped_time = time - phase_start;
  time_t new_phase_duration = phase_duration - skipped_time;
  time_t dimr_phase_duration = delta_time * (1 + new_phase_duration / (delta_time + 1));

  // A large skipped time indicates that we started inside a phase.
  // In that case, shorten the original duration.
  if (skipped_time >= delta_time) {
    if (skipped_time < phase_duration) {
      phase_duration -= skipped_time;
    } else {
      phase_duration = 0;
    }
  }

  double correction_factor = 1.0 * phase_duration / dimr_phase_duration;

  // Should always be true due to update before calculation.
  assert(time >= phase_start);
  assert(time < phase_end);
  assert(new_phase_duration > 0);
  assert(delta_time > 0);

  log_debug("%s: delta_time = %lu, skipped_time = %lu\n", __func__, delta_time, skipped_time);
  log_debug("%s: duration = %lu, dimr_phase_duration = %lu\n", __func__, phase_duration, dimr_phase_duration);
  log_debug("%s: correction factor: %g\n", __func__, correction_factor);

  log_info("Correcting for change of phase.\n");
  // Calculate (corrected) volume, and resulting salinity for lake and sea.
  // Note: The total salt mass should remain the same, so no correction is needed there.
  log_info("Applying correction to discharge_from_lake : %g -> %g\n",
            lock->results.discharge_from_lake,
            lock->results.discharge_from_lake * correction_factor);
  lock->results.discharge_from_lake *= correction_factor;
  log_info("Applying correction to discharge_from_sea  : %g -> %g\n",
            lock->results.discharge_from_sea,
            lock->results.discharge_from_sea * correction_factor);
  lock->results.discharge_from_sea *= correction_factor;
  log_info("Applying correction to discharge_to_lake : %g -> %g\n",
            lock->results.discharge_to_lake, lock->results.discharge_to_lake * correction_factor);
  lock->results.discharge_to_lake *= correction_factor;
  log_info("Applying correction to discharge_to_sea  : %g -> %g\n",
            lock->results.discharge_to_sea, lock->results.discharge_to_sea * correction_factor);
  lock->results.discharge_to_sea *= correction_factor;

  return SEALOCK_OK;
}

static int sealock_update_phase_wise_parameters(sealock_state_t *lock, time_t time) {
  int status = SEALOCK_OK;
  phase_wise_row_t row_data = PHASE_WISE_CLEAR_ROW();
  // Update when we enter a new phase, or don't know the time_step yet (at start).
  if (sealock_update_current_row(lock, time)) {
    status = get_csv_row_data(&lock->timeseries_data, lock->current_row, &row_data);
    if (status == SEALOCK_OK) {
      // copy relevant args to lock.
      lock->phase_args.run_update = 1;
      lock->phase_args.routine = row_data.routine;
      lock->parameters.density_current_factor_sea = row_data.density_current_factor_sea;
      lock->parameters.density_current_factor_lake = row_data.density_current_factor_lake;
      lock->parameters.ship_volume_sea_to_lake = 0;
      lock->parameters.ship_volume_lake_to_sea = 0;
      lock->parameters.distance_door_bubble_screen_lake = row_data.distance_door_bubble_screen_lake;
      lock->parameters.distance_door_bubble_screen_sea = row_data.distance_door_bubble_screen_sea;
      lock->parameters.flushing_discharge_high_tide = row_data.flushing_discharge_high_tide;
      lock->parameters.flushing_discharge_low_tide = row_data.flushing_discharge_low_tide;
      lock->parameters.sill_height_lake = row_data.sill_height_lake;
      lock->parameters.sill_height_sea = row_data.sill_height_sea;
      switch (row_data.routine) {
      case 1:
      case 3:
        lock->phase_args.duration = row_data.t_level;
        break;
      case 2:
        lock->phase_args.duration = row_data.t_open_lake;
        lock->parameters.ship_volume_lake_to_sea = row_data.ship_volume_lake_to_sea;
        break;
      case 4:
        lock->phase_args.duration = row_data.t_open_sea;
        lock->parameters.ship_volume_sea_to_lake = row_data.ship_volume_sea_to_lake;
        break;
      default:
        if (row_data.routine < 0) {
          lock->phase_args.duration = row_data.t_flushing;
        } else {
          status = SEALOCK_ERROR;
        }
        break;
      }
      lock->phase_args.time_duration_end = lock->times[lock->current_row] + (time_t)lock->phase_args.duration;
    }
  }
  return status;
}

static int sealock_phase_wise_step(sealock_state_t *lock, time_t time) {
  int status = SEALOCK_OK;
  time_t prev_time = lock->phase_args.time;
  int do_start_correction = (prev_time > 0 && lock->phase_args.time_step == 0);
  // Advance time and/or update time step.
  lock->phase_args.time_step = prev_time > 0 ? time - prev_time : 0;
  lock->phase_args.time = time;

  log_debug("%s: Handling '%s' (run_update = %d)\n", __func__, lock->id,
            lock->phase_args.run_update);
  if (lock->phase_args.run_update) {
    log_info("%s: Updating '%s' to phase %d.\n", __func__, lock->id, lock->phase_args.routine);
    switch (lock->phase_args.routine) {
    case 1:
      status = zsf_step_phase_1(&lock->parameters, lock->phase_args.duration, &lock->phase_state,
                                &lock->phase_results);
      break;
    case 2:
      status = zsf_step_phase_2(&lock->parameters, lock->phase_args.duration, &lock->phase_state,
                                &lock->phase_results);
      break;
    case 3:
      status = zsf_step_phase_3(&lock->parameters, lock->phase_args.duration, &lock->phase_state,
                                &lock->phase_results);
      break;
    case 4:
      status = zsf_step_phase_4(&lock->parameters, lock->phase_args.duration, &lock->phase_state,
                                &lock->phase_results);
      break;
    default:
      if (lock->phase_args.routine < 0) {
        status = zsf_step_flush_doors_closed(&lock->parameters, lock->phase_args.duration,
                                             &lock->phase_state, &lock->phase_results);
      } else {
        status = SEALOCK_ERROR;
      }
      break;
    }

    if (status == 2 && (lock->phase_args.routine == 2 || lock->phase_args.routine == 4)) {
      // There was a larger than allowed difference between the head and the lock when opening the doors.
      // Calculations should continue, but we do need to log a warning.
      log_warning("zsf_step_phase_%d(..) returned %d: %s!\n", lock->phase_args.routine, status,
                  zsf_error_msg(status));
      status = SEALOCK_OK;
    }

    if (status == SEALOCK_OK) {
      log_debug("Status is OK. Converting results.\n");
      status = sealock_phase_results_to_results(lock);
    } else {
      if (lock->phase_args.routine > 0) {
        log_error("zsf_step_phase_%d(..) returned %d: %s!\n", lock->phase_args.routine, status,
                  zsf_error_msg(status));
      } else if (lock->phase_args.routine < 0) {
        log_error("zsf_step_flush_doors_closed(..) returned %d: %s!\n", status,
                  zsf_error_msg(status));
      }
    }
  }

  if (lock->phase_args.run_update || do_start_correction) {
    if (status == SEALOCK_OK && lock->phase_args.time_step > 0) {
      log_debug("Status is OK. Applying correction.\n");
      status = sealock_apply_phase_wise_result_correction(lock, time);
    }
  }

  if (time >= lock->phase_args.time_duration_end) {
    // Phase duration has ended, stop the discharge.
    log_debug("%s: Time past duration (%g seconds), result set to zero.\n", __func__,
              lock->phase_args.duration);
    lock->results.discharge_from_lake = 0;
    lock->results.discharge_from_sea = 0;
    lock->results.discharge_to_lake = 0;
    lock->results.discharge_to_sea = 0;
  }

  lock->phase_args.run_update = 0; // Done with update, so reset flag.
  return status;
}

int sealock_set_parameters_for_time(sealock_state_t *lock, time_t time) {
  int status = SEALOCK_OK;

  switch (lock->computation_mode) {
  case cycle_average_mode:
    status = sealock_update_cycle_average_parameters(lock, time);
    break;
  case phase_wise_mode:
    status = sealock_update_phase_wise_parameters(lock, time);
    break;
  default:
    status = SEALOCK_ERROR; // Should never happen.
    break;
  }
  return status;
}

static void sealock_get_active_cells(dfm_volumes_t *volumes) {
  // Determine amount of active volumes.
  unsigned amount = 0;
  unsigned first = 0;
  unsigned last = volumes->num_volumes - 1;
  double total_volume = 0.0;
  unsigned index = 0;
  while (first < volumes->num_volumes && volumes->volumes[first] <= DBL_EPSILON) {
    first++;
  }
  while (last > 0 && volumes->volumes[last] <= 0) {
    last--;
  }
  if (last >= first) {
    amount = last - first + 1;
  }
  volumes->num_active_cells = amount;
  volumes->first_active_cell = first;
  // calculate normalized volumes
  for (index = first; index < first + amount; index++) {
    total_volume += volumes->volumes[index];
  }
  log_debug("total_volume   = %g\n", total_volume);
  for (index = first; index < first + amount; index++) {
    volumes->normalized[index] = volumes->volumes[index] / total_volume;
    log_debug("volumes   [%d] = %g\n", index, volumes->volumes[index]);
    log_debug("normalized[%d] = %g\n", index, volumes->volumes[index] / total_volume);
  }
}

// Determine the number and start of active layers.
// Underlying assumption is that the volumes have one
// contiguous run of non-zero cells.
void sealock_get_active_layers(sealock_state_t *lock) {
  log_debug("from_lake_volumes:\n");
  sealock_get_active_cells(&lock->from_lake_volumes);
  log_debug("from_sea _volumes:\n");
  sealock_get_active_cells(&lock->from_sea_volumes);
  log_debug("to_lake_volumes:\n");
  sealock_get_active_cells(&lock->to_lake_volumes);
  log_debug("to_sea_volumes:\n");
  sealock_get_active_cells(&lock->to_sea_volumes);
}

// Collect aggregate of quantity from dfm layer buffer into scalar.
static double sealock_collect(dfm_volumes_t *volumes, double *buffer_ptr) {
  double aggregate = 0.0;

  assert(volumes);
  assert(buffer_ptr);

  int first = volumes->first_active_cell;
  int last = first + volumes->num_active_cells - 1;
  double used_volume = 0.0;

  for (int i = first; i <= last; i++) {
    log_debug("Layer %d, value = %g, volume = %g\n", i, buffer_ptr[i], volumes->normalized[i]);
    aggregate += buffer_ptr[i] * volumes->normalized[i];
    used_volume += volumes->normalized[i];
  }

  log_debug("Aggregate = %g\n", aggregate);
  log_debug("Used volume = %g\n", used_volume);

  // Adjust aggregate to normalized volume.
  // Note: if used_volume is zero, so is aggregate.
  if (used_volume > DBL_EPSILON) {
    aggregate /= used_volume;
  }

  log_debug("Volume adjusted aggregate = %g\n\n", aggregate);

  return aggregate;
}

// Collect all scalar inputs that are provided in layers by dfm.
static int sealock_collect_layers(sealock_state_t *lock) {
  dfm_volumes_t *lake_volumes, *sea_volumes;

  assert(lock);
  sealock_get_active_layers(lock);

  lake_volumes = &lock->from_lake_volumes;
  if (lake_volumes->num_volumes == 1) {
    lock->parameters.salinity_lake =
        lock->parameters3d.salinity_lake[lake_volumes->first_active_cell];
  } else {
    log_debug("Collecting salinity_lake from layers:\n");
    lock->parameters.salinity_lake =
        sealock_collect(lake_volumes, lock->parameters3d.salinity_lake);
  }

  sea_volumes = &lock->from_sea_volumes;
  if (sea_volumes->num_volumes == 1) {
    lock->parameters.salinity_sea = lock->parameters3d.salinity_sea[sea_volumes->first_active_cell];
  } else {
    log_debug("Collecting salinity_sea from layers:\n");
    lock->parameters.salinity_sea = sealock_collect(sea_volumes, lock->parameters3d.salinity_sea);
  }

  return SEALOCK_OK;
}

// Distribute scalar quantity over layers into layer buffer using a provided profile.
// If no profile is provided (NULL), the quanity is simply duplicated.
static int sealock_distribute(dfm_volumes_t *volumes, profile_t *profile, double quantity,
                              double *buffer_ptr) {
  layers_t layers;
  layered_discharge_t result;
  unsigned first_active;

  assert(volumes);
  assert(buffer_ptr);

  // Clear also non-active output cells to zero.
  memset(buffer_ptr, 0, MAX_NUM_VOLUMES * sizeof(double));

  first_active = volumes->first_active_cell;

  if (profile) {
    layers.number_of_layers = volumes->num_active_cells;
    layers.normalized_target_volumes = volumes->normalized;
    result.number_of_layers = layers.number_of_layers;
    result.discharge_per_layer = &buffer_ptr[first_active];

    return distribute_discharge_over_layers(quantity, profile, &layers, &result);
  }

  for (int layer = 0; layer < volumes->num_active_cells; layer++) {
    buffer_ptr[first_active + layer] = quantity;
    log_debug("layer quantity         [%d] = %g\n", layer, quantity);
  }
  log_debug("(no profile was used)\n\n");

  return SEALOCK_OK;
}

// Set layer entries in buffer to zero if mask buffer is zero.
static int sealock_mask_layers(dfm_volumes_t *volumes, double *mask_ptr, double *buffer_ptr) {
  layers_t layers;
  layered_discharge_t result;
  unsigned first_active;

  assert(volumes);
  assert(buffer_ptr);

  first_active = volumes->first_active_cell;

  for (int layer = 0; layer < volumes->num_active_cells; layer++) {
    if (fabs(mask_ptr[layer]) < DBL_EPSILON) {
      buffer_ptr[first_active + layer] = 0.0;
    }
    log_debug("masked layer quantity         [%d] = %g\n", layer, buffer_ptr[first_active + layer]);
  }

  return SEALOCK_OK;
}

static int sealock_distribute_results(sealock_state_t *lock) {
  profile_t *lake_profile, *sea_profile;
  dfm_volumes_t *from_lake_volumes, *from_sea_volumes;
  dfm_volumes_t *to_lake_volumes, *to_sea_volumes;

  assert(lock);

  lake_profile = &lock->flow_profile;
  sea_profile = &lock->flow_profile;
  from_lake_volumes = &lock->from_lake_volumes;
  from_sea_volumes = &lock->from_sea_volumes;
  to_lake_volumes = &lock->to_lake_volumes;
  to_sea_volumes = &lock->to_sea_volumes;

  log_debug("quantity = mass_transport_lake\n");
  if (sealock_distribute(to_lake_volumes, lake_profile, lock->results.mass_transport_lake,
                         lock->results3d.mass_transport_lake) != 0) {
    return SEALOCK_ERROR;
  }
  log_debug("quantity = salt_load_lake\n");
  if (sealock_distribute(to_lake_volumes, lake_profile, lock->results.salt_load_lake,
                         lock->results3d.salt_load_lake) != 0) {
    return SEALOCK_ERROR;
  }
  log_debug("quantity = discharge_from_lake\n");
  if (sealock_distribute(from_lake_volumes, lake_profile, lock->results.discharge_from_lake,
                         lock->results3d.discharge_from_lake) != 0) {
    return SEALOCK_ERROR;
  }
  log_debug("quantity = discharge_to_lake\n");
  if (sealock_distribute(to_lake_volumes, lake_profile, lock->results.discharge_to_lake,
                         lock->results3d.discharge_to_lake) != 0) {
    return SEALOCK_ERROR;
  }
  log_debug("quantity = salinity_to_lake\n");
  if (sealock_distribute(to_lake_volumes, NULL, lock->results.salinity_to_lake,
                         lock->results3d.salinity_to_lake) != 0) {
    return SEALOCK_ERROR;
  }
  if (sealock_mask_layers(to_lake_volumes, lock->results3d.discharge_to_lake,
                          lock->results3d.salinity_to_lake) != 0) {
    return SEALOCK_ERROR;
  }
  log_debug("quantity = mass_transport_sea\n");
  if (sealock_distribute(to_sea_volumes, sea_profile, lock->results.mass_transport_sea,
                         lock->results3d.mass_transport_sea) != 0) {
    return SEALOCK_ERROR;
  }
  log_debug("quantity = salt_load_sea\n");
  if (sealock_distribute(to_sea_volumes, sea_profile, lock->results.salt_load_sea,
                         lock->results3d.salt_load_sea) != 0) {
    return SEALOCK_ERROR;
  }
  log_debug("quantity = discharge_from_sea\n");
  if (sealock_distribute(from_sea_volumes, sea_profile, lock->results.discharge_from_sea,
                         lock->results3d.discharge_from_sea) != 0) {
    return SEALOCK_ERROR;
  }
  log_debug("quantity = discharge_to_sea\n");
  if (sealock_distribute(to_sea_volumes, sea_profile, lock->results.discharge_to_sea,
                         lock->results3d.discharge_to_sea) != 0) {
    return SEALOCK_ERROR;
  }
  log_debug("quantity = salinity_to_sea\n");
  if (sealock_distribute(to_sea_volumes, NULL, lock->results.salinity_to_sea,
                         lock->results3d.salinity_to_sea) != 0) {
    return SEALOCK_ERROR;
  }
  if (sealock_mask_layers(to_sea_volumes, lock->results3d.discharge_to_sea,
                          lock->results3d.salinity_to_sea) != 0) {
    return SEALOCK_ERROR;
  }
  return SEALOCK_OK;
}

int sealock_update(sealock_state_t *lock, time_t time) {
  int status = sealock_set_parameters_for_time(lock, time);
  if (status == SEALOCK_OK) {
    status = sealock_collect_layers(lock);
  }
  if (status == SEALOCK_OK) {
    switch (lock->computation_mode) {
    case cycle_average_mode:
      status = sealock_cycle_average_step(lock, time);
      break;
    case phase_wise_mode:
      status = sealock_phase_wise_step(lock, time);
      break;
    default:
      assert(0); // Should never happen.
      status = SEALOCK_ERROR;
      break;
    }
  }
  if (status == SEALOCK_OK) {
    status = sealock_distribute_results(lock);
  }
  return status;
}

// Check if none of the time steps in the timeseries is shorter than delta_time
int sealock_delta_time_ok(sealock_state_t *lock, time_t delta_time, time_t start_time) {
  int i = 1;
  time_t current_time = 0;
  time_t previous_time = start_time;

  for (int i = 1; i < lock->times_len; i++) {
    current_time = lock->times[i];
    if (current_time < previous_time) {
      continue;
    }
    if (current_time - previous_time <= delta_time) {
      return 0;
    }
    previous_time = current_time;
  }
  return 1;
}