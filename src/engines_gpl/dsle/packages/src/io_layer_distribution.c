#include "io_layer_distribution.h"
#include <assert.h>
#include <float.h>
#include <math.h>
#include <stdlib.h>

#include "log/log.h"

inline const int fsign(const double x) { return (x > 0) - (x < 0); }

void cleanup_profile(profile_t *profile) {
  if (profile == NULL) {
    return;
  }
  free(profile->relative_z_position);
  free(profile->relative_discharge_from_lock);
}

void cleanup_layers(layers_t *layers) {
  if (layers == NULL) {
    return;
  }
  free(layers->normalized_target_volumes);
}

void cleanup_layered_discharge(layered_discharge_t *layered_discharge) {
  if (layered_discharge == NULL) {
    return;
  }
  free(layered_discharge->discharge_per_layer);
}

// Generate a default array linear z positions for supplied number_of_layers.
// Returns pointer to allocated array of doubles.
// Note: Caller is responsible to deallocating the created array.
double *io_layer_linear_z_positions(const int number_of_layers) {
  double *linear_z_positions = NULL;
  if (number_of_layers > 0) {
    linear_z_positions = calloc(number_of_layers, sizeof(double));
    if (linear_z_positions) {
      double z_step = 1.0 / (number_of_layers - 1.0);
      for (int i = 0; i < number_of_layers; i++) {
        linear_z_positions[i] = z_step * i;
      }
    }
  }
  return linear_z_positions;
}

// Set up a default '2D' profile
#define IO_2D_PROFILE_SIZE (2)
#define IO_2D_PROFILE()                                                                            \
  { 1.0, 1.0 }
int io_layer_init_2d(profile_t *profile) {
  int array_length = 0;
  double default_values[IO_2D_PROFILE_SIZE] = IO_2D_PROFILE();
  double *value_array = calloc(IO_2D_PROFILE_SIZE, sizeof(double));
  double *linear_z_positions = io_layer_linear_z_positions(IO_2D_PROFILE_SIZE);
  if (value_array && linear_z_positions) {
    memcpy(value_array, default_values, sizeof(double) * IO_2D_PROFILE_SIZE);
    *profile = (profile_t){.number_of_positions = IO_2D_PROFILE_SIZE,
                           .relative_discharge_from_lock = value_array,
                           .relative_z_position = linear_z_positions};
  } else {
    free(value_array);
    free(linear_z_positions);
    return -1;
  }
  return 0;
}

// Normalize profile.
// 1. Detect if there is a positive and negative lobe. (S-curve)
// 2. If there's not exactly one zero crossing, we throw an error.
// 3. Normalize of each lobe such that they integrate to +1 for positive or -1 for negative.
int io_normalize_profile(profile_t *profile) {
  int max_index = profile->number_of_positions - 1;
  int index_after_zero = max_index;
  int index_before_zero = 0;
  double z_start = profile->relative_z_position[0];
  double z_end = profile->relative_z_position[max_index];
  double z_zero = -1.0;
  int start_sign = fsign(profile->relative_discharge_from_lock[0]);
  int end_sign = fsign(profile->relative_discharge_from_lock[max_index]);

  if (start_sign == end_sign || start_sign == 0 || end_sign == 0) {
    // Profile has invalid shape (not exactly one zero in interval interior)
    log_error("Invalid profile shape! (Start and end should differ in sign)\n");
    return -1;
  }
  // Find z_zero.
  while (index_before_zero < max_index &&
         fsign(profile->relative_discharge_from_lock[index_before_zero + 1]) == start_sign) {
    ++index_before_zero;
  }
  while (index_after_zero > 0 &&
         fsign(profile->relative_discharge_from_lock[index_after_zero - 1]) == end_sign) {
    --index_after_zero;
  }

  assert(index_after_zero > index_before_zero);

  if (index_after_zero > index_before_zero + 1) {
    for (int i = index_before_zero + 1; i < index_after_zero; i++) {
      if (profile->relative_discharge_from_lock[i] > DBL_EPSILON) {
        log_error("Invalid profile shape!\n");
        return -1;
      }
    }
    z_zero = (profile->relative_z_position[index_before_zero + 1] + profile->relative_z_position[index_after_zero-1])/2;
  } else {
    // Find z_zero position by linear interpolation.
    const double z_before = profile->relative_z_position[index_before_zero];
    const double z_after = profile->relative_z_position[index_after_zero];
    const double p_before = profile->relative_discharge_from_lock[index_before_zero];
    const double p_after = profile->relative_discharge_from_lock[index_after_zero];
    z_zero = z_before - p_before * (z_after - z_before) / (p_after - p_before);
  }

  // Integrate top.
  const double integral_before = fabs(integrate_piecewise_linear_profile(profile, z_start, z_zero));
  // Integrate bottom.
  const double integral_after = fabs(integrate_piecewise_linear_profile(profile, z_zero, z_end));

  log_debug("raw profile.\n");
  log_debug("before = %g\n", integral_before);
  log_debug("after  = %g\n", integral_after);

  // Calculate correction factors.
  assert(integral_before > DBL_EPSILON);
  assert(integral_after > DBL_EPSILON);
  const double correction_factor_before = 1.0 / integral_before;
  const double correction_factor_after = 1.0 / integral_after;

  // Normalize profile discharges
  for (int i = 0; i <= index_before_zero; i++) {
    profile->relative_discharge_from_lock[i] *= correction_factor_before;
  }
  for (int i = index_after_zero; i <= max_index; i++) {
    profile->relative_discharge_from_lock[i] *= correction_factor_after;
  }
  // Keep zero information.
  profile->relative_z_zero = z_zero;
  profile->start_sign = start_sign;

  // Integrate top.
  const double check_integral_before =
      fabs(integrate_piecewise_linear_profile(profile, z_start, z_zero));
  // Integrate bottom.
  const double check_integral_after =
      fabs(integrate_piecewise_linear_profile(profile, z_zero, z_end));

  log_debug("normalized profile.\n");
  log_debug("before = %g\n", check_integral_before);
  log_debug("after  = %g\n\n", check_integral_after);

  return 0;
}

// Integrate relative_discharge_from_lock(x) with x from 0 to 1, assuming that relative_z_position[0] == 0 and relative_z_position[number_of_positions - 1] == 1.
// Further, the profile is assumed to be piecewise linear, and is a linear interpolation between the given relative_z_positions.
double integrate_piecewise_linear_profile(const profile_t *profile, const double lower_bound,
                                          const double upper_bound) {
  assert(0.0 <= lower_bound && lower_bound <= upper_bound && upper_bound <= 1.0);
  assert(profile->number_of_positions >= 2); // 0 and 1 have to be in

  int index_below_lower_bound = 0;
  while (index_below_lower_bound + 1 < profile->number_of_positions &&
         profile->relative_z_position[index_below_lower_bound + 1] < lower_bound) {
    ++index_below_lower_bound;
  }
  int index_above_upper_bound = profile->number_of_positions - 1;
  while (index_above_upper_bound - 1 >= 0 &&
         profile->relative_z_position[index_above_upper_bound - 1] > upper_bound) {
    --index_above_upper_bound;
  }
  assert(index_below_lower_bound <= index_above_upper_bound);
  double integrated_profile = 0.0;

  // Integrate from first profile point below lower bound to first profile point above upper bound (overestimates the integral)
  for (int i = index_below_lower_bound; i < index_above_upper_bound; ++i) {
    const double z_below = profile->relative_z_position[i];
    const double z_above = profile->relative_z_position[i + 1];
    const double p_below = profile->relative_discharge_from_lock[i];
    const double p_above = profile->relative_discharge_from_lock[i + 1];
    integrated_profile += 0.5 * (p_above + p_below) * (z_above - z_below);
  }

  const double z_lower_below = profile->relative_z_position[index_below_lower_bound];
  const double z_lower_above = profile->relative_z_position[index_below_lower_bound + 1];
  const double p_lower_below = profile->relative_discharge_from_lock[index_below_lower_bound];
  const double p_lower_above = profile->relative_discharge_from_lock[index_below_lower_bound + 1];
  // Subtract integral over profile from first profile point below lower bound to lower bound
  integrated_profile -= p_lower_below * (lower_bound - z_lower_below) +
                        0.5 * (p_lower_above - p_lower_below) *
                            pow(lower_bound - z_lower_below, 2.0) / (z_lower_above - z_lower_below);

  const double z_upper_below = profile->relative_z_position[index_above_upper_bound - 1];
  const double z_upper_above = profile->relative_z_position[index_above_upper_bound];
  const double p_upper_below = profile->relative_discharge_from_lock[index_above_upper_bound - 1];
  const double p_upper_above = profile->relative_discharge_from_lock[index_above_upper_bound];
  // Subtract integral over profile from upper bound to first profile point above upper bound
  integrated_profile -= p_upper_above * (z_upper_above - upper_bound) +
                        0.5 * (p_upper_below - p_upper_above) *
                            pow(z_upper_above - upper_bound, 2.0) / (z_upper_above - z_upper_below);
  return integrated_profile;
}

// Distribute the total_quantity over layers.
// The quantity that each layer receives is given by a relative profile
int distribute_discharge_over_layers(double total_quantity, const profile_t *profile,
                                     const layers_t *layers,
                                     layered_discharge_t *layered_quantity_result) {
  assert(layers != NULL);
  assert(layers->number_of_layers > 0);
  assert(layered_quantity_result != NULL);
  assert(layered_quantity_result->number_of_layers == layers->number_of_layers);
  assert(layered_quantity_result->discharge_per_layer != NULL);
  assert(profile->relative_z_position != NULL);
  assert(profile->relative_discharge_from_lock != NULL);
  assert(profile->number_of_positions > 0);

  if (profile->number_of_positions <= 0 || fabs(profile->relative_z_position[0]) > FLT_EPSILON ||
      fabs(profile->relative_z_position[profile->number_of_positions - 1] - 1.0) > FLT_EPSILON) {
    return -1;
  }

  double profile_integral = 0;
  double relative_z_start = 0.0;
  double relative_z_end = 1.0;
  double relative_z_prev = 0.0;
  double relative_z = 0.0;

  if (fsign(total_quantity) == profile->start_sign) {
    relative_z_end = profile->relative_z_zero;
  } else {
    relative_z_start = profile->relative_z_zero;
  }

  double relative_quantity_layer = 0.0;

  log_debug("quantity total = %g\n", total_quantity);
  log_debug("num_layers = %d\n", layers->number_of_layers);
  if (layers->number_of_layers > 1) {
    for (int layer = 0; layer < layers->number_of_layers; ++layer) {
      relative_z += layers->normalized_target_volumes[layer];
      if (relative_z < relative_z_start || relative_z_prev > relative_z_end) {
        relative_quantity_layer = 0.0;
      } else if (relative_z >= relative_z_start && relative_z_prev < relative_z_start) {
        relative_quantity_layer = integrate_piecewise_linear_profile(profile, relative_z_start, relative_z);
      } else if (relative_z > relative_z_end && relative_z_prev >= relative_z_start) {
        relative_quantity_layer = integrate_piecewise_linear_profile(profile, relative_z_prev, relative_z_end);
      } else {
        relative_quantity_layer = integrate_piecewise_linear_profile(profile, relative_z_prev, relative_z);
      }
      const double layer_quantity = fabs(relative_quantity_layer) * total_quantity;
      profile_integral += relative_quantity_layer;
      layered_quantity_result->discharge_per_layer[layer] = layer_quantity;
      log_debug("layer quantity         [%d] = %g (== %g * %g)\n", layer, layer_quantity,
                fabs(relative_quantity_layer), total_quantity);
      relative_z_prev = relative_z;
    }
  } else {
    layered_quantity_result->discharge_per_layer[0] = total_quantity;
    profile_integral = 1.0;
    log_debug("layer quantity         [0] = %g (== 1.0 * %g)\n", total_quantity, total_quantity);
  }

  log_debug("profile integral = %g (should be +/-1)\n\n", profile_integral);

  return 0;
}
