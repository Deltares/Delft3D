#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "util.h"
#include "zsf.h"

// The zsf_calculate loop can take advantage of shared values (e.g. a
// reciprocal volume) between steps and the derivative parameters. Most
// compilers cannot seem to recognize the ~20% speedup that can be gained this
// way, so we have to force it.
#ifdef _MSC_VER
#  define forceinline __forceinline
#elif defined(__GNUC__)
#  define forceinline inline __attribute__((__always_inline__))
#elif defined(__CLANG__)
#  if __has_attribute(__always_inline__)
#    define forceinline inline __attribute__((__always_inline__))
#  else
#    define forceinline inline
#  endif
#else
#  define forceinline inline
#endif

#ifdef ZSF_USE_FAST_TANH
inline double TANH(const double x) {
  const double ax = fabs(x);
  const double x2 = x * x;

  const double z1 =
      (x *
       (2.45550750702956 + 2.45550750702956 * ax +
        (0.893229853513558 + 0.821226666969744 * ax) * x2) /
       (2.44506634652299 + (2.44506634652299 + x2) * fabs(x + 0.814642734961073 * x * ax)));

  return fmin(z1, 1.0);
}
#else
#  define TANH tanh
#endif

#define ERROR_CODES(X)                                                                             \
  X(ZSF_SUCCESS, "Success")                                                                        \
  X(ZSF_SHIP_TOO_BIG, "The ship is too large for the lock")                                        \
  X(ZSF_ERR_REMAINING_HEAD_DIFF, "Remaining head difference when opening doors")                   \
  X(ZSF_ERR_SAL_LOCK_OUT_OF_BOUNDS, "The salinity of the lock exceeds that of the boundaries")

#define ERROR_ENUM(ID, TEXT) ID,
enum error_ids { ERROR_CODES(ERROR_ENUM) ZSF_NUM_ERRORS };
#undef ERROR_ENUM

#define ERROR_TEXT(ID, TEXT)                                                                       \
  case ID:                                                                                         \
    return TEXT;
const char *ZSF_CALLCONV zsf_error_msg(int code) {
  switch (code) { ERROR_CODES(ERROR_TEXT) }
  return "Unknown error";
}
#undef ERROR_TEXT
#undef ERROR_CODES

typedef struct derived_parameters_t {
  double g;
  int is_high_tide;
  int is_low_tide;
  double volume_lock_at_sea;
  double volume_lock_at_lake;
  double t_cycle;
  double t_open_avg;
  double t_open;
  double t_open_lake;
  double t_open_sea;
  double flushing_discharge;
  double density_average;
} derived_parameters_t;

const char *ZSF_CALLCONV zsf_version() { return ZSF_GIT_DESCRIBE; }

static forceinline void calculate_derived_parameters(const zsf_param_t *p,
                                                     derived_parameters_t *o) {
  // Gravitational constant
  o->g = 9.81;

  // Calculate derived parameters
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  // Tide signal
  o->is_high_tide = p->head_sea >= p->head_lake;
  o->is_low_tide = 1 - o->is_high_tide;

  // Volumes
  o->volume_lock_at_sea = p->lock_length * p->lock_width * (p->head_sea - p->lock_bottom);
  o->volume_lock_at_lake = p->lock_length * p->lock_width * (p->head_lake - p->lock_bottom);

  // Door open times
  o->t_cycle = 24.0 * 3600.0 / p->num_cycles;
  o->t_open_avg = 0.5 * o->t_cycle - (p->leveling_time + 2.0 * 0.5 * p->door_time_to_open);
  o->t_open = p->calibration_coefficient * o->t_open_avg;
  o->t_open_lake = p->symmetry_coefficient * o->t_open;
  o->t_open_sea = (2.0 - p->symmetry_coefficient) * o->t_open;

  // Flushing discharge
  o->flushing_discharge =
      o->is_low_tide ? p->flushing_discharge_low_tide : p->flushing_discharge_high_tide;

  // Average density (for lock exchange)
  o->density_average =
      0.5 * (sal_2_density(p->salinity_lake, p->temperature_lake, p->rtol, p->atol) +
             sal_2_density(p->salinity_sea, p->temperature_sea, p->rtol, p->atol));
}

static int check_parameters_state(const zsf_param_t *p, const derived_parameters_t *o,
                                  const zsf_phase_state_t *state) {

  if (fmax(p->ship_volume_lake_to_sea, p->ship_volume_sea_to_lake) >
      fmin(o->volume_lock_at_lake, o->volume_lock_at_sea)) {
    return ZSF_SHIP_TOO_BIG;
  }
  if ((state->salinity_lock > fmax(p->salinity_lake, p->salinity_sea) + 1E-8) ||
      (state->salinity_lock < fmin(p->salinity_lake, p->salinity_sea) - 1E-8)) {
    return ZSF_ERR_SAL_LOCK_OUT_OF_BOUNDS;
  }

  return ZSF_SUCCESS;
}

void ZSF_CALLCONV zsf_param_default(zsf_param_t *p) {
  /* */
  memset(p, 0, sizeof(zsf_param_t));

  // Lock properties
  p->lock_length = 100.0;
  p->lock_width = 10.0;
  p->lock_bottom = -5.0;

  // Operation properties
  p->num_cycles = 12.0;
  p->door_time_to_open = 300.0;
  p->leveling_time = 300.0;
  p->symmetry_coefficient = 1.0;
  p->ship_volume_sea_to_lake = 0.0;
  p->ship_volume_lake_to_sea = 0.0;
  p->calibration_coefficient = 1.0;

  // Salt intrusion counter-measures
  p->flushing_discharge_high_tide = 0.0;
  p->flushing_discharge_low_tide = 0.0;
  p->density_current_factor_sea = 1.0;
  p->density_current_factor_lake = 1.0;
  p->distance_door_bubble_screen_sea = 0.0;
  p->distance_door_bubble_screen_lake = 0.0;

  p->sill_height_sea = 0.0;
  p->sill_height_lake = 0.0;

  // Initial condition
  p->salinity_lock = ZSF_NAN;

  // Boundary conditions
  p->head_sea = 0.0;
  p->salinity_sea = 30.0;
  p->temperature_sea = 15.0;
  p->head_lake = 0.0;
  p->salinity_lake = 1.0;
  p->temperature_lake = 15.0;

  // Convergence criterion
  p->rtol = 1E-5;
  p->atol = 1E-8;

  // Head difference allowance
  p->allowed_head_difference = 1E-8;
}

static forceinline void step_phase_1(const zsf_param_t *p, const derived_parameters_t *o,
                                     double t_level, zsf_phase_state_t *state,
                                     zsf_phase_transports_t *results) {
  // Phase 1: Leveling lock to lake side
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //              Low Tide                                       High Tide
  //
  //      Lake                          Sea              Lake                          Sea
  //                |             |                                |--\   ↓   /--|------------
  //    ------------|             |                    ------------|   \_____/   |
  //                |--\   ↑   /--|------------                    |             |
  //                →   \_____/   |                                ←             |
  //    ____________|_____________|____________        ____________|_____________|____________
  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  double saltmass_lock_4 = state->saltmass_lock;
  double sal_lock_4 = state->salinity_lock;
  double volume_ship_in_lock_4 = state->volume_ship_in_lock;

  // Leveling
  double vol_to_lake = fmax(state->head_lock - p->head_lake, 0.0) * p->lock_width * p->lock_length;
  double vol_from_lake =
      fmax(p->head_lake - state->head_lock, 0.0) * p->lock_width * p->lock_length;
  double mt_lake_1 = vol_from_lake * p->salinity_lake - vol_to_lake * sal_lock_4;

  // Update the results
  results->mass_transport_lake = mt_lake_1;
  results->volume_from_lake = vol_from_lake;
  results->volume_to_lake = vol_to_lake;
  results->discharge_from_lake = vol_from_lake / t_level;
  results->discharge_to_lake = vol_to_lake / t_level;
  results->salinity_to_lake = sal_lock_4;

  results->mass_transport_sea = 0.0;
  results->volume_from_sea = 0.0;
  results->volume_to_sea = 0.0;
  results->discharge_from_sea = 0.0;
  results->discharge_to_sea = 0.0;
  results->salinity_to_sea = sal_lock_4;

  // Update state variables of the lock
  double saltmass_lock_1 = saltmass_lock_4 + mt_lake_1;
  double sal_lock_1 = saltmass_lock_1 / (o->volume_lock_at_lake - volume_ship_in_lock_4);

  assert((sal_lock_1 >= p->salinity_lake - 1E-8) & (sal_lock_1 <= p->salinity_sea + 1E-8));

  // Rounding errors can lead to ever so slight exceedences of the boundary
  // conditions, so we clip the salinity and recalculate the salt mass.
  sal_lock_1 = fmax(sal_lock_1, p->salinity_lake);
  sal_lock_1 = fmin(sal_lock_1, p->salinity_sea);
  saltmass_lock_1 = sal_lock_1 * (o->volume_lock_at_lake - volume_ship_in_lock_4);

  state->salinity_lock = sal_lock_1;
  state->saltmass_lock = saltmass_lock_1;
  state->head_lock = p->head_lake;
  // state->volume_ship_in_lock = state->volume_ship_in_lock;  /* Unchanged */
}

static forceinline void step_phase_2(const zsf_param_t *p, const derived_parameters_t *o,
                                     double t_open_lake, zsf_phase_state_t *state,
                                     zsf_phase_transports_t *results) {
  // Phase 2: Gate opening at lake side
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //              Low Tide                                       High Tide
  //
  //      Lake                          Sea             Lake                          Sea
  //                              |                                              |------------
  //    --------\  <->  /---------|                    --------\  <->  /---------|
  //             \_____/          |------------                 \_____/          |
  //                '             → flushing                       '             → flushing
  //    ____________'_____________|____________        ____________'_____________|____________
  //
  // Consists of three subphases:
  // a. Ships exiting lock
  // b. Lock exchange + flushing
  // c. Ships entering lock
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  double saltmass_lock_1 = state->saltmass_lock;
  double sal_lock_1 = state->salinity_lock;
  double volume_ship_in_lock_1 = state->volume_ship_in_lock;

  // Subphase a. Ships exiting the lock chamber towards the lake
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  double mt_lake_2_ship_exit = volume_ship_in_lock_1 * p->salinity_lake;

  // Update state variables of the lock
  double saltmass_lock_2a = saltmass_lock_1 + mt_lake_2_ship_exit;
  double sal_lock_2a = saltmass_lock_2a / o->volume_lock_at_lake;

  // Subphase b. Flushing compensated lock exchange
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // A sill is only 80% effective for reducing the lock exchange head, and
  // also 80% effective for reducing the total amount of water that can be
  // exchanged. In this subphase that means a salty layer of 80% the sill
  // height will is unaffected by lock exchange or flushing.
  double head_above_sill = p->head_lake - p->lock_bottom - p->sill_height_lake;
  double head_above_sill_dc_effective = p->head_lake - p->lock_bottom - 0.8 * p->sill_height_lake;
  double volume_lock_at_lake_effective =
      head_above_sill_dc_effective / (p->head_lake - p->lock_bottom) * o->volume_lock_at_lake;

  double velocity_flushing = o->flushing_discharge / (p->lock_width * head_above_sill);

  double sal_diff = sal_lock_2a - p->salinity_lake;
  double velocity_exchange_raw =
      0.5 * sqrt(o->g * 0.8 * sal_diff / o->density_average * head_above_sill_dc_effective);

  // Calculate the time that the density current is running unprotected, in
  // the case that the bubble screen is not exactly at the door opening. Note
  // that for equal (absolute) distance, the time differs between a bubble
  // screen inside and outside the lock chamber.
  double volume_exchange_2 = 0.0;
  double t_raw_exchange = 0.0;

  // Until the density current reaches the bubble screen
  if (p->distance_door_bubble_screen_lake != 0.0) {
    double velocity_t_raw_exchange =
        velocity_exchange_raw - copysign(velocity_flushing, p->distance_door_bubble_screen_lake);
    velocity_t_raw_exchange = fmax(velocity_t_raw_exchange, 1E-10);
    t_raw_exchange = fabs(p->distance_door_bubble_screen_lake) / velocity_t_raw_exchange;
    t_raw_exchange = fmin(t_raw_exchange, t_open_lake);

    double frac_lock_exchange_raw =
        fmax((velocity_exchange_raw - velocity_flushing) / velocity_exchange_raw, 0.0);
    double t_lock_exchange_raw = 2 * p->lock_length / velocity_exchange_raw;
    volume_exchange_2 += frac_lock_exchange_raw * volume_lock_at_lake_effective *
                         TANH(t_raw_exchange / t_lock_exchange_raw);
  }

  // After the current reaches the bubble screen
  double velocity_exchange_eta = p->density_current_factor_lake * velocity_exchange_raw;
  double frac_lock_exchange =
      fmax((velocity_exchange_eta - velocity_flushing) / velocity_exchange_eta, 0.0);
  double t_lock_exchange = 2 * p->lock_length / velocity_exchange_eta;
  volume_exchange_2 += frac_lock_exchange * (volume_lock_at_lake_effective - volume_exchange_2) *
                       TANH(fmax(t_open_lake - t_raw_exchange, 0.0) / t_lock_exchange);

  // Flushing itself (taking lock exchange into account)
  double volume_flush = o->flushing_discharge * t_open_lake;

  // Max volume that will lead to the lock being refreshed (before we
  // reach steady state where we are flushing to the sea with salinity of
  // lake)
  double max_volume_flush_refresh = volume_lock_at_lake_effective - volume_exchange_2;

  double volume_flush_refresh = fmin(volume_flush, max_volume_flush_refresh);
  double volume_flush_passthrough = fmax(volume_flush - max_volume_flush_refresh, 0.0);

  double mt_sea_2_flushing =
      volume_flush_refresh * sal_lock_2a + volume_flush_passthrough * p->salinity_lake;

  double volume_to_sea_2b = volume_flush;
  double volume_from_lake_2b = volume_exchange_2 + volume_flush;
  double volume_to_lake_2b = volume_exchange_2;

  double mt_to_sea_2b = mt_sea_2_flushing;
  double mt_to_lake_2b = volume_exchange_2 * sal_lock_2a;
  double mt_from_lake_2b = (volume_exchange_2 + volume_flush) * p->salinity_lake;

  // Update state variables of the lock
  double saltmass_lock_2b = saltmass_lock_2a + mt_from_lake_2b - mt_to_lake_2b - mt_to_sea_2b;
  double sal_lock_2b = saltmass_lock_2b / o->volume_lock_at_lake;

  // Subphase c. Ship entering the lock chamber from the lake
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  double mt_lake_2_ship_enter = -1 * p->ship_volume_lake_to_sea * sal_lock_2b;

#ifndef NDEBUG
  // These variables are only needed for the assertion later on
  // Update state variables of the lock
  double saltmass_lock_2c = saltmass_lock_2b + mt_lake_2_ship_enter;
  double sal_lock_2c = saltmass_lock_2c / (o->volume_lock_at_lake - p->ship_volume_lake_to_sea);
#endif

  // Totals for Phase 2
  // ~~~~~~~~~~~~~~~~~~
  // Total mass transports over both gates
  double mt_lake_2 = mt_lake_2_ship_exit + mt_lake_2_ship_enter + mt_from_lake_2b - mt_to_lake_2b;
  double mt_sea_2 = mt_to_sea_2b;

  // Update state variables of the lock
  double saltmass_lock_2 = saltmass_lock_1 + mt_lake_2 - mt_sea_2;
  double sal_lock_2 = saltmass_lock_2 / (o->volume_lock_at_lake - p->ship_volume_lake_to_sea);

  assert(fabs(saltmass_lock_2 - saltmass_lock_2c) < 1E-8);
  assert(fabs(sal_lock_2 - sal_lock_2c) < 1E-8);

  assert((sal_lock_2 >= p->salinity_lake - 1E-8) & (sal_lock_2 <= p->salinity_sea + 1E-8));

  // Rounding errors can lead to ever so slight exceedences of the boundary
  // conditions, so we clip the salinity and recalculate the salt mass.
  sal_lock_2 = fmax(sal_lock_2, p->salinity_lake);
  sal_lock_2 = fmin(sal_lock_2, p->salinity_sea);
  saltmass_lock_2 = sal_lock_2 * (o->volume_lock_at_lake - p->ship_volume_lake_to_sea);

  // Update the results
  results->mass_transport_lake = mt_lake_2;
  results->volume_from_lake = volume_ship_in_lock_1 + volume_from_lake_2b;
  results->volume_to_lake = volume_to_lake_2b + p->ship_volume_lake_to_sea;
  results->discharge_from_lake = results->volume_from_lake / t_open_lake;
  results->discharge_to_lake = results->volume_to_lake / t_open_lake;
  results->salinity_to_lake = (results->volume_to_lake > 0.0)
                                  ? -1 *
                                        (mt_lake_2 - results->volume_from_lake * p->salinity_lake) /
                                        results->volume_to_lake
                                  : sal_lock_1;

  results->mass_transport_sea = mt_sea_2;
  results->volume_from_sea = 0.0;
  results->volume_to_sea = o->flushing_discharge * t_open_lake;
  results->discharge_from_sea = 0.0;
  results->discharge_to_sea = o->flushing_discharge;
  results->salinity_to_sea =
      (results->volume_to_sea > 0.0) ? mt_sea_2 / results->volume_to_sea : sal_lock_1;

  // Update state variables of the lock
  state->saltmass_lock = saltmass_lock_2;
  state->salinity_lock = sal_lock_2;
  // state->head_lock = state->head_lock;  /* Unchanged */
  state->volume_ship_in_lock = p->ship_volume_lake_to_sea;
}

static forceinline void step_phase_3(const zsf_param_t *p, const derived_parameters_t *o,
                                     double t_level, zsf_phase_state_t *state,
                                     zsf_phase_transports_t *results) {
  // Phase 3: Leveling lock to sea side
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //              Low Tide                                       High Tide
  //
  //      Lake                          Sea              Lake                          Sea
  //                |             |                                |             |------------
  //    ------------|--\   ↓   /--|                    ------------|--\   ↑   /--|
  //                |   \_____/   |------------                    |   \_____/   |
  //                |             →                                |             ←
  //    ____________|_____________|____________        ____________|_____________|____________
  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  double saltmass_lock_2 = state->saltmass_lock;
  double sal_lock_2 = state->salinity_lock;
  double volume_ship_in_lock_2 = state->volume_ship_in_lock;

  // Leveling
  double vol_to_sea = fmax(state->head_lock - p->head_sea, 0.0) * p->lock_width * p->lock_length;
  double vol_from_sea = fmax(p->head_sea - state->head_lock, 0.0) * p->lock_width * p->lock_length;
  double mt_sea_3 = vol_to_sea * sal_lock_2 - vol_from_sea * p->salinity_sea;

  // Update the results
  results->mass_transport_lake = 0.0;
  results->volume_from_lake = 0.0;
  results->volume_to_lake = 0.0;
  results->discharge_from_lake = 0.0;
  results->discharge_to_lake = 0.0;
  results->salinity_to_lake = sal_lock_2;

  results->mass_transport_sea = mt_sea_3;
  results->volume_from_sea = vol_from_sea;
  results->volume_to_sea = vol_to_sea;
  results->discharge_from_sea = vol_from_sea / t_level;
  results->discharge_to_sea = vol_to_sea / t_level;
  results->salinity_to_sea = sal_lock_2;

  // Update state variables of the lock
  double saltmass_lock_3 = saltmass_lock_2 - mt_sea_3;
  double sal_lock_3 = saltmass_lock_3 / (o->volume_lock_at_sea - volume_ship_in_lock_2);

  assert((sal_lock_3 >= p->salinity_lake - 1E-8) & (sal_lock_3 <= p->salinity_sea + 1E-8));

  // Rounding errors can lead to ever so slight exceedences of the boundary
  // conditions, so we clip the salinity and recalculate the salt mass.
  sal_lock_3 = fmax(sal_lock_3, p->salinity_lake);
  sal_lock_3 = fmin(sal_lock_3, p->salinity_sea);
  saltmass_lock_3 = sal_lock_3 * (o->volume_lock_at_sea - volume_ship_in_lock_2);

  state->salinity_lock = sal_lock_3;
  state->saltmass_lock = saltmass_lock_3;
  state->head_lock = p->head_sea;
  // state->volume_ship_in_lock = state->volume_ship_in_lock;  /* Unchanged */
}

static forceinline void step_phase_4(const zsf_param_t *p, const derived_parameters_t *o,
                                     double t_open_sea, zsf_phase_state_t *state,
                                     zsf_phase_transports_t *results) {
  // Phase 4: Gate opening at sea side
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //              Low Tide                                       High Tide
  //
  //      Lake                          Sea              Lake                          Sea
  //                |                                              |---------\  <->  /--------
  //    ------------|                                  ------------|          \_____/
  //                |---------\  <->  /--------                    |             '
  //                |          \_____/                             |             '
  //    ____________|_____________'____________        ____________|_____________'____________
  //
  // Consists of three subphases:
  // a. Ships exiting lock
  // b. Lock exchange + flushing
  // c. Ships entering lock
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  double saltmass_lock_3 = state->saltmass_lock;
  double sal_lock_3 = state->salinity_lock;
  double volume_ship_in_lock_3 = state->volume_ship_in_lock;

  // Subphase a. Ships exiting the lock chamber towards the sea
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  double mt_sea_4_ship_exit = -1 * volume_ship_in_lock_3 * p->salinity_sea;

  // Update state variables of the lock
  double saltmass_lock_4a = saltmass_lock_3 - mt_sea_4_ship_exit;
  double sal_lock_4a = saltmass_lock_4a / o->volume_lock_at_sea;

  // Subphase b. Flushing compensated lock exchange
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // A sill is only 80% effective for reducing the lock exchange head, but on
  // this side not effective in reducing the maximum amount of water that can
  // be exchanged.
  double head_above_sill = p->head_sea - p->lock_bottom - p->sill_height_sea;
  double head_above_sill_dc_effective = p->head_sea - p->lock_bottom - 0.8 * p->sill_height_sea;

  double velocity_flushing = o->flushing_discharge / (p->lock_width * head_above_sill);

  double sal_diff = p->salinity_sea - sal_lock_4a;
  double velocity_exchange_raw =
      0.5 * sqrt(o->g * 0.8 * sal_diff / o->density_average * head_above_sill_dc_effective);

  // The equilibrium depth of the boundary layer between the salt (sal_sea)
  // and fresh (sal_lake) water when flushing for a very long time.
  double head_equilibrium =
      cbrt(2.0 * pow(o->flushing_discharge / p->lock_width, 2.0) * o->density_average /
           (o->g * 0.8 * (p->salinity_sea - p->salinity_lake)));

  head_equilibrium = fmin(head_equilibrium, p->head_sea - p->lock_bottom);

  // If we flush so much that the density current never enters the lock, we
  // might get division by zero. Avoid by branching such that we can still
  // use fast math (which typically does not work with non-finite values).
  double volume_exchange_4 = 0.0;
  double t_raw_exchange = 0.0;

  double frac_lock_exchange =
      (p->head_sea - p->lock_bottom - head_equilibrium) / (p->head_sea - p->lock_bottom);

  // Until the density current reaches the bubble screen
  if (p->distance_door_bubble_screen_sea != 0.0) {
    double velocity_t_raw_exchange =
        velocity_exchange_raw + copysign(velocity_flushing, p->distance_door_bubble_screen_sea);
    velocity_t_raw_exchange = fmax(velocity_t_raw_exchange, 1E-10);
    t_raw_exchange = fabs(p->distance_door_bubble_screen_sea) / velocity_t_raw_exchange;
    t_raw_exchange = fmin(t_raw_exchange, t_open_sea);

    double t_lock_exchange_raw =
        2 * p->lock_length * frac_lock_exchange / (velocity_exchange_raw - velocity_flushing);

    volume_exchange_4 +=
        frac_lock_exchange * o->volume_lock_at_sea * TANH(t_raw_exchange / t_lock_exchange_raw);
  }

  // After the current reaches the bubble screen
  double velocity_exchange_eta = p->density_current_factor_sea * velocity_exchange_raw;

  if (velocity_exchange_eta > velocity_flushing) {
    double t_lock_exchange =
        2 * p->lock_length * frac_lock_exchange / (velocity_exchange_eta - velocity_flushing);
    volume_exchange_4 += frac_lock_exchange * (o->volume_lock_at_sea - volume_exchange_4) *
                         TANH(fmax(t_open_sea - t_raw_exchange, 0.0) / t_lock_exchange);
  }

  // Flushing itself (taking lock exchange into account)
  double volume_flush = o->flushing_discharge * t_open_sea;

  // Max volume that will lead to the lock being refreshed (before we
  // reach steady state where we are flushing to the sea with salinity of
  // lake)
  double max_volume_flush_refresh = o->volume_lock_at_sea - volume_exchange_4;

  double volume_flush_refresh = fmin(volume_flush, max_volume_flush_refresh);
  double volume_flush_passthrough = fmax(volume_flush - max_volume_flush_refresh, 0.0);

  double mt_lake_4_flushing =
      volume_flush_refresh * p->salinity_lake + volume_flush_passthrough * p->salinity_lake;
  double mt_sea_4_flushing =
      volume_flush_refresh * sal_lock_4a + volume_flush_passthrough * p->salinity_lake;

  double volume_to_sea_4b = volume_exchange_4 + volume_flush;
  double volume_from_sea_4b = volume_exchange_4;
  double volume_from_lake_4b = volume_flush;

  double mt_to_sea_4b = mt_sea_4_flushing + volume_exchange_4 * sal_lock_4a;
  double mt_from_sea_4b = volume_exchange_4 * p->salinity_sea;
  double mt_from_lake_4b = mt_lake_4_flushing;

  // Update state variables of the lock
  double saltmass_lock_4b = saltmass_lock_4a + mt_from_sea_4b - mt_to_sea_4b + mt_from_lake_4b;
  double sal_lock_4b = saltmass_lock_4b / o->volume_lock_at_sea;

  // Subphase c. Ship entering the lock chamber from the sea
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  double mt_sea_4_ship_enter = p->ship_volume_sea_to_lake * sal_lock_4b;

#ifndef NDEBUG
  // These variables are only needed for the assertion later on
  // Update state variables of the lock
  double saltmass_lock_4c = saltmass_lock_4b - mt_sea_4_ship_enter;
  double sal_lock_4c = saltmass_lock_4c / (o->volume_lock_at_sea - p->ship_volume_sea_to_lake);
#endif

  // Totals for Phase 4
  // ~~~~~~~~~~~~~~~~~~
  // Total mass transports over both gates
  double mt_sea_4 = mt_sea_4_ship_exit + mt_sea_4_ship_enter + mt_to_sea_4b - mt_from_sea_4b;
  double mt_lake_4 = mt_from_lake_4b;

  // Update state variables of the lock
  double saltmass_lock_4 = saltmass_lock_3 + mt_lake_4 - mt_sea_4;
  double sal_lock_4 = saltmass_lock_4 / (o->volume_lock_at_sea - p->ship_volume_sea_to_lake);

  assert(fabs(saltmass_lock_4 - saltmass_lock_4c) < 1E-8);
  assert(fabs(sal_lock_4 - sal_lock_4c) < 1E-8);

  assert((sal_lock_4 >= p->salinity_lake - 1E-8) & (sal_lock_4 <= p->salinity_sea + 1E-8));

  // Rounding errors can lead to ever so slight exceedences of the boundary
  // conditions, so we clip the salinity and recalculate the salt mass.
  sal_lock_4 = fmax(sal_lock_4, p->salinity_lake);
  sal_lock_4 = fmin(sal_lock_4, p->salinity_sea);
  saltmass_lock_4 = sal_lock_4 * (o->volume_lock_at_sea - p->ship_volume_sea_to_lake);

  // Update the results
  results->mass_transport_lake = mt_lake_4;
  results->volume_from_lake = volume_from_lake_4b;
  results->volume_to_lake = 0.0;
  results->discharge_from_lake = o->flushing_discharge;
  results->discharge_to_lake = 0.0;
  results->salinity_to_lake = sal_lock_3;

  results->mass_transport_sea = mt_sea_4;
  results->volume_from_sea = volume_from_sea_4b + volume_ship_in_lock_3;
  results->volume_to_sea = volume_to_sea_4b + p->ship_volume_sea_to_lake;
  results->discharge_from_sea = results->volume_from_sea / t_open_sea;
  results->discharge_to_sea = results->volume_to_sea / t_open_sea;
  results->salinity_to_sea =
      (results->volume_to_sea > 0.0)
          ? (mt_sea_4 + results->volume_from_sea * p->salinity_sea) / results->volume_to_sea
          : sal_lock_3;

  // Update state variables of the lock
  state->saltmass_lock = saltmass_lock_4;
  state->salinity_lock = sal_lock_4;
  // state->head_lock = state->head_lock;  /* Unchanged */
  state->volume_ship_in_lock = p->ship_volume_sea_to_lake;
}

static forceinline void step_flush_doors_closed(const zsf_param_t *p, const derived_parameters_t *o,
                                                double t_flushing, zsf_phase_state_t *state,
                                                zsf_phase_transports_t *results) {
  // Flushing with gates closed
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //            Low Tide (for example)
  //
  //      Lake                           Sea
  //            |                   |
  //    --------|-------------------|
  //            |                   |-----------
  //            → flushing          → flushing
  //    ________|___________________|___________
  //
  // Note that the above schematics do not include a ship inside the lock, as
  // flushing with the doors closed is typically done between ships going out
  // of the lock, and ships going into the lock. This routine does however work
  // correctly when there is a ship inside nonetheless.
  // Also note that this routine does not care whether the lock is at sea or
  // lake level, or anywhere inbetween. It will however not raise/lower the
  // level in the lock, for which the levelling routines (phase 1 and 3)
  // should be used.
  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  // Contrary to the superposition of velocities in step_phase_2/4 (which
  // would correspond to a linear decay), we do an exponential decay. The
  // initial "speed" of this exponential decay is the same as that of the
  // linear decay.
  double sal_diff = state->salinity_lock - p->salinity_lake;
  double volume_water_in_lock =
      p->lock_length * p->lock_width * (state->head_lock - p->lock_bottom) -
      state->volume_ship_in_lock;

  double lam_exp = o->flushing_discharge * sal_diff / state->saltmass_lock;
  double saltmass_lock = volume_water_in_lock * sal_diff * exp(-1.0 * lam_exp * t_flushing) +
                         volume_water_in_lock * p->salinity_lake;
  double saltmass_out = state->saltmass_lock - saltmass_lock;

  // Update state variables of the lock
  double sal_lock = saltmass_lock / volume_water_in_lock;

  assert((sal_lock >= p->salinity_lake - 1E-8) & (sal_lock <= p->salinity_sea + 1E-8));

  // Rounding errors can lead to ever so slight exceedences of the boundary
  // conditions, so we clip the salinity and recalculate the salt mass.
  sal_lock = fmax(sal_lock, p->salinity_lake);
  sal_lock = fmin(sal_lock, p->salinity_sea);
  saltmass_lock = sal_lock * volume_water_in_lock;

  // Update the results
  results->mass_transport_lake = o->flushing_discharge * t_flushing * p->salinity_lake;
  results->volume_from_lake = o->flushing_discharge * t_flushing;
  results->volume_to_lake = 0.0;
  results->discharge_from_lake = o->flushing_discharge;
  results->discharge_to_lake = 0.0;
  results->salinity_to_lake = sal_lock;

  results->mass_transport_sea = saltmass_out;
  results->volume_from_sea = 0.0;
  results->volume_to_sea = o->flushing_discharge * t_flushing;
  results->discharge_from_sea = 0.0;
  results->discharge_to_sea = o->flushing_discharge;
  results->salinity_to_sea =
      (results->volume_to_sea > 0.0) ? saltmass_out / results->volume_to_sea : sal_lock;

  // Update state variables of the lock
  state->saltmass_lock = saltmass_lock;
  state->salinity_lock = sal_lock;
  // state->head_lock = state->head_lock;  /* Unchanged */
  // state->volume_ship_in_lock = state->ship_volume_lake_to_sea; /* Unchanged */
}

int ZSF_CALLCONV zsf_initialize_state(const zsf_param_t *p, zsf_phase_state_t *state,
                                      double sal_lock, double head_lock) {
  state->salinity_lock = sal_lock;
  state->saltmass_lock = sal_lock * (p->lock_length * p->lock_width * (head_lock - p->lock_bottom));
  state->head_lock = head_lock;
  state->volume_ship_in_lock = 0.0;

  return ZSF_SUCCESS;
}

int ZSF_CALLCONV zsf_step_phase_1(const zsf_param_t *p, double t_level, zsf_phase_state_t *state,
                                  zsf_phase_transports_t *results) {
  // Get the derived parameters
  derived_parameters_t o;
  calculate_derived_parameters(p, &o);

  int err = check_parameters_state(p, &o, state);
  if (err) {
    return err;
  }

  step_phase_1(p, &o, t_level, state, results);

  return ZSF_SUCCESS;
}

int ZSF_CALLCONV zsf_step_phase_2(const zsf_param_t *p, double t_open_lake,
                                  zsf_phase_state_t *state, zsf_phase_transports_t *results) {
  // Get the derived parameters
  derived_parameters_t o;
  calculate_derived_parameters(p, &o);

  int err = check_parameters_state(p, &o, state);
  if (err) {
    return err;
  }
  if (fabs(state->head_lock - p->head_lake) > p->allowed_head_difference) {
    return ZSF_ERR_REMAINING_HEAD_DIFF;
  }

  step_phase_2(p, &o, t_open_lake, state, results);

  return ZSF_SUCCESS;
}

int ZSF_CALLCONV zsf_step_flush_doors_closed(const zsf_param_t *p, double t_flushing,
                                             zsf_phase_state_t *state,
                                             zsf_phase_transports_t *results) {
  // Get the derived parameters
  derived_parameters_t o;
  calculate_derived_parameters(p, &o);

  int err = check_parameters_state(p, &o, state);
  if (err) {
    return err;
  }

  step_flush_doors_closed(p, &o, t_flushing, state, results);

  return ZSF_SUCCESS;
}

int ZSF_CALLCONV zsf_step_phase_3(const zsf_param_t *p, double t_level, zsf_phase_state_t *state,
                                  zsf_phase_transports_t *results) {
  // Get the derived parameters
  derived_parameters_t o;
  calculate_derived_parameters(p, &o);

  int err = check_parameters_state(p, &o, state);
  if (err) {
    return err;
  }

  step_phase_3(p, &o, t_level, state, results);

  return ZSF_SUCCESS;
}

int ZSF_CALLCONV zsf_step_phase_4(const zsf_param_t *p, double t_open_sea, zsf_phase_state_t *state,
                                  zsf_phase_transports_t *results) {
  // Get the derived parameters
  derived_parameters_t o;
  calculate_derived_parameters(p, &o);

  int err = check_parameters_state(p, &o, state);
  if (err) {
    return err;
  }
  if (fabs(state->head_lock - p->head_sea) > p->allowed_head_difference) {
    return ZSF_ERR_REMAINING_HEAD_DIFF;
  }

  step_phase_4(p, &o, t_open_sea, state, results);

  return ZSF_SUCCESS;
}

int ZSF_CALLCONV zsf_calc_steady(const zsf_param_t *p, zsf_results_t *results,
                                 zsf_aux_results_t *aux_results) {

  derived_parameters_t o;
  calculate_derived_parameters(p, &o);

  // Start salinity and salt mass
  zsf_phase_state_t state;

  double sal_lock_4 = p->salinity_lock;
  if (sal_lock_4 == ZSF_NAN)
    sal_lock_4 = 0.5 * (p->salinity_sea + p->salinity_lake);

  state.volume_ship_in_lock = p->ship_volume_sea_to_lake;
  state.saltmass_lock = sal_lock_4 * (o.volume_lock_at_sea - state.volume_ship_in_lock);
  state.head_lock = p->head_sea;
  state.salinity_lock = sal_lock_4;

  int err = check_parameters_state(p, &o, &state);
  if (err) {
    return err;
  }

  while (1) {
    // Backup old salinity value for convergence check
    double sal_lock_4_prev = sal_lock_4;

    zsf_phase_transports_t tp1;
    step_phase_1(p, &o, p->leveling_time, &state, &tp1);
    double sal_lock_1 = state.salinity_lock;

    zsf_phase_transports_t tp2;
    step_phase_2(p, &o, o.t_open_lake, &state, &tp2);
    double sal_lock_2 = state.salinity_lock;

    zsf_phase_transports_t tp3;
    step_phase_3(p, &o, p->leveling_time, &state, &tp3);
    double sal_lock_3 = state.salinity_lock;

    zsf_phase_transports_t tp4;
    step_phase_4(p, &o, o.t_open_sea, &state, &tp4);

    sal_lock_4 = state.salinity_lock;

    // Convergence check
    // ~~~~~~~~~~~~~~~~~
    if (is_close(sal_lock_4, sal_lock_4_prev, p->rtol, p->atol)) {
      // Cycle-averaged discharges and salinities
      // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // Lake side
      double mt_lake = tp1.mass_transport_lake + tp2.mass_transport_lake + tp3.mass_transport_lake +
                       tp4.mass_transport_lake;

      double vol_from_lake =
          tp1.volume_from_lake + tp2.volume_from_lake + tp3.volume_from_lake + tp4.volume_from_lake;
      double disch_from_lake = vol_from_lake / o.t_cycle;

      double vol_to_lake =
          tp1.volume_to_lake + tp2.volume_to_lake + tp3.volume_to_lake + tp4.volume_to_lake;
      double disch_to_lake = vol_to_lake / o.t_cycle;

      double salt_load_lake = mt_lake / o.t_cycle;
      double sal_to_lake = -1 * (mt_lake - vol_from_lake * p->salinity_lake) / vol_to_lake;

      // Sea side
      double mt_sea = tp1.mass_transport_sea + tp2.mass_transport_sea + tp3.mass_transport_sea +
                      tp4.mass_transport_sea;

      double vol_from_sea =
          tp1.volume_from_sea + tp2.volume_from_sea + tp3.volume_from_sea + tp4.volume_from_sea;
      double disch_from_sea = vol_from_sea / o.t_cycle;

      double vol_to_sea =
          tp1.volume_to_sea + tp2.volume_to_sea + tp3.volume_to_sea + tp4.volume_to_sea;
      double disch_to_sea = vol_to_sea / o.t_cycle;

      double salt_load_sea = mt_sea / o.t_cycle;
      double sal_to_sea = (mt_sea + vol_from_sea * p->salinity_sea) / vol_to_sea;

      // Put the main results in the output stucture
      results->mass_transport_lake = mt_lake;
      results->salt_load_lake = salt_load_lake;
      results->discharge_from_lake = disch_from_lake;
      results->discharge_to_lake = disch_to_lake;
      results->salinity_to_lake = sal_to_lake;

      results->mass_transport_sea = mt_sea;
      results->salt_load_sea = salt_load_sea;
      results->discharge_from_sea = disch_from_sea;
      results->discharge_to_sea = disch_to_sea;
      results->salinity_to_sea = sal_to_sea;

      // Additional results. Only interesting when one wants to get a closer
      // understanding of what is going on, what happens in each phase, etc.
      if (aux_results != NULL) {
        // Equivalent full lock exchanges
        aux_results->z_fraction = 0.5 * (mt_lake + mt_sea) /
                                  (0.5 * (o.volume_lock_at_lake + o.volume_lock_at_sea) *
                                   (p->salinity_sea - p->salinity_lake));

        // Dimensionless door open time
        double sal_diff = p->salinity_sea - p->salinity_lake;
        double head_avg = 0.5 * (p->head_sea + p->head_lake);
        double velocity_exchange =
            0.5 * sqrt(o.g * 0.8 * sal_diff / o.density_average * (head_avg - p->lock_bottom));
        double t_lock_exchange = 2 * p->lock_length / velocity_exchange;

        aux_results->dimensionless_door_open_time = t_lock_exchange / o.t_open;

        // Volumes from/to lake and sea
        aux_results->volume_to_lake = vol_to_lake;
        aux_results->volume_from_lake = vol_from_lake;
        aux_results->volume_to_sea = vol_to_sea;
        aux_results->volume_from_sea = vol_from_sea;

        // Dependent parameters
        aux_results->volume_lock_at_lake = o.volume_lock_at_lake;
        aux_results->volume_lock_at_sea = o.volume_lock_at_sea;

        aux_results->t_cycle = o.t_cycle;
        aux_results->t_open = o.t_open;
        aux_results->t_open_lake = o.t_open_lake;
        aux_results->t_open_sea = o.t_open_sea;

        // Salinities after each phase
        aux_results->salinity_lock_1 = sal_lock_1;
        aux_results->salinity_lock_2 = sal_lock_2;
        aux_results->salinity_lock_3 = sal_lock_3;
        aux_results->salinity_lock_4 = sal_lock_4;

        // Transports in each phase
        memcpy(&aux_results->transports_phase_1, &tp1, sizeof(zsf_phase_transports_t));
        memcpy(&aux_results->transports_phase_2, &tp2, sizeof(zsf_phase_transports_t));
        memcpy(&aux_results->transports_phase_3, &tp3, sizeof(zsf_phase_transports_t));
        memcpy(&aux_results->transports_phase_4, &tp4, sizeof(zsf_phase_transports_t));
      }

      break;
    }
  }

  return ZSF_SUCCESS;
}
