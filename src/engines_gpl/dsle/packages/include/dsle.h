/*****************************************************************************
 * dsle.h: dsle public header
 *****************************************************************************/

// We want to be compatible with as many languages as possible. Most compilers
// do 8-byte alignment, but VBA wants structs packed to 4-byte boundaries.
// Other languages have different assumptions. We try to keep everything
// packed at 8-bytes ourselves, by only using 8-byte types.

#ifndef DSLE_DSLE_H
#define DSLE_DSLE_H

#if defined(_WIN32)
#  if defined DSLE_STATIC
#    define DSLE_EXPORT
#  elif defined DSLE_EXPORTS
#    define DSLE_EXPORT __declspec(dllexport)
#  else
#    define DSLE_EXPORT __declspec(dllimport)
#  endif
#elif defined(__CYGWIN__)
#  define DSLE_EXPORT
#else
#  if (defined __GNUC__ && __GNUC__ >= 4) || defined __INTEL_COMPILER
#    define DSLE_EXPORT __attribute__((visibility("default")))
#  else
#    define DSLE_EXPORT
#  endif
#endif

#if (defined DSLE_USE_STDCALL) && (defined _WIN32)
#  define DSLE_CALLCONV __stdcall
#else
#  define DSLE_CALLCONV
#endif

// A custom value to signify "not specified"
#define DSLE_NAN -999.0

#ifdef __cplusplus
extern "C" {
#endif

typedef struct dsle_param_t {
  double lock_length;
  double lock_width;
  double lock_bottom;
  double num_cycles;
  double door_time_to_open;
  double leveling_time;
  double calibration_coefficient;
  double symmetry_coefficient;
  double ship_volume_sea_to_lake;
  double ship_volume_lake_to_sea;
  double salinity_lock;
  double head_sea;
  double salinity_sea;
  double temperature_sea;
  double head_lake;
  double salinity_lake;
  double temperature_lake;
  double flushing_discharge_high_tide;
  double flushing_discharge_low_tide;
  double density_current_factor_sea;
  double density_current_factor_lake;
  double distance_door_bubble_screen_sea;
  double distance_door_bubble_screen_lake;
  double sill_height_sea;
  double sill_height_lake;
  double rtol;
  double atol;
  double allowed_head_difference;
} dsle_param_t;

typedef struct dsle_results_t {
  double mass_transport_lake;
  double salt_load_lake;
  double discharge_from_lake;
  double discharge_to_lake;
  double salinity_to_lake;

  double mass_transport_sea;
  double salt_load_sea;
  double discharge_from_sea;
  double discharge_to_sea;
  double salinity_to_sea;
} dsle_results_t;

/* Structs when stepping through phases explicitly, i.e. not looping until steady */
typedef struct dsle_phase_state_t {
  double salinity_lock;
  double saltmass_lock;
  double head_lock;
  double volume_ship_in_lock;
} dsle_phase_state_t;

/* Per phase we calculate the mass transports and volume transports over the
   lock gates/openings. A positive values means "from lake to lock" or "from lock
   to sea". */
typedef struct dsle_phase_transports_t {
  double mass_transport_lake;
  double volume_from_lake;
  double volume_to_lake;
  double discharge_from_lake;
  double discharge_to_lake;
  double salinity_to_lake;

  double mass_transport_sea;
  double volume_from_sea;
  double volume_to_sea;
  double discharge_from_sea;
  double discharge_to_sea;
  double salinity_to_sea;
} dsle_phase_transports_t;

typedef struct dsle_aux_results_t {
  double z_fraction;
  double dimensionless_door_open_time;
  double volume_to_lake;
  double volume_from_lake;
  double volume_to_sea;
  double volume_from_sea;
  double volume_lock_at_lake;
  double volume_lock_at_sea;
  double t_cycle;
  double t_open;
  double t_open_lake;
  double t_open_sea;
  double salinity_lock_1;
  double salinity_lock_2;
  double salinity_lock_3;
  double salinity_lock_4;
  dsle_phase_transports_t transports_phase_1;
  dsle_phase_transports_t transports_phase_2;
  dsle_phase_transports_t transports_phase_3;
  dsle_phase_transports_t transports_phase_4;
} dsle_aux_results_t;

/* dsle_initialize_state:
 *      fill dsle_state_t with an initial condition for an empty (no ships) lock */
DSLE_EXPORT int DSLE_CALLCONV dsle_initialize_state(const dsle_param_t *p, dsle_phase_state_t *state,
                                                 double salinity_lock, double head_lock);

/* dsle_step_phase_1:
 *      Perform step 1: levelling to lake side */
DSLE_EXPORT int DSLE_CALLCONV dsle_step_phase_1(const dsle_param_t *p, double t_level,
                                             dsle_phase_state_t *state,
                                             dsle_phase_transports_t *results);

/* dsle_step_phase_2:
 *      Perform step 1: door open to lake side (ships out, lock exchange + flushing, ships in) */
DSLE_EXPORT int DSLE_CALLCONV dsle_step_phase_2(const dsle_param_t *p, double t_open_lake,
                                             dsle_phase_state_t *state,
                                             dsle_phase_transports_t *results);

/* dsle_step_phase_3:
 *      Perform step 1: levelling to sea side */
DSLE_EXPORT int DSLE_CALLCONV dsle_step_phase_3(const dsle_param_t *p, double t_level,
                                             dsle_phase_state_t *state,
                                             dsle_phase_transports_t *results);

/* dsle_step_phase_4:
 *      Perform step 1: door open to sea side (ships out, lock exchange + flushing, ships in) */
DSLE_EXPORT int DSLE_CALLCONV dsle_step_phase_4(const dsle_param_t *p, double t_open_sea,
                                             dsle_phase_state_t *state,
                                             dsle_phase_transports_t *results);

/* dsle_step_flush_doors_closed:
 *      Doors closed, but still flushing. */
DSLE_EXPORT int DSLE_CALLCONV dsle_step_flush_doors_closed(const dsle_param_t *p, double t_flushing,
                                                        dsle_phase_state_t *state,
                                                        dsle_phase_transports_t *results);

/* dsle_param_default:
 *      fill dsle_param_t with default values */
DSLE_EXPORT void DSLE_CALLCONV dsle_param_default(dsle_param_t *p);

/* dsle_calc_steady:
 *      calculate the salt intrusion for a set of parameters, assuming steady operation*/
DSLE_EXPORT int DSLE_CALLCONV dsle_calc_steady(const dsle_param_t *p, dsle_results_t *results,
                                            dsle_aux_results_t *aux_results);
/* dsle_error_msg:
 *      Get error messeage corresponding to error code */
DSLE_EXPORT const char *DSLE_CALLCONV dsle_error_msg(int code);

/* dsle_version:
 *      Get version string */
DSLE_EXPORT const char *DSLE_CALLCONV dsle_version();

#ifdef __cplusplus
}
#endif

#endif
