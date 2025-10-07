

#ifndef SEALOCK_H
#define SEALOCK_H

#include "csv/load_csv.h"
#include "zsf.h"
#include "io_layer_distribution.h"
#include <time.h>

#define SEALOCK_OK (0)
#define SEALOCK_ERROR (-1)

#if defined(__cplusplus)
extern "C" {
#endif

typedef int sealock_index_t;

typedef enum zsf_computation_mode_enum {
  cycle_average_mode = 0,
  phase_wise_mode
} zsf_computation_mode_t;

typedef struct zsf_phase_wise_args_struct {
  int run_update;
  int routine;
  time_t time;              // current time
  time_t time_step;         // time step taken from previous time to current time
  time_t time_duration_end; // time when the phase ends.
  double duration;          // duration of the active phase.
} zsf_phase_wise_args_t;

#define PHASE_WISE_CLEAR_ARGS()                                                                     \
  (zsf_phase_wise_args_t) {                                                                         \
    .run_update = 0, .routine = 0, .time = 0, .time_step = 0, .time_duration_end = 0, .duration = 0 \
  }

#define MAX_NUM_VOLUMES 50
#define NO_CURRENT_ROW (size_t)(-1)

typedef struct dfm_volumes_struct {
  // configured
  unsigned int num_volumes;
  // input from fdm
  double volumes[MAX_NUM_VOLUMES];
  // determined from volumes
  double normalized[MAX_NUM_VOLUMES];
  unsigned num_active_cells;
  unsigned first_active_cell;
} dfm_volumes_t;

typedef struct dfm_parameters3d_struct {
  double salinity_lake[MAX_NUM_VOLUMES];
  double salinity_sea[MAX_NUM_VOLUMES];
} dfm_parameters3d_t;

typedef struct dfm_results3d_struct {
  // determined from results
  double mass_transport_lake[MAX_NUM_VOLUMES];
  double salt_load_lake[MAX_NUM_VOLUMES];
  double discharge_from_lake[MAX_NUM_VOLUMES];
  double discharge_to_lake[MAX_NUM_VOLUMES];
  double salinity_to_lake[MAX_NUM_VOLUMES];
  double mass_transport_sea[MAX_NUM_VOLUMES];
  double salt_load_sea[MAX_NUM_VOLUMES];
  double discharge_from_sea[MAX_NUM_VOLUMES];
  double discharge_to_sea[MAX_NUM_VOLUMES];
  double salinity_to_sea[MAX_NUM_VOLUMES];
} dfm_results3d_t;

typedef struct sealock_state_struct {
  char *id;
  // ZSF
  zsf_param_t parameters;
  zsf_phase_wise_args_t phase_args;
  zsf_phase_state_t phase_state;
  zsf_phase_transports_t phase_results;
  zsf_results_t results;
  zsf_aux_results_t aux_results;
  zsf_computation_mode_t computation_mode;
  // Cycle average
  char *operational_parameters_file;
  csv_context_t timeseries_data;
  size_t current_row;
  time_t *times;
  size_t times_len;
  profile_t flow_profile;
  // Volumes
  // Note that the 'to' and 'from' locations may differ
  // so their actual water volumes may be different and
  // should be treated as such.
  dfm_volumes_t from_lake_volumes;
  dfm_volumes_t from_sea_volumes;
  dfm_volumes_t to_lake_volumes;
  dfm_volumes_t to_sea_volumes;
  // 3D parameters
  dfm_parameters3d_t parameters3d;
  // Results split into layers
  dfm_results3d_t results3d;
} sealock_state_t;

int sealock_defaults(sealock_state_t *lock);
int sealock_init(sealock_state_t *lock, time_t start_time, unsigned int max_num_z_layers);
int sealock_load_timeseries(sealock_state_t *lock, char *filepath);
int sealock_set_parameters_for_time(sealock_state_t *lock, time_t time);
int sealock_update(sealock_state_t *lock, time_t time);
int sealock_delta_time_ok(sealock_state_t *lock, time_t delta_time, time_t start_time);

#if defined(__cplusplus)
}
#endif

#endif // SEALOCK_H
