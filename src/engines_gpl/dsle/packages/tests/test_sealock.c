#include "load_phase_wise.h"
#include "sealock.h"
#include "timestamp.h"
#include "unity.h"

#include <string.h>

// defined in `load_phase_wise.c`
extern int init_phase_wise_timeseries_csv_context(csv_context_t *context);

// defined in `load_time_averaged.c`
extern int init_time_averaged_timeseries_csv_context(csv_context_t *context);

void setUp(void) {}

void tearDown(void) {}

// Populates a lock with the equivalent of loading time_averaged.csv,
// without touching the filesystem. Mirrors the two data rows in the file:
//   197001011200.0: ship_vol_l2s=1, ship_vol_s2l=2, door_time=3, level=4,
//                   dc_factor_sea=5, dc_factor_lake=6, num_cycle=7, flush=8
//   197001021200.0: ship_vol_l2s=9, ship_vol_s2l=10, ...etc
static void setup_cycle_average_lock_without_file(sealock_state_t *lock, csv_row_t rows[2],
                                                  time_t times[2]) {
  sealock_defaults(lock);
  lock->computation_mode = cycle_average_mode;
  lock->operational_parameters_file = NULL;

  rows[0][0] = (csv_value_t){.type = double_type, .data.double_value = 197001011200.0};
  rows[0][1] = (csv_value_t){.type = double_type, .data.double_value = 1.0};
  rows[0][2] = (csv_value_t){.type = double_type, .data.double_value = 2.0};
  rows[0][3] = (csv_value_t){.type = double_type, .data.double_value = 3.0};
  rows[0][4] = (csv_value_t){.type = double_type, .data.double_value = 4.0};
  rows[0][5] = (csv_value_t){.type = double_type, .data.double_value = 5.0};
  rows[0][6] = (csv_value_t){.type = double_type, .data.double_value = 6.0};
  rows[0][7] = (csv_value_t){.type = double_type, .data.double_value = 7.0};
  rows[0][8] = (csv_value_t){.type = double_type, .data.double_value = 8.0};

  rows[1][0] = (csv_value_t){.type = double_type, .data.double_value = 197001021200.0};
  rows[1][1] = (csv_value_t){.type = double_type, .data.double_value = 9.0};
  rows[1][2] = (csv_value_t){.type = double_type, .data.double_value = 10.0};
  rows[1][3] = (csv_value_t){.type = double_type, .data.double_value = 11.0};
  rows[1][4] = (csv_value_t){.type = double_type, .data.double_value = 12.0};
  rows[1][5] = (csv_value_t){.type = double_type, .data.double_value = 13.0};
  rows[1][6] = (csv_value_t){.type = double_type, .data.double_value = 14.0};
  rows[1][7] = (csv_value_t){.type = double_type, .data.double_value = 15.0};
  rows[1][8] = (csv_value_t){.type = double_type, .data.double_value = 16.0};

  init_time_averaged_timeseries_csv_context(&lock->timeseries_data);
  lock->timeseries_data.num_rows = 2;
  lock->timeseries_data.row_cap = 2;
  lock->timeseries_data.rows = rows;

  times[0] = timestamp_to_time(197001011200.0);
  times[1] = timestamp_to_time(197001021200.0);
  lock->times = times;
  lock->times_len = 2;
  lock->current_row = NO_CURRENT_ROW;
}

static void test_sealock_defaults(void) {
  sealock_state_t lock = {0};

  int result = sealock_defaults(&lock);

  TEST_ASSERT_EQUAL(0, result);

  dsle_phase_wise_args_t expected_phase_args = PHASE_WISE_CLEAR_ARGS();
  TEST_ASSERT_EQUAL_MEMORY(&expected_phase_args, &lock.phase_args, sizeof(dsle_phase_wise_args_t));

  // Assert lock.parameters is default.
  dsle_param_t expected_parameters;
  dsle_param_default(&expected_parameters);
  expected_parameters.allowed_head_difference = 0.1;
  TEST_ASSERT_EQUAL_MEMORY(&expected_parameters, &lock.parameters, sizeof(dsle_param_t));

  // Assert dfm_volume_t's are default.
  dfm_volumes_t *volumes[] = {
      &lock.from_lake_volumes,
      &lock.from_sea_volumes,
      &lock.to_lake_volumes,
      &lock.to_sea_volumes,
      NULL,
  };
  for (dfm_volumes_t **volumes_ptr = volumes; *volumes_ptr != NULL; ++volumes_ptr) {
    dfm_volumes_t *volumes = *volumes_ptr;

    TEST_ASSERT_EQUAL(1, volumes->num_volumes);
    TEST_ASSERT_EQUAL(1.0, volumes->volumes[0]);
    TEST_ASSERT_EQUAL(0, volumes->first_active_cell);
    TEST_ASSERT_EQUAL(1, volumes->num_active_cells);
  }

  // Assert flow profile is default.
  profile_t *profile = &lock.flow_profile;
  TEST_ASSERT_EQUAL(2, profile->number_of_positions);

  double expected_discharge[2] = {1.0, 1.0};
  TEST_ASSERT_EQUAL_MEMORY(expected_discharge, profile->relative_discharge_from_lock,
                           2 * sizeof(double));

  double expected_z_position[2] = {0.0, 1.0};
  TEST_ASSERT_EQUAL_MEMORY(expected_z_position, profile->relative_z_position, 2 * sizeof(double));
}

static void test_sealock_init(void) {
  // Arrange
  sealock_state_t lock = {
      .computation_mode = cycle_average_mode,
      .operational_parameters_file = "test_data/sealock/time_averaged.csv",
  };
  time_t time = timestamp_to_time(197001011200.0);

  // Act
  int status = sealock_init(&lock, time, 3);

  // Assert
  TEST_ASSERT_EQUAL(SEALOCK_OK, status);

  TEST_ASSERT_EQUAL(3, lock.from_lake_volumes.num_volumes);
  TEST_ASSERT_EQUAL(3, lock.from_sea_volumes.num_volumes);
  TEST_ASSERT_EQUAL(3, lock.to_lake_volumes.num_volumes);
  TEST_ASSERT_EQUAL(3, lock.to_sea_volumes.num_volumes);

  TEST_ASSERT_EQUAL(0.0, lock.phase_state.salinity_lock);
  TEST_ASSERT_EQUAL(0.0, lock.phase_state.saltmass_lock);
  TEST_ASSERT_EQUAL(0.0, lock.phase_state.head_lock);
  TEST_ASSERT_EQUAL(0.0, lock.phase_state.volume_ship_in_lock);
}

static int double_setter(void *struct_ptr, csv_value_t value) {
  double *double_ptr = (double *)struct_ptr;
  if (double_ptr == NULL || value.type != double_type) {
    return CSV_ERROR;
  }
  *double_ptr = value.data.double_value;
  return CSV_OK;
}

static void test_sealock_set_parameters_for_time__cycle_average_mode(void) {
  // Arrange
  time_t times[] = {0, 10, 20};
  csv_row_t rows[3];
  rows[0][0] = (csv_value_t){.type = double_type, .data.double_value = 41.};
  rows[1][0] = (csv_value_t){.type = double_type, .data.double_value = 42.};
  rows[2][0] = (csv_value_t){.type = double_type, .data.double_value = 43.};

  sealock_state_t lock = (sealock_state_t){
      .computation_mode = cycle_average_mode,
      .current_row = NO_CURRENT_ROW,
      .times = times,
      .times_len = 3,
  };

  csv_context_t *csv_context = &lock.timeseries_data;
  init_csv_context(csv_context);
  csv_context->num_rows = 3;
  csv_context->rows = rows;
  csv_context->num_columns = 1;
  csv_context->num_column_defs = 1;
  csv_context->column_def_index[0] = 0;
  csv_context->column_defs[0] =
      (csv_column_def_t){.label = "the_answer_to_life_the_universe_and_everything",
                         .value_type = double_type,
                         .setter = double_setter};

  // Act
  int result = sealock_set_parameters_for_time(&lock, 15);

  // Assert
  TEST_ASSERT_EQUAL(SEALOCK_OK, result);
  TEST_ASSERT_EQUAL(1, lock.current_row);

  double expected_parameter = 42.0;
  TEST_ASSERT_EQUAL_MEMORY(&expected_parameter, &lock.parameters, sizeof(double));
}

static void make_phase_wise_csv_row(phase_wise_row_t in, csv_row_t out) {
  csv_row_t result = {
      {.type = double_type, .data.double_value = in.time},
      {.type = int_type, .data.int_value = in.routine},
      {.type = double_type, .data.double_value = in.ship_volume_lake_to_sea},
      {.type = double_type, .data.double_value = in.ship_volume_sea_to_lake},
      {.type = double_type, .data.double_value = in.t_flushing},
      {.type = double_type, .data.double_value = in.t_level},
      {.type = double_type, .data.double_value = in.t_open_lake},
      {.type = double_type, .data.double_value = in.t_open_sea},
      {.type = double_type, .data.double_value = in.density_current_factor_lake},
      {.type = double_type, .data.double_value = in.density_current_factor_sea},
      {.type = double_type, .data.double_value = in.distance_door_bubble_screen_lake},
      {.type = double_type, .data.double_value = in.distance_door_bubble_screen_sea},
      {.type = double_type, .data.double_value = in.flushing_discharge_high_tide},
      {.type = double_type, .data.double_value = in.flushing_discharge_low_tide},
      {.type = double_type, .data.double_value = in.sill_height_lake},
      {.type = double_type, .data.double_value = in.sill_height_sea},
  };

  memset(out, 0, sizeof(csv_row_t));
  memcpy(out, result, 16 * sizeof(csv_value_t));
}

static void make_phase_wise_csv_context(csv_row_t *rows, int num_rows, csv_context_t *out) {
  init_phase_wise_timeseries_csv_context(out);
  out->num_columns = 16; // phase_wise_row_t has 16 fields.
  int column_def_index[16] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
  memcpy(out->column_def_index, column_def_index, 16 * sizeof(int));
  out->num_rows = num_rows;
  out->row_cap = num_rows;
  out->rows = rows;
}

// Populates a lock with the equivalent of loading phase_wise.csv row 0,
// without touching the filesystem. Mirrors the first data row:
//   197001011200.0: routine=1, t_level=5.0, dc_factor_lake=8.0, dc_factor_sea=9.0, ...
static void setup_phase_wise_lock_without_file(sealock_state_t *lock, csv_row_t rows[2],
                                               time_t times[2]) {
  sealock_defaults(lock);
  lock->computation_mode = phase_wise_mode;
  lock->operational_parameters_file = NULL;

  make_phase_wise_csv_row(
      (phase_wise_row_t){
          .time = 197001011200.0,
          .routine = 1,
          .ship_volume_lake_to_sea = 2.0,
          .ship_volume_sea_to_lake = 3.0,
          .t_flushing = 4.0,
          .t_level = 5.0,
          .t_open_lake = 6.0,
          .t_open_sea = 7.0,
          .density_current_factor_lake = 8.0,
          .density_current_factor_sea = 9.0,
          .distance_door_bubble_screen_lake = 10.0,
          .distance_door_bubble_screen_sea = 11.0,
          .flushing_discharge_high_tide = 12.0,
          .flushing_discharge_low_tide = 13.0,
          .sill_height_lake = 14.0,
          .sill_height_sea = 15.0,
      },
      rows[0]);

  make_phase_wise_csv_row(
      (phase_wise_row_t){
          .time = 197001021200.0,
          .routine = 16,
          .ship_volume_lake_to_sea = 17.0,
          .ship_volume_sea_to_lake = 18.0,
          .t_flushing = 19.0,
          .t_level = 20.0,
          .t_open_lake = 21.0,
          .t_open_sea = 22.0,
          .density_current_factor_lake = 23.0,
          .density_current_factor_sea = 24.0,
          .distance_door_bubble_screen_lake = 25.0,
          .distance_door_bubble_screen_sea = 26.0,
          .flushing_discharge_high_tide = 27.0,
          .flushing_discharge_low_tide = 28.0,
          .sill_height_lake = 29.0,
          .sill_height_sea = 30.0,
      },
      rows[1]);

  make_phase_wise_csv_context(rows, 2, &lock->timeseries_data);

  times[0] = timestamp_to_time(197001011200.0);
  times[1] = timestamp_to_time(197001021200.0);
  lock->times = times;
  lock->times_len = 2;
  lock->current_row = NO_CURRENT_ROW;
}


static void test_sealock_set_parameters_for_time__phase_wise_mode__routine_negative(void) {
  // Arrange
  time_t time = 1;
  sealock_state_t lock = {
      .computation_mode = phase_wise_mode,
      .current_row = NO_CURRENT_ROW,
      .times = &time,
      .times_len = 1,
  };
  csv_row_t row;
  make_phase_wise_csv_row(
      (phase_wise_row_t){
          .routine = -1,
          .time = 1.0,
          .ship_volume_lake_to_sea = 2.0,
          .ship_volume_sea_to_lake = 3.0,
          .t_flushing = 4.0,
          .t_level = 5.0,
          .t_open_lake = 6.0,
          .t_open_sea = 7.0,
          .density_current_factor_lake = 8.0,
          .density_current_factor_sea = 9.0,
          .distance_door_bubble_screen_lake = 10.0,
          .distance_door_bubble_screen_sea = 11.0,
          .flushing_discharge_high_tide = 12.0,
          .flushing_discharge_low_tide = 13.0,
          .sill_height_lake = 14.0,
          .sill_height_sea = 15.0,
      },
      row);
  make_phase_wise_csv_context(&row, 1, &lock.timeseries_data);

  // Act
  int result = sealock_set_parameters_for_time(&lock, 0);

  // Assert
  TEST_ASSERT_EQUAL(SEALOCK_OK, result);
  TEST_ASSERT_EQUAL(0, lock.current_row);

  TEST_ASSERT_EQUAL(1, lock.phase_args.run_update);
  TEST_ASSERT_EQUAL(-1, lock.phase_args.routine);

  TEST_ASSERT_EQUAL(8., lock.parameters.density_current_factor_lake);
  TEST_ASSERT_EQUAL(9., lock.parameters.density_current_factor_sea);
  TEST_ASSERT_EQUAL(0., lock.parameters.ship_volume_sea_to_lake);
  TEST_ASSERT_EQUAL(0., lock.parameters.ship_volume_lake_to_sea);
  TEST_ASSERT_EQUAL(10., lock.parameters.distance_door_bubble_screen_lake);
  TEST_ASSERT_EQUAL(11., lock.parameters.distance_door_bubble_screen_sea);
  TEST_ASSERT_EQUAL(12., lock.parameters.flushing_discharge_high_tide);
  TEST_ASSERT_EQUAL(13., lock.parameters.flushing_discharge_low_tide);
  TEST_ASSERT_EQUAL(14., lock.parameters.sill_height_lake);
  TEST_ASSERT_EQUAL(15., lock.parameters.sill_height_sea);

  TEST_ASSERT_EQUAL(4., lock.phase_args.duration);         // When routine < 0, t_flushing
  TEST_ASSERT_EQUAL(5, lock.phase_args.time_duration_end); // current_time + phase_args.duration
}

static void test_sealock_set_parameters_for_time__phase_wise_mode__routine_one(void) {
  // Arrange
  time_t time = 0;
  sealock_state_t lock = {
      .computation_mode = phase_wise_mode,
      .current_row = NO_CURRENT_ROW,
      .times = &time,
      .times_len = 1,
  };
  csv_row_t row;
  make_phase_wise_csv_row((phase_wise_row_t){.routine = 1, .t_level = 42.0}, row);
  make_phase_wise_csv_context(&row, 1, &lock.timeseries_data);

  // Act
  int result = sealock_set_parameters_for_time(&lock, 0);

  // Assert
  TEST_ASSERT_EQUAL(SEALOCK_OK, result);
  TEST_ASSERT_EQUAL(0, lock.current_row);
  TEST_ASSERT_EQUAL(42.0, lock.phase_args.duration); // Duration set to t_level
}

static void test_sealock_set_parameters_for_time__phase_wise_mode__routine_two(void) {
  // Arrange
  time_t time = 0;
  sealock_state_t lock = {
      .computation_mode = phase_wise_mode,
      .current_row = NO_CURRENT_ROW,
      .times = &time,
      .times_len = 1,
  };
  csv_row_t row;
  make_phase_wise_csv_row(
      (phase_wise_row_t){.routine = 2, .t_open_lake = 42.0, .ship_volume_lake_to_sea = 43.0}, row);

  csv_context_t *csv_context = &lock.timeseries_data;
  init_phase_wise_timeseries_csv_context(csv_context);
  make_phase_wise_csv_context(&row, 1, &lock.timeseries_data);

  // Act
  int result = sealock_set_parameters_for_time(&lock, 0);

  // Assert
  TEST_ASSERT_EQUAL(SEALOCK_OK, result);
  TEST_ASSERT_EQUAL(0, lock.current_row);
  TEST_ASSERT_EQUAL(42.0, lock.phase_args.duration); // Duration set to t_open_lake
  TEST_ASSERT_EQUAL(43.0, lock.parameters.ship_volume_lake_to_sea);
}

static void test_sealock_set_parameters_for_time__phase_wise_mode__routine_three(void) {
  // Arrange
  time_t time = 0;
  sealock_state_t lock = {
      .computation_mode = phase_wise_mode,
      .current_row = NO_CURRENT_ROW,
      .times = &time,
      .times_len = 1,
  };
  csv_row_t row;
  make_phase_wise_csv_row((phase_wise_row_t){.routine = 3, .t_level = 42.0}, row);
  make_phase_wise_csv_context(&row, 1, &lock.timeseries_data);

  // Act
  int result = sealock_set_parameters_for_time(&lock, 0);

  // Assert
  TEST_ASSERT_EQUAL(SEALOCK_OK, result);
  TEST_ASSERT_EQUAL(0, lock.current_row);
  TEST_ASSERT_EQUAL(42.0, lock.phase_args.duration); // Duration set to t_level
}

static void test_sealock_set_parameters_for_time__phase_wise_mode__routine_four(void) {
  // Arrange
  time_t time = 0;
  sealock_state_t lock = {
      .computation_mode = phase_wise_mode,
      .current_row = NO_CURRENT_ROW,
      .times = &time,
      .times_len = 1,
  };
  csv_row_t row;
  make_phase_wise_csv_row(
      (phase_wise_row_t){.routine = 4, .t_open_sea = 42.0, .ship_volume_sea_to_lake = 43.0}, row);
  make_phase_wise_csv_context(&row, 1, &lock.timeseries_data);

  // Act
  int result = sealock_set_parameters_for_time(&lock, 0);

  // Assert
  TEST_ASSERT_EQUAL(SEALOCK_OK, result);
  TEST_ASSERT_EQUAL(0, lock.current_row);
  TEST_ASSERT_EQUAL(42.0, lock.phase_args.duration); // Duration set to t_open_sea
  TEST_ASSERT_EQUAL(43.0, lock.parameters.ship_volume_sea_to_lake);
}

static void test_sealock_load_timeseries__time_averaged_mode(void) {
  // Arrange
  sealock_state_t lock = {
      .computation_mode = cycle_average_mode,
      .current_row = NO_CURRENT_ROW,
  };

  // Act
  int result = sealock_load_timeseries(&lock, "test_data/sealock/time_averaged.csv");

  // Assert
  TEST_ASSERT_EQUAL(cycle_average_mode, lock.computation_mode);
  TEST_ASSERT_EQUAL(2, lock.times_len);
  TEST_ASSERT_NOT_NULL(lock.times);
  TEST_ASSERT_EQUAL(timestamp_to_time(197001011200.0), lock.times[0]);
  TEST_ASSERT_EQUAL(timestamp_to_time(197001021200.0), lock.times[1]);
  free(lock.times); // Allocated by `timestamp_array_to_times`.
  TEST_ASSERT_EQUAL(SEALOCK_OK, result);
}

static void test_sealock_load_timeseries__time_averaged_mode__time_non_increasing(void) {
  // Arrange
  sealock_state_t lock = {
      .computation_mode = cycle_average_mode,
      .current_row = NO_CURRENT_ROW,
  };

  // Act
  int result = sealock_load_timeseries(&lock, "test_data/sealock/time_averaged_non_increasing.csv");

  // Assert
  TEST_ASSERT_NULL(lock.times);
  TEST_ASSERT_EQUAL(SEALOCK_ERROR, result);
}

static void test_sealock_load_timeseries__phase_wise_mode(void) {
  // Arrange
  sealock_state_t lock = {
      .computation_mode = phase_wise_mode,
      .current_row = NO_CURRENT_ROW,
  };

  // Act
  int result = sealock_load_timeseries(&lock, "test_data/sealock/phase_wise.csv");

  // Assert
  TEST_ASSERT_EQUAL(phase_wise_mode, lock.computation_mode);
  TEST_ASSERT_EQUAL(2, lock.times_len);
  TEST_ASSERT_NOT_NULL(lock.times);
  TEST_ASSERT_EQUAL(timestamp_to_time(197001011200.0), lock.times[0]);
  TEST_ASSERT_EQUAL(timestamp_to_time(197001021200.0), lock.times[1]);
  free(lock.times); // Allocated by `timestamp_array_to_times`.
  TEST_ASSERT_EQUAL(SEALOCK_OK, result);
}

static void test_sealock_delta_time_ok__times_len_one__always_ok(void) {
  time_t time = 0;
  sealock_state_t lock = {.times_len = 1, .times = &time};

  int ok = sealock_delta_time_ok(&lock, 0, time);

  TEST_ASSERT_EQUAL(1, ok);
}

static void test_sealock_delta_time_ok(void) {
  time_t times[] = {0, 11, 22};
  sealock_state_t lock = {.times_len = 3, .times = times};

  int ok = sealock_delta_time_ok(&lock, 10, times[0]);

  TEST_ASSERT_EQUAL(1, ok);
}

static void test_sealock_delta_time_ok__diff_eq_delta_time__not_ok(void) {
  time_t times[] = {0, 9, 19};
  sealock_state_t lock = {.times_len = 3, .times = times};

  int ok = sealock_delta_time_ok(&lock, 10, times[0]);

  TEST_ASSERT_EQUAL(0, ok);
}

static void test_sealock_init__reserves_temperature_slot(void) {
  csv_row_t rows[2];
  time_t times[2];
  sealock_state_t lock = {0};
  setup_cycle_average_lock_without_file(&lock, rows, times);
  lock.num_constituents = 0;

  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_init(&lock, times[0], 1));

  TEST_ASSERT_EQUAL(1u, lock.num_constituents);
}

static void test_sealock_init__reserves_temperature_slot_after_user_constituents(void) {
  csv_row_t rows[2];
  time_t times[2];
  sealock_state_t lock = {0};
  setup_cycle_average_lock_without_file(&lock, rows, times);
  lock.num_constituents = 2;

  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_init(&lock, times[0], 1));

  TEST_ASSERT_EQUAL(3u, lock.num_constituents);
}

static void test_sealock_update__constituent_results_cycle_average(void) {
  // Verify that passive constituents are transported using the same mixing
  // fraction as salinity.

  // Arrange
  csv_row_t rows[2];
  time_t times[2];
  sealock_state_t lock = {0};
  setup_cycle_average_lock_without_file(&lock, rows, times);
  lock.num_constituents = 2;
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_init(&lock, times[0], 1));

  // Set boundary conditions AFTER sealock_init to avoid being overwritten.
  // head_lake/sea survive because the CSV setters don't touch them.
  // salinity must go into parameters3d because sealock_collect_layers
  // reads from there on every update call.
  lock.parameters.head_lake = 0.0;
  lock.parameters.head_sea = 0.5;
  lock.parameters3d.salinity_lake[0] = 0.0;
  lock.parameters3d.salinity_sea[0] = 30.0;

  // Constituent 0: lake=100, sea=200
  lock.parameters3d.constituent_lake[0][0] = 100.0;
  lock.parameters3d.constituent_sea[0][0] = 200.0;

  // Constituent 1: lake=50, sea=0 (opposite direction)
  lock.parameters3d.constituent_lake[1][0] = 50.0;
  lock.parameters3d.constituent_sea[1][0] = 0.0;

  // Act
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_update(&lock, times[0] + 3600));

  // Assert: constituent fraction == salinity fraction, per side.
  double sal_lake = lock.parameters.salinity_lake; // populated by sealock_collect_layers
  double sal_sea = lock.parameters.salinity_sea;
  double sal_range = sal_sea - sal_lake;

  double sal_to_lake = lock.results3d.salinity_to_lake[0];
  double sal_to_sea = lock.results3d.salinity_to_sea[0];

  // Salinity results must be finite and within [sal_lake, sal_sea].
  TEST_ASSERT_TRUE(sal_to_lake >= sal_lake && sal_to_lake <= sal_sea);
  TEST_ASSERT_TRUE(sal_to_sea >= sal_lake && sal_to_sea <= sal_sea);

  double frac_lake = (sal_to_lake - sal_lake) / sal_range;
  double frac_sea = (sal_to_sea - sal_lake) / sal_range;

  // Constituent 0
  TEST_ASSERT_DOUBLE_WITHIN(1e-9, 100.0 + frac_lake * 100.0,
                            lock.results3d.constituent_to_lake[0][0]);
  TEST_ASSERT_DOUBLE_WITHIN(1e-9, 100.0 + frac_sea * 100.0,
                            lock.results3d.constituent_to_sea[0][0]);

  // Constituent 1
  TEST_ASSERT_DOUBLE_WITHIN(1e-9, 50.0 + frac_lake * (0.0 - 50.0),
                            lock.results3d.constituent_to_lake[1][0]);
  TEST_ASSERT_DOUBLE_WITHIN(1e-9, 50.0 + frac_sea * (0.0 - 50.0),
                            lock.results3d.constituent_to_sea[1][0]);
}

static void test_sealock_update__constituent_results_zero_when_no_constituents(void) {
  // When num_constituents is 0 before init (only the temperature slot is reserved),
  // all user constituent result slots must remain zero.

  // Arrange
  csv_row_t rows[2];
  time_t times[2];
  sealock_state_t lock = {0};
  setup_cycle_average_lock_without_file(&lock, rows, times);
  lock.num_constituents = 0;
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_init(&lock, times[0], 1));
  // After init, num_constituents == 1 (temperature slot only). No user slots.

  // Act
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_update(&lock, times[0] + 3600));

  // Assert: all slots beyond the temperature slot remain zero.
  double zeros[MAX_NUM_VOLUMES] = {0};
  for (unsigned int c = 1; c < MAX_NUM_CONSTITUENTS; c++) {
    TEST_ASSERT_EQUAL_MEMORY(zeros, lock.results3d.constituent_to_lake[c],
                             MAX_NUM_VOLUMES * sizeof(double));
    TEST_ASSERT_EQUAL_MEMORY(zeros, lock.results3d.constituent_to_sea[c],
                             MAX_NUM_VOLUMES * sizeof(double));
  }
}

static void test_sealock_update__constituent_results_equal_cycle_avg_no_gradient(void) {
  // When sal_sea == sal_lake (no gradient), frac = 0 and constituent_to_x
  // must equal constituent_lake.

  // Arrange
  csv_row_t rows[2];
  time_t times[2];
  sealock_state_t lock = {0};
  setup_cycle_average_lock_without_file(&lock, rows, times);
  lock.num_constituents = 1;

  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_init(&lock, times[0], 1));

  // Force equal salinities so sal_range == 0.
  lock.parameters.salinity_lake = 5.0;
  lock.parameters.salinity_sea = 5.0;
  lock.parameters3d.salinity_lake[0] = 5.0;
  lock.parameters3d.salinity_sea[0] = 5.0;
  lock.phase_state.salinity_lock = 5.0;

  lock.parameters3d.constituent_lake[0][0] = 42.0;
  lock.parameters3d.constituent_sea[0][0] = 99.0;

  // Act
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_update(&lock, times[0] + 3600));

  // Assert: no gradient -> constituent_to_x == constituent_lake
  TEST_ASSERT_DOUBLE_WITHIN(1e-9, 42.0, lock.results3d.constituent_to_lake[0][0]);
  TEST_ASSERT_DOUBLE_WITHIN(1e-9, 42.0, lock.results3d.constituent_to_sea[0][0]);
}

static void test_sealock_update__phase_wise__constituent_lock_evolves_after_phase1(void) {
  // Phase 1 leveling: lock levels to lake side. Water flows from lake into
  // lock (or vice versa). constituent_lock must evolve via the volume mass
  // balance, not the salinity fraction.
  //
  // Setup: head_lock > head_lake -> water flows FROM lock TO lake.
  // vol_to_lake = (head_lock - head_lake) * lock_length * lock_width
  //             = (1.0 - 0.0) * 100.0 * 10.0 = 1000.0 m3
  // vol_from_lake = 0.0
  // volume_lock_before = lock_length * lock_width * (head_lock - lock_bottom)
  //                    = 100 * 10 * (1.0 - (-5.0)) = 6000.0 m3
  // volume_lock_after  = 100 * 10 * (0.0 - (-5.0)) = 5000.0 m3
  //
  // constituent mass balance:
  //   mass_after = c_lock * vol_before + 0 * c_lake - vol_to_lake * c_lock
  //              = c_lock * (vol_before - vol_to_lake)
  //              = c_lock * 5000.0
  //   c_lock_new = mass_after / vol_after = c_lock_old

  // Arrange
  csv_row_t rows[2];
  time_t times[2];
  sealock_state_t lock = {0};
  setup_phase_wise_lock_without_file(&lock, rows, times);
  lock.num_constituents = 1;
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_init(&lock, times[0], 1));

  // Set up a simple leveling scenario: lock is at sea level (1.0),
  // lake is at 0.0 -> water drains from lock to lake.
  lock.parameters.head_lake = 0.0;
  lock.parameters.head_sea = 1.0;
  lock.phase_state.head_lock = 1.0;
  lock.phase_state.salinity_lock = 10.0;
  lock.phase_state.saltmass_lock = 10.0 * lock.parameters.lock_length * lock.parameters.lock_width *
                                   (1.0 - lock.parameters.lock_bottom);
  lock.parameters3d.salinity_lake[0] = 0.0;
  lock.parameters3d.salinity_sea[0] = 30.0;

  // User constituent 0: lock starts at 50.0, lake=10.0, sea=90.0
  lock.constituent_lock[0] = 50.0;
  lock.parameters3d.constituent_lake[0][0] = 10.0;
  lock.parameters3d.constituent_sea[0][0] = 90.0;

  // Act
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_update(&lock, times[0] + 3600));

  // Assert: output equals constituent_lock (phase_wise_mode).
  TEST_ASSERT_DOUBLE_WITHIN(1e-6, lock.constituent_lock[0],
                            lock.results3d.constituent_to_lake[0][0]);
  TEST_ASSERT_DOUBLE_WITHIN(1e-6, lock.constituent_lock[0],
                            lock.results3d.constituent_to_sea[0][0]);

  // Assert: constituent_lock evolved via volume mass balance, not salinity fraction.
  // Since vol_from_lake=0, vol_to_lake=1000, vol_before=6000, vol_after=5000:
  // c_lock_new = (50.0 * 6000 + 0 - 1000 * 50.0) / 5000 = 50.0
  // (concentration unchanged because outflow carries lock concentration)
  TEST_ASSERT_DOUBLE_WITHIN(1e-6, 50.0, lock.constituent_lock[0]);
}

static void test_sealock_update__phase_wise__constituent_lock_mixes_on_inflow(void) {
  // Phase 1 leveling: head_lock < head_lake -> water flows FROM lake INTO lock.
  // vol_from_lake = (head_lake - head_lock) * lock_length * lock_width
  //              = (1.0 - 0.0) * 100.0 * 10.0 = 1000.0 m3
  // vol_to_lake   = 0.0
  // volume_lock_before = 100 * 10 * (0.0 - (-5.0)) = 5000.0 m3
  // volume_lock_after  = 100 * 10 * (1.0 - (-5.0)) = 6000.0 m3
  //
  // c_lock_new = (c_lock * 5000 + c_lake * 1000) / 6000
  //            = (50.0 * 5000 + 10.0 * 1000) / 6000
  //            = 260000 / 6000 = 43.333...

  // Arrange
  csv_row_t rows[2];
  time_t times[2];
  sealock_state_t lock = {0};
  setup_phase_wise_lock_without_file(&lock, rows, times);
  lock.num_constituents = 1;
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_init(&lock, times[0], 1));

  lock.parameters.head_lake = 1.0;
  lock.parameters.head_sea = 1.0;
  lock.phase_state.head_lock = 0.0;
  lock.phase_state.salinity_lock = 10.0;
  lock.phase_state.saltmass_lock = 10.0 * lock.parameters.lock_length * lock.parameters.lock_width *
                                   (0.0 - lock.parameters.lock_bottom);
  lock.parameters3d.salinity_lake[0] = 10.0;
  lock.parameters3d.salinity_sea[0] = 10.0;

  lock.constituent_lock[0] = 50.0;
  lock.parameters3d.constituent_lake[0][0] = 10.0;
  lock.parameters3d.constituent_sea[0][0] = 10.0;

  // Act
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_update(&lock, times[0] + 3600));

  // Assert: constituent_lock mixed with lake inflow.
  double expected = (50.0 * 5000.0 + 10.0 * 1000.0) / 6000.0;
  TEST_ASSERT_DOUBLE_WITHIN(1e-6, expected, lock.constituent_lock[0]);

  // Output must equal constituent_lock.
  TEST_ASSERT_DOUBLE_WITHIN(1e-6, lock.constituent_lock[0],
                            lock.results3d.constituent_to_lake[0][0]);
}
static void test_sealock_update__phase_wise__multiple_constituents_evolve_independently(void) {
  // Verify that two constituents with different concentrations each follow
  // their own mass balance independently — they must not interfere.
  //
  // Setup: head_lock < head_lake -> inflow from lake.
  // vol_from_lake = (1.0 - 0.0) * 100.0 * 10.0 = 1000.0 m3
  // volume_lock_before = 100 * 10 * (0.0 - (-5.0)) = 5000.0 m3
  // volume_lock_after  = 100 * 10 * (1.0 - (-5.0)) = 6000.0 m3
  //
  // Constituent 0: c_lock=50, c_lake=10
  //   c_lock_new = (50 * 5000 + 10 * 1000) / 6000 = 260000 / 6000 = 43.333...
  //
  // Constituent 1: c_lock=20, c_lake=80
  //   c_lock_new = (20 * 5000 + 80 * 1000) / 6000 = 180000 / 6000 = 30.0

  // Arrange
  csv_row_t rows[2];
  time_t times[2];
  sealock_state_t lock = {0};
  setup_phase_wise_lock_without_file(&lock, rows, times);
  lock.num_constituents = 2;
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_init(&lock, times[0], 1));

  lock.parameters.head_lake = 1.0;
  lock.parameters.head_sea = 1.0;
  lock.phase_state.head_lock = 0.0;
  lock.phase_state.salinity_lock = 10.0;
  lock.phase_state.saltmass_lock = 10.0 * lock.parameters.lock_length * lock.parameters.lock_width *
                                   (0.0 - lock.parameters.lock_bottom);
  lock.parameters3d.salinity_lake[0] = 10.0;
  lock.parameters3d.salinity_sea[0] = 10.0;

  lock.constituent_lock[0] = 50.0;
  lock.parameters3d.constituent_lake[0][0] = 10.0;
  lock.parameters3d.constituent_sea[0][0] = 10.0;

  lock.constituent_lock[1] = 20.0;
  lock.parameters3d.constituent_lake[1][0] = 80.0;
  lock.parameters3d.constituent_sea[1][0] = 80.0;

  // Act
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_update(&lock, times[0] + 3600));

  // Assert: each constituent evolved independently.
  double expected_0 = (50.0 * 5000.0 + 10.0 * 1000.0) / 6000.0;
  double expected_1 = (20.0 * 5000.0 + 80.0 * 1000.0) / 6000.0;

  TEST_ASSERT_DOUBLE_WITHIN(1e-6, expected_0, lock.constituent_lock[0]);
  TEST_ASSERT_DOUBLE_WITHIN(1e-6, expected_1, lock.constituent_lock[1]);

  // Outputs must equal their respective constituent_lock values.
  TEST_ASSERT_DOUBLE_WITHIN(1e-6, lock.constituent_lock[0],
                            lock.results3d.constituent_to_lake[0][0]);
  TEST_ASSERT_DOUBLE_WITHIN(1e-6, lock.constituent_lock[1],
                            lock.results3d.constituent_to_lake[1][0]);
}

static void test_sealock_init__temperature_slot_initialised_from_temperature_lake(void) {
  // sealock_init must copy parameters.temperature_lake into
  // constituent_lock[temp_slot] so that phase-wise temperature output
  // starts from the correct initial lock temperature.

  // Arrange
  csv_row_t rows[2];
  time_t times[2];
  sealock_state_t lock = {0};
  setup_phase_wise_lock_without_file(&lock, rows, times);
  lock.num_constituents = 0;
  // Set initial lock temperature via the field that dsle_ini_handler populates.
  lock.parameters.temperature_lake = 6.0;
  lock.parameters.temperature_sea = 20.0; // must not influence the initial lock value

  // Act
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_init(&lock, times[0], 1));

  // Assert: temperature slot (last slot) holds temperature_lake, not temperature_sea.
  unsigned int temp_slot = lock.num_constituents - 1;
  TEST_ASSERT_DOUBLE_WITHIN(1e-9, 6.0, lock.constituent_lock[temp_slot]);
}

static void test_sealock_update__phase_wise__initial_temperature_affects_output(void) {
  // Verify that the initial lock temperature (constituent_lock[temp_slot])
  // has an observable effect on output when it differs from the boundary
  // temperatures, even when lake == sea temperature.
  //
  // Setup: lake == sea temperature (10.0), but lock starts at 20.0.
  // head_lock < head_lake -> inflow from lake dilutes the lock.
  //
  // vol_from_lake = (1.0 - 0.0) * 100.0 * 10.0 = 1000.0 m3
  // volume_lock_before = 100 * 10 * (0.0 - (-5.0)) = 5000.0 m3
  // volume_lock_after  = 100 * 10 * (1.0 - (-5.0)) = 6000.0 m3
  //
  // c_lock_new = (20.0 * 5000 + 10.0 * 1000) / 6000 = 110000 / 6000 = 18.333...
  //
  // If constituent_lock were ignored and boundary values used instead,
  // output would be 10.0 (lake==sea, frac=0). The different value proves
  // constituent_lock is driving the result.

  // Arrange
  csv_row_t rows[2];
  time_t times[2];
  sealock_state_t lock = {0};
  setup_phase_wise_lock_without_file(&lock, rows, times);
  lock.num_constituents = 0;               // temperature slot only after init
  lock.parameters.temperature_lake = 20.0; // initial lock temperature (from ini)
  lock.parameters.temperature_sea = 20.0;
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_init(&lock, times[0], 1));

  lock.parameters.head_lake = 1.0;
  lock.parameters.head_sea = 1.0;
  lock.phase_state.head_lock = 0.0;
  lock.phase_state.salinity_lock = 10.0;
  lock.phase_state.saltmass_lock = 10.0 * lock.parameters.lock_length * lock.parameters.lock_width *
                                   (0.0 - lock.parameters.lock_bottom);
  lock.parameters3d.salinity_lake[0] = 10.0;
  lock.parameters3d.salinity_sea[0] = 10.0;

  // Boundary temperatures are both 10.0 — different from initial lock temp of 20.0.
  lock.parameters.temperature_lake = 10.0;
  lock.parameters.temperature_sea = 10.0;

  // Act
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_update(&lock, times[0] + 3600));

  // Assert: output reflects mixing of initial lock temperature with inflow,
  // not the boundary temperature alone.
  unsigned int temp_slot = lock.num_constituents - 1;
  double expected = (20.0 * 5000.0 + 10.0 * 1000.0) / 6000.0; // 18.333...

  TEST_ASSERT_DOUBLE_WITHIN(1e-6, expected, lock.constituent_lock[temp_slot]);
  TEST_ASSERT_DOUBLE_WITHIN(1e-6, expected, lock.results3d.constituent_to_lake[temp_slot][0]);

  // Explicitly verify it is NOT the boundary value.
  TEST_ASSERT_TRUE(lock.results3d.constituent_to_lake[temp_slot][0] > 10.0);
}

int main(void) {
  UNITY_BEGIN();

  RUN_TEST(test_sealock_defaults);
  RUN_TEST(test_sealock_set_parameters_for_time__cycle_average_mode);
  RUN_TEST(test_sealock_set_parameters_for_time__phase_wise_mode__routine_negative);
  RUN_TEST(test_sealock_set_parameters_for_time__phase_wise_mode__routine_one);
  RUN_TEST(test_sealock_set_parameters_for_time__phase_wise_mode__routine_two);
  RUN_TEST(test_sealock_set_parameters_for_time__phase_wise_mode__routine_three);
  RUN_TEST(test_sealock_set_parameters_for_time__phase_wise_mode__routine_four);
  RUN_TEST(test_sealock_load_timeseries__time_averaged_mode);
  RUN_TEST(test_sealock_load_timeseries__time_averaged_mode__time_non_increasing);
  RUN_TEST(test_sealock_load_timeseries__phase_wise_mode);
  RUN_TEST(test_sealock_init);
  RUN_TEST(test_sealock_delta_time_ok);
  RUN_TEST(test_sealock_delta_time_ok__times_len_one__always_ok);
  RUN_TEST(test_sealock_delta_time_ok__diff_eq_delta_time__not_ok);
  RUN_TEST(test_sealock_update__constituent_results_cycle_average);
  RUN_TEST(test_sealock_update__constituent_results_zero_when_no_constituents);
  RUN_TEST(test_sealock_update__constituent_results_equal_cycle_avg_no_gradient);
  RUN_TEST(test_sealock_init__reserves_temperature_slot);
  RUN_TEST(test_sealock_init__reserves_temperature_slot_after_user_constituents);
  RUN_TEST(test_sealock_update__phase_wise__constituent_lock_evolves_after_phase1);
  RUN_TEST(test_sealock_update__phase_wise__constituent_lock_mixes_on_inflow);
  RUN_TEST(test_sealock_update__phase_wise__multiple_constituents_evolve_independently);
  RUN_TEST(test_sealock_init__temperature_slot_initialised_from_temperature_lake);
  RUN_TEST(test_sealock_update__phase_wise__initial_temperature_affects_output);

  return UNITY_END();
}
