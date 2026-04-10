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

static void test_sealock_update__constituent_results_match_salinity_mixing_fraction(void) {
  // Verify that passive constituents are transported using the same mixing
  // fraction as salinity. That is:
  //
  //   (constituent_to_x - constituent_lake) / (constituent_sea - constituent_lake)
  //     == (salinity_to_x - sal_lake) / (sal_sea - sal_lake)
  //
  // Arrange
  csv_row_t rows[2];
  time_t times[2];
  sealock_state_t lock = {0};
  setup_cycle_average_lock_without_file(&lock, rows, times);
  lock.num_constituents = 2;
  sealock_init(&lock, times[0], 1);

  // Constituent 0: lake=100, sea=200 (same direction as salinity gradient)
  lock.parameters3d.constituent_lake[0][0] = 100.0;
  lock.parameters3d.constituent_sea[0][0] = 200.0;

  // Constituent 1: lake=50, sea=0 (opposite direction)
  lock.parameters3d.constituent_lake[1][0] = 50.0;
  lock.parameters3d.constituent_sea[1][0] = 0.0;

  // Act
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_update(&lock, times[0] + 3600));

  // Assert
  double sal_lake = lock.parameters.salinity_lake;
  double sal_sea = lock.parameters.salinity_sea;
  double sal_range = sal_sea - sal_lake;

  // Mixing fraction implied by salinity result.
  double frac_lake = (lock.results3d.salinity_to_lake[0] - sal_lake) / sal_range;
  double frac_sea = (lock.results3d.salinity_to_sea[0] - sal_lake) / sal_range;

  // Constituent 0
  TEST_ASSERT_DOUBLE_WITHIN(1e-9, 100.0 + frac_lake * (200.0 - 100.0),
                            lock.results3d.constituent_to_lake[0][0]);
  TEST_ASSERT_DOUBLE_WITHIN(1e-9, 100.0 + frac_sea * (200.0 - 100.0),
                            lock.results3d.constituent_to_sea[0][0]);

  // Constituent 1
  TEST_ASSERT_DOUBLE_WITHIN(1e-9, 50.0 + frac_lake * (0.0 - 50.0),
                            lock.results3d.constituent_to_lake[1][0]);
  TEST_ASSERT_DOUBLE_WITHIN(1e-9, 50.0 + frac_sea * (0.0 - 50.0),
                            lock.results3d.constituent_to_sea[1][0]);

  free(lock.times);
}

static void test_sealock_update__constituent_results_zero_when_no_constituents(void) {
  // When num_constituents == 0, the constituent result arrays must remain zero.

  // Arrange
  csv_row_t rows[2];
  time_t times[2];
  sealock_state_t lock = {0};
  setup_cycle_average_lock_without_file(&lock, rows, times);
  lock.num_constituents = 0;
  sealock_init(&lock, times[0], 1);

  time_t start = times[0];
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_init(&lock, start, 1));

  // Act
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_update(&lock, start + 3600));

  // Assert: all constituent result arrays remain zero.
  double zeros[MAX_NUM_VOLUMES] = {0};
  for (unsigned int c = 0; c < MAX_NUM_CONSTITUENTS; c++) {
    TEST_ASSERT_EQUAL_MEMORY(zeros, lock.results3d.constituent_to_lake[c],
                             MAX_NUM_VOLUMES * sizeof(double));
    TEST_ASSERT_EQUAL_MEMORY(zeros, lock.results3d.constituent_to_sea[c],
                             MAX_NUM_VOLUMES * sizeof(double));
  }

  free(lock.times);
}

static void test_sealock_update__constituent_results_equal_lake_when_no_salinity_gradient(void) {
  // When sal_sea == sal_lake (no gradient), frac = 0 and constituent_to_x
  // must equal constituent_lake.

  // Arrange
  csv_row_t rows[2];
  time_t times[2];
  sealock_state_t lock = {0};
  setup_cycle_average_lock_without_file(&lock, rows, times);
  lock.num_constituents = 1;
  sealock_init(&lock, times[0], 1);

  time_t start = times[0];
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_init(&lock, start, 1));

  // Force equal salinities so sal_range == 0.
  lock.parameters.salinity_lake = 5.0;
  lock.parameters.salinity_sea = 5.0;
  lock.parameters3d.salinity_lake[0] = 5.0;
  lock.parameters3d.salinity_sea[0] = 5.0;
  lock.phase_state.salinity_lock = 5.0;

  lock.parameters3d.constituent_lake[0][0] = 42.0;
  lock.parameters3d.constituent_sea[0][0] = 99.0;

  // Act
  TEST_ASSERT_EQUAL(SEALOCK_OK, sealock_update(&lock, start + 3600));

  // Assert: no gradient -> constituent_to_x == constituent_lake
  TEST_ASSERT_DOUBLE_WITHIN(1e-9, 42.0, lock.results3d.constituent_to_lake[0][0]);
  TEST_ASSERT_DOUBLE_WITHIN(1e-9, 42.0, lock.results3d.constituent_to_sea[0][0]);

  free(lock.times);
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
  RUN_TEST(test_sealock_update__constituent_results_match_salinity_mixing_fraction);
  RUN_TEST(test_sealock_update__constituent_results_zero_when_no_constituents);
  RUN_TEST(test_sealock_update__constituent_results_equal_lake_when_no_salinity_gradient);

  return UNITY_END();
}
