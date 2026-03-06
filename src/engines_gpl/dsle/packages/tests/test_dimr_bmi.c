#include "dimr_bmi.h"
#include "dsle_config.h"
#include "timestamp.h"
#include "unity.h"
#include <string.h>

extern dsle_config_t config; // Declared in `dimr_bmi.c`

void setUp(void) {
  // Initialize the config.
  config = (dsle_config_t){
    .num_locks = 1,
    .max_num_z_layers = 1,
    .start_time = timestamp_to_time(197001011200.0), // YYYYMMDDhhmm
    .end_time = timestamp_to_time(197001021200.0),
    .current_time = timestamp_to_time(197001011200.0),
    .log_level = logDEBUG,
    .locks = {
      [0] = (sealock_state_t){
        .id = "test_sealock",
        .computation_mode = cycle_average_mode,
        .operational_parameters_file = "this-file-does-not-exist.csv",
        .phase_state = (dsle_phase_state_t) {
          .head_lock = 1.0,
          .salinity_lock = 2.0,
          .saltmass_lock = 3.0,
          .volume_ship_in_lock = 4.0,
        },
        .parameters = (dsle_param_t){
          .salinity_lock = 2.0,
          .lock_length = 5.0,
          .lock_width = 6.0,
          .lock_bottom = 7.0,
          .temperature_lake = 8.0,
          .temperature_sea = 9.0,
          .allowed_head_difference = 10.0,
        },
        .from_lake_volumes.num_volumes = 1,
        .from_sea_volumes.num_volumes = 1,
        .to_lake_volumes.num_volumes = 1,
        .to_sea_volumes.num_volumes = 1,
      }
    }
  };
}

void tearDown(void) {}

static void test_initialize(void) {
  // Act
  int status = initialize("test_data/dimr_bmi/config.ini");

  // Assert
  TEST_ASSERT_EQUAL(DIMR_BMI_OK, status);
  TEST_ASSERT_EQUAL(timestamp_to_time(202510221200.0), config.start_time);
  TEST_ASSERT_EQUAL(timestamp_to_time(202510221300.0), config.end_time);
  TEST_ASSERT_EQUAL(timestamp_to_time(202510221200.0), config.current_time);
  TEST_ASSERT_EQUAL(1, config.num_locks);

  sealock_state_t *lock = config.locks;

  TEST_ASSERT_EQUAL_STRING("ini_sealock", lock->id);
  TEST_ASSERT_EQUAL_STRING("test_data/dimr_bmi/test_sealock_cycle_average.csv", lock->operational_parameters_file);

  TEST_ASSERT_EQUAL(cycle_average_mode, lock->computation_mode);
  TEST_ASSERT_EQUAL(4.0, lock->parameters.temperature_lake);
  TEST_ASSERT_EQUAL(5.0, lock->parameters.lock_length);
  TEST_ASSERT_EQUAL(6.0, lock->parameters.lock_width);
  TEST_ASSERT_EQUAL(7.0, lock->parameters.lock_bottom);

  TEST_ASSERT_EQUAL(2, config.locks[0].times_len);
  time_t expected_times[2] = {timestamp_to_time(202510221200.0), timestamp_to_time(202510221300.0)};
  TEST_ASSERT_EQUAL_MEMORY(expected_times, config.locks[0].times, 2 * sizeof(double));

  finalize();
}

static void test_finalize(void) {
  // Arrange
  initialize("test_data/dimr_bmi/config.ini");

  // Act
  int status = finalize();

  // Assert
  TEST_ASSERT_EQUAL(DIMR_BMI_OK, status);

  csv_context_t *timeseries = &config.locks[0].timeseries_data;
  TEST_ASSERT_NULL(timeseries->rows);
  TEST_ASSERT_EQUAL(0, timeseries->num_rows);
  TEST_ASSERT_EQUAL(0, timeseries->num_columns);
  TEST_ASSERT_NULL(timeseries->filepath);

  TEST_ASSERT_EQUAL(0, config.num_locks);
}

#define TEST_GET_VAR(name, source) \
  static void test_get_var__##name(void) { test_get_var_parameterized(#name, (source)); }

static void test_get_var_parameterized(char *variable_name, double *source) {
  double *destination;
  int status = get_var(variable_name, &destination);
  TEST_ASSERT_EQUAL(DIMR_BMI_OK, status);
  TEST_ASSERT_EQUAL(source, destination);
}

TEST_GET_VAR(mass_transport_lake, config.locks[0].results3d.mass_transport_lake)
TEST_GET_VAR(salt_load_lake, config.locks[0].results3d.salt_load_lake)
TEST_GET_VAR(discharge_from_lake, config.locks[0].results3d.discharge_from_lake)
TEST_GET_VAR(discharge_to_lake, config.locks[0].results3d.discharge_to_lake)
TEST_GET_VAR(salinity_to_lake, config.locks[0].results3d.salinity_to_lake)
TEST_GET_VAR(mass_transport_sea, config.locks[0].results3d.mass_transport_sea)
TEST_GET_VAR(salt_load_sea, config.locks[0].results3d.salt_load_sea)
TEST_GET_VAR(discharge_from_sea, config.locks[0].results3d.discharge_from_sea)
TEST_GET_VAR(discharge_to_sea, config.locks[0].results3d.discharge_to_sea)
TEST_GET_VAR(salinity_to_sea, config.locks[0].results3d.salinity_to_sea)
TEST_GET_VAR(water_volume_from_lake, config.locks[0].from_lake_volumes.volumes)
TEST_GET_VAR(water_volume_from_sea, config.locks[0].from_sea_volumes.volumes)
TEST_GET_VAR(water_volume_to_lake, config.locks[0].to_lake_volumes.volumes)
TEST_GET_VAR(water_volume_to_sea, config.locks[0].to_sea_volumes.volumes)
TEST_GET_VAR(salinity_sea, config.locks[0].parameters3d.salinity_sea)
TEST_GET_VAR(salinity_lake, config.locks[0].parameters3d.salinity_lake)

static void test_get_var__unknown_var_name(void) {
  double *result;
  int status = get_var("the_answer_to_life_the_universe_and_everything", &result);
  TEST_ASSERT_EQUAL(DIMR_BMI_FAILURE, status);
}

#define TEST_SET_VAR(name, destination) \
  static void test_set_var__##name(void) { test_set_var_parameterized(#name, (destination)); }

static void test_set_var_parameterized(char *variable_name, double *destination) {
  // Arrange
  double orig_value = *destination;
  double new_value = 42.0;

  // Act
  int status = set_var(variable_name, &new_value);

  // Assert
  TEST_ASSERT_EQUAL(DIMR_BMI_OK, status);
  TEST_ASSERT_EQUAL(new_value, *destination);

  // Try to restore original value.
  *destination = orig_value;
}

TEST_SET_VAR(salinity_lake, config.locks[0].parameters3d.salinity_lake)
TEST_SET_VAR(head_lake, &config.locks[0].parameters.head_lake)
TEST_SET_VAR(salinity_sea, config.locks[0].parameters3d.salinity_sea)
TEST_SET_VAR(head_sea, &config.locks[0].parameters.head_sea)
TEST_SET_VAR(water_volume_from_lake, config.locks[0].from_lake_volumes.volumes)
TEST_SET_VAR(water_volume_from_sea, config.locks[0].from_sea_volumes.volumes)
TEST_SET_VAR(water_volume_to_lake, config.locks[0].to_lake_volumes.volumes)
TEST_SET_VAR(water_volume_to_sea, config.locks[0].to_sea_volumes.volumes)
TEST_SET_VAR(temperature_lake, &config.locks[0].parameters.temperature_lake)
TEST_SET_VAR(temperature_sea, &config.locks[0].parameters.temperature_sea)

static void test_set_var__unknown_var_name(void) {
  double value = 42.0;
  int status = set_var("the_answer_to_life_the_universe_and_everything", &value);
  TEST_ASSERT_EQUAL(DIMR_BMI_OK, status);  // Calling `set_var` on a non-existing variable is explicitly allowed.
}

#define TEST_GET_VAR_SHAPE(name, expected_dims) \
  static void test_get_var_shape__##name(void) { test_get_var_shape_parameterized(#name, (expected_dims)); }

static void test_get_var_shape_parameterized(char *variable_name, int expected_dims) {
  // Arrange
  int dims[DIMR_BMI_MAXDIMS];
  int expected_dims_array[DIMR_BMI_MAXDIMS];
  memset(expected_dims_array, 0, DIMR_BMI_MAXDIMS * sizeof(int));
  expected_dims_array[0] = expected_dims;

  // Act
  int status = get_var_shape(variable_name, dims);

  // Assert
  TEST_ASSERT_EQUAL(DIMR_BMI_OK, status);
  TEST_ASSERT_EQUAL_MEMORY(expected_dims_array, dims, DIMR_BMI_MAXDIMS * sizeof(int));
}

TEST_GET_VAR_SHAPE(mass_transport_lake, 1)
TEST_GET_VAR_SHAPE(salt_load_lake, 1)
TEST_GET_VAR_SHAPE(discharge_from_lake, 1)
TEST_GET_VAR_SHAPE(discharge_to_lake, 1)
TEST_GET_VAR_SHAPE(salinity_to_lake, 1)
TEST_GET_VAR_SHAPE(mass_transport_sea, 1)
TEST_GET_VAR_SHAPE(salt_load_sea, 1)
TEST_GET_VAR_SHAPE(discharge_from_sea, 1)
TEST_GET_VAR_SHAPE(discharge_to_sea, 1)
TEST_GET_VAR_SHAPE(salinity_to_sea, 1)
TEST_GET_VAR_SHAPE(water_volume_from_lake, 1)
TEST_GET_VAR_SHAPE(water_volume_from_sea, 1)
TEST_GET_VAR_SHAPE(water_volume_to_lake, 1)
TEST_GET_VAR_SHAPE(water_volume_to_sea, 1)

static void test_get_var_shape__unknown_var_name(void) {
  test_get_var_shape_parameterized("the_answer_to_life_the_universe_and_everything", 1);
}

static void test_version_string__is_not_empty(void) {
  char *version_string = NULL;
  get_version_string(&version_string);
  TEST_ASSERT(strlen(version_string) > 0);
}

static void test_get_start_time(void) {
  double start_time = 0.0;
  get_start_time(&start_time);
  TEST_ASSERT_EQUAL(197001011200.0, start_time);
}

static void test_get_end_time(void) {
  double end_time = 0.0;
  get_end_time(&end_time);
  TEST_ASSERT_EQUAL(197001021200.0, end_time);
}

static void test_get_current_time(void) {
  double current_time = 0.0;
  get_current_time(&current_time);
  TEST_ASSERT_EQUAL(197001011200.0, current_time); // `current_time` is `start_time` after initialization.
}

int main(void) {
  UNITY_BEGIN();

  RUN_TEST(test_initialize);
  RUN_TEST(test_finalize);

  RUN_TEST(test_get_var__mass_transport_lake);
  RUN_TEST(test_get_var__salt_load_lake);
  RUN_TEST(test_get_var__discharge_from_lake);
  RUN_TEST(test_get_var__discharge_to_lake);
  RUN_TEST(test_get_var__salinity_to_lake);
  RUN_TEST(test_get_var__mass_transport_sea);
  RUN_TEST(test_get_var__salt_load_sea);
  RUN_TEST(test_get_var__discharge_from_sea);
  RUN_TEST(test_get_var__discharge_to_sea);
  RUN_TEST(test_get_var__salinity_to_sea);
  RUN_TEST(test_get_var__water_volume_from_lake);
  RUN_TEST(test_get_var__water_volume_from_sea);
  RUN_TEST(test_get_var__water_volume_to_lake);
  RUN_TEST(test_get_var__water_volume_to_sea);
  RUN_TEST(test_get_var__salinity_sea);
  RUN_TEST(test_get_var__salinity_lake);
  RUN_TEST(test_get_var__unknown_var_name);

  RUN_TEST(test_set_var__salinity_lake);
  RUN_TEST(test_set_var__head_lake);
  RUN_TEST(test_set_var__salinity_sea);
  RUN_TEST(test_set_var__head_sea);
  RUN_TEST(test_set_var__water_volume_from_lake);
  RUN_TEST(test_set_var__water_volume_from_sea);
  RUN_TEST(test_set_var__water_volume_to_lake);
  RUN_TEST(test_set_var__water_volume_to_sea);
  RUN_TEST(test_set_var__temperature_lake);
  RUN_TEST(test_set_var__temperature_sea);
  RUN_TEST(test_set_var__unknown_var_name);

  RUN_TEST(test_get_var_shape__mass_transport_lake);
  RUN_TEST(test_get_var_shape__salt_load_lake);
  RUN_TEST(test_get_var_shape__discharge_from_lake);
  RUN_TEST(test_get_var_shape__discharge_to_lake);
  RUN_TEST(test_get_var_shape__salinity_to_lake);
  RUN_TEST(test_get_var_shape__mass_transport_sea);
  RUN_TEST(test_get_var_shape__salt_load_sea);
  RUN_TEST(test_get_var_shape__discharge_from_sea);
  RUN_TEST(test_get_var_shape__discharge_to_sea);
  RUN_TEST(test_get_var_shape__salinity_to_sea);
  RUN_TEST(test_get_var_shape__water_volume_from_lake);
  RUN_TEST(test_get_var_shape__water_volume_from_sea);
  RUN_TEST(test_get_var_shape__water_volume_to_lake);
  RUN_TEST(test_get_var_shape__water_volume_to_sea);
  RUN_TEST(test_get_var_shape__unknown_var_name);

  RUN_TEST(test_version_string__is_not_empty);
  RUN_TEST(test_get_start_time);
  RUN_TEST(test_get_end_time);
  RUN_TEST(test_get_current_time);

  return UNITY_END();
}
