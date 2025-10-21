#include "dimr_bmi.h"
#include "timestamp.h"
#include "unity.h"
#include <string.h>

void setUp(void) {
  initialize("test_data/dimr_bmi/config.ini");
}

void tearDown(void) {
  finalize();
}

#define TEST_GET_VAR(name) \
  static void test_get_var__##name(void) { test_get_var_parameterized(#name); }

static void test_get_var_parameterized(const char *variable_name) {
  double *result;
  int status = get_var(variable_name, &result);
  TEST_ASSERT_EQUAL(DIMR_BMI_OK, status);
}

TEST_GET_VAR(mass_transport_lake)
TEST_GET_VAR(salt_load_lake)
TEST_GET_VAR(discharge_from_lake)
TEST_GET_VAR(discharge_to_lake)
TEST_GET_VAR(salinity_to_lake)
TEST_GET_VAR(mass_transport_sea)
TEST_GET_VAR(salt_load_sea)
TEST_GET_VAR(discharge_from_sea)
TEST_GET_VAR(discharge_to_sea)
TEST_GET_VAR(salinity_to_sea)
TEST_GET_VAR(water_volume_from_lake)
TEST_GET_VAR(water_volume_from_sea)
TEST_GET_VAR(water_volume_to_lake)
TEST_GET_VAR(water_volume_to_sea)
TEST_GET_VAR(salinity_sea)
TEST_GET_VAR(salinity_lake)

static void test_get_var__unknown_var_name(void) {
  double *result;
  int status = get_var("the_answer_to_life_the_universe_and_everything", &result);
  TEST_ASSERT_EQUAL(DIMR_BMI_FAILURE, status);
}

#define TEST_SET_VAR(name) \
  static void test_set_var__##name(void) { test_set_var_parameterized(#name); }

static void test_set_var_parameterized(const char *variable_name) {
  double value = 42.0;
  int status = set_var(variable_name, &value);
  TEST_ASSERT_EQUAL(DIMR_BMI_OK, status);
}

TEST_SET_VAR(salinity_lake)
TEST_SET_VAR(head_lake)
TEST_SET_VAR(salinity_sea)
TEST_SET_VAR(head_sea)
TEST_SET_VAR(water_volume_from_lake)
TEST_SET_VAR(water_volume_from_sea)
TEST_SET_VAR(water_volume_to_lake)
TEST_SET_VAR(water_volume_to_sea)
TEST_SET_VAR(temperature_lake)
TEST_SET_VAR(temperature_sea)

static void test_set_var__unknown_var_name(void) {
  double value = 42.0;
  int status = set_var("the_answer_to_life_the_universe_and_everything", &value);
  TEST_ASSERT_EQUAL(DIMR_BMI_OK, status);  // Calling `set_var` on a non-existing variable is explicitly allowed.
}

#define TEST_GET_VAR_SHAPE(name) \
  static void test_get_var_shape__##name(void) { test_get_var_shape_parameterized(#name); }

static void test_get_var_shape_parameterized(char *variable_name) {
  // Arrange
  int dims[DIMR_BMI_MAXDIMS];
  int expected_dims[DIMR_BMI_MAXDIMS];
  memset(expected_dims, 0, DIMR_BMI_MAXDIMS * sizeof(int));
  expected_dims[0] = 1;

  // Act
  int status = get_var_shape(variable_name, dims);

  // Assert
  TEST_ASSERT_EQUAL(DIMR_BMI_OK, status);
  TEST_ASSERT_EQUAL_MEMORY(expected_dims, dims, DIMR_BMI_MAXDIMS * sizeof(int));
}

TEST_GET_VAR_SHAPE(mass_transport_lake)
TEST_GET_VAR_SHAPE(salt_load_lake)
TEST_GET_VAR_SHAPE(discharge_from_lake)
TEST_GET_VAR_SHAPE(discharge_to_lake)
TEST_GET_VAR_SHAPE(salinity_to_lake)
TEST_GET_VAR_SHAPE(mass_transport_sea)
TEST_GET_VAR_SHAPE(salt_load_sea)
TEST_GET_VAR_SHAPE(discharge_from_sea)
TEST_GET_VAR_SHAPE(discharge_to_sea)
TEST_GET_VAR_SHAPE(salinity_to_sea)
TEST_GET_VAR_SHAPE(water_volume_from_lake)
TEST_GET_VAR_SHAPE(water_volume_from_sea)
TEST_GET_VAR_SHAPE(water_volume_to_lake)
TEST_GET_VAR_SHAPE(water_volume_to_sea)

static void test_get_var_shape__unknown_var_name(void) {
  test_get_var_shape_parameterized("the_answer_to_life_the_universe_and_everything");
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
