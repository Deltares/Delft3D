#include "io_layer_distribution.h"
#include "unity.h"

static const double epsilon = 1.0e-8;

void setUp(void) {}

void tearDown(void) {}

// Create a linear profile starting with discharge_0 at z = 0 and discharge_1 at z = 1.
// Use number_of_positions nodes in the piecewise linear profile.
static int create_linear_profile(profile_t *profile, const int number_of_positions,
                                 const double discharge_0, const double discharge_1) {
  profile->number_of_positions = number_of_positions;
  profile->relative_z_position = malloc(number_of_positions * sizeof(double));
  profile->relative_discharge_from_lock = malloc(number_of_positions * sizeof(double));
  profile->start_sign = discharge_0 < 0 ? -1 : 1;
  if (discharge_0 * discharge_1 < 0) {
    profile->relative_z_zero = discharge_0 / (discharge_0 - discharge_1);
  } else {
    profile->relative_z_zero = discharge_0 < 0 ? 0.0 : 1.0;
  }

  if (profile->relative_z_position == NULL || profile->relative_discharge_from_lock == NULL) {
    return -1;
  }

  const double slope = (discharge_1 - discharge_0) / 1.0;
  const double z_step = 1.0 / (number_of_positions - 1.0);

  for (int i = 0; i < number_of_positions; ++i) {
    const double z = z_step * i;
    const double d = discharge_0 + slope * z;
    profile->relative_z_position[i] = z;
    profile->relative_discharge_from_lock[i] = d;
  }
  return 0;
}

static double expected_integration_result(const double lower_bound, const double upper_bound,
                                          const double discharge_0, const double discharge_1) {
  const double slope = (discharge_1 - discharge_0) / 1.0;
  return discharge_0 * (upper_bound - lower_bound) +
         0.5 * slope * (pow(upper_bound, 2.0) - pow(lower_bound, 2.0));
}

static void test_integrate_constant_profile(void) {
  const double constant_value = 1.0;
  const double lower_bound = 0.3;
  const double upper_bound = 0.7;
  const int number_of_positions = 2;
  profile_t constant_profile = profile_default;
  TEST_ASSERT_EQUAL(
      create_linear_profile(&constant_profile, number_of_positions, constant_value, constant_value),
      0);

  const double result =
      integrate_piecewise_linear_profile(&constant_profile, lower_bound, upper_bound);
  TEST_ASSERT_DOUBLE_WITHIN(
      epsilon,
      expected_integration_result(lower_bound, upper_bound, constant_value, constant_value),
      result);
  cleanup_profile(&constant_profile);
}

static void test_integrate_to_edges(void) {
  const double constant_value = 0.618;
  const double lower_bound = 0.0;
  const double upper_bound = 1.0;
  const int number_of_positions = 2;
  profile_t constant_profile = profile_default;
  TEST_ASSERT_EQUAL(
      create_linear_profile(&constant_profile, number_of_positions, constant_value, constant_value),
      0);

  const double result =
      integrate_piecewise_linear_profile(&constant_profile, lower_bound, upper_bound);
  TEST_ASSERT_DOUBLE_WITHIN(
      epsilon,
      expected_integration_result(lower_bound, upper_bound, constant_value, constant_value),
      result);
  cleanup_profile(&constant_profile);
}

static void test_integrate_linear_profile(void) {
  const double lower_bound = 0.273;
  const double upper_bound = 0.81;
  const double left_value = 0.1;
  const double right_value = 4.33;
  const int number_of_positions = 6;
  profile_t linear_profile = profile_default;
  TEST_ASSERT_EQUAL(
      create_linear_profile(&linear_profile, number_of_positions, left_value, right_value), 0);

  const double result =
      integrate_piecewise_linear_profile(&linear_profile, lower_bound, upper_bound);
  TEST_ASSERT_DOUBLE_WITHIN(
      epsilon, expected_integration_result(lower_bound, upper_bound, left_value, right_value),
      result);
  cleanup_profile(&linear_profile);
}

static void test_distribute_over_two_layers(void) {
  // Create a normalized profile, so that calculating the expected outcome is simpler
  const double left_profile_value = 0.0;
  const double right_profile_value = 2.0;
  profile_t linear_profile = profile_default;
  TEST_ASSERT_EQUAL(
      create_linear_profile(&linear_profile, 10, left_profile_value, right_profile_value), 0);

  layers_t two_layers = layers_default;
  two_layers.number_of_layers = 2;
  two_layers.normalized_target_volumes = malloc(sizeof(double) * two_layers.number_of_layers);
  TEST_ASSERT_NOT_EQUAL(two_layers.normalized_target_volumes, NULL);
  two_layers.normalized_target_volumes[0] = 0.3;
  two_layers.normalized_target_volumes[1] = 0.7;

  const double total_discharge = 95.13;
  double discharges[2] = {0.0, 0.0};
  layered_discharge_t result = layered_discharge_default;
  result.number_of_layers = 2;
  result.discharge_per_layer = discharges;
  TEST_ASSERT_EQUAL(
      distribute_discharge_over_layers(total_discharge, &linear_profile, &two_layers, &result), 0);

  TEST_ASSERT_EQUAL(result.number_of_layers, two_layers.number_of_layers);
  const double expected_first_discharge =
      total_discharge * pow(two_layers.normalized_target_volumes[0], 2.0);
  const double expected_second_discharge = total_discharge - expected_first_discharge;
  TEST_ASSERT_DOUBLE_WITHIN(epsilon, expected_first_discharge, result.discharge_per_layer[0]);
  TEST_ASSERT_DOUBLE_WITHIN(epsilon, expected_second_discharge, result.discharge_per_layer[1]);

  cleanup_profile(&linear_profile);
  cleanup_layers(&two_layers);
}

static void test_normalize_profile() {
  // Create a non-normalized profile.
  const double left_profile_value = -2.0;
  const double right_profile_value = 2.0;
  profile_t linear_profile = profile_default;
  TEST_ASSERT_EQUAL(0,
      create_linear_profile(&linear_profile, 10, left_profile_value, right_profile_value));
  TEST_ASSERT_EQUAL(0, io_normalize_profile(&linear_profile));
  TEST_ASSERT_DOUBLE_WITHIN(epsilon,-4.0, linear_profile.relative_discharge_from_lock[0]);
  TEST_ASSERT_DOUBLE_WITHIN(epsilon, 4.0, linear_profile.relative_discharge_from_lock[9]);
  cleanup_profile(&linear_profile);
}

int main(void) {
  UNITY_BEGIN();

  RUN_TEST(test_integrate_constant_profile);
  RUN_TEST(test_integrate_to_edges);
  RUN_TEST(test_integrate_linear_profile);
  RUN_TEST(test_distribute_over_two_layers);
  RUN_TEST(test_normalize_profile);

  return UNITY_END();
}
