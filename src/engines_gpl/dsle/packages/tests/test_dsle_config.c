#include "dsle_config.h"
#include "ini/ini_read.h"
#include "unity.h"

void setUp(void) {}

void tearDown(void) {}

static void test_dsle_config_load_and_unload(void) {
  // Arrange
  dsle_config_t config = {0};

  // Act
  int result = dsle_config_load(&config, "test_data/dsle_config/config.ini");

  // Assert
  TEST_ASSERT_EQUAL(INI_OK, result);

  TEST_ASSERT_EQUAL(timestamp_to_time(197001011200.0), config.start_time);
  TEST_ASSERT_EQUAL(timestamp_to_time(197001021200.0), config.end_time);
  TEST_ASSERT_EQUAL(3, config.max_num_z_layers);

  TEST_ASSERT_EQUAL(2, config.num_locks);
  sealock_state_t *first_lock = &config.locks[0];
  sealock_state_t *second_lock = &config.locks[1];

  TEST_ASSERT_EQUAL_STRING("test_sealock_1", config.locks[0].id);
  TEST_ASSERT_EQUAL(cycle_average_mode, config.locks[0].computation_mode);

  TEST_ASSERT_EQUAL_STRING("test_sealock_2", config.locks[1].id);
  TEST_ASSERT_EQUAL(phase_wise_mode, config.locks[1].computation_mode);

  for (int i = 0; i < config.num_locks; ++i) {
    sealock_state_t *lock_ptr = &config.locks[i];

    TEST_ASSERT_EQUAL(0.0, lock_ptr->phase_state.head_lock);
    TEST_ASSERT_EQUAL(1.0, lock_ptr->phase_state.salinity_lock);
    TEST_ASSERT_EQUAL(2.0, lock_ptr->phase_state.saltmass_lock);
    TEST_ASSERT_EQUAL(3.0, lock_ptr->parameters.temperature_lake);
    TEST_ASSERT_EQUAL(3.0, lock_ptr->parameters.temperature_sea);
    TEST_ASSERT_EQUAL(4.0, lock_ptr->phase_state.volume_ship_in_lock);

    TEST_ASSERT_EQUAL(5.0, lock_ptr->parameters.lock_length);
    TEST_ASSERT_EQUAL(6.0, lock_ptr->parameters.lock_width);
    TEST_ASSERT_EQUAL(7.0, lock_ptr->parameters.lock_bottom);
  }

  dsle_config_unload(&config);
  TEST_ASSERT_EQUAL(0, config.num_locks);
}

static void test_dsle_config_get_lock_index__lock_found(void) {
  // Arrange
  dsle_config_t config = {
      .num_locks = 3,
      .locks = {
        [0] = {.id = "foo"},
        [1] = {.id = "bar"},
        [2] = {.id = "baz"},
    },
  };

  // Act
  sealock_index_t result = dsle_config_get_lock_index(&config, "baz");

  // Assert
  TEST_ASSERT_EQUAL(2, result);
}

static void test_dsle_config_get_lock_index__lock_not_found(void) {
  // Arrange
  dsle_config_t config = {
      .num_locks = 3,
      .locks = {
        [0] = {.id = "foo"},
        [1] = {.id = "bar"},
        [2] = {.id = "baz"},
    },
  };

  // Act
  sealock_index_t result = dsle_config_get_lock_index(&config, "qux");

  // Assert
  TEST_ASSERT_EQUAL(-1, result);
}

int main(void) {
  UNITY_BEGIN();
  RUN_TEST(test_dsle_config_load_and_unload);
  RUN_TEST(test_dsle_config_get_lock_index__lock_found);
  RUN_TEST(test_dsle_config_get_lock_index__lock_not_found);
  return UNITY_END();
}
