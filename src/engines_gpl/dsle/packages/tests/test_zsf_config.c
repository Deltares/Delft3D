#include "zsf_config.h"
#include "unity.h"

void setUp(void) {}

void tearDown(void) {}

static void test_zsf_config_get_lock_index__lock_found(void) {
    zsf_config_t config = {
        .num_locks = 3,
        .locks = {
            [0] = {.id = "foo"},
            [1] = {.id = "bar"},
            [2] = {.id = "baz"},
        }
    };

    sealock_index_t result = zsf_config_get_lock_index(&config, "baz");
    
    TEST_ASSERT_EQUAL(2, result);
}

static void test_zsf_config_get_lock_index__lock_not_found(void) {
    zsf_config_t config = {
        .num_locks = 3,
        .locks = {
            [0] = {.id = "foo"},
            [1] = {.id = "bar"},
            [2] = {.id = "baz"},
        }
    };

    sealock_index_t result = zsf_config_get_lock_index(&config, "qux");
    
    TEST_ASSERT_EQUAL(-1, result);
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_zsf_config_get_lock_index__lock_found);
    RUN_TEST(test_zsf_config_get_lock_index__lock_not_found);
    return UNITY_END();
}