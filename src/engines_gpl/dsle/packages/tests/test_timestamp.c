#include "timestamp.h"
#include "unity.h"

#include <string.h>

void setUp(void) {}

void tearDown(void) {}

static void test_timestamp_conversion(void) {
  char *s = "202409181404";
  time_t t = timestamp_string_to_time(s, NULL);
  double d = time_to_timestamp(t);
  TEST_ASSERT_EQUAL_INT64(1726661040, t);
  TEST_ASSERT_EQUAL_DOUBLE(202409181404.0, d);
  TEST_ASSERT_EQUAL_INT64(1726661040, timestamp_to_time(d));
}

static void test_timestamp_advance(void) {
  double d1 = 202409181326.0;
  time_t t1 = timestamp_to_time(d1);
  TEST_ASSERT_EQUAL_INT64(1726658760, t1);

  time_t t2 = t1 + 60;
  double d2 = time_to_timestamp(t2);
  TEST_ASSERT_EQUAL_DOUBLE_MESSAGE(202409181327.0, d2,
                                   "Adding 60 seconds did not advance timestamp by 1 minute.");
}

static void test_timestamp_arrays(void) {
  // Test array conversion.
  double d_arr[3] = {197001010100.0, 200001011234.0, 202409181326.0};
  time_t t_refs[3] = {0, 946726440, 1726658760};
  time_t *t_arr = timestamp_array_to_times(d_arr, 3);
  TEST_ASSERT_INT64_ARRAY_WITHIN(0, t_refs, t_arr, 3);
  // Result should be strictly increasing.
  TEST_ASSERT(times_strictly_increasing(t_arr, 3) == 1);
  // Non strictly increasing times.
  time_t t_non_strict1[3] = {1726658760, 1726658760, 1726659000};
  TEST_ASSERT(times_strictly_increasing(t_non_strict1, 3) == 0);
  time_t t_non_strict2[3] = {946726440, 0, 1726658760};
  TEST_ASSERT(times_strictly_increasing(t_non_strict2, 3) == 0);
  // Cleanup
  free(t_arr);
}

int main(void) {
  UNITY_BEGIN();

  RUN_TEST(test_timestamp_conversion);
  RUN_TEST(test_timestamp_advance);
  RUN_TEST(test_timestamp_arrays);

  return UNITY_END();
}