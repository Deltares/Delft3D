#include "timestamp.h"
#include "unity.h"

#include <stdlib.h>
#include <string.h>
#include <time.h>

void setUp(void) {}

void tearDown(void) {}

static int get_tz_offset(time_t time) {
  struct tm buf;
  localtime_r(&time, &buf);
  return buf.tm_gmtoff;
}

static void test_timestamp_conversion(void) {
  char *local_timestamp = "202409181404";
  time_t time_minus_local_tz = timestamp_string_to_time(local_timestamp, NULL);
  double local_timestamp_as_double = time_to_timestamp(time_minus_local_tz);
  time_t time_at_utc = 1726668240;

  int local_tz_offset = get_tz_offset(time_minus_local_tz);
  TEST_ASSERT_EQUAL_INT64(time_at_utc, time_minus_local_tz + local_tz_offset);
  TEST_ASSERT_EQUAL_DOUBLE(202409181404.0, local_timestamp_as_double);
  TEST_ASSERT_EQUAL_INT64(time_at_utc, timestamp_to_time(local_timestamp_as_double) + local_tz_offset);
}

static void test_timestamp_advance(void) {
  double local_timestamp = 202409181326.0;
  time_t t1_minus_local_tz = timestamp_to_time(local_timestamp);
  time_t t1_at_utc = 1726665960;
  int local_tz_offset = get_tz_offset(t1_minus_local_tz);
  TEST_ASSERT_EQUAL_INT64(t1_at_utc, t1_minus_local_tz + local_tz_offset);

  time_t t2 = t1_minus_local_tz + 60;
  double d2 = time_to_timestamp(t2);
  TEST_ASSERT_EQUAL_DOUBLE_MESSAGE(202409181327.0, d2,
                                   "Adding 60 seconds did not advance timestamp by 1 minute.");
}

static void test_timestamp_arrays(void) {
  // Test array conversion.
  double d_arr[3] = {197001020000.0, 200001011234.0, 202409181326.0};
  time_t t_at_utc[3] = {86400, 946730040, 1726665960};
  time_t *t_arr = timestamp_array_to_times(d_arr, 3);
  time_t t_refs[3];
  for (int i = 0; i < 3; ++i) {
    t_refs[i] = t_arr[i] + get_tz_offset(t_arr[i]);
  }

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