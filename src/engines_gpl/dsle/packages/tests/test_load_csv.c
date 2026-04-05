#include "csv/load_csv.h"
#include "unity.h"

#include <string.h>

void setUp(void) {}

void tearDown(void) {}

typedef struct s_data {
  int64_t time;
  char *label;
  double value;
} t_data;

CSV_LONG_SETTER(t_data, time);
CSV_STRING_SETTER(t_data, label);
CSV_DOUBLE_SETTER(t_data, value);

static void test_read_csv() {
  csv_context_t ctx;
  t_data test_data;
  int64_t data_array[4];
  int status = CSV_OK;

  /* initialize context */
  status = init_csv_context(&ctx);
  TEST_ASSERT_MESSAGE(status == CSV_OK, "Failed to initialize csv context.");
  /* set up expected columns */
  status = def_csv_column(&ctx, "time", long_type, set_time);
  TEST_ASSERT_MESSAGE(status == CSV_OK, "Failed to define time column.");
  status = def_csv_column(&ctx, "value", double_type, set_value);
  TEST_ASSERT_MESSAGE(status == CSV_OK, "Failed to define value column.");
  status = def_csv_column(&ctx, "label", string_type, set_label);
  TEST_ASSERT_MESSAGE(status == CSV_OK, "Failed to define label column.");
  /* load csv file */
  status = load_csv(&ctx, "test_data/load_csv/test_csv_input.csv");
  TEST_ASSERT_MESSAGE(status == CSV_OK, "Failed to load CSV file.");
  /* get_csv_num_rows */
  TEST_ASSERT_EQUAL_size_t_MESSAGE(4, get_csv_num_rows(&ctx), "Unexpected number of rows.");
  /* get_csv_row_data */
  status = get_csv_row_data(&ctx, 1, &test_data);
  TEST_ASSERT_MESSAGE(status == CSV_OK, "Failed to get row 1 data.");
  TEST_ASSERT_EQUAL_INT64(202403260200, test_data.time);
  TEST_ASSERT_DOUBLE_WITHIN(0.005, 42.02, test_data.value);
  TEST_ASSERT_EQUAL_STRING("value two", test_data.label);
  /* get data from a row where value is not set, we expect the old value to remain. */
  status = get_csv_row_data(&ctx, 3, &test_data);
  TEST_ASSERT_MESSAGE(status == CSV_OK, "Failed to get row 3 data.");
  TEST_ASSERT_EQUAL_INT64(202403260600, test_data.time);
  TEST_ASSERT_DOUBLE_WITHIN(0.005, 42.02, test_data.value); // unchanged.
  TEST_ASSERT_EQUAL_STRING("no value", test_data.label);
  /* get_csv_column_data */
  TEST_ASSERT_MESSAGE(get_csv_column_data(&ctx, "notexisting", data_array, 4) == CSV_ERROR,
                      "Found not existing column?");
  TEST_ASSERT_MESSAGE(get_csv_column_data(&ctx, "time", data_array, 4) == CSV_OK,
                      "Failed to get time column.");
  /* unload csv file */
  status = unload_csv(&ctx);
  TEST_ASSERT_MESSAGE(status == CSV_OK, "Failed to unload CSV file.");
}

int main(void) {
  UNITY_BEGIN();

  RUN_TEST(test_read_csv);

  return UNITY_END();
}
