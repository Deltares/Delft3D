/* Reading CSV files */

#pragma once

#ifndef CSV_H
#  define CSV_H

#  include <stdint.h>
#  include <stddef.h>

#  define CSV_OK (0)
#  define CSV_ERROR (-1)

#  define CSV_SEPARATOR ';'
#  define CSV_MAX_LINE_LENGTH (4096)
#  define CSV_MAX_COLUMNS (100)

#  ifdef __cplusplus
extern "C" {
#  endif

// Supported value types.
typedef enum {
  none_type = 0,
  int_type,
  uint_type,
  long_type,
  float_type,
  double_type,
  string_type,
  error_type
} csv_type_t;

// Typed value struct.
typedef struct s_csv_value {
  csv_type_t type;
  union {
    int int_value;
    unsigned int uint_value;
    int64_t long_value;
    float float_value;
    double double_value;
    char *string_value;
  } data;
} csv_value_t;

// Column value setter function pointer.
typedef int (*csv_setter_t)(void *struct_ptr, csv_value_t value);

// Column definition struct.
typedef struct s_csv_column_def {
  char *label;
  csv_type_t value_type;
  csv_setter_t setter;
} csv_column_def_t;

// Row data type.
typedef csv_value_t csv_row_t[CSV_MAX_COLUMNS];

// CSV Reader context struct.
typedef struct s_csv_context {
  char *filepath;                                // Filepath as supplied when reading file.
  csv_column_def_t column_defs[CSV_MAX_COLUMNS]; // Column definitions.
  int column_def_index[CSV_MAX_COLUMNS];         // Mapping between defs and actual columns in CSV.
  size_t num_column_defs;                        // Number of defined CSV column headers
  csv_row_t *rows;                               // Row data array (as typed values)
  size_t row_cap;                                // Number of currently allocated rows.
  size_t num_rows;                               // Number of rows currently read from CSV.
  size_t num_columns;                            // Number of columns read from CSV (headers).
} csv_context_t;


int init_csv_context(csv_context_t *context);
int def_csv_column(csv_context_t *context, char *label, csv_type_t value_type, csv_setter_t setter);
int load_csv(csv_context_t *context, char *filepath);
int unload_csv(csv_context_t *context);
size_t get_csv_num_rows(csv_context_t *context);
int get_csv_row_data(csv_context_t *context, size_t row_index, void *struct_ptr);
int get_csv_column_data(csv_context_t *context, char *label, void *array_ptr, size_t array_len);

#  define CSV_SETTER(S, A, VA)                                                                     \
    static int set_##A(void *ptr, csv_value_t value) {                                                    \
      S *dataptr = (S *)ptr;                                                                       \
      dataptr->A = value.data.VA;                                                                  \
      return CSV_OK;                                                                               \
    }

int set_dummy(void *ptr, csv_value_t value);

#  define CSV_INT_SETTER(S, A) CSV_SETTER(S, A, int_value)
#  define CSV_UINT_SETTER(S, A) CSV_SETTER(S, A, uint_value)
#  define CSV_LONG_SETTER(S, A) CSV_SETTER(S, A, long_value)
#  define CSV_FLOAT_SETTER(S, A) CSV_SETTER(S, A, float_value)
#  define CSV_DOUBLE_SETTER(S, A) CSV_SETTER(S, A, double_value)
#  define CSV_STRING_SETTER(S, A) CSV_SETTER(S, A, string_value)

#  ifdef __cplusplus
}
#  endif

#endif // CSV_H
