// Generic CSV reader

#include "load_csv.h"

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define _CSV_NO_DEF_INDEX (-1)
#define _CSV_ROW_CAP_MULTIPLIER (2)
#define _CSV_ROW_CAP_INITIAL_SIZE (100)

// Private functions.

#ifdef _WIN32

// strncasecmp and strndup are not defined in VS2019 :/

int strncasecmp(const char *s1, const char *s2, size_t n) {
  int diff = 0;
  while (*s1 && *s2 && n-- && !diff) {
    diff = tolower(*s1++) - tolower(*s2++);
  }
  if (*s1 != *s2 && n && !diff) {
    diff = tolower(*s1) - tolower(*s2);
  }
  return diff;
}

char *strndup(const char *s, size_t n) {
  char *p = NULL, *e = NULL;
  e = memchr(s, '\0', n);
  if (e) {
    n = e - s;
  }
  p = (char *)malloc(n + 1);
  if (p) {
    memcpy(p, s, n);
    p[n] = '\0';
  }
  return p;
}

#endif // _WIN32

// Return pointer to first non-space character in asciiz string.
static char *skip_spaces(char *ptr) {
  while (ptr && *ptr && isspace(*ptr)) {
    ptr++;
  }
  return ptr;
}

// Trim spaces from a substring of length *len, starting at *str
static void trim_string(char **str, size_t *len) {
  char *start = *str;
  size_t trimmed_len = *len;

  while (*start && trimmed_len && isspace(*start)) {
    start++;
    trimmed_len--;
  }
  while (trimmed_len && isspace(start[trimmed_len - 1])) {
    trimmed_len--;
  }

  *str = start;
  *len = trimmed_len;
  return;
}

// Try to convert a string representation to the given value type.
// Note: For strings, new memory is allocated for a copy.
// Returns typed value, or error typed value if parsing failed.
static csv_value_t field_to_value(csv_type_t field_type, char *field_str, size_t field_len) {
  char *endptr = NULL;
  csv_value_t value = (csv_value_t){.type = field_type};

  if (!*field_str || !field_len) {
    value.type = none_type;
    return value;
  }

  switch (field_type) {
  case int_type:
    value.data.int_value = (int)strtol(field_str, &endptr, 0);
    break;
  case uint_type:
    value.data.uint_value = (unsigned int)strtol(field_str, &endptr, 0);
    break;
  case long_type:
    value.data.long_value = strtoll(field_str, &endptr, 0);
    break;
  case float_type:
    value.data.float_value = strtof(field_str, &endptr);
    break;
  case double_type:
    value.data.double_value = strtod(field_str, &endptr);
    break;
  case string_type:
    value.data.string_value = strndup(field_str, field_len);
    if (value.data.string_value == NULL) {
      value.type = error_type;
    }
    break;
  case none_type:
    break;
  default:
    value.type = error_type;
    break;
  }
  if (endptr && (endptr - field_str != field_len)) {
    value.type = error_type;
  }

  return value;
}

// Discard value. Deallocates memory if it was a string.
static void free_value(csv_value_t value) {
  if (value.type == string_type) {
    free(value.data.string_value);
  }
}

// Collect the (next) csv field contents.
// The start of the substring is stored in *field_str, its length in *field_len.
// Returns pointer to first character of next field, or the end of the asciiz string.
// TODO: Add support for comma separator and quoted strings.
static char *collect_field_str(char *line, char **field_str, size_t *field_len) {
  char *ptr = line;

  *field_str = ptr;
  while (*ptr && *ptr != CSV_SEPARATOR) {
    ptr++;
  }
  *field_len = ptr - *field_str;
  trim_string(field_str, field_len);

  // If not at the end, return pointer to first character of next field.
  return *ptr ? ptr + 1 : ptr;
}

// Get column definition index associated with field (sub) string.
// Returns column index >=0, or _CSV_NO_DEF_INDEX (-1) if not found.
static int get_column_def_index(csv_context_t *context, char *header_str, int header_len) {
  for (int column_def_index = 0; column_def_index < context->num_column_defs; column_def_index++) {
    if (header_len &&
        !strncasecmp(context->column_defs[column_def_index].label, header_str, header_len)) {
      return column_def_index;
    }
  }
  return _CSV_NO_DEF_INDEX;
}

// Parse a line as headers.
// Returns CSV_OK on succes and the column-to-definitions mapping is set up in the provided context.
// NOTE: Parsing currently always 'succeeds', it may just end up without any defined mappings.
static int parse_headers(csv_context_t *context, char *line) {
  char *next = line;
  char *header_str = NULL;
  size_t header_len = 0;
  size_t column_count = 0;

  memset((void *)(context->column_def_index), -1, CSV_MAX_COLUMNS * sizeof(int));
  while (*next) {
    next = collect_field_str(next, &header_str, &header_len);
    // TODO: The _get_column_def_index status may return -1, maybe throw an error if we want to be strict.
    context->column_def_index[column_count] =
        get_column_def_index(context, header_str, header_len);
    column_count++;
  }
  context->num_columns = column_count;

  return CSV_OK;
}

// Parse a line as values.
// Uses column-to-definitions mapping from the provided context.
// Returns CSV_OK on success, CSV_ERROR if conversion of a field failed.
// NOTE: Fields of 'undefined' columns are ignored and stored as 'none' values.
static int parse_values(csv_context_t *context, char *line) {
  char *field_str = NULL;
  size_t field_len = 0;
  csv_type_t field_type = none_type;
  csv_value_t field_value;
  int column_def_index = -1;
  int column_index = 0;

  while (*line && column_index < context->num_columns) {
    line = collect_field_str(line, &field_str, &field_len);
    column_def_index = context->column_def_index[column_index];
    if (column_def_index != _CSV_NO_DEF_INDEX) {
      field_type = context->column_defs[column_def_index].value_type;
      field_value = field_to_value(field_type, field_str, field_len);
      if (field_value.type == error_type) {
        return CSV_ERROR;
      }
    } else {
      // Ignore 'undefined' column and just add 'None' value.
      field_value = (csv_value_t){.type = none_type};
    }
    context->rows[context->num_rows][column_index] = field_value;
    column_index++;
  }
  context->num_rows++;

  return CSV_OK;
}

// Update the row data capacity and allocate additional space if needed.
// Guarantees room for at least one more row in context->rows.
// Returns CSV_OK on success, CSV_ERROR if there was an allocation failure.
static int update_row_cap(csv_context_t *context) {
  size_t new_cap = 0;
  csv_row_t *new_rows = NULL;

  if (!context->rows || context->num_rows == context->row_cap) {
    new_cap = context->row_cap > 0 ? context->row_cap * _CSV_ROW_CAP_MULTIPLIER
                                   : _CSV_ROW_CAP_INITIAL_SIZE;
    new_rows = realloc(context->rows, new_cap * sizeof(csv_row_t));
    if (!new_rows) {
      return CSV_ERROR;
    }
    context->rows = new_rows;
    context->row_cap = new_cap;
  }

  return CSV_OK;
}

// Parse a line from a csv file and updates context accordingly.
// This means either setting up headers or reading values into rows.
// This function ignores comments and empty lines and simply returns.
// TODO: Add support for reading meta data hidden in comments.
// Returns CSV_OK on success, CSV_ERROR otherwise.
static int parse_line(csv_context_t *context, char *line) {
  line = skip_spaces(line);
  if (*line == '\0' || *line == '#') {
    return CSV_OK; // Ignore empty lines and comments
  }
  if (context->num_columns == 0) { // No columns detected yet ...
    return parse_headers(context, line);
  }
  if (update_row_cap(context) == CSV_ERROR) {
    return CSV_ERROR;
  }
  return parse_values(context, line);
}

// Public functions.

// Dummy setter function.
int set_dummy(void *ptr, csv_value_t value) { return CSV_OK; }

// Get a sane, empty, csv_context.
int init_csv_context(csv_context_t* context) {
  assert(context);

  context->rows = NULL;
  context->num_rows = 0;
  context->row_cap = 0;
  context->filepath = NULL;
  context->num_columns = 0;
  context->num_column_defs = 0;

  return CSV_OK;
}

// Define csv column header, value type and setter function in the supplied context.
// NOTE: This function must be called for each header and before calling load_csv()!
// Returns CSV_OK on success. CSV_ERROR if the maximum number of columns defined in CSV_MAX_COLUMNS was exceeded.
int def_csv_column(csv_context_t *context, char *label, csv_type_t value_type,
                   csv_setter_t setter) {
  assert(context);
  assert(label && *label);
  assert(setter);

  if (context->num_column_defs >= CSV_MAX_COLUMNS) {
    return CSV_ERROR;
  }
  context->column_defs[context->num_column_defs++] =
      (csv_column_def_t){.label = label, .value_type = value_type, .setter = setter};

  return CSV_OK;
}

// Loads csv data and parses it according to previously defined columns.
// Returns CSV_OK on success, CSV_ERROR if there was a problem opening or reading the file.
int load_csv(csv_context_t *context, char *filepath) {
  FILE *fp = NULL;
  char line[CSV_MAX_LINE_LENGTH + 1];
  int status = CSV_OK;

  assert(context);
  assert(filepath);

  fp = fopen(filepath, "r");
  if (fp == NULL) {
    return CSV_ERROR;
  }
  context->filepath = filepath;
  while (!feof(fp) && status == CSV_OK) {
    if (fgets(line, CSV_MAX_LINE_LENGTH, fp)) {
      status = parse_line(context, line);
    }
  }
  fclose(fp);

  return status;
}

// Unloads the csv data and column mappings, but maintains the column definitions.
int unload_csv(csv_context_t *context) {
  assert(context);

  if (context->rows && context->num_rows) {
    for (int row = 0; row < context->num_rows; row++) {
      for (int column = 0; column < context->num_columns; column++) {
        free_value(context->rows[row][column]);
      }
    }
    free(context->rows);
  }

  context->rows = NULL;
  context->num_rows = 0;
  context->row_cap = 0;

  context->filepath = NULL;
  memset((void *)(context->column_def_index), -1, CSV_MAX_COLUMNS * sizeof(int));
  context->num_columns = 0;

  return CSV_OK;
}

// Get the number of data rows that were loaded from the csv.
// Returns the number of rows read in the supplied context.
size_t get_csv_num_rows(csv_context_t *context) {
  assert(context);
  return context->num_rows;
}

// Collect the data from a row into a struct, using the previously defined setters.
// Undefined columns are ignored.
// Returns CSV_OK on success.
// If any of the setters returns a different value, the process stops and that value is returned.
int get_csv_row_data(csv_context_t *context, size_t row_index, void *struct_ptr) {
  int status = CSV_OK;
  csv_value_t value;

  assert(context);
  assert(struct_ptr);

  if (row_index >= context->num_rows) {
    return CSV_ERROR;
  }

  for (size_t column_index = 0; column_index < context->num_columns; column_index++) {
    int column_def_index = context->column_def_index[column_index];
    if (column_def_index != _CSV_NO_DEF_INDEX) {
      value = context->rows[row_index][column_index];
      if (value.type != none_type) {
        status = context->column_defs[column_def_index].setter(struct_ptr, value);
        if (status != CSV_OK) {
          break;
        }
      }
    }
  }

  return status;
}

// Copy values of a given column and type to an array.
static int get_array(csv_context_t *context, csv_type_t type, size_t column_index, void *array_ptr,
               size_t array_len) {
  int row_index = 0;
  if (array_len > context->num_rows) {
    array_len = context->num_rows;
  }

  for (row_index = 0; row_index < array_len; row_index++) {
    csv_value_t value = context->rows[row_index][column_index];
    switch (value.type) {
    case int_type:
      ((int *)array_ptr)[row_index] = value.data.int_value;
      break;
    case uint_type:
      ((unsigned int *)array_ptr)[row_index] = value.data.uint_value;
      break;
    case long_type:
      ((int64_t *)array_ptr)[row_index] = value.data.long_value;
      break;
    case float_type:
      ((float *)array_ptr)[row_index] = value.data.float_value;
      break;
    case double_type:
      ((double *)array_ptr)[row_index] = value.data.double_value;
      break;
    case string_type:
      ((char **)array_ptr)[row_index] = value.data.string_value;
      break;
    case error_type:
    case none_type:
    default:
      return CSV_ERROR;
    }
  }

  return CSV_OK;
}

// Collect data from a column into an array of the appropriate type.
// array_ptr is a pre-allocated array, array_len is the size of the array in number of elements.
// Addressing undefined columns will result in a failure and nothing will be written to the array.
int get_csv_column_data(csv_context_t *context, char *label, void *array_ptr, size_t array_len) {
  size_t column_index = 0;
  int column_def_index = _CSV_NO_DEF_INDEX;

  assert(context);
  assert(array_ptr);
  assert(label);

  for (column_index = 0; column_index < context->num_columns; column_index++) {
    column_def_index = context->column_def_index[column_index];
    if (column_def_index != _CSV_NO_DEF_INDEX &&
        !strcmp(context->column_defs[column_def_index].label, label)) {
      return get_array(context, context->column_defs[column_def_index].value_type, column_index,
                        array_ptr, array_len);
    }
  }

  return CSV_ERROR;
}
