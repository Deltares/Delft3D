
#include "ini_read.h"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INI_SECTION_LEN (200)
#define INI_LINE_LEN (4096)

static const char ini_string_delim = '"';
static const char ini_rem_char1 = '#';
static const char ini_rem_char2 = ';';
static const char ini_section_start_char = '[';
static const char ini_section_end_char = ']';
static const char ini_equal_char1 = '=';
static const char ini_equal_char2 = ':';

static inline int at_eol(const char *p) { return (*p == '\n'); }
static inline int at_remark(const char *p) { return (*p == ini_rem_char1 || *p == ini_rem_char2); }
static inline int at_equals(const char *p) {
  return (*p == ini_equal_char1 || *p == ini_equal_char2);
}
static inline int at_section_start(const char *p) { return (*p == ini_section_start_char); }
static inline int at_section_end(const char *p) { return (*p == ini_section_end_char); }

static char *skip_space(char *ptr) {
  while (ptr && *ptr && isspace(*ptr) && !at_eol(ptr)) {
    ptr++;
  }
  return ptr;
}

static int end_ptr_at_end(char *end_ptr) {
  char *real_end = skip_space(end_ptr);
  return real_end && !*real_end;
}

static int parse_section_header(char **parse_pos_ptr, char *section) {
  int i = 0;
  char *parse_pos = *parse_pos_ptr;
  parse_pos++; // Skip '['
  while (*parse_pos && !at_section_end(parse_pos) && i < INI_SECTION_LEN) {
    section[i++] = *parse_pos;
    parse_pos++;
  }
  section[i] = '\0';
  if (!at_section_end(parse_pos)) {
    return INI_FAIL;
  }
  parse_pos++; // Skip ']'
  *parse_pos_ptr = parse_pos;
  return INI_OK;
}

static int parse_key(char **parse_pos_ptr, char *key_str) {
  int i = 0;
  char *parse_pos = *parse_pos_ptr;
  while (*parse_pos && !at_equals(parse_pos) && i < INI_LINE_LEN) {
    key_str[i++] = *parse_pos;
    parse_pos++;
  }
  while (i && isspace(key_str[i - 1])) {
    i--;
  }
  key_str[i] = '\0';
  if (!at_equals(parse_pos)) {
    return INI_FAIL;
  }
  *parse_pos_ptr = parse_pos;
  return INI_OK;
}

static int parse_value(char **parse_pos_ptr, char *value_str) {
  int i = 0;
  char *parse_pos = *parse_pos_ptr;
  if (*parse_pos == ini_string_delim) {
    parse_pos++; // Skip '"'
    while (*parse_pos && *parse_pos != ini_string_delim && i < INI_LINE_LEN) {
      value_str[i++] = *parse_pos;
      parse_pos++;
    }
    if (*parse_pos != ini_string_delim) {
      return INI_FAIL;
    }
    parse_pos++; // Skip '"'
  } else {
    while (*parse_pos && !at_remark(parse_pos) && !at_eol(parse_pos)) {
      value_str[i++] = *parse_pos;
      parse_pos++;
    }
    while (i && isspace(value_str[i - 1])) {
      i--;
    }
  }
  value_str[i] = '\0';
  *parse_pos_ptr = parse_pos;
  return INI_OK;
}

int ini_read(const char *filepath, ini_callback callback, void *data_ptr) {
  char section[INI_SECTION_LEN + 1];
  char buffer[INI_LINE_LEN + 1];
  char key_str[INI_LINE_LEN + 1];
  char value_str[INI_LINE_LEN + 1];
  int error_code = INI_OK;
  FILE *file_ptr = NULL;
  char *parse_pos = NULL;
  int line_no = 0;

  assert(filepath != NULL);
  assert(callback != NULL);

  section[0] = '\0';
  key_str[0] = '\0';
  value_str[0] = '\0';

  file_ptr = fopen(filepath, "r");
  if (file_ptr == NULL) {
    return INI_FAIL;
  }

  while (!feof(file_ptr) && error_code == INI_OK) {
    parse_pos = fgets(buffer, INI_LINE_LEN, file_ptr);
    if (parse_pos == NULL || feof(file_ptr)) {
      break;
    }

    line_no++;
    parse_pos = skip_space(parse_pos);
    if (at_section_start(parse_pos)) {
      if (parse_section_header(&parse_pos, section) != INI_OK) {
        error_code = line_no;
        break;
      }
      // Call callback with empty key and value to signal new section block.
      if (callback(section, "", "", data_ptr) != INI_OK) {
        error_code = line_no;
        break;
      }
    } else if (isalpha(*parse_pos)) {
      if (parse_key(&parse_pos, key_str) != INI_OK) {
        error_code = line_no;
        break;
      }
      parse_pos = skip_space(parse_pos);
      if (!at_equals(parse_pos)) {
        error_code = line_no;
        break;
      }
      parse_pos++;
      parse_pos = skip_space(parse_pos);
      if (parse_value(&parse_pos, value_str) != INI_OK) {
        error_code = line_no;
        break;
      }
      if (*value_str) {
        // Only try to update if there is a non-empty value string.
        if (callback(section, key_str, value_str, data_ptr) != INI_OK) {
          error_code = line_no;
          break;
        }
      }
    }

    parse_pos = skip_space(parse_pos);
    if (*parse_pos && !(at_remark(parse_pos) || at_eol(parse_pos))) {
      error_code = line_no;
      break;
    }
  }

  fclose(file_ptr);
  return error_code;
}

// Helper functions:
int ini_parse_int(const char *value, int *status_ptr) {
  int result = 0;
  char *end_ptr = NULL;

  assert(value);

  errno = 0;
  result = strtol(value, &end_ptr, 0);
  if (status_ptr) {
    if (errno || end_ptr == value || !end_ptr_at_end(end_ptr)) {
      *status_ptr = INI_FAIL;
    } else {
      *status_ptr = INI_OK;
    }
  }

  return result;
}

double ini_parse_double(const char *value, int *status_ptr) {
  double result = 0.0;
  char *end_ptr = NULL;

  assert(value);

  errno = 0;
  result = strtod(value, &end_ptr);
  if (status_ptr) {
    if (errno || end_ptr == value || !end_ptr_at_end(end_ptr)) {
      *status_ptr = INI_FAIL;
    } else {
      *status_ptr = INI_OK;
    }
  }

  return result;
}

double *ini_parse_double_list(const char *value, int *length_ptr, int *status_ptr) {
  int result_length = 0;
  double *result_array = NULL;
  char *tempstr = strdup(value);
  char *token = NULL;
  int status = INI_OK;

  assert(value);
  assert(length_ptr);
  assert(tempstr);

  // Count number of tokens.
  token = strtok(tempstr, ",");
  while (token) {
    result_length++;
    token = strtok(NULL, ",");
  }

  // Restore orignal value
  strcpy(tempstr, value);

  // Collect values into array
  result_array = result_length ? calloc(result_length, sizeof(double)) : NULL;
  if (result_array) {
    token = strtok(tempstr, ",");
    for (int i = 0; i < result_length && token && status == INI_OK; i++) {
      result_array[i] = ini_parse_double(token, &status);
      token = strtok(NULL, ",");
    }
  } else {
    status = INI_FAIL;
  }
  free(tempstr);

  if (status == INI_FAIL) {
    free(result_array);
    result_array = NULL;
  } else {
    *length_ptr = result_length;
  }

  if (status_ptr) {
    *status_ptr = status;
  }

  return result_array;
}
