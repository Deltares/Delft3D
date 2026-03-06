
#pragma once

#ifndef INI_READ_H
#  define INI_READ_H

#  define INI_OK (0)    /* okay */
#  define INI_FAIL (-1) /* failed */

#  ifdef __cplusplus
extern "C" {
#  endif

typedef int (*ini_callback)(char *section, char *key, char *value, void *data_ptr);

/* Read a .ini file at filepath, for each key/value pair in the file
 * the callback function is called. It is up to the callback function
 * to figure out what to do with the value and store it into memory
 * pointed to by data_ptr in a meaningful way. the callback function
 * should return INI_FAIL if there was a fatal problem, or INI_OK otherwise.
 *
 * Note: The callback function is also called with an empty key and value when
 *       a new section header is encountered, such that the callback can handle
 *       settings specified in seperate blocks.
 *
 * Returns: 0  if all was ok.
 *          -1 if there was a file i/o problem (file not found or read error).
 *          >0 line number if there was a problem in that line of the file.
 */
int ini_read(const char *filepath, ini_callback callback, void *data_ptr);

/* Helper functions for handling values within the callback function.
 * Returns: converted value, success INI_OK was written to the supplied
 *          status pointer. On failure, INI_FAIL was written instead and
 *          the return value should be zero or NULL, but don't rely on it.
 * NOTE: The caller is responsible for deallocating the array returned from
 *       ini_parse_double_list.
 */
int ini_parse_int(const char *value, int *status_ptr);
double ini_parse_double(const char *value, int *status_ptr);
double *ini_parse_double_list(const char *value, int *length_ptr, int *status_ptr);

#  ifdef __cplusplus
}
#  endif

#endif // INI_READ_H
