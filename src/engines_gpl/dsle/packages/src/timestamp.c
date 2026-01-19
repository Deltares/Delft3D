

#include "timestamp.h"

#include <errno.h>
#include <stdlib.h>

// Convert 'double' timestamp to time_t.
// Returns -1 on error.
time_t timestamp_to_time(double timestamp) {
  long long converted_ts = timestamp;
  struct tm date_time;

  date_time.tm_sec = 0;
  date_time.tm_min = converted_ts % 100;
  date_time.tm_hour = (converted_ts / 100) % 100;
  date_time.tm_mday = (converted_ts / 10000) % 100;
  date_time.tm_mon = (converted_ts / 1000000) % 100 - 1;
  date_time.tm_year = (converted_ts / 100000000) - 1900;
  date_time.tm_wday = 0;
  date_time.tm_yday = 0;
  date_time.tm_isdst = -1;

  return mktime(&date_time);
}

int times_strictly_increasing(time_t *times, size_t length) {
  for (int i = 0; i < length - 1; i++) {
    if (times[i + 1] <= times[i]) {
      return 0;
    }
  }
  return 1;
}

// Duplicate and convert timestamp double array to a time_t array.
// Note: The caller is responsible for deallocating the time_t array.
time_t *timestamp_array_to_times(double *timestamps, size_t length) {
  time_t *times = NULL;

  if (length) {
    times = malloc(sizeof(time_t) * length);
    if (times) {
      for (size_t i = 0; i < length; i++) {
        times[i] = timestamp_to_time(timestamps[i]);
      }
    }
  }

  return times;
}

// Convert time_t time to timstamp.
// Returns -1.0 on error.
double time_to_timestamp(time_t t) {
  double timestamp = -1.0;
  struct tm *date_time;

  if (t >= 0) {
    date_time = localtime(&t);
    timestamp = date_time->tm_min;
    timestamp += date_time->tm_hour * 100.0;
    timestamp += date_time->tm_mday * 10000.0;
    timestamp += (date_time->tm_mon + 1.0) * 1000000.0;
    timestamp += (double)(date_time->tm_year + 1900.0) * 100000000.0;
  }

  return timestamp;
}

// Convert timestamp 'YYYYMMDDhhmm' string format to time_t.
// Returns -1 on error.
time_t timestamp_string_to_time(const char *str, char **end_ptr) {
  double timestamp = strtod(str, end_ptr);

  if (!errno && ((end_ptr && *end_ptr != str) || !end_ptr)) {
    return timestamp_to_time(timestamp);
  }

  return -1;
}
