
#ifndef TIMESTAMP_H
#define TIMESTAMP_H

#include <time.h>

#if defined(__cplusplus)
extern "C" {
#endif

time_t timestamp_to_time(double timestamp);
time_t *timestamp_array_to_times(double *timestamps, size_t length);
double time_to_timestamp(time_t t);
time_t timestamp_string_to_time(const char *str, char **end_ptr);
int times_strictly_increasing(time_t *times, size_t length);

#if defined(__cplusplus)
}
#endif

#endif // TIMESTAMP_H
