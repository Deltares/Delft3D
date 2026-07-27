
#pragma once

#include "csv/load_csv.h"

#ifndef LOAD_TIME_AVERAGED_H
#  define LOAD_TIME_AVERAGED_H

#  if defined(__cplusplus)
extern "C" {
#  endif

int load_time_averaged_timeseries(csv_context_t *context, char *filepath);

#  if defined(__cplusplus)
}
#  endif

#endif // LOAD_TIME_AVERAGED_H
