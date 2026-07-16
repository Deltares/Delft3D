#ifndef LOGGER_H
#define LOGGER_H

#include <stdarg.h>
#include <stdio.h>

#if defined(__cplusplus)
extern "C" {
#endif

typedef enum { logDEBUG = 0, logINFO, logWARNING, logERROR } log_level_t;

typedef struct s_logger {
  char *name;
  FILE *fp;
  log_level_t level;
} logger_t;

void log_init(char *name, FILE *fp);
void log_set_level(log_level_t lvl);
void log_msg(log_level_t lvl, char *fmt, ...);
void log_debug(char *fmt, ...);
void log_info(char *fmt, ...);
void log_warning(char *fmt, ...);
void log_error(char *fmt, ...);

// parse level_str and return the associated log_level, or the default
// when parsing fails.
// Valid entries are: DEBUG, INFO, WARNING, ERROR.
log_level_t log_level(char *level_str, log_level_t default_level);

#if defined(__cplusplus)
}
#endif

#endif // LOGGER_H
