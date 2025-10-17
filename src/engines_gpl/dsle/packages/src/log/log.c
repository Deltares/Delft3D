
#include "log.h"

#include <assert.h>
#include <string.h>

logger_t logger;
const char *level_strings[logERROR + 1] = {"DEBUG", "INFO", "WARNING", "ERROR"};

void log_init(char *name, FILE *fp) {
  logger.name = name;
  logger.fp = fp;
  logger.level = logWARNING;
}

void log_set_level(log_level_t lvl) {
  logger.level = lvl;
}

static void vlog(log_level_t lvl, char *fmt, va_list ap) {
  if (lvl >= logger.level && lvl <= logERROR) {
    FILE *stream = logger.fp ? logger.fp : stderr;
    fprintf(stream, "[%s] %s: ", level_strings[lvl], logger.name);
    vfprintf(stream, fmt, ap);
  }
}

void log_msg(log_level_t lvl, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vlog(lvl, fmt, ap);
  va_end(ap);
}

void log_debug(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vlog(logDEBUG, fmt, ap);
  va_end(ap);
}

void log_info(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vlog(logINFO, fmt, ap);
  va_end(ap);
}

void log_warning(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vlog(logWARNING, fmt, ap);
  va_end(ap);
}

void log_error(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vlog(logERROR, fmt, ap);
  va_end(ap);
}

// parse level_str and return the associated log_level, or the default
// when parsing fails.
// Valid entries are: DEBUG, INFO, WARNING, ERROR. (case is ignored)
log_level_t log_level(char* level_str, log_level_t default_level) {
  log_level_t level = default_level;

  for (int i = 0; i <= logERROR; i++) {
#   ifdef _WIN32
    if (!strcmpi(level_str, level_strings[i])) {
#   else
    if (!strcasecmp(level_str, level_strings[i])) {
#   endif
    level = (log_level_t)i;
    }
  }

  return level;
}
