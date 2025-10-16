
#ifndef DSLE_CONFIG_H_
#  define DSLE_CONFIG_H_

#  ifdef __cplusplus
extern "C" {
#  endif

#  include "sealock.h"
#  include "log/log.h"

#  define DSLE_MAX_LOCKS 50

typedef struct dsle_config_struct {
  sealock_state_t locks[DSLE_MAX_LOCKS];
  unsigned int num_locks;
  unsigned int max_num_z_layers;
  time_t start_time;
  time_t current_time;
  time_t end_time;
  log_level_t log_level;
} dsle_config_t;

int dsle_config_load(dsle_config_t *config_ptr, const char *filepath);
void dsle_config_unload(dsle_config_t *config_ptr);
sealock_index_t dsle_config_get_lock_index(const dsle_config_t *config_ptr, const char *lock_id);

#  ifdef __cplusplus
}
#  endif

#endif // DSLE_CONFIG_H_
