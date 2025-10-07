
#ifndef ZSF_CONFIG_H_
#  define ZSF_CONFIG_H_

#  ifdef __cplusplus
extern "C" {
#  endif

#  include "sealock.h"
#  include "log/log.h"

#  define ZSF_MAX_LOCKS 50

typedef struct zsf_config_struct {
  sealock_state_t locks[ZSF_MAX_LOCKS];
  unsigned int num_locks;
  unsigned int max_num_z_layers;
  time_t start_time;
  time_t current_time;
  time_t end_time;
  log_level_t log_level;
} zsf_config_t;

int zsf_config_load(zsf_config_t *config_ptr, const char *filepath);
void zsf_config_unload(zsf_config_t *config_ptr);
sealock_index_t zsf_config_get_lock_index(const zsf_config_t *config_ptr, const char *lock_id);

#  ifdef __cplusplus
}
#  endif

#endif // ZSF_CONFIG_H_
