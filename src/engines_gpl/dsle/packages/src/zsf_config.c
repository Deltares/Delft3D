
#include "zsf_config.h"
#include "csv/load_csv.h"
#include "ini/ini_read.h"
#include "timestamp.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

static int zsf_ini_handler(char *section, char *key, char *value, void *data_ptr) {
  zsf_config_t *config_ptr = (zsf_config_t *)data_ptr;
  int status = INI_OK;

  assert(section);
  assert(key);
  assert(value);
  assert(data_ptr);

  if (!strcmp(section, "sealock")) {
    sealock_index_t lock_index = config_ptr->num_locks - 1;
    assert(!*key || lock_index >= 0);
    if (!*key) {
      if (config_ptr->num_locks < ZSF_MAX_LOCKS) {
        status = sealock_defaults(&config_ptr->locks[config_ptr->num_locks]);
        config_ptr->num_locks++;
      } else {
        return INI_FAIL;
      }
    } else if (!strcmp(key, "id")) {
      config_ptr->locks[lock_index].id = strdup(value);
    } else if (!strcmp(key, "computation_mode")) {
      if (!strcmp(value, "cycle_average")) {
        config_ptr->locks[lock_index].computation_mode = cycle_average_mode;
      } else if (!strcmp(value, "phase_wise")) {
        config_ptr->locks[lock_index].computation_mode = phase_wise_mode;
      } else {
        status = INI_FAIL;
      }
    } else if (!strcmp(key, "sealock_operational_parameters")) {
      config_ptr->locks[lock_index].operational_parameters_file = strdup(value);
    } else if (!strcmp(key, "initial_head_lock")) {
      config_ptr->locks[lock_index].phase_state.head_lock = ini_parse_double(value, &status);
    } else if (!strcmp(key, "initial_salinity_lock")) {
      config_ptr->locks[lock_index].phase_state.salinity_lock = ini_parse_double(value, &status);
      config_ptr->locks[lock_index].parameters.salinity_lock = ini_parse_double(value, &status);
    } else if (!strcmp(key, "initial_saltmass_lock")) {
      config_ptr->locks[lock_index].phase_state.saltmass_lock = ini_parse_double(value, &status);
    } else if (!strcmp(key, "initial_temperature_lock")) {
      double temperature = ini_parse_double(value, &status);
      if (status == INI_OK) {
        config_ptr->locks[lock_index].parameters.temperature_lake = temperature;
        config_ptr->locks[lock_index].parameters.temperature_sea = temperature;
      }
    } else if (!strcmp(key, "initial_volume_ship_in_lock")) {
      config_ptr->locks[lock_index].phase_state.volume_ship_in_lock =
          ini_parse_double(value, &status);
    } else if (!strcmp(key, "lock_length")) {
      config_ptr->locks[lock_index].parameters.lock_length = ini_parse_double(value, &status);
    } else if (!strcmp(key, "lock_width")) {
      config_ptr->locks[lock_index].parameters.lock_width = ini_parse_double(value, &status);
    } else if (!strcmp(key, "lock_bottom")) {
      config_ptr->locks[lock_index].parameters.lock_bottom = ini_parse_double(value, &status);
    } else if (!strcmp(key, "flow_profile")) {
      int array_length = 0;
      double *value_array = ini_parse_double_list(value, &array_length, &status);
      double *linear_z_positions = io_layer_linear_z_positions(array_length);
      if (status == INI_OK && linear_z_positions) {
        config_ptr->locks[lock_index].flow_profile.number_of_positions = array_length;
        config_ptr->locks[lock_index].flow_profile.relative_discharge_from_lock = value_array;
        config_ptr->locks[lock_index].flow_profile.relative_z_position = linear_z_positions;
        // Normalize profile to ensure positive and negative parts each integrate to unity.
        // Only profiles with a single zero(-crossing) are supported. Non-conforming
        // profiles will result in an error.
        if (io_normalize_profile(&config_ptr->locks[lock_index].flow_profile) != 0) {
          cleanup_profile(&config_ptr->locks[lock_index].flow_profile);
          status = INI_FAIL;
        }
      } else {
        free(value_array);
        status = INI_FAIL;
      }
    } else if (!strcmp(key, "allowed_head_difference")) {
      config_ptr->locks[lock_index].parameters.allowed_head_difference =
          ini_parse_double(value, &status);
    }
  } else if (!strcmp(section, "general") || !*section) {
    char *end_ptr = NULL;
    if (!strcmp(key, "start_time")) {
      time_t start_time = timestamp_string_to_time(value, &end_ptr);
      if (start_time >= 0) {
        config_ptr->start_time = start_time;
        config_ptr->current_time = config_ptr->start_time;
      } else {
        status = INI_FAIL;
      }
    } else if (!strcmp(key, "end_time")) {
      time_t end_time = timestamp_string_to_time(value, &end_ptr);
      if (end_time >= 0) {
        config_ptr->end_time = end_time;
      } else {
        status = INI_FAIL;
      }
    } else if (!strcmp(key, "kmx")) {
      config_ptr->max_num_z_layers = ini_parse_int(value, &status);
      if (config_ptr->max_num_z_layers < 1 || config_ptr->max_num_z_layers > MAX_NUM_VOLUMES) {
        status = INI_FAIL;
      }
    } else if (!strcmp(key, "log_level")) {
      // Set new log level, or keep the default one already set.
      config_ptr->log_level = log_level(value, config_ptr->log_level);
    }
  }

  return status;
}

int zsf_config_load(zsf_config_t *config_ptr, const char *filepath) {
  assert(config_ptr);
  assert(filepath);
  config_ptr->num_locks = 0;
  config_ptr->max_num_z_layers = 1;
  config_ptr->start_time = 0.0;
  config_ptr->end_time = 0.0;
  config_ptr->current_time = 0.0;
  config_ptr->log_level = logINFO;
  return ini_read(filepath, zsf_ini_handler, config_ptr);
}

void zsf_config_unload(zsf_config_t *config_ptr) {
  if (config_ptr) {
    while (config_ptr->num_locks) {
      config_ptr->num_locks--;
      unload_csv(&config_ptr->locks[config_ptr->num_locks].timeseries_data);
      free(config_ptr->locks[config_ptr->num_locks].operational_parameters_file);
      free(config_ptr->locks[config_ptr->num_locks].id);
    }
  }
  return;
}

// Find sealock in config by id.
// Returns -1 if not found.
sealock_index_t zsf_config_get_lock_index(const zsf_config_t *config_ptr, const char *lock_id) {
  for (sealock_index_t index = 0; index < config_ptr->num_locks; index++) {
    if (!strcmp(lock_id, config_ptr->locks[index].id)) {
      return index;
    }
  }
  return -1;
}
