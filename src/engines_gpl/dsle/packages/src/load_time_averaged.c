/* read time averaged data from csv */

#include "load_time_averaged.h"
#include "zsf.h"

// -- Define setters, all are named set_(attribute_name)
CSV_DOUBLE_SETTER(zsf_param_t, ship_volume_lake_to_sea);
CSV_DOUBLE_SETTER(zsf_param_t, ship_volume_sea_to_lake);
CSV_DOUBLE_SETTER(zsf_param_t, door_time_to_open);
CSV_DOUBLE_SETTER(zsf_param_t, leveling_time);
CSV_DOUBLE_SETTER(zsf_param_t, density_current_factor_sea);
CSV_DOUBLE_SETTER(zsf_param_t, density_current_factor_lake);
CSV_DOUBLE_SETTER(zsf_param_t, num_cycles);

// Custom setter for flushing_discharge.
int set_flushing_discharge(void *ptr, csv_value_t value) {
  zsf_param_t *dataptr = (zsf_param_t *)ptr;
  // TODO: Confirm this is ok?
  dataptr->flushing_discharge_high_tide = value.data.double_value;
  dataptr->flushing_discharge_low_tide = value.data.double_value;
  return CSV_OK;
}

// Load time averaged data from csv.
int load_time_averaged_timeseries(csv_context_t *context, char *filepath) {
  int status = init_csv_context(context);

  // Set up columns
  status = status || def_csv_column(context, "time", double_type, set_dummy);
  status = status || def_csv_column(context, "ship_volume_lake_to_sea", double_type,
                                    set_ship_volume_lake_to_sea);
  status = status || def_csv_column(context, "ship_volume_sea_to_lake", double_type,
                                    set_ship_volume_sea_to_lake);
  status =
      status || def_csv_column(context, "door_time_to_open", double_type, set_door_time_to_open);
  status = status || def_csv_column(context, "leveling_time", double_type, set_leveling_time);
  status = status || def_csv_column(context, "density_current_factor_sea", double_type,
                                    set_density_current_factor_sea);
  status = status || def_csv_column(context, "density_current_factor_lake", double_type,
                                    set_density_current_factor_lake);
  status = status || def_csv_column(context, "num_cycle", double_type, set_num_cycles);
  status =
      status || def_csv_column(context, "flushing_discharge", double_type, set_flushing_discharge);
  return status || load_csv(context, filepath) ? CSV_ERROR : CSV_OK;
}
