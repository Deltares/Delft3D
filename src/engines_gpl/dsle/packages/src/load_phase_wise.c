/* read time averaged data from csv */

#include "load_phase_wise.h"
#include "dsle.h"

// -- Define setters, all are named set_(attribute_name)
CSV_INT_SETTER(phase_wise_row_t, routine);
CSV_DOUBLE_SETTER(phase_wise_row_t, ship_volume_lake_to_sea);
CSV_DOUBLE_SETTER(phase_wise_row_t, ship_volume_sea_to_lake);
CSV_DOUBLE_SETTER(phase_wise_row_t, t_flushing);
CSV_DOUBLE_SETTER(phase_wise_row_t, t_level);
CSV_DOUBLE_SETTER(phase_wise_row_t, t_open_lake);
CSV_DOUBLE_SETTER(phase_wise_row_t, t_open_sea);
CSV_DOUBLE_SETTER(phase_wise_row_t, density_current_factor_sea);
CSV_DOUBLE_SETTER(phase_wise_row_t, density_current_factor_lake);
CSV_DOUBLE_SETTER(phase_wise_row_t, distance_door_bubble_screen_lake);
CSV_DOUBLE_SETTER(phase_wise_row_t, distance_door_bubble_screen_sea);
CSV_DOUBLE_SETTER(phase_wise_row_t, flushing_discharge_high_tide);
CSV_DOUBLE_SETTER(phase_wise_row_t, flushing_discharge_low_tide);
CSV_DOUBLE_SETTER(phase_wise_row_t, sill_height_lake);
CSV_DOUBLE_SETTER(phase_wise_row_t, sill_height_sea);


int init_phase_wise_timeseries_csv_context(csv_context_t* context) {
  int status = init_csv_context(context);

  // Set up columns
  status = status || def_csv_column(context, "time", double_type, set_dummy);
  status = status || def_csv_column(context, "routine", int_type, set_routine);
  status = status || def_csv_column(context, "ship_volume_lake_to_sea", double_type,
                                    set_ship_volume_lake_to_sea);
  status = status || def_csv_column(context, "ship_volume_sea_to_lake", double_type,
                                    set_ship_volume_sea_to_lake);
  status = status || def_csv_column(context, "t_flushing", double_type, set_t_flushing);
  status = status || def_csv_column(context, "t_level", double_type, set_t_level);
  status = status || def_csv_column(context, "t_open_lake", double_type, set_t_open_lake);
  status = status || def_csv_column(context, "t_open_sea", double_type, set_t_open_sea);
  status = status || def_csv_column(context, "density_current_factor_lake", double_type,
                                    set_density_current_factor_lake);
  status = status || def_csv_column(context, "density_current_factor_sea", double_type,
                                    set_density_current_factor_sea);
  // Mitigations
  status = status || def_csv_column(context, "distance_door_bubble_screen_lake", double_type,
                                    set_distance_door_bubble_screen_lake);
  status = status || def_csv_column(context, "distance_door_bubble_screen_sea", double_type,
                                    set_distance_door_bubble_screen_sea);
  status = status || def_csv_column(context, "flushing_discharge_high_tide", double_type,
                                    set_flushing_discharge_high_tide);
  status = status || def_csv_column(context, "flushing_discharge_low_tide", double_type,
                                    set_flushing_discharge_low_tide);
  status = status || def_csv_column(context, "sill_height_lake", double_type, set_sill_height_lake);
  status = status || def_csv_column(context, "sill_height_sea", double_type, set_sill_height_sea);

  return status;
}

// Load time averaged data from csv.
int load_phase_wise_timeseries(csv_context_t *context, char *filepath) {
  int status = init_phase_wise_timeseries_csv_context(context);
  return status || load_csv(context, filepath) ? CSV_ERROR : CSV_OK;
}
