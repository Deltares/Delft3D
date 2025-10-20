import os

from cffi import FFI


ffibuilder = FFI()

ffibuilder.cdef(
    """
    typedef struct dsle_param_t {
        double lock_length;
        double lock_width;
        double lock_bottom;
        double num_cycles;
        double door_time_to_open;
        double leveling_time;
        double calibration_coefficient;
        double symmetry_coefficient;
        double ship_volume_sea_to_lake;
        double ship_volume_lake_to_sea;
        double salinity_lock;
        double head_sea;
        double salinity_sea;
        double temperature_sea;
        double head_lake;
        double salinity_lake;
        double temperature_lake;
        double flushing_discharge_high_tide;
        double flushing_discharge_low_tide;
        double density_current_factor_sea;
        double density_current_factor_lake;
        double distance_door_bubble_screen_sea;
        double distance_door_bubble_screen_lake;
        double sill_height_sea;
        double sill_height_lake;
        double rtol;
        double atol;
    } dsle_param_t;

    typedef struct dsle_results_t {
        double mass_transport_lake;
        double salt_load_lake;
        double discharge_from_lake;
        double discharge_to_lake;
        double salinity_to_lake;

        double mass_transport_sea;
        double salt_load_sea;
        double discharge_from_sea;
        double discharge_to_sea;
        double salinity_to_sea;
    } dsle_results_t;

    typedef struct dsle_phase_state_t {
        double salinity_lock;
        double saltmass_lock;
        double head_lock;
        double volume_ship_in_lock;
    } dsle_phase_state_t;

    typedef struct dsle_phase_transports_t {
        double mass_transport_lake;
        double volume_from_lake;
        double volume_to_lake;
        double discharge_from_lake;
        double discharge_to_lake;
        double salinity_to_lake;

        double mass_transport_sea;
        double volume_from_sea;
        double volume_to_sea;
        double discharge_from_sea;
        double discharge_to_sea;
        double salinity_to_sea;
    } dsle_phase_transports_t;

    typedef struct dsle_aux_results_t {
        double z_fraction;
        double dimensionless_door_open_time;
        double volume_to_lake;
        double volume_from_lake;
        double volume_to_sea;
        double volume_from_sea;
        double volume_lock_at_lake;
        double volume_lock_at_sea;
        double t_cycle;
        double t_open;
        double t_open_lake;
        double t_open_sea;
        double salinity_lock_1;
        double salinity_lock_2;
        double salinity_lock_3;
        double salinity_lock_4;
        dsle_phase_transports_t transports_phase_1;
        dsle_phase_transports_t transports_phase_2;
        dsle_phase_transports_t transports_phase_3;
        dsle_phase_transports_t transports_phase_4;
    } dsle_aux_results_t;

    int dsle_initialize_state(const dsle_param_t *p, dsle_phase_state_t *state,
                              double salinity_lock, double head_lock);

    int dsle_step_phase_1(const dsle_param_t *p, double t_level,
                          dsle_phase_state_t *state,
                          dsle_phase_transports_t *results);

    int dsle_step_phase_2(const dsle_param_t *p, double t_open_lake,
                          dsle_phase_state_t *state,
                          dsle_phase_transports_t *results);

    int dsle_step_phase_3(const dsle_param_t *p, double t_level,
                          dsle_phase_state_t *state,
                          dsle_phase_transports_t *results);

    int dsle_step_phase_4(const dsle_param_t *p, double t_open_sea,
                          dsle_phase_state_t *state,
                          dsle_phase_transports_t *results);

    int dsle_step_flush_doors_closed(const dsle_param_t *p,
                                    double t_flushing,
                                    dsle_phase_state_t *state,
                                    dsle_phase_transports_t *results);

    void dsle_param_default(dsle_param_t *p);

    int dsle_calc_steady(const dsle_param_t *p, dsle_results_t *results,
                         dsle_aux_results_t *aux_results);

    const char * dsle_error_msg(int code);

    const char * dsle_version();
"""
)

if os.name == "posix":
    extra_compile_args = []
else:
    extra_compile_args = ["/MD"]

ffibuilder.set_source(
    "pydsle._dsle_cffi",
    '#include "dsle.h"',
    libraries=["dsle-static"],
    define_macros=[("DSLE_STATIC", None), ("Py_LIMITED_API", None)],
    py_limited_api=True,
    extra_compile_args=extra_compile_args,
)


if __name__ == "__main__":
    ffibuilder.compile(verbose=True)
