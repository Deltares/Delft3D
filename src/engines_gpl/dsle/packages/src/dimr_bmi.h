
#ifndef DIMR_BMI_H
#define DIMR_BMI_H

/* DIMR bmi interface prototypes */
// TODO: Move/include this header for export purposes?
#include "dsle.h"

#if defined(__cplusplus)
extern "C" {
#endif

#define BMI_MAX_UNITS_NAME (2048)
#define BMI_MAX_TYPE_NAME (2048)
#define BMI_MAX_COMPONENT_NAME (2048)
#define BMI_MAX_VAR_NAME (2048)

#define DIMR_BMI_OK (0)      // Should be returned on success.
#define DIMR_BMI_FAILURE (1) // Generic 'failure'.

#define DIMR_BMI_MAXDIMS (6) // Used in get_var_shape.

DSLE_EXPORT int DSLE_CALLCONV initialize(const char *config_file);
DSLE_EXPORT int DSLE_CALLCONV finalize();                              // Always returns DIMR_BMI_OK
DSLE_EXPORT int DSLE_CALLCONV set_var(const char *key, void *src_ptr); // In BMI 2.0 = set_value
DSLE_EXPORT int DSLE_CALLCONV get_var(const char *key, void **dst_ptr); // In BMI 2.0 = get_value
int get_value_ptr(char *key, void **dst_ptr); // In DIMR **dst_ptr always is a double.
DSLE_EXPORT int DSLE_CALLCONV update(double dt);
DSLE_EXPORT int DSLE_CALLCONV get_var_shape(char *key, int *dims); // dims -> int[6]

/* Not needed? (also mostly not BMI standard) */
int update_until(double update_time);

DSLE_EXPORT void DSLE_CALLCONV get_version_string(char **version_string);
void get_attribute(char *name, char *value);
DSLE_EXPORT void DSLE_CALLCONV get_start_time(double *start_time_ptr);
DSLE_EXPORT void DSLE_CALLCONV get_end_time(double *end_time_ptr);
DSLE_EXPORT void DSLE_CALLCONV get_time_step(double *time_step_ptr);
DSLE_EXPORT void DSLE_CALLCONV get_current_time(double *current_time_ptr);
void set_dimr_logger(void *logptr); // Points to a Log object (see log.h in DIMR)
void set_logger_c_callback(
    void (*callback)(char *msg)); // Takes a function pointer (not sure what is prototype is)

// void set_logger(void *ptr); // Seems to have ambiguous definitions in DFM.

#if defined(__cplusplus)
}
#endif

#endif
