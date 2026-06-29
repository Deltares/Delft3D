#ifndef DFLOWFM_IO_H
#define DFLOWFM_IO_H

#include <dflowfm_io_api/dflowfm_io_api_export.h>

#include <stdint.h>

#define DFLOWFM_IO_API DFLOWFM_IO_EXPORT

typedef int32_t dflowfm_io_result_t;
#define DFLOWFM_IO_RESULT_SUCCESS 0
#define DFLOWFM_IO_RESULT_ERROR 1

typedef int32_t dflowfm_io_bool_t;
#define DFLOWFM_IO_FALSE 0
#define DFLOWFM_IO_TRUE 1

#ifdef __cplusplus
extern "C" {
#endif

typedef void* mdu_handle_t;

typedef enum mdu_severity_t
{
    MDU_SEVERITY_INFO = 0,
    MDU_SEVERITY_WARNING = 1,
    MDU_SEVERITY_ERROR = 2
} mdu_severity_t;

typedef struct mdu_issue_t
{
    int32_t line_number;
    mdu_severity_t severity;
    const char* message;
} mdu_issue_t;

DFLOWFM_IO_API_EXPORT const char* dflowfm_io_get_last_error();

DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_create(mdu_handle_t* handle_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_destroy(mdu_handle_t* handle);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_load_from_file(mdu_handle_t handle, const char* filename);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_load_from_string(mdu_handle_t handle, const char* data, uint64_t size);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_save_to_file(mdu_handle_t handle, const char* filename);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_save_to_string(mdu_handle_t handle, const char** data_out);

DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_get_dummy_value(mdu_handle_t handle, int32_t* value_out);

DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_get_int(mdu_handle_t handle, const char* key, int32_t* int_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_get_bool(mdu_handle_t handle, const char* key, dflowfm_io_bool_t* bool_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_get_double(mdu_handle_t handle, const char* key, double* double_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_get_string(mdu_handle_t handle, const char* key, const char** string_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_get_path(mdu_handle_t handle, const char* key, const char** path_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_get_datetime(mdu_handle_t handle, const char* key, int64_t* epoch_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_get_enum(mdu_handle_t handle, const char* key, int32_t* enum_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_get_string_list(mdu_handle_t handle, const char* key, const char*** string_list_out, uint64_t* size_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_get_path_list(mdu_handle_t handle, const char* key, const char*** path_list_out, uint64_t* size_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_get_double_list(mdu_handle_t handle, const char* key, const double** double_list_out, uint64_t* size_out);

DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_set_int(mdu_handle_t handle, const char* key, int32_t value);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_set_bool(mdu_handle_t handle, const char* key, dflowfm_io_bool_t value);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_set_double(mdu_handle_t handle, const char* key, double value);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_set_string(mdu_handle_t handle, const char* key, const char* value);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_set_path(mdu_handle_t handle, const char* key, const char* value);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_set_datetime(mdu_handle_t handle, const char* key, int64_t epoch);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_set_enum(mdu_handle_t handle, const char* key, int32_t enum_value);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_set_string_list(mdu_handle_t handle, const char* key, const char** string_list, uint64_t size);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_set_path_list(mdu_handle_t handle, const char* key, const char** path_list, uint64_t size);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_set_double_list(mdu_handle_t handle, const char* key, const double* double_list, uint64_t size);

DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_get_issue_list(mdu_handle_t handle, const mdu_issue_t** issue_list_out, uint64_t* size_out);

#ifdef __cplusplus
}
#endif

#endif // DFLOWFM_IO_H
