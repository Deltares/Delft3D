#ifndef DFLOWFM_IO_H
#define DFLOWFM_IO_H

#include <dflowfm_io_api/dflowfm_io_api_export.h>

#define DFLOWFM_IO_API DFLOWFM_IO_EXPORT

typedef int dflowfm_io_result_t;
#define DFLOWFM_IO_RESULT_SUCCESS 0
#define DFLOWFM_IO_RESULT_ERROR 1

#ifdef __cplusplus
extern "C" {
#endif

typedef void* MduModelHandle;

DFLOWFM_IO_API_EXPORT const char* dflowfm_io_get_last_error();

DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_create(MduModelHandle* handle_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_destroy(MduModelHandle* handle);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_load_from_file(MduModelHandle handle, const char* filename);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_load_from_string(MduModelHandle handle, const char* data, size_t size);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_save_to_file(MduModelHandle handle, const char* filename);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_save_to_string(MduModelHandle handle, const char** data_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_dummy_value(MduModelHandle handle, int* value_out);

DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_int(MduModelHandle handle, const char* key, int* int_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_bool(MduModelHandle handle, const char* key, int* bool_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_double(MduModelHandle handle, const char* key, double* double_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_string(MduModelHandle handle, const char* key, const char** string_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_path(MduModelHandle handle, const char* key, const char** path_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_string_list(MduModelHandle handle, const char* key, const char*** string_list_out, size_t* size_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_path_list(MduModelHandle handle, const char* key, const char*** path_list_out, size_t* size_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_double_list(MduModelHandle handle, const char* key, const double** double_list_out, size_t* size_out);

DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_set_int(MduModelHandle handle, const char* key, int value);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_set_bool(MduModelHandle handle, const char* key, int value);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_set_double(MduModelHandle handle, const char* key, double value);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_set_string(MduModelHandle handle, const char* key, const char* value);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_set_path(MduModelHandle handle, const char* key, const char* value);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_set_string_list(MduModelHandle handle, const char* key, const char** string_list, size_t size);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_set_path_list(MduModelHandle handle, const char* key, const char** path_list, size_t size);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_set_double_list(MduModelHandle handle, const char* key, const double* double_list, size_t size);

#ifdef __cplusplus
}
#endif

#endif // DFLOWFM_IO_H
