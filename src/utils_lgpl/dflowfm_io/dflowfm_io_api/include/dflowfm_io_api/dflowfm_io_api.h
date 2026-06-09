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
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_load_file(MduModelHandle handle, const char* filename);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_dummy_value(MduModelHandle handle, int* value_out);

DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_int(MduModelHandle handle, const char* key, int* int_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_double(MduModelHandle handle, const char* key, double* double_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_string(MduModelHandle handle, const char* key, const char** string_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_string_list(MduModelHandle handle, const char* key, const char*** string_list_out, size_t* size_out);

#ifdef __cplusplus
}
#endif

#endif // DFLOWFM_IO_H
