#ifndef DFLOWFM_IO_H
#define DFLOWFM_IO_H

#include <dflowfm_io_api/dflowfm_io_api_export.h>

#include <stdint.h>

#define DFLOWFM_IO_API DFLOWFM_IO_EXPORT

typedef int dflowfm_io_result_t;
#define DFLOWFM_IO_RESULT_SUCCESS 0
#define DFLOWFM_IO_RESULT_ERROR 1

#ifdef __cplusplus
extern "C" {
#endif

typedef void* MduDocumentHandle;

typedef enum mdu_severity_t
{
    MDU_SEVERITY_INFO = 0,
    MDU_SEVERITY_WARNING = 1,
    MDU_SEVERITY_ERROR = 2
} mdu_severity_t;

typedef struct mdu_issue_t
{
    int line_number;
    mdu_severity_t severity;
    const char* message;
} mdu_issue_t;

DFLOWFM_IO_API_EXPORT const char* dflowfm_io_get_last_error();

DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_document_create(MduDocumentHandle* handle_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_document_destroy(MduDocumentHandle* handle);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_document_load_from_file(MduDocumentHandle handle, const char* filename);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_document_load_from_string(MduDocumentHandle handle, const char* data, size_t size);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_document_save_to_file(MduDocumentHandle handle, const char* filename);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_document_save_to_string(MduDocumentHandle handle, const char** data_out);

DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_dummy_value(MduDocumentHandle handle, int* value_out);

DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_int(MduDocumentHandle handle, const char* key, int* int_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_bool(MduDocumentHandle handle, const char* key, int* bool_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_double(MduDocumentHandle handle, const char* key, double* double_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_string(MduDocumentHandle handle, const char* key, const char** string_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_path(MduDocumentHandle handle, const char* key, const char** path_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_datetime(MduDocumentHandle handle, const char* key, int64_t* epoch_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_enum(MduDocumentHandle handle, const char* key, int* enum_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_string_list(MduDocumentHandle handle, const char* key, const char*** string_list_out, size_t* size_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_path_list(MduDocumentHandle handle, const char* key, const char*** path_list_out, size_t* size_out);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_get_double_list(MduDocumentHandle handle, const char* key, const double** double_list_out, size_t* size_out);

DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_set_int(MduDocumentHandle handle, const char* key, int value);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_set_bool(MduDocumentHandle handle, const char* key, int value);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_set_double(MduDocumentHandle handle, const char* key, double value);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_set_string(MduDocumentHandle handle, const char* key, const char* value);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_set_path(MduDocumentHandle handle, const char* key, const char* value);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_set_datetime(MduDocumentHandle handle, const char* key, int64_t epoch);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_set_enum(MduDocumentHandle handle, const char* key, int enum_value);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_set_string_list(MduDocumentHandle handle, const char* key, const char** string_list, size_t size);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_set_path_list(MduDocumentHandle handle, const char* key, const char** path_list, size_t size);
DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_model_set_double_list(MduDocumentHandle handle, const char* key, const double* double_list, size_t size);

DFLOWFM_IO_API_EXPORT dflowfm_io_result_t mdu_report_get_issue_list(MduDocumentHandle handle, const mdu_issue_t** issue_list_out, size_t* size_out);

#ifdef __cplusplus
}
#endif

#endif // DFLOWFM_IO_H
