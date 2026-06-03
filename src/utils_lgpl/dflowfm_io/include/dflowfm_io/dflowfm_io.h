#ifndef DFLOWFM_IO_H
#define DFLOWFM_IO_H

#include <dflowfm_io/dflowfm_io_export.h>

#define DFLOWFM_IO_API DFLOWFM_IO_EXPORT

typedef int dflowfm_io_result_t;
#define DFLOWFM_IO_RESULT_SUCCESS 0
#define DFLOWFM_IO_RESULT_ERROR 1

#ifdef __cplusplus
extern "C" {
#endif

typedef void* MduModelHandle;

DFLOWFM_IO_API dflowfm_io_result_t mdu_model_create(MduModelHandle* out_handle);
DFLOWFM_IO_API dflowfm_io_result_t mdu_model_destroy(MduModelHandle handle);
DFLOWFM_IO_API dflowfm_io_result_t mdu_model_get_dummy_value(MduModelHandle handle, int* out_value);

DFLOWFM_IO_API const char* dflowfm_io_get_last_error(void);

#ifdef __cplusplus
}
#endif

#endif // DFLOWFM_IO_H
