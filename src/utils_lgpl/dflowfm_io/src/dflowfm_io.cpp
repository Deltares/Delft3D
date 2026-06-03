#include <dflowfm_io/dflowfm_io.h>
#include <dflowfm_io/MduModel.h>

#include <exception>
#include <functional>
#include <string>

static std::string last_error;

#define ENSURE_ARGUMENT_NOT_NULL(arg) \
    do { \
        if (!(arg)) \
        { \
            last_error = std::string(__func__) + ": invalid argument '" #arg "' is null"; \
            return DFLOWFM_IO_RESULT_ERROR; \
        } \
    } while (0)

static dflowfm_io_result_t exception_to_result(const std::function<void()>& func)
{
    try
    {
        func();
        return DFLOWFM_IO_RESULT_SUCCESS;
    }
    catch (const std::exception& e)
    {
        last_error = e.what();
        return DFLOWFM_IO_RESULT_ERROR;
    }
    catch (...)
    {
        last_error = "unknown error";
        return DFLOWFM_IO_RESULT_ERROR;
    }
}

const char* dflowfm_io_get_last_error(void)
{
    return last_error.c_str();
}

dflowfm_io_result_t mdu_model_create(MduModelHandle* out_handle)
{
    ENSURE_ARGUMENT_NOT_NULL(out_handle);

    return exception_to_result([&]()
    {
        *out_handle = new dflowfm_io::MduModel();
    });
}

dflowfm_io_result_t mdu_model_destroy(MduModelHandle handle)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);

    return exception_to_result([&]()
    {
        delete static_cast<dflowfm_io::MduModel*>(handle);
    });
}

dflowfm_io_result_t mdu_model_get_dummy_value(MduModelHandle handle, int* out_value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(out_value);

    return exception_to_result([&]()
    {
        *out_value = static_cast<dflowfm_io::MduModel*>(handle)->GetDummyValue();
    });
}
