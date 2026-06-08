#include <exception>
#include <functional>
#include <string>

#include <dflowfm_io_api/dflowfm_io_api.h>

#include <dflowfm_io/MduModel.h>

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

const char* dflowfm_io_get_last_error()
{
    return last_error.c_str();
}

dflowfm_io_result_t mdu_model_create(MduModelHandle* handle_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle_out);

    return exception_to_result([&]()
    {
        *handle_out = new dflowfm_io::MduModel();
    });
}

dflowfm_io_result_t mdu_model_destroy(MduModelHandle* handle)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);

    return exception_to_result([&]()
    {
        if (*handle)
        {
            delete static_cast<dflowfm_io::MduModel*>(*handle);
            *handle = nullptr;
        }
    });
}

dflowfm_io_result_t mdu_model_get_dummy_value(MduModelHandle handle, int* value_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(value_out);

    return exception_to_result([&]()
    {
        *value_out = static_cast<dflowfm_io::MduModel*>(handle)->GetDummyValue();
    });
}
