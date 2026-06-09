#include <exception>
#include <functional>
#include <string>
#include <vector>

#include <dflowfm_io_api/dflowfm_io_api.h>

#include <dflowfm_io/MduData.h>

static std::string last_error;

#define ENSURE_ARGUMENT_NOT_NULL(arg) \
    do { \
        if (!(arg)) \
        { \
            last_error = std::string(__func__) + ": invalid argument '" #arg "' is null"; \
            return DFLOWFM_IO_RESULT_ERROR; \
        } \
    } while (0)

namespace
{
    dflowfm_io_result_t exceptionToResult(const std::function<void()>& func)
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
} // namespace

const char* dflowfm_io_get_last_error()
{
    return last_error.c_str();
}

dflowfm_io_result_t mdu_model_create(MduModelHandle* handle_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle_out);

    return exceptionToResult([&]()
    {
        *handle_out = new dflowfm_io::MduData();
    });
}

dflowfm_io_result_t mdu_model_destroy(MduModelHandle* handle)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);

    return exceptionToResult([&]()
    {
        if (*handle)
        {
            delete static_cast<dflowfm_io::MduData*>(*handle);
            *handle = nullptr;
        }
    });
}

dflowfm_io_result_t mdu_model_get_dummy_value(MduModelHandle handle, int* value_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(value_out);

    return exceptionToResult([&]()
    {
        *value_out = static_cast<dflowfm_io::MduData*>(handle)->GetDummyValue();
    });
}

dflowfm_io_result_t mdu_model_get_string(MduModelHandle handle, const char* key, const char** string_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(string_out);

    static std::string stored_string;

    return exceptionToResult([&]()
    {
        stored_string = "some_string_value";
        *string_out = stored_string.c_str();
    });
}

dflowfm_io_result_t mdu_model_get_string_list(MduModelHandle handle, const char* key, const char*** string_list_out, size_t* size_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(string_list_out);
    ENSURE_ARGUMENT_NOT_NULL(size_out);

    static std::vector<std::string> stored_strings;
    static std::vector<const char*> stored_pointers;

    return exceptionToResult([&]()
    {
        stored_strings.clear();
        stored_pointers.clear();
        stored_strings.emplace_back("first_string");
        stored_strings.emplace_back("second_string");
        for (const auto& s : stored_strings)
        {
            stored_pointers.emplace_back(s.c_str());
        }
        
        *string_list_out = stored_pointers.data();
        *size_out = stored_pointers.size();
    });
}