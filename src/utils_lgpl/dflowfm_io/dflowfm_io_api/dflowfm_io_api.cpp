#include <exception>
#include <functional>
#include <string>
#include <vector>
#include <filesystem>
#include <sstream>

#include <dflowfm_io_api/dflowfm_io_api.h>

#include "dflowfm_io/MduFile.h"

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

dflowfm_io_result_t mdu_model_load_from_file(MduModelHandle handle, const char* filename)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(filename);

    return exceptionToResult([&]() {

        auto mdu_file = dflowfm_io::MduFile::LoadFrom(filename);
        *static_cast<dflowfm_io::MduData*>(handle) = mdu_file.GetData();
    });
}

dflowfm_io_result_t mdu_model_load_from_string(MduModelHandle handle, const char* data, size_t size)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(data);

    return exceptionToResult([&]() {
        const std::string data_str(data, size);
        std::istringstream stream(data_str);
        auto mdu_file = dflowfm_io::MduFile::LoadFrom(stream);
        *static_cast<dflowfm_io::MduData*>(handle) = mdu_file.GetData();
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

dflowfm_io_result_t mdu_model_get_int(MduModelHandle handle, const char* key, int* int_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(int_out);

    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        *int_out = mdu_data->getValueAs<int>(key);
    });
}

dflowfm_io_result_t mdu_model_get_bool(MduModelHandle handle, const char* key, int* bool_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(bool_out);

    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        *bool_out = mdu_data->getValueAs<bool>(key) ? 1 : 0;
    });
}

dflowfm_io_result_t mdu_model_get_double(MduModelHandle handle, const char* key, double* double_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(double_out);

    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        *double_out = mdu_data->getValueAs<double>(key);
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
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        stored_string = mdu_data->getValueAs<std::string>(key);

        *string_out = stored_string.c_str();
    });
}

dflowfm_io_result_t mdu_model_get_path(MduModelHandle handle, const char* key, const char** path_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(path_out);

    static std::string stored_path;

    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        stored_path = mdu_data->getValueAs<std::filesystem::path>(key).string();

        *path_out = stored_path.c_str();
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
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        stored_strings = mdu_data->getValueAs<std::vector<std::string>>(key);
        stored_pointers.clear();
        for (const auto& s : stored_strings) stored_pointers.push_back(s.c_str());

        *string_list_out = stored_pointers.data();
        *size_out = stored_pointers.size();
    });
}

dflowfm_io_result_t mdu_model_get_path_list(MduModelHandle handle, const char* key, const char*** path_list_out, size_t* size_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(path_list_out);
    ENSURE_ARGUMENT_NOT_NULL(size_out);

    static std::vector<std::string> stored_paths;
    static std::vector<const char*> stored_pointers;

    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        const auto& paths = mdu_data->getValueAs<std::vector<std::filesystem::path>>(key);
        stored_paths.clear();
        stored_pointers.clear();
        for (const auto& p : paths) stored_paths.push_back(p.string());
        for (const auto& s : stored_paths) stored_pointers.push_back(s.c_str());

        *path_list_out = stored_pointers.data();
        *size_out = stored_pointers.size();
    });
}
