#include <dflowfm_io_api/dflowfm_io_api.h>
#include <dflowfm_io/MduDocument.h>
#include <dflowfm_io/MduSchema.h>

#include <chrono>
#include <exception>
#include <filesystem>
#include <functional>
#include <sstream>
#include <string>
#include <vector>

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
    dflowfm_io::MduDocument* asDocument(mdu_handle_t handle)
    {
        return static_cast<dflowfm_io::MduDocument*>(handle);
    }

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

    void storeStaticStrings(std::vector<std::string>&& strings, const char*** strings_out, uint64_t* size_out)
    {
        static std::vector<std::string> stored_strings;
        static std::vector<const char*> string_ptrs;
        
        stored_strings = std::move(strings);
        string_ptrs.clear();
        for (const auto& str : stored_strings)
        {
            string_ptrs.push_back(str.c_str());
        }

        *strings_out = string_ptrs.data();
        *size_out = string_ptrs.size();
    }

    mdu_severity_t toCSeverity(dflowfm_io::Severity severity)
    {
        switch (severity)
        {
        case dflowfm_io::Severity::Warning:
            return MDU_SEVERITY_WARNING;
        case dflowfm_io::Severity::Error:
            return MDU_SEVERITY_ERROR;
        case dflowfm_io::Severity::Info:
        default:
            return MDU_SEVERITY_INFO;
        }
    }
} // namespace

const char* dflowfm_io_get_last_error()
{
    return last_error.c_str();
}

dflowfm_io_result_t mdu_create(mdu_handle_t* handle_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle_out);
    
    return exceptionToResult([&]()
    {
        *handle_out = new dflowfm_io::MduDocument();
    });
}

dflowfm_io_result_t mdu_destroy(mdu_handle_t* handle)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);

    return exceptionToResult([&]()
    {
        if (*handle)
        {
            delete asDocument(*handle);
            *handle = nullptr;
        }
    });
}

dflowfm_io_result_t mdu_load_from_file(mdu_handle_t handle, const char* filename)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(filename);

    return exceptionToResult([&]()
    {
        asDocument(handle)->Load(std::filesystem::path(filename));
    });
}

dflowfm_io_result_t mdu_load_from_string(mdu_handle_t handle, const char* data, uint64_t size)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(data);

    return exceptionToResult([&]()
    {
        std::istringstream stream(std::string(data, size));
        asDocument(handle)->Load(stream);
    });
}

dflowfm_io_result_t mdu_save_to_file(mdu_handle_t handle, const char* filename)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(filename);

    return exceptionToResult([&]()
    {
        asDocument(handle)->Save(std::filesystem::path(filename));
    });
}

dflowfm_io_result_t mdu_save_to_string(mdu_handle_t handle, const char** data_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(data_out);

    static std::string stored_string;

    return exceptionToResult([&]()
    {
        std::ostringstream stream;
        asDocument(handle)->Save(stream);
        stored_string = stream.str();
        *data_out = stored_string.c_str();
    });
}

dflowfm_io_result_t mdu_get_int(mdu_handle_t handle, const char* key, int32_t* int_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(int_out);

    return exceptionToResult([&]()
    {
        *int_out = asDocument(handle)->GetValue<int>(key);
    });
}

dflowfm_io_result_t mdu_get_bool(mdu_handle_t handle, const char* key, dflowfm_io_bool_t* bool_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(bool_out);

    return exceptionToResult([&]()
    {
        *bool_out = asDocument(handle)->GetValue<bool>(key) ? DFLOWFM_IO_TRUE : DFLOWFM_IO_FALSE;
    });
}

dflowfm_io_result_t mdu_get_double(mdu_handle_t handle, const char* key, double* double_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(double_out);

    return exceptionToResult([&]()
    {
        *double_out = asDocument(handle)->GetValue<double>(key);
    });
}

dflowfm_io_result_t mdu_get_string(mdu_handle_t handle, const char* key, const char** string_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(string_out);

    static std::string stored_string;

    return exceptionToResult([&]()
    {
        stored_string = asDocument(handle)->GetValue<std::string>(key);
        *string_out = stored_string.c_str();
    });
}

dflowfm_io_result_t mdu_get_path(mdu_handle_t handle, const char* key, const char** path_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(path_out);

    static std::string stored_path;

    return exceptionToResult([&]()
    {
        stored_path = asDocument(handle)->GetValue<std::filesystem::path>(key).string();
        *path_out = stored_path.c_str();
    });
}

dflowfm_io_result_t mdu_get_datetime(mdu_handle_t handle, const char* key, int64_t* epoch_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(epoch_out);

    return exceptionToResult([&]()
    {
        const auto& tp = asDocument(handle)->GetValue<std::chrono::system_clock::time_point>(key);
        *epoch_out = std::chrono::duration_cast<std::chrono::seconds>(tp.time_since_epoch()).count();
    });
}

dflowfm_io_result_t mdu_get_enum(mdu_handle_t handle, const char* key, int32_t* enum_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(enum_out);

    return exceptionToResult([&]()
    {
        *enum_out = asDocument(handle)->GetValue<dflowfm_io::EnumValue>(key).value;
    });
}

dflowfm_io_result_t mdu_get_string_list(mdu_handle_t handle, const char* key, const char*** string_list_out, uint64_t* size_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(string_list_out);
    ENSURE_ARGUMENT_NOT_NULL(size_out);

    return exceptionToResult([&]()
    {
        auto strings = asDocument(handle)->GetValue<std::vector<std::string>>(key);
        storeStaticStrings(std::move(strings), string_list_out, size_out);
    });
}

dflowfm_io_result_t mdu_get_path_list(mdu_handle_t handle, const char* key, const char*** path_list_out, uint64_t* size_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(path_list_out);
    ENSURE_ARGUMENT_NOT_NULL(size_out);

    return exceptionToResult([&]()
    {
        const auto& paths = asDocument(handle)->GetValue<std::vector<std::filesystem::path>>(key);

        std::vector<std::string> path_strings;
        path_strings.reserve(paths.size());
        for (const auto& p : paths) path_strings.push_back(p.string());

        storeStaticStrings(std::move(path_strings), path_list_out, size_out);
    });
}

dflowfm_io_result_t mdu_get_double_list(mdu_handle_t handle, const char* key, const double** double_list_out, uint64_t* size_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(double_list_out);
    ENSURE_ARGUMENT_NOT_NULL(size_out);

    return exceptionToResult([&]()
    {
        const auto& doubles = asDocument(handle)->GetValue<std::vector<double>>(key);
        *double_list_out = doubles.data();
        *size_out = doubles.size();
    });
}

dflowfm_io_result_t mdu_set_int(mdu_handle_t handle, const char* key, int32_t value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);

    return exceptionToResult([&]()
    {
        asDocument(handle)->SetValue(key, value);
    });
}

dflowfm_io_result_t mdu_set_bool(mdu_handle_t handle, const char* key, dflowfm_io_bool_t value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);

    return exceptionToResult([&]()
    {
        asDocument(handle)->SetValue(key, value != DFLOWFM_IO_FALSE);
    });
}

dflowfm_io_result_t mdu_set_double(mdu_handle_t handle, const char* key, double value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);

    return exceptionToResult([&]()
    {
        asDocument(handle)->SetValue(key, value);
    });
}

dflowfm_io_result_t mdu_set_string(mdu_handle_t handle, const char* key, const char* value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(value);

    return exceptionToResult([&]()
    {
        asDocument(handle)->SetValue(key, std::string(value));
    });
}

dflowfm_io_result_t mdu_set_path(mdu_handle_t handle, const char* key, const char* value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(value);

    return exceptionToResult([&]()
    {
        asDocument(handle)->SetValue(key, std::filesystem::path(value));
    });
}

dflowfm_io_result_t mdu_set_datetime(mdu_handle_t handle, const char* key, int64_t epoch)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);

    return exceptionToResult([&]()
    {
        auto tp = std::chrono::system_clock::time_point(std::chrono::seconds(epoch));
        asDocument(handle)->SetValue(key, tp);
    });
}

dflowfm_io_result_t mdu_set_enum(mdu_handle_t handle, const char* key, int32_t enum_value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);

    return exceptionToResult([&]()
    {
        asDocument(handle)->SetValue(key, dflowfm_io::EnumValue{enum_value});
    });
}

dflowfm_io_result_t mdu_set_string_list(mdu_handle_t handle, const char* key, const char** string_list, uint64_t size)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(string_list);

    return exceptionToResult([&]()
    {
        asDocument(handle)->SetValue(key, std::vector<std::string>(string_list, string_list + size));
    });
}

dflowfm_io_result_t mdu_set_path_list(mdu_handle_t handle, const char* key, const char** path_list, uint64_t size)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(path_list);

    return exceptionToResult([&]()
    {
        std::vector<std::filesystem::path> vec;
        for (uint64_t i = 0; i < size; ++i) vec.emplace_back(path_list[i]);
        asDocument(handle)->SetValue(key, std::move(vec));
    });
}

dflowfm_io_result_t mdu_set_double_list(mdu_handle_t handle, const char* key, const double* double_list, uint64_t size)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(double_list);

    return exceptionToResult([&]()
    {
        asDocument(handle)->SetValue(key, std::vector<double>(double_list, double_list + size));
    });
}

dflowfm_io_result_t mdu_get_issue_list(mdu_handle_t handle, const mdu_issue_t** issue_list_out, uint64_t* size_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(issue_list_out);
    ENSURE_ARGUMENT_NOT_NULL(size_out);

    static std::vector<std::string> stored_messages;
    static std::vector<mdu_issue_t> stored_issues;

    return exceptionToResult([&]() {
        auto report = asDocument(handle)->GetReport();

        stored_messages.clear();
        stored_issues.clear();
        for (const auto& issue : report)
        {
            stored_messages.push_back(issue.message);
        }

        // Build the issue array in a second pass so the stored message strings
        // are not reallocated while we capture pointers into them.
        size_t index = 0;
        for (const auto& issue : report)
        {
            stored_issues.push_back(mdu_issue_t{
                .line_number = issue.lineNumber.value_or(-1),
                .severity = toCSeverity(issue.severity),
                .message = stored_messages[index].c_str()});
            ++index;
        }

        *issue_list_out = stored_issues.data();
        *size_out = stored_issues.size();
    });
}