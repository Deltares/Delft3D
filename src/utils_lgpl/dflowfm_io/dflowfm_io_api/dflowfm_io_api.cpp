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
    std::string last_error;

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

    mdu_severity_t toCSeverity(dflowfm_io::Severity severity)
    {
        switch (severity)
        {
        case dflowfm_io::Severity::Warning:
            return MDU_SEVERITY_WARNING;
        case dflowfm_io::Severity::Error:
            return MDU_SEVERITY_ERROR;
        case dflowfm_io::Severity::Info:
            return MDU_SEVERITY_INFO;
        case dflowfm_io::Severity::Debug:
            return MDU_SEVERITY_DEBUG;
        default:
            return MDU_SEVERITY_INFO;
        }
    }

    class StringStorage
    {
    public:
        [[nodiscard]] const char* clearAndStore(std::string str)
        { 
            size_t dummy_size = 0;
            return *clearAndStore({std::move(str)}, dummy_size);
        }

        [[nodiscard]] const char** clearAndStore(std::vector<std::string>&& strings, size_t& size_out)
        { 
            stored_strings = std::move(strings);
            string_ptrs.clear();
            for (const auto& str : stored_strings)
            {
                string_ptrs.push_back(str.c_str());
            }
            size_out = string_ptrs.size();
            return string_ptrs.data();
        }

    private:
        std::vector<std::string> stored_strings;
        std::vector<const char*> string_ptrs;
    };
} // namespace

struct mdu_handle_t
{
    dflowfm_io::MduDocument mduDocument;
    dflowfm_io::IssueReport lastIssueReport;

    StringStorage stringStorage;
    std::vector<mdu_issue_t> storedIssues;
};

const char* dflowfm_io_get_last_error()
{
    return last_error.c_str();
}

dflowfm_io_result_t mdu_create(mdu_handle_t** handle_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle_out);
    
    return exceptionToResult([&]()
    {
        *handle_out = new mdu_handle_t();
    });
}

dflowfm_io_result_t mdu_destroy(mdu_handle_t** handle)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);

    return exceptionToResult([&]()
    {
        if (*handle)
        {
            delete *handle;
            *handle = nullptr;
        }
    });
}

dflowfm_io_result_t mdu_load_from_file(mdu_handle_t* handle, const char* filename)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(filename);

    return exceptionToResult([&]()
    {
        handle->lastIssueReport = handle->mduDocument.Load(std::filesystem::path(filename));
    });
}

dflowfm_io_result_t mdu_load_from_string(mdu_handle_t* handle, const char* data, uint64_t size)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(data);

    return exceptionToResult([&]()
    {
        std::istringstream stream(std::string(data, size));
        handle->lastIssueReport = handle->mduDocument.Load(stream);
    });
}

dflowfm_io_result_t mdu_save_to_file(mdu_handle_t* handle, const char* filename)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(filename);

    return exceptionToResult([&]()
    {
        handle->mduDocument.Save(std::filesystem::path(filename));
    });
}

dflowfm_io_result_t mdu_save_to_string(mdu_handle_t* handle, const char** data_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(data_out);

    return exceptionToResult([&]()
    {
        std::ostringstream stream;
        handle->mduDocument.Save(stream);
        *data_out = handle->stringStorage.clearAndStore(stream.str());
    });
}

dflowfm_io_result_t mdu_get_int(mdu_handle_t* handle, const char* key, int32_t* int_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(int_out);

    return exceptionToResult([&]()
    {
        *int_out = handle->mduDocument.GetValue<int>(key);
    });
}

dflowfm_io_result_t mdu_get_bool(mdu_handle_t* handle, const char* key, dflowfm_io_bool_t* bool_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(bool_out);

    return exceptionToResult([&]()
    {
        *bool_out = handle->mduDocument.GetValue<bool>(key) ? DFLOWFM_IO_TRUE : DFLOWFM_IO_FALSE;
    });
}

dflowfm_io_result_t mdu_get_double(mdu_handle_t* handle, const char* key, double* double_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(double_out);

    return exceptionToResult([&]()
    {
        *double_out = handle->mduDocument.GetValue<double>(key);
    });
}

dflowfm_io_result_t mdu_get_string(mdu_handle_t* handle, const char* key, const char** string_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(string_out);

    return exceptionToResult([&]()
    {
        *string_out = handle->stringStorage.clearAndStore(handle->mduDocument.GetValue<std::string>(key));
    });
}

dflowfm_io_result_t mdu_get_path(mdu_handle_t* handle, const char* key, const char** path_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(path_out);

    return exceptionToResult([&]()
    {
        *path_out = handle->stringStorage.clearAndStore(handle->mduDocument.GetValue<std::filesystem::path>(key).string());
    });
}

dflowfm_io_result_t mdu_get_datetime(mdu_handle_t* handle, const char* key, int64_t* epoch_out, dflowfm_io_bool_t* has_value_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(epoch_out);
    ENSURE_ARGUMENT_NOT_NULL(has_value_out);

    return exceptionToResult([&]()
    {
        const auto& tp = handle->mduDocument.GetValue<std::optional<std::chrono::system_clock::time_point>>(key);
        *epoch_out = tp.has_value() ? std::chrono::duration_cast<std::chrono::seconds>(tp->time_since_epoch()).count() : 0;
        *has_value_out = tp.has_value() ? DFLOWFM_IO_TRUE : DFLOWFM_IO_FALSE;
    });
}

dflowfm_io_result_t mdu_get_string_enum(mdu_handle_t* handle, const char* key, const char** enum_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(enum_out);

    return exceptionToResult([&]()
    {
        *enum_out = handle->stringStorage.clearAndStore(handle->mduDocument.GetValue<dflowfm_io::StringEnumValue>(key).value);
    });
}

dflowfm_io_result_t mdu_get_int_enum(mdu_handle_t* handle, const char* key, int32_t* enum_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(enum_out);

    return exceptionToResult([&]()
    {
        *enum_out = handle->mduDocument.GetValue<dflowfm_io::IntEnumValue>(key).value;
    });
}

dflowfm_io_result_t mdu_get_string_list(mdu_handle_t* handle, const char* key, const char*** string_list_out, uint64_t* size_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(string_list_out);
    ENSURE_ARGUMENT_NOT_NULL(size_out);

    return exceptionToResult([&]()
    {
        auto strings = handle->mduDocument.GetValue<std::vector<std::string>>(key);
        *string_list_out = handle->stringStorage.clearAndStore(std::move(strings), *size_out);
    });
}

dflowfm_io_result_t mdu_get_path_list(mdu_handle_t* handle, const char* key, const char*** path_list_out, uint64_t* size_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(path_list_out);
    ENSURE_ARGUMENT_NOT_NULL(size_out);

    return exceptionToResult([&]()
    {
        const auto& paths = handle->mduDocument.GetValue<std::vector<std::filesystem::path>>(key);

        std::vector<std::string> path_strings;
        path_strings.reserve(paths.size());
        for (const auto& p : paths) path_strings.push_back(p.string());

        *path_list_out = handle->stringStorage.clearAndStore(std::move(path_strings), *size_out);
    });
}

dflowfm_io_result_t mdu_get_double_list(mdu_handle_t* handle, const char* key, const double** double_list_out, uint64_t* size_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(double_list_out);
    ENSURE_ARGUMENT_NOT_NULL(size_out);

    return exceptionToResult([&]()
    {
        const auto& doubles = handle->mduDocument.GetValue<std::vector<double>>(key);
        *double_list_out = doubles.data();
        *size_out = doubles.size();
    });
}

dflowfm_io_result_t mdu_set_int(mdu_handle_t* handle, const char* key, int32_t value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);

    return exceptionToResult([&]()
    {
        handle->mduDocument.SetValue(key, value);
    });
}

dflowfm_io_result_t mdu_set_bool(mdu_handle_t* handle, const char* key, dflowfm_io_bool_t value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);

    return exceptionToResult([&]()
    {
        handle->mduDocument.SetValue(key, value != DFLOWFM_IO_FALSE);
    });
}

dflowfm_io_result_t mdu_set_double(mdu_handle_t* handle, const char* key, double value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);

    return exceptionToResult([&]()
    {
        handle->mduDocument.SetValue(key, value);
    });
}

dflowfm_io_result_t mdu_set_string(mdu_handle_t* handle, const char* key, const char* value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(value);

    return exceptionToResult([&]()
    {
        handle->mduDocument.SetValue(key, std::string(value));
    });
}

dflowfm_io_result_t mdu_set_path(mdu_handle_t* handle, const char* key, const char* value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(value);

    return exceptionToResult([&]()
    {
        handle->mduDocument.SetValue(key, std::filesystem::path(value));
    });
}

dflowfm_io_result_t mdu_set_datetime(mdu_handle_t* handle, const char* key, int64_t epoch, dflowfm_io_bool_t has_value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);

    return exceptionToResult([&]()
    {
        std::optional<std::chrono::system_clock::time_point> tp;
        if (has_value != DFLOWFM_IO_FALSE)
            tp = std::chrono::system_clock::time_point(std::chrono::seconds(epoch));
        handle->mduDocument.SetValue(key, tp);
    });
}

dflowfm_io_result_t mdu_set_string_enum(mdu_handle_t* handle, const char* key, const char* enum_value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(enum_value);

    return exceptionToResult([&]()
    {
        handle->mduDocument.SetValue(key, dflowfm_io::StringEnumValue{std::string(enum_value)});
    });
}

dflowfm_io_result_t mdu_set_int_enum(mdu_handle_t* handle, const char* key, int32_t enum_value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);

    return exceptionToResult([&]()
    {
        handle->mduDocument.SetValue(key, dflowfm_io::IntEnumValue{enum_value});
    });
}

dflowfm_io_result_t mdu_set_string_list(mdu_handle_t* handle, const char* key, const char** string_list, uint64_t size)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(string_list);

    return exceptionToResult([&]()
    {
        handle->mduDocument.SetValue(key, std::vector<std::string>(string_list, string_list + size));
    });
}

dflowfm_io_result_t mdu_set_path_list(mdu_handle_t* handle, const char* key, const char** path_list, uint64_t size)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(path_list);

    return exceptionToResult([&]()
    {
        std::vector<std::filesystem::path> vec;
        for (uint64_t i = 0; i < size; ++i) vec.emplace_back(path_list[i]);
        handle->mduDocument.SetValue(key, std::move(vec));
    });
}

dflowfm_io_result_t mdu_set_double_list(mdu_handle_t* handle, const char* key, const double* double_list, uint64_t size)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(double_list);

    return exceptionToResult([&]()
    {
        handle->mduDocument.SetValue(key, std::vector<double>(double_list, double_list + size));
    });
}

dflowfm_io_result_t mdu_get_issue_list(mdu_handle_t* handle, const mdu_issue_t** issue_list_out, uint64_t* size_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(issue_list_out);
    ENSURE_ARGUMENT_NOT_NULL(size_out);

    return exceptionToResult([&]() {
        handle->storedIssues.clear();
        for (const auto& issue : handle->lastIssueReport.GetIssues())
        {
            handle->storedIssues.push_back(mdu_issue_t{
                .line_number = issue.lineNumber.value_or(-1),
                .severity = toCSeverity(issue.severity),
                .message = issue.message.c_str()});
        }

        *issue_list_out = handle->storedIssues.data();
        *size_out = handle->storedIssues.size();
    });
}