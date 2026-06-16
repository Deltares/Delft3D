#include <chrono>
#include <exception>
#include <functional>
#include <string>
#include <vector>
#include <filesystem>
#include <sstream>

#include "dflowfm_io_api/dflowfm_io_api.h"

#include "dflowfm_io/MduFile.h"
#include "dflowfm_io/MduData.h"
#include "dflowfm_io/MduValidator.h"
#include "dflowfm_io/IssueReport.h"

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

    void storeStaticStrings(std::vector<std::string>&& strings, const char*** strings_out, size_t* size_out)
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

dflowfm_io_result_t mdu_model_load_from_file(MduModelHandle handle, const char* filename, MduReportHandle* report_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(filename);
    ENSURE_ARGUMENT_NOT_NULL(report_out);

    return exceptionToResult([&]() {
        auto [mdu_data, report] = dflowfm_io::MduFile::Load(filename);
        *static_cast<dflowfm_io::MduData*>(handle) = mdu_data;
        *report_out = new dflowfm_io::IssueReport(std::move(report));
    });
}

dflowfm_io_result_t mdu_model_load_from_string(MduModelHandle handle, const char* data, size_t size, MduReportHandle* report_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(data);
    ENSURE_ARGUMENT_NOT_NULL(report_out);

    return exceptionToResult([&]() {
        const std::string data_str(data, size);
        std::istringstream stream(data_str);
        auto [mdu_data, report] = dflowfm_io::MduFile::Load(stream);
        *static_cast<dflowfm_io::MduData*>(handle) = mdu_data;
        *report_out = new dflowfm_io::IssueReport(std::move(report));
    });
}

dflowfm_io_result_t mdu_model_save_to_file(MduModelHandle handle, const char* filename)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(filename);

    return exceptionToResult([&]() {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        dflowfm_io::MduFile::Save(filename, *mdu_data);
    });
}

dflowfm_io_result_t mdu_model_save_to_string(MduModelHandle handle, const char** data_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(data_out);

    static std::string stored_string;

    return exceptionToResult([&]() {
        std::ostringstream stream;
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        dflowfm_io::MduFile::Save(stream, *mdu_data);
        stored_string = stream.str();
        *data_out = stored_string.c_str();
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

dflowfm_io_result_t mdu_model_get_datetime(MduModelHandle handle, const char* key, int64_t* epoch_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(epoch_out);

    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        const auto& tp = mdu_data->getValueAs<std::chrono::system_clock::time_point>(key);
        *epoch_out = std::chrono::duration_cast<std::chrono::seconds>(tp.time_since_epoch()).count();
    });
}

dflowfm_io_result_t mdu_model_get_enum(MduModelHandle handle, const char* key, int* enum_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(enum_out);

    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        *enum_out = mdu_data->getValueAs<dflowfm_io::EnumValue>(key).value;
    });
}

dflowfm_io_result_t mdu_model_get_string_list(MduModelHandle handle, const char* key, const char*** string_list_out, size_t* size_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(string_list_out);
    ENSURE_ARGUMENT_NOT_NULL(size_out);

    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        auto strings = mdu_data->getValueAs<std::vector<std::string>>(key);
        storeStaticStrings(std::move(strings), string_list_out, size_out);
    });
}

dflowfm_io_result_t mdu_model_get_path_list(MduModelHandle handle, const char* key, const char*** path_list_out, size_t* size_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(path_list_out);
    ENSURE_ARGUMENT_NOT_NULL(size_out);

    return exceptionToResult([&]() 
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        const auto& paths = mdu_data->getValueAs<std::vector<std::filesystem::path>>(key);

        std::vector<std::string> path_strings;
        path_strings.reserve(paths.size());
        for (const auto& p : paths) path_strings.push_back(p.string());

        storeStaticStrings(std::move(path_strings), path_list_out, size_out);
    });
}

dflowfm_io_result_t mdu_model_get_double_list(MduModelHandle handle, const char* key, const double** double_list_out, size_t* size_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(double_list_out);
    ENSURE_ARGUMENT_NOT_NULL(size_out);

    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        const auto& doubles = mdu_data->getValueAs<std::vector<double>>(key);
        *double_list_out = doubles.data();
        *size_out = doubles.size();
    });
}

dflowfm_io_result_t mdu_model_set_int(MduModelHandle handle, const char* key, int value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);

    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        mdu_data->setValue(key, value);
    });
}

dflowfm_io_result_t mdu_model_set_bool(MduModelHandle handle, const char* key, int value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        mdu_data->setValue(key, value != 0);
    });
}

dflowfm_io_result_t mdu_model_set_double(MduModelHandle handle, const char* key, double value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        mdu_data->setValue(key, value);
    });
}

dflowfm_io_result_t mdu_model_set_string(MduModelHandle handle, const char* key, const char* value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(value);
    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        mdu_data->setValue(key, std::string(value));
    });
}

dflowfm_io_result_t mdu_model_set_path(MduModelHandle handle, const char* key, const char* value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(value);
    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        mdu_data->setValue(key, std::filesystem::path(value));
    });
}

dflowfm_io_result_t mdu_model_set_datetime(MduModelHandle handle, const char* key, int64_t epoch)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        auto tp = std::chrono::system_clock::time_point(std::chrono::seconds(epoch));
        mdu_data->setValue(key, tp);
    });
}

dflowfm_io_result_t mdu_model_set_enum(MduModelHandle handle, const char* key, int enum_value)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        mdu_data->setValue(key, dflowfm_io::EnumValue{enum_value});
    });
}

dflowfm_io_result_t mdu_model_set_string_list(MduModelHandle handle, const char* key, const char** string_list, size_t size)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(string_list);
    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        std::vector<std::string> vec(string_list, string_list + size);
        mdu_data->setValue(key, vec);
    });
}

dflowfm_io_result_t mdu_model_set_path_list(MduModelHandle handle, const char* key, const char** path_list, size_t size)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(path_list);
    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        std::vector<std::filesystem::path> vec;
        for (size_t i = 0; i < size; ++i) vec.emplace_back(path_list[i]);
        mdu_data->setValue(key, vec);
    });
}

dflowfm_io_result_t mdu_model_set_double_list(MduModelHandle handle, const char* key, const double* double_list, size_t size)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(key);
    ENSURE_ARGUMENT_NOT_NULL(double_list);
    return exceptionToResult([&]()
    {
        auto mdu_data = static_cast<dflowfm_io::MduData*>(handle);
        std::vector<double> vec(double_list, double_list + size);
        mdu_data->setValue(key, vec);
    });
}
dflowfm_io_result_t mdu_report_destroy(MduReportHandle* handle)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);

    return exceptionToResult([&]() {
        if (*handle)
        {
            delete static_cast<dflowfm_io::IssueReport*>(*handle);
            *handle = nullptr;
        }
    });
}

dflowfm_io_result_t mdu_report_get_issue_list(MduReportHandle handle, const mdu_issue_t** issue_list_out, size_t* size_out)
{
    ENSURE_ARGUMENT_NOT_NULL(handle);
    ENSURE_ARGUMENT_NOT_NULL(issue_list_out);
    ENSURE_ARGUMENT_NOT_NULL(size_out);

    static std::vector<std::string> stored_messages;
    static std::vector<mdu_issue_t> stored_issues;

    return exceptionToResult([&]() {
        auto report = static_cast<dflowfm_io::IssueReport*>(handle);

        stored_messages.clear();
        stored_issues.clear();
        for (const auto& issue : *report)
        {
            stored_messages.push_back(issue.message);
        }

        // Build the issue array in a second pass so the stored message strings
        // are not reallocated while we capture pointers into them.
        size_t index = 0;
        for (const auto& issue : *report)
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