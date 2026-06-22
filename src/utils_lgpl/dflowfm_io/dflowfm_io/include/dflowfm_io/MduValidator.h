#pragma once

#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/IssueReport.h>

namespace ini
{
    class IniData;
}

namespace dflowfm_io
{

    /// @brief Performs schema-based validation of MDU file contents.
    ///
    /// MduValidator checks a parsed MDU file (represented as an @ref ini::IniData) against
    /// the MDU schema (@ref MDU_SCHEMA). The validation consists of two passes:
    ///
    /// 1. **Required validation** — verifies that all sections and properties marked as
    ///    required in the schema are present and have a value. Missing optional properties
    ///    that carry a default value are reported as informational issues.
    ///
    /// 2. **Unsupported validation** — reports any sections or properties found in the
    ///    parsed data that are not defined in the schema.
    ///
    /// All findings are collected into an @ref IssueReport and returned to the caller.
    class MduValidator
    {
    public:
        /// @brief Validates the given MDU data against the MDU schema.
        /// @param iniData The parsed MDU file contents to validate.
        /// @return An @ref IssueReport containing all errors, warnings, and informational
        ///         messages produced during validation. The report is empty if the data
        ///         fully conforms to the schema.
        static IssueReport Validate(const ini::IniData& iniData);

    private:
        static void ValidateRequired(const ini::IniData& iniData, IssueReport& report);
        static void ValidateUnsupported(const ini::IniData& iniData, IssueReport& report);
    };

} // namespace dflowfm_io