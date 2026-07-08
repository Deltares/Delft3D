#pragma once

#include <dflowfm_io/IssueReport.h>
#include <dflowfm_io/MduData.h>

#include <utility>

namespace ini
{
    class IniData;
}

namespace dflowfm_io
{
    /// @brief Converts between the parsed INI file representation and the typed @ref MduData.
    ///
    /// MduDataConverter provides two conversion directions:
    ///
    /// - **IniData → MduData**: Validates the parsed INI file representation and converts
    ///   it to a typed @ref MduData. All issues found during validation and conversion are
    ///   collected in the returned @ref IssueReport rather than thrown.
    ///
    /// - **MduData → IniData**: Converts a typed @ref MduData back to an @ref ini::IniData,
    ///   in the section and property order defined by the MDU schema.
    class MduDataConverter
    {
    public:
        /// @brief Converts the parsed INI file representation to a typed @ref MduData.
        /// @param iniData The parsed MDU file contents to convert.
        /// @return A pair of the populated @ref MduData and an @ref IssueReport describing
        ///         any validation or conversion problems.
        static std::pair<MduData, IssueReport> Convert(const ini::IniData& iniData);

        /// @brief Converts typed @ref MduData to an INI file representation.
        /// @param mduData  The typed MDU data to convert.
        /// @return An @ref ini::IniData ready to be written to disk.
        /// @throws std::logic_error if a required property is absent from @p mduData.
        static ini::IniData Convert(const MduData& mduData);
    };

} // namespace dflowfm_io