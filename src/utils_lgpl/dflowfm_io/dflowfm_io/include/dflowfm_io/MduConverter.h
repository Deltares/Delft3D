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
    /// MduConverter provides two conversion directions:
    ///
    /// - **IniData → MduData**: Validates the parsed INI file representation against the
    ///   given @ref MduSchema and converts it to a typed @ref MduData. All issues found during
    ///   validation and conversion are collected in the returned @ref IssueReport rather than
    ///   thrown.
    ///
    /// - **MduData → IniData**: Converts a typed @ref MduData back to an @ref ini::IniData,
    ///   in the section and property order defined by the given @ref MduSchema.
    class MduConverter
    {
    public:
        /// @brief Converts the parsed INI file representation to a typed @ref MduData.
        /// @param iniData The parsed MDU file contents to convert.
        /// @param schema  The schema to validate and convert against.
        static std::pair<MduData, IssueReport> Convert(const ini::IniData& iniData, const MduSchema& schema);

        /// @brief Converts typed @ref MduData to an INI file representation.
        /// @param mduData The typed MDU data to convert.
        /// @param schema The schema describing section/property order.
        /// @throws std::logic_error if a required property is absent from @p mduData.
        static ini::IniData Convert(const MduData& mduData, const MduSchema& schema);

        /// @brief Converts a raw MDU value string into a typed @ref Value.
        /// @param schema The schema of the property whose value is being converted.
        /// @param raw The raw value string to convert.
        /// @return The converted @ref Value.
        /// @throws std::invalid_argument if @p raw is not a valid representation for the
        ///         property's @ref schema.
        static Value ValueFromString(const PropertySchema& schema, const std::string& raw);

        /// @brief Converts a typed @ref Value into its raw MDU string representation.
        /// @param schema The schema of the property whose value is being converted.
        /// @param value  The typed value to convert. Its active alternative must match the
        ///               property's @ref ValueType.
        /// @return The raw string representation of @p value for use in an MDU file.
        /// @throws std::invalid_argument if @p value is not a valid representation for the
        ///         property's @ref ValueType
        static std::string ValueToString(const PropertySchema& schema, const Value& value);
    };

} // namespace dflowfm_io