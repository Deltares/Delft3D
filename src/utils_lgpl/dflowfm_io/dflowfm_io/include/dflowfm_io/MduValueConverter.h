#pragma once

#include <dflowfm_io/MduSchema.h>

#include <optional>
#include <string>

namespace dflowfm_io
{
    /// @brief Converts a single MDU property value between its raw string form and a typed @ref Value.
    ///
    /// MduValueConverter provides two conversion directions:
    ///
    /// - **string → Value**: Converts a raw MDU value string into a typed @ref Value according
    ///   to the property's @ref ValueType. Returns std::nullopt if @p raw is not a valid
    ///   representation for the property's @ref ValueType.
    ///
    /// - **Value → string**: Converts a typed @ref Value into the raw string representation
    ///   used in an MDU file, matching the conventions applied when parsing.
    class MduValueConverter
    {
    public:
        /// @brief Converts a raw MDU value string into a typed @ref Value.
        /// @param schema The schema of the property whose value is being converted.
        /// @param raw The raw value string to convert.
        /// @return The converted @ref Value, or std::nullopt if @p raw is not a valid
        ///         representation for the property's @ref ValueType.
        static std::optional<Value> FromString(const PropertySchema& schema, const std::string& raw);

        /// @brief Converts a typed @ref Value into its raw MDU string representation.
        /// @param schema The schema of the property whose value is being converted.
        /// @param value  The typed value to convert. Its active alternative must match the
        ///               property's @ref ValueType.
        /// @return The raw string representation of @p value for use in an MDU file.
        /// @throws std::out_of_range if @p value is an enumeration whose numeric value is not
        ///         present in @p schema.
        static std::string ToString(const PropertySchema& schema, const Value& value);
    };

} // namespace dflowfm_io