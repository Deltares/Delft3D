#pragma once

#include <dflowfm_io/StringUtils.h>

#include <chrono>
#include <filesystem>
#include <locale>
#include <map>
#include <string>
#include <variant>
#include <vector>

namespace dflowfm_io
{
    /// @brief Describes the expected data type of a property value in an MDU file.
    enum class ValueType
    {
        String,
        Int,
        Float,
        IntBool,
        Path,
        Enum,
        IntEnum,
        PathList,
        StringList,
        FloatList,
        DateTime,
    };

    /// @brief Wrapper for enum values so they can be distinguished from 
    ///        plain integers in the @ref Value variant.
    struct EnumValue
    {
        int value;
    };

    /// @brief A discriminated union of all supported MDU property value types.
    using Value = std::variant<
        std::string,
        int,
        double,
        bool,
        std::filesystem::path,
        EnumValue,
        std::chrono::system_clock::time_point,
        std::vector<std::string>,
        std::vector<std::filesystem::path>,
        std::vector<double>
    >;

    /// @brief Schema definition for a single property within an MDU section.
    struct PropertySchema
    {
        std::string key; ///< Case-insensitive property key as it appears in the MDU file.
        bool required; ///< Whether the property must be present in the MDU file.
        bool nullable; ///< Whether the property may hold an explicit null (empty) value.
        ValueType value_type; ///< Expected type of the property value.
        std::string default_value; ///< Default value as a raw string, as it appears in the MDU file.
        std::map<int, std::string> enum_values; ///< Mapping from integer to name for Enum and IntEnum types.
        std::string description; ///< Human-readable description of the property.
    };

    /// @brief Schema definition for a single section within an MDU file.
    struct SectionSchema
    {
        std::string name; ///< Case-insensitive section name as it appears in the MDU file.
        bool required; ///< Whether this section must be present in the MDU file.
        std::string description; ///< Human-readable description of the section.
        std::vector<PropertySchema> properties; ///< Ordered list of property schemas within this section.
    };

    /// @brief Top-level schema definition for an MDU file.
    ///
    /// Contains the ordered list of sections and their properties. Sections and
    /// properties are listed in the order they should appear in the MDU file.
    struct MduSchema
    {
        std::string description; ///< Human-readable description of the MDU file format.
        std::vector<SectionSchema> sections; ///< Ordered list of section schemas.
    };

    /// @brief Returns the global MDU schema instance, generated from mdu.json.
    const MduSchema& GetMduSchema();

} // namespace dflowfm_io

#define MDU_SCHEMA GetMduSchema()