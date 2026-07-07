#pragma once

#include <chrono>
#include <filesystem>
#include <locale>
#include <map>
#include <string>
#include <variant>
#include <vector>

#include <dflowfm_io/StringUtils.h>

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
        /// @brief Finds a property schema by key (case-insensitive).
        /// @param key The property key to look up.
        /// @return Pointer to the matching PropertySchema, or nullptr if not found.
        const PropertySchema* FindProperty(const std::string& key) const
        {
            for (const auto& ps : properties)
                if (iequals(ps.key, key)) return &ps;
            return nullptr;
        }

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
        /// @brief Finds a section schema by name (case-insensitive).
        /// @param name The section name to look up.
        /// @return Pointer to the matching SectionSchema, or nullptr if not found.
        const SectionSchema* FindSection(const std::string& name) const
        {
            for (const auto& ss : sections)
                if (iequals(ss.name, name)) return &ss;
            return nullptr;
        }

        /// @brief Finds a property schema by its fully qualified "section.property" key (case-insensitive).
        /// @param key The dot-separated key in the form "sectionName.propertyKey".
        /// @return Pointer to the matching PropertySchema, or nullptr if not found.
        const PropertySchema* FindProperty(const std::string& key) const
        {
            auto dot = key.find('.');
            if (dot == std::string::npos) return nullptr;

            const auto* ss = FindSection(key.substr(0, dot));
            if (!ss) return nullptr;
            return ss->FindProperty(key.substr(dot + 1));
        }

        std::string description; ///< Human-readable description of the MDU file format.
        std::vector<SectionSchema> sections; ///< Ordered list of section schemas.
    };

    /// @brief Formats a fully qualified property key from a section name and property key.
    /// @param section The section name.
    /// @param property The property key.
    /// @return A lowercase dot-separated string in the form "sectionname.propertykey".
    inline std::string FormatKey(const std::string& section, const std::string& property)
    {
        return tolower(section + "." + property);
    }

    /// @brief The global MDU schema instance, generated from mdu.json.
    extern const MduSchema MDU_SCHEMA;

} // namespace dflowfm_io