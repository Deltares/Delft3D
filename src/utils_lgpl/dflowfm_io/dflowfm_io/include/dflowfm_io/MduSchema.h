#pragma once

#include <dflowfm_io/StringUtils.h>

#include <chrono>
#include <filesystem>
#include <locale>
#include <map>
#include <string>
#include <unordered_map>
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

    /// @brief Represents a single value within an enumeration property.
    struct EnumValue
    {
        int value; ///< The integer value.
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

    /// @brief Describes the lifecycle status of a property in the MDU schema.
    enum class StatusType
    {
        GA, ///< Generally Available; stable and supported.
        Research, ///< Experimental feature; may change without notice.
        Deprecated, ///< Discouraged; still functional but scheduled for removal.
        Obsolete ///< No longer supported; may be ignored or cause errors.
    };

    /// @brief Lifecycle status of a property or enum value.
    struct Status
    {
        StatusType type = StatusType::GA; ///< The lifecycle status of the property or enum value.
        std::string comment; ///< Explanation for Deprecated and Obsolete status types.
    };

    /// @brief Schema metadata for a single value within an enumeration property.
    struct EnumValueSchema
    {
        int value; ///< Integer index (Enum) or integer value (IntEnum).
        std::string label; ///< String label for Enum types. Empty for IntEnum types.
        Status status; ///< Lifecycle status of this enum value.
    };

    /// @brief Schema definition for a single property within an MDU section.
    struct PropertySchema
    {
        std::string key; ///< Case-insensitive property key as it appears in the MDU file.
        bool required; ///< Whether the property must be present in the MDU file.
        bool nullable; ///< Whether the property may hold an explicit null (empty) value.
        ValueType value_type; ///< Expected type of the property value.
        std::string default_value; ///< Default value as a raw string, as it appears in the MDU file.
        std::vector<EnumValueSchema> enum_values; ///< Ordered list of enum value schemas for Enum and IntEnum types.
        std::string format; ///< Optional format string for DateTime and Float properties.
        std::string description; ///< Human-readable description of the property.
        Status status; ///< Describes the lifecycle status of the property.
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
    class MduSchema
    {
    public:
        /// @brief Constructs an @ref MduSchema.
        /// @param description The overall schema description.
        /// @param sections The sections that make up the schema.
        MduSchema(std::string description, std::vector<SectionSchema> sections);

        /// @brief Non-copyable and non-movable.
        MduSchema(const MduSchema&) = delete;
        MduSchema& operator=(const MduSchema&) = delete;
        MduSchema(MduSchema&&) = delete;
        MduSchema& operator=(MduSchema&&) = delete;

        /// @brief Returns the overall description of the schema.
        const std::string& Description() const { return description; };

        /// @brief Returns all sections in the schema.
        const std::vector<SectionSchema>& Sections() const { return sections; };

        /// @brief Finds a section schema by name (case-insensitive).
        /// @param name The section name to look up.
        /// @return The matching @ref SectionSchema, or nullptr if not found.
        const SectionSchema* FindSection(const std::string& name) const;

        /// @brief Finds a property schema by its fully qualified key (case-insensitive).
        /// @param key Fully qualified property key in the form "section.property".
        /// @return The matching @ref PropertySchema, or nullptr if not found.
        const PropertySchema* FindProperty(const std::string& key) const;

        /// @brief Finds a property schema by section name and property key (case-insensitive).
        /// @param section The section name to look up.
        /// @param property The property key to look up.
        /// @return The matching @ref PropertySchema, or nullptr if not found.
        const PropertySchema* FindProperty(const std::string& section, const std::string& property) const;

    private:
        std::string description;
        std::vector<SectionSchema> sections;
        std::unordered_map<std::string, const SectionSchema*> section_map;
        std::unordered_map<std::string, const PropertySchema*> property_map;
    };

    /// @brief Returns the global MDU schema instance, generated from mdu.json.
    const MduSchema& GetMduSchema();

} // namespace dflowfm_io

#define MDU_SCHEMA GetMduSchema()