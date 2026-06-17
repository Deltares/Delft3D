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

    // Wrapper for enum values so they can be distinguished from regular integers in the Value variant.
    struct EnumValue
    {
        int value;
    };

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

    struct PropertySchema
    {
        std::string key;
        bool required;
        ValueType value_type;
        std::optional<Value> default_value;
        std::string default_value_str;
        std::map<int, std::string> enum_values;
        std::string description;
    };

    struct SectionSchema
    {
        const PropertySchema* FindProperty(const std::string& key) const
        {
            for (const auto& ps : properties)
                if (iequals(ps.key, key)) return &ps;
            return nullptr;
        }

        std::string name;
        bool required;
        std::string description;
        std::vector<PropertySchema> properties;
    };

    struct MduSchema
    {
        const SectionSchema* FindSection(const std::string& name) const
        {
            for (const auto& ss : sections)
                if (iequals(ss.name, name)) return &ss;
            return nullptr;
        }

        const PropertySchema* FindProperty(const std::string& key) const
        {
            auto dot = key.find('.');
            if (dot == std::string::npos) return nullptr;

            const auto* ss = FindSection(key.substr(0, dot));
            if (!ss) return nullptr;
            return ss->FindProperty(key.substr(dot + 1));
        }

        std::string description;
        std::vector<SectionSchema> sections;
    };

    inline std::string FormatKey(const std::string& section, const std::string& property)
    {
        return to_lowercase(section + "." + property);
    }

    extern const MduSchema MDU_SCHEMA;

} // namespace dflowfm_io