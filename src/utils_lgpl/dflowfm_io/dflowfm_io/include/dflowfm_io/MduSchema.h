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

    using Value = std::variant<
        std::string,
        int,
        double,
        bool,
        std::filesystem::path,
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
        std::map<std::string, std::string> enum_values;
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

        std::string description;
        std::vector<SectionSchema> sections;
    };

} // namespace dflowfm_io