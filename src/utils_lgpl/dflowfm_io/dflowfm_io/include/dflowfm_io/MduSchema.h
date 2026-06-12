#pragma once

#include <locale>
#include <string>
#include <variant>
#include <vector>

#include <dflowfm_io/StringUtils.h>

namespace dflowfm_io
{
    enum class ValueType
    {
        Path,
        String,
        Integer,
        IntBool,
        FloatingPoint,
        DateTime,
        StringList,
        PathList,
        FloatingPointList,
    };

    struct PropertySchema
    {
        bool HasDefault() const { return !default_value.empty(); }

        std::string key;
        bool required;
        ValueType value_type;
        std::string default_value;
        std::string description;
    };

    struct SectionSchema
    {
        const PropertySchema* FindProperty(const std::string& key) const
        {
            for (const auto& ps : properties)
            {
                if (iequals(ps.key, key)) return &ps;
            }
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
            {
                if (iequals(ss.name, name)) return &ss;
            }            
            return nullptr;
        }

        std::string description;
        std::vector<SectionSchema> sections;
    };

} // namespace dflowfm_io