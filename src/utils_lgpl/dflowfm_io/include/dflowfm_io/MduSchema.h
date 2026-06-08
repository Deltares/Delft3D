#pragma once

#include <locale>
#include <string>
#include <variant>
#include <vector>

#include <dflowfm_io/StringUtils.h>

namespace dflowfm_io
{
    enum class PropertyType
    {
        String,
        Int,
        Float,
        Double,
        Bool,
        Path
    };

    struct PropertySchema
    {
        std::string key;
        PropertyType type;
        bool required;
        std::string default_value;

        bool HasDefault() const { return !default_value.empty(); }
    };

    struct SectionSchema
    {
        std::string name;
        bool required;
        std::vector<PropertySchema> properties;

        const PropertySchema* FindProperty(const std::string& key) const
        {
            for (const auto& ps : properties)
                if (iequals(ps.key, key)) return &ps;
            return nullptr;
        }
    };

    struct MduSchema
    {
        std::vector<SectionSchema> sections;

        const SectionSchema* FindSection(const std::string& name) const
        {
            for (const auto& ss : sections)
                if (iequals(ss.name, name)) return &ss;
            return nullptr;
        }
    };

    inline MduSchema BuildMduSchema()
    {
        return MduSchema {
            {
                SectionSchema {
                    "General", true, {
                        { "Program", PropertyType::String, false, "D-Flow FM" },
                        { "fileVersion", PropertyType::String, true },
                    }
                },
                SectionSchema {
                    "geometry", true, {
                        { "netFile", PropertyType::Path, true },
                        { "useCaching", PropertyType::Bool, false, "1" },
                    }
                },
                SectionSchema {
                    "numerics", true, {
                        { "cflMax", PropertyType::Float, false, "0.7" },
                        { "kmx", PropertyType::Int, false, "0" }
                    }
                }
            },
        };
    }

} // namespace dflowfm_io