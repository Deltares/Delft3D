#pragma once

#include <locale>
#include <string>
#include <variant>
#include <vector>

#include <dflowfm_io/StringUtils.h>

namespace dflowfm_io
{

    struct PropertySchema
    {
        bool HasDefault() const { return !default_value.empty(); }

        std::string key;
        bool required;
        std::string default_value;
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

        std::vector<SectionSchema> sections;
    };

    inline MduSchema BuildMduSchema()
    {
        return MduSchema {
            {
                SectionSchema {
                    "General", true, {
                        { "Program", false, "D-Flow FM" },
                        { "fileVersion", true },
                    }
                },
                SectionSchema {
                    "geometry", true, {
                        { "netFile", true },
                        { "useCaching", false, "1" },
                        {"kmx", false, "0"},
                        {"waterLevIni", false, "0"},
                        {"dryPointsFile", false, ""}
                    }
                },
                SectionSchema {
                    "numerics", true, {
                        { "cflMax", false, "0.7" },
                    }
                }
            },
        };
    }

} // namespace dflowfm_io