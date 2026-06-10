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
        StringList,
        PathList
    };

    struct PropertySchema
    {
        bool HasDefault() const { return !default_value.empty(); }

        std::string key;
        bool required;
        ValueType value_type;
        std::string default_value;
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

        std::vector<SectionSchema> sections;
    };

    const static MduSchema MDU_SCHEMA {
        {
            SectionSchema {
                "General", true, {
                    { "Program", false, ValueType::String, "D-Flow FM" },
                    { "fileVersion", true, ValueType::String },
                }
            },
            SectionSchema {
                "geometry", true, {
                    { "netFile", true, ValueType::Path },
                    { "useCaching", false, ValueType::IntBool, "1" },
                    {"kmx", false, ValueType::Integer, "0"},
                    {"waterLevIni", false, ValueType::FloatingPoint, "0"},
                    {"dryPointsFile", false, ValueType::PathList, ""}
                }
            },
            SectionSchema {
                "numerics", true, {
                    { "cflMax", false, ValueType::FloatingPoint, "0.7" },
                }
            }
        }
    };

} // namespace dflowfm_io