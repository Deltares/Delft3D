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

    const static MduSchema MDU_SCHEMA {
        "The master definition file of D-Flow FM",
        {
            SectionSchema {
                "general", true, "This section contains the program name and its version.", {
                    { "program", false, ValueType::String, "D-Flow FM", "Program." },
                    { "fileVersion", true, ValueType::String, "File version. Do not edit this." },
                }
            },
            SectionSchema {
                "geometry", true, "In this section, the main entry comprises the specification of the grid (i.e. the netCDF network file). In addition, thin dams and thin dykes can be specified.", {
                    { "netFile", true, ValueType::Path, "", "Net file (*_net.nc) containing mesh information." },
                    { "useCaching", false, ValueType::IntBool, "1", "Use caching for geometrical/network-related items." },
                    { "kmx", false, ValueType::Integer, "0", "Number of vertical layers. NB. If keyword `zLayerGrowthFactor` is used, then number of layers is determined by D-Flow FM." },
                    { "waterLevIni", false, ValueType::FloatingPoint, "0", "Initial water levels sample file (*.xyz)." },
                    { "dryPointsFile", false, ValueType::PathList, "", "Dry points file (*.xyz), third column dummy z values, or polygon file (*.pol)." }
                }
            },
            SectionSchema {
                "numerics", true, "This section contains the settings of specific parts of the flow solver, such as limiters and the iterative solver type.", {
                    { "cflMax", false, ValueType::FloatingPoint, "0.7", "Maximum Courant nr." },
                }
            }
        }
    };

} // namespace dflowfm_io