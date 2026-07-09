#pragma once

#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/StringUtils.h>

#include <string>
#include <unordered_map>

namespace dflowfm_io
{
    /// @brief Fast lookup index over a @ref MduSchema instance.
    class MduSchemaIndex
    {
    public:
        /// @brief Constructs an @ref MduSchemaIndex from a @ref MduSchema instance.
        /// @param schema The schema to build the index from. Must outlive this index.
        explicit MduSchemaIndex(const MduSchema& schema)
        {
            for (const auto& ss : schema.sections)
            {
                section_map.emplace(tolower(ss.name), &ss);
                for (const auto& ps : ss.properties)
                    property_map.emplace(FormatKey(ss.name, ps.key), &ps);
            }
        }

        /// @brief Finds a section schema by name (case-insensitive).
        /// @param name The section name to look up.
        /// @return Pointer to the matching SectionSchema, or nullptr if not found.
        const SectionSchema* FindSection(const std::string& name) const
        {
            auto it = section_map.find(tolower(name));
            return it != section_map.end() ? it->second : nullptr;
        }

        /// @brief Finds a property schema by its fully qualified "section.property" key (case-insensitive).
        /// @param key The dot-separated key in the form "sectionName.propertyKey".
        /// @return Pointer to the matching PropertySchema, or nullptr if not found.
        const PropertySchema* FindProperty(const std::string& key) const
        {
            auto it = property_map.find(tolower(key));
            return it != property_map.end() ? it->second : nullptr;
        }

        /// @brief Finds a property schema by section name and property key (case-insensitive).
        /// @param section The section name to look up.
        /// @param property The property key to look up.
        /// @return Pointer to the matching PropertySchema, or nullptr if not found.
        const PropertySchema* FindProperty(const std::string& section, const std::string& property) const
        {
            return FindProperty(FormatKey(section, property));
        }

    private:
        std::unordered_map<std::string, const SectionSchema*> section_map;
        std::unordered_map<std::string, const PropertySchema*> property_map;
    };

    /// @brief Returns global MDU schema index, built from @ref MDU_SCHEMA.
    const MduSchemaIndex& GetMduSchemaIndex();

} // namespace dflowfm_io

#define MDU_SCHEMA_INDEX GetMduSchemaIndex()