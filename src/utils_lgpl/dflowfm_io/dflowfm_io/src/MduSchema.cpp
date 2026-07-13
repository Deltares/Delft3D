#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/MduSchemaGenerated.h>
#include <dflowfm_io/StringUtils.h>

namespace dflowfm_io
{

    MduSchema::MduSchema(std::string description, std::vector<SectionSchema> sections)
        : description(std::move(description)), sections(std::move(sections))
    {
        for (const auto& ss : this->sections)
        {
            section_map.emplace(tolower(ss.name), &ss);
            for (const auto& ps : ss.properties)
                property_map.emplace(FormatKey(ss.name, ps.key), &ps);
        }
    }

    const SectionSchema* MduSchema::FindSection(const std::string& name) const
    {
        auto it = section_map.find(tolower(name));
        return it != section_map.end() ? it->second : nullptr;
    }

    const PropertySchema* MduSchema::FindProperty(const std::string& key) const
    {
        auto it = property_map.find(tolower(key));
        return it != property_map.end() ? it->second : nullptr;
    }

    const PropertySchema* MduSchema::FindProperty(const std::string& section, const std::string& property) const
    {
        return FindProperty(FormatKey(section, property));
    }

    const MduSchema& GetMduSchema()
    {
        static const MduSchema instance = BuildMduSchema();
        return instance;
    }

} // namespace dflowfm_io