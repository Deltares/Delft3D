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

    const EnumValueSchema* MduSchema::FindEnumValue(const PropertySchema& propertySchema, const std::string& rawValue) const
    {
        if (propertySchema.value_type != ValueType::Enum && propertySchema.value_type != ValueType::IntEnum)
            return nullptr;

        for (const auto& enumValueSchema : propertySchema.enum_values)
        {
            const std::string enumValue = propertySchema.value_type == ValueType::IntEnum
                                              ? std::to_string(enumValueSchema.value)
                                              : enumValueSchema.label;

            if (rawValue == enumValue) return &enumValueSchema;
        }

        return nullptr;
    }

    bool MduSchema::IsObsolete(const PropertySchema& propertySchema, const std::string& rawValue) const
    {
        if (propertySchema.status.type == StatusType::Obsolete) return true;

        const auto* enumValueSchema = FindEnumValue(propertySchema, rawValue);
        return enumValueSchema && enumValueSchema->status.type == StatusType::Obsolete;
    }

    const MduSchema& GetMduSchema()
    {
        static const MduSchema instance = BuildMduSchema();
        return instance;
    }

} // namespace dflowfm_io