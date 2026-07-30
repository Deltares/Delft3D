#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/MduSchemaGenerated.h>
#include <dflowfm_io/MduValueConverter.h>
#include <dflowfm_io/StringUtils.h>

#include <format>
#include <stdexcept>

namespace dflowfm_io
{

    namespace
    {
        // TODO this is a temporary solution to provide default values for properties that do not have a default value
        // defined in the schema. Eventually every property should have a default value defined in the schema, and this
        // function can be removed.
        std::string GetDummyDefault(const PropertySchema& schema)
        {
            switch (schema.value_type)
            {
                case ValueType::String:
                    return "";
                case ValueType::Int:
                    return "0";
                case ValueType::Float:
                    return "0.0";
                case ValueType::IntBool:
                    return "false";
                case ValueType::Path:
                    return std::filesystem::path().string();
                case ValueType::DateTime:
                    return "20010101000000";
                case ValueType::StringList:
                    return "";
                case ValueType::PathList:
                    return "";
                case ValueType::FloatList:
                    return "";
                case ValueType::StringEnum:
                case ValueType::IntEnum:
                    return schema.enum_values.front().value;
                default:
                    throw std::logic_error(
                        std::format("Unhandled ValueType for property '{}'.", schema.key));
            }
        }
    } // namespace

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
        if (propertySchema.value_type != ValueType::StringEnum && propertySchema.value_type != ValueType::IntEnum)
            return nullptr;

        for (const auto& enumValueSchema : propertySchema.enum_values)
            if (iequals(rawValue, enumValueSchema.value))
                return &enumValueSchema;
                
        return nullptr;
    }

    bool MduSchema::IsObsolete(const PropertySchema& propertySchema, const std::string& rawValue) const
    {
        if (propertySchema.status.type == StatusType::Obsolete) return true;

        const auto* enumValueSchema = FindEnumValue(propertySchema, rawValue);
        return enumValueSchema && enumValueSchema->status.type == StatusType::Obsolete;
    }

    std::unordered_map<std::string, Value> MduSchema::CreateDefaultValues() const
    {
        std::unordered_map<std::string, Value> entries;

        for (const auto& sectionSchema : Sections())
        {
            if (sectionSchema.status.type == StatusType::Obsolete) continue;

            for (const auto& propertySchema : sectionSchema.properties)
            {
                if (IsObsolete(propertySchema, propertySchema.default_value)) continue;

                const std::string default_value = propertySchema.default_value.empty()
                                                       ? GetDummyDefault(propertySchema)
                                                       : propertySchema.default_value;

                Value value = MduValueConverter::FromString(propertySchema, default_value);
                const std::string key = FormatKey(sectionSchema.name, propertySchema.key);
                entries[key] = std::move(value);
            }
        }

        return entries;
    }

    const MduSchema& GetMduSchema()
    {
        static const MduSchema instance = BuildMduSchema();
        return instance;
    }

} // namespace dflowfm_io