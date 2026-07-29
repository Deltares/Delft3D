#include <dflowfm_io/MduData.h>
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
                case ValueType::Enum:
                    return schema.enum_values.empty() ? "0" : schema.enum_values.front().label;
                case ValueType::IntEnum:
                    return schema.enum_values.empty() ? "0" : std::to_string(schema.enum_values.front().value);
                default:
                    throw std::logic_error(std::format("Unhandled ValueType for property '{}'.", schema.key));
            }
        }
    }


    MduData MduData::CreateFromSchema(const MduSchema& schema)
    {
        decltype(data_entries) entries;

        for (const auto& sectionSchema : schema.Sections())
        {
            if (sectionSchema.status.type == StatusType::Obsolete) continue;

            for (const auto& propertySchema : sectionSchema.properties)
            {
                if (schema.IsObsolete(propertySchema, propertySchema.default_value)) continue;

                const std::string default_value = propertySchema.default_value.empty() ? GetDummyDefault(propertySchema)
                                                                          : propertySchema.default_value;

                std::optional<Value> value =
                    MduValueConverter::FromString(propertySchema, default_value);

                if (!value.has_value())
                {
                    throw std::logic_error(std::format(
                        "Invalid default value \"{}\" for property [{}].{}.",
                        propertySchema.default_value, sectionSchema.name, propertySchema.key));
                }

                const std::string key = FormatKey(sectionSchema.name, propertySchema.key);
                entries[key] = std::move(*value);
            }
        }

        return CreateFromRawData(std::move(entries));
    }

    MduData MduData::CreateFromRawData(std::unordered_map<std::string, Value> raw_data)
    {
        MduData mduData;
        mduData.data_entries = std::move(raw_data);
        return mduData;
    }

} // namespace dflowfm_io