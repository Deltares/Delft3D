#include <dflowfm_io/MduData.h>
#include <dflowfm_io/MduValueConverter.h>
#include <dflowfm_io/StringUtils.h>

#include <format>
#include <stdexcept>

namespace dflowfm_io
{
    MduData MduData::CreateFromSchema(const MduSchema& schema)
    {
        decltype(data_entries) entries;

        for (const auto& sectionSchema : schema.Sections())
        {
            if (sectionSchema.status.type == StatusType::Obsolete) continue;

            for (const auto& propertySchema : sectionSchema.properties)
            {
                if (propertySchema.default_value.empty()) continue;

                if (schema.IsObsolete(propertySchema, propertySchema.default_value)) continue;

                std::optional<Value> value =
                    MduValueConverter::FromString(propertySchema, propertySchema.default_value);

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