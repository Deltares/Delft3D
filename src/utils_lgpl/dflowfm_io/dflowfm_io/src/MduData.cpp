#include <dflowfm_io/MduData.h>
#include <dflowfm_io/MduValueConverter.h>
#include <dflowfm_io/StringUtils.h>

#include <format>
#include <stdexcept>

namespace dflowfm_io
{
    MduData MduData::CreateFromSchema()
    {
        MduData mduData;

        for (const auto& sectionSchema : MDU_SCHEMA.Sections())
        {
            for (const auto& propertySchema : sectionSchema.properties)
            {
                if (propertySchema.default_value.empty()) continue;

                std::optional<Value> value =
                    MduValueConverter::FromString(propertySchema, propertySchema.default_value);

                if (!value.has_value())
                {
                    throw std::logic_error(std::format(
                        "Invalid default value \"{}\" for property [{}].{}.",
                        propertySchema.default_value, sectionSchema.name, propertySchema.key));
                }

                const std::string key = FormatKey(sectionSchema.name, propertySchema.key);
                mduData.data_entries[key] = std::move(*value);
            }
        }

        return mduData;
    }

} // namespace dflowfm_io