#include <dflowfm_io/MduDataConverter.h>
#include <dflowfm_io/MduValueConverter.h>
#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/MduValidator.h>
#include <dflowfm_io/StringUtils.h>

#include <ini/IniData.h>

#include <format>

namespace dflowfm_io
{

    static const ini::IniProperty* FindProperty(
        const ini::IniData& iniData,
        const std::string& sectionName,
        const std::string& propertyKey)
    {
        if (!iniData.HasSection(sectionName)) return nullptr;
        const auto& section = iniData.GetSection(sectionName);

        if (!section.HasProperty(propertyKey)) return nullptr;
        return &section.GetProperty(propertyKey);
    }

    static std::string GetCurrentTimeString()
    {
        const auto now = std::chrono::system_clock::now();
        const auto nowSeconds = std::chrono::floor<std::chrono::seconds>(now);
        const auto time = std::chrono::zoned_time{std::chrono::current_zone(), nowSeconds};

        return std::format("{:%H:%M:%S, %d-%m-%Y}", time);
    }

    std::pair<MduData, IssueReport> MduDataConverter::Convert(const ini::IniData& iniData)
    {
        MduValidator validator;
        IssueReport report = validator.Validate(iniData);

        MduData mduData = MduData::CreateFromSchema();

        for (const auto& sectionSchema : MDU_SCHEMA.sections)
        {
            for (const auto& propertySchema : sectionSchema.properties)
            {
                const auto* iniProperty = FindProperty(iniData, sectionSchema.name, propertySchema.key);
                const std::string key = FormatKey(sectionSchema.name, propertySchema.key);

                if (!iniProperty || !iniProperty->HasValue()) continue;

                auto converted_value = MduValueConverter::FromString(propertySchema, iniProperty->GetValue());
                if (!converted_value.has_value())
                {
                    report.AddError(iniProperty->GetLineNumber(), "Property [{}].{} contains invalid value: \"{}\".",
                                    sectionSchema.name, propertySchema.key, iniProperty->GetValue());
                    continue;
                }

                mduData.data_entries[key] = std::move(*converted_value);
            }
        }

        return {std::move(mduData), std::move(report)};
    }

    ini::IniData MduDataConverter::Convert(const MduData& mduData)
    {
        ini::IniData iniData;

        for (const auto& sectionSchema : MDU_SCHEMA.sections)
        {
            auto& iniSection = iniData.AddSection(sectionSchema.name);

            if (iniData.size() == 1)
            {
                iniSection.AddComment(std::format("Generated on {}\n", GetCurrentTimeString()));
            }

            for (const auto& propertySchema : sectionSchema.properties)
            {
                const std::string key = FormatKey(sectionSchema.name, propertySchema.key);
                if (!mduData.hasValue(key))
                {
                    continue;
                }

                auto value = MduValueConverter::ToString(propertySchema, mduData.data_entries.at(key));

                ini::IniProperty property(propertySchema.key, std::move(value), propertySchema.description);
                iniSection.AddProperty(std::move(property));
            }
        }

        return std::move(iniData);
    }

} // namespace dflowfm_io