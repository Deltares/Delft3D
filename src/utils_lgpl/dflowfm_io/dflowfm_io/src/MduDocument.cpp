#include <dflowfm_io/MduConverter.h>
#include <dflowfm_io/MduDocument.h>

#include <ini/IniFile.h>
#include <ini/IniParserOptions.h>

#include <format>
#include <stdexcept>

namespace dflowfm_io
{
    static const ini::IniFileOptions mduIniOptions = {
        .parserOptions =
            {
                .allowDuplicateSections = false,
                .allowDuplicateProperties = false,
                .allowMultiLineValues = true,
            },
        .formatterOptions =
            {
                .propertyKeyWidth = 42,
                .propertyValueWidth = 20,
                .writeEmptySections = false
            },
    };

    MduDocument::MduDocument(const MduSchema& schema)
        : schema(schema), mduData(schema.CreateDefaultValues()) {}

    IssueReport MduDocument::LoadString(const std::string& text)
    {
        ini::IniFile iniFile{mduIniOptions};
        iniFile.LoadString(text);
        ini::IniData& iniData = iniFile.GetData();

        std::pair<MduData, IssueReport> result = MduConverter::Convert(iniData, schema);
        mduData = std::move(result.first);
        return std::move(result.second);
    }

    IssueReport MduDocument::Load(const std::filesystem::path& path)
    {
        ini::IniFile iniFile{mduIniOptions};
        iniFile.Load(path);

        std::pair<MduData, IssueReport> result = MduConverter::Convert(iniFile.GetData(), schema);
        mduData = std::move(result.first);
        return std::move(result.second);
    }

    std::string MduDocument::ToString() const
    {
        ini::IniData iniData = MduConverter::Convert(mduData, schema);

        ini::IniFile iniFile{mduIniOptions};
        iniFile.SetData(iniData);
        return iniFile.ToString();
    }

    void MduDocument::Save(const std::filesystem::path& path) const
    {
        ini::IniFile iniFile{mduIniOptions};
        iniFile.SetData(MduConverter::Convert(mduData, schema));
        iniFile.Save(path);
    }

    void MduDocument::EnsureKeyInSchema(const std::string& key) const
    {
        if (!schema.FindProperty(key))
            throw std::invalid_argument(
                std::format("Unknown MDU property: '{}'.", key));
    }

    void MduDocument::EnsureEnumInRange(const std::string& key, const IntEnumValue& value) const
    {
        const auto* ps = schema.FindProperty(key);
        if (ps && !schema.FindEnumValue(*ps, std::to_string(value.value)))
            throw std::out_of_range(
                std::format("Enum value '{}' is out of range for '{}'.", value.value, key));
    }

    void MduDocument::EnsureEnumInRange(const std::string& key, const StringEnumValue& value) const
    {
        const auto* ps = schema.FindProperty(key);
        if (ps && !schema.FindEnumValue(*ps, value.value))
            throw std::out_of_range(
                std::format("Enum value '{}' is out of range for '{}'.", value.value, key));
    }

} // namespace dflowfm_io