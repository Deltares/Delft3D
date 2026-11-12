#include <dflowfm_io/MduConverter.h>
#include <dflowfm_io/MduDocument.h>

#include <ini/IniFileIO.h>
#include <ini/IniParserOptions.h>

#include <format>
#include <stdexcept>

namespace dflowfm_io
{
    static const ini::IniFileIOOptions mduIniOptions = {
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
        const ini::IniData iniData = ini::file_io::LoadString(text, mduIniOptions);

        std::pair<MduData, IssueReport> result = MduConverter::Convert(iniData, schema);
        mduData = std::move(result.first);
        return std::move(result.second);
    }

    IssueReport MduDocument::Load(const std::filesystem::path& path)
    {
        const ini::IniData iniData = ini::file_io::Load(path, mduIniOptions);

        std::pair<MduData, IssueReport> result = MduConverter::Convert(iniData, schema);
        mduData = std::move(result.first);
        return std::move(result.second);
    }

    std::string MduDocument::ToString() const
    {
        return ini::file_io::ToString(MduConverter::Convert(mduData, schema), mduIniOptions);
    }

    void MduDocument::Save(const std::filesystem::path& path) const
    {
        ini::file_io::Save(path, MduConverter::Convert(mduData, schema), mduIniOptions);
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