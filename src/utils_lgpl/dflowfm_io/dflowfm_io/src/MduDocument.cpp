#include <dflowfm_io/MduDocument.h>

#include "MduConversion.h"

#include <ini/IniFile.h>
#include <ini/IniFormatter.h>
#include <ini/IniOptions.h>
#include <ini/IniParser.h>

#include <format>
#include <stdexcept>
#include <variant>

namespace dflowfm_io
{
    namespace
    {
        const ini::IniFileOptions mduIniOptions = {
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

        void EnsureEnumInRange(const MduSchema& schema, const std::string& key, const IntEnumValue& value)
        {
            const auto* ps = schema.FindProperty(key);
            if (ps && !schema.FindEnumValue(*ps, std::to_string(value.value)))
                throw std::out_of_range(
                    std::format("Enum value '{}' is out of range for '{}'.", value.value, key));
        }

        void EnsureEnumInRange(const MduSchema& schema, const std::string& key, const StringEnumValue& value)
        {
            const auto* ps = schema.FindProperty(key);
            if (ps && !schema.FindEnumValue(*ps, value.value))
                throw std::out_of_range(
                    std::format("Enum value '{}' is out of range for '{}'.", value.value, key));
        }
    } // namespace

    MduDocument::MduDocument(const MduSchema& schema)
        : schema(schema), mduData(schema.CreateDefaultValues()) {}

    IssueReport MduDocument::LoadFromString(const std::string& text)
    {
        const ini::IniData iniData = ini::Parse(text, mduIniOptions.scheme, mduIniOptions.parserOptions);
        std::pair<MduData, IssueReport> result = conversion::ToMduData(iniData, schema);
        mduData = std::move(result.first);
        return std::move(result.second);
    }

    IssueReport MduDocument::LoadFromFile(const std::filesystem::path& path)
    {
        const ini::IniData iniData = ini::Load(path, mduIniOptions);
        std::pair<MduData, IssueReport> result = conversion::ToMduData(iniData, schema);
        mduData = std::move(result.first);
        return std::move(result.second);
    }

    std::string MduDocument::SaveToString() const
    {
        const ini::IniData iniData = conversion::ToIniData(mduData, schema);
        return ini::Format(iniData, mduIniOptions.scheme, mduIniOptions.formatterOptions);
    }

    void MduDocument::SaveToFile(const std::filesystem::path& path) const
    {
        const ini::IniData iniData = conversion::ToIniData(mduData, schema);
        ini::Save(path, iniData, mduIniOptions);
    }

    void MduDocument::EnsureKeyInSchema(const std::string& key) const
    {
        if (!schema.FindProperty(key))
            throw std::invalid_argument(
                std::format("Unknown MDU property: '{}'.", key));
    }

    void MduDocument::EnsureValueInRange(const std::string& key, const Value& value) const
    {
        if (const auto* intEnum = std::get_if<IntEnumValue>(&value))
            EnsureEnumInRange(schema, key, *intEnum);
        else if (const auto* stringEnum = std::get_if<StringEnumValue>(&value))
            EnsureEnumInRange(schema, key, *stringEnum);
    }

} // namespace dflowfm_io