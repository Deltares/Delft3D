#include <dflowfm_io/MduDocument.h>
#include <dflowfm_io/MduDataConverter.h>

#include <ini/IniFile.h>
#include <ini/IniParserOptions.h>

#include <iostream>
#include <format>
#include <fstream>
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
        .formatterOptions
            {
                .propertyKeyWidth = 42,
                .propertyValueWidth = 20,
                .writeEmptySections = false
            },
    };

    MduDocument::MduDocument() : mduData(MduData::CreateFromSchema()) {}

    void MduDocument::Load(std::istream& in)
    {
        if (in.fail())
            throw std::ios_base::failure("Stream is not in a readable state.");

        ini::IniFile iniFile{mduIniOptions};
        iniFile.Load(in);
        ini::IniData& iniData = iniFile.GetData();

        std::pair<MduData, IssueReport> result = MduDataConverter::Convert(iniData);
        mduData = std::move(result.first);
        issues = std::move(result.second);
    }

    void MduDocument::Load(const std::filesystem::path& path)
    {
        if (path.empty())
            throw std::invalid_argument("Path must not be empty.");

        std::ifstream stream(path);
        if (!stream.is_open())
            throw std::ios_base::failure("Failed to open file for reading: " + path.string());

        Load(stream);
    }

    void MduDocument::Save(std::ostream& out) const
    {
        if (out.fail())
            throw std::ios_base::failure("Stream is not in a writable state.");

        ini::IniData iniData = MduDataConverter::Convert(mduData);

        ini::IniFile iniFile{mduIniOptions};
        iniFile.SetData(iniData);
        iniFile.Save(out);
    }

    void MduDocument::Save(const std::filesystem::path& path) const
    {
        if (path.empty())
            throw std::invalid_argument("Path must not be empty.");

        std::ofstream stream(path);
        if (!stream.is_open())
            throw std::ios_base::failure("Failed to open file for writing: " + path.string());

        Save(stream);
    }

    void MduDocument::EnsureKnownKey(const std::string& key) const
    {
        if (!MDU_SCHEMA.FindProperty(key))
            throw std::invalid_argument(
                std::format("Unknown MDU property: '{}'.", key));
    }

    void MduDocument::EnsureEnumInRange(const std::string& key, EnumValue value) const
    {
        const auto* ps = MDU_SCHEMA.FindProperty(key);
        if (!ps) return;
        if (ps->enum_values.find(value.value) == ps->enum_values.end())
            throw std::out_of_range(
                std::format("Enum value {} is out of range for '{}'.", value.value, key));
    }

} // namespace dflowfm_io