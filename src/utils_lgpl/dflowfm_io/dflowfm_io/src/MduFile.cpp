#include <dflowfm_io/MduFile.h>
#include <dflowfm_io/MduConverter.h>

#include <ini/IniFile.h>
#include <ini/IniParserOptions.h>

#include <iostream>
#include <fstream>
#include <stdexcept>

using namespace ini;

namespace dflowfm_io
{
    static const ini::IniFileOptions mduIniOptions = {
        .parserOptions =
            {
                .allowDuplicateSections = false,
                .allowDuplicateProperties = false,
                .allowMultiLineValues = true,
            },
        .formatterOptions{.writeEmptySections = false},
    };

    MduData MduFile::Load(std::istream& in)
    {
        if (in.fail())
        {
            throw std::ios_base::failure("Stream is not in a readable state.");
        }

        IniFile iniFile{mduIniOptions};
        iniFile.Load(in);
        IniData& iniData = iniFile.GetData();

        ConversionResult<MduData> result = MduConverter::Convert(iniData);

        if (result.HasIssues())
        {
            std::cout << result.FormatIssues();
        }

        if (!result.IsValid())
        {
            throw std::runtime_error("Failed to load MDU file.");
        }

        return std::move(result.value);
    }

    MduData MduFile::Load(const std::filesystem::path& path)
    {
        if (path.empty())
        {
            throw std::invalid_argument("Path must not be empty.");
        }

        std::ifstream stream(path);
        if (!stream.is_open())
        {
            throw std::ios_base::failure("Failed to open file for reading: " + path.string());
        }

        return Load(stream);
    }

    void MduFile::Save(std::ostream& out, const MduData& data)
    {
        if (out.fail())
        {
            throw std::ios_base::failure("Stream is not in a writable state.");
        }

        ConversionResult<IniData> result = MduConverter::Convert(data);

        if (result.HasIssues())
        {
            std::cout << result.FormatIssues();
        }

        if (!result.IsValid())
        {
            throw std::runtime_error("Failed to save MDU file.");
        }

        IniFile iniFile{mduIniOptions};
        iniFile.SetData(result.value);
        iniFile.Save(out);
    }

    void MduFile::Save(const std::filesystem::path& path, const MduData& data)
    {
        if (path.empty())
        {
            throw std::invalid_argument("Path must not be empty.");
        }

        std::ofstream stream(path);
        if (!stream.is_open())
        {
            throw std::ios_base::failure("Failed to open file for writing: " + path.string());
        }

        Save(stream, data);
    }

} // namespace dflowfm_io