#include <iostream>

#include <dflowfm_io/MduFile.h>
#include <dflowfm_io/MduConverter.h>

namespace dflowfm_io
{

    MduFile::MduFile(std::filesystem::path path) : iniFile(ini::IniFile(std::move(path))) {}

    MduFile MduFile::LoadFrom(std::filesystem::path path)
    {
        MduFile file(std::move(path));
        file.Load();

        return file;
    }

    void MduFile::Load()
    {
        iniFile.Load();
        ini::IniData& iniData = iniFile.GetData();

        MduConverter mduConverter;
        ConversionResult<MduModel> result = mduConverter.ToModel(iniData);

        if (result.HasIssues())
        {
            std::cout << result.FormatIssues();
        }

        if (!result.IsValid())
        {
            throw std::runtime_error("Failed to read MDU file: " + iniFile.GetPath().string());
        }

        model = std::move(result.value);
    }

} // namespace dflowfm_io
