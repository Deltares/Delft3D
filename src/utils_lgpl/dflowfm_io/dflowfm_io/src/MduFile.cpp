#include <dflowfm_io/MduFile.h>
#include <dflowfm_io/MduConverter.h>

#include "ini/IniFile.h"
#include "ini/IniParserOptions.h"

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
    };

    struct MduFile::Impl
    {
        IniFile iniFile{mduIniOptions};
        MduData mduData;
    };

    MduFile::MduFile() : impl_(std::make_unique<Impl>()) {}

    MduFile::~MduFile() = default;

    MduFile::MduFile(MduFile&&) noexcept = default;

    MduFile& MduFile::operator=(MduFile&&) noexcept = default;

    MduFile MduFile::LoadFrom(std::istream& in)
    {
        MduFile file;
        file.Load(in);

        return file;
    }

    MduFile MduFile::LoadFrom(const std::filesystem::path& path)
    {
        MduFile file;
        file.Load(path);

        return file;
    }

    void MduFile::Load(std::istream& in)
    {
        if (in.fail())
        {
            throw std::ios_base::failure("Stream is not in a readable state.");
        }

        impl_->iniFile.Load(in);
        IniData& iniData = impl_->iniFile.GetData();

        ConversionResult<MduData> result = MduConverter::Convert(iniData);

        if (result.HasIssues())
        {
            std::cout << result.FormatIssues();
        }

        if (!result.IsValid())
        {
            throw std::runtime_error("Failed to load MDU file.");
        }

        impl_->mduData = std::move(result.value);
    }

    void MduFile::Load(const std::filesystem::path& path)
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

        Load(stream);
    }

    void MduFile::Save(std::ostream& out)
    {
        if (out.fail())
        {
            throw std::ios_base::failure("Stream is not in a writable state.");
        }

        ConversionResult<IniData> result = MduConverter::Convert(impl_->mduData);

        if (result.HasIssues())
        {
            std::cout << result.FormatIssues();
        }

        if (!result.IsValid())
        {
            throw std::runtime_error("Failed to save MDU file.");
        }

        impl_->iniFile.SetData(result.value);
        impl_->iniFile.Save(out);
    }

    void MduFile::Save(const std::filesystem::path& path)
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

        Save(stream);
    }

    MduData& MduFile::GetData() { return impl_->mduData; }
    const MduData& MduFile::GetData() const { return impl_->mduData; }

    void MduFile::SetData(MduData data) { impl_->mduData = std::move(data); }

} // namespace dflowfm_io