#include <iostream>

#include <dflowfm_io/MduFile.h>
#include <dflowfm_io/MduConverter.h>

#include "ini/IniFile.h"

namespace dflowfm_io
{
    struct MduFile::Impl
    {
        ini::IniFile iniFile;

        explicit Impl(std::filesystem::path path) : iniFile(std::move(path)) {}
    };

    MduFile::MduFile(std::filesystem::path path) : impl_(std::make_unique<Impl>(std::move(path))) {}

    MduFile::~MduFile() = default;

    MduFile::MduFile(MduFile&&) noexcept = default;

    MduFile& MduFile::operator=(MduFile&&) noexcept = default;

    MduFile MduFile::LoadFrom(std::filesystem::path path)
    {
        MduFile file(std::move(path));
        file.Load();

        return file;
    }

    void MduFile::Load()
    {
        impl_->iniFile.Load();
        ini::IniData& iniData = impl_->iniFile.GetData();

        MduConverter mduConverter;
        ConversionResult<MduModel> result = mduConverter.ToModel(iniData);

        if (result.HasIssues())
        {
            std::cout << result.FormatIssues();
        }

        if (!result.IsValid())
        {
            throw std::runtime_error("Failed to read MDU file: " + impl_->iniFile.GetPath().string());
        }

        model = std::move(result.value);
    }

} // namespace dflowfm_io
