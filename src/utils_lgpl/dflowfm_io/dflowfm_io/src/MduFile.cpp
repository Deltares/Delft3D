#include <iostream>

#include <dflowfm_io/MduFile.h>
#include <dflowfm_io/MduConverter.h>

#include "ini/IniFile.h"
#include "ini/IniParserOptions.h"

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
        IniFile iniFile;
        MduData mduData;

        explicit Impl(std::filesystem::path path) : iniFile(std::move(path), mduIniOptions) {}
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
        IniData& iniData = impl_->iniFile.GetData();

        ConversionResult<MduData> result = MduConverter::Convert(iniData);

        if (result.HasIssues())
        {
            std::cout << result.FormatIssues();
        }

        if (!result.IsValid())
        {
            throw std::runtime_error("Failed to read MDU file: " + impl_->iniFile.GetPath().string());
        }

        impl_->mduData = std::move(result.value);
    }

    MduData& MduFile::GetData() { return impl_->mduData; }
    const MduData& MduFile::GetData() const { return impl_->mduData; }

} // namespace dflowfm_io
