#pragma once

#include <filesystem>
#include <memory>

#include <dflowfm_io/MduData.h>

namespace dflowfm_io
{

    class MduFile
    {
    public:
        MduFile();
        ~MduFile();

        MduFile(MduFile&&) noexcept;
        MduFile& operator=(MduFile&&) noexcept;

        MduFile(const MduFile&) = delete;
        MduFile& operator=(const MduFile&) = delete;

        static MduFile LoadFrom(std::istream& in);
        static MduFile LoadFrom(const std::filesystem::path& path);

        void Load(std::istream& in);
        void Load(const std::filesystem::path& path);

        MduData& GetData();
        const MduData& GetData() const;

    private:
        struct Impl;
        std::unique_ptr<Impl> impl_;
    };

} // namespace dflowfm_io
