#pragma once

#include <filesystem>
#include <memory>

#include <dflowfm_io/MduModel.h>

namespace dflowfm_io
{

    class MduFile
    {
    public:
        explicit MduFile(std::filesystem::path path);
        ~MduFile();

        MduFile(MduFile&&) noexcept;
        MduFile& operator=(MduFile&&) noexcept;

        MduFile(const MduFile&) = delete;
        MduFile& operator=(const MduFile&) = delete;

        static MduFile LoadFrom(std::filesystem::path path);

        void Load();

        MduModel& GetModel() { return model; }
        const MduModel& GetModel() const { return model; }

    private:
        struct Impl;
        std::unique_ptr<Impl> impl_;

        MduModel model;
    };

} // namespace dflowfm_io
