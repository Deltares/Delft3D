#pragma once

#include <filesystem>

#include <dflowfm_io/dflowfm_io_export.h>
#include <dflowfm_io/MduModel.h>
#include <ini/IniFile.h>

namespace dflowfm_io
{

    class DFLOWFM_IO_EXPORT MduFile
    {
    public:
        explicit MduFile(std::filesystem::path path);

        static MduFile LoadFrom(std::filesystem::path path);

        void Load();

        MduModel& GetModel() { return model; }
        const MduModel& GetModel() const { return model; }

    private:
        ini::IniFile iniFile;
        MduModel model;
    };

} // namespace dflowfm_io
