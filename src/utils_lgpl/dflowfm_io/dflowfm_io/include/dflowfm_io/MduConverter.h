#pragma once

#include <dflowfm_io/ConversionResult.h>
#include <dflowfm_io/MduData.h>
#include <ini/IniData.h>

namespace dflowfm_io
{
    class MduConverter
    {
    public:
        static ConversionResult<MduData> Convert(const ini::IniData& iniData);
        static ConversionResult<ini::IniData> Convert(const MduData& mduData);
    };

} // namespace dflowfm_io