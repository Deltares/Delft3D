#pragma once

#include <dflowfm_io/ConversionResult.h>
#include <dflowfm_io/IssueReport.h>
#include <dflowfm_io/MduModel.h>
#include <dflowfm_io/MduSchema.h>
#include <ini/IniData.h>

namespace dflowfm_io
{
    class MduConverter
    {
    public:
        MduConverter();

        ConversionResult<MduModel> ToModel(const ini::IniData& iniData);

    private:
        void ValidateStructure(const ini::IniData& iniData);
        void ValidateSchemaAgainstData(const ini::IniData& iniData);
        void ValidateDataAgainstSchema(const ini::IniData& iniData);
        void ConvertValues(const ini::IniData& iniData);

        MduSchema mduSchema;
        MduModel mduModel;
        IssueReport report;
    };

} // namespace dflowfm_io