#pragma once

#include <dflowfm_io/MduDataConverter.h>
#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/StringUtils.h>

#include <ini/IniData.h>
#include <ini/IniFormatter.h>
#include <ini/IniSection.h>

#include <filesystem>
#include <format>
#include <sstream>
#include <stdexcept>
#include <string>

namespace dflowfm_io::test
{

    inline const MduSchema& TestSchema()
    {
        static const MduSchema schema = [] {
            // [general]
            SectionSchema general;
            general.name = "general";
            general.required = true;

            PropertySchema fileType;
            fileType.key = "fileType";
            fileType.required = true;
            fileType.value_type = ValueType::Enum;
            fileType.default_value = "modelDef";
            fileType.enum_values = {{0, "modelDef"}};
            fileType.description = "File type. Do not edit this.";
            general.properties.push_back(fileType);

            PropertySchema fileVersion;
            fileVersion.key = "fileVersion";
            fileVersion.required = true;
            fileVersion.value_type = ValueType::String;
            fileVersion.default_value = "1.09";
            fileVersion.description = "File version. Do not edit this.";
            general.properties.push_back(fileVersion);

            // [geometry]
            SectionSchema geometry;
            geometry.name = "geometry";
            geometry.required = true;

            PropertySchema netFile;
            netFile.key = "netFile";
            netFile.required = true;
            netFile.value_type = ValueType::Path;
            netFile.default_value = "test_net.nc";
            netFile.description = "Net file (*_net.nc) containing mesh information.";
            geometry.properties.push_back(netFile);

            PropertySchema useCaching;
            useCaching.key = "useCaching";
            useCaching.required = false;
            useCaching.value_type = ValueType::IntBool;
            useCaching.default_value = "1";
            useCaching.description = "Use caching for geometrical/network-related items.";
            geometry.properties.push_back(useCaching);

            PropertySchema bedLevUni;
            bedLevUni.key = "bedLevUni";
            bedLevUni.required = false;
            bedLevUni.value_type = ValueType::Float;
            bedLevUni.default_value = "-5.0";
            bedLevUni.description = "Uniform bed level.";
            geometry.properties.push_back(bedLevUni);

            PropertySchema structureFile;
            structureFile.key = "structureFile";
            structureFile.required = false;
            structureFile.value_type = ValueType::PathList;
            structureFile.default_value = "structures.ini";
            structureFile.description = "File (*.ini) containing list of hydraulic structures.";
            geometry.properties.push_back(structureFile);

            PropertySchema stretchCoef;
            stretchCoef.key = "stretchCoef";
            stretchCoef.required = false;
            stretchCoef.value_type = ValueType::FloatList;
            stretchCoef.default_value = "0.1 0.3 0.6";
            stretchCoef.description = "Coefficients for sigma layer.";
            geometry.properties.push_back(stretchCoef);

            PropertySchema activeProcesses;
            activeProcesses.key = "activeProcesses";
            activeProcesses.required = false;
            activeProcesses.value_type = ValueType::StringList;
            activeProcesses.default_value = "Nitrification Denitrification Reaeration";
            activeProcesses.description = "Active processes.";
            geometry.properties.push_back(activeProcesses);

            // [numerics]
            SectionSchema numerics;
            numerics.name = "numerics";
            numerics.required = false;

            PropertySchema maxNonLinearIterations;
            maxNonLinearIterations.key = "maxNonLinearIterations";
            maxNonLinearIterations.required = false;
            maxNonLinearIterations.value_type = ValueType::Int;
            maxNonLinearIterations.default_value = "100";
            maxNonLinearIterations.description = "Maximal iterations in non-linear iteration loop.";
            numerics.properties.push_back(maxNonLinearIterations);

            PropertySchema timeStepType;
            timeStepType.key = "timeStepType";
            timeStepType.required = false;
            timeStepType.value_type = ValueType::IntEnum;
            timeStepType.default_value = "0";
            timeStepType.enum_values = {{0}, {1}, {2}, {3}, {4}};
            timeStepType.description = "Type of time stepping.";
            numerics.properties.push_back(timeStepType);

            PropertySchema verticalAdvectionType;
            verticalAdvectionType.key = "verticalAdvectionType";
            verticalAdvectionType.required = false;
            verticalAdvectionType.value_type = ValueType::Enum;
            verticalAdvectionType.default_value = "higherOrderUpwindExplicit";
            verticalAdvectionType.enum_values = {{0, "centralImplicit"}, {1, "higherOrderUpwindExplicit"}};
            verticalAdvectionType.description = "Vertical advection type for salinity.";
            numerics.properties.push_back(verticalAdvectionType);

            PropertySchema vertAdvTypSal;
            vertAdvTypSal.key = "vertAdvTypSal";
            vertAdvTypSal.required = false;
            vertAdvTypSal.value_type = ValueType::IntEnum;
            vertAdvTypSal.default_value = "6";
            vertAdvTypSal.enum_values = {{0}, {4}, {6}};
            vertAdvTypSal.description = "Vertical advection type for salinity.";
            vertAdvTypSal.status.type = StatusType::Deprecated;
            vertAdvTypSal.status.comment = "Use [numerics] verticalAdvectionType instead.";
            numerics.properties.push_back(vertAdvTypSal);

            PropertySchema layerType;
            layerType.key = "layerType";
            layerType.required = false;
            layerType.value_type = ValueType::IntEnum;
            layerType.default_value = "1";
            layerType.enum_values = {{1, ""}, {2, ""}, {3, "", {StatusType::Deprecated, "Option is deprecated."}}};
            layerType.description = "Vertical layer type.";
            numerics.properties.push_back(layerType);

            // [time]
            SectionSchema time;
            time.name = "time";
            time.required = false;

            PropertySchema refDate;
            refDate.key = "refDate";
            refDate.required = false;
            refDate.value_type = ValueType::DateTime;
            refDate.default_value = "20000101";
            refDate.format = FormatType::Date;
            refDate.description = "Reference date.";
            time.properties.push_back(refDate);

            PropertySchema tStart;
            tStart.key = "tStart";
            tStart.required = false;
            tStart.value_type = ValueType::DateTime;
            tStart.default_value = "20000101000010";
            tStart.format = FormatType::DateTime;
            tStart.description = "Start time w.r.t. `refDate`.";
            time.properties.push_back(tStart);

            return MduSchema{"Test schema", {general, geometry, numerics, time}};
        }();

        return schema;
    }

    inline ini::IniData TestIniData()
    {
        ini::IniData iniData;
        const auto& mduSchema = TestSchema();
        for (const auto& sectionSchema : mduSchema.Sections())
        {
            ini::IniSection section(sectionSchema.name);
            for (const auto& propSchema : sectionSchema.properties)
                if (propSchema.status.type == StatusType::GA)
                    section.AddProperty(propSchema.key, propSchema.default_value);
            iniData.AddSection(std::move(section));
        }
        return iniData;
    }

    inline std::string TestMduString()
    {
        std::ostringstream out;
        ini::IniFormatter{}.Format(TestIniData(), out);
        return out.str();
    }

    inline MduData TestMduData()
    {
        MduData mduData = MduData::CreateFromSchema(TestSchema());
        return mduData;
    }

    inline const Issue* FirstIssue(const IssueReport& report, Severity severity)
    {
        for (const auto& issue : report)
            if (issue.severity == severity) return &issue;
        return nullptr;
    }

    inline std::pair<std::string, std::string> SplitKey(const std::string& key)
    {
        const auto pos = key.find('.');
        if (pos == std::string::npos) throw std::runtime_error("Key has no '.' separator: " + key);
        return {key.substr(0, pos), key.substr(pos + 1)};
    }

} // namespace dflowfm_io::test