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
        static const MduSchema schema = []
        {
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
            general.properties.push_back(fileType);

            PropertySchema fileVersion;
            fileVersion.key = "fileVersion";
            fileVersion.required = true;
            fileVersion.value_type = ValueType::String;
            fileVersion.default_value = "1.09";
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
            geometry.properties.push_back(netFile);

            PropertySchema useCaching;
            useCaching.key = "useCaching";
            useCaching.required = false;
            useCaching.value_type = ValueType::IntBool;
            useCaching.default_value = "1";
            geometry.properties.push_back(useCaching);

            PropertySchema bedLevUni;
            bedLevUni.key = "bedLevUni";
            bedLevUni.required = false;
            bedLevUni.value_type = ValueType::Float;
            bedLevUni.default_value = "-5.0";
            geometry.properties.push_back(bedLevUni);

            PropertySchema structureFile;
            structureFile.key = "structureFile";
            structureFile.required = false;
            structureFile.value_type = ValueType::PathList;
            geometry.properties.push_back(structureFile);

            PropertySchema stretchCoef;
            stretchCoef.key = "stretchCoef";
            stretchCoef.required = false;
            stretchCoef.value_type = ValueType::FloatList;
            stretchCoef.default_value = "0.1 0.3 0.6";
            geometry.properties.push_back(stretchCoef);

            PropertySchema activeProcesses;
            activeProcesses.key = "activeProcesses";
            activeProcesses.required = false;
            activeProcesses.value_type = ValueType::StringList;
            activeProcesses.default_value = "Nitrification Denitrification Reaeration";
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
            numerics.properties.push_back(maxNonLinearIterations);

            PropertySchema timeStepType;
            timeStepType.key = "timeStepType";
            timeStepType.required = false;
            timeStepType.value_type = ValueType::IntEnum;
            timeStepType.default_value = "0";
            timeStepType.enum_values = {{0}, {1}, {2}, {3}, {4}};
            numerics.properties.push_back(timeStepType);

            PropertySchema vertAdvTypSal;
            vertAdvTypSal.key = "vertAdvTypSal";
            vertAdvTypSal.required = false;
            vertAdvTypSal.value_type = ValueType::IntEnum;
            vertAdvTypSal.default_value = "6";
            timeStepType.enum_values = {{0}, {4}, {6}};
            vertAdvTypSal.status.type = StatusType::Deprecated;
            vertAdvTypSal.status.comment = "Use [numerics] verticalAdvectionType instead.";
            numerics.properties.push_back(vertAdvTypSal);

            PropertySchema layerType;
            layerType.key = "layerType";
            layerType.required = false;
            layerType.value_type = ValueType::IntEnum;
            layerType.default_value = "1";
            layerType.enum_values = {{1, ""}, {2, ""}, {3, "", {StatusType::Deprecated, "Option is deprecated."}}};
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
            time.properties.push_back(refDate);


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

    inline std::pair<const SectionSchema*, const PropertySchema*> FirstRequiredProperty()
    {
        for (const auto& s : MDU_SCHEMA.Sections())
            for (const auto& p : s.properties)
                if (p.required) return {&s, &p};
        throw std::runtime_error("No required property found in MDU_SCHEMA");
    }

    inline std::pair<const SectionSchema*, const PropertySchema*> FirstOptionalPropertyWithDefault(
        ValueType type = ValueType::Int)
    {
        for (const auto& s : MDU_SCHEMA.Sections())
            for (const auto& p : s.properties)
                if (!p.required && p.value_type == type && !p.default_value.empty()) return {&s, &p};
        throw std::runtime_error(
            std::format("No optional property of the specified type with a default value found in MDU_SCHEMA"));
    }

    inline std::pair<const SectionSchema*, const PropertySchema*> FirstPropertyOfType(ValueType type)
    {
        for (const auto& s : MDU_SCHEMA.Sections())
            for (const auto& p : s.properties)
                if (p.value_type == type) return {&s, &p};
        throw std::runtime_error("No property of the specified type found in MDU_SCHEMA");
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

    inline ini::IniData MakeCompliantIniData()
    {
        ini::IniData iniData;
        for (const auto& sectionSchema : MDU_SCHEMA.Sections())
        {
            ini::IniSection section(sectionSchema.name);
            for (const auto& propSchema : sectionSchema.properties)
                if (propSchema.status.type == StatusType::GA && (propSchema.required || !propSchema.default_value.empty()))
                    section.AddProperty(propSchema.key, propSchema.default_value);
            iniData.AddSection(std::move(section));
        }
        iniData.GetSection("general").SetPropertyValue("fileType", "modelDef");
        iniData.GetSection("general").SetPropertyValue("fileVersion", "1.09");
        iniData.GetSection("geometry").SetPropertyValue("netFile", "test_net.nc");
        return iniData;
    }

    inline MduData MakeCompliantMduData()
    {
        MduData mduData = MduData::CreateFromSchema();
        mduData.data_entries[FormatKey("general", "fileType")] = EnumValue{0};
        mduData.data_entries[FormatKey("general", "fileVersion")] = std::string{"1.09"};
        mduData.data_entries[FormatKey("geometry", "netFile")] = std::filesystem::path{"test_net.nc"};
        return mduData;
    }

    inline std::string MakeCompliantMduString()
    {
        std::ostringstream out;
        ini::IniFormatter{}.Format(MakeCompliantIniData(), out);
        return out.str();
    }

} // namespace dflowfm_io::test