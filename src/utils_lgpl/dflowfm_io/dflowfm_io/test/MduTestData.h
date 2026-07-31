#pragma once

#include <dflowfm_io/IssueReport.h>
#include <dflowfm_io/MduData.h>
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
            fileType.value_type = ValueType::StringEnum;
            fileType.default_value = "modelDef";
            fileType.description = "File type. Do not edit this.";
            fileType.required = true;
            fileType.enum_values = {{"modelDef"}};
            general.properties.push_back(fileType);

            PropertySchema fileVersion;
            fileVersion.key = "fileVersion";
            fileVersion.value_type = ValueType::String;
            fileVersion.default_value = "1.09";
            fileVersion.description = "File version. Do not edit this.";
            fileVersion.required = true;
            general.properties.push_back(fileVersion);

            // [model]
            SectionSchema model;
            model.name = "model";
            model.status = {StatusType::Obsolete, "Section has been replaced with `[general]` section."};

            PropertySchema mduFormatVersion;
            mduFormatVersion.key = "mduFormatVersion";
            mduFormatVersion.value_type = ValueType::String;
            mduFormatVersion.description = "File version. Do not edit this.";
            mduFormatVersion.status = {StatusType::Deprecated, "Keyword has been replaced with `[general] fileVersion`."};
            model.properties.push_back(mduFormatVersion);

            PropertySchema program;
            program.key = "program";
            program.value_type = ValueType::String;
            program.default_value = "D-Flow FM";
            program.description = "Program.";
            program.required = true;
            model.properties.push_back(program);

            PropertySchema autoStart;
            autoStart.key = "autoStart";
            autoStart.value_type = ValueType::IntEnum;
            autoStart.default_value = "0";
            autoStart.description = "Autostart simulation after loading MDU or not.";
            autoStart.enum_values = {{"1"},
                                     {"2", {StatusType::Deprecated, "Option is deprecated."}},
                                     {"3", {StatusType::Obsolete, "Option is obsololete.", "2026.02"}}};
            model.properties.push_back(autoStart);

            PropertySchema convertLongCulverts;
            convertLongCulverts.key = "convertLongCulverts";
            convertLongCulverts.value_type = ValueType::IntBool;
            convertLongCulverts.description = "Convert long culvert input to 1D2D long culverts.";
            convertLongCulverts.status = {StatusType::Obsolete, "Property is obsolete"};
            model.properties.push_back(convertLongCulverts);

            // [sediment]
            SectionSchema sediment;
            sediment.name = "sediment";
            sediment.status = {StatusType::Deprecated, "Section has been replaced with the `[morphology]` section."};

            PropertySchema morFile;
            morFile.key = "morFile";
            morFile.value_type = ValueType::Path;
            morFile.default_value = "test.mor";
            morFile.description = "Morphology settings file.";
            morFile.status = {StatusType::Deprecated, "Keyword has been moved to the `[morphology]` section."};
            sediment.properties.push_back(morFile);

            PropertySchema morMaxDtEps;
            morMaxDtEps.key = "morMaxDtEps";
            morMaxDtEps.value_type = ValueType::Float;
            morMaxDtEps.description = "Maximum morphological time step tolerance.";
            morMaxDtEps.status = {StatusType::Obsolete, "Property is obsolete.", "2022.02"};
            sediment.properties.push_back(morMaxDtEps);

            PropertySchema nrOfSedFractions;
            nrOfSedFractions.key = "nrOfSedFractions";
            nrOfSedFractions.value_type = ValueType::Int;
            nrOfSedFractions.default_value = "0";
            nrOfSedFractions.description = "Number of sediment fractions.";
            sediment.properties.push_back(nrOfSedFractions);

            PropertySchema sedimentModelNr;
            sedimentModelNr.key = "sedimentModelNr";
            sedimentModelNr.value_type = ValueType::IntEnum;
            sedimentModelNr.default_value = "0";
            sedimentModelNr.description = "Sediment model number.";
            sedimentModelNr.enum_values = {{"0"},
                                           {"1", {StatusType::Deprecated, "Option is deprecated."}},
                                           {"2", {StatusType::Obsolete, "Option is obsolete.", "2022.02"}}};
            sediment.properties.push_back(sedimentModelNr);

            // [geometry]
            SectionSchema geometry;
            geometry.name = "geometry";
            geometry.required = true;

            PropertySchema netFile;
            netFile.key = "netFile";
            netFile.value_type = ValueType::Path;
            netFile.default_value = "test_net.nc";
            netFile.description = "Net file (*_net.nc) containing mesh information.";
            netFile.required = true;
            geometry.properties.push_back(netFile);

            PropertySchema useCaching;
            useCaching.key = "useCaching";
            useCaching.value_type = ValueType::IntBool;
            useCaching.default_value = "1";
            useCaching.description = "Use caching for geometrical/network-related items.";
            geometry.properties.push_back(useCaching);

            PropertySchema bedLevUni;
            bedLevUni.key = "bedLevUni";
            bedLevUni.value_type = ValueType::Float;
            bedLevUni.default_value = "-5.0";
            bedLevUni.description = "Uniform bed level.";
            geometry.properties.push_back(bedLevUni);

            PropertySchema structureFile;
            structureFile.key = "structureFile";
            structureFile.value_type = ValueType::PathList;
            structureFile.default_value = "structures.ini";
            structureFile.description = "File (*.ini) containing list of hydraulic structures.";
            geometry.properties.push_back(structureFile);

            PropertySchema layerType;
            layerType.key = "layerType";
            layerType.value_type = ValueType::IntEnum;
            layerType.default_value = "1";
            layerType.description = "Vertical layer type.";
            layerType.enum_values = {{"1"},
                                     {"2"},
                                     {"3", {StatusType::Deprecated, "Option is deprecated."}},
                                     {"4", {StatusType::Obsolete, "Option is obsololete.", "2026.02"}}};
            geometry.properties.push_back(layerType);

            PropertySchema stretchCoef;
            stretchCoef.key = "stretchCoef";
            stretchCoef.value_type = ValueType::FloatList;
            stretchCoef.default_value = "0.1 0.3 0.6";
            stretchCoef.description = "Coefficients for sigma layer.";
            geometry.properties.push_back(stretchCoef);

            PropertySchema activeProcesses;
            activeProcesses.key = "activeProcesses";
            activeProcesses.value_type = ValueType::StringList;
            activeProcesses.default_value = "Nitrification Denitrification Reaeration";
            activeProcesses.description = "Active processes.";
            geometry.properties.push_back(activeProcesses);

            // [numerics]
            SectionSchema numerics;
            numerics.name = "numerics";

            PropertySchema maxNonLinearIterations;
            maxNonLinearIterations.key = "maxNonLinearIterations";
            maxNonLinearIterations.value_type = ValueType::Int;
            maxNonLinearIterations.default_value = "100";
            maxNonLinearIterations.description = "Maximal iterations in non-linear iteration loop.";
            numerics.properties.push_back(maxNonLinearIterations);

            PropertySchema timeStepType;
            timeStepType.key = "timeStepType";
            timeStepType.value_type = ValueType::IntEnum;
            timeStepType.default_value = "0";
            timeStepType.description = "Type of time stepping.";
            timeStepType.enum_values = {{"0"}, {"1"}, {"2"}, {"3"}, {"4"}};
            numerics.properties.push_back(timeStepType);

            PropertySchema qhRelax;
            qhRelax.key = "qhRelax";
            qhRelax.value_type = ValueType::Float;
            qhRelax.default_value = "0.01";
            qhRelax.description = "Relaxation on Q-h open boundaries.";
            qhRelax.status = {StatusType::Obsolete, "Option no longer supported.", "2022.02"};
            numerics.properties.push_back(qhRelax);

            PropertySchema verticalAdvectionType;
            verticalAdvectionType.key = "verticalAdvectionType";
            verticalAdvectionType.value_type = ValueType::StringEnum;
            verticalAdvectionType.default_value = "higherOrderUpwindExplicit";
            verticalAdvectionType.description = "Vertical advection type for salinity.";
            verticalAdvectionType.enum_values = {{"centralImplicit"}, {"higherOrderUpwindExplicit"}};
            numerics.properties.push_back(verticalAdvectionType);

            PropertySchema vertAdvTypSal;
            vertAdvTypSal.key = "vertAdvTypSal";
            vertAdvTypSal.value_type = ValueType::IntEnum;
            vertAdvTypSal.default_value = "6";
            vertAdvTypSal.description = "Vertical advection type for salinity.";
            vertAdvTypSal.status = {StatusType::Deprecated, "Use [numerics] verticalAdvectionType instead."};
            vertAdvTypSal.enum_values = {{"0"}, {"4"}, {"6"}};
            numerics.properties.push_back(vertAdvTypSal);

            // [time]
            SectionSchema time;
            time.name = "time";

            PropertySchema refDate;
            refDate.key = "refDate";
            refDate.value_type = ValueType::DateTime;
            refDate.default_value = "20000101";
            refDate.format = FormatType::Date;
            refDate.description = "Reference date.";
            time.properties.push_back(refDate);

            PropertySchema tStart;
            tStart.key = "tStart";
            tStart.value_type = ValueType::DateTime;
            tStart.default_value = "20000101000000";
            tStart.format = FormatType::DateTime;
            tStart.description = "Start time w.r.t. `refDate`.";
            time.properties.push_back(tStart);

            PropertySchema startDateTime;
            startDateTime.key = "startDateTime";
            startDateTime.value_type = ValueType::DateTime;
            startDateTime.format = FormatType::DateTime;
            startDateTime.description = "Computation start datetime.";
            time.properties.push_back(startDateTime);

            return MduSchema{"Test schema", {general, model, sediment, geometry, numerics, time}};
        }();

        return schema;
    }

    inline ini::IniData TestIniData()
    {
        ini::IniData iniData;
        const auto& mduSchema = TestSchema();
        for (const auto& sectionSchema : mduSchema.Sections())
        {
            if (sectionSchema.status.type == StatusType::Available)
            {
                ini::IniSection section(sectionSchema.name);
                for (const auto& propSchema : sectionSchema.properties)
                    // Only add non-obsolete and non-deprecated properties so the ini data is valid.
                    if (propSchema.status.type == StatusType::Available)
                        section.AddProperty(propSchema.key, propSchema.default_value);
                iniData.AddSection(std::move(section));
            }
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
        const MduSchema& schema = TestSchema();
        MduData mduData(schema.CreateDefaultValues());
        return mduData;
    }

    inline const Issue* FirstIssue(const IssueReport& report, Severity severity)
    {
        for (const auto& issue : report)
            if (issue.severity == severity) return &issue;
        return nullptr;
    }

    inline const Issue* FindIssue(const IssueReport& report, Severity severity, const std::string& substring)
    {
        for (const auto& issue : report)
            if (issue.severity == severity && issue.message.find(substring) != std::string::npos)
                return &issue;
        return nullptr;
    }

    inline const Issue* FindIssue(const IssueReport& report, const std::string& substring)
    {
        for (const auto& issue : report)
            if (issue.message.find(substring) != std::string::npos)
                return &issue;
        return nullptr;
    }

    inline std::pair<std::string, std::string> SplitKey(const std::string& key)
    {
        const auto pos = key.find('.');
        if (pos == std::string::npos) throw std::runtime_error("Key has no '.' separator: " + key);
        return {key.substr(0, pos), key.substr(pos + 1)};
    }

} // namespace dflowfm_io::test