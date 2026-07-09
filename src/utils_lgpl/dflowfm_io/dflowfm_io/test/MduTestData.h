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

    inline std::pair<const SectionSchema*, const PropertySchema*> FirstRequiredProperty()
    {
        for (const auto& s : MDU_SCHEMA.sections)
            for (const auto& p : s.properties)
                if (p.required) return {&s, &p};
        throw std::runtime_error("No required property found in MDU_SCHEMA");
    }

    inline std::pair<const SectionSchema*, const PropertySchema*> FirstOptionalPropertyWithDefault(
        ValueType type = ValueType::Int)
    {
        for (const auto& s : MDU_SCHEMA.sections)
            for (const auto& p : s.properties)
                if (!p.required && p.value_type == type && !p.default_value.empty()) return {&s, &p};
        throw std::runtime_error(
            std::format("No optional property of the specified type with a default value found in MDU_SCHEMA"));
    }

    inline std::pair<const SectionSchema*, const PropertySchema*> FirstPropertyOfType(ValueType type)
    {
        for (const auto& s : MDU_SCHEMA.sections)
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
        for (const auto& sectionSchema : MDU_SCHEMA.sections)
        {
            ini::IniSection section(sectionSchema.name);
            for (const auto& propSchema : sectionSchema.properties)
                if (propSchema.required || !propSchema.default_value.empty())
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