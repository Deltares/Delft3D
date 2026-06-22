#pragma once

#include <dflowfm_io/MduConverter.h>
#include <dflowfm_io/MduSchema.h>

#include <ini/IniData.h>
#include <ini/IniFormatter.h>
#include <ini/IniSection.h>
#include <ini/IniValueConverter.h>

#include <algorithm>
#include <filesystem>
#include <format>
#include <sstream>
#include <stdexcept>
#include <string>

namespace dflowfm_io::test
{

    inline Value ConvertToValue(const PropertySchema& propSchema)
    {
        const auto& str = propSchema.default_value;
        switch (propSchema.value_type)
        {
            case ValueType::Int:
                return ini::IniValueConverter::FromString<int>(str);
            case ValueType::Float:
                return ini::IniValueConverter::FromString<double>(str);
            case ValueType::IntBool:
                return ini::IniValueConverter::FromString<bool>(str);
            case ValueType::Enum: {
                const auto it = std::find_if(propSchema.enum_values.begin(), propSchema.enum_values.end(),
                                             [&](const auto& pair) { return pair.second == str; });
                return EnumValue{it->first};
            }
            case ValueType::IntEnum: {
                const auto val = ini::IniValueConverter::FromString<int>(str);
                const auto it = std::find_if(propSchema.enum_values.begin(), propSchema.enum_values.end(),
                                             [&](const auto& pair) { return pair.first == val; });
                return EnumValue{it->first};
            }
            case ValueType::FloatList:
                return ini::IniValueConverter::FromMultiValueString<double>(str);
            case ValueType::DateTime:
                return ini::IniValueConverter::FromString<std::chrono::system_clock::time_point>(str);
            default:
                return str;
        }
    }

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
        MduData mduData;
        for (const auto& sectionSchema : MDU_SCHEMA.sections)
            for (const auto& propSchema : sectionSchema.properties)
                if (propSchema.required || !propSchema.default_value.empty())
                    mduData.data_entries[FormatKey(sectionSchema.name, propSchema.key)] = ConvertToValue(propSchema);

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