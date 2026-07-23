#include <gtest/gtest.h>

#include <dflowfm_io/MduDataConverter.h>
#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/StringUtils.h>

#include "MduTestData.h"

namespace dflowfm_io::test
{

    // -------------------------------------------------------------------------
    // Fixture
    // -------------------------------------------------------------------------

    class MduDataConverterTest : public ::testing::Test
    {
    protected:
        const MduSchema& schema = TestSchema();
    };

    // -------------------------------------------------------------------------
    // Convert IniData → MduData — fully compliant input
    // -------------------------------------------------------------------------

    TEST_F(MduDataConverterTest, ConvertIniData_FullyCompliantInput_ReportHasNoErrors)
    {
        ini::IniData iniData = TestIniData();

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasErrors());
    }

    TEST_F(MduDataConverterTest, ConvertIniData_FullyCompliantInput_ReportHasNoWarnings)
    {
        ini::IniData iniData = TestIniData();

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasWarnings());
    }

    TEST_F(MduDataConverterTest, ConvertIniData_FullyCompliantInput_MduDataIsNotEmpty)
    {
        ini::IniData iniData = TestIniData();

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_FALSE(mduData.data_entries.empty());
    }

    // -------------------------------------------------------------------------
    // Convert IniData → MduData — validation issues forwarded to report
    // -------------------------------------------------------------------------

    TEST_F(MduDataConverterTest, ConvertIniData_MissingRequiredProperty_ReportHasError)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("general").RemoveAllProperties("fileVersion");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_TRUE(report.HasErrors());
        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find("general"), std::string::npos);
        EXPECT_NE(error->message.find("fileVersion"), std::string::npos);
    }

    TEST_F(MduDataConverterTest, ConvertIniData_UnknownProperty_ReportHasWarning)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("general").AddProperty("unknownProperty_XYZ", "value");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_TRUE(report.HasWarnings());
        const Issue* warning = FirstIssue(report, Severity::Warning);
        ASSERT_NE(warning, nullptr);
        EXPECT_NE(warning->message.find("general"), std::string::npos);
        EXPECT_NE(warning->message.find("unknownProperty_XYZ"), std::string::npos);
    }

    TEST_F(MduDataConverterTest, ConvertIniData_MissingOptionalProperty_ReportHasInfo)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").RemoveAllProperties("useCaching");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_TRUE(report.HasInfos());
        const Issue* error = FirstIssue(report, Severity::Info);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find("geometry"), std::string::npos);
        EXPECT_NE(error->message.find("useCaching"), std::string::npos);
    }

    // -------------------------------------------------------------------------
    // Convert IniData → MduData — invalid property value
    // -------------------------------------------------------------------------

    struct InvalidValueTarget
    {
        ValueType type;
        std::string section;
        std::string key;
    };

    void PrintTo(const InvalidValueTarget& target, std::ostream* os) { *os << target.section << "." << target.key; }

    class MduDataConverterInvalidValueTest : public MduDataConverterTest,
                                             public ::testing::WithParamInterface<InvalidValueTarget>
    {
    };

    TEST_P(MduDataConverterInvalidValueTest, ConvertIniData_InvalidValue_ReportHasError)
    {
        const auto& target = GetParam();

        ini::IniData iniData = TestIniData();
        iniData.GetSection(target.section).SetPropertyValue(target.key, "##invalid##");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_TRUE(report.HasErrors());
        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find(target.section), std::string::npos);
        EXPECT_NE(error->message.find(target.key), std::string::npos);
    }

    INSTANTIATE_TEST_SUITE_P(
        MduDataConverterTest, MduDataConverterInvalidValueTest,
        // Note: String and Path types are excluded because any raw string is a valid value for those types.
        ::testing::Values(InvalidValueTarget{ValueType::Int, "numerics", "maxNonLinearIterations"},
                          InvalidValueTarget{ValueType::Float, "geometry", "bedLevUni"},
                          InvalidValueTarget{ValueType::IntBool, "geometry", "useCaching"},
                          InvalidValueTarget{ValueType::Enum, "general", "fileType"},
                          InvalidValueTarget{ValueType::IntEnum, "numerics", "timeStepType"},
                          InvalidValueTarget{ValueType::DateTime, "time", "tStart"},
                          InvalidValueTarget{ValueType::FloatList, "geometry", "stretchCoef"}),
        [](const ::testing::TestParamInfo<InvalidValueTarget>& info) {
            std::string name = info.param.section + "_" + info.param.key;
            std::replace(name.begin(), name.end(), '.', '_');
            return name;
        });

    TEST_F(MduDataConverterTest, ConvertIniData_InvalidEnumValue_ErrorMessageContainsAllEnumDescriptions)
    {
        const auto* targetProperty = schema.FindProperty("numerics", "verticalAdvectionType");
        ASSERT_NE(targetProperty, nullptr);

        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").SetPropertyValue("verticalAdvectionType", "##invalid##");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_TRUE(report.HasErrors());
        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        for (const auto& ev : targetProperty->enum_values)
            EXPECT_NE(error->message.find(ev.label), std::string::npos)
                << "Expected enum label \"" << ev.label << "\" in error message";
    }

    TEST_F(MduDataConverterTest, ConvertIniData_InvalidIntEnumValue_ErrorMessageContainsAllEnumValues)
    {
        const auto* targetProperty = schema.FindProperty("numerics", "timeStepType");
        ASSERT_NE(targetProperty, nullptr);

        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").SetPropertyValue("timeStepType", "##invalid##");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_TRUE(report.HasErrors());
        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        for (const auto& ev : targetProperty->enum_values)
            EXPECT_NE(error->message.find(std::to_string(ev.value)), std::string::npos)
                << "Expected enum value " << ev.value << " in error message";
    }

    TEST_F(MduDataConverterTest, ConvertIniData_InvalidDateTimeValue_ErrorMessageContainsExpectedFormat)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("time").SetPropertyValue("tStart", "##invalid##");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_TRUE(report.HasErrors());
        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find("yyyymmddhhmmss"), std::string::npos)
            << "Expected date/time format \"yyyymmddhhmmss\" in error message";
    }

    TEST_F(MduDataConverterTest, ConvertIniData_InvalidDateValue_ErrorMessageContainsExpectedFormat)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("time").SetPropertyValue("refDate", "##invalid##");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_TRUE(report.HasErrors());
        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find("yyyymmdd"), std::string::npos)
            << "Expected date format \"yyyymmdd\" in error message";
    }

    // -------------------------------------------------------------------------
    // Convert IniData → MduData — absent property with default falls back to schema default
    // -------------------------------------------------------------------------

    TEST_F(MduDataConverterTest, ConvertIniData_AbsentIntPropertyWithDefault_UsesCorrectDefaultValue)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").RemoveAllProperties("maxNonLinearIterations");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        const std::string key = FormatKey("numerics", "maxNonLinearIterations");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<int>(key), 100);
    }

    TEST_F(MduDataConverterTest, ConvertIniData_AbsentFloatPropertyWithDefault_UsesCorrectDefaultValue)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").RemoveAllProperties("bedLevUni");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        const std::string key = FormatKey("geometry", "bedLevUni");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_DOUBLE_EQ(mduData.getValueAs<double>(key), -5.0);
    }

    TEST_F(MduDataConverterTest, ConvertIniData_AbsentEnumPropertyWithDefault_UsesCorrectDefaultValue)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").RemoveAllProperties("verticalAdvectionType");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        const std::string key = FormatKey("numerics", "verticalAdvectionType");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<EnumValue>(key).value, 1);
    }

    // -------------------------------------------------------------------------
    // Convert IniData → MduData — obsolete properties and enum values are skipped
    // -------------------------------------------------------------------------

    TEST_F(MduDataConverterTest, ConvertIniData_ObsoleteProperty_PropertyOmittedFromMduData)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").AddProperty("qhRelax", 0.05);

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        const std::string key = FormatKey("numerics", "qhRelax");
        EXPECT_FALSE(mduData.hasValue(key));
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ObsoletePropertyValue_ObsoleteValueSkippedAndDefaultValueUsedInstead)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").SetPropertyValue("layerType", 4);

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        const std::string key = FormatKey("geometry", "layerType");
        // The obsolete value (4) is skipped, so the schema default value (1) is used instead.
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<EnumValue>(key).value, 1);
    }

    TEST_F(MduDataConverterTest, ConvertIniData_DeprecatedPropertyValue_PropertyPresentInMduData)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").SetPropertyValue("layerType", 3);

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        const std::string key = FormatKey("geometry", "layerType");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<EnumValue>(key).value, 3);
    }

    // -------------------------------------------------------------------------
    // Convert IniData → MduData — valid property values per type
    // -------------------------------------------------------------------------

    TEST_F(MduDataConverterTest, ConvertIniData_ValidStringValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("general").SetPropertyValue("fileVersion", "some_string");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey("general", "fileVersion");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<std::string>(key), "some_string");
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidIntValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").SetPropertyValue("maxNonLinearIterations", "42");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey("numerics", "maxNonLinearIterations");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<int>(key), 42);
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidFloatValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").SetPropertyValue("bedLevUni", "3.14");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey("geometry", "bedLevUni");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_DOUBLE_EQ(mduData.getValueAs<double>(key), 3.14);
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidIntBoolValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").SetPropertyValue("useCaching", "1");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey("geometry", "useCaching");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_TRUE(mduData.getValueAs<bool>(key));
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidPathValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").SetPropertyValue("netFile", "some/path/file.nc");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey("geometry", "netFile");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<std::filesystem::path>(key), std::filesystem::path("some/path/file.nc"));
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidEnumValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").SetPropertyValue("verticalAdvectionType", "higherOrderUpwindExplicit");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey("numerics", "verticalAdvectionType");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<EnumValue>(key).value, 1);
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidIntEnumValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").SetPropertyValue("timeStepType", 2);

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey("numerics", "timeStepType");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<EnumValue>(key).value, 2);
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidPathListValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").SetPropertyValue("structureFile", "path/a.nc path/b.nc path/c.nc");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey("geometry", "structureFile");
        EXPECT_TRUE(mduData.hasValue(key));
        const auto& paths = mduData.getValueAs<std::vector<std::filesystem::path>>(key);
        ASSERT_EQ(paths.size(), 3);
        EXPECT_EQ(paths[0], std::filesystem::path("path/a.nc"));
        EXPECT_EQ(paths[1], std::filesystem::path("path/b.nc"));
        EXPECT_EQ(paths[2], std::filesystem::path("path/c.nc"));
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidStringListValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").SetPropertyValue("activeProcesses", "proc1 proc2 proc3");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey("geometry", "activeProcesses");
        EXPECT_TRUE(mduData.hasValue(key));
        const auto& paths = mduData.getValueAs<std::vector<std::string>>(key);
        ASSERT_EQ(paths.size(), 3);
        EXPECT_EQ(paths[0], "proc1");
        EXPECT_EQ(paths[1], "proc2");
        EXPECT_EQ(paths[2], "proc3");
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidFloatListValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").SetPropertyValue("stretchCoef", "1.0 2.0 3.0");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey("geometry", "stretchCoef");
        EXPECT_TRUE(mduData.hasValue(key));
        const auto& values = mduData.getValueAs<std::vector<double>>(key);
        ASSERT_EQ(values.size(), 3);
        EXPECT_DOUBLE_EQ(values[0], 1.0);
        EXPECT_DOUBLE_EQ(values[1], 2.0);
        EXPECT_DOUBLE_EQ(values[2], 3.0);
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidDateTimeValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("time").SetPropertyValue("tStart", "20230115120000");

        const auto [mduData, report] = MduDataConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey("time", "tStart");
        EXPECT_TRUE(mduData.hasValue(key));
        const auto expected =
            std::chrono::sys_days{std::chrono::year{2023} / std::chrono::January / std::chrono::day{15}} +
            std::chrono::hours{12};
        EXPECT_EQ(mduData.getValueAs<std::chrono::system_clock::time_point>(key), expected);
    }

    // -------------------------------------------------------------------------
    // Convert MduData → IniData — fully compliant input
    // -------------------------------------------------------------------------

    TEST_F(MduDataConverterTest, ConvertMduData_FullyCompliantInput_ReturnsNonEmptyIniData)
    {
        MduData mduData = TestMduData();

        const ini::IniData iniData = MduDataConverter::Convert(mduData, schema);

        EXPECT_FALSE(iniData.empty());
    }

    TEST_F(MduDataConverterTest, ConvertMduData_FullyCompliantInput_AllPropertiesPresentInIniData)
    {
        MduData mduData = TestMduData();

        const ini::IniData iniData = MduDataConverter::Convert(mduData, schema);

        for (const auto& [key, value] : mduData.data_entries)
        {
            const auto [sectionName, propertyKey] = SplitKey(key);

            ASSERT_TRUE(iniData.HasSection(sectionName)) << "Missing section for MduData entry: " << sectionName;

            EXPECT_TRUE(iniData.GetSection(sectionName).HasProperty(propertyKey))
                << "Missing property for MduData entry: " << sectionName << "." << propertyKey;
        }
    }

    TEST_F(MduDataConverterTest, ConvertMduData_PropertyAbsentInMduData_OmittedFromIniData)
    {
        MduData mduData = TestMduData();
        const std::string key = FormatKey("geometry", "bedLevUni");
        mduData.data_entries.erase(key);

        const ini::IniData iniData = MduDataConverter::Convert(mduData, schema);

        ASSERT_TRUE(iniData.HasSection("geometry"));
        EXPECT_FALSE(iniData.GetSection("geometry").HasProperty("bedLevUni"))
            << "Property should be omitted when no value exists in MduData: geometry.bedLevUni";
    }

    TEST_F(MduDataConverterTest, ConvertMduData_FullyCompliantInput_PropertiesInSchemaOrder)
    {
        MduData mduData = TestMduData();

        const ini::IniData iniData = MduDataConverter::Convert(mduData, schema);

        std::size_t previousIndex = 0;
        for (const auto& section : iniData)
        {
            const auto schemaIt = std::find_if(schema.Sections().begin(), schema.Sections().end(),
                                               [&](const auto& s) { return s.name == section.GetName(); });
            ASSERT_NE(schemaIt, schema.Sections().end());
            const std::size_t schemaIndex = std::distance(schema.Sections().begin(), schemaIt);
            EXPECT_GE(schemaIndex, previousIndex) << "Section out of schema order: " << section.GetName();
            previousIndex = schemaIndex;

            std::size_t previousPropertyIndex = 0;
            for (const auto& property : section)
            {
                const auto propIt = std::find_if(schemaIt->properties.begin(), schemaIt->properties.end(),
                                                 [&](const auto& p) { return p.key == property.GetKey(); });
                ASSERT_NE(propIt, schemaIt->properties.end())
                    << "Property not in schema: " << section.GetName() << "." << property.GetKey();
                const std::size_t propertyIndex = std::distance(schemaIt->properties.begin(), propIt);
                EXPECT_GE(propertyIndex, previousPropertyIndex)
                    << "Property out of schema order: " << section.GetName() << "." << property.GetKey();
                previousPropertyIndex = propertyIndex;
            }
        }
    }

    // -------------------------------------------------------------------------
    // Convert MduData → IniData — comments
    // -------------------------------------------------------------------------

    TEST_F(MduDataConverterTest, ConvertMduData_FullyCompliantInput_FirstSectionHasCommentBlock)
    {
        MduData mduData = TestMduData();

        const ini::IniData iniData = MduDataConverter::Convert(mduData, schema);

        ASSERT_FALSE(iniData.empty());
        const auto& firstSection = *iniData.begin();
        EXPECT_FALSE(firstSection.GetComments().empty());
    }

    TEST_F(MduDataConverterTest, ConvertMduData_FullyCompliantInput_NonFirstSectionsHaveNoCommentBlock)
    {
        MduData mduData = TestMduData();

        const ini::IniData iniData = MduDataConverter::Convert(mduData, schema);

        ASSERT_GT(std::distance(iniData.begin(), iniData.end()), 1);
        for (auto it = std::next(iniData.begin()); it != iniData.end(); ++it)
            EXPECT_TRUE(it->GetComments().empty()) << "Unexpected comment block on section: " << it->GetName();
    }

    TEST_F(MduDataConverterTest, ConvertMduData_FullyCompliantInput_AllPropertiesHaveComment)
    {
        MduData mduData = TestMduData();

        const ini::IniData iniData = MduDataConverter::Convert(mduData, schema);

        for (const auto& section : iniData)
            for (const auto& property : section)
                EXPECT_TRUE(property.HasComment())
                    << "Property missing comment: " << section.GetName() << "." << property.GetKey();
    }

    // -------------------------------------------------------------------------
    // Round-trip: IniData → MduData → IniData
    // -------------------------------------------------------------------------

    TEST_F(MduDataConverterTest, RoundTrip_IniToMduToIni_PropertyValuesPreserved)
    {
        const ini::IniData original = TestIniData();

        const auto [mduData, report] = MduDataConverter::Convert(original, schema);
        ASSERT_FALSE(report.HasErrors());

        const ini::IniData roundTripped = MduDataConverter::Convert(mduData, schema);

        for (const auto& sectionSchema : schema.Sections())
        {
            if (!original.HasSection(sectionSchema.name)) continue;

            ASSERT_TRUE(roundTripped.HasSection(sectionSchema.name))
                << "Missing section after round-trip: " << sectionSchema.name;

            const auto& originalSection = original.GetSection(sectionSchema.name);
            const auto& roundTrippedSection = roundTripped.GetSection(sectionSchema.name);

            for (const auto& propSchema : sectionSchema.properties)
            {
                if (!originalSection.HasProperty(propSchema.key)) continue;

                ASSERT_TRUE(roundTrippedSection.HasProperty(propSchema.key))
                    << "Missing property after round-trip: " << sectionSchema.name << "." << propSchema.key;

                EXPECT_EQ(originalSection.GetProperty(propSchema.key).GetValue(),
                          roundTrippedSection.GetProperty(propSchema.key).GetValue())
                    << "Value changed in round-trip: " << sectionSchema.name << "." << propSchema.key;
            }
        }
    }

} // namespace dflowfm_io::test