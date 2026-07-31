#include <gtest/gtest.h>

#include <dflowfm_io/MduConverter.h>
#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/StringUtils.h>

#include "MduTestData.h"

namespace dflowfm_io::test
{

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

    namespace
    {
        PropertySchema MakePropertySchema(ValueType type, const std::string& key = "TestProperty")
        {
            PropertySchema schema;
            schema.key = key;
            schema.value_type = type;
            return schema;
        }

        PropertySchema MakePropertySchema(ValueType type, FormatType format, const std::string& key = "TestProperty")
        {
            PropertySchema schema = MakePropertySchema(type, key);
            schema.format = format;
            return schema;
        }

        PropertySchema MakeEnumSchema(ValueType type, std::vector<std::string> enumValues)
        {
            PropertySchema schema = MakePropertySchema(type);
            for (const auto& value : enumValues) schema.enum_values.push_back({value});
            return schema;
        }
    } // namespace

    // -------------------------------------------------------------------------
    // Fixture
    // -------------------------------------------------------------------------

    class MduConverterTest : public ::testing::Test
    {
    protected:
        const MduSchema& schema = TestSchema();
    };

    // -------------------------------------------------------------------------
    // Convert IniData → MduData — fully compliant input
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ConvertIniData_FullyCompliantInput_ReportHasNoErrors)
    {
        ini::IniData iniData = TestIniData();

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasError());
    }

    TEST_F(MduConverterTest, ConvertIniData_FullyCompliantInput_ReportHasNoWarnings)
    {
        ini::IniData iniData = TestIniData();

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasWarning());
    }

    TEST_F(MduConverterTest, ConvertIniData_FullyCompliantInput_MduDataIsNotEmpty)
    {
        ini::IniData iniData = TestIniData();

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_FALSE(mduData.empty());
    }

    // -------------------------------------------------------------------------
    // Convert IniData → MduData — validation issues forwarded to report
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ConvertIniData_MissingRequiredProperty_ReportHasError)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("general").RemoveAllProperties("fileVersion");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_TRUE(report.HasError());
        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find("general"), std::string::npos);
        EXPECT_NE(error->message.find("fileVersion"), std::string::npos);
    }

    TEST_F(MduConverterTest, ConvertIniData_UnknownProperty_ReportHasWarning)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("general").AddProperty("unknownProperty_XYZ", "value");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_TRUE(report.HasWarning());
        const Issue* warning = FirstIssue(report, Severity::Warning);
        ASSERT_NE(warning, nullptr);
        EXPECT_NE(warning->message.find("general"), std::string::npos);
        EXPECT_NE(warning->message.find("unknownProperty_XYZ"), std::string::npos);
    }

    TEST_F(MduConverterTest, ConvertIniData_OptionalPropertyValueIsEmpty_ReportHasInfo)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").SetPropertyValue("useCaching", "");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_TRUE(report.HasInfo());
        const Issue* info = FirstIssue(report, Severity::Info);
        ASSERT_NE(info, nullptr);
        EXPECT_NE(info->message.find("geometry"), std::string::npos);
        EXPECT_NE(info->message.find("useCaching"), std::string::npos);
    }

    TEST_F(MduConverterTest, ConvertIniData_MissingOptionalProperty_ReportHasDebug)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").RemoveAllProperties("useCaching");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_TRUE(report.HasDebug());
        const Issue* debug = FirstIssue(report, Severity::Debug);
        ASSERT_NE(debug, nullptr);
        EXPECT_NE(debug->message.find("geometry"), std::string::npos);
        EXPECT_NE(debug->message.find("useCaching"), std::string::npos);
    }

    // -------------------------------------------------------------------------
    // Convert IniData → MduData — invalid property value
    // -------------------------------------------------------------------------

    struct InvalidValueTarget
    {
        std::string type;
        std::string section;
        std::string key;
    };

    void PrintTo(const InvalidValueTarget& target, std::ostream* os) { *os << target.section << "." << target.key; }

    class MduDataConverterInvalidValueTest : public MduConverterTest,
                                             public ::testing::WithParamInterface<InvalidValueTarget>
    {
    };

    TEST_P(MduDataConverterInvalidValueTest, ConvertIniData_InvalidValue_ReportHasError)
    {
        const auto& target = GetParam();

        ini::IniData iniData = TestIniData();
        iniData.GetSection(target.section).SetPropertyValue(target.key, "##invalid##");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_TRUE(report.HasError());
        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find(target.type), std::string::npos);
        EXPECT_NE(error->message.find(target.section), std::string::npos);
        EXPECT_NE(error->message.find(target.key), std::string::npos);
    }

    INSTANTIATE_TEST_SUITE_P(
        MduConverterTest, MduDataConverterInvalidValueTest,
        // Note: String and Path types are excluded because any raw string is a valid value for those types.
        ::testing::Values(InvalidValueTarget{"integer", "numerics", "maxNonLinearIterations"},
                          InvalidValueTarget{"float", "geometry", "bedLevUni"},
                          InvalidValueTarget{"integer (0 or 1)", "geometry", "useCaching"},
                          InvalidValueTarget{"Supported values", "general", "fileType"},
                          InvalidValueTarget{"Supported values", "numerics", "timeStepType"},
                          InvalidValueTarget{"datetime", "time", "tStart"},
                          InvalidValueTarget{"list of floats", "geometry", "stretchCoef"}),
        [](const ::testing::TestParamInfo<InvalidValueTarget>& info) {
            std::string name = info.param.section + "_" + info.param.key;
            std::replace(name.begin(), name.end(), '.', '_');
            return name;
        });

    struct EnumErrorMessageTarget
    {
        std::string section;
        std::string key;
    };

    void PrintTo(const EnumErrorMessageTarget& target, std::ostream* os) { *os << target.section << "." << target.key; }

    class MduDataConverterEnumErrorMessageTest : public MduConverterTest,
                                                 public ::testing::WithParamInterface<EnumErrorMessageTarget>
    {
    };

    TEST_P(MduDataConverterEnumErrorMessageTest, ConvertIniData_InvalidEnumValue_ErrorMessageContainsAllEnumValues)
    {
        const auto& target = GetParam();

        const auto* targetProperty = schema.FindProperty(target.section, target.key);
        ASSERT_NE(targetProperty, nullptr);

        ini::IniData iniData = TestIniData();
        iniData.GetSection(target.section).SetPropertyValue(target.key, "##invalid##");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_TRUE(report.HasError());
        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        for (const auto& ev : targetProperty->enum_values)
            EXPECT_NE(error->message.find(ev.value), std::string::npos)
                << "Expected enum value \"" << ev.value << "\" in error message";
    }

    INSTANTIATE_TEST_SUITE_P(MduConverterTest, MduDataConverterEnumErrorMessageTest,
                             ::testing::Values(EnumErrorMessageTarget{"numerics", "verticalAdvectionType"},
                                               EnumErrorMessageTarget{"numerics", "timeStepType"}),
                             [](const ::testing::TestParamInfo<EnumErrorMessageTarget>& info) {
                                 return info.param.section + "_" + info.param.key;
                             });

    TEST_F(MduConverterTest, ConvertIniData_InvalidDateTimeValue_ErrorMessageContainsExpectedFormat)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("time").SetPropertyValue("tStart", "##invalid##");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_TRUE(report.HasError());
        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find("yyyymmddhhmmss"), std::string::npos)
            << "Expected date/time format \"yyyymmddhhmmss\" in error message";
    }

    TEST_F(MduConverterTest, ConvertIniData_InvalidDateValue_ErrorMessageContainsExpectedFormat)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("time").SetPropertyValue("refDate", "##invalid##");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_TRUE(report.HasError());
        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find("yyyymmdd"), std::string::npos)
            << "Expected date format \"yyyymmdd\" in error message";
    }

    // -------------------------------------------------------------------------
    // Convert IniData → MduData — absent property with default falls back to schema default
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ConvertIniData_AbsentIntPropertyWithDefault_UsesCorrectDefaultValue)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").RemoveAllProperties("maxNonLinearIterations");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        const std::string key = FormatKey("numerics", "maxNonLinearIterations");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<int>(key), 100);
    }

    TEST_F(MduConverterTest, ConvertIniData_AbsentFloatPropertyWithDefault_UsesCorrectDefaultValue)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").RemoveAllProperties("bedLevUni");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        const std::string key = FormatKey("geometry", "bedLevUni");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_DOUBLE_EQ(mduData.getValueAs<double>(key), -5.0);
    }

    TEST_F(MduConverterTest, ConvertIniData_AbsentEnumPropertyWithDefault_UsesCorrectDefaultValue)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").RemoveAllProperties("verticalAdvectionType");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        const std::string key = FormatKey("numerics", "verticalAdvectionType");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<StringEnumValue>(key).value, "higherOrderUpwindExplicit");
    }

    // -------------------------------------------------------------------------
    // Convert IniData → MduData — obsolete sections, properties and enum values are skipped
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ConvertIniData_ObsoleteSection_PropertiesOmittedFromMduData)
    {
        ini::IniData iniData = TestIniData();
        auto& section = iniData.AddSection("model");
        section.AddProperty("program", "D-Flow FM");
        section.AddProperty("mduFormatVersion", "1.09");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_FALSE(mduData.hasValue(FormatKey("model", "program")))
            << "Property from obsolete section should be omitted: model.program";
        EXPECT_FALSE(mduData.hasValue(FormatKey("model", "mduFormatVersion")))
            << "Property from obsolete section should be omitted: model.mduFormatVersion";
    }

    TEST_F(MduConverterTest, ConvertIniData_ObsoleteProperty_PropertyOmittedFromMduData)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").AddProperty("qhRelax", 0.05);

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        const std::string key = FormatKey("numerics", "qhRelax");
        EXPECT_FALSE(mduData.hasValue(key));
    }

    TEST_F(MduConverterTest, ConvertIniData_ObsoletePropertyValue_ObsoleteValueSkippedAndDefaultValueUsedInstead)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").SetPropertyValue("layerType", 4);

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        const std::string key = FormatKey("geometry", "layerType");
        // The obsolete value (4) is skipped, so the schema default value (1) is used instead.
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<IntEnumValue>(key).value, 1);
    }

    // -------------------------------------------------------------------------
    // Convert IniData → MduData — deprecated sections, properties and enum values are converted
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ConvertIniData_DeprecatedSection_PropertiesPresentInMduData)
    {
        ini::IniData iniData = TestIniData();
        auto& section = iniData.AddSection("sediment");
        section.AddProperty("nrOfSedFractions", 2);

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        const std::string key = FormatKey("sediment", "nrOfSedFractions");
        EXPECT_TRUE(mduData.hasValue(key))
            << "Property from deprecated section should still be converted: sediment.nrOfSedFractions";
        EXPECT_EQ(mduData.getValueAs<int>(key), 2);
    }

    TEST_F(MduConverterTest, ConvertIniData_DeprecatedProperty_PropertyPresentInMduData)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").SetPropertyValue("vertAdvTypSal", 6);

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        const std::string key = FormatKey("numerics", "vertAdvTypSal");
        EXPECT_TRUE(mduData.hasValue(key)) << "Deprecated property should still be converted: numerics.vertAdvTypSal";
        EXPECT_EQ(mduData.getValueAs<IntEnumValue>(key).value, 6);
    }

    TEST_F(MduConverterTest, ConvertIniData_DeprecatedEnumValue_PropertyPresentInMduData)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").SetPropertyValue("layerType", 3);

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        const std::string key = FormatKey("geometry", "layerType");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<IntEnumValue>(key).value, 3);
    }

    // -------------------------------------------------------------------------
    // Convert IniData → MduData — valid property values per type
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ConvertIniData_ValidStringValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("general").SetPropertyValue("fileVersion", "some_string");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasError());
        const std::string key = FormatKey("general", "fileVersion");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<std::string>(key), "some_string");
    }

    TEST_F(MduConverterTest, ConvertIniData_ValidIntValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").SetPropertyValue("maxNonLinearIterations", "42");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasError());
        const std::string key = FormatKey("numerics", "maxNonLinearIterations");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<int>(key), 42);
    }

    TEST_F(MduConverterTest, ConvertIniData_ValidFloatValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").SetPropertyValue("bedLevUni", "3.14");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasError());
        const std::string key = FormatKey("geometry", "bedLevUni");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_DOUBLE_EQ(mduData.getValueAs<double>(key), 3.14);
    }

    TEST_F(MduConverterTest, ConvertIniData_ValidIntBoolValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").SetPropertyValue("useCaching", "1");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasError());
        const std::string key = FormatKey("geometry", "useCaching");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_TRUE(mduData.getValueAs<bool>(key));
    }

    TEST_F(MduConverterTest, ConvertIniData_ValidPathValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").SetPropertyValue("netFile", "some/path/file.nc");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasError());
        const std::string key = FormatKey("geometry", "netFile");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<std::filesystem::path>(key), std::filesystem::path("some/path/file.nc"));
    }

    TEST_F(MduConverterTest, ConvertIniData_ValidStringEnumValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").SetPropertyValue("verticalAdvectionType", "higherOrderUpwindExplicit");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasError());
        const std::string key = FormatKey("numerics", "verticalAdvectionType");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<StringEnumValue>(key).value, "higherOrderUpwindExplicit");
    }

    TEST_F(MduConverterTest, ConvertIniData_ValidIntEnumValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").SetPropertyValue("timeStepType", 2);

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasError());
        const std::string key = FormatKey("numerics", "timeStepType");
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<IntEnumValue>(key).value, 2);
    }

    TEST_F(MduConverterTest, ConvertIniData_ValidPathListValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").SetPropertyValue("structureFile", "path/a.nc path/b.nc path/c.nc");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasError());
        const std::string key = FormatKey("geometry", "structureFile");
        EXPECT_TRUE(mduData.hasValue(key));
        const auto& paths = mduData.getValueAs<std::vector<std::filesystem::path>>(key);
        ASSERT_EQ(paths.size(), 3);
        EXPECT_EQ(paths[0], std::filesystem::path("path/a.nc"));
        EXPECT_EQ(paths[1], std::filesystem::path("path/b.nc"));
        EXPECT_EQ(paths[2], std::filesystem::path("path/c.nc"));
    }

    TEST_F(MduConverterTest, ConvertIniData_ValidStringListValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").SetPropertyValue("activeProcesses", "proc1 proc2 proc3");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasError());
        const std::string key = FormatKey("geometry", "activeProcesses");
        EXPECT_TRUE(mduData.hasValue(key));
        const auto& paths = mduData.getValueAs<std::vector<std::string>>(key);
        ASSERT_EQ(paths.size(), 3);
        EXPECT_EQ(paths[0], "proc1");
        EXPECT_EQ(paths[1], "proc2");
        EXPECT_EQ(paths[2], "proc3");
    }

    TEST_F(MduConverterTest, ConvertIniData_ValidFloatListValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").SetPropertyValue("stretchCoef", "1.0 2.0 3.0");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasError());
        const std::string key = FormatKey("geometry", "stretchCoef");
        EXPECT_TRUE(mduData.hasValue(key));
        const auto& values = mduData.getValueAs<std::vector<double>>(key);
        ASSERT_EQ(values.size(), 3);
        EXPECT_DOUBLE_EQ(values[0], 1.0);
        EXPECT_DOUBLE_EQ(values[1], 2.0);
        EXPECT_DOUBLE_EQ(values[2], 3.0);
    }

    TEST_F(MduConverterTest, ConvertIniData_ValidDateTimeValue_ConvertsSuccessfully)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("time").SetPropertyValue("tStart", "20230115120000");

        const auto [mduData, report] = MduConverter::Convert(iniData, schema);

        EXPECT_FALSE(report.HasError());
        const std::string key = FormatKey("time", "tStart");
        EXPECT_TRUE(mduData.hasValue(key));
        const auto expected =
            std::chrono::sys_days{std::chrono::year{2023} / std::chrono::January / std::chrono::day{15}} +
            std::chrono::hours{12};
        const auto& actual = mduData.getValueAs<std::optional<std::chrono::system_clock::time_point>>(key);
        ASSERT_TRUE(actual.has_value());
        EXPECT_EQ(actual.value(), expected);
    }

    // -------------------------------------------------------------------------
    // Convert MduData → IniData — fully compliant input
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ConvertMduData_FullyCompliantInput_ReturnsNonEmptyIniData)
    {
        MduData mduData = TestMduData();

        const ini::IniData iniData = MduConverter::Convert(mduData, schema);

        EXPECT_FALSE(iniData.empty());
    }

    TEST_F(MduConverterTest, ConvertMduData_FullyCompliantInput_AllPropertiesPresentInIniData)
    {
        MduData mduData = TestMduData();

        const ini::IniData iniData = MduConverter::Convert(mduData, schema);

        mduData.visitKeyValuePairs([&](std::string_view key, const Value& value) {
            const auto [sectionName, propertyKey] = SplitKey(std::string(key));

            ASSERT_TRUE(iniData.HasSection(sectionName)) << "Missing section for MduData entry: " << sectionName;

            EXPECT_TRUE(iniData.GetSection(sectionName).HasProperty(propertyKey))
                << "Missing property for MduData entry: " << sectionName << "." << propertyKey;
        });
    }

    TEST_F(MduConverterTest, ConvertMduData_PropertyAbsentInMduData_OmittedFromIniData)
    {
        std::unordered_map<std::string, Value> filteredKeyValuePairs;
        const MduData completeMduData = TestMduData();
        completeMduData.visitKeyValuePairs([&](std::string_view key, const Value& value) {
            if (key == FormatKey("geometry", "bedLevUni"))
                return; // Skip this property to simulate it being absent in MduData
            filteredKeyValuePairs[std::string(key)] = value;
        });

        const MduData mduData(std::move(filteredKeyValuePairs));

        const ini::IniData iniData = MduConverter::Convert(mduData, schema);

        ASSERT_TRUE(iniData.HasSection("geometry"));
        EXPECT_FALSE(iniData.GetSection("geometry").HasProperty("bedLevUni"))
            << "Property should be omitted when no value exists in MduData: geometry.bedLevUni";
    }

    TEST_F(MduConverterTest, ConvertMduData_FullyCompliantInput_PropertiesInSchemaOrder)
    {
        MduData mduData = TestMduData();

        const ini::IniData iniData = MduConverter::Convert(mduData, schema);

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

    TEST_F(MduConverterTest, ConvertMduData_FullyCompliantInput_FirstSectionHasCommentBlock)
    {
        MduData mduData = TestMduData();

        const ini::IniData iniData = MduConverter::Convert(mduData, schema);

        ASSERT_FALSE(iniData.empty());
        const auto& firstSection = *iniData.begin();
        EXPECT_FALSE(firstSection.GetComments().empty());
    }

    TEST_F(MduConverterTest, ConvertMduData_FullyCompliantInput_NonFirstSectionsHaveNoCommentBlock)
    {
        MduData mduData = TestMduData();

        const ini::IniData iniData = MduConverter::Convert(mduData, schema);

        ASSERT_GT(std::distance(iniData.begin(), iniData.end()), 1);
        for (auto it = std::next(iniData.begin()); it != iniData.end(); ++it)
            EXPECT_TRUE(it->GetComments().empty()) << "Unexpected comment block on section: " << it->GetName();
    }

    TEST_F(MduConverterTest, ConvertMduData_FullyCompliantInput_AllPropertiesHaveComment)
    {
        MduData mduData = TestMduData();

        const ini::IniData iniData = MduConverter::Convert(mduData, schema);

        for (const auto& section : iniData)
            for (const auto& property : section)
                EXPECT_TRUE(property.HasComment())
                    << "Property missing comment: " << section.GetName() << "." << property.GetKey();
    }

    // -------------------------------------------------------------------------
    // Round-trip: IniData → MduData → IniData
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, RoundTrip_IniToMduToIni_PropertyValuesPreserved)
    {
        const ini::IniData original = TestIniData();

        const auto [mduData, report] = MduConverter::Convert(original, schema);
        ASSERT_FALSE(report.HasError());

        const ini::IniData roundTripped = MduConverter::Convert(mduData, schema);

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

    // -------------------------------------------------------------------------
    // ValueFromString — String
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ValueFromString_String_ReturnsCorrectValue)
    {
        auto schema = MakePropertySchema(ValueType::String);
        auto result = MduConverter::ValueFromString(schema, "hello");

        EXPECT_EQ(std::get<std::string>(result), "hello");
    }

    // -------------------------------------------------------------------------
    // ValueFromString — Int
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ValueFromString_Int_ReturnsCorrectValue)
    {
        auto schema = MakePropertySchema(ValueType::Int);
        auto result = MduConverter::ValueFromString(schema, "42");

        EXPECT_EQ(std::get<int>(result), 42);
    }

    TEST_F(MduConverterTest, ValueFromString_Int_InvalidValue_ThrowsInvalidArgument)
    {
        auto schema = MakePropertySchema(ValueType::Int);

        EXPECT_THROW(MduConverter::ValueFromString(schema, "not_an_int"), std::invalid_argument);
    }

    // -------------------------------------------------------------------------
    // ValueFromString — Float
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ValueFromString_Float_ReturnsCorrectValue)
    {
        auto schema = MakePropertySchema(ValueType::Float);
        auto result = MduConverter::ValueFromString(schema, "3.14");

        EXPECT_DOUBLE_EQ(std::get<double>(result), 3.14);
    }

    TEST_F(MduConverterTest, ValueFromString_Float_FortranExponent_ReturnsCorrectValue)
    {
        auto schema = MakePropertySchema(ValueType::Float);
        auto result = MduConverter::ValueFromString(schema, "1.0d-3");

        EXPECT_DOUBLE_EQ(std::get<double>(result), 1.0e-3);
    }

    TEST_F(MduConverterTest, ValueFromString_Float_InvalidValue_ThrowsInvalidArgument)
    {
        auto schema = MakePropertySchema(ValueType::Float);

        EXPECT_THROW(MduConverter::ValueFromString(schema, "not_a_float"), std::invalid_argument);
    }

    // -------------------------------------------------------------------------
    // ValueFromString — IntBool
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ValueFromString_IntBool_Zero_ReturnsFalse)
    {
        auto schema = MakePropertySchema(ValueType::IntBool);
        auto result = MduConverter::ValueFromString(schema, "0");

        EXPECT_EQ(std::get<bool>(result), false);
    }

    TEST_F(MduConverterTest, ValueFromString_IntBool_One_ReturnsTrue)
    {
        auto schema = MakePropertySchema(ValueType::IntBool);
        auto result = MduConverter::ValueFromString(schema, "1");

        EXPECT_EQ(std::get<bool>(result), true);
    }

    TEST_F(MduConverterTest, ValueFromString_IntBool_InvalidValue_ThrowsInvalidArgument)
    {
        auto schema = MakePropertySchema(ValueType::IntBool);

        EXPECT_THROW(MduConverter::ValueFromString(schema, "not_a_bool"), std::invalid_argument);
    }

    // -------------------------------------------------------------------------
    // ValueFromString — Path
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ValueFromString_Path_ReturnsCorrectValue)
    {
        auto schema = MakePropertySchema(ValueType::Path);
        auto result = MduConverter::ValueFromString(schema, "some/path/file.txt");

        EXPECT_EQ(std::get<std::filesystem::path>(result), std::filesystem::path("some/path/file.txt"));
    }

    // -------------------------------------------------------------------------
    // ValueFromString — DateTime
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ValueFromString_DateTime_CompactDateTime_ReturnsCorrectValue)
    {
        auto schema = MakePropertySchema(ValueType::DateTime);
        auto result = MduConverter::ValueFromString(schema, "20200130120000");

        const auto expected =
            std::chrono::sys_days{std::chrono::year{2020} / std::chrono::month{1} / std::chrono::day{30}} +
            std::chrono::hours{12};

        const auto& actual = std::get<std::optional<std::chrono::system_clock::time_point>>(result);
        ASSERT_TRUE(actual.has_value());
        EXPECT_EQ(actual.value(), expected);
    }

    TEST_F(MduConverterTest, ValueFromString_DateTime_DateFormat_ReturnsCorrectValue)
    {
        auto schema = MakePropertySchema(ValueType::DateTime, FormatType::Date);
        auto result = MduConverter::ValueFromString(schema, "20200130");

        const auto expected =
            std::chrono::sys_days{std::chrono::year{2020} / std::chrono::month{1} / std::chrono::day{30}};

        const auto& actual = std::get<std::optional<std::chrono::system_clock::time_point>>(result);
        ASSERT_TRUE(actual.has_value());
        EXPECT_EQ(actual.value(), expected);
    }

    TEST_F(MduConverterTest, ValueFromString_DateTime_DateFormat_WithTimeComponent_ThrowsInvalidArgument)
    {
        // Schema expects CompactDateOnly, but the value carries a time component.
        auto schema = MakePropertySchema(ValueType::DateTime, FormatType::Date);

        EXPECT_THROW(MduConverter::ValueFromString(schema, "20200130120000"), std::invalid_argument);
    }

    TEST_F(MduConverterTest, ValueFromString_DateTime_CompactDateTime_DateOnlyValue_ThrowsInvalidArgument)
    {
        // Schema expects CompactDateTime, but the value only carries a date.
        auto schema = MakePropertySchema(ValueType::DateTime);

        EXPECT_THROW(MduConverter::ValueFromString(schema, "20200130"), std::invalid_argument);
    }

    TEST_F(MduConverterTest, ValueFromString_DateTime_InvalidValue_ThrowsInvalidArgument)
    {
        auto schema = MakePropertySchema(ValueType::DateTime);

        EXPECT_THROW(MduConverter::ValueFromString(schema, "not_a_date"), std::invalid_argument);
    }

    TEST_F(MduConverterTest, ValueFromString_DateTime_EmptyValue_ReturnsNullopt)
    {
        auto schema = MakePropertySchema(ValueType::DateTime);
        auto result = MduConverter::ValueFromString(schema, "");

        const auto& actual = std::get<std::optional<std::chrono::system_clock::time_point>>(result);
        EXPECT_FALSE(actual.has_value());
    }

    // -------------------------------------------------------------------------
    // ValueFromString — List types
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ValueFromString_StringList_ReturnsCorrectValues)
    {
        auto schema = MakePropertySchema(ValueType::StringList);
        auto result = MduConverter::ValueFromString(schema, "a b c");

        auto values = std::get<std::vector<std::string>>(result);
        ASSERT_EQ(values.size(), 3u);
        EXPECT_EQ(values[0], "a");
        EXPECT_EQ(values[1], "b");
        EXPECT_EQ(values[2], "c");
    }

    TEST_F(MduConverterTest, ValueFromString_FloatList_ReturnsCorrectValues)
    {
        auto schema = MakePropertySchema(ValueType::FloatList);
        auto result = MduConverter::ValueFromString(schema, "1.0 2.0 3.0");

        auto values = std::get<std::vector<double>>(result);
        ASSERT_EQ(values.size(), 3u);
        EXPECT_DOUBLE_EQ(values[0], 1.0);
        EXPECT_DOUBLE_EQ(values[1], 2.0);
        EXPECT_DOUBLE_EQ(values[2], 3.0);
    }

    TEST_F(MduConverterTest, ValueFromString_PathList_ReturnsCorrectValues)
    {
        auto schema = MakePropertySchema(ValueType::PathList);
        auto result = MduConverter::ValueFromString(schema, "a.txt b.txt");

        auto values = std::get<std::vector<std::filesystem::path>>(result);
        ASSERT_EQ(values.size(), 2u);
        EXPECT_EQ(values[0], std::filesystem::path("a.txt"));
        EXPECT_EQ(values[1], std::filesystem::path("b.txt"));
    }

    // -------------------------------------------------------------------------
    // ValueFromString — Enum types
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ValueFromString_StringEnum_ValidName_ReturnsCorrectValue)
    {
        auto schema = MakeEnumSchema(ValueType::StringEnum, {"None", "Explicit", "Implicit"});
        auto result = MduConverter::ValueFromString(schema, "Explicit");

        EXPECT_EQ(std::get<StringEnumValue>(result).value, "Explicit");
    }

    TEST_F(MduConverterTest, ValueFromString_StringEnum_NameCaseInsensitive_ReturnsCorrectValue)
    {
        auto schema = MakeEnumSchema(ValueType::StringEnum, {"None", "Explicit"});
        auto result = MduConverter::ValueFromString(schema, "explicit");

        EXPECT_EQ(std::get<StringEnumValue>(result).value, "Explicit");
    }

    TEST_F(MduConverterTest, ValueFromString_StringEnum_InvalidName_ThrowsInvalidArgument)
    {
        auto schema = MakeEnumSchema(ValueType::StringEnum, {"None", "Explicit"});

        EXPECT_THROW(MduConverter::ValueFromString(schema, "Unknown"), std::invalid_argument);
    }

    TEST_F(MduConverterTest, ValueFromString_IntEnum_ValidNumber_ReturnsCorrectValue)
    {
        auto schema = MakeEnumSchema(ValueType::IntEnum, {"0", "1", "2"});
        auto result = MduConverter::ValueFromString(schema, "2");

        EXPECT_EQ(std::get<IntEnumValue>(result).value, 2);
    }

    TEST_F(MduConverterTest, ValueFromString_IntEnum_OutOfRangeNumber_ThrowsInvalidArgument)
    {
        auto schema = MakeEnumSchema(ValueType::IntEnum, {"0", "1"});

        EXPECT_THROW(MduConverter::ValueFromString(schema, "99"), std::invalid_argument);
    }

    TEST_F(MduConverterTest, ValueFromString_IntEnum_InvalidString_ThrowsInvalidArgument)
    {
        auto schema = MakeEnumSchema(ValueType::IntEnum, {"0", "1"});

        EXPECT_THROW(MduConverter::ValueFromString(schema, "not_a_number"), std::invalid_argument);
    }

    // -------------------------------------------------------------------------
    // ValueToString — String
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ValueToString_String_ReturnsCorrectValue)
    {
        auto schema = MakePropertySchema(ValueType::String);
        auto result = MduConverter::ValueToString(schema, Value{std::string("hello")});

        EXPECT_EQ(result, "hello");
    }

    // -------------------------------------------------------------------------
    // ValueToString — Int
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ValueToString_Int_ReturnsCorrectValue)
    {
        auto schema = MakePropertySchema(ValueType::Int);
        auto result = MduConverter::ValueToString(schema, Value{42});

        EXPECT_EQ(result, "42");
    }

    // -------------------------------------------------------------------------
    // ValueToString — Float
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ValueToString_Float_ReturnsCorrectValue)
    {
        auto schema = MakePropertySchema(ValueType::Float);
        auto result = MduConverter::ValueToString(schema, Value{3.14});

        EXPECT_EQ(result, "3.14");
    }

    TEST_F(MduConverterTest, ValueToString_Float_FixedFormat_ReturnsFixedNotation)
    {
        auto schema = MakePropertySchema(ValueType::Float, FormatType::Fixed);
        auto result = MduConverter::ValueToString(schema, Value{1234.5});

        EXPECT_EQ(result, "1234.500000");
    }

    TEST_F(MduConverterTest, ValueToString_Float_ScientificFormat_ReturnsScientificNotation)
    {
        auto schema = MakePropertySchema(ValueType::Float, FormatType::Scientific);
        auto result = MduConverter::ValueToString(schema, Value{1234.5});

        EXPECT_EQ(result, "1.234500e+03");
    }

    // -------------------------------------------------------------------------
    // ValueToString — IntBool
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ValueToString_IntBool_False_ReturnsZero)
    {
        auto schema = MakePropertySchema(ValueType::IntBool);
        auto result = MduConverter::ValueToString(schema, Value{false});

        EXPECT_EQ(result, "0");
    }

    TEST_F(MduConverterTest, ValueToString_IntBool_True_ReturnsOne)
    {
        auto schema = MakePropertySchema(ValueType::IntBool);
        auto result = MduConverter::ValueToString(schema, Value{true});

        EXPECT_EQ(result, "1");
    }

    // -------------------------------------------------------------------------
    // ValueToString — Path
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ValueToString_Path_ReturnsCorrectValue)
    {
        auto schema = MakePropertySchema(ValueType::Path);
        auto result = MduConverter::ValueToString(schema, Value{std::filesystem::path("some/path")});

        EXPECT_EQ(result, "some/path");
    }

    // -------------------------------------------------------------------------
    // ValueToString — DateTime
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ValueToString_DateTime_ReturnsCorrectValue)
    {
        auto schema = MakePropertySchema(ValueType::DateTime);

        const auto timePoint =
            std::chrono::sys_days{std::chrono::year{2020} / std::chrono::month{1} / std::chrono::day{30}};

        auto result =
            MduConverter::ValueToString(schema, Value{std::optional<std::chrono::system_clock::time_point>{timePoint}});

        EXPECT_EQ(result, "20200130000000");
    }

    TEST_F(MduConverterTest, ValueToString_DateTime_DateFormat_ReturnsDateOnly)
    {
        auto schema = MakePropertySchema(ValueType::DateTime, FormatType::Date);

        const auto timePoint =
            std::chrono::sys_days{std::chrono::year{2020} / std::chrono::month{1} / std::chrono::day{30}};

        auto result =
            MduConverter::ValueToString(schema, Value{std::optional<std::chrono::system_clock::time_point>{timePoint}});

        EXPECT_EQ(result, "20200130");
    }

    TEST_F(MduConverterTest, ValueToString_DateTime_Nullopt_ReturnsEmptyString)
    {
        auto schema = MakePropertySchema(ValueType::DateTime);

        auto result =
            MduConverter::ValueToString(schema, Value{std::optional<std::chrono::system_clock::time_point>{std::nullopt}});

        EXPECT_TRUE(result.empty());
    }

    // -------------------------------------------------------------------------
    // ValueToString — List types
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ValueToString_StringList_ReturnsSpaceSeparated)
    {
        auto schema = MakePropertySchema(ValueType::StringList);
        Value v = std::vector<std::string>{"a", "b", "c"};
        auto result = MduConverter::ValueToString(schema, v);

        EXPECT_EQ(result, "a b c");
    }

    TEST_F(MduConverterTest, ValueToString_FloatList_ReturnsSpaceSeparated)
    {
        auto schema = MakePropertySchema(ValueType::FloatList);
        Value v = std::vector<double>{1.0, 2.0, 3.0};
        auto result = MduConverter::ValueToString(schema, v);

        EXPECT_EQ(result, "1.0 2.0 3.0");
    }

    TEST_F(MduConverterTest, ValueToString_FloatList_FixedFormat_ReturnsFixedNotation)
    {
        auto schema = MakePropertySchema(ValueType::FloatList, FormatType::Fixed);
        Value v = std::vector<double>{1.0, 2.5};
        auto result = MduConverter::ValueToString(schema, v);

        EXPECT_EQ(result, "1.000000 2.500000");
    }

    TEST_F(MduConverterTest, ValueToString_FloatList_ScientificFormat_ReturnsScientificNotation)
    {
        auto schema = MakePropertySchema(ValueType::FloatList, FormatType::Scientific);
        Value v = std::vector<double>{1.0, 2.5};
        auto result = MduConverter::ValueToString(schema, v);

        EXPECT_EQ(result, "1.000000e+00 2.500000e+00");
    }

    // -------------------------------------------------------------------------
    // ValueToString — Enum types
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ValueToString_StringEnum_ReturnsEnumName)
    {
        auto schema = MakeEnumSchema(ValueType::StringEnum, {"None", "Explicit", "Implicit"});
        auto result = MduConverter::ValueToString(schema, Value{StringEnumValue{"Explicit"}});

        EXPECT_EQ(result, "Explicit");
    }

    TEST_F(MduConverterTest, ValueToString_StringEnum_NameCaseInsensitive_ReturnsEnumName)
    {
        auto schema = MakeEnumSchema(ValueType::StringEnum, {"None", "Explicit"});
        auto result = MduConverter::ValueToString(schema, Value{StringEnumValue{"explicit"}});

        EXPECT_EQ(result, "Explicit");
    }

    TEST_F(MduConverterTest, ValueToString_StringEnum_OutOfRange_ThrowsInvalidArgument)
    {
        auto schema = MakeEnumSchema(ValueType::StringEnum, {"None", "Explicit"});

        EXPECT_THROW(MduConverter::ValueToString(schema, Value{StringEnumValue{"Implicit"}}), std::invalid_argument);
    }

    TEST_F(MduConverterTest, ValueToString_IntEnum_ReturnsIntegerString)
    {
        auto schema = MakeEnumSchema(ValueType::IntEnum, {"0", "1", "2"});
        auto result = MduConverter::ValueToString(schema, Value{IntEnumValue{2}});

        EXPECT_EQ(result, "2");
    }

    TEST_F(MduConverterTest, ValueToString_IntEnum_OutOfRange_ThrowsInvalidArgument)
    {
        auto schema = MakeEnumSchema(ValueType::IntEnum, {"0", "1", "2"});

        EXPECT_THROW(MduConverter::ValueToString(schema, Value{IntEnumValue{3}}), std::invalid_argument);
    }

    // -------------------------------------------------------------------------
    // Error handling
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, ValueFromString_InvalidValueType_ThrowsLogicError)
    {
        auto schema = MakePropertySchema(static_cast<ValueType>(9999));

        EXPECT_THROW(MduConverter::ValueFromString(schema, "value"), std::logic_error);
    }

    TEST_F(MduConverterTest, ValueToString_InvalidValueType_ThrowsLogicError)
    {
        auto schema = MakePropertySchema(static_cast<ValueType>(9999));

        EXPECT_THROW(MduConverter::ValueToString(schema, Value{std::string("value")}), std::logic_error);
    }

    // -------------------------------------------------------------------------
    // Round-trip
    // -------------------------------------------------------------------------

    TEST_F(MduConverterTest, RoundTrip_Int)
    {
        const auto schema = MakePropertySchema(ValueType::Int);
        const Value original = 42;

        auto raw = MduConverter::ValueToString(schema, original);
        auto result = MduConverter::ValueFromString(schema, raw);

        EXPECT_EQ(std::get<int>(result), 42);
    }

    TEST_F(MduConverterTest, RoundTrip_Float)
    {
        const auto schema = MakePropertySchema(ValueType::Float);
        const Value original = 1.5;

        auto raw = MduConverter::ValueToString(schema, original);
        auto result = MduConverter::ValueFromString(schema, raw);

        EXPECT_DOUBLE_EQ(std::get<double>(result), 1.5);
    }

    TEST_F(MduConverterTest, RoundTrip_StringEnum)
    {
        auto schema = MakeEnumSchema(ValueType::StringEnum, {"None", "Explicit"});
        const Value original = StringEnumValue{"Explicit"};

        auto raw = MduConverter::ValueToString(schema, original);
        auto result = MduConverter::ValueFromString(schema, raw);

        EXPECT_EQ(std::get<StringEnumValue>(result).value, "Explicit");
    }

    TEST_F(MduConverterTest, RoundTrip_IntEnum)
    {
        auto schema = MakeEnumSchema(ValueType::IntEnum, {"0", "1", "2"});
        const Value original = IntEnumValue{1};

        auto raw = MduConverter::ValueToString(schema, original);
        auto result = MduConverter::ValueFromString(schema, raw);

        EXPECT_EQ(std::get<IntEnumValue>(result).value, 1);
    }

    TEST_F(MduConverterTest, RoundTrip_DateTime_CompactDateTime)
    {
        auto schema = MakePropertySchema(ValueType::DateTime, FormatType::DateTime);
        const auto timePoint = std::chrono::system_clock::time_point{
            std::chrono::sys_days{std::chrono::year{2020} / std::chrono::month{1} / std::chrono::day{30}} +
            std::chrono::hours{12}};
        const Value original = std::optional<std::chrono::system_clock::time_point>{timePoint};

        auto raw = MduConverter::ValueToString(schema, original);
        auto result = MduConverter::ValueFromString(schema, raw);

        const auto& actual = std::get<std::optional<std::chrono::system_clock::time_point>>(result);
        ASSERT_TRUE(actual.has_value());
        EXPECT_EQ(actual.value(), timePoint);
    }

    TEST_F(MduConverterTest, RoundTrip_DateTime_DateFormat)
    {
        auto schema = MakePropertySchema(ValueType::DateTime, FormatType::Date);
        const auto timePoint = std::chrono::system_clock::time_point{
            std::chrono::sys_days{std::chrono::year{2020} / std::chrono::month{1} / std::chrono::day{30}}};
        const Value original = std::optional<std::chrono::system_clock::time_point>{timePoint};

        auto raw = MduConverter::ValueToString(schema, original);
        auto result = MduConverter::ValueFromString(schema, raw);

        const auto& actual = std::get<std::optional<std::chrono::system_clock::time_point>>(result);
        ASSERT_TRUE(actual.has_value());
        EXPECT_EQ(actual.value(), timePoint);
    }

} // namespace dflowfm_io::test