#include <gtest/gtest.h>

#include <dflowfm_io/MduDataConverter.h>
#include <dflowfm_io/MduSchema.h>

#include "MduTestData.h"

namespace dflowfm_io::test
{

    // -------------------------------------------------------------------------
    // Fixture
    // -------------------------------------------------------------------------

    class MduDataConverterTest : public ::testing::Test
    {
    protected:
        static void SetUpTestSuite()
        {
            compliantIniData = std::make_unique<ini::IniData>(MakeCompliantIniData());
            compliantMduData = std::make_unique<MduData>(MakeCompliantMduData());
        }

        static void TearDownTestSuite()
        {
            compliantIniData.reset();
            compliantMduData.reset();
        }

        static ini::IniData CompliantIniData() { return *compliantIniData; }
        static MduData CompliantMduData() { return *compliantMduData; }

        static inline std::unique_ptr<ini::IniData> compliantIniData;
        static inline std::unique_ptr<MduData> compliantMduData;
    };

    // -------------------------------------------------------------------------
    // Convert IniData → MduData — fully compliant input
    // -------------------------------------------------------------------------

    TEST_F(MduDataConverterTest, ConvertIniData_FullyCompliantInput_ReportHasNoErrors)
    {
        const ini::IniData iniData = CompliantIniData();

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        EXPECT_FALSE(report.HasErrors());
    }

    TEST_F(MduDataConverterTest, ConvertIniData_FullyCompliantInput_ReportHasNoWarnings)
    {
        const ini::IniData iniData = CompliantIniData();

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        EXPECT_FALSE(report.HasWarnings());
    }

    TEST_F(MduDataConverterTest, ConvertIniData_FullyCompliantInput_MduDataIsNotEmpty)
    {
        const ini::IniData iniData = CompliantIniData();

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        EXPECT_FALSE(mduData.data_entries.empty());
    }

    // -------------------------------------------------------------------------
    // Convert IniData → MduData — validation issues forwarded to report
    // -------------------------------------------------------------------------

    TEST_F(MduDataConverterTest, ConvertIniData_MissingRequiredProperty_ReportHasError)
    {
        const auto [targetSection, targetProperty] = FirstRequiredProperty();

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).RemoveAllProperties(targetProperty->key);

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        EXPECT_TRUE(report.HasErrors());
        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find(targetSection->name), std::string::npos);
        EXPECT_NE(error->message.find(targetProperty->key), std::string::npos);
    }

    TEST_F(MduDataConverterTest, ConvertIniData_UnknownProperty_ReportHasWarning)
    {
        const auto [targetSection, targetProperty] = FirstRequiredProperty();

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).AddProperty("UnknownProperty_XYZ", "value");

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        EXPECT_TRUE(report.HasWarnings());
        const Issue* warning = FirstIssue(report, Severity::Warning);
        ASSERT_NE(warning, nullptr);
        EXPECT_NE(warning->message.find(targetSection->name), std::string::npos);
        EXPECT_NE(warning->message.find("UnknownProperty_XYZ"), std::string::npos);
    }

    TEST_F(MduDataConverterTest, ConvertIniData_MissingOptionalProperty_ReportHasInfo)
    {
        const auto [targetSection, targetProperty] = FirstOptionalPropertyWithDefault();

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).RemoveAllProperties(targetProperty->key);

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        EXPECT_TRUE(report.HasInfos());
        const Issue* error = FirstIssue(report, Severity::Info);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find(targetSection->name), std::string::npos);
        EXPECT_NE(error->message.find(targetProperty->key), std::string::npos);
    }

    // -------------------------------------------------------------------------
    // Convert IniData → MduData — invalid property value
    // -------------------------------------------------------------------------

    class MduDataConverterInvalidValueTest : public MduDataConverterTest, public ::testing::WithParamInterface<ValueType>
    {
    };

    TEST_P(MduDataConverterInvalidValueTest, ConvertIniData_InvalidValue_ReportHasError)
    {
        const auto [targetSection, targetProperty] = FirstPropertyOfType(GetParam());

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).SetPropertyValue(targetProperty->key, "##invalid##");

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        EXPECT_TRUE(report.HasErrors());
        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find(targetSection->name), std::string::npos);
        EXPECT_NE(error->message.find(targetProperty->key), std::string::npos);
    }

    INSTANTIATE_TEST_SUITE_P(MduDataConverterTest, MduDataConverterInvalidValueTest,
                             // Note: String and Path types are excluded because any raw string
                             // is a valid value for those types.
                             ::testing::Values(ValueType::Int, ValueType::Float, ValueType::IntBool, ValueType::Enum,
                                               ValueType::IntEnum, ValueType::DateTime, ValueType::FloatList));

    // -------------------------------------------------------------------------
    // Convert IniData → MduData — absent property with default falls back to schema default
    // -------------------------------------------------------------------------

    TEST_F(MduDataConverterTest, ConvertIniData_AbsentIntPropertyWithDefault_UsesCorrectDefaultValue)
    {
        const auto [targetSection, targetProperty] = FirstOptionalPropertyWithDefault(ValueType::Int);

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).RemoveAllProperties(targetProperty->key);

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        const std::string key = FormatKey(targetSection->name, targetProperty->key);
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<int>(key), std::stoi(targetProperty->default_value));
    }

    TEST_F(MduDataConverterTest, ConvertIniData_AbsentFloatPropertyWithDefault_UsesCorrectDefaultValue)
    {
        const auto [targetSection, targetProperty] = FirstOptionalPropertyWithDefault(ValueType::Float);

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).RemoveAllProperties(targetProperty->key);

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        const std::string key = FormatKey(targetSection->name, targetProperty->key);
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_DOUBLE_EQ(mduData.getValueAs<double>(key), std::stod(targetProperty->default_value));
    }

    TEST_F(MduDataConverterTest, ConvertIniData_AbsentEnumPropertyWithDefault_UsesCorrectDefaultValue)
    {
        const auto [targetSection, targetProperty] = FirstOptionalPropertyWithDefault(ValueType::Enum);

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).RemoveAllProperties(targetProperty->key);

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        const std::string key = FormatKey(targetSection->name, targetProperty->key);
        const auto it =
            std::find_if(targetProperty->enum_values.begin(), targetProperty->enum_values.end(),
                         [&](const auto& pair) { return pair.second == targetProperty->default_value; });
        ASSERT_NE(it, targetProperty->enum_values.end());
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<EnumValue>(key).value, it->first);
    }

    // -------------------------------------------------------------------------
    // Convert IniData → MduData — valid property values per type
    // -------------------------------------------------------------------------

    TEST_F(MduDataConverterTest, ConvertIniData_ValidStringValue_ConvertsSuccessfully)
    {
        const auto [targetSection, targetProperty] = FirstPropertyOfType(ValueType::String);

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).SetPropertyValue(targetProperty->key, "some_string");

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey(targetSection->name, targetProperty->key);
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<std::string>(key), "some_string");
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidIntValue_ConvertsSuccessfully)
    {
        const auto [targetSection, targetProperty] = FirstPropertyOfType(ValueType::Int);

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).SetPropertyValue(targetProperty->key, "42");

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey(targetSection->name, targetProperty->key);
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<int>(key), 42);
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidFloatValue_ConvertsSuccessfully)
    {
        const auto [targetSection, targetProperty] = FirstPropertyOfType(ValueType::Float);

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).SetPropertyValue(targetProperty->key, "3.14");

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey(targetSection->name, targetProperty->key);
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_DOUBLE_EQ(mduData.getValueAs<double>(key), 3.14);
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidIntBoolValue_ConvertsSuccessfully)
    {
        const auto [targetSection, targetProperty] = FirstPropertyOfType(ValueType::IntBool);

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).SetPropertyValue(targetProperty->key, "1");

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey(targetSection->name, targetProperty->key);
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_TRUE(mduData.getValueAs<bool>(key));
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidPathValue_ConvertsSuccessfully)
    {
        const auto [targetSection, targetProperty] = FirstPropertyOfType(ValueType::Path);

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).SetPropertyValue(targetProperty->key, "some/path/file.nc");

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey(targetSection->name, targetProperty->key);
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<std::filesystem::path>(key), std::filesystem::path("some/path/file.nc"));
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidEnumValue_ConvertsSuccessfully)
    {
        const auto [targetSection, targetProperty] = FirstPropertyOfType(ValueType::Enum);

        ini::IniData iniData = CompliantIniData();
        const auto& [number, name] = *targetProperty->enum_values.begin();
        iniData.GetSection(targetSection->name).SetPropertyValue(targetProperty->key, name);

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey(targetSection->name, targetProperty->key);
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<EnumValue>(key).value, number);
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidIntEnumValue_ConvertsSuccessfully)
    {
        const auto [targetSection, targetProperty] = FirstPropertyOfType(ValueType::IntEnum);

        ini::IniData iniData = CompliantIniData();
        const auto& [number, name] = *targetProperty->enum_values.begin();
        iniData.GetSection(targetSection->name).SetPropertyValue(targetProperty->key, std::to_string(number));

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey(targetSection->name, targetProperty->key);
        EXPECT_TRUE(mduData.hasValue(key));
        EXPECT_EQ(mduData.getValueAs<EnumValue>(key).value, number);
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidPathListValue_ConvertsSuccessfully)
    {
        const auto [targetSection, targetProperty] = FirstPropertyOfType(ValueType::PathList);

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).SetPropertyValue(targetProperty->key, "path/a.nc path/b.nc path/c.nc");

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey(targetSection->name, targetProperty->key);
        EXPECT_TRUE(mduData.hasValue(key));
        const auto& paths = mduData.getValueAs<std::vector<std::filesystem::path>>(key);
        ASSERT_EQ(paths.size(), 3);
        EXPECT_EQ(paths[0], std::filesystem::path("path/a.nc"));
        EXPECT_EQ(paths[1], std::filesystem::path("path/b.nc"));
        EXPECT_EQ(paths[2], std::filesystem::path("path/c.nc"));
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidFloatListValue_ConvertsSuccessfully)
    {
        const auto [targetSection, targetProperty] = FirstPropertyOfType(ValueType::FloatList);

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).SetPropertyValue(targetProperty->key, "1.0 2.0 3.0");

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey(targetSection->name, targetProperty->key);
        EXPECT_TRUE(mduData.hasValue(key));
        const auto& values = mduData.getValueAs<std::vector<double>>(key);
        ASSERT_EQ(values.size(), 3);
        EXPECT_DOUBLE_EQ(values[0], 1.0);
        EXPECT_DOUBLE_EQ(values[1], 2.0);
        EXPECT_DOUBLE_EQ(values[2], 3.0);
    }

    TEST_F(MduDataConverterTest, ConvertIniData_ValidDateTimeValue_ConvertsSuccessfully)
    {
        const auto [targetSection, targetProperty] = FirstPropertyOfType(ValueType::DateTime);

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).SetPropertyValue(targetProperty->key, "20230115120000");

        const auto [mduData, report] = MduDataConverter::Convert(iniData);

        EXPECT_FALSE(report.HasErrors());
        const std::string key = FormatKey(targetSection->name, targetProperty->key);
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
        const MduData mduData = CompliantMduData();

        const ini::IniData iniData = MduDataConverter::Convert(mduData);

        EXPECT_FALSE(iniData.empty());
    }

    TEST_F(MduDataConverterTest, ConvertMduData_FullyCompliantInput_AllPropertiesPresentInIniData)
    {
        const MduData mduData = CompliantMduData();

        const ini::IniData iniData = MduDataConverter::Convert(mduData);

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
        const auto [targetSection, targetProperty] = FirstOptionalPropertyWithDefault();

        MduData mduData = CompliantMduData();
        const std::string key = FormatKey(targetSection->name, targetProperty->key);
        mduData.data_entries.erase(key);

        const ini::IniData iniData = MduDataConverter::Convert(mduData);

        ASSERT_TRUE(iniData.HasSection(targetSection->name));
        EXPECT_FALSE(iniData.GetSection(targetSection->name).HasProperty(targetProperty->key))
            << "Property should be omitted when no value exists in MduData: " << targetSection->name << "."
            << targetProperty->key;
    }

    TEST_F(MduDataConverterTest, ConvertMduData_FullyCompliantInput_PropertiesInSchemaOrder)
    {
        const MduData mduData = CompliantMduData();

        const ini::IniData iniData = MduDataConverter::Convert(mduData);

        std::size_t previousIndex = 0;
        for (const auto& section : iniData)
        {
            const auto schemaIt = std::find_if(MDU_SCHEMA.sections.begin(), MDU_SCHEMA.sections.end(),
                                               [&](const auto& s) { return s.name == section.GetName(); });
            ASSERT_NE(schemaIt, MDU_SCHEMA.sections.end());
            const std::size_t schemaIndex = std::distance(MDU_SCHEMA.sections.begin(), schemaIt);
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
        const MduData mduData = CompliantMduData();

        const ini::IniData iniData = MduDataConverter::Convert(mduData);

        ASSERT_FALSE(iniData.empty());
        const auto& firstSection = *iniData.begin();
        EXPECT_FALSE(firstSection.GetComments().empty());
    }

    TEST_F(MduDataConverterTest, ConvertMduData_FullyCompliantInput_NonFirstSectionsHaveNoCommentBlock)
    {
        const MduData mduData = CompliantMduData();

        const ini::IniData iniData = MduDataConverter::Convert(mduData);

        ASSERT_GT(std::distance(iniData.begin(), iniData.end()), 1);
        for (auto it = std::next(iniData.begin()); it != iniData.end(); ++it)
            EXPECT_TRUE(it->GetComments().empty()) << "Unexpected comment block on section: " << it->GetName();
    }

    TEST_F(MduDataConverterTest, ConvertMduData_FullyCompliantInput_AllPropertiesHaveComment)
    {
        const MduData mduData = CompliantMduData();

        const ini::IniData iniData = MduDataConverter::Convert(mduData);

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
        ini::IniData original = CompliantIniData();

        const auto [mduData, report] = MduDataConverter::Convert(original);
        ASSERT_FALSE(report.HasErrors());

        const ini::IniData roundTripped = MduDataConverter::Convert(mduData);

        for (const auto& sectionSchema : MDU_SCHEMA.sections)
        {
            if (!CompliantIniData().HasSection(sectionSchema.name)) continue;

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