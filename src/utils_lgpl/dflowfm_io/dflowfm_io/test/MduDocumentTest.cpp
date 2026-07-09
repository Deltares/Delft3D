#include <gtest/gtest.h>

#include <dflowfm_io/MduDocument.h>
#include <dflowfm_io/StringUtils.h>

#include "MduTestData.h"

#include <filesystem>
#include <limits>
#include <sstream>

namespace dflowfm_io::test
{

    // -------------------------------------------------------------------------
    // Constructor
    // -------------------------------------------------------------------------

    TEST(MduDocumentTest, Constructor_PopulatesSchemaDefaults)
    {
        MduDocument doc;
        EXPECT_FALSE(doc.GetData().data_entries.empty());
    }

    // -------------------------------------------------------------------------
    // Load(stream)
    // -------------------------------------------------------------------------

    TEST(MduDocumentTest, Load_ValidStream_NoErrors)
    {
        const auto mdu_str = MakeCompliantMduString();
        auto stream = std::istringstream(mdu_str);

        MduDocument doc;
        doc.Load(stream);

        EXPECT_FALSE(doc.GetReport().HasErrors());
    }

    TEST(MduDocumentTest, Load_ValidStream_PopulatesMduData)
    {
        const auto mdu_str = MakeCompliantMduString();
        auto stream = std::istringstream(mdu_str);

        MduDocument doc;
        doc.Load(stream);

        EXPECT_FALSE(doc.GetData().data_entries.empty());
    }

    TEST(MduDocumentTest_Stream, Load_FailedStream_ThrowsIosBaseFailure)
    {
        std::istringstream stream;
        stream.setstate(std::ios::failbit);
        MduDocument doc;

        EXPECT_THROW(doc.Load(stream), std::ios_base::failure);
    }

    TEST(MduDocumentTest, Load_ValidStream_OverridesDefaultValues)
    {
        const auto [intSection, intProperty] = FirstOptionalPropertyWithDefault(ValueType::Int);
        const auto [floatSection, floatProperty] = FirstOptionalPropertyWithDefault(ValueType::Float);
        const auto [stringSection, stringProperty] = FirstOptionalPropertyWithDefault(ValueType::String);

        auto iniData = MakeCompliantIniData();
        iniData.GetSection(intSection->name).SetPropertyValue(intProperty->key, -1212);
        iniData.GetSection(stringSection->name).SetPropertyValue(stringProperty->key, "overriden");
        iniData.GetSection(floatSection->name).SetPropertyValue(floatProperty->key, 32424.22);

        std::ostringstream out;
        ini::IniFormatter{}.Format(iniData, out);
        std::istringstream stream = std::istringstream(out.str());

        MduDocument doc;
        doc.Load(stream);

        EXPECT_EQ(doc.GetValue<int>(FormatKey(intSection->name, intProperty->key)), -1212);
        EXPECT_EQ(doc.GetValue<std::string>(FormatKey(stringSection->name, stringProperty->key)), "overriden");
        EXPECT_DOUBLE_EQ(doc.GetValue<double>(FormatKey(floatSection->name, floatProperty->key)), 32424.22);
    }

    TEST(MduDocumentTest, Load_ValidStream_RetainsDefaultForAbsentProperty)
    {
        const auto [intSection, intProperty] = FirstOptionalPropertyWithDefault(ValueType::Int);
        const auto [floatSection, floatProperty] = FirstOptionalPropertyWithDefault(ValueType::Float);
        const auto [stringSection, stringProperty] = FirstOptionalPropertyWithDefault(ValueType::String);

        auto iniData = MakeCompliantIniData();
        iniData.GetSection(intSection->name).RemoveAllProperties(intProperty->key);
        iniData.GetSection(stringSection->name).RemoveAllProperties(stringProperty->key);
        iniData.GetSection(floatSection->name).RemoveAllProperties(floatProperty->key);

        std::ostringstream out;
        ini::IniFormatter{}.Format(iniData, out);
        std::istringstream stream = std::istringstream(out.str());

        MduDocument doc;
        doc.Load(stream);

        EXPECT_EQ(doc.GetValue<int>(FormatKey(intSection->name, intProperty->key)),
                  std::stoi(intProperty->default_value));
        EXPECT_EQ(doc.GetValue<std::string>(FormatKey(stringSection->name, stringProperty->key)),
                  stringProperty->default_value);
        EXPECT_DOUBLE_EQ(doc.GetValue<double>(FormatKey(floatSection->name, floatProperty->key)),
                         std::stof(floatProperty->default_value));
    }

    // -------------------------------------------------------------------------
    // Load(path)
    // -------------------------------------------------------------------------

    TEST(MduDocumentTest_Path, Load_EmptyPath_ThrowsInvalidArgument)
    {
        MduDocument doc;
        EXPECT_THROW(doc.Load(std::filesystem::path{}), std::invalid_argument);
    }

    TEST(MduDocumentTest_Path, Load_NonExistingPath_ThrowsIosBaseFailure)
    {
        MduDocument doc;
        EXPECT_THROW(doc.Load(std::filesystem::path{"nonexistent_file_xyz.mdu"}), std::ios_base::failure);
    }

    // -------------------------------------------------------------------------
    // Save(stream)
    // -------------------------------------------------------------------------

    TEST(MduDocumentTest, Save_ValidStream_WritesNonEmptyContent)
    {
        const auto mdu_str = MakeCompliantMduString();
        auto stream = std::istringstream(mdu_str);

        MduDocument doc;
        doc.Load(stream);

        std::ostringstream out;
        doc.Save(out);

        EXPECT_FALSE(out.str().empty());
    }

    TEST(MduDocumentTest, Save_FailedStream_ThrowsIosBaseFailure)
    {
        MduDocument doc;

        std::ostringstream out;
        out.setstate(std::ios::failbit);

        EXPECT_THROW(doc.Save(out), std::ios_base::failure);
    }

    // -------------------------------------------------------------------------
    // Save(path)
    // -------------------------------------------------------------------------

    TEST(MduDocumentTest_Path, Save_EmptyPath_ThrowsInvalidArgument)
    {
        MduDocument doc;
        EXPECT_THROW(doc.Save(std::filesystem::path{}), std::invalid_argument);
    }

    // -------------------------------------------------------------------------
    // GetValue / SetValue
    // -------------------------------------------------------------------------

    TEST(MduDocumentTest, GetValue_UnknownKey_ThrowsInvalidArgument)
    {
        MduDocument doc;
        EXPECT_THROW(doc.GetValue<int>("general.nonexistent_xyz"), std::invalid_argument);
    }

    TEST(MduDocumentTest, GetValue_ExistingIntProperty_ReturnsDefaultValue)
    {
        MduDocument doc;

        const auto [targetSection, targetProperty] = FirstOptionalPropertyWithDefault(ValueType::Int);
        const std::string key = FormatKey(targetSection->name, targetProperty->key);

        EXPECT_EQ(doc.GetValue<int>(key), std::stoi(targetProperty->default_value));
    }

    TEST(MduDocumentTest, GetValue_ExistingFloatProperty_ReturnsDefaultValue)
    {
        MduDocument doc;

        const auto [targetSection, targetProperty] = FirstOptionalPropertyWithDefault(ValueType::Float);
        const std::string key = FormatKey(targetSection->name, targetProperty->key);

        EXPECT_DOUBLE_EQ(doc.GetValue<double>(key), std::stof(targetProperty->default_value));
    }

    TEST(MduDocumentTest, GetValue_ExistingStringProperty_ReturnsDefaultValue)
    {
        MduDocument doc;

        const auto [targetSection, targetProperty] = FirstOptionalPropertyWithDefault(ValueType::String);
        const std::string key = FormatKey(targetSection->name, targetProperty->key);

        EXPECT_EQ(doc.GetValue<std::string>(key), targetProperty->default_value);
    }

    TEST(MduDocumentTest, SetValue_UnknownKey_ThrowsInvalidArgument)
    {
        MduDocument doc;
        EXPECT_THROW(doc.SetValue("general.nonexistent_xyz", 42), std::invalid_argument);
    }

    TEST(MduDocumentTest, SetValue_EnumOutOfRange_ThrowsOutOfRange)
    {
        MduDocument doc;

        const auto [targetSection, targetProperty] = FirstPropertyOfType(ValueType::Enum);
        const std::string key = FormatKey(targetSection->name, targetProperty->key);

        EXPECT_THROW(doc.SetValue(key, EnumValue{std::numeric_limits<int>::max()}), std::out_of_range);
    }

    TEST(MduDocumentTest, SetValue_ValidIntValue_UpdatesData)
    {
        MduDocument doc;

        const auto [targetSection, targetProperty] = FirstPropertyOfType(ValueType::Int);
        const std::string key = FormatKey(targetSection->name, targetProperty->key);

        doc.SetValue(key, 99);

        EXPECT_EQ(doc.GetValue<int>(key), 99);
    }

} // namespace dflowfm_io::test