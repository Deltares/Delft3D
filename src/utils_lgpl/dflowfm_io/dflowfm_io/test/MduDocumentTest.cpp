#include <gtest/gtest.h>

#include <dflowfm_io/MduDocument.h>
#include <dflowfm_io/MduSchema.h>

#include "MduTestData.h"

#include <chrono>
#include <filesystem>
#include <limits>
#include <ios>
#include <utility>

namespace dflowfm_io::test
{

    // -------------------------------------------------------------------------
    // Fixture
    // -------------------------------------------------------------------------

    class MduDocumentTest : public ::testing::Test
    {
    protected:
        const MduSchema& schema = TestSchema();
        MduDocument doc{schema};
        const std::filesystem::path path = std::filesystem::temp_directory_path() /
            ("MduDocumentTest_" + std::to_string(std::chrono::steady_clock::now().time_since_epoch().count()) + ".mdu");

        void TearDown() override { std::filesystem::remove(path); }
    };

    // -------------------------------------------------------------------------
    // Constructor
    // -------------------------------------------------------------------------

    TEST_F(MduDocumentTest, Constructor_PopulatesSchemaDefaults)
    {
        EXPECT_EQ(doc.GetValue<std::string>("general.fileVersion"), "1.09");
        EXPECT_EQ(doc.GetValue<int>("numerics.maxNonLinearIterations"), 100);
        EXPECT_DOUBLE_EQ(doc.GetValue<double>("geometry.bedLevUni"), -5.0);
    }

    // -------------------------------------------------------------------------
    // LoadFromString
    // -------------------------------------------------------------------------

    TEST_F(MduDocumentTest, LoadFromString_ValidString_NoErrors)
    {
        const IssueReport report = doc.LoadFromString(TestMduString());

        EXPECT_FALSE(HasIssue(report, Severity::Error));
    }

    TEST_F(MduDocumentTest, LoadFromString_ValidString_PopulatesValues)
    {
        doc.LoadFromString(TestMduString());

        EXPECT_EQ(doc.GetValue<std::string>("general.fileVersion"), "1.09");
        EXPECT_EQ(doc.GetValue<int>("numerics.maxNonLinearIterations"), 100);
        EXPECT_DOUBLE_EQ(doc.GetValue<double>("geometry.bedLevUni"), -5.0);
    }

    TEST_F(MduDocumentTest, LoadFromString_EmptyString_ReportsErrorsAndRetainsSchemaDefaults)
    {
        const IssueReport report = doc.LoadFromString("");
        EXPECT_TRUE(HasIssue(report, Severity::Error));
        EXPECT_EQ(doc.GetValue<int>("numerics.maxNonLinearIterations"), 100);
    }

    TEST_F(MduDocumentTest, LoadFromString_ValidString_OverridesDefaultValues)
    {
        auto iniData = TestIniData();
        iniData.GetSection("general").SetPropertyValue("fileVersion", "overriden");
        iniData.GetSection("numerics").SetPropertyValue("maxNonLinearIterations", -1212);
        iniData.GetSection("geometry").SetPropertyValue("bedLevUni", 32424.22);

        doc.LoadFromString(ini::Format(iniData));

        const std::string fileVersionKey = FormatKey("general", "fileVersion");
        const std::string maxIterKey = FormatKey("numerics", "maxNonLinearIterations");
        const std::string bedLevKey = FormatKey("geometry", "bedLevUni");

        EXPECT_EQ(doc.GetValue<std::string>(fileVersionKey), "overriden");
        EXPECT_EQ(doc.GetValue<int>(maxIterKey), -1212);
        EXPECT_DOUBLE_EQ(doc.GetValue<double>(bedLevKey), 32424.22);
    }

    TEST_F(MduDocumentTest, LoadFromString_ValidString_RetainsDefaultForAbsentProperty)
    {
        auto iniData = TestIniData();
        iniData.GetSection("general").RemoveAllProperties("fileVersion");
        iniData.GetSection("numerics").RemoveAllProperties("maxNonLinearIterations");
        iniData.GetSection("geometry").RemoveAllProperties("bedLevUni");

        doc.LoadFromString(ini::Format(iniData));

        const std::string fileVersionKey = FormatKey("general", "fileVersion");
        const std::string maxIterKey = FormatKey("numerics", "maxNonLinearIterations");
        const std::string bedLevKey = FormatKey("geometry", "bedLevUni");

        EXPECT_EQ(doc.GetValue<std::string>(fileVersionKey), "1.09");
        EXPECT_EQ(doc.GetValue<int>(maxIterKey), 100);
        EXPECT_DOUBLE_EQ(doc.GetValue<double>(bedLevKey), -5.0);
    }

    // -------------------------------------------------------------------------
    // LoadFromFile
    // -------------------------------------------------------------------------

    TEST_F(MduDocumentTest, LoadFromFile_EmptyPath_ThrowsInvalidArgument)
    {
        EXPECT_THROW(doc.LoadFromFile(std::filesystem::path{}), std::invalid_argument);
    }

    TEST_F(MduDocumentTest, LoadFromFile_NonExistingPath_ThrowsIosBaseFailure)
    {
        const auto path = std::filesystem::path{"nonexistent_file_xyz.mdu"};

        EXPECT_THROW(doc.LoadFromFile(path), std::ios_base::failure);
    }

    // -------------------------------------------------------------------------
    // SaveToString
    // -------------------------------------------------------------------------

    TEST_F(MduDocumentTest, SaveToString_LoadedData_WritesNonEmptyContent)
    {
        doc.LoadFromString(TestMduString());

        EXPECT_FALSE(doc.SaveToString().empty());
    }

    TEST_F(MduDocumentTest, SaveToStringThenLoadFromString_PreservesValues)
    {
        doc.LoadFromString(TestMduString());
        doc.SetValue("numerics.maxNonLinearIterations", 42);
        doc.SetValue("geometry.bedLevUni", -12.5);

        MduDocument reader{schema};
        const IssueReport report = reader.LoadFromString(doc.SaveToString());

        EXPECT_FALSE(HasIssue(report, Severity::Error));
        EXPECT_EQ(reader.GetValue<int>("numerics.maxNonLinearIterations"), 42);
        EXPECT_DOUBLE_EQ(reader.GetValue<double>("geometry.bedLevUni"), -12.5);
    }

    // -------------------------------------------------------------------------
    // SaveToFile
    // -------------------------------------------------------------------------

    TEST_F(MduDocumentTest, SaveToFile_EmptyPath_ThrowsInvalidArgument)
    {
        EXPECT_THROW(doc.SaveToFile(std::filesystem::path{}), std::invalid_argument);
    }

    TEST_F(MduDocumentTest, SaveToFileThenLoadFromFile_FileContent_MatchesStringRoundTrip)
    {
        doc.LoadFromString(TestMduString());
        doc.SetValue("numerics.maxNonLinearIterations", 42);
        doc.SaveToFile(path);

        MduDocument reader{schema};
        const IssueReport report = reader.LoadFromFile(path);

        EXPECT_FALSE(HasIssue(report, Severity::Error));
        EXPECT_EQ(reader.SaveToString(), doc.SaveToString());
        EXPECT_EQ(reader.GetValue<int>("numerics.maxNonLinearIterations"), 42);
    }

    // -------------------------------------------------------------------------
    // GetValue
    // -------------------------------------------------------------------------

    TEST_F(MduDocumentTest, GetValue_UnknownKey_ThrowsInvalidArgument)
    {
        EXPECT_THROW(doc.GetValue<int>("general.nonexistent_xyz"), std::invalid_argument);
    }

    TEST_F(MduDocumentTest, GetValue_ExistingIntProperty_ReturnsDefaultValue)
    {
        const std::string key = FormatKey("numerics", "maxNonLinearIterations");

        EXPECT_EQ(doc.GetValue<int>(key), 100);
    }

    TEST_F(MduDocumentTest, GetValue_ExistingFloatProperty_ReturnsDefaultValue)
    {
        const std::string key = FormatKey("geometry", "bedLevUni");

        EXPECT_DOUBLE_EQ(doc.GetValue<double>(key), -5.0);
    }

    TEST_F(MduDocumentTest, GetValue_ExistingStringProperty_ReturnsDefaultValue)
    {
        const std::string key = FormatKey("general", "fileVersion");

        EXPECT_EQ(doc.GetValue<std::string>(key), "1.09");
    }

    TEST_F(MduDocumentTest, GetValue_ExistingStringEnumProperty_ReturnsDefaultValue)
    {
        const std::string key = FormatKey("numerics", "verticalAdvectionType");

        EXPECT_EQ(doc.GetValue<StringEnumValue>(key).value, "higherOrderUpwindExplicit");
    }

    TEST_F(MduDocumentTest, GetValue_ExistingIntEnumProperty_ReturnsDefaultValue)
    {
        const std::string key = FormatKey("numerics", "vertAdvTypSal");

        EXPECT_EQ(doc.GetValue<IntEnumValue>(key).value, 6);
    }

    TEST_F(MduDocumentTest, GetValue_ExistingBoolProperty_ReturnsDefaultValue)
    {
        const std::string key = FormatKey("geometry", "useCaching");

        EXPECT_TRUE(doc.GetValue<bool>(key));
    }

    TEST_F(MduDocumentTest, GetValue_ExistingPathProperty_ReturnsDefaultValue)
    {
        const std::string key = FormatKey("geometry", "netFile");

        EXPECT_EQ(doc.GetValue<std::filesystem::path>(key), std::filesystem::path("test_net.nc"));
    }

    TEST_F(MduDocumentTest, GetValue_ExistingPathListProperty_ReturnsDefaultValue)
    {
        const std::string key = FormatKey("geometry", "structureFile");

        const auto& value = doc.GetValue<std::vector<std::filesystem::path>>(key);

        ASSERT_EQ(value.size(), 1u);
        EXPECT_EQ(value[0], std::filesystem::path("structures.ini"));
    }

    TEST_F(MduDocumentTest, GetValue_ExistingStringListProperty_ReturnsDefaultValue)
    {
        const std::string key = FormatKey("geometry", "activeProcesses");

        const auto& value = doc.GetValue<std::vector<std::string>>(key);

        ASSERT_EQ(value.size(), 3u);
        EXPECT_EQ(value[0], "Nitrification");
        EXPECT_EQ(value[1], "Denitrification");
        EXPECT_EQ(value[2], "Reaeration");
    }

    TEST_F(MduDocumentTest, GetValue_ExistingFloatListProperty_ReturnsDefaultValue)
    {
        const std::string key = FormatKey("geometry", "stretchCoef");

        const auto& value = doc.GetValue<std::vector<double>>(key);

        ASSERT_EQ(value.size(), 3u);
        EXPECT_DOUBLE_EQ(value[0], 0.1);
        EXPECT_DOUBLE_EQ(value[1], 0.3);
        EXPECT_DOUBLE_EQ(value[2], 0.6);
    }

    TEST_F(MduDocumentTest, GetValue_ExistingDateTimeProperty_ReturnsDefaultValue)
    {
        const std::string key = FormatKey("time", "refDate");

        const auto& value = doc.GetValue<std::optional<std::chrono::system_clock::time_point>>(key);

        const auto expected = std::chrono::system_clock::time_point{
            std::chrono::sys_days{std::chrono::year{2000} / std::chrono::month{1} / std::chrono::day{1}}};
        ASSERT_TRUE(value.has_value());
        EXPECT_EQ(value.value(), expected);
    }

    TEST_F(MduDocumentTest, GetValue_DateTimePropertyWithoutDefault_ReturnsNullopt)
    {
        const std::string key = FormatKey("time", "startDateTime");

        const auto& value = doc.GetValue<std::optional<std::chrono::system_clock::time_point>>(key);

        EXPECT_FALSE(value.has_value());
    }
    // -------------------------------------------------------------------------
    // SetValue
    // -------------------------------------------------------------------------

    TEST_F(MduDocumentTest, SetValue_UnknownKey_ThrowsInvalidArgument)
    {
        EXPECT_THROW(doc.SetValue("general.nonexistent_xyz", 42), std::invalid_argument);
    }

    TEST_F(MduDocumentTest, SetValue_StringEnumOutOfRange_ThrowsOutOfRange)
    {
        const std::string key = FormatKey("general", "fileType");
        const StringEnumValue outOfRange{"invalid"};

        EXPECT_THROW(doc.SetValue(key, outOfRange), std::out_of_range);
    }

    TEST_F(MduDocumentTest, SetValue_IntEnumOutOfRange_ThrowsOutOfRange)
    {
        const std::string key = FormatKey("geometry", "layerType");
        const IntEnumValue outOfRange{-1};

        EXPECT_THROW(doc.SetValue(key, outOfRange), std::out_of_range);
    }

    TEST_F(MduDocumentTest, SetValue_IntEnumWrappedInValueOutOfRange_ThrowsOutOfRange)
    {
        const std::string key = FormatKey("geometry", "layerType");

        EXPECT_THROW(doc.SetValue(key, Value{IntEnumValue{-1}}), std::out_of_range);
    }

    TEST_F(MduDocumentTest, SetValue_StringEnumWrappedInValueOutOfRange_ThrowsOutOfRange)
    {
        const std::string key = FormatKey("general", "fileType");

        EXPECT_THROW(doc.SetValue(key, Value{StringEnumValue{"invalid"}}), std::out_of_range);
    }

    TEST_F(MduDocumentTest, SetValue_ValidIntEnumWrappedInValue_UpdatesData)
    {
        const std::string key = FormatKey("geometry", "layerType");

        doc.SetValue(key, Value{IntEnumValue{2}});

        EXPECT_EQ(doc.GetValue<IntEnumValue>(key).value, 2);
    }

    TEST_F(MduDocumentTest, SetValue_ValidIntValue_UpdatesData)
    {
        const std::string key = FormatKey("numerics", "maxNonLinearIterations");

        doc.SetValue(key, 99);

        EXPECT_EQ(doc.GetValue<int>(key), 99);
    }

    TEST_F(MduDocumentTest, SetValue_ValidFloatValue_UpdatesData)
    {
        const std::string key = FormatKey("geometry", "bedLevUni");

        doc.SetValue(key, -20.0);

        EXPECT_DOUBLE_EQ(doc.GetValue<double>(key), -20.0);
    }

    TEST_F(MduDocumentTest, SetValue_ValidStringValue_UpdatesData)
    {
        const std::string key = FormatKey("general", "fileVersion");

        doc.SetValue(key, std::string("1.10"));

        EXPECT_EQ(doc.GetValue<std::string>(key), "1.10");
    }

    TEST_F(MduDocumentTest, SetValue_ValidStringEnumValue_UpdatesData)
    {
        const std::string key = FormatKey("numerics", "verticalAdvectionType");

        doc.SetValue(key, StringEnumValue{"centralImplicit"});

        EXPECT_EQ(doc.GetValue<StringEnumValue>(key).value, "centralImplicit");
    }

    TEST_F(MduDocumentTest, SetValue_ValidIntEnumValue_UpdatesData)
    {
        const std::string key = FormatKey("numerics", "vertAdvTypSal");

        doc.SetValue(key, IntEnumValue{4});

        EXPECT_EQ(doc.GetValue<IntEnumValue>(key).value, 4);
    }

    TEST_F(MduDocumentTest, SetValue_ValidBoolValue_UpdatesData)
    {
        const std::string key = FormatKey("geometry", "useCaching");

        doc.SetValue(key, false);

        EXPECT_FALSE(doc.GetValue<bool>(key));
    }

    TEST_F(MduDocumentTest, SetValue_ValidPathValue_UpdatesData)
    {
        const std::string key = FormatKey("geometry", "netFile");

        doc.SetValue(key, std::filesystem::path("other_net.nc"));

        EXPECT_EQ(doc.GetValue<std::filesystem::path>(key), std::filesystem::path("other_net.nc"));
    }

    TEST_F(MduDocumentTest, SetValue_ValidPathListValue_UpdatesData)
    {
        const std::string key = FormatKey("geometry", "structureFile");
        std::vector<std::filesystem::path> newValue{"a.ini", "b.ini"};

        doc.SetValue(key, std::move(newValue));

        const auto& value = doc.GetValue<std::vector<std::filesystem::path>>(key);
        ASSERT_EQ(value.size(), 2u);
        EXPECT_EQ(value[0], std::filesystem::path("a.ini"));
        EXPECT_EQ(value[1], std::filesystem::path("b.ini"));
    }

    TEST_F(MduDocumentTest, SetValue_ValidStringListValue_UpdatesData)
    {
        const std::string key = FormatKey("geometry", "activeProcesses");
        std::vector<std::string> newValue{"Foo", "Bar"};

        doc.SetValue(key, std::move(newValue));

        const auto& value = doc.GetValue<std::vector<std::string>>(key);
        ASSERT_EQ(value.size(), 2u);
        EXPECT_EQ(value[0], "Foo");
        EXPECT_EQ(value[1], "Bar");
    }

    TEST_F(MduDocumentTest, SetValue_ValidFloatListValue_UpdatesData)
    {
        const std::string key = FormatKey("geometry", "stretchCoef");
        std::vector<double> newValue{0.2, 0.3, 0.5};

        doc.SetValue(key, std::move(newValue));

        const auto& value = doc.GetValue<std::vector<double>>(key);
        ASSERT_EQ(value.size(), 3u);
        EXPECT_DOUBLE_EQ(value[0], 0.2);
        EXPECT_DOUBLE_EQ(value[1], 0.3);
        EXPECT_DOUBLE_EQ(value[2], 0.5);
    }

    TEST_F(MduDocumentTest, SetValue_ValidDateTimeValue_UpdatesData)
    {
        const std::string key = FormatKey("time", "refDate");
        const auto newValue = std::chrono::system_clock::now();

        doc.SetValue(key, newValue);

        const auto& actual = doc.GetValue<std::optional<std::chrono::system_clock::time_point>>(key);
        ASSERT_TRUE(actual.has_value());
        EXPECT_EQ(actual.value(), newValue);
    }

    TEST_F(MduDocumentTest, SetValue_NulloptDateTimeValue_ClearsData)
    {
        const std::string key = FormatKey("time", "refDate");

        doc.SetValue(key, std::optional<std::chrono::system_clock::time_point>{std::nullopt});

        const auto& actual = doc.GetValue<std::optional<std::chrono::system_clock::time_point>>(key);
        EXPECT_FALSE(actual.has_value());
    }

} // namespace dflowfm_io::test