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
    // Fixture
    // -------------------------------------------------------------------------

    class MduDocumentTest : public ::testing::Test
    {
    protected:
        const MduSchema& schema = TestSchema();
        MduDocument doc{schema};
    };

    // -------------------------------------------------------------------------
    // Constructor
    // -------------------------------------------------------------------------

    TEST_F(MduDocumentTest, Constructor_PopulatesSchemaDefaults) { EXPECT_FALSE(doc.GetData().data_entries.empty()); }

    // -------------------------------------------------------------------------
    // Load(stream)
    // -------------------------------------------------------------------------

    TEST_F(MduDocumentTest, Load_ValidStream_NoErrors)
    {
        auto stream = std::istringstream(TestMduString());

        doc.Load(stream);

        EXPECT_FALSE(doc.GetReport().HasErrors());
    }

    TEST_F(MduDocumentTest, Load_ValidStream_PopulatesMduData)
    {
        auto stream = std::istringstream(TestMduString());

        doc.Load(stream);

        EXPECT_FALSE(doc.GetData().data_entries.empty());
    }

    TEST_F(MduDocumentTest, Load_FailedStream_ThrowsIosBaseFailure)
    {
        std::istringstream stream;
        stream.setstate(std::ios::failbit);

        EXPECT_THROW(doc.Load(stream), std::ios_base::failure);
    }

    TEST_F(MduDocumentTest, Load_ValidStream_OverridesDefaultValues)
    {
        auto iniData = TestIniData();
        iniData.GetSection("general").SetPropertyValue("fileVersion", "overriden");
        iniData.GetSection("numerics").SetPropertyValue("maxNonLinearIterations", -1212);
        iniData.GetSection("geometry").SetPropertyValue("bedLevUni", 32424.22);

        std::ostringstream out;
        ini::IniFormatter{}.Format(iniData, out);
        auto stream = std::istringstream(out.str());

        doc.Load(stream);

        const std::string fileVersionKey = FormatKey("general", "fileVersion");
        const std::string maxIterKey = FormatKey("numerics", "maxNonLinearIterations");
        const std::string bedLevKey = FormatKey("geometry", "bedLevUni");

        EXPECT_EQ(doc.GetValue<std::string>(fileVersionKey), "overriden");
        EXPECT_EQ(doc.GetValue<int>(maxIterKey), -1212);
        EXPECT_DOUBLE_EQ(doc.GetValue<double>(bedLevKey), 32424.22);
    }

    TEST_F(MduDocumentTest, Load_ValidStream_RetainsDefaultForAbsentProperty)
    {
        auto iniData = TestIniData();
        iniData.GetSection("general").RemoveAllProperties("fileVersion");
        iniData.GetSection("numerics").RemoveAllProperties("maxNonLinearIterations");
        iniData.GetSection("geometry").RemoveAllProperties("bedLevUni");

        std::ostringstream out;
        ini::IniFormatter{}.Format(iniData, out);
        auto stream = std::istringstream(out.str());

        doc.Load(stream);

        const std::string fileVersionKey = FormatKey("general", "fileVersion");
        const std::string maxIterKey = FormatKey("numerics", "maxNonLinearIterations");
        const std::string bedLevKey = FormatKey("geometry", "bedLevUni");

        EXPECT_EQ(doc.GetValue<std::string>(fileVersionKey), "1.09");
        EXPECT_EQ(doc.GetValue<int>(maxIterKey), 100);
        EXPECT_DOUBLE_EQ(doc.GetValue<double>(bedLevKey), -5.0);
    }

    // -------------------------------------------------------------------------
    // Load(path)
    // -------------------------------------------------------------------------

    TEST_F(MduDocumentTest, Load_EmptyPath_ThrowsInvalidArgument)
    {
        EXPECT_THROW(doc.Load(std::filesystem::path{}), std::invalid_argument);
    }

    TEST_F(MduDocumentTest, Load_NonExistingPath_ThrowsIosBaseFailure)
    {
        const auto path = std::filesystem::path{"nonexistent_file_xyz.mdu"};

        EXPECT_THROW(doc.Load(path), std::ios_base::failure);
    }

    // -------------------------------------------------------------------------
    // Save(stream)
    // -------------------------------------------------------------------------

    TEST_F(MduDocumentTest, Save_ValidStream_WritesNonEmptyContent)
    {
        auto stream = std::istringstream(TestMduString());
        doc.Load(stream);

        std::ostringstream out;
        doc.Save(out);

        EXPECT_FALSE(out.str().empty());
    }

    TEST_F(MduDocumentTest, Save_FailedStream_ThrowsIosBaseFailure)
    {
        std::ostringstream out;
        out.setstate(std::ios::failbit);

        EXPECT_THROW(doc.Save(out), std::ios_base::failure);
    }

    // -------------------------------------------------------------------------
    // Save(path)
    // -------------------------------------------------------------------------

    TEST_F(MduDocumentTest, Save_EmptyPath_ThrowsInvalidArgument)
    {
        EXPECT_THROW(doc.Save(std::filesystem::path{}), std::invalid_argument);
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

    TEST_F(MduDocumentTest, GetValue_ExistingEnumProperty_ReturnsDefaultValue)
    {
        const std::string key = FormatKey("numerics", "vertAdvTypSal");

        EXPECT_EQ(doc.GetValue<EnumValue>(key).value, 6);
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

        EXPECT_NO_THROW(doc.GetValue<std::chrono::system_clock::time_point>(key));
    }

    // -------------------------------------------------------------------------
    // SetValue
    // -------------------------------------------------------------------------

    TEST_F(MduDocumentTest, SetValue_UnknownKey_ThrowsInvalidArgument)
    {
        EXPECT_THROW(doc.SetValue("general.nonexistent_xyz", 42), std::invalid_argument);
    }

    TEST_F(MduDocumentTest, SetValue_EnumOutOfRange_ThrowsOutOfRange)
    {
        const std::string key = FormatKey("general", "fileType");
        const EnumValue outOfRange{std::numeric_limits<int>::max()};

        EXPECT_THROW(doc.SetValue(key, outOfRange), std::out_of_range);
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

    TEST_F(MduDocumentTest, SetValue_ValidEnumValue_UpdatesData)
    {
        const std::string key = FormatKey("numerics", "vertAdvTypSal");

        doc.SetValue(key, EnumValue{4});

        EXPECT_EQ(doc.GetValue<EnumValue>(key).value, 4);
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
        const std::vector<std::filesystem::path> newValue{"a.ini", "b.ini"};

        doc.SetValue(key, newValue);

        const auto& value = doc.GetValue<std::vector<std::filesystem::path>>(key);
        ASSERT_EQ(value.size(), 2u);
        EXPECT_EQ(value[0], std::filesystem::path("a.ini"));
        EXPECT_EQ(value[1], std::filesystem::path("b.ini"));
    }

    TEST_F(MduDocumentTest, SetValue_ValidStringListValue_UpdatesData)
    {
        const std::string key = FormatKey("geometry", "activeProcesses");
        const std::vector<std::string> newValue{"Foo", "Bar"};

        doc.SetValue(key, newValue);

        const auto& value = doc.GetValue<std::vector<std::string>>(key);
        ASSERT_EQ(value.size(), 2u);
        EXPECT_EQ(value[0], "Foo");
        EXPECT_EQ(value[1], "Bar");
    }

    TEST_F(MduDocumentTest, SetValue_ValidFloatListValue_UpdatesData)
    {
        const std::string key = FormatKey("geometry", "stretchCoef");
        const std::vector<double> newValue{0.2, 0.3, 0.5};

        doc.SetValue(key, newValue);

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

        EXPECT_EQ(doc.GetValue<std::chrono::system_clock::time_point>(key), newValue);
    }

} // namespace dflowfm_io::test