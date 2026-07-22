#include <gtest/gtest.h>

#include <dflowfm_io/IssueReport.h>
#include <dflowfm_io/MduValidator.h>

#include <ini/IniData.h>
#include <ini/IniSection.h>

#include "MduTestData.h"

namespace dflowfm_io::test
{

    // -------------------------------------------------------------------------
    // Fixture
    // -------------------------------------------------------------------------

    class MduValidatorTest : public ::testing::Test
    {
    protected:
        MduValidator validator{TestSchema()};
    };

    // -------------------------------------------------------------------------
    // Validate — fully compliant input
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_FullyCompliantInput_HasNoErrors)
    {
        const IssueReport report = validator.Validate(TestIniData());

        EXPECT_FALSE(report.HasErrors());
    }

    TEST_F(MduValidatorTest, Validate_FullyCompliantInput_HasNoWarnings)
    {
        const IssueReport report = validator.Validate(TestIniData());

        EXPECT_FALSE(report.HasWarnings());
    }

    // -------------------------------------------------------------------------
    // Validate — missing required section
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_MissingRequiredSection_ReturnsError)
    {
        const IssueReport report = validator.Validate(ini::IniData{});

        EXPECT_TRUE(report.HasErrors());
    }

    TEST_F(MduValidatorTest, Validate_MissingRequiredSection_ErrorMentionsSectionName)
    {
        const IssueReport report = validator.Validate(ini::IniData{});

        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find("general"), std::string::npos);
    }

    // -------------------------------------------------------------------------
    // Validate — missing required property
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_MissingRequiredProperty_ReturnsError)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("general").RemoveAllProperties("fileVersion");

        const IssueReport report = validator.Validate(iniData);

        EXPECT_TRUE(report.HasErrors());
    }

    TEST_F(MduValidatorTest, Validate_MissingRequiredProperty_ErrorMentionsSectionAndPropertyKey)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("general").RemoveAllProperties("fileVersion");

        const IssueReport report = validator.Validate(iniData);

        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find("general"), std::string::npos);
        EXPECT_NE(error->message.find("fileVersion"), std::string::npos);
    }

    // -------------------------------------------------------------------------
    // Validate — required property present but without a value
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_RequiredPropertyWithoutValue_ReturnsError)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("general").SetPropertyValue("fileVersion", "");

        const IssueReport report = validator.Validate(iniData);

        EXPECT_TRUE(report.HasErrors());
    }

    TEST_F(MduValidatorTest, Validate_RequiredPropertyWithoutValue_ErrorMentionsSectionAndPropertyKey)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("general").SetPropertyValue("fileVersion", "");

        const IssueReport report = validator.Validate(iniData);

        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find("general"), std::string::npos);
        EXPECT_NE(error->message.find("fileVersion"), std::string::npos);
    }

    // -------------------------------------------------------------------------
    // Validate — missing optional property with default value
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_MissingOptionalPropertyWithDefault_ReturnsInfo)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").RemoveAllProperties("bedLevUni");

        const IssueReport report = validator.Validate(iniData);

        EXPECT_TRUE(report.HasInfos());
    }

    TEST_F(MduValidatorTest, Validate_MissingOptionalPropertyWithDefault_InfoMentionsSectionAndPropertyKeyAndDefault)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("geometry").RemoveAllProperties("bedLevUni");

        const IssueReport report = validator.Validate(iniData);

        const Issue* info = FirstIssue(report, Severity::Info);
        ASSERT_NE(info, nullptr);
        EXPECT_NE(info->message.find("geometry"), std::string::npos);
        EXPECT_NE(info->message.find("bedLevUni"), std::string::npos);
        EXPECT_NE(info->message.find("-5.0"), std::string::npos);
    }

    // -------------------------------------------------------------------------
    // Validate — unknown section
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_UnknownSection_ReturnsWarning)
    {
        ini::IniData iniData = TestIniData();
        iniData.AddSection("UnknownSection_XYZ");

        const IssueReport report = validator.Validate(iniData);

        EXPECT_TRUE(report.HasWarnings());
    }

    TEST_F(MduValidatorTest, Validate_UnknownSection_WarningMentionsSectionName)
    {
        ini::IniData iniData = TestIniData();
        iniData.AddSection("UnknownSection_XYZ");

        const IssueReport report = validator.Validate(iniData);

        const Issue* warning = FirstIssue(report, Severity::Warning);
        ASSERT_NE(warning, nullptr);
        EXPECT_NE(warning->message.find("UnknownSection_XYZ"), std::string::npos);
    }

    // -------------------------------------------------------------------------
    // Validate — unknown property
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_UnknownProperty_ReturnsWarning)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("general").AddProperty("unknownProperty_XYZ", "value");

        const IssueReport report = validator.Validate(iniData);

        EXPECT_TRUE(report.HasWarnings());
    }

    TEST_F(MduValidatorTest, Validate_UnknownProperty_WarningMentionsSectionAndPropertyKey)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("general").AddProperty("unknownProperty_XYZ", "value");

        const IssueReport report = validator.Validate(iniData);

        const Issue* warning = FirstIssue(report, Severity::Warning);
        ASSERT_NE(warning, nullptr);
        EXPECT_NE(warning->message.find("general"), std::string::npos);
        EXPECT_NE(warning->message.find("unknownProperty_XYZ"), std::string::npos);
    }

    // -------------------------------------------------------------------------
    // Validate — deprecated property
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_DeprecatedProperty_ReturnsWarning)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").SetPropertyValue("vertAdvTypSal", 6);

        const IssueReport report = validator.Validate(iniData);

        EXPECT_TRUE(report.HasWarnings());
    }

    TEST_F(MduValidatorTest, Validate_DeprecatedProperty_WarningMentionsSectionAndPropertyKey)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").SetPropertyValue("vertAdvTypSal", 6);

        const IssueReport report = validator.Validate(iniData);

        const Issue* warning = FirstIssue(report, Severity::Warning);
        ASSERT_NE(warning, nullptr);
        EXPECT_NE(warning->message.find("deprecated"), std::string::npos);
        EXPECT_NE(warning->message.find("numerics"), std::string::npos);
        EXPECT_NE(warning->message.find("vertAdvTypSal"), std::string::npos);
    }

    // -------------------------------------------------------------------------
    // Validate — deprecated enum value
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_DeprecatedEnumValue_ReturnsWarning)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").SetPropertyValue("layerType", 3);

        const IssueReport report = validator.Validate(iniData);

        EXPECT_TRUE(report.HasWarnings());
    }

    TEST_F(MduValidatorTest, Validate_DeprecatedEnumValue_WarningMentionsSectionPropertyKeyAndValue)
    {
        ini::IniData iniData = TestIniData();
        iniData.GetSection("numerics").SetPropertyValue("layerType", 3);

        const IssueReport report = validator.Validate(iniData);

        const Issue* warning = FirstIssue(report, Severity::Warning);
        ASSERT_NE(warning, nullptr);
        EXPECT_NE(warning->message.find("deprecated"), std::string::npos);
        EXPECT_NE(warning->message.find("numerics"), std::string::npos);
        EXPECT_NE(warning->message.find("layerType"), std::string::npos);
        EXPECT_NE(warning->message.find("3"), std::string::npos);
    }

} // namespace dflowfm_io::test