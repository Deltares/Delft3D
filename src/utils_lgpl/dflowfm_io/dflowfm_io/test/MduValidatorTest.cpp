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
        const MduSchema& schema = TestSchema();
        ini::IniData iniData = TestIniData();
    };

    // -------------------------------------------------------------------------
    // Validate — fully compliant input
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_FullyCompliantInput_HasNoErrors)
    {
        const IssueReport report = MduValidator::Validate(iniData, schema);

        EXPECT_FALSE(report.HasErrors());
    }

    TEST_F(MduValidatorTest, Validate_FullyCompliantInput_HasNoWarnings)
    {
        const IssueReport report = MduValidator::Validate(iniData, schema);

        EXPECT_FALSE(report.HasWarnings());
    }

    // -------------------------------------------------------------------------
    // Validate — missing required section
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_MissingRequiredSection_ReturnsError)
    {
        const IssueReport report = MduValidator::Validate(ini::IniData{}, schema);

        EXPECT_TRUE(report.HasErrors());
    }

    TEST_F(MduValidatorTest, Validate_MissingRequiredSection_ErrorMentionsSectionName)
    {
        const IssueReport report = MduValidator::Validate(ini::IniData{}, schema);

        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find("general"), std::string::npos);
    }

    // -------------------------------------------------------------------------
    // Validate — missing required property
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_MissingRequiredProperty_ReturnsError)
    {
        iniData.GetSection("general").RemoveAllProperties("fileVersion");

        const IssueReport report = MduValidator::Validate(iniData, schema);

        EXPECT_TRUE(report.HasErrors());
    }

    TEST_F(MduValidatorTest, Validate_MissingRequiredProperty_ErrorMentionsSectionAndPropertyKey)
    {
        iniData.GetSection("general").RemoveAllProperties("fileVersion");

        const IssueReport report = MduValidator::Validate(iniData, schema);

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
        iniData.GetSection("general").SetPropertyValue("fileVersion", "");

        const IssueReport report = MduValidator::Validate(iniData, schema);

        EXPECT_TRUE(report.HasErrors());
    }

    TEST_F(MduValidatorTest, Validate_RequiredPropertyWithoutValue_ErrorMentionsSectionAndPropertyKey)
    {
        iniData.GetSection("general").SetPropertyValue("fileVersion", "");

        const IssueReport report = MduValidator::Validate(iniData, schema);

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
        iniData.GetSection("geometry").RemoveAllProperties("bedLevUni");

        const IssueReport report = MduValidator::Validate(iniData, schema);

        EXPECT_TRUE(report.HasInfos());
    }

    TEST_F(MduValidatorTest, Validate_MissingOptionalPropertyWithDefault_InfoMentionsSectionAndPropertyKeyAndDefault)
    {
        iniData.GetSection("geometry").RemoveAllProperties("bedLevUni");

        const IssueReport report = MduValidator::Validate(iniData, schema);

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
        iniData.AddSection("UnknownSection_XYZ");

        const IssueReport report = MduValidator::Validate(iniData, schema);

        EXPECT_TRUE(report.HasWarnings());
    }

    TEST_F(MduValidatorTest, Validate_UnknownSection_WarningMentionsSectionName)
    {
        iniData.AddSection("UnknownSection_XYZ");

        const IssueReport report = MduValidator::Validate(iniData, schema);

        const Issue* warning = FirstIssue(report, Severity::Warning);
        ASSERT_NE(warning, nullptr);
        EXPECT_NE(warning->message.find("UnknownSection_XYZ"), std::string::npos);
    }

    // -------------------------------------------------------------------------
    // Validate — unknown property
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_UnknownProperty_ReturnsWarning)
    {
        iniData.GetSection("general").AddProperty("unknownProperty_XYZ", "value");

        const IssueReport report = MduValidator::Validate(iniData, schema);

        EXPECT_TRUE(report.HasWarnings());
    }

    TEST_F(MduValidatorTest, Validate_UnknownProperty_WarningMentionsSectionAndPropertyKey)
    {
        iniData.GetSection("general").AddProperty("unknownProperty_XYZ", "value");

        const IssueReport report = MduValidator::Validate(iniData, schema);

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
        iniData.GetSection("numerics").SetPropertyValue("vertAdvTypSal", 6);

        const IssueReport report = MduValidator::Validate(iniData, schema);

        EXPECT_TRUE(report.HasWarnings());
    }

    TEST_F(MduValidatorTest, Validate_DeprecatedProperty_WarningMentionsSectionAndPropertyKey)
    {
        iniData.GetSection("numerics").SetPropertyValue("vertAdvTypSal", 6);

        const IssueReport report = MduValidator::Validate(iniData, schema);

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
        iniData.GetSection("numerics").SetPropertyValue("layerType", 3);

        const IssueReport report = MduValidator::Validate(iniData, schema);

        EXPECT_TRUE(report.HasWarnings());
    }

    TEST_F(MduValidatorTest, Validate_DeprecatedEnumValue_WarningMentionsSectionPropertyKeyAndValue)
    {
        iniData.GetSection("numerics").SetPropertyValue("layerType", 3);

        const IssueReport report = MduValidator::Validate(iniData, schema);

        const Issue* warning = FirstIssue(report, Severity::Warning);
        ASSERT_NE(warning, nullptr);
        EXPECT_NE(warning->message.find("deprecated"), std::string::npos);
        EXPECT_NE(warning->message.find("numerics"), std::string::npos);
        EXPECT_NE(warning->message.find("layerType"), std::string::npos);
        EXPECT_NE(warning->message.find("3"), std::string::npos);
    }

} // namespace dflowfm_io::test