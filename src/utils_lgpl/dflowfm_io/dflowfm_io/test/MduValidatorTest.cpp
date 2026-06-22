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
        static void SetUpTestSuite() { compliantIniData = std::make_unique<ini::IniData>(MakeCompliantIniData()); }

        static void TearDownTestSuite() { compliantIniData.reset(); }

        static ini::IniData CompliantIniData() { return *compliantIniData; }

        static inline std::unique_ptr<ini::IniData> compliantIniData;
    };

    // -------------------------------------------------------------------------
    // Validate — fully compliant input
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_FullyCompliantInput_ReturnsEmptyReport)
    {
        const ini::IniData iniData = CompliantIniData();

        const IssueReport report = MduValidator::Validate(iniData);

        EXPECT_TRUE(report.empty());
    }

    TEST_F(MduValidatorTest, Validate_FullyCompliantInput_HasNoErrors)
    {
        const ini::IniData iniData = CompliantIniData();

        const IssueReport report = MduValidator::Validate(iniData);

        EXPECT_FALSE(report.HasErrors());
    }

    TEST_F(MduValidatorTest, Validate_FullyCompliantInput_HasNoWarnings)
    {
        const ini::IniData iniData = CompliantIniData();

        const IssueReport report = MduValidator::Validate(iniData);

        EXPECT_FALSE(report.HasWarnings());
    }

    TEST_F(MduValidatorTest, Validate_FullyCompliantInput_HasNoInfos)
    {
        const ini::IniData iniData = CompliantIniData();

        const IssueReport report = MduValidator::Validate(iniData);

        EXPECT_FALSE(report.HasInfos());
    }

    // -------------------------------------------------------------------------
    // Validate — missing required section
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_MissingRequiredSection_ReturnsError)
    {
        const IssueReport report = MduValidator::Validate(ini::IniData{});

        EXPECT_TRUE(report.HasErrors());
    }

    TEST_F(MduValidatorTest, Validate_MissingRequiredSection_ErrorMentionsSectionName)
    {
        const auto [targetSection, targetProperty] = FirstRequiredProperty();

        const IssueReport report = MduValidator::Validate(ini::IniData{});

        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find(targetSection->name), std::string::npos);
    }

    // -------------------------------------------------------------------------
    // Validate — missing required property
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_MissingRequiredProperty_ReturnsError)
    {
        const auto [targetSection, targetProperty] = FirstRequiredProperty();

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).RemoveAllProperties(targetProperty->key);

        const IssueReport report = MduValidator::Validate(iniData);

        EXPECT_TRUE(report.HasErrors());
    }

    TEST_F(MduValidatorTest, Validate_MissingRequiredProperty_ErrorMentionsPropertyKey)
    {
        const auto [targetSection, targetProperty] = FirstRequiredProperty();

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).RemoveAllProperties(targetProperty->key);

        const IssueReport report = MduValidator::Validate(iniData);

        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find(targetSection->name), std::string::npos);
        EXPECT_NE(error->message.find(targetProperty->key), std::string::npos);
    }

    // -------------------------------------------------------------------------
    // Validate — required property present but without a value
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_RequiredPropertyWithoutValue_ReturnsError)
    {
        const auto [targetSection, targetProperty] = FirstRequiredProperty();

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).SetPropertyValue(targetProperty->key, "");

        const IssueReport report = MduValidator::Validate(iniData);

        EXPECT_TRUE(report.HasErrors());
    }

    TEST_F(MduValidatorTest, Validate_RequiredPropertyWithoutValue_ErrorMentionsSectionAndPropertyKey)
    {
        const auto [targetSection, targetProperty] = FirstRequiredProperty();

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).SetPropertyValue(targetProperty->key, "");

        const IssueReport report = MduValidator::Validate(iniData);

        const Issue* error = FirstIssue(report, Severity::Error);
        ASSERT_NE(error, nullptr);
        EXPECT_NE(error->message.find(targetSection->name), std::string::npos);
        EXPECT_NE(error->message.find(targetProperty->key), std::string::npos);
    }

    // -------------------------------------------------------------------------
    // Validate — optional property with default value absent
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_OptionalPropertyWithDefaultAbsent_ReturnsInfo)
    {
        const auto [targetSection, targetProperty] = FirstOptionalPropertyWithDefault();

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).RemoveAllProperties(targetProperty->key);

        const IssueReport report = MduValidator::Validate(iniData);

        EXPECT_TRUE(report.HasInfos());
    }

    TEST_F(MduValidatorTest, Validate_OptionalPropertyWithDefaultAbsent_InfoMentionsSectionAndPropertyKey)
    {
        const auto [targetSection, targetProperty] = FirstOptionalPropertyWithDefault();

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).RemoveAllProperties(targetProperty->key);

        const IssueReport report = MduValidator::Validate(iniData);

        const Issue* info = FirstIssue(report, Severity::Info);
        ASSERT_NE(info, nullptr);
        EXPECT_NE(info->message.find(targetSection->name), std::string::npos);
        EXPECT_NE(info->message.find(targetProperty->key), std::string::npos);
    }

    TEST_F(MduValidatorTest, Validate_OptionalPropertyWithDefaultAbsent_InfoMentionsDefaultValue)
    {
        const auto [targetSection, targetProperty] = FirstOptionalPropertyWithDefault();

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).RemoveAllProperties(targetProperty->key);

        const IssueReport report = MduValidator::Validate(iniData);

        const Issue* info = FirstIssue(report, Severity::Info);
        ASSERT_NE(info, nullptr);
        EXPECT_NE(info->message.find(targetProperty->default_value), std::string::npos);
    }

    // -------------------------------------------------------------------------
    // Validate — unknown section
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_UnknownSection_ReturnsWarning)
    {
        ini::IniData iniData = CompliantIniData();
        iniData.AddSection("UnknownSection_XYZ");

        const IssueReport report = MduValidator::Validate(iniData);

        EXPECT_TRUE(report.HasWarnings());
    }

    TEST_F(MduValidatorTest, Validate_UnknownSection_WarningMentionsSectionName)
    {
        ini::IniData iniData = CompliantIniData();
        iniData.AddSection("UnknownSection_XYZ");

        const IssueReport report = MduValidator::Validate(iniData);

        const Issue* warning = FirstIssue(report, Severity::Warning);
        ASSERT_NE(warning, nullptr);
        EXPECT_NE(warning->message.find("UnknownSection_XYZ"), std::string::npos);
    }

    // -------------------------------------------------------------------------
    // Validate — unknown property
    // -------------------------------------------------------------------------

    TEST_F(MduValidatorTest, Validate_UnknownProperty_ReturnsWarning)
    {
        const auto [targetSection, targetProperty] = FirstRequiredProperty();

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).AddProperty("UnknownProperty_XYZ", "value");

        const IssueReport report = MduValidator::Validate(iniData);

        EXPECT_TRUE(report.HasWarnings());
    }

    TEST_F(MduValidatorTest, Validate_UnknownProperty_WarningMentionsSectionAndPropertyKey)
    {
        const auto [targetSection, targetProperty] = FirstRequiredProperty();

        ini::IniData iniData = CompliantIniData();
        iniData.GetSection(targetSection->name).AddProperty("UnknownProperty_XYZ", "value");

        const IssueReport report = MduValidator::Validate(iniData);

        const Issue* warning = FirstIssue(report, Severity::Warning);
        ASSERT_NE(warning, nullptr);
        EXPECT_NE(warning->message.find(targetSection->name), std::string::npos);
        EXPECT_NE(warning->message.find("UnknownProperty_XYZ"), std::string::npos);
    }

} // namespace dflowfm_io::test