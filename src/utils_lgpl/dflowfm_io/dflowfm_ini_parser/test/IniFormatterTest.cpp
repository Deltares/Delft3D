#include <gtest/gtest.h>
#include <sstream>
#include <string>

#include "ini/IniData.h"
#include "ini/IniFormatter.h"
#include "ini/IniFormatterOptions.h"
#include "ini/IniProperty.h"
#include "ini/IniScheme.h"
#include "ini/IniSection.h"

namespace ini::test
{

    // -------------------------------------------------------------------------
    // Fixtures
    // -------------------------------------------------------------------------

    static IniProperty CreateProperty(const std::string& key = "property", const std::string& value = "value",
                                      const std::string& comment = "comment")
    {
        return IniProperty(key, value, comment);
    }

    static IniSection CreateEmptySection() { return IniSection("section"); }

    static IniSection CreateSection(const std::string& name = "section")
    {
        IniSection section(name);
        section.AddProperty(CreateProperty("property1", "value1"));
        section.AddProperty(CreateProperty("property2", "value2"));
        section.AddProperty(CreateProperty("property3", "value3"));
        return section;
    }

    static IniData CreateEmptyIniData() { return IniData(); }

    static IniData CreateIniData(const IniSection& section)
    {
        IniData iniData;
        iniData.AddSection(section);
        return iniData;
    }

    static IniData CreateIniDataWithSingleSection() { return CreateIniData(CreateSection()); }

    static IniData CreateIniDataFromProperty(const std::string& key = "property", const std::string& value = "value",
                                             const std::string& comment = "comment")
    {
        IniSection section = CreateEmptySection();
        section.AddProperty(CreateProperty(key, value, comment));
        return CreateIniData(section);
    }

    static IniFormatter CreateFormatter() { return IniFormatter(); }

    // -------------------------------------------------------------------------
    // Format - empty
    // -------------------------------------------------------------------------

    TEST(IniFormatterTest, Format_EmptyIniData_ReturnsEmptyString)
    {
        IniData iniData = CreateEmptyIniData();
        IniFormatter formatter = CreateFormatter();

        const std::string ini = formatter.Format(iniData);

        EXPECT_EQ(ini, "");
    }

    // -------------------------------------------------------------------------
    // Format - sections
    // -------------------------------------------------------------------------

    TEST(IniFormatterTest, Format_EmptySection_ReturnsFormattedString)
    {
        IniData iniData = CreateIniData(CreateEmptySection());
        IniFormatter formatter = CreateFormatter();

        const std::string ini = formatter.Format(iniData);
        const std::string expected = "[section]\n";

        EXPECT_EQ(ini, expected);
    }

    TEST(IniFormatterTest, Format_SectionWithComments_ReturnsFormattedString)
    {
        IniSection section = CreateEmptySection();
        section.AddComment("comment1");
        section.AddComment("comment2");

        IniData iniData = CreateIniData(section);
        IniFormatter formatter = CreateFormatter();

        const std::string ini = formatter.Format(iniData);
        const std::string expected =
            "# comment1\n"
            "# comment2\n"
            "\n"
            "[section]\n";

        EXPECT_EQ(ini, expected);
    }

    TEST(IniFormatterTest, Format_SectionWithCommentsAndWriteCommentsIsFalse_ReturnsFormattedString)
    {
        IniSection section = CreateEmptySection();
        section.AddComment("comment1");
        section.AddComment("comment2");

        IniData iniData = CreateIniData(section);
        IniFormatter formatter = CreateFormatter();

        IniFormatterOptions& options = formatter.GetOptions();
        options.writeComments = false;

        const std::string ini = formatter.Format(iniData);
        const std::string expected = "[section]\n";

        EXPECT_EQ(ini, expected);
    }

    TEST(IniFormatterTest, Format_SectionWithPropertiesAndComments_ReturnsFormattedString)
    {
        IniData iniData = CreateIniDataWithSingleSection();
        IniFormatter formatter = CreateFormatter();

        const std::string ini = formatter.Format(iniData);
        const std::string expected =
            "[section]\n"
            "property1             = value1              # comment\n"
            "property2             = value2              # comment\n"
            "property3             = value3              # comment\n"
            "\n";

        EXPECT_EQ(ini, expected);
    }

    TEST(IniFormatterTest, Format_SectionWithPropertiesAndCommentsAndWriteCommentsIsFalse_ReturnsFormattedString)
    {
        IniData iniData = CreateIniDataWithSingleSection();
        IniFormatter formatter = CreateFormatter();

        IniFormatterOptions& options = formatter.GetOptions();
        options.writeComments = false;

        const std::string ini = formatter.Format(iniData);
        const std::string expected =
            "[section]\n"
            "property1             = value1              \n"
            "property2             = value2              \n"
            "property3             = value3              \n"
            "\n";

        EXPECT_EQ(ini, expected);
    }

    // -------------------------------------------------------------------------
    // Format - WritePropertyWithoutValue
    // -------------------------------------------------------------------------

    TEST(IniFormatterTest, Format_WritePropertyWithoutValueIsFalse_SkipsEmptyValueProperty)
    {
        IniData iniData = CreateIniDataFromProperty("property", "", "");
        IniFormatter formatter = CreateFormatter();

        IniFormatterOptions& options = formatter.GetOptions();
        options.writePropertyWithoutValue = false;

        const std::string ini = formatter.Format(iniData);
        const std::string expected =
            "[section]\n"
            "\n";

        EXPECT_EQ(ini, expected);
    }

    TEST(IniFormatterTest, Format_WritePropertyWithoutValueIsTrue_WritesEmptyValueProperty)
    {
        IniData iniData = CreateIniDataFromProperty("property", "", "");
        IniFormatter formatter = CreateFormatter();

        IniFormatterOptions& options = formatter.GetOptions();
        options.writePropertyWithoutValue = true;

        const std::string ini = formatter.Format(iniData);
        const std::string expected =
            "[section]\n"
            "property              =                     \n"
            "\n";

        EXPECT_EQ(ini, expected);
    }

    // -------------------------------------------------------------------------
    // Format - property formatting
    // -------------------------------------------------------------------------

    TEST(IniFormatterTest, Format_WithPropertyFormattingConfigured_ReturnsFormattedString)
    {
        IniData iniData = CreateIniDataWithSingleSection();
        IniFormatter formatter = CreateFormatter();

        IniFormatterOptions& options = formatter.GetOptions();
        options.propertyIndentationLevel = 4;
        options.propertyKeyWidth = 10;
        options.propertyValueWidth = 10;
        options.propertyAssignmentPadding = 1;

        const std::string ini = formatter.Format(iniData);
        const std::string expected =
            "[section]\n"
            "    property1  = value1    # comment\n"
            "    property2  = value2    # comment\n"
            "    property3  = value3    # comment\n"
            "\n";

        EXPECT_EQ(ini, expected);
    }

    TEST(IniFormatterTest, Format_WithEmptySpacePropertyFormattingConfigured_ReturnsFormattedString)
    {
        IniData iniData = CreateIniDataWithSingleSection();
        IniFormatter formatter = CreateFormatter();

        IniFormatterOptions options = IniFormatterOptions::EmptySpace();
        options.writeComments = false;
        formatter.SetOptions(options);

        const std::string ini = formatter.Format(iniData);
        const std::string expected =
            "[section]\n"
            "property1=value1\n"
            "property2=value2\n"
            "property3=value3\n"
            "\n";

        EXPECT_EQ(ini, expected);
    }

    // -------------------------------------------------------------------------
    // Format - special characters
    // -------------------------------------------------------------------------

    class IniFormatterSpecialCharactersTest : public ::testing::TestWithParam<std::pair<std::string, std::string>>
    {
    };

    TEST_P(IniFormatterSpecialCharactersTest, Format_PropertyWithSpecialCharacters_ReturnsFormattedString)
    {
        auto [key, value] = GetParam();
        IniData iniData = CreateIniDataFromProperty(key, value, "");
        IniFormatter formatter = CreateFormatter();

        const std::string ini = formatter.Format(iniData);

        EXPECT_TRUE(ini.find(key) != std::string::npos) << "Expected key '" << key << "' in:\n" << ini;
        EXPECT_TRUE(ini.find(value) != std::string::npos) << "Expected value '" << value << "' in:\n" << ini;
    }

    INSTANTIATE_TEST_SUITE_P(IniFormatterTest, IniFormatterSpecialCharactersTest,
                             ::testing::Values(std::make_pair("property-1", "value-1"),
                                               std::make_pair("property\\1", "value\\1"),
                                               std::make_pair("property¹²³", "value¹²³"),
                                               std::make_pair("p][r[o]p][e[]rt[y", "v][a[l]u][e[]")));

    // -------------------------------------------------------------------------
    // Format - scheme
    // -------------------------------------------------------------------------

    TEST(IniFormatterTest, Format_WithIniSchemeConfigured_ReturnsFormattedString)
    {
        IniData iniData = CreateIniDataWithSingleSection();
        IniFormatter formatter = CreateFormatter();

        IniScheme scheme;
        scheme.commentDelimiter = ';';
        scheme.sectionStartDelimiter = '<';
        scheme.sectionEndDelimiter = '>';
        scheme.propertyAssignmentDelimiter = ':';
        formatter.SetScheme(scheme);

        const std::string ini = formatter.Format(iniData);
        const std::string expected =
            "<section>\n"
            "property1             : value1              ; comment\n"
            "property2             : value2              ; comment\n"
            "property3             : value3              ; comment\n"
            "\n";

        EXPECT_EQ(ini, expected);
    }

    // -------------------------------------------------------------------------
    // Format - stream
    // -------------------------------------------------------------------------

    TEST(IniFormatterTest, Format_EmptyIniData_WritesEmptyStringToStream)
    {
        IniData iniData = CreateEmptyIniData();
        IniFormatter formatter = CreateFormatter();

        std::ostringstream oss;
        formatter.Format(iniData, oss);

        EXPECT_EQ(oss.str(), "");
    }

    TEST(IniFormatterTest, Format_SectionWithPropertiesAndComments_WritesToStream)
    {
        IniData iniData = CreateIniDataWithSingleSection();
        IniFormatter formatter = CreateFormatter();

        std::ostringstream oss;
        formatter.Format(iniData, oss);

        const std::string expected =
            "[section]\n"
            "property1             = value1              # comment\n"
            "property2             = value2              # comment\n"
            "property3             = value3              # comment\n"
            "\n";

        EXPECT_EQ(oss.str(), expected);
    }

    TEST(IniFormatterTest, Format_FormatToStreamAndFormatToString_ReturnSameResult)
    {
        IniData iniData = CreateIniDataWithSingleSection();
        IniFormatter formatter = CreateFormatter();

        const std::string fromString = formatter.Format(iniData);

        std::ostringstream oss;
        formatter.Format(iniData, oss);

        EXPECT_EQ(fromString, oss.str());
    }

} // namespace ini::test