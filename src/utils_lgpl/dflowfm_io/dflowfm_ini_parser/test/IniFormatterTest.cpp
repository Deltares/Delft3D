#include <gtest/gtest.h>
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

    // -------------------------------------------------------------------------
    // Format - empty
    // -------------------------------------------------------------------------

    TEST(IniFormatterTest, Format_EmptyIniData_ReturnsEmptyString)
    {
        IniData iniData = CreateEmptyIniData();

        const std::string ini = Format(iniData);

        EXPECT_EQ(ini, "");
    }

    // -------------------------------------------------------------------------
    // Format - sections
    // -------------------------------------------------------------------------

    TEST(IniFormatterTest, Format_EmptySection_ReturnsFormattedString)
    {
        IniData iniData = CreateIniData(CreateEmptySection());

        const std::string ini = Format(iniData);
        const std::string expected = "[section]\n";

        EXPECT_EQ(ini, expected);
    }

    TEST(IniFormatterTest, Format_SectionWithComments_ReturnsFormattedString)
    {
        IniSection section = CreateEmptySection();
        section.AddComment("comment1");
        section.AddComment("comment2");

        IniData iniData = CreateIniData(section);

        const std::string ini = Format(iniData);
        const std::string expected =
            "# comment1\n"
            "# comment2\n"
            "[section]\n";

        EXPECT_EQ(ini, expected);
    }

    TEST(IniFormatterTest, Format_SectionWithEmptyComment_WritesEmptyLine)
    {
        IniSection section = CreateEmptySection();
        section.AddComment("comment");
        section.AddComment("");

        IniData iniData = CreateIniData(section);

        const std::string ini = Format(iniData);
        const std::string expected =
            "# comment\n"
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
        IniFormatterOptions options;
        options.writeComments = false;

        const std::string ini = Format(iniData, {}, options);
        const std::string expected = "[section]\n";

        EXPECT_EQ(ini, expected);
    }

    // -------------------------------------------------------------------------
    // Format - empty sections
    // -------------------------------------------------------------------------

    TEST(IniFormatterTest, Format_WriteEmptySectionsIsFalse_SkipsEmptySection)
    {
        IniData iniData = CreateIniData(CreateEmptySection());
        IniFormatterOptions options;
        options.writeEmptySections = false;

        const std::string ini = Format(iniData, {}, options);
        const std::string expected = "";

        EXPECT_EQ(ini, expected);
    }

    TEST(IniFormatterTest, Format_WriteEmptySectionsIsTrue_WritesEmptySection)
    {
        IniData iniData = CreateIniData(CreateEmptySection());
        IniFormatterOptions options;
        options.writeEmptySections = true;

        const std::string ini = Format(iniData, {}, options);
        const std::string expected = "[section]\n";

        EXPECT_EQ(ini, expected);
    }

    // -------------------------------------------------------------------------
    // Format - properties
    // -------------------------------------------------------------------------

    TEST(IniFormatterTest, Format_PropertiesAndComments_ReturnsFormattedString)
    {
        IniData iniData = CreateIniDataWithSingleSection();

        const std::string ini = Format(iniData);
        const std::string expected =
            "[section]\n"
            "property1             = value1               # comment\n"
            "property2             = value2               # comment\n"
            "property3             = value3               # comment\n"
            "\n";

        EXPECT_EQ(ini, expected);
    }

    TEST(IniFormatterTest, Format_PropertyAndEmptyComment_SkipsComment)
    {
        IniData iniData = CreateIniDataFromProperty("property", "value", "");

        const std::string ini = Format(iniData);
        const std::string expected =
            "[section]\n"
            "property              = value               \n"
            "\n";

        EXPECT_EQ(ini, expected);
    }

    TEST(IniFormatterTest, Format_PropertiesAndCommentsAndWriteCommentsIsFalse_ReturnsFormattedString)
    {
        IniData iniData = CreateIniDataWithSingleSection();
        IniFormatterOptions options;
        options.writeComments = false;

        const std::string ini = Format(iniData, {}, options);
        const std::string expected =
            "[section]\n"
            "property1             = value1              \n"
            "property2             = value2              \n"
            "property3             = value3              \n"
            "\n";

        EXPECT_EQ(ini, expected);
    }

    // -------------------------------------------------------------------------
    // Format - properties without value
    // -------------------------------------------------------------------------

    TEST(IniFormatterTest, Format_WritePropertyWithoutValueIsFalse_SkipsEmptyValueProperty)
    {
        IniData iniData = CreateIniDataFromProperty("property", "", "");
        IniFormatterOptions options;
        options.writePropertyWithoutValue = false;

        const std::string ini = Format(iniData, {}, options);
        const std::string expected =
            "[section]\n"
            "\n";

        EXPECT_EQ(ini, expected);
    }

    TEST(IniFormatterTest, Format_WritePropertyWithoutValueIsTrue_WritesEmptyValueProperty)
    {
        IniData iniData = CreateIniDataFromProperty("property", "", "");
        IniFormatterOptions options;
        options.writePropertyWithoutValue = true;

        const std::string ini = Format(iniData, {}, options);
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
        IniFormatterOptions options;
        options.propertyIndentationLevel = 4;
        options.propertyKeyWidth = 10;
        options.propertyValueWidth = 10;
        options.propertyAssignmentPadding = 1;
        options.propertyCommentPadding = 1;

        const std::string ini = Format(iniData, {}, options);
        const std::string expected =
            "[section]\n"
            "    property1  = value1     # comment\n"
            "    property2  = value2     # comment\n"
            "    property3  = value3     # comment\n"
            "\n";

        EXPECT_EQ(ini, expected);
    }

    TEST(IniFormatterTest, Format_PropertyKeyExceedsKeyWidth_CompensatesValueWidth)
    {
        IniSection section = CreateEmptySection();
        section.AddProperty(CreateProperty("short", "value1", "comment1"));
        section.AddProperty(CreateProperty("averylongkey", "value2", "comment2"));
        section.AddProperty(CreateProperty("medium", "value3", "comment3"));

        IniData iniData = CreateIniData(section);
        IniFormatterOptions options;
        options.propertyKeyWidth = 10;
        options.propertyValueWidth = 10;
        options.propertyAssignmentPadding = 1;
        options.propertyCommentPadding = 1;

        const std::string ini = Format(iniData, {}, options);
        const std::string expected =
            "[section]\n"
            "short      = value1     # comment1\n"
            "averylongkey = value2   # comment2\n"
            "medium     = value3     # comment3\n"
            "\n";

        EXPECT_EQ(ini, expected);
    }

    TEST(IniFormatterTest, Format_WithEmptySpacePropertyFormattingConfigured_ReturnsFormattedString)
    {
        IniData iniData = CreateIniDataWithSingleSection();
        IniFormatterOptions options = IniFormatterOptions::EmptySpace();

        const std::string ini = Format(iniData, {}, options);
        const std::string expected =
            "[section]\n"
            "property1=value1#comment\n"
            "property2=value2#comment\n"
            "property3=value3#comment\n"
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

        const std::string ini = Format(iniData);

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

        IniScheme scheme;
        scheme.commentDelimiter = ';';
        scheme.sectionStartDelimiter = '<';
        scheme.sectionEndDelimiter = '>';
        scheme.propertyAssignmentDelimiter = ':';

        const std::string ini = Format(iniData, scheme);
        const std::string expected =
            "<section>\n"
            "property1             : value1               ; comment\n"
            "property2             : value2               ; comment\n"
            "property3             : value3               ; comment\n"
            "\n";

        EXPECT_EQ(ini, expected);
    }

} // namespace ini::test