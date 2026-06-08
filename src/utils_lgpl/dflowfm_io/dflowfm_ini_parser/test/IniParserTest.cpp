#include <gtest/gtest.h>
#include <algorithm>
#include <sstream>
#include <string>

#include "ini/IniData.h"
#include "ini/IniParser.h"
#include "ini/IniParserOptions.h"
#include "ini/IniProperty.h"
#include "ini/IniScheme.h"
#include "ini/IniSection.h"

namespace ini::test
{

    static IniParser CreateParser() { return IniParser{}; }

    // -------------------------------------------------------------------------
    // Parse - empty input
    // -------------------------------------------------------------------------

    TEST(IniParserTest, Parse_EmptyString_ReturnsIniDataWithoutSections)
    {
        IniParser parser = CreateParser();

        const IniData iniData = parser.Parse("");

        EXPECT_TRUE(iniData.empty());
    }

    TEST(IniParserTest, Parse_EmptyLinesString_ReturnsIniDataWithoutSections)
    {
        IniParser parser = CreateParser();

        const IniData iniData = parser.Parse("\n\n");

        EXPECT_TRUE(iniData.empty());
    }

    // -------------------------------------------------------------------------
    // Parse - sections
    // -------------------------------------------------------------------------

    class IniParserValidSectionFormatTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniParserValidSectionFormatTest, Parse_ValidSectionFormat_IniDataHasSection)
    {
        IniParser parser = CreateParser();

        const IniData iniData = parser.Parse(GetParam());

        EXPECT_TRUE(iniData.HasSection("section"));
    }

    INSTANTIATE_TEST_SUITE_P(IniParserTest, IniParserValidSectionFormatTest,
                             ::testing::Values("[section]", " [section] ", "\t[section]\t"));

    class IniParserSpecialCharactersSectionNameTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniParserSpecialCharactersSectionNameTest, Parse_SpecialCharactersInSectionName_IniDataHasSection)
    {
        IniParser parser = CreateParser();
        const std::string sectionName = GetParam();

        const IniData iniData = parser.Parse("[" + sectionName + "]");

        EXPECT_TRUE(iniData.HasSection(sectionName));
    }

    INSTANTIATE_TEST_SUITE_P(IniParserTest, IniParserSpecialCharactersSectionNameTest,
                             ::testing::Values("section#1", "section-1", "section²", "section\\subsection",
                                               "section~subsection", "section*subsection", "section \" xyz",
                                               "#section#", "s][e[c]t][i[]on[", "https://example.com/page",
                                               "{C3BA7795-F319-4CC0-B091-783DDEBCCDF1}"));

    class IniParserInvalidSectionFormatTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniParserInvalidSectionFormatTest, Parse_InvalidSectionFormat_ThrowsFormatError)
    {
        IniParser parser = CreateParser();

        EXPECT_THROW(
            {
                try
                {
                    parser.Parse(GetParam());
                }
                catch (const std::format_error& ex)
                {
                    EXPECT_STREQ(ex.what(), "Error on line 1: invalid INI-formatted text.");
                    throw;
                }
            },
            std::format_error);
    }

    INSTANTIATE_TEST_SUITE_P(IniParserTest, IniParserInvalidSectionFormatTest,
                             ::testing::Values("[section", "[section[", "]section[", "a[section]"));

    class IniParserEmptySectionNameTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniParserEmptySectionNameTest, Parse_EmptySectionName_ThrowsFormatError)
    {
        IniParser parser = CreateParser();

        EXPECT_THROW(
            {
                try
                {
                    parser.Parse(GetParam());
                }
                catch (const std::format_error& ex)
                {
                    EXPECT_STREQ(ex.what(), "Error on line 1: section name cannot be empty.");
                    throw;
                }
            },
            std::format_error);
    }

    INSTANTIATE_TEST_SUITE_P(IniParserTest, IniParserEmptySectionNameTest, ::testing::Values("[]", "[ ]", "[\t\t]"));

    TEST(IniParserTest, Parse_DuplicateSectionNamesAndAllowDuplicateSectionsIsFalse_ThrowsFormatError)
    {
        IniParser parser = CreateParser();

        IniParserOptions& options = parser.GetOptions();
        options.allowDuplicateSections = false;

        const std::string ini = "[section]\n[section]";

        EXPECT_THROW(
            {
                try
                {
                    parser.Parse(ini);
                }
                catch (const std::format_error& ex)
                {
                    EXPECT_STREQ(ex.what(), "Error on line 2: duplicate section with name 'section'.");
                    throw;
                }
            },
            std::format_error);
    }

    TEST(IniParserTest, Parse_DuplicateSectionNamesAndAllowDuplicateSectionsIsTrue_IniDataHasMultipleSections)
    {
        IniParser parser = CreateParser();

        IniParserOptions& options = parser.GetOptions();
        options.allowDuplicateSections = true;

        const std::string ini = "[section]\n[section]";

        const IniData iniData = parser.Parse(ini);

        EXPECT_EQ(iniData.size(), 2);
        ASSERT_TRUE(iniData.HasSection("section"));
    }

    TEST(IniParserTest, Parse_MultipleSections_SectionsHaveLineNumbers)
    {
        IniParser parser = CreateParser();

        const std::string ini = "[section1]\n[section2]";

        const IniData iniData = parser.Parse(ini);

        ASSERT_TRUE(iniData.HasSection("section1"));
        ASSERT_TRUE(iniData.HasSection("section2"));

        const IniSection section1 = iniData.GetSection("section1");
        const IniSection section2 = iniData.GetSection("section2");

        EXPECT_EQ(section1.GetLineNumber(), 1);
        EXPECT_EQ(section2.GetLineNumber(), 2);
    }

    // -------------------------------------------------------------------------
    // Parse - section comments
    // -------------------------------------------------------------------------

    class IniParserInvalidCommentDelimiterTest : public ::testing::TestWithParam<char>
    {
    };

    TEST_P(IniParserInvalidCommentDelimiterTest, Parse_SectionWithInvalidCommentDelimiter_ThrowsFormatError)
    {
        IniParser parser = CreateParser();
        const std::string ini = std::string(1, GetParam()) + " section comment\n[section]";

        EXPECT_THROW(
            {
                try
                {
                    parser.Parse(ini);
                }
                catch (const std::format_error& ex)
                {
                    EXPECT_STREQ(ex.what(), "Error on line 1: invalid INI-formatted text.");
                    throw;
                }
            },
            std::format_error);
    }

    INSTANTIATE_TEST_SUITE_P(IniParserTest, IniParserInvalidCommentDelimiterTest,
                             ::testing::Values('!', '*', ';', ':', '-'));

    class IniParserSectionCommentLineTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniParserSectionCommentLineTest, Parse_SectionWithCommentLine_SectionHasComment)
    {
        IniParser parser = CreateParser();
        const std::string ini = GetParam() + "\n[section]";

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");

        ASSERT_EQ(section.GetComments().size(), 1);
        EXPECT_EQ(section.GetComments()[0], "section comment");
    }

    INSTANTIATE_TEST_SUITE_P(IniParserTest, IniParserSectionCommentLineTest,
                             ::testing::Values("# section comment", " #section comment ", " # section comment  ",
                                               "\t\t#section comment\t"));

    class IniParserEmptySectionCommentTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniParserEmptySectionCommentTest, Parse_SectionWithEmptyCommentLine_SectionHasEmptyComment)
    {
        IniParser parser = CreateParser();
        const std::string ini = "#" + GetParam() + "\n[section]";

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");

        ASSERT_EQ(section.GetComments().size(), 1);
        EXPECT_EQ(section.GetComments()[0], "");
    }

    INSTANTIATE_TEST_SUITE_P(IniParserTest, IniParserEmptySectionCommentTest, ::testing::Values("", " ", "\t"));

    TEST(IniParserTest, Parse_SectionWithCommentLineAndParseCommentsIsFalse_SectionCommentsIsEmpty)
    {
        IniParser parser = CreateParser();

        IniParserOptions& options = parser.GetOptions();
        options.parseComments = false;

        const std::string ini = "# section comment\n[section]";

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");

        EXPECT_TRUE(section.GetComments().empty());
    }

    TEST(IniParserTest, Parse_SectionWithInlineComment_SectionCommentsIsEmpty)
    {
        IniParser parser = CreateParser();

        const IniData iniData = parser.Parse("[section] # inline comment");
        const IniSection section = iniData.GetSection("section");

        EXPECT_TRUE(section.GetComments().empty());
    }

    // -------------------------------------------------------------------------
    // Parse - properties
    // -------------------------------------------------------------------------

    TEST(IniParserTest, Parse_PropertyWithoutSection_ThrowsFormatError)
    {
        IniParser parser = CreateParser();

        EXPECT_THROW(
            {
                try
                {
                    parser.Parse("property = value");
                }
                catch (const std::format_error& ex)
                {
                    EXPECT_STREQ(ex.what(), "Error on line 1: properties must be defined within a section.");
                    throw;
                }
            },
            std::format_error);
    }

    class IniParserValidPropertyFormatTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniParserValidPropertyFormatTest, Parse_ValidPropertyFormat_SectionHasProperty)
    {
        IniParser parser = CreateParser();
        const std::string ini = "[section]\n" + GetParam();

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");

        ASSERT_TRUE(section.HasProperty("property"));
        const IniProperty property = section.GetProperty("property");

        EXPECT_EQ(property.GetValue(), "value");
    }

    INSTANTIATE_TEST_SUITE_P(IniParserTest, IniParserValidPropertyFormatTest,
                             ::testing::Values("property=value", " property = value", "\tproperty\t=\tvalue"));

    class IniParserSpecialCharactersPropertyKeyTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniParserSpecialCharactersPropertyKeyTest, Parse_SpecialCharactersInPropertyKey_SectionHasProperty)
    {
        IniParser parser = CreateParser();
        const std::string propertyKey = GetParam();
        const std::string ini = "[section]\n" + propertyKey + "=value";

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");

        ASSERT_TRUE(section.HasProperty(propertyKey));
        const IniProperty property = section.GetProperty(propertyKey);

        EXPECT_EQ(property.GetValue(), "value");
    }

    INSTANTIATE_TEST_SUITE_P(IniParserTest, IniParserSpecialCharactersPropertyKeyTest,
                             ::testing::Values("_property_1", ".property_1", "-property_1", "property_1", "property-1",
                                               "property~1", "property*1", "property.1", "property#1", "property\\1",
                                               "property²", "p][r[o]p][e[]rt[y"));

    class IniParserPropertyWithoutKeyTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniParserPropertyWithoutKeyTest, Parse_PropertyWithoutKey_ThrowsFormatError)
    {
        IniParser parser = CreateParser();
        const std::string ini = "[section]\n" + GetParam();

        EXPECT_THROW(
            {
                try
                {
                    parser.Parse(ini);
                }
                catch (const std::format_error& ex)
                {
                    EXPECT_STREQ(ex.what(), "Error on line 2: property key cannot be empty.");
                    throw;
                }
            },
            std::format_error);
    }

    INSTANTIATE_TEST_SUITE_P(IniParserTest, IniParserPropertyWithoutKeyTest,
                             ::testing::Values("=value", " = value", "\t=value"));

    class IniParserPropertyKeyWithSpacesTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniParserPropertyKeyWithSpacesTest, Parse_PropertyKeyWithSpaces_ThrowsFormatError)
    {
        IniParser parser = CreateParser();
        const std::string ini = "[section]\n" + GetParam();

        EXPECT_THROW(
            {
                try
                {
                    parser.Parse(ini);
                }
                catch (const std::format_error& ex)
                {
                    EXPECT_STREQ(ex.what(), "Error on line 2: property key cannot contain spaces.");
                    throw;
                }
            },
            std::format_error);
    }

    INSTANTIATE_TEST_SUITE_P(IniParserTest, IniParserPropertyKeyWithSpacesTest,
                             ::testing::Values("property with spaces = value", "property\twith\ttabs=value"));

    class IniParserPropertyKeyWithSpacesAllowedTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniParserPropertyKeyWithSpacesAllowedTest,
           Parse_PropertyKeyWithSpacesAndAllowPropertyKeysWithSpacesIsTrue_SectionHasProperty)
    {
        IniParser parser = CreateParser();

        IniParserOptions& options = parser.GetOptions();
        options.allowPropertyKeysWithSpaces = true;

        const std::string propertyKey = GetParam();
        const std::string ini = "[section]\n" + propertyKey + "=value";

        std::string normalizedKey = propertyKey;
        std::replace(normalizedKey.begin(), normalizedKey.end(), '\t', ' ');

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");

        ASSERT_TRUE(section.HasProperty(normalizedKey));
        const IniProperty property = section.GetProperty(normalizedKey);

        EXPECT_EQ(property.GetValue(), "value");
    }

    INSTANTIATE_TEST_SUITE_P(IniParserTest, IniParserPropertyKeyWithSpacesAllowedTest,
                             ::testing::Values("property with spaces", "property\twith\ttabs"));

    TEST(IniParserTest, Parse_DuplicatePropertyKeysAndAllowDuplicatePropertiesIsFalse_ThrowsFormatError)
    {
        IniParser parser = CreateParser();

        IniParserOptions& options = parser.GetOptions();
        options.allowDuplicateProperties = false;

        const std::string ini = "[section]\nproperty=value1\nproperty=value2";

        EXPECT_THROW(
            {
                try
                {
                    parser.Parse(ini);
                }
                catch (const std::format_error& ex)
                {
                    EXPECT_STREQ(ex.what(), "Error on line 3: duplicate property with key 'property'.");
                    throw;
                }
            },
            std::format_error);
    }

    TEST(IniParserTest, Parse_DuplicatePropertyKeysAndAllowDuplicatePropertiesIsTrue_SectionHasProperties)
    {
        IniParser parser = CreateParser();

        IniParserOptions& options = parser.GetOptions();
        options.allowDuplicateProperties = true;

        const std::string ini = "[section]\nproperty=value1\nproperty=value2";

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");

        EXPECT_EQ(section.size(), 2);
        ASSERT_TRUE(section.HasProperty("property"));
    }

    TEST(IniParserTest, Parse_MultipleProperties_PropertiesHaveLineNumbers)
    {
        IniParser parser = CreateParser();

        const std::string ini = "[section]\nproperty1=value1\nproperty2=value2";

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");

        ASSERT_TRUE(section.HasProperty("property1"));
        ASSERT_TRUE(section.HasProperty("property2"));

        const IniProperty property1 = section.GetProperty("property1");
        const IniProperty property2 = section.GetProperty("property2");

        EXPECT_EQ(property1.GetLineNumber(), 2);
        EXPECT_EQ(property2.GetLineNumber(), 3);
    }

    TEST(IniParserTest, Parse_PropertyWithoutValue_PropertyHasEmptyValue)
    {
        IniParser parser = CreateParser();

        const std::string ini = "[section]\nproperty=";

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");

        ASSERT_TRUE(section.HasProperty("property"));
        const IniProperty property = section.GetProperty("property");

        EXPECT_TRUE(property.GetValue().empty());
    }

    class IniParserSpecialCharactersPropertyValueTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniParserSpecialCharactersPropertyValueTest, Parse_SpecialCharactersInPropertyValue_SectionHasProperty)
    {
        IniParser parser = CreateParser();
        const std::string propertyValue = GetParam();
        const std::string ini = "[section]\nproperty=" + propertyValue;

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");

        ASSERT_TRUE(section.HasProperty("property"));

        const IniProperty property = section.GetProperty("property");

        EXPECT_EQ(property.GetValue(), propertyValue);
    }

    INSTANTIATE_TEST_SUITE_P(IniParserTest, IniParserSpecialCharactersPropertyValueTest,
                             ::testing::Values("value-1", "value ¹²³", "value\\1", "value \" xyz", "v][a[l]u][e[",
                                               "https://example.com/page", "{C3BA7795-F319-4CC0-B091-783DDEBCCDF1}"));

    // -------------------------------------------------------------------------
    // Parse - property comments
    // -------------------------------------------------------------------------

    TEST(IniParserTest, Parse_PropertyWithCommentLines_CommentLinesAreIgnored)
    {
        IniParser parser = CreateParser();

        const std::string ini = "[section]\n# property comment 1\n# property comment 2\nkey=value";

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");
        const IniProperty property = section.GetProperty("key");

        EXPECT_TRUE(section.GetComments().empty());
        EXPECT_TRUE(property.GetComment().empty());
    }

    class IniParserPropertyInvalidCommentDelimiterTest : public ::testing::TestWithParam<char>
    {
    };

    TEST_P(IniParserPropertyInvalidCommentDelimiterTest, Parse_PropertyWithInvalidCommentDelimiter_ThrowsFormatError)
    {
        IniParser parser = CreateParser();
        const std::string ini =
            "[section]\nproperty1=value1\n" + std::string(1, GetParam()) + " property comment\nproperty2=value2";

        EXPECT_THROW(
            {
                try
                {
                    parser.Parse(ini);
                }
                catch (const std::format_error& ex)
                {
                    EXPECT_STREQ(ex.what(), "Error on line 3: invalid INI-formatted text.");
                    throw;
                }
            },
            std::format_error);
    }

    INSTANTIATE_TEST_SUITE_P(IniParserTest, IniParserPropertyInvalidCommentDelimiterTest,
                             ::testing::Values('!', '*', ';', ':'));

    TEST(IniParserTest, Parse_PropertyWithInlineComment_PropertyHasComment)
    {
        IniParser parser = CreateParser();

        const std::string ini = "[section]\nproperty=value # inline comment";

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");
        const IniProperty property = section.GetProperty("property");

        EXPECT_EQ(property.GetComment(), "inline comment");
    }

    TEST(IniParserTest, Parse_PropertyWithInlineCommentAndParseCommentsIsFalse_PropertyCommentIsEmpty)
    {
        IniParser parser = CreateParser();

        IniParserOptions& options = parser.GetOptions();
        options.parseComments = false;

        const std::string ini = "[section]\nproperty=value # inline comment";

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");
        const IniProperty property = section.GetProperty("property");

        EXPECT_TRUE(property.GetComment().empty());
    }

    class IniParserPropertyEmptyValueAndCommentTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniParserPropertyEmptyValueAndCommentTest,
           Parse_PropertyWithEmptyValueAndComment_PropertyValueAndCommentIsEmpty)
    {
        IniParser parser = CreateParser();
        const std::string ini = "[section]\nproperty=#" + GetParam();

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");
        const IniProperty property = section.GetProperty("property");

        EXPECT_TRUE(property.GetValue().empty());
        EXPECT_TRUE(property.GetComment().empty());
    }

    INSTANTIATE_TEST_SUITE_P(IniParserTest, IniParserPropertyEmptyValueAndCommentTest,
                             ::testing::Values("", " ", "\t"));

    TEST(IniParserTest, Parse_PropertyWithDelimitedValueAndCleanDelimitedValuesIsTrue_PropertyValueIsCleaned)
    {
        IniParser parser = CreateParser();

        IniParserOptions& options = parser.GetOptions();
        options.cleanQuotedValues = true;

        const std::string ini = "[section]\nproperty=#value# # comment";

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");
        const IniProperty property = section.GetProperty("property");

        EXPECT_EQ(property.GetValue(), "value");
        EXPECT_EQ(property.GetComment(), "comment");
    }

    // -------------------------------------------------------------------------
    // Parse - multi-line values
    // -------------------------------------------------------------------------

    class IniParserMultiLineValueTest : public ::testing::TestWithParam<char>
    {
    };

    TEST_P(IniParserMultiLineValueTest, Parse_PropertyWithMultiLineValue_PropertyHasMultiLineValue)
    {
        IniParser parser = CreateParser();

        IniParserOptions& options = parser.GetOptions();
        options.allowMultiLineValues = true;

        IniScheme& scheme = parser.GetScheme();
        const char delimiter = GetParam();
        scheme.multiLineValueDelimiter = delimiter;

        const std::string ini = "[section]\nproperty=value1 " + std::string(1, delimiter) + "\nvalue2 " +
                                std::string(1, delimiter) + "\nvalue3";

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");
        const IniProperty property = section.GetProperty("property");

        EXPECT_EQ(property.GetValue(), "value1\nvalue2\nvalue3");
    }

    INSTANTIATE_TEST_SUITE_P(IniParserTest, IniParserMultiLineValueTest, ::testing::Values('-', '^', '$', '&', '\\'));

    TEST(IniParserTest, Parse_PropertyWithMultiLineValueWithoutDelimiter_PropertyHasMultiLineValue)
    {
        IniParser parser = CreateParser();

        IniParserOptions& options = parser.GetOptions();
        options.allowMultiLineValues = true;

        IniScheme& scheme = parser.GetScheme();
        scheme.multiLineValueDelimiter = '\0';

        const std::string ini = "[section]\nproperty=value1\nvalue2\nvalue3";

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");
        const IniProperty property = section.GetProperty("property");

        EXPECT_EQ(property.GetValue(), "value1\nvalue2\nvalue3");
    }

    TEST(IniParserTest, Parse_MultiLineValueWithoutProperty_ThrowsFormatError)
    {
        IniParser parser = CreateParser();

        IniParserOptions& options = parser.GetOptions();
        options.allowMultiLineValues = true;

        const std::string ini = "[section]\nvalue1 \\\nvalue2 \\\nvalue3";

        EXPECT_THROW(
            {
                try
                {
                    parser.Parse(ini);
                }
                catch (const std::format_error& ex)
                {
                    EXPECT_STREQ(ex.what(), "Error on line 2: invalid INI-formatted text.");
                    throw;
                }
            },
            std::format_error);
    }

    TEST(IniParserTest, Parse_PropertyWithMultiLineValueAndAllowMultiLineValuesIsFalse_ThrowsFormatError)
    {
        IniParser parser = CreateParser();

        IniParserOptions& options = parser.GetOptions();
        options.allowMultiLineValues = false;

        const std::string ini = "[section]\nproperty=value1 \\\nvalue2 \\\nvalue3";

        EXPECT_THROW(
            {
                try
                {
                    parser.Parse(ini);
                }
                catch (const std::format_error& ex)
                {
                    EXPECT_STREQ(ex.what(), "Error on line 2: multi-line values are not allowed.");
                    throw;
                }
            },
            std::format_error);
    }

    TEST(IniParserTest, Parse_PropertyWithMultiLineValueAndCommentLines_CommentLinesAreIgnored)
    {
        IniParser parser = CreateParser();

        IniParserOptions& options = parser.GetOptions();
        options.allowMultiLineValues = true;

        const std::string ini =
            "[section]\nproperty=value1 \\\n# value comment 1\nvalue2 \\\n# value comment 2\nvalue3";

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");
        const IniProperty property = section.GetProperty("property");

        EXPECT_TRUE(section.GetComments().empty());
        EXPECT_TRUE(property.GetComment().empty());
        EXPECT_EQ(property.GetValue(), "value1\nvalue2\nvalue3");
    }

    TEST(IniParserTest, Parse_PropertyWithMultiLineValueAndInlineComment_PropertyHasComment)
    {
        IniParser parser = CreateParser();

        IniParserOptions& options = parser.GetOptions();
        options.allowMultiLineValues = true;

        const std::string ini = "[section]\nproperty=value1 \\ # comment1\nvalue2 \\ # comment2\nvalue3 # comment3";

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");
        const IniProperty property = section.GetProperty("property");

        EXPECT_EQ(property.GetComment(), "comment1\ncomment2\ncomment3");
        EXPECT_EQ(property.GetValue(), "value1\nvalue2\nvalue3");
    }

    TEST(IniParserTest, Parse_PropertyWithMultiLineValueAndInlineCommentAndParseCommentsIsFalse_PropertyCommentIsEmpty)
    {
        IniParser parser = CreateParser();

        IniParserOptions& options = parser.GetOptions();
        options.allowMultiLineValues = true;
        options.parseComments = false;

        const std::string ini = "[section]\nproperty=value1 \\ # comment1\nvalue2 \\ # comment2\nvalue3 # comment3";

        const IniData iniData = parser.Parse(ini);
        const IniSection section = iniData.GetSection("section");
        const IniProperty property = section.GetProperty("property");

        EXPECT_TRUE(property.GetComment().empty());
    }

    // -------------------------------------------------------------------------
    // Parse - custom scheme
    // -------------------------------------------------------------------------

    TEST(IniParserTest, Parse_WithIniSchemeConfigured_ReturnsExpectedIniData)
    {
        IniParser parser = CreateParser();

        IniParserOptions& options = parser.GetOptions();
        options.allowMultiLineValues = true;

        IniScheme& scheme = parser.GetScheme();
        scheme.sectionStartDelimiter = '<';
        scheme.sectionEndDelimiter = '>';
        scheme.propertyAssignmentDelimiter = ':';
        scheme.commentDelimiter = ';';
        scheme.multiLineValueDelimiter = '-';

        const std::string ini = "<section>\nproperty1:value1 - ; comment1\nvalue2 ; comment2";

        const IniData iniData = parser.Parse(ini);

        IniData expected;
        IniSection section("section");
        section.SetLineNumber(1);
        IniProperty property("property1", "value1\nvalue2", "comment1\ncomment2");
        property.SetLineNumber(2);
        section.AddProperty(property);
        expected.AddSection(std::move(section));

        EXPECT_EQ(iniData, expected);
    }

    // -------------------------------------------------------------------------
    // Parse - stream
    // -------------------------------------------------------------------------

    TEST(IniParserTest, Parse_ValidStream_KeepsStreamOpen)
    {
        IniParser parser = CreateParser();

        std::istringstream stream("");
        parser.Parse(stream);

        EXPECT_FALSE(stream.bad());
    }

    TEST(IniParserTest, Parse_AnsiEncodedTextWithUnicodeCharacters_ReadsFromStream)
    {
        IniParser parser = CreateParser();

        const std::string ini = "[section]\nproperty1=value¹²³";
        std::istringstream stream(ini);

        const IniData iniData = parser.Parse(stream);
        const IniSection section = iniData.GetSection("section");
        const IniProperty property = section.GetProperty("property1");

        EXPECT_EQ(property.GetValue(), "value¹²³");
    }

} // namespace ini::test