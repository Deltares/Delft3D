#include <gtest/gtest.h>

#include <array>
#include <string_view>
#include <vector>

#include "xml_parsing_utils.hpp"

TEST(XmlParsingUtilsTest, ParseXmlDocumentHandlesValidAndInvalidXml)
{
    const auto document = xml_parsing_utils::parseXmlDocument("<root/>");
    ASSERT_TRUE(document.has_value());
    EXPECT_STREQ(document->document_element().name(), "root");

    const auto invalid_document = xml_parsing_utils::parseXmlDocument("<root>");
    ASSERT_FALSE(invalid_document.has_value());
    EXPECT_TRUE(invalid_document.error().message.starts_with("Failed to parse XML: "));
}

TEST(XmlParsingUtilsTest, ValidateRootAcceptsAliasesCaseInsensitively)
{
    const auto document = xml_parsing_utils::parseXmlDocument("<csumo/>");
    ASSERT_TRUE(document.has_value());

    const auto root = xml_parsing_utils::validateRoot(*document, {"COSUMO", "CSUMO"});
    ASSERT_TRUE(root.has_value());
    EXPECT_STREQ(root->name(), "csumo");

    const auto wrong_root = xml_parsing_utils::validateRoot(*document, {"NF2FF"});
    ASSERT_FALSE(wrong_root.has_value());
    EXPECT_EQ(wrong_root.error().message, "Root element must be <NF2FF>, got: <csumo>");

    const auto no_expected_names = xml_parsing_utils::validateRoot(*document, {});
    ASSERT_FALSE(no_expected_names.has_value());
    EXPECT_EQ(no_expected_names.error().message, "At least one expected root element name must be provided");

    pugi::xml_document empty_document;
    const auto missing_root = xml_parsing_utils::validateRoot(empty_document, {"root"});
    ASSERT_FALSE(missing_root.has_value());
    EXPECT_EQ(missing_root.error().message, "XML document is empty");
}

TEST(XmlParsingUtilsTest, NodeTextAndTypedChildParsersUseSharedXmlHelpers)
{
    const auto document = xml_parsing_utils::parseXmlDocument(
        "<root><flow>1.25</flow><invalidFlow>1x</invalidFlow>"
        "<point>3 4</point><invalidPoint>3</invalidPoint></root>");
    ASSERT_TRUE(document.has_value());
    const auto root = document->document_element();
    const auto point_node = xml_parsing_utils::findChild(root, "POINT");
    ASSERT_TRUE(point_node);
    EXPECT_EQ(xml_parsing_utils::nodeText(point_node), "3 4");

    const auto flow = xml_parsing_utils::requiredChildDouble(root, "flow");
    ASSERT_TRUE(flow.has_value());
    EXPECT_DOUBLE_EQ(*flow, 1.25);
    const auto missing_flow = xml_parsing_utils::requiredChildDouble(root, "missingFlow");
    ASSERT_FALSE(missing_flow.has_value());
    EXPECT_EQ(missing_flow.error().message, "Required element <missingFlow> not found");
    const auto invalid_flow = xml_parsing_utils::requiredChildDouble(root, "invalidFlow");
    ASSERT_FALSE(invalid_flow.has_value());
    EXPECT_EQ(invalid_flow.error().message, "<invalidFlow> contains invalid token: '1x'");
}

TEST(XmlParsingUtilsTest, ParseDoubleVectorAcceptsWhitespaceAndScientificNotation)
{
    const auto values = xml_parsing_utils::parseDoubleVector("\n 1.25\t-2.5e+1\r\n 3E-2 \n", "values");

    ASSERT_TRUE(values.has_value()) << values.error().message;
    EXPECT_EQ(*values, (std::vector<double>{1.25, -25.0, 0.03}));
}

TEST(XmlParsingUtilsTest, ParseDoubleVectorRejectsPartiallyParsedTokens)
{
    constexpr std::array<std::string_view, 8> inputs = {"1junk", "1.2.3",   "1e",        "1e+",
                                                        "1e-",   "1e2junk", "10 20x 30", "1.0, 2.0"};

    for (const auto text : inputs)
    {
        SCOPED_TRACE(text);
        const auto values = xml_parsing_utils::parseDoubleVector(text, "values");
        EXPECT_FALSE(values.has_value());
    }
}

TEST(XmlParsingUtilsTest, ParseDoubleVectorRejectsNonFiniteValues)
{
    for (const auto text : {std::string_view{"nan"}, std::string_view{"inf"}, std::string_view{"-inf"}})
    {
        SCOPED_TRACE(text);
        const auto values = xml_parsing_utils::parseDoubleVector(text, "values");
        ASSERT_FALSE(values.has_value());
        EXPECT_TRUE(values.error().message.contains("non-finite"));
    }
}

