#include <gtest/gtest.h>

#include "csumo_xml_utils.hpp"
#include "parsing_utils.hpp"

TEST(CSumoXmlUtilsTest, ParsesConstituentsOperatorCaseInsensitively)
{
    const auto document = parsing_utils::parseXmlDocument(
        "<discharge><constituentsOperator>AbSolute</constituentsOperator></discharge>");
    ASSERT_TRUE(document.has_value()) << document.error().message;

    const auto result = pre_c_sumo::xml_utils::parseConstituentsOperator(document->document_element());
    ASSERT_TRUE(result.has_value()) << result.error().message;
    EXPECT_EQ(*result, pre_c_sumo::ConstituentsOperator::Absolute);
}

TEST(CSumoXmlUtilsTest, ParsesExcessConstituentsOperator)
{
    const auto document =
        parsing_utils::parseXmlDocument("<discharge><constituentsOperator>EXCESS</constituentsOperator></discharge>");
    ASSERT_TRUE(document.has_value()) << document.error().message;

    const auto result = pre_c_sumo::xml_utils::parseConstituentsOperator(document->document_element());
    ASSERT_TRUE(result.has_value()) << result.error().message;
    EXPECT_EQ(*result, pre_c_sumo::ConstituentsOperator::Excess);
}

TEST(CSumoXmlUtilsTest, ReturnsErrorForUnknownConstituentsOperator)
{
    const auto document =
        parsing_utils::parseXmlDocument("<discharge><constituentsOperator>unknown</constituentsOperator></discharge>");
    ASSERT_TRUE(document.has_value()) << document.error().message;

    const auto result = pre_c_sumo::xml_utils::parseConstituentsOperator(document->document_element());
    ASSERT_FALSE(result.has_value());
    EXPECT_EQ(result.error().message,
              "<constituentsOperator> has unknown value: 'unknown'; expected 'absolute' or 'excess'");
}

TEST(CSumoXmlUtilsTest, ReturnsErrorWhenConstituentsOperatorIsMissing)
{
    const auto document = parsing_utils::parseXmlDocument("<discharge/>");
    ASSERT_TRUE(document.has_value()) << document.error().message;

    const auto result = pre_c_sumo::xml_utils::parseConstituentsOperator(document->document_element());
    ASSERT_FALSE(result.has_value());
    EXPECT_EQ(result.error().message, "Required element <constituentsOperator> not found");
}
