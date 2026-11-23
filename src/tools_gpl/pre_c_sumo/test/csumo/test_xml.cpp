#include <gtest/gtest.h>

#include "csumo/xml.hpp"
#include "utils/xml_parsing.hpp"

TEST(CSumoXmlTest, ParsesRequiredAndOptionalPointChildren)
{
    pugi::xml_document document;
    ASSERT_TRUE(document.load_string("<root><point>3 4</point><invalidPoint>3</invalidPoint></root>"));
    const auto root = document.document_element();

    const auto point = pre_c_sumo::c_sumo::xml::requiredChildPoint2D(root, "point");
    ASSERT_TRUE(point.has_value());
    EXPECT_DOUBLE_EQ(point->x_coordinate, 3.0);
    EXPECT_DOUBLE_EQ(point->y_coordinate, 4.0);
    const auto missing = pre_c_sumo::c_sumo::xml::requiredChildPoint2D(root, "missingPoint");
    ASSERT_FALSE(missing.has_value());
    EXPECT_EQ(missing.error().message, "Required element <missingPoint> not found");
    const auto invalid = pre_c_sumo::c_sumo::xml::requiredChildPoint2D(root, "invalidPoint");
    ASSERT_FALSE(invalid.has_value());
    EXPECT_EQ(invalid.error().message, "<invalidPoint> must contain two numeric values, got: '3'");
    const auto optional = pre_c_sumo::c_sumo::xml::optionalChildPoint2D(root, "point");
    ASSERT_TRUE(optional.has_value());
    EXPECT_DOUBLE_EQ(optional->x_coordinate, 3.0);
    EXPECT_DOUBLE_EQ(optional->y_coordinate, 4.0);
    EXPECT_FALSE(pre_c_sumo::c_sumo::xml::optionalChildPoint2D(root, "missingPoint").has_value());
    EXPECT_FALSE(pre_c_sumo::c_sumo::xml::optionalChildPoint2D(root, "invalidPoint").has_value());
}
TEST(CSumoXmlTest, ParsesConstituentsOperatorCaseInsensitively)
{
    const auto document = pre_c_sumo::utils::xml_parsing::parseXmlDocument(
        "<discharge><constituentsOperator>AbSolute</constituentsOperator></discharge>");
    ASSERT_TRUE(document.has_value()) << document.error().message;

    const auto result = pre_c_sumo::c_sumo::xml::parseConstituentsOperator(document->document_element());
    ASSERT_TRUE(result.has_value()) << result.error().message;
    EXPECT_EQ(*result, pre_c_sumo::c_sumo::ConstituentsOperator::Absolute);
}

TEST(CSumoXmlTest, ParsesExcessConstituentsOperator)
{
    const auto document = pre_c_sumo::utils::xml_parsing::parseXmlDocument(
        "<discharge><constituentsOperator>EXCESS</constituentsOperator></discharge>");
    ASSERT_TRUE(document.has_value()) << document.error().message;

    const auto result = pre_c_sumo::c_sumo::xml::parseConstituentsOperator(document->document_element());
    ASSERT_TRUE(result.has_value()) << result.error().message;
    EXPECT_EQ(*result, pre_c_sumo::c_sumo::ConstituentsOperator::Excess);
}

TEST(CSumoXmlTest, ReturnsErrorForUnknownConstituentsOperator)
{
    const auto document = pre_c_sumo::utils::xml_parsing::parseXmlDocument(
        "<discharge><constituentsOperator>unknown</constituentsOperator></discharge>");
    ASSERT_TRUE(document.has_value()) << document.error().message;

    const auto result = pre_c_sumo::c_sumo::xml::parseConstituentsOperator(document->document_element());
    ASSERT_FALSE(result.has_value());
    EXPECT_EQ(result.error().message,
              "<constituentsOperator> has unknown value: 'unknown'; expected 'absolute' or 'excess'");
}

TEST(CSumoXmlTest, ReturnsErrorWhenConstituentsOperatorIsMissing)
{
    const auto document = pre_c_sumo::utils::xml_parsing::parseXmlDocument("<discharge/>");
    ASSERT_TRUE(document.has_value()) << document.error().message;

    const auto result = pre_c_sumo::c_sumo::xml::parseConstituentsOperator(document->document_element());
    ASSERT_FALSE(result.has_value());
    EXPECT_EQ(result.error().message, "Required element <constituentsOperator> not found");
}
