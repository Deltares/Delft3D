#include <gtest/gtest.h>

#include <string_view>

#include "test_utilities.hpp"
#include "NF2FF_reader.hpp"

namespace
{
    constexpr std::string_view valid_xml = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
     </discharge>
     <NFResult>
        <sinks>
            250.000 350.087 9.700 1.000 0.000 0.000
            252.500 350.048 9.700 5     0.250 0.380
            252.500 350.048 9.700 5     0.250 0.380 0.95
            252.500 350.048 9.700 5     0.250 0.380 2.345, 3.142
            252.500 350.048 9.700 5     0.250 0.380 2.345, 3.142, 0.95
        </sinks>
        <sources>
            1050.000 350.365 5.000 5.000 5 15.000
            1050.500 350.365 5.000 5.000 5 15.000
        </sources>
     </NFResult>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_no_discharge = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_no_intake_flow = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
     </discharge>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_empty_intake_flow = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake></Qintake>
     </discharge>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_wrong_intake_flow = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>abcd</Qintake>
     </discharge>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_surplus_intake_flow = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0 11.0</Qintake>
     </discharge>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_no_source_flow = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
     </discharge>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_empty_source_flow = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource></Qsource>
     </discharge>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_wrong_source_flow = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>abcd</Qsource>
     </discharge>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_surplus_source_flow = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0 11.0</Qsource>
     </discharge>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_no_operator = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
     </discharge>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_empty_operator = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator></constituentsOperator>
     </discharge>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_wrong_operator = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>abcd</constituentsOperator>
     </discharge>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_no_constituents = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
     </discharge>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_empty_constituents = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents></constituents>
     </discharge>
 </NF2FF>)";

    constexpr std::string_view invalid_xml_wrong_constituents = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 abcd</constituents>
     </discharge>
 </NF2FF>)";

    constexpr std::string_view invalid_xml_no_nfresult = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
     </discharge>
 </NF2FF>)";

    constexpr std::string_view invalid_xml_no_sources = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
     </discharge>
     <NFResult>
     </NFResult>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_empty_sources = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
     </discharge>
     <NFResult>
        <sources>
        </sources>
     </NFResult>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_wrong_sources = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
     </discharge>
     <NFResult>
        <sources>
            1050.000 350.365 abcd  5.000 5 15.000
            1050.500 350.365 5.000 5.000 5 15.000
        </sources>
     </NFResult>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_wrong_sources_too_few_values = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
     </discharge>
     <NFResult>
        <sources>
            1050.000 350.365 5.000 5.000 5
            1050.500 350.365 5.000 5.000 5
        </sources>
     </NFResult>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_wrong_sources_too_many_values = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
     </discharge>
     <NFResult>
        <sources>
            1050.000 350.365 5.000 5.000 5 15.000 0.95
            1050.500 350.365 5.000 5.000 5 15.000 0.05
        </sources>
     </NFResult>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_no_sinks = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
     </discharge>
     <NFResult>
        <sources>
            1050.000 350.365 5.000 5.000 5 15.000
            1050.500 350.365 5.000 5.000 5 15.000
        </sources>
     </NFResult>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_empty_sinks = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
     </discharge>
     <NFResult>
        <sinks>
        </sinks>
        <sources>
            1050.000 350.365 5.000 5.000 5 15.000
            1050.500 350.365 5.000 5.000 5 15.000
        </sources>
     </NFResult>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_wrong_sinks = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
     </discharge>
     <NFResult>
        <sinks>
            250.000 350.087 9.700 1.000 0.000 0.000
            252.500 350.048 abcd  5     0.250 0.380
        </sinks>
        <sources>
            1050.000 350.365 5.000 5.000 5 15.000
            1050.500 350.365 5.000 5.000 5 15.000
        </sources>
     </NFResult>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_wrong_sinks_too_few_values = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
     </discharge>
     <NFResult>
        <sinks>
            250.000 350.087 9.700 1.000 0.000
            252.500 350.048 9.700 5     0.250
        </sinks>
        <sources>
            1050.000 350.365 5.000 5.000 5 15.000
            1050.500 350.365 5.000 5.000 5 15.000
        </sources>
     </NFResult>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_wrong_sinks_too_many_values = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
     </discharge>
     <NFResult>
        <sinks>
            250.000 350.087 9.700 1.000 0.000 0.000 2.345, 3.142, 0.05, 1.000
            252.500 350.048 9.700 5     0.250 0.380 2.345, 3.142, 0.95, 1.000
        </sinks>
        <sources>
            1050.000 350.365 5.000 5.000 5 15.000
            1050.500 350.365 5.000 5.000 5 15.000
        </sources>
     </NFResult>
  </NF2FF>)";

} // namespace

TEST(NF2FFReaderTest, ParsesFileVersion)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    EXPECT_EQ(result->fileVersion(), "0.3");
}

TEST(NF2FFReaderTest, ReturnsErrorOnInvalidXml)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString("not valid xml at all <<<");
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::starts_with, result.error().message, "Failed to parse XML: ");
}

TEST(NF2FFReaderTest, ReturnsErrorOnWrongRootElement)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?><notvalid><fileVersion>0.3</fileVersion></notvalid>)";
    const auto result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Root element must be <NF2FF>, got: <notvalid>");
}

TEST(NF2FFReaderTest, ReturnsErrorOnMissingFileVersion)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?><NF2FF></NF2FF>)";
    const auto result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Required element <fileVersion> not found");
}

TEST(NF2FFReaderTest, ReturnsErrorOnEmptyFileVersion)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?><NF2FF><fileVersion></fileVersion></NF2FF>)";
    const auto result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Element <fileVersion> is empty");
}

TEST(NF2FFReaderTest, ReturnsErrorOnMissingDischarge)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_no_discharge);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Required element <discharge> not found");
}

TEST(NF2FFReaderTest, ParsesQintake)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    const auto& value = result.value().intakeFlowRate();
    EXPECT_DOUBLE_EQ(value, 10.0);
}

TEST(NF2FFReaderTest, ReturnsErrorEmptyQintake)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_empty_intake_flow);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Element <Qintake> is empty");
}

TEST(NF2FFReaderTest, ReturnsErrorOnWrongQintake)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_wrong_intake_flow);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "<Qintake> contains invalid token: 'abcd'");
}

TEST(NF2FFReaderTest, ReturnsErrorOnSurplusQintake)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_surplus_intake_flow);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "<Qintake> must contain exactly one numeric value");
}

TEST(NF2FFReaderTest, ReturnsErrorOnMissingQintake)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_no_intake_flow);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Required element <Qintake> not found");
}

TEST(NF2FFReaderTest, ParsesQsource)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    const auto& value = result.value().sourceFlowRate();
    EXPECT_DOUBLE_EQ(value, 10.0);
}

TEST(NF2FFReaderTest, ReturnsErrorEmptyQsource)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_empty_source_flow);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Element <Qsource> is empty");
}

TEST(NF2FFReaderTest, ReturnsErrorOnWrongQsource)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_wrong_source_flow);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "<Qsource> contains invalid token: 'abcd'");
}

TEST(NF2FFReaderTest, ReturnsErrorOnSurplusQsource)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_surplus_source_flow);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "<Qsource> must contain exactly one numeric value");
}

TEST(NF2FFReaderTest, ReturnsErrorOnMissingQsource)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_no_source_flow);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Required element <Qsource> not found");
}

TEST(NF2FFReaderTest, ParsesOperator)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    const auto& value = result.value().constituentsOperator();
    EXPECT_TRUE(value == pre_c_sumo::ConstituentsOperator::Excess);
}

TEST(NF2FFReaderTest, ReturnsErrorOnEmptyOperator)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_empty_operator);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Element <constituentsOperator> is empty");
}

TEST(NF2FFReaderTest, ReturnsErrorOnWrongOperator)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_wrong_operator);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message,
                 "<constituentsOperator> has unknown value: 'abcd'; expected 'absolute' or 'excess'");
}

TEST(NF2FFReaderTest, ReturnsErrorOnMissingOperator)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_no_operator);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Required element <constituentsOperator> not found");
}

TEST(NF2FFReaderTest, ParsesConstituents)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    const auto& value = result.value().constituents();
    ASSERT_EQ(value.size(), 2u);
    EXPECT_DOUBLE_EQ(value[0], 10.0);
    EXPECT_DOUBLE_EQ(value[1], 0.0);
}

TEST(NF2FFReaderTest, ReturnsErrorOnEmptyConstituents)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_empty_constituents);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Element <constituents> is empty");
}

TEST(NF2FFReaderTest, ReturnsErrorOnWrongConstituents)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_wrong_constituents);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "<constituents> contains invalid token: 'abcd'");
}

TEST(NF2FFReaderTest, ReturnsErrorOnMissingConstituents)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_no_constituents);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Required element <constituents> not found");
}

TEST(NF2FFReaderTest, ReturnsErrorOnMissingNFResult)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_no_nfresult);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Required element <NFResult> not found");
}

TEST(NF2FFReaderTest, ParsesSources)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    const auto& sources = result.value().sources();
    ASSERT_EQ(sources.size(), 2u);
    EXPECT_DOUBLE_EQ(sources[0].x_coordinate, 1050.000);
    EXPECT_DOUBLE_EQ(sources[0].y_coordinate, 350.365);
    EXPECT_DOUBLE_EQ(sources[0].z_coordinate, 5.000);
    EXPECT_DOUBLE_EQ(sources[0].entrainment, 5.000);
    EXPECT_DOUBLE_EQ(sources[0].half_plume_height, 5);
    EXPECT_FALSE(sources[0].has_u);
    EXPECT_FALSE(sources[0].has_weight);
    EXPECT_DOUBLE_EQ(sources[0].half_plume_width, 15.000);
    EXPECT_DOUBLE_EQ(sources[1].x_coordinate, 1050.500);
    EXPECT_DOUBLE_EQ(sources[1].y_coordinate, 350.365);
    EXPECT_DOUBLE_EQ(sources[1].z_coordinate, 5.000);
    EXPECT_DOUBLE_EQ(sources[1].entrainment, 5.000);
    EXPECT_DOUBLE_EQ(sources[1].half_plume_height, 5);
    EXPECT_DOUBLE_EQ(sources[1].half_plume_width, 15.000);
    EXPECT_FALSE(sources[1].has_u);
    EXPECT_FALSE(sources[1].has_weight);
}

TEST(NF2FFReaderTest, ReturnsErrorOnMissingSources)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_no_sources);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Required element <sources> not found");
}

TEST(NF2FFReaderTest, ReturnsErrorOnEmptySources)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_empty_sources);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Element <sources> is empty");
}

TEST(NF2FFReaderTest, ReturnsErrorOnWrongSources)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_wrong_sources);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "<sources> contains invalid token: 'abcd'");
}

TEST(NF2FFReaderTest, ReturnsErrorOnWrongSourcesTooFewValues)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_wrong_sources_too_few_values);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message,
                 "Found line in <sources> with 5 values; expected 6 to 6 values");
}

TEST(NF2FFReaderTest, ReturnsErrorOnWrongSourcesTooManyValues)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_wrong_sources_too_many_values);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message,
                 "Found line in <sources> with 7 values; expected 6 to 6 values");
}

TEST(NF2FFReaderTest, ParsesSinks)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    const auto& sinks = result.value().sinks();
    ASSERT_EQ(sinks.size(), 5u);
    EXPECT_DOUBLE_EQ(sinks[0].x_coordinate, 250.000);
    EXPECT_DOUBLE_EQ(sinks[0].y_coordinate, 350.087);
    EXPECT_DOUBLE_EQ(sinks[0].z_coordinate, 9.700);
    EXPECT_DOUBLE_EQ(sinks[0].entrainment, 1.000);
    EXPECT_DOUBLE_EQ(sinks[0].half_plume_height, 0.000);
    EXPECT_DOUBLE_EQ(sinks[0].half_plume_width, 0.000);
    EXPECT_FALSE(sinks[0].has_u);
    EXPECT_FALSE(sinks[0].has_weight);
    EXPECT_DOUBLE_EQ(sinks[1].x_coordinate, 252.500);
    EXPECT_DOUBLE_EQ(sinks[1].y_coordinate, 350.048);
    EXPECT_DOUBLE_EQ(sinks[1].z_coordinate, 9.700);
    EXPECT_DOUBLE_EQ(sinks[1].entrainment, 5);
    EXPECT_DOUBLE_EQ(sinks[1].half_plume_height, 0.250);
    EXPECT_DOUBLE_EQ(sinks[1].half_plume_width, 0.380);
    EXPECT_FALSE(sinks[1].has_u);
    EXPECT_FALSE(sinks[1].has_weight);
    EXPECT_DOUBLE_EQ(sinks[2].x_coordinate, 252.500);
    EXPECT_DOUBLE_EQ(sinks[2].y_coordinate, 350.048);
    EXPECT_DOUBLE_EQ(sinks[2].z_coordinate, 9.700);
    EXPECT_DOUBLE_EQ(sinks[2].entrainment, 5);
    EXPECT_DOUBLE_EQ(sinks[2].half_plume_height, 0.250);
    EXPECT_DOUBLE_EQ(sinks[2].half_plume_width, 0.380);
    EXPECT_DOUBLE_EQ(sinks[2].weight, 0.95);
    EXPECT_FALSE(sinks[2].has_u);
    EXPECT_TRUE(sinks[2].has_weight);
    EXPECT_DOUBLE_EQ(sinks[3].x_coordinate, 252.500);
    EXPECT_DOUBLE_EQ(sinks[3].y_coordinate, 350.048);
    EXPECT_DOUBLE_EQ(sinks[3].z_coordinate, 9.700);
    EXPECT_DOUBLE_EQ(sinks[3].entrainment, 5);
    EXPECT_DOUBLE_EQ(sinks[3].half_plume_height, 0.250);
    EXPECT_DOUBLE_EQ(sinks[3].half_plume_width, 0.380);
    EXPECT_DOUBLE_EQ(sinks[3].u_magnitude, 2.345);
    EXPECT_DOUBLE_EQ(sinks[3].u_direction, 3.142);
    EXPECT_TRUE(sinks[3].has_u);
    EXPECT_FALSE(sinks[3].has_weight);
    EXPECT_DOUBLE_EQ(sinks[4].x_coordinate, 252.500);
    EXPECT_DOUBLE_EQ(sinks[4].y_coordinate, 350.048);
    EXPECT_DOUBLE_EQ(sinks[4].z_coordinate, 9.700);
    EXPECT_DOUBLE_EQ(sinks[4].entrainment, 5);
    EXPECT_DOUBLE_EQ(sinks[4].half_plume_height, 0.250);
    EXPECT_DOUBLE_EQ(sinks[4].half_plume_width, 0.380);
    EXPECT_DOUBLE_EQ(sinks[4].u_magnitude, 2.345);
    EXPECT_DOUBLE_EQ(sinks[4].u_direction, 3.142);
    EXPECT_DOUBLE_EQ(sinks[4].weight, 0.95);
    EXPECT_TRUE(sinks[4].has_u);
    EXPECT_TRUE(sinks[4].has_weight);
}

TEST(NF2FFReaderTest, ReturnsErrorOnMissingSinks)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_no_sinks);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Required element <sinks> not found");
}

TEST(NF2FFReaderTest, ReturnsErrorOnEmptySinks)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_empty_sinks);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Element <sinks> is empty");
}

TEST(NF2FFReaderTest, ReturnsErrorOnWrongSinks)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(invalid_xml_wrong_sinks);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "<sinks> contains invalid token: 'abcd'");
}
