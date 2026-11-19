#include <gtest/gtest.h>

#include <string>
#include <string_view>

#include "csumo_nf2ff_reader.hpp"

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
      <intakes>
         1550.000 950.000 0.500
         1550.000 950.000 1.500
      </intakes>
        <sinks>
            250.000 350.087 9.700 1.000 0.000 0.000
            252.500 350.048 9.700 5     0.250 0.380
        </sinks>
        <sources>
            1050.000 350.365 5.000 5.000 5 15.000
            1050.500 350.365 5.000 5.000 5 15.000
            1050.500 350.365 5.000 5.000 5 15.000 0.95
            1050.500 350.365 5.000 5.000 5 15.000 2.345 3.142
            1050.500 350.365 5.000 5.000 5 15.000 2.345 3.142 0.95
        </sources>
     </NFResult>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_wrong_version = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.1</fileVersion>
  </NF2FF>)";

    constexpr std::string_view invalid_xml_no_discharge = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
  </NF2FF>)";

    constexpr std::string_view valid_xml_no_intake_flow = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
      <Qsource>10.0</Qsource>
      <constituentsOperator>excess</constituentsOperator>
      <constituents>10.0 0.0</constituents>
     </discharge>
    <NFResult>
      <sinks>
         250.000 350.087 9.700 1.000 0.000 0.000
      </sinks>
      <sources>
         1050.000 350.365 5.000 5.000 5 15.000
      </sources>
    </NFResult>
  </NF2FF>)";

    constexpr std::string_view valid_xml_empty_intakes = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>0.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
     </discharge>
     <NFResult>
        <intakes/>
        <sinks>
           250.000 350.087 9.700 1.000 0.000 0.000
        </sinks>
        <sources>
           1050.000 350.365 5.000 5.000 5 15.000
        </sources>
     </NFResult>
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

    constexpr std::string_view invalid_xml_negative_intake_flow = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>-1.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
     </discharge>
     <NFResult>
      <sinks>
         250.000 350.087 9.700 1.000 0.000 0.000
      </sinks>
      <sources>
         1050.000 350.365 5.000 5.000 5 15.000
      </sources>
    </NFResult>
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
            1050.000 350.365 5.000 5.000 5 15.000 2.345 3.142 0.95 42
            1050.500 350.365 5.000 5.000 5 15.000 2.345 3.142 0.05 42
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

    constexpr std::string_view valid_xml_empty_sinks = R"(<?xml version="1.0" encoding="utf-8"?>
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
            250.000 350.087 9.700 1.000 0.000 0.000 2.345
            252.500 350.048 9.700 5     0.250 0.380 2.345
        </sinks>
        <sources>
            1050.000 350.365 5.000 5.000 5 15.000
            1050.500 350.365 5.000 5.000 5 15.000
        </sources>
     </NFResult>
  </NF2FF>)";

} // namespace

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnInvalidXml)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString("not valid xml at all <<<");
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.starts_with("Failed to parse XML: "));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnWrongRootElement)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?><notvalid><fileVersion>0.3</fileVersion></notvalid>)";
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Root element must be <NF2FF>, got: <notvalid>"));
}

TEST(CSumoNF2FFReaderTest, ParsesFileVersion)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    EXPECT_EQ(result->file_version, "0.3");
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnMissingFileVersion)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?><NF2FF></NF2FF>)";
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Required element <fileVersion> not found"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnEmptyFileVersion)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?><NF2FF><fileVersion></fileVersion></NF2FF>)";
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Element <fileVersion> is empty"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnWrongFileVersion)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_wrong_version);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Element <fileVersion> should be 0.3, got: 0.1 instead"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnMissingDischarge)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_no_discharge);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Required element <discharge> not found"));
}

TEST(CSumoNF2FFReaderTest, ParsesQintake)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    const auto& value = result.value().intake_flow_rate;
    EXPECT_DOUBLE_EQ(value, 10.0);
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorEmptyQintake)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_empty_intake_flow);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Element <Qintake> is empty"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnWrongQintake)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_wrong_intake_flow);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("<Qintake> contains invalid token: 'abcd'"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnSurplusQintake)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_surplus_intake_flow);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("<Qintake> must contain exactly one numeric value"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnNegativeQintake)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_negative_intake_flow);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Element <Qintake> should be a value >= 0.0"));
}

TEST(CSumoNF2FFReaderTest, ParsesMissingQintakeAsZero)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(valid_xml_no_intake_flow);
    ASSERT_TRUE(result.has_value());
    EXPECT_DOUBLE_EQ(result->intake_flow_rate, 0.0);
}

TEST(CSumoNF2FFReaderTest, ParsesQsource)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    const auto& value = result.value().source_flow_rate;
    EXPECT_DOUBLE_EQ(value, 10.0);
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorEmptyQsource)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_empty_source_flow);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Element <Qsource> is empty"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnWrongQsource)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_wrong_source_flow);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("<Qsource> contains invalid token: 'abcd'"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnSurplusQsource)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_surplus_source_flow);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("<Qsource> must contain exactly one numeric value"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnMissingQsource)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_no_source_flow);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Required element <Qsource> not found"));
}

TEST(CSumoNF2FFReaderTest, ParsesOperator)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    const auto& value = result.value().constituents_operator;
    EXPECT_TRUE(value == pre_c_sumo::ConstituentsOperator::Excess);
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnEmptyOperator)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_empty_operator);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Element <constituentsOperator> is empty"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnWrongOperator)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_wrong_operator);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains(
        "<constituentsOperator> has unknown value: 'abcd'; expected 'absolute' or 'excess'"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnMissingOperator)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_no_operator);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Required element <constituentsOperator> not found"));
}

TEST(CSumoNF2FFReaderTest, ParsesConstituents)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    const auto& value = result.value().constituents;
    ASSERT_EQ(value.size(), 2u);
    EXPECT_DOUBLE_EQ(value[0], 10.0);
    EXPECT_DOUBLE_EQ(value[1], 0.0);
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnEmptyConstituents)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_empty_constituents);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Element <constituents> is empty"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnWrongConstituents)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_wrong_constituents);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("<constituents> contains invalid token: 'abcd'"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnMissingConstituents)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_no_constituents);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Required element <constituents> not found"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnMissingNFResult)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_no_nfresult);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Required element <NFResult> not found"));
}

TEST(CSumoNF2FFReaderTest, ParsesSources)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    const auto& sources = result.value().sources;
    ASSERT_EQ(sources.size(), 5u);
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
    EXPECT_DOUBLE_EQ(sources[2].x_coordinate, 1050.500);
    EXPECT_DOUBLE_EQ(sources[2].y_coordinate, 350.365);
    EXPECT_DOUBLE_EQ(sources[2].z_coordinate, 5.000);
    EXPECT_DOUBLE_EQ(sources[2].entrainment, 5.000);
    EXPECT_DOUBLE_EQ(sources[2].half_plume_height, 5);
    EXPECT_DOUBLE_EQ(sources[2].half_plume_width, 15.000);
    EXPECT_DOUBLE_EQ(sources[2].weight, 0.95);
    EXPECT_FALSE(sources[2].has_u);
    EXPECT_TRUE(sources[2].has_weight);
    EXPECT_DOUBLE_EQ(sources[3].x_coordinate, 1050.500);
    EXPECT_DOUBLE_EQ(sources[3].y_coordinate, 350.365);
    EXPECT_DOUBLE_EQ(sources[3].z_coordinate, 5.000);
    EXPECT_DOUBLE_EQ(sources[3].entrainment, 5.000);
    EXPECT_DOUBLE_EQ(sources[3].half_plume_height, 5);
    EXPECT_DOUBLE_EQ(sources[3].half_plume_width, 15.000);
    EXPECT_DOUBLE_EQ(sources[3].u_magnitude, 2.345);
    EXPECT_DOUBLE_EQ(sources[3].u_direction, 3.142);
    EXPECT_TRUE(sources[3].has_u);
    EXPECT_FALSE(sources[3].has_weight);
    EXPECT_DOUBLE_EQ(sources[4].x_coordinate, 1050.500);
    EXPECT_DOUBLE_EQ(sources[4].y_coordinate, 350.365);
    EXPECT_DOUBLE_EQ(sources[4].z_coordinate, 5.000);
    EXPECT_DOUBLE_EQ(sources[4].entrainment, 5.000);
    EXPECT_DOUBLE_EQ(sources[4].half_plume_height, 5);
    EXPECT_DOUBLE_EQ(sources[4].half_plume_width, 15.000);
    EXPECT_DOUBLE_EQ(sources[4].u_magnitude, 2.345);
    EXPECT_DOUBLE_EQ(sources[4].u_direction, 3.142);
    EXPECT_DOUBLE_EQ(sources[4].weight, 0.95);
    EXPECT_TRUE(sources[4].has_u);
    EXPECT_TRUE(sources[4].has_weight);
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnMissingSources)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_no_sources);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Required element <sources> not found"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnEmptySources)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_empty_sources);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Element <sources> is empty"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnWrongSources)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_wrong_sources);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("<sources> contains invalid token: 'abcd'"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnWrongSourcesTooFewValues)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_wrong_sources_too_few_values);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Found line in <sources> with 5 values; expected 6 to 9 values"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnWrongSourcesTooManyValues)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_wrong_sources_too_many_values);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Found line in <sources> with 10 values; expected 6 to 9 values"));
}

TEST(CSumoNF2FFReaderTest, ParsesSinks)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    const auto& sinks = result.value().sinks;
    ASSERT_EQ(sinks.size(), 2u);
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
}

TEST(CSumoNF2FFReaderTest, ParsesIntakes)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    const auto& intakes = result.value().intakes;
    ASSERT_EQ(intakes.size(), 2u);
    EXPECT_DOUBLE_EQ(intakes[0].x_coordinate, 1550.000);
    EXPECT_DOUBLE_EQ(intakes[0].y_coordinate, 950.000);
    EXPECT_DOUBLE_EQ(intakes[0].z_coordinate, 0.500);
    EXPECT_FALSE(intakes[0].has_weight);
    EXPECT_DOUBLE_EQ(intakes[1].x_coordinate, 1550.000);
    EXPECT_DOUBLE_EQ(intakes[1].y_coordinate, 950.000);
    EXPECT_DOUBLE_EQ(intakes[1].z_coordinate, 1.500);
    EXPECT_FALSE(intakes[1].has_weight);
}

TEST(CSumoNF2FFReaderTest, ParsesWeightedIntakeAndRejectsInvalidRowLengths)
{
    const auto with_intake_row = [](std::string_view row) {
        std::string xml{valid_xml};
        const auto start = xml.find("<intakes>");
        const auto end = xml.find("</intakes>", start);
        xml.replace(start + std::string_view{"<intakes>"}.size(),
                    end - start - std::string_view{"<intakes>"}.size(), row);
        return xml;
    };

    const auto weighted = pre_c_sumo::CSumoNF2FFReader::fromString(with_intake_row("\n1 2 3 0.25\n"));
    ASSERT_TRUE(weighted.has_value());
    ASSERT_EQ(weighted->intakes.size(), 1u);
    EXPECT_DOUBLE_EQ(weighted->intakes[0].z_coordinate, 3.0);
    EXPECT_TRUE(weighted->intakes[0].has_weight);
    EXPECT_DOUBLE_EQ(weighted->intakes[0].weight, 0.25);

    for (const auto row : {"\n1 2\n", "\n1 2 3 4 5\n"})
    {
        SCOPED_TRACE(row);
        const auto invalid = pre_c_sumo::CSumoNF2FFReader::fromString(with_intake_row(row));
        ASSERT_FALSE(invalid.has_value());
        EXPECT_TRUE(invalid.error().message.contains("expected 3 to 4 values"));
    }
}

TEST(CSumoNF2FFReaderTest, ParsesEmptyIntakesWhenIntakeDischargeIsZero)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(valid_xml_empty_intakes);
    ASSERT_TRUE(result.has_value());
    EXPECT_DOUBLE_EQ(result->intake_flow_rate, 0.0);
    EXPECT_TRUE(result->intakes.empty());
}

TEST(CSumoNF2FFReaderTest, ParsesEmptyIntakesWithNonzeroIntakeDischarge)
{
    std::string xml{valid_xml_empty_intakes};
    constexpr std::string_view zero_intake_flow = "<Qintake>0.0</Qintake>";
    const auto intake_flow_position = xml.find(zero_intake_flow);
    ASSERT_NE(intake_flow_position, std::string::npos);
    xml.replace(intake_flow_position, zero_intake_flow.size(), "<Qintake>10.0</Qintake>");

    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(xml);
    ASSERT_TRUE(result.has_value());
    EXPECT_DOUBLE_EQ(result->intake_flow_rate, 10.0);
    EXPECT_TRUE(result->intakes.empty());
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnMissingSinks)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_no_sinks);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Required element <sinks> not found"));
}

TEST(CSumoNF2FFReaderTest, ParsesEmptySinks)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(valid_xml_empty_sinks);
    ASSERT_TRUE(result.has_value()) << result.error().message;
    EXPECT_TRUE(result->sinks.empty());
}

TEST(CSumoNF2FFReaderTest, RejectsWhitespaceOnlySources)
{
    std::string xml{valid_xml};
    constexpr std::string_view open_tag = "<sources>";
    constexpr std::string_view close_tag = "</sources>";
    const auto tag_start = xml.find(open_tag);
    ASSERT_NE(tag_start, std::string::npos);
    const auto content_start = tag_start + open_tag.size();
    const auto content_end = xml.find(close_tag, content_start);
    ASSERT_NE(content_end, std::string::npos);
    xml.replace(content_start, content_end - content_start, " \n\t ");

    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_EQ(result.error().message, "Element <sources> is empty");
}

TEST(CSumoNF2FFReaderTest, RejectsSourceBlockWithNoParsedRows)
{
    std::string xml{valid_xml};
    constexpr std::string_view open_tag = "<sources>";
    constexpr std::string_view close_tag = "</sources>";
    const auto tag_start = xml.find(open_tag);
    ASSERT_NE(tag_start, std::string::npos);
    const auto content_start = tag_start + open_tag.size();
    const auto content_end = xml.find(close_tag, content_start);
    ASSERT_NE(content_end, std::string::npos);
    xml.replace(content_start, content_end - content_start, "<![CDATA[ \n\t ]]>");

    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_EQ(result.error().message, "Element <sources> must contain at least one data row");
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnWrongSinks)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_wrong_sinks);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("<sinks> contains invalid token: 'abcd'"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnWrongSinksTooFewValues)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_wrong_sinks_too_few_values);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Found line in <sinks> with 5 values; expected 6 to 6 values"));
}

TEST(CSumoNF2FFReaderTest, ReturnsErrorOnWrongSinksTooManyValues)
{
    const auto result = pre_c_sumo::CSumoNF2FFReader::fromString(invalid_xml_wrong_sinks_too_many_values);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Found line in <sinks> with 7 values; expected 6 to 6 values"));
}