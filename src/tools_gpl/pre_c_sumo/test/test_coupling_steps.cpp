#include <gtest/gtest.h>

#include "test_utilities.hpp"
#include "coupling_steps.hpp"

// This test verifies that the read_csumo_config_file function returns an error when given an invalid file path.
TEST(CsumoPreciceCouplingStepsTest, ReadCosumoConfigFile)
{
    auto result = pre_c_sumo::readCsumoSettingsFile("non_existent_file.xml");
    EXPECT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Cannot open file: ");
}

TEST(CsumoPreciceCouplingStepsTest, CreateDiffuserModelFromOneSourceAndOneSink)
{
    constexpr std::string_view xml = R"(<?xml version="1.0" encoding="utf-8"?>
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
            1010.000 300.000 5.000 123.456 0.000 0.000
        </sinks>
        <sources>
            1000.000 300.000 5.000 123.456 10.000 20.000
        </sources>
     </NFResult>
  </NF2FF>)";
    const auto diffuser = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_TRUE(diffuser.has_value());
    EXPECT_EQ(diffuser->sources().size(), 1);
    EXPECT_EQ(diffuser->sinks().size(), 1);

    const auto sources = pre_c_sumo::createDiffuserModel(diffuser.value());
    // Expect 1000 sources have been generated
    EXPECT_EQ(sources.size(), 1000);
    // Expect line to be in y-direction.
    EXPECT_DOUBLE_EQ(sources[0].x_coordinate, 1000.000);
    EXPECT_DOUBLE_EQ(sources[0].y_coordinate, 320.000);
    EXPECT_DOUBLE_EQ(sources[999].x_coordinate, 1000.000);
    EXPECT_DOUBLE_EQ(sources[999].y_coordinate, 280.000);
    EXPECT_DOUBLE_EQ(sources[0].weight, 0.001);
}

TEST(CsumoPreciceCouplingStepsTest, CreateDiffuserModelFromOneSourceAndTwoSinks)
{
    constexpr std::string_view xml = R"(<?xml version="1.0" encoding="utf-8"?>
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
            1010.000 300.000 5.000 123.456 0.000 0.000
            1000.000 350.000 5.000 123.456 0.000 0.000
        </sinks>
        <sources>
            1000.000 300.000 5.000 123.456 10.000 20.000
        </sources>
     </NFResult>
  </NF2FF>)";
    const auto diffuser = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_TRUE(diffuser.has_value());
    EXPECT_EQ(diffuser->sources().size(), 1);
    EXPECT_EQ(diffuser->sinks().size(), 2);

    const auto sources = pre_c_sumo::createDiffuserModel(diffuser.value());
    // Expect 1000 sources have been generated
    EXPECT_EQ(sources.size(), 1000);
    // Expect line to be in x-direction, ignoring the first sink
    EXPECT_DOUBLE_EQ(sources[0].x_coordinate, 1020.000);
    EXPECT_DOUBLE_EQ(sources[0].y_coordinate, 300.000);
    EXPECT_DOUBLE_EQ(sources[999].x_coordinate, 980.000);
    EXPECT_DOUBLE_EQ(sources[999].y_coordinate, 300.000);
    EXPECT_DOUBLE_EQ(sources[0].weight, 0.001);
}
