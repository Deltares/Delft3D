#include <gtest/gtest.h>

#include "test_utilities.hpp"
#include "coupling_steps.hpp"
#include "NF2FF_reader.hpp"
#include "pre_c_sumo_lib.hpp"

// This test verifies that the read_csumo_config_file function returns an error when given an invalid file path.
TEST(CsumoPreciceCouplingStepsTest, ReadCosumoConfigFile)
{
    auto result = pre_c_sumo::readCsumoSettingsFile("non_existent_file.xml");
    EXPECT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Cannot open file: ");
}

TEST(CsumoPreciceCouplingStepsTest, ConvertNFSinksToFFBuildsExpectedRecords)
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
            250.000 350.087 9.700 1.000 0.000 0.000
            252.500 350.048 9.700 5.000 0.250 0.380
        </sinks>
        <sources>
            1050.000 350.365 5.000 5.000 5.000 15.000
            1050.500 350.365 5.000 5.000 5.000 15.000
        </sources>
     </NFResult>
  </NF2FF>)";

    const auto reader_result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_TRUE(reader_result.has_value());

    pre_c_sumo::SourcesSinks output;
    const auto next_id = pre_c_sumo::convertNFSinksToFF(reader_result.value(), output, 1.0,
                                                        parsing_utils::Point2D{1500.6, 1000.6});

    EXPECT_DOUBLE_EQ(next_id, 8.0);
    EXPECT_EQ(output.ids.size(), 7U);
    EXPECT_EQ(output.coordinates.size(), 14U);

    EXPECT_DOUBLE_EQ(output.ids[0], 1.0);
    EXPECT_DOUBLE_EQ(output.connected_ids[0], 2.0);
    EXPECT_DOUBLE_EQ(output.z_mins[0], -9.95);
    EXPECT_DOUBLE_EQ(output.z_maxs[0], -9.45);
    EXPECT_DOUBLE_EQ(output.discharges[0], -20.0);

    EXPECT_DOUBLE_EQ(output.ids[1], 2.0);
    EXPECT_DOUBLE_EQ(output.connected_ids[1], 1.0);
    EXPECT_DOUBLE_EQ(output.discharges[1], 20.0);

    EXPECT_DOUBLE_EQ(output.ids[4], 5.0);
    EXPECT_DOUBLE_EQ(output.connected_ids[4], 0.0);
    EXPECT_DOUBLE_EQ(output.discharges[4], 5.0);

    EXPECT_DOUBLE_EQ(output.ids[6], 7.0);
    EXPECT_DOUBLE_EQ(output.connected_ids[6], 0.0);
    EXPECT_DOUBLE_EQ(output.discharges[6], 10.0);
}

TEST(CsumoPreciceCouplingStepsTest, ConvertNFSinksToFFBuildsExpectedSourceSinkTopology)
{
    constexpr std::string_view xml = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>0.0</Qintake>
        <Qsource>8.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>0.0 0.0</constituents>
     </discharge>
     <NFResult>
        <sinks>
            250.0 350.0 9.7 1.0 0.25 0.38
            252.5 350.0 9.7 1.5 0.25 0.38
            255.0 350.0 9.7 2.0 0.25 0.38
        </sinks>
        <sources>
            1050.0 350.3 5.0 5.0 5.0 15.0 3.0
            1050.5 350.3 5.0 5.0 5.0 15.0 1.0
        </sources>
     </NFResult>
  </NF2FF>)";

    const auto reader_result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_TRUE(reader_result.has_value());

    pre_c_sumo::SourcesSinks output;
    const auto next_id = pre_c_sumo::convertNFSinksToFF(reader_result.value(), output, 100.0, std::nullopt);

    // Two sink deltas * two sources => 8 entrainment records, plus 2 source discharge records.
    EXPECT_DOUBLE_EQ(next_id, 110.0);
    EXPECT_EQ(output.ids.size(), 10U);
    EXPECT_EQ(output.connected_ids.size(), 10U);
    EXPECT_EQ(output.discharges.size(), 10U);

    // First entrainment pair (deltaS = 0.5, normalized weight = 0.75 => Qe = 3.0).
    EXPECT_DOUBLE_EQ(output.ids[0], 100.0);
    EXPECT_DOUBLE_EQ(output.connected_ids[0], 101.0);
    EXPECT_DOUBLE_EQ(output.discharges[0], -3.0);
    EXPECT_DOUBLE_EQ(output.ids[1], 101.0);
    EXPECT_DOUBLE_EQ(output.connected_ids[1], 100.0);
    EXPECT_DOUBLE_EQ(output.discharges[1], 3.0);

    // Second source for same sink delta (normalized weight = 0.25 => Qe = 1.0).
    EXPECT_DOUBLE_EQ(output.ids[2], 102.0);
    EXPECT_DOUBLE_EQ(output.connected_ids[2], 103.0);
    EXPECT_DOUBLE_EQ(output.discharges[2], -1.0);
    EXPECT_DOUBLE_EQ(output.ids[3], 103.0);
    EXPECT_DOUBLE_EQ(output.connected_ids[3], 102.0);
    EXPECT_DOUBLE_EQ(output.discharges[3], 1.0);

    // Final discharge-only source records with connected_id = 0.
    EXPECT_DOUBLE_EQ(output.ids[8], 108.0);
    EXPECT_DOUBLE_EQ(output.connected_ids[8], 0.0);
    EXPECT_DOUBLE_EQ(output.discharges[8], 6.0);
    EXPECT_DOUBLE_EQ(output.ids[9], 109.0);
    EXPECT_DOUBLE_EQ(output.connected_ids[9], 0.0);
    EXPECT_DOUBLE_EQ(output.discharges[9], 2.0);
}
