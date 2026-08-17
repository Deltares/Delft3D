#include <gtest/gtest.h>

#include <array>
#include <limits>
#include <sstream>
#include <utility>

#include "test_utilities.hpp"
#include "coupling_steps.hpp"

namespace
{
    constexpr std::string_view minimal_csumo_settings_xml = R"(<?xml version="1.0"?>
<COSUMO>
    <fileVersion>0.3</fileVersion>
    <settings>
        <general><ID>D1</ID><subGridModel/><farFieldModel/></general>
        <comm><FF2NFdir>a</FF2NFdir><FFrundir>b</FFrundir></comm>
        <data>
            <XYdiff>1.0 2.0</XYdiff>
            <discharge><M3s>5.0</M3s><constituentsOperator>absolute</constituentsOperator><constituents>0.0</constituents></discharge>
            <D0>0.1</D0><H0>0.2</H0><Theta0>0.3</Theta0><Sigma0>0.4</Sigma0>
        </data>
    </settings>
</COSUMO>)";

    constexpr std::string_view minimal_csumo_settings_with_intake_xml = R"(<?xml version="1.0"?>
<COSUMO>
    <fileVersion>0.3</fileVersion>
    <settings>
        <general><ID>D1</ID><subGridModel/><farFieldModel/></general>
        <comm><FF2NFdir>a</FF2NFdir><FFrundir>b</FFrundir></comm>
        <data>
            <XYdiff>1.0 2.0</XYdiff>
            <XYintake>3.0 4.0</XYintake>
            <discharge><M3s>5.0</M3s><constituentsOperator>absolute</constituentsOperator><constituents>0.0</constituents></discharge>
            <D0>0.1</D0><H0>0.2</H0><Theta0>0.3</Theta0><Sigma0>0.4</Sigma0>
        </data>
    </settings>
</COSUMO>)";

    /**
     * @brief Builds a synthetic NF2FF XML payload for conversion tests.
     *
     * @param sink_count Number of sink records to emit in <sinks>.
     * @param source_count Number of source records to emit in <sources>.
     * @param include_qintake Whether to include the <Qintake> element.
     * @param qintake Intake discharge value written when @p include_qintake is true.
     * @param intake_count Number of intake entries to emit; defaults to derived behavior.
     * @return std::string XML document as a string.
     */
    std::string build_nf2ff_xml(std::size_t sink_count, std::size_t source_count, bool include_qintake, double qintake,
                                std::size_t intake_count = std::numeric_limits<std::size_t>::max())
    {
        const std::size_t effective_intake_count =
            intake_count == std::numeric_limits<std::size_t>::max() ? (include_qintake ? 1u : 0u) : intake_count;

        std::ostringstream xml;
        xml << R"(<?xml version="1.0" encoding="utf-8"?>
<NF2FF>
  <fileVersion>0.3</fileVersion>
  <discharge>
)";
        if (include_qintake)
        {
            xml << "    <Qintake>" << qintake << "</Qintake>\n";
        }
        xml << R"(    <Qsource>10.0</Qsource>
    <constituentsOperator>excess</constituentsOperator>
    <constituents>10.0 0.0</constituents>
  </discharge>
  <NFResult>
)";

        if (effective_intake_count > 0)
        {
            xml << R"(    <intakes>
)";
            for (std::size_t i = 0; i < effective_intake_count; ++i)
            {
                const double z = 0.5 + static_cast<double>(i);
                xml << "      1550.0 950.0 " << z << "\n";
            }
            xml << R"(    </intakes>
)";
        }

        xml << R"(    <sinks>
)";

        for (std::size_t i = 0; i < sink_count; ++i)
        {
            const double x = 250.0 + static_cast<double>(i) * 0.5;
            const double y = 350.087 - static_cast<double>(i) * 0.039;
            const double z = 9.7;
            const double entrainment = i == 0 ? 1.0 : (5.0 + static_cast<double>(i - 1) * 0.1);
            const double plume_height = 5.0;
            xml << "      " << x << " " << y << " " << z << " " << entrainment << " " << plume_height << " 0.000\n";
        }

        xml << R"(    </sinks>
    <sources>
)";

        for (std::size_t i = 0; i < source_count; ++i)
        {
            const double x = 1050.0 + static_cast<double>(i) * 0.5;
            xml << "      " << x << " 350.365 5.000 5.000 5.000 15.000\n";
        }

        xml << R"(    </sources>
  </NFResult>
</NF2FF>)";
        return xml.str();
    }

    /**
     * @brief Build synthetic NF2FF input and run the coupling conversion.
     *
     * Test helper for count/discharge scenarios driven by generated sink/source/intake
     * records while using the same production conversion function.
     *
     * @param sink_count Number of synthetic sink rows.
     * @param source_count Number of synthetic source rows.
     * @param include_qintake Whether to include Qintake in the generated XML.
     * @param qintake Qintake value used when include_qintake is true.
     * @param intake_count Number of synthetic intake rows.
     * @return Converted connected sink/source entries.
     */
    pre_c_sumo::ConnectedSinkSources convert_from_synthetic_case(
        std::size_t sink_count, std::size_t source_count, bool include_qintake, double qintake,
        std::size_t intake_count = std::numeric_limits<std::size_t>::max())
    {
        const auto settings = pre_c_sumo::CSumoSettingsReader::fromString(minimal_csumo_settings_with_intake_xml);
        EXPECT_TRUE(settings.has_value());

        const auto xml = build_nf2ff_xml(sink_count, source_count, include_qintake, qintake, intake_count);
        auto reader = pre_c_sumo::NF2FFReader::fromString(xml);
        EXPECT_TRUE(reader.has_value());

        std::vector<pre_c_sumo::NF2FFReader> nf2ff_readers;
        nf2ff_readers.emplace_back(std::move(*reader));
        return pre_c_sumo::convertNFtoConnectedSinkSources(*settings, nf2ff_readers);
    }

    /**
     * @brief Parse NF2FF XML and run the coupling conversion with minimal settings.
     *
     * Test helper that keeps per-test setup compact while exercising the same
     * production conversion path as file-based inputs.
     *
     * @param nf2ff_xml NF2FF XML payload to parse.
     * @return Converted connected sink/source entries.
     */
    pre_c_sumo::ConnectedSinkSources convert_from_nf2ff_xml(const std::string_view nf2ff_xml)
    {
        const auto settings = pre_c_sumo::CSumoSettingsReader::fromString(minimal_csumo_settings_with_intake_xml);
        EXPECT_TRUE(settings.has_value());

        auto reader = pre_c_sumo::NF2FFReader::fromString(nf2ff_xml);
        EXPECT_TRUE(reader.has_value());

        std::vector<pre_c_sumo::NF2FFReader> nf2ff_readers;
        nf2ff_readers.emplace_back(std::move(*reader));
        return pre_c_sumo::convertNFtoConnectedSinkSources(*settings, nf2ff_readers);
    }
} // namespace

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

TEST(CsumoPreciceCouplingStepsTest, ConvertNFToConnectedSinkSourcesUsesGeneratedDiffuserModel)
{
    constexpr std::string_view nf2ff_xml = R"(<?xml version="1.0" encoding="utf-8"?>
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

    const auto csumo_settings = pre_c_sumo::CSumoSettingsReader::fromString(minimal_csumo_settings_xml);
    ASSERT_TRUE(csumo_settings.has_value());

    auto diffuser = pre_c_sumo::NF2FFReader::fromString(nf2ff_xml);
    ASSERT_TRUE(diffuser.has_value());

    std::vector<pre_c_sumo::NF2FFReader> nf2ff_readers;
    nf2ff_readers.emplace_back(std::move(*diffuser));

    const auto connected_sources_sinks = pre_c_sumo::convertNFtoConnectedSinkSources(*csumo_settings, nf2ff_readers);

    EXPECT_EQ(connected_sources_sinks.get_number_of_entries(), 2000u);
}

TEST(CsumoPreciceCouplingStepsTest, ConvertNFToConnectedSinkSourcesUsesGeneratedDiffuserModelWithOneSink)
{
    constexpr std::string_view nf2ff_xml = R"(<?xml version="1.0" encoding="utf-8"?>
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
            1010.000 300.000 5.000 123.456 0.250 0.380
        </sinks>
        <sources>
            1000.000 300.000 5.000 123.456 10.000 20.000
        </sources>
     </NFResult>
  </NF2FF>)";

    const auto csumo_settings = pre_c_sumo::CSumoSettingsReader::fromString(minimal_csumo_settings_xml);
    ASSERT_TRUE(csumo_settings.has_value());

    auto diffuser = pre_c_sumo::NF2FFReader::fromString(nf2ff_xml);
    ASSERT_TRUE(diffuser.has_value());

    std::vector<pre_c_sumo::NF2FFReader> nf2ff_readers;
    nf2ff_readers.emplace_back(std::move(*diffuser));

    const auto connected_sources_sinks = pre_c_sumo::convertNFtoConnectedSinkSources(*csumo_settings, nf2ff_readers);

    EXPECT_EQ(connected_sources_sinks.get_number_of_entries(), 1000u);
}

TEST(CsumoPreciceCouplingStepsTest, SyntheticNF2FFCasesYieldExpectedConnectedCounts)
{
    struct SyntheticCase
    {
        std::string_view name;
        std::size_t sink_count;
        std::size_t source_count;
        bool include_qintake;
        double qintake;
        std::size_t intake_count;
        std::size_t expected_count;
    };

    constexpr std::array<SyntheticCase, 5> cases = {
        SyntheticCase{"i0si2so2", 2u, 2u, true, 10.0, 1u, 5u},
        SyntheticCase{"i0si2so1", 2u, 1u, false, 0.0, 0u, 2000u},
        SyntheticCase{"i1si2so2", 2u, 2u, true, 10.0, 1u, 5u},
        SyntheticCase{"i10si2so1", 2u, 1u, true, 10.0, 10u, 2010u},
        SyntheticCase{"i1si42so1", 42u, 1u, true, 10.0, 1u, 42001u},
    };

    for (const auto& scenario : cases)
    {
        const auto connected =
            convert_from_synthetic_case(scenario.sink_count, scenario.source_count, scenario.include_qintake,
                                        scenario.qintake, scenario.intake_count);
        EXPECT_EQ(connected.get_number_of_entries(), scenario.expected_count) << scenario.name;
    }
}

TEST(CsumoPreciceCouplingStepsTest, SyntheticCaseCopiedToAllTimestepsKeepsStableConnectedCount)
{
    struct SyntheticCase
    {
        std::string_view name;
        std::size_t sink_count;
        std::size_t source_count;
        bool include_qintake;
        double qintake;
        std::size_t intake_count;
    };

    constexpr std::array<SyntheticCase, 5> cases = {
        SyntheticCase{"i0si2so2", 2u, 2u, true, 10.0, 1u},   SyntheticCase{"i0si2so1", 2u, 1u, false, 0.0, 0u},
        SyntheticCase{"i1si2so2", 2u, 2u, true, 10.0, 1u},   SyntheticCase{"i10si2so1", 2u, 1u, true, 10.0, 10u},
        SyntheticCase{"i1si42so1", 42u, 1u, true, 10.0, 1u},
    };

    for (const auto& scenario : cases)
    {
        const auto initial_connected =
            convert_from_synthetic_case(scenario.sink_count, scenario.source_count, scenario.include_qintake,
                                        scenario.qintake, scenario.intake_count);
        const auto step_connected =
            convert_from_synthetic_case(scenario.sink_count, scenario.source_count, scenario.include_qintake,
                                        scenario.qintake, scenario.intake_count);
        EXPECT_EQ(step_connected.get_number_of_entries(), initial_connected.get_number_of_entries()) << scenario.name;
        EXPECT_GT(step_connected.get_number_of_entries(), 0u) << scenario.name;
    }
}

TEST(CsumoPreciceCouplingStepsTest, SyntheticI0Si2So2YieldsExpectedDischarges)
{
    const auto connected = convert_from_synthetic_case(2u, 2u, true, 10.0);
    ASSERT_EQ(connected.get_number_of_entries(), 5u);

    const auto& discharges = connected.get_discharge_value();
    ASSERT_EQ(discharges.size(), 5u);
    EXPECT_DOUBLE_EQ(discharges[0], 20.0);
    EXPECT_DOUBLE_EQ(discharges[1], 20.0);
    EXPECT_DOUBLE_EQ(discharges[2], 5.0);
    EXPECT_DOUBLE_EQ(discharges[3], 5.0);
    EXPECT_DOUBLE_EQ(discharges[4], 10.0);
}

TEST(CsumoPreciceCouplingStepsTest, SyntheticI0Si2So1UsesDESAAndZeroIntakeDischarge)
{
    const auto xml = build_nf2ff_xml(2u, 1u, false, 0.0, 0u);
    const auto settings = pre_c_sumo::CSumoSettingsReader::fromString(minimal_csumo_settings_with_intake_xml);
    ASSERT_TRUE(settings.has_value());

    auto reader = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_TRUE(reader.has_value());
    std::vector<pre_c_sumo::NF2FFReader> nf2ff_readers;
    nf2ff_readers.emplace_back(std::move(*reader));

    EXPECT_NEAR(nf2ff_readers.front().intakeFlowRate(), 0.0, 1e-12);
    EXPECT_TRUE(nf2ff_readers.front().intakes().empty());

    const auto connected = pre_c_sumo::convertNFtoConnectedSinkSources(*settings, nf2ff_readers);
    ASSERT_EQ(connected.get_number_of_entries(), 2000u);

    const auto& discharges = connected.get_discharge_value();
    ASSERT_EQ(discharges.size(), 2000u);
    EXPECT_NEAR(discharges[0], 0.04, 1e-12);
    EXPECT_NEAR(discharges[999], 0.04, 1e-12);
    EXPECT_NEAR(discharges[1000], 0.01, 1e-12);
    EXPECT_NEAR(discharges[1999], 0.01, 1e-12);
}
TEST(CsumoPreciceCouplingStepsTest, SyntheticI0Si2So1UsesSettingsIntakeFallbackWithNonZeroQintake)
{
    const auto connected = convert_from_synthetic_case(2u, 1u, true, 10.0, 0u);
    ASSERT_EQ(connected.get_number_of_entries(), 2001u);

    const auto& discharges = connected.get_discharge_value();
    ASSERT_EQ(discharges.size(), 2001u);
    EXPECT_NEAR(discharges[0], 0.04, 1e-12);
    EXPECT_NEAR(discharges[999], 0.04, 1e-12);
    EXPECT_NEAR(discharges[1000], 0.01, 1e-12);
    EXPECT_NEAR(discharges[1999], 0.01, 1e-12);
    EXPECT_NEAR(discharges[2000], 10.0, 1e-12);
}

TEST(CsumoPreciceCouplingStepsTest, SyntheticI10Si2So1WithTenIntakesYieldsExpectedCount)
{
    const auto connected = convert_from_synthetic_case(2u, 1u, true, 10.0, 10u);
    ASSERT_EQ(connected.get_number_of_entries(), 2010u);

    const auto& discharges = connected.get_discharge_value();
    ASSERT_EQ(discharges.size(), 2010u);
    EXPECT_NEAR(discharges[0], 0.04, 1e-12);
    EXPECT_NEAR(discharges[999], 0.04, 1e-12);
    EXPECT_NEAR(discharges[1000], 0.01, 1e-12);
    EXPECT_NEAR(discharges[1999], 0.01, 1e-12);
    EXPECT_NEAR(discharges[2000], 1.0, 1e-12);
    EXPECT_NEAR(discharges[2009], 1.0, 1e-12);
}

TEST(CsumoPreciceCouplingStepsTest, SourceWeightSumBelowOneUsesClampAtOne)
{
    constexpr std::string_view nf2ff_xml = R"(<?xml version="1.0" encoding="utf-8"?>
<NF2FF>
    <fileVersion>0.3</fileVersion>
    <discharge>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
    </discharge>
    <NFResult>
        <sinks>
            1010.0 300.0 5.0 1.0 5.0 0.0
        </sinks>
        <sources>
            1000.0 300.0 5.0 1.0 5.0 15.0 0.2
            1001.0 300.0 5.0 1.0 5.0 15.0 0.3
        </sources>
    </NFResult>
</NF2FF>)";

    const auto connected = convert_from_nf2ff_xml(nf2ff_xml);
    ASSERT_EQ(connected.get_number_of_entries(), 2u);

    const auto& discharges = connected.get_discharge_value();
    ASSERT_EQ(discharges.size(), 2u);
    // Intended behavior: denominator is clamped to 1.0 when sum(weights) < 1.
    EXPECT_NEAR(discharges[0], 2.0, 1e-12);
    EXPECT_NEAR(discharges[1], 3.0, 1e-12);
}

TEST(CsumoPreciceCouplingStepsTest, IntakeWeightSumBelowOneUsesClampAtOne)
{
    constexpr std::string_view nf2ff_xml = R"(<?xml version="1.0" encoding="utf-8"?>
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
            1550.0 950.0 0.5 0.2
            1551.0 950.0 0.5 0.3
        </intakes>
        <sinks>
            1010.0 300.0 5.0 1.0 5.0 0.0
        </sinks>
        <sources>
            1000.0 300.0 5.0 1.0 5.0 15.0
            1001.0 300.0 5.0 1.0 5.0 15.0
        </sources>
    </NFResult>
</NF2FF>)";

    const auto connected = convert_from_nf2ff_xml(nf2ff_xml);
    ASSERT_EQ(connected.get_number_of_entries(), 4u);

    const auto& discharges = connected.get_discharge_value();
    ASSERT_EQ(discharges.size(), 4u);
    // Intended behavior: denominator is clamped to 1.0 when sum(weights) < 1.
    EXPECT_NEAR(discharges[2], 2.0, 1e-12);
    EXPECT_NEAR(discharges[3], 3.0, 1e-12);
}

TEST(CsumoPreciceCouplingStepsTest, SourceWeightSumBelowOneProducesLowerDischargeBecauseOfClamp)
{
    constexpr std::string_view nf2ff_xml_below_one = R"(<?xml version="1.0" encoding="utf-8"?>
<NF2FF>
    <fileVersion>0.3</fileVersion>
    <discharge>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
    </discharge>
    <NFResult>
        <sinks>
            1010.0 300.0 5.0 1.0 5.0 0.0
        </sinks>
        <sources>
            1000.0 300.0 5.0 1.0 5.0 15.0 0.2
            1001.0 300.0 5.0 1.0 5.0 15.0 0.3
        </sources>
    </NFResult>
</NF2FF>)";

    constexpr std::string_view nf2ff_xml_equals_one = R"(<?xml version="1.0" encoding="utf-8"?>
<NF2FF>
    <fileVersion>0.3</fileVersion>
    <discharge>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
    </discharge>
    <NFResult>
        <sinks>
            1010.0 300.0 5.0 1.0 5.0 0.0
        </sinks>
        <sources>
            1000.0 300.0 5.0 1.0 5.0 15.0 0.4
            1001.0 300.0 5.0 1.0 5.0 15.0 0.6
        </sources>
    </NFResult>
</NF2FF>)";

    const auto connected_below_one = convert_from_nf2ff_xml(nf2ff_xml_below_one);
    const auto connected_equals_one = convert_from_nf2ff_xml(nf2ff_xml_equals_one);

    const auto& discharges_below_one = connected_below_one.get_discharge_value();
    const auto& discharges_equals_one = connected_equals_one.get_discharge_value();

    ASSERT_EQ(discharges_below_one.size(), 2u);
    ASSERT_EQ(discharges_equals_one.size(), 2u);

    const double total_below_one = discharges_below_one[0] + discharges_below_one[1];
    const double total_equals_one = discharges_equals_one[0] + discharges_equals_one[1];

    EXPECT_NEAR(total_below_one, 5.0, 1e-12);
    EXPECT_NEAR(total_equals_one, 10.0, 1e-12);
    EXPECT_LT(total_below_one, total_equals_one);
}

TEST(CsumoPreciceCouplingStepsTest, NegativeEntrainmentFactor)
{
    // Create NF2FF XML with decreasing entrainment factors
    constexpr std::string_view nf2ff_xml = R"(<?xml version="1.0" encoding="utf-8"?>
<NF2FF>
    <fileVersion>0.3</fileVersion>
    <discharge>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
    </discharge>
    <NFResult>
        <sinks>
            1010.0 300.0 5.0 5.0 5.0 0.0
            1000.0 350.0 5.0 2.5 5.0 0.0
            990.0 400.0 5.0 1.0 5.0 0.0
        </sinks>
        <sources>
            1000.0 300.0 5.0 1.0 5.0 15.0
        </sources>
    </NFResult>
</NF2FF>)";

    const auto settings = pre_c_sumo::CSumoSettingsReader::fromString(minimal_csumo_settings_xml);
    ASSERT_TRUE(settings.has_value());

    auto reader = pre_c_sumo::NF2FFReader::fromString(nf2ff_xml);
    ASSERT_TRUE(reader.has_value());

    std::vector<pre_c_sumo::NF2FFReader> nf2ff_readers;
    nf2ff_readers.emplace_back(std::move(*reader));

    // Verify that the entrainment values are indeed decreasing
    ASSERT_GT(nf2ff_readers[0].sinks()[0].entrainment, nf2ff_readers[0].sinks()[1].entrainment);

    // Verify that the error message contains the sink index and the negative delta_s value
    try
    {
        pre_c_sumo::convertNFtoConnectedSinkSources(*settings, nf2ff_readers);
        FAIL() << "Expected std::runtime_error to be thrown";
    }
    catch (const std::runtime_error& e)
    {
        const std::string error_message = e.what();
        // Verify that the error message contains "Negative entrainment factor"
        EXPECT_PRED2(test_utilities::contains, error_message, "Negative entrainment factor");
        // Verify that the error message contains the sink index (should be 1, since delta from sink 0 to 1)
        EXPECT_PRED2(test_utilities::contains, error_message, "1");
        // Verify that the error message contains the negative delta_s value
        EXPECT_PRED2(test_utilities::contains, error_message, "-");
    }

    // Verify that the exception is thrown
    EXPECT_THROW(pre_c_sumo::convertNFtoConnectedSinkSources(*settings, nf2ff_readers), std::runtime_error);
}
