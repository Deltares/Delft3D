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
    std::string build_nf2ff_xml(std::size_t sink_count, std::size_t source_count, bool include_qintake,
                                double qintake,
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
            xml << "      " << x << " " << y << " " << z << " " << entrainment << " " << plume_height
                << " 0.000\n";
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

    pre_c_sumo::ConnectedSinkSources convert_from_synthetic_case(std::size_t sink_count, std::size_t source_count,
                                                                  bool include_qintake, double qintake,
                                                                  std::size_t intake_count =
                                                                      std::numeric_limits<std::size_t>::max())
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
}

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

    const auto connected_sources_sinks =
        pre_c_sumo::convertNFtoConnectedSinkSources(*csumo_settings, nf2ff_readers);

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

    const auto connected_sources_sinks =
        pre_c_sumo::convertNFtoConnectedSinkSources(*csumo_settings, nf2ff_readers);

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
        const auto connected = convert_from_synthetic_case(scenario.sink_count, scenario.source_count,
                                                           scenario.include_qintake, scenario.qintake,
                                                           scenario.intake_count);
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
        SyntheticCase{"i0si2so2", 2u, 2u, true, 10.0, 1u},
        SyntheticCase{"i0si2so1", 2u, 1u, false, 0.0, 0u},
        SyntheticCase{"i1si2so2", 2u, 2u, true, 10.0, 1u},
        SyntheticCase{"i10si2so1", 2u, 1u, true, 10.0, 10u},
        SyntheticCase{"i1si42so1", 42u, 1u, true, 10.0, 1u},
    };

    for (const auto& scenario : cases)
    {
        const auto initial_connected = convert_from_synthetic_case(scenario.sink_count, scenario.source_count,
                                                                   scenario.include_qintake, scenario.qintake,
                                                                   scenario.intake_count);
        const auto step_connected = convert_from_synthetic_case(scenario.sink_count, scenario.source_count,
                                                                scenario.include_qintake, scenario.qintake,
                                                                scenario.intake_count);
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
    const auto connected = convert_from_synthetic_case(2u, 1u, false, 0.0);
    ASSERT_EQ(connected.get_number_of_entries(), 2000u);

    const auto& discharges = connected.get_discharge_value();
    ASSERT_EQ(discharges.size(), 2000u);
    EXPECT_NEAR(discharges[0], 0.04, 1e-12);
    EXPECT_NEAR(discharges[999], 0.04, 1e-12);
    EXPECT_NEAR(discharges[1000], 0.01, 1e-12);
    EXPECT_NEAR(discharges[1999], 0.01, 1e-12);
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
