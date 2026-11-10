#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_TEST_COUPLING_TEST_DATA_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_TEST_COUPLING_TEST_DATA_HPP

#include <cstddef>
#include <limits>
#include <sstream>
#include <string>
#include <string_view>

namespace coupling_test_data
{
    constexpr std::string_view minimal_csumo_settings_xml = R"(<?xml version="1.0"?>
<COSUMO>
    <fileVersion>0.3</fileVersion>
    <settings>
        <general><ID>D1</ID><subGridModel/><farFieldModel/></general>
        <comm><FF2NFdir>a</FF2NFdir><FFrundir>b</FFrundir></comm>
        <data>
            <XYdiff>1.0 2.0</XYdiff>
            <discharge><M3s>5.0</M3s><constituentsOperator>excess</constituentsOperator><constituents>0.0</constituents></discharge>
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
            <discharge><M3s>5.0</M3s><constituentsOperator>excess</constituentsOperator><constituents>0.0</constituents></discharge>
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
    inline std::string build_nf2ff_xml(std::size_t sink_count, std::size_t source_count, bool include_qintake, double qintake,
                                     std::size_t intake_count = std::numeric_limits<std::size_t>::max(),
                                     double qsource = 10.0)
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
        xml << "    <Qsource>" << qsource << "</Qsource>\n";
        xml << R"(    <constituentsOperator>excess</constituentsOperator>
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
}

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_TEST_COUPLING_TEST_DATA_HPP
