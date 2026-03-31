#include "csumo_precice_lib.hpp"

#include <precice/precice.hpp>
#include <print>
#include <string_view>
#include <vector>

#include "csumo_settings_reader.hpp"

namespace csumo_precice
{
    /**
     * @details Entry point into the C-SUMO preCICE library.
     */
    int run(const std::string_view csumoConfigFileName, const std::string_view adapterConfigFileName)
    {
        auto expectedCsumoSettings = CSumoSettingsReader::fromFile(csumoConfigFileName);

        if (!expectedCsumoSettings.has_value())
        {
            std::println(stderr, "Error parsing C-SUMO configuration: {}", expectedCsumoSettings.error().message);
            return 1;
        }
        const auto csumoSettings = std::move(expectedCsumoSettings).value();

        std::println("Successfully parsed C-SUMO configuration file version: {}", csumoSettings.fileVersion());

        (void)adapterConfigFileName; // Unused parameter, avoid compiler warning
        constexpr std::string_view csumo_config_file = "csumo_config.xml";
        constexpr int mpiRank = 0;
        constexpr int mpiSize = 1;
        precice::Participant participant{"C-SUMO", csumo_config_file, mpiRank, mpiSize};
        return 0;
    }

    /**
     * @details This function prints a greeting message to the console using C++23's std::println.
     */
    int run()
    {
        std::println("Hello, world from C-SUMO PreCICE library!");
        return 0;
    }
} // namespace csumo_precice
