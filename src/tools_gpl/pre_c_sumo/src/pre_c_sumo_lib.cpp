#include "pre_c_sumo_lib.hpp"

#include <precice/precice.hpp>
#include <print>
#include <string_view>
#include <vector>

#include "csumo_settings_reader.hpp"

namespace pre_c_sumo
{
    /**
     * @details Entry point into the C-SUMO preCICE library.
     */
    int run(const std::string_view csumoConfigFileName, const std::string_view adapterConfigFileName)
    {
        constexpr int mpiRank = 0;
        constexpr int mpiSize = 1;
        precice::Participant participant{"C-SUMO", adapterConfigFileName, mpiRank, mpiSize};

        while (do_timeloop())
        {
            const auto csumoSettings = read_csumo_config_file(csumoConfigFileName);
            receive_ff_data();
            write_ff2nf_files();
            wait_for_nf2ff_files();
            read_nf2ff_files();
            convert_nf_to_sources_sinks();
            send_sources_sinks_to_ff();
        }
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
    bool do_timeloop()
    {
        static int iteration = 0;
        return iteration++ < 2; // Run the loop 2 times for demonstration
    }

    std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError> read_csumo_config_file(
        const std::string_view csumoConfigFileName)
    {
        std::println("Reading C-SUMO configuration file...");
        auto expectedCsumoSettings = CSumoSettingsReader::fromFile(csumoConfigFileName);

        if (!expectedCsumoSettings.has_value())
        {
            std::println(stderr, "Error parsing C-SUMO configuration: {}", expectedCsumoSettings.error().message);
            return expectedCsumoSettings;
        }
        const auto csumoSettings = std::move(expectedCsumoSettings).value();
        std::println("Successfully parsed C-SUMO configuration file version: {}", csumoSettings.fileVersion());
        return expectedCsumoSettings.value();
    }

    void receive_ff_data() { std::println("Receiving far-field data..."); }

    void write_ff2nf_files() { std::println("Writing FF2NF files..."); }

    void wait_for_nf2ff_files() { std::println("Waiting for NF2FF files..."); }

    void read_nf2ff_files() { std::println("Reading NF2FF files..."); }

    void convert_nf_to_sources_sinks() { std::println("Converting NF data to sources/sinks..."); }

    void send_sources_sinks_to_ff() { std::println("Sending sources/sinks data to far-field..."); }

} // namespace pre_c_sumo
