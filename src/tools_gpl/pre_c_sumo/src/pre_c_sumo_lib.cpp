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
            write_ff2nf_files(csumoSettings);
            wait_for_nf2ff_files(csumoSettings);
            read_nf2ff_files(csumoSettings);
            convert_nf_to_sources_sinks(csumoSettings);
            send_sources_sinks_to_ff(csumoSettings);
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

    void write_ff2nf_files(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError> csumoSettings)
    {
        if (!csumoSettings.has_value())
        {
            std::println(stderr, "Cannot write FF2NF files: invalid C-SUMO settings.");
            return;
        }
        for (const auto& diffuser : csumoSettings.value().diffusers())
        {
            std::println("Write FF2NF file for diffuser with position: ({}, {})", diffuser.position.x,
                         diffuser.position.y);
            // Here you would add the actual logic to write the FF2NF files based on the diffuser settings
        }
    }

    void wait_for_nf2ff_files(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError> csumoSettings)
    {
        for (const auto& diffuser : csumoSettings.value().diffusers())
        {
            if (diffuser.nf2ff_file.has_value())
            {
                std::println("Waiting for NF2FF file: {}", diffuser.nf2ff_file.value());
                // Here you would add the actual logic to wait for the NF2FF files to be available
            }
        }
    }

    void read_nf2ff_files(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError> csumoSettings)
    {
        for (const auto& diffuser : csumoSettings.value().diffusers())
        {
            if (diffuser.nf2ff_file.has_value())
            {
                std::println("Reading NF2FF file: {}", diffuser.nf2ff_file.value());
                // Here you would add the actual logic to read the NF2FF files and extract the necessary data
            }
        }
    }

    void convert_nf_to_sources_sinks(
        std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError> csumoSettings)
    {
        for (const auto& diffuser : csumoSettings.value().diffusers())
        {
            std::println("Converting NF data to sources/sinks for diffuser {} ...", diffuser.nf2ff_file.value());
            process_sinks();
            process_intakes();
            process_sources();
        }
    }

    void send_sources_sinks_to_ff(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError> csumoSettings)
    {
        std::println("Sending sources/sinks data to far-field...");
        (void)csumoSettings;
    }

    void process_sinks() { std::println("Processing sinks..."); }
    void process_intakes() { std::println("Processing intakes..."); }
    void process_sources()
    {
        if (is_diffuser_modelled())
        {
            process_source_locations();
        }
        else
        {
            create_diffuser_model();
        }
    }
    bool is_diffuser_modelled()
    {
        // Placeholder logic to determine if the diffuser is modelled
        return true; // Assume it's modelled for demonstration
    }
    void process_source_locations() { std::println("Processing source locations..."); }
    void create_diffuser_model() { std::println("Creating diffuser model..."); }
} // namespace pre_c_sumo
