#include "coupling_steps.hpp"

#include <precice/precice.hpp>
#include <print>
#include <string_view>
#include <vector>

#include "csumo_settings_reader.hpp"

namespace pre_c_sumo
{

    bool doTimeloop()
    {
        static int iteration = 0;
        return iteration++ < 2; // Run the loop 2 times for demonstration
    }

    std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError> readCsumoConfigFile(
        const std::string_view csumo_config_file_name)
    {
        std::println("Reading C-SUMO configuration file...");
        auto expectedCsumoSettings = pre_c_sumo::CSumoSettingsReader::fromFile(csumo_config_file_name);

        if (!expectedCsumoSettings.has_value())
        {
            std::println(stderr, "Error parsing C-SUMO configuration: {}", expectedCsumoSettings.error().message);
            return expectedCsumoSettings;
        }
        const auto csumo_settings = std::move(expectedCsumoSettings).value();
        std::println("Successfully parsed C-SUMO configuration file version: {}", csumo_settings.fileVersion());
        return expectedCsumoSettings.value();
    }

    void receiveFFData() { std::println("Receiving far-field data..."); }

    void writeFF2NFFiles(CSumoSettingsReader csumo_settings)
    {
        for (const auto& diffuser : csumo_settings.diffusers())
        {
            std::println("Write FF2NF file for diffuser with position: ({}, {})", diffuser.position.x,
                         diffuser.position.y);
            // Here you would add the actual logic to write the FF2NF files based on the diffuser settings
        }
    }

    void waitForNF2FFFiles(CSumoSettingsReader csumo_settings)
    {
        for (const auto& diffuser : csumo_settings.diffusers())
        {
            if (diffuser.nf2ff_file.has_value())
            {
                std::println("Waiting for NF2FF file: {}", diffuser.nf2ff_file.value());
                // Here you would add the actual logic to wait for the NF2FF files to be available
            }
        }
    }

    void readNF2FFFiles(CSumoSettingsReader csumo_settings)
    {
        for (const auto& diffuser : csumo_settings.diffusers())
        {
            if (diffuser.nf2ff_file.has_value())
            {
                std::println("Reading NF2FF file: {}", diffuser.nf2ff_file.value());
                // Here you would add the actual logic to read the NF2FF files and extract the necessary data
            }
        }
    }

    void convertNFToSourcesSinks(CSumoSettingsReader csumo_settings)
    {
        for (const auto& diffuser : csumo_settings.diffusers())
        {
            std::println("Converting NF data to sources/sinks for diffuser {} ...", diffuser.nf2ff_file.value());
            convertNFSinksToFF();
            convertNFIntakesToFF();
            convertNFSourcesToFF();
        }
    }

    void sendSourcesSinksToFF(CSumoSettingsReader csumo_settings)
    {
        std::println("Sending sources/sinks data to far-field...");
        (void)csumo_settings;
    }

    void convertNFSinksToFF() { std::println("Processing sinks..."); }

    void convertNFIntakesToFF() { std::println("Processing intakes..."); }

    void convertNFSourcesToFF()
    {
        if (isDiffuserModelled())
        {
            processSourceLocations();
        }
        else
        {
            createDiffuserModel();
        }
    }

    bool isDiffuserModelled()
    {
        // Placeholder logic to determine if the diffuser is modelled
        return true; // Assume it's modelled for demonstration
    }

    void processSourceLocations() { std::println("Processing source locations..."); }

    void createDiffuserModel() { std::println("Creating diffuser model..."); }

} // namespace pre_c_sumo
