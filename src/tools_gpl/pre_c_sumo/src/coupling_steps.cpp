#include "coupling_steps.hpp"

#include <precice/precice.hpp>
#include <format>
#include <print>
#include <ranges>
#include <string_view>
#include <vector>

#include "csumo_settings_reader.hpp"
#include "FF2NF_writer.hpp"
#include "parsing_types.hpp"

namespace pre_c_sumo
{

    bool doTimeloop()
    {
        static int iteration = 0;
        return iteration++ < 2; // Run the loop 2 times for demonstration
    }

    std::expected<pre_c_sumo::CSumoSettingsReader, parsing_utils::ParseError> readCsumoSettingsFile(
        const std::string_view csumo_settings_file_name)
    {
        std::println("Reading C-SUMO configuration file...");
        auto expectedCsumoSettings = pre_c_sumo::CSumoSettingsReader::fromFile(csumo_settings_file_name);

        if (!expectedCsumoSettings.has_value())
        {
            std::println(stderr, "Error parsing C-SUMO configuration: {}", expectedCsumoSettings.error().message);
            return expectedCsumoSettings;
        }
        const auto csumo_settings = std::move(expectedCsumoSettings).value();
        std::println("Successfully parsed C-SUMO configuration file version: {}", csumo_settings.fileVersion());
        return csumo_settings;
    }

    void receiveFFData() { std::println("Receiving far-field data..."); }

    void writeFF2NFFiles(const CSumoSettingsReader& csumo_settings)
    {
        // TODO: obtain these from the far-field model / coupling state
        const double current_time_seconds = 0.0;
        const std::string run_id = "FlowFM";
        const std::vector<std::string> constituent_names = {"temperature"}; // TODO: derive from settings
        const FarFieldLayer default_layer{.depth_from_surface = 0.0, .x_velocity = 0.0, .y_velocity = 0.0};

        for (const auto& [index, diffuser] : csumo_settings.diffusers() | std::views::enumerate)
        {
            const auto subgrid_model_nr = static_cast<int>(index + 1);
            // TODO: populate from received far-field data instead of placeholders
            auto make_point = [&constituents = diffuser.discharge.constituents,
                               default_layer](const parsing_utils::Point2D& pos) {
                return FarFieldPoint2D{
                    .x = pos.x,
                    .y = pos.y,
                    .water_depth = 0.0, // TODO: obtain from far-field
                    .density = 1000.0,  // TODO: obtain from far-field
                    .constituents = constituents,
                    .layers = {default_layer}, // TODO: obtain layered data from far-field
                };
            };

            const auto ambient_points =
                diffuser.ambient_positions | std::views::transform(make_point) | std::ranges::to<std::vector>();

            const auto ff2nf_filename = diffuser.ff2nf_dir / std::format("FF2NF__{}_SubMod{:03d}_{:.3f}.xml", run_id,
                                                                         subgrid_model_nr, current_time_seconds / 60.0);

            const auto nf2ff_wait_file = diffuser.nf2ff_file.value_or("");

            auto writer = FF2NFWriter()
                              .setFF2NFFilename(ff2nf_filename.string())
                              .setWaitForFile(nf2ff_wait_file)
                              .setFFRunDirectory(diffuser.ff_run_dir.string())
                              .setRunId(run_id)
                              .setUniqueId(diffuser.id.value_or(""))
                              .setSubgridModelNumber(subgrid_model_nr)
                              .setCurrentTimeSeconds(current_time_seconds)
                              .setConstituentNames(constituent_names)
                              .setDiffuser(make_point(diffuser.position))
                              .setAmbientPoints(ambient_points);

            if (diffuser.intake.has_value())
            {
                writer.setIntake(make_point(*diffuser.intake));
            }

            const auto result = writer.toFile(ff2nf_filename);
            if (!result.has_value())
            {
                std::println(stderr, "Error writing FF2NF file: {}", result.error().message);
                continue;
            }
            std::println("Wrote FF2NF file: {}", ff2nf_filename.string());
        }
    }

    void waitForNF2FFFiles(const CSumoSettingsReader& csumo_settings)
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

    void readNF2FFFiles(const CSumoSettingsReader& csumo_settings)
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

    void convertNFToSourcesSinks(const CSumoSettingsReader& csumo_settings)
    {
        for (const auto& diffuser : csumo_settings.diffusers())
        {
            std::println("Converting NF data to sources/sinks for diffuser {} ...", diffuser.nf2ff_file.value());
            convertNFSinksToFF();
            convertNFIntakesToFF();
            convertNFSourcesToFF();
        }
    }

    void sendSourcesSinksToFF(const CSumoSettingsReader& csumo_settings)
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
