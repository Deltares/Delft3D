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

        for (const auto& [index, diffuser] : csumo_settings.diffusers() | std::views::enumerate)
        {
            const auto subgrid_model_nr = static_cast<int>(index + 1);
            // TODO: populate from received far-field data instead of placeholders
            auto make_point = [&constituents =
                                   diffuser.discharge.constituents](const parsing_utils::Point2D& position) {
                return FarFieldPoint2D{
                    .position = position,
                    .water_depth = 0.0, // TODO: obtain from far-field
                    .layers = {FarFieldLayer{.z_coordinate = 0.0,
                                             .x_velocity = 0.0,
                                             .y_velocity = 0.0,
                                             .density = 1000.0,
                                             .constituents = constituents}}, // TODO: obtain layered data from far-field
                };
            };

            const auto ambient_points =
                diffuser.ambient_positions | std::views::transform(make_point) | std::ranges::to<std::vector>();

            const auto ff2nf_filename = diffuser.ff2nf_dir / std::format("FF2NF__{}_SubMod{:03d}_{:.3f}.xml", run_id,
                                                                         subgrid_model_nr, current_time_seconds / 60.0);

            const auto nf2ff_wait_file = diffuser.nf2ff_file.value_or("");

            auto ff2nf_config = FF2NFConfig{
                .ff2nf_filename = ff2nf_filename.string(),
                .wait_for_file = nf2ff_wait_file,
                .ff_run_directory = diffuser.ff_run_dir.string(),
                .run_id = run_id,
                .unique_id = diffuser.id.value_or(""),
                .subgrid_model_nr = subgrid_model_nr,
                .current_time_seconds = current_time_seconds,
                .constituent_names = constituent_names,
                .diffuser = make_point(diffuser.position),
                .intake = diffuser.intake.has_value() ? std::optional{make_point(*diffuser.intake)} : std::nullopt,
                .ambient_points = ambient_points,
            };

            const auto result = FF2NFWriter(std::move(ff2nf_config)).toFile(ff2nf_filename);
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
