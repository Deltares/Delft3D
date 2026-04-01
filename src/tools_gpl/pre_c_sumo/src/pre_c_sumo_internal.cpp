#include "pre_c_sumo_internal.hpp"

#include <precice/precice.hpp>
#include <print>
#include <string_view>
#include <vector>

#include "csumo_settings_reader.hpp"

namespace pre_c_sumo
{
    /**
     * @brief Drive the main timeloop of the preC-SUMO demonstration.
     *
     * This demo implementation runs two iterations and then stops.
     *
     * @return true if another iteration should run, false to stop.
     */
    bool do_timeloop()
    {
        static int iteration = 0;
        return iteration++ < 2; // Run the loop 2 times for demonstration
    }

    /**
     * @brief Read and parse the C-SUMO configuration file.
     *
     * Logs progress and returns a `CSumoSettingsReader` on success or
     * a `ParseError` on failure.
     *
     * @param csumoConfigFileName Path to the configuration file.
     * @return expected containing `CSumoSettingsReader` or `ParseError`.
     */
    std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError> read_csumo_config_file(
        const std::string_view csumoConfigFileName)
    {
        std::println("Reading C-SUMO configuration file...");
        auto expectedCsumoSettings = pre_c_sumo::CSumoSettingsReader::fromFile(csumoConfigFileName);

        if (!expectedCsumoSettings.has_value())
        {
            std::println(stderr, "Error parsing C-SUMO configuration: {}", expectedCsumoSettings.error().message);
            return expectedCsumoSettings;
        }
        const auto csumoSettings = std::move(expectedCsumoSettings).value();
        std::println("Successfully parsed C-SUMO configuration file version: {}", csumoSettings.fileVersion());
        return expectedCsumoSettings.value();
    }

    /**
     * @brief Receive farfield (FF) data from external sources via preCICE.
     *
     * Blocking receive of farfield data via preCICE.
     * The demo implementation only logs a message.
     */
    void receive_ff_data() { std::println("Receiving far-field data..."); }

    /**
     * @brief Write FF2NF files based on parsed C-SUMO settings and received farfield data.
     *
     * Writes a FF2NF file for each configured diffuser.
     * If `csumoSettings` holds an error, no files are written.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
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

    /**
     * @brief Wait until NF2FF files become available.
     *
     * For each diffuser configured in `csumoSettings` this will wait for
     * the corresponding NF2FF file to appear. If `csumoSettings` contains
     * a parse error, the behaviour is undefined in the demo implementation.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
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

    /**
     * @brief Read NF2FF files and extract the required data.
     *
     * Reads NF2FF files referenced in `csumoSettings` and extracts the
     * data that will be converted to sources/sinks.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
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

    /**
     * @brief Convert NF data to sources and sinks to be communicated via preCICE.
     *
     * Uses the data referenced in `csumoSettings` to perform the conversion.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
    void convert_nf_to_sources_sinks(
        std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError> csumoSettings)
    {
        for (const auto& diffuser : csumoSettings.value().diffusers())
        {
            std::println("Converting NF data to sources/sinks for diffuser {} ...", diffuser.nf2ff_file.value());
            convert_nf_sinks_to_ff();
            convert_nf_intakes_to_ff();
            convert_nf_sources_to_ff();
        }
    }

    /**
     * @brief Send computed sources/sinks to the farfield model.
     *
     * Sends the converted sources and sinks to the farfield component.
     * The demo implementation logs an informational message.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
    void send_sources_sinks_to_ff(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError> csumoSettings)
    {
        std::println("Sending sources/sinks data to far-field...");
        (void)csumoSettings;
    }

    /**
     * @brief Convert NF sinks to farfield sinks.
     *
     * Converts NF sink information into the format required by the
     * farfield component.
     */
    void convert_nf_sinks_to_ff() { std::println("Processing sinks..."); }

    /**
     * @brief Convert NF intakes to farfield sinks.
     *
     * Converts NF intake information into the format required by the
     * farfield component.
     */
    void convert_nf_intakes_to_ff() { std::println("Processing intakes..."); }

    /**
     * @brief Convert NF source definitions to farfield sources.
     *
     * Depending on whether a diffuser is modelled this will either
     * process explicit source locations or build a diffuser model.
     */
    void convert_nf_sources_to_ff()
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

    /**
     * @brief Query whether the diffuser is modelled explicitly.
     *
     * @return true if the diffuser is modelled, false otherwise.
     */
    bool is_diffuser_modelled()
    {
        // Placeholder logic to determine if the diffuser is modelled
        return true; // Assume it's modelled for demonstration
    }

    /**
     * @brief Process explicit source locations from NF data.
     *
     * Converts NF source information into the format required by the farfield component.
     */
    void process_source_locations() { std::println("Processing source locations..."); }

    /**
     * @brief Create an approximate diffuser model from NF source data.
     *
     * When diffusers are not modelled explicitly this function creates
     * a simplified diffuser representation and converts the created source information into the format required by the
     * farfield component.
     */
    void create_diffuser_model() { std::println("Creating diffuser model..."); }

} // namespace pre_c_sumo
