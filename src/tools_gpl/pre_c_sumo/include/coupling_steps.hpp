#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_COUPLING_STEPS_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_COUPLING_STEPS_HPP
#include <expected>

#include <string_view>
#include "csumo_settings_reader.hpp"

namespace pre_c_sumo
{
    /**
     * @file pre_c_sumo_internal.hpp
     * @brief Internal helper functions for the preC-SUMO tool.
     *
     * These functions are internal implementation helpers used by the
     * preC-SUMO library. They handle timestepping control, configuration
     * file parsing and the conversion/communication of NF/FF data.
     */

    /**
     * @brief Drive the main timeloop of the preC-SUMO demonstration.
     *
     * This function returns true while the timeloop should continue, and
     * false when it should stop. The current implementation contains a
     * simple demonstration counter.
     *
     * @return true if the timeloop should continue, false otherwise.
     */
    bool do_timeloop();

    /**
     * @brief Read and parse the C-SUMO configuration file.
     *
     * Attempts to read the C-SUMO configuration from the given file.
     * On success returns a populated `CSumoSettingsReader`. On failure
     * returns a `ParseError` describing the problem.
     *
     * @param csumoConfigFileName Path or name of the C-SUMO configuration file.
     * @return std::expected containing `CSumoSettingsReader` on success or `ParseError` on failure.
     */
    std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError> read_csumo_config_file(
        const std::string_view csumoConfigFileName);

    /**
     * @brief Receive farfield (FF) data from external sources via preCICE.
     *
     * Blocking receive of farfield data via preCICE.
     * The demo implementation only logs a message.
     */
    void receive_ff_data();

    /**
     * @brief Write FF2NF files based on parsed C-SUMO settings and received farfield data.
     *
     * Writes a FF2NF file for each configured diffuser.
     * If `csumoSettings` holds an error, no files are written.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
    void write_ff2nf_files(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError> csumoSettings);

    /**
     * @brief Wait until NF2FF files become available.
     *
     * For each diffuser configured in `csumoSettings` this will wait for
     * the corresponding NF2FF file to appear. If `csumoSettings` contains
     * a parse error, the behaviour is undefined in the demo implementation.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
    void wait_for_nf2ff_files(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError> csumoSettings);

    /**
     * @brief Read NF2FF files and extract the required data.
     *
     * Reads NF2FF files referenced in `csumoSettings` and extracts the
     * data that will be converted to sources/sinks.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
    void read_nf2ff_files(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError> csumoSettings);

    /**
     * @brief Convert NF data to sources and sinks to be communicated via preCICE.
     *
     * Uses the data referenced in `csumoSettings` to perform the conversion.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
    void convert_nf_to_sources_sinks(
        std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError> csumoSettings);

    /**
     * @brief Send computed sources/sinks to the farfield model.
     *
     * Sends the converted sources and sinks to the farfield component.
     * The demo implementation logs an informational message.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
    void send_sources_sinks_to_ff(std::expected<pre_c_sumo::CSumoSettingsReader, pre_c_sumo::ParseError> csumoSettings);

    /**
     * @brief Convert NF sinks to farfield sinks.
     *
     * Converts NF sink information into the format required by the
     * farfield component.
     */
    void convert_nf_sinks_to_ff();

    /**
     * @brief Convert NF intakes to farfield sinks.
     *
     * Converts NF intake information into the format required by the
     * farfield component.
     */
    void convert_nf_intakes_to_ff();

    /**
     * @brief Convert NF source definitions to farfield sources.
     *
     * Depending on whether a diffuser is modelled this will either
     * process explicit source locations or build a diffuser model.
     */
    void convert_nf_sources_to_ff();

    /**
     * @brief Query whether the diffuser is modelled explicitly.
     *
     * @return true if the diffuser is modelled, false otherwise.
     */
    bool is_diffuser_modelled();

    /**
     * @brief Process explicit source locations from NF data.
     *
     * Converts NF source information into the format required by the farfield component.
     */
    void process_source_locations();

    /**
     * @brief Create an approximate diffuser model from NF source data.
     *
     * When diffusers are not modelled explicitly this function creates
     * a simplified diffuser representation and converts the created source information into the format required by the
     * farfield component.
     */
    void create_diffuser_model();

} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_COUPLING_STEPS_HPP
