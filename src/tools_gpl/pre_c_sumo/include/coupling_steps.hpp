#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_COUPLING_STEPS_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_COUPLING_STEPS_HPP

#include <expected>
#include <string_view>

#include "csumo_settings_reader.hpp"
#include "parsing_types.hpp"
#include "precice_state.hpp"

namespace pre_c_sumo
{
    /**
     * @file pre_c_sumo_internal.hpp
     * @brief Internal helper functions for the preC-SUMO tool.
     *
     * These functions are internal implementation helpers used by the
     * preC-SUMO library. They handle timestepping control, configuration and settings
     * files parsing and the conversion/communication of NF/FF data.
     */

    /**
     * @brief Read and parse the C-SUMO settings file.
     *
     * Attempts to read the C-SUMO settings from the given file.
     * On success returns a populated `CSumoSettingsReader`. On failure
     * returns a `ParseError` describing the problem.
     *
     * @param csumoSettingsFileName Path or name of the C-SUMO settings file.
     * @return std::expected containing `CSumoSettingsReader` on success or `ParseError` on failure.
     */
    std::expected<pre_c_sumo::CSumoSettingsReader, parsing_utils::ParseError> readCsumoSettingsFile(
        const std::string_view csumoSettingsFileName);

    /**
     * @brief Receive farfield (FF) data from external sources via preCICE.
     *
     * Blocking receive of farfield data via preCICE.
     * The demo implementation only logs a message.
     */
    void receiveFFData(const PreCICEState& precice_state);

    /**
     * @brief Write FF2NF files based on parsed C-SUMO settings and received farfield data.
     *
     * Writes a FF2NF file for each configured diffuser.
     * If `csumoSettings` holds an error, no files are written.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
    void writeFF2NFFiles(const CSumoSettingsReader& csumoSettings);

    /**
     * @brief Wait until NF2FF files become available.
     *
     * For each diffuser configured in `csumoSettings` this will wait for
     * the corresponding NF2FF file to appear. If `csumoSettings` contains
     * a parse error, the behaviour is undefined in the demo implementation.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
    void waitForNF2FFFiles(const CSumoSettingsReader& csumoSettings);

    /**
     * @brief Read NF2FF files and extract the required data.
     *
     * Reads NF2FF files referenced in `csumoSettings` and extracts the
     * data that will be converted to sources/sinks.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
    void readNF2FFFiles(const CSumoSettingsReader& csumoSettings);

    /**
     * @brief Convert NF data to sources and sinks to be communicated via preCICE.
     *
     * Uses the data referenced in `csumoSettings` to perform the conversion.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
    void convertNFToSourcesSinks(const CSumoSettingsReader& csumoSettings);

    /**
     * @brief Send computed sources/sinks to the farfield model.
     *
     * Sends the converted sources and sinks to the farfield component.
     * The demo implementation logs an informational message.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
    void sendSourcesSinksToFF(const CSumoSettingsReader& csumoSettings);

    /**
     * @brief Convert NF sinks to farfield sinks.
     *
     * Converts NF sink information into the format required by the
     * farfield component.
     */
    void convertNFSinksToFF();

    /**
     * @brief Convert NF intakes to farfield sinks.
     *
     * Converts NF intake information into the format required by the
     * farfield component.
     */
    void convertNFIntakesToFF();

    /**
     * @brief Convert NF source definitions to farfield sources.
     *
     * Depending on whether a diffuser is modelled this will either
     * process explicit source locations or build a diffuser model.
     */
    void convertNFSourcesToFF();

    /**
     * @brief Query whether the diffuser is modelled explicitly.
     *
     * @return true if the diffuser is modelled, false otherwise.
     */
    bool isDiffuserModelled();

    /**
     * @brief Process explicit source locations from NF data.
     *
     * Converts NF source information into the format required by the farfield component.
     */
    void processSourceLocations();

    /**
     * @brief Create an approximate diffuser model from NF source data.
     *
     * When diffusers are not modelled explicitly this function creates
     * a simplified diffuser representation and converts the created source information into the format required by the
     * farfield component.
     */
    void createDiffuserModel();

} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_COUPLING_STEPS_HPP
