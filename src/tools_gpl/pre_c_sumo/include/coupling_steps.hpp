#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_COUPLING_STEPS_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_COUPLING_STEPS_HPP

#include <precice/precice.hpp>
#include <expected>
#include <string_view>
#include <vector>
#include <unordered_map>

#include "csumo_settings_reader.hpp"
#include "connected_sinks_sources.hpp"
#include "parsing_types.hpp"
#include "NF2FF_reader.hpp"
#include "pre_c_sumo_lib.hpp"

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
    // TODO?: Move/fold into class(es)?
    constexpr std::string_view water_levels_id = "sea_surface_height";
    constexpr std::string_view bed_levels_id = "sea_floor_depth_below_geoid";
    constexpr std::string_view water_depth_id = "sea_floor_depth_below_sea_surface";
    constexpr std::string_view densities_id = "sea_water_potential_density";

    struct DiffuserMapping
    {
        std::size_t diffuser_index;
        bool has_intake;
        std::size_t intake_index;
        std::size_t number_of_ambient_points;
        std::size_t first_ambient_point_index;
    };

    struct Mesh
    {
        std::string name;
        std::vector<double> coordinates;
        std::vector<int> vertex_ids;
        std::vector<DiffuserMapping> forward_map;
        std::size_t number_of_nodes;
        std::size_t number_of_zcoordinates;
        std::unordered_map<std::string_view, std::vector<double>> quantities;
    };

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
    void receiveFFData(precice::Participant& participant, Mesh& csumo_2d_mesh, Mesh& csumo_3d_mesh,
                       double coupling_time_step);

    /**
     * @brief Write FF2NF files based on parsed C-SUMO settings and received farfield data.
     *
     * Writes a FF2NF file for each configured diffuser.
     * If `csumoSettings` holds an error, no files are written.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
    void writeFF2NFFiles(const CSumoSettingsReader& csumoSettings, Mesh& csumo_2d_mesh, Mesh& csumo_3d_mesh,
                         double current_time_seconds);

    /**
     * @brief Wait until NF2FF files become available.
     *
     * For each diffuser configured in `csumoSettings` this will wait for
     * the corresponding NF2FF file to appear. If `csumoSettings` contains
     * a parse error, the function returns immediately without waiting.
     *
     * Note: If any diffuser is configured, this function will wait
     *       INDEFINITELY for file(s) to appear.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     * @param current_time_seconds Current time in seconds.
     */
    void waitForNF2FFFiles(const CSumoSettingsReader& csumoSettings, double current_time_seconds);

    /**
     * @brief Read NF2FF files and extract the required data.
     *
     * Reads NF2FF files referenced in `csumoSettings` and extracts the
     * data that will be converted to sources/sinks.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     * @returns std::vector<NF2FFReader> with the content of all NF2FF files of all
     * diffusers in the settings.
     */
    const std::vector<NF2FFReader> readNF2FFFiles(const CSumoSettingsReader& csumoSettings,
                                                  double current_time_seconds);

    /**
     * @brief Convert NF data to sources and sinks to be communicated via preCICE.
     *
     * Uses the data referenced in `csumoSettings` to perform the conversion.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
    void convertNFToSourcesSinks(const CSumoSettingsReader& csumoSettings);

    pre_c_sumo::ConnectedSinkSources convertNFtoConnectedSinkSources(const pre_c_sumo::CSumoSettingsReader& settings,
                                                                     const std::vector<NF2FFReader>& readers);
    /**
     * @brief Send computed sources/sinks to the farfield model.
     *
     * Sends the converted sources and sinks to the farfield component.
     * The demo implementation logs an informational message.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
    void sendSourcesSinksToFF(precice::Participant& participant, SourcesSinks& sources_sinks);

    /**
     * @brief Query whether the diffuser is modelled explicitly.
     *
     * @return true if the diffuser is modelled, false otherwise.
     */
    bool isDiffuserModelled(const NF2FFReader& diffuser);

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
