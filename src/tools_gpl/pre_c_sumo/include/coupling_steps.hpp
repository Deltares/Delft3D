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
     * @file coupling_steps.hpp
     * @brief Internal helper functions for the preC-SUMO tool.
     *
     * These functions are internal implementation helpers used by the
     * preC-SUMO library. They handle timestepping control, configuration and settings
     * file parsing, and the conversion and communication of near-field/far-field data.
     *
     * Workflow overview for the current implementation:
     * @dotfile preC_SUMO_Swimlanes.dot
     */
    // TODO?: Move/fold into class(es)?
    constexpr std::string_view water_levels_id = "sea_surface_height";
    constexpr std::string_view bed_levels_id = "sea_floor_depth_below_geoid";
    constexpr std::string_view water_depth_id = "sea_floor_depth_below_sea_surface";
    constexpr std::string_view densities_id = "sea_water_potential_density";

    /**
     * @anchor pre_c_sumo_diffuser_mapping
     * @brief Maps a diffuser onto its intake and ambient point ranges.
     */
    struct DiffuserMapping
    {
        std::size_t diffuser_index;
        bool has_intake;
        std::size_t intake_index;
        std::size_t number_of_ambient_points;
        std::size_t first_ambient_point_index;
    };

    /**
     * @anchor pre_c_sumo_mesh
     * @brief Aggregated mesh state used during the coupling exchange.
     */
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
     * @anchor pre_c_sumo_read_settings
     * @brief Reads and parses a C-SUMO settings file.
     *
     * Attempts to read the C-SUMO settings from the given file. On success it returns a populated
     * `CSumoSettingsReader`; on failure it returns a `ParseError` describing the problem.
     *
     * @param csumoSettingsFileName Path or name of the C-SUMO settings file.
     * @return Parsed settings or a parse error.
     */
    std::expected<pre_c_sumo::CSumoSettingsReader, parsing_utils::ParseError> readCsumoSettingsFile(
        const std::string_view csumoSettingsFileName);

    /**
     * @anchor pre_c_sumo_receive_ff_data
     * @brief Receives far-field data from external sources through preCICE.
     *
     * This is a blocking receive step. The demo implementation logs the action rather than applying
     * additional logic.
     *
     * @param participant preCICE participant used for the receive operation.
     * @param csumo_2d_mesh 2D mesh data received from preCICE.
     * @param csumo_3d_mesh 3D mesh data received from preCICE.
     * @param coupling_time_step Time step size in seconds.
     */
    void receiveFFData(precice::Participant& participant, Mesh& csumo_2d_mesh, Mesh& csumo_3d_mesh,
                       double coupling_time_step);

    /**
     * @anchor pre_c_sumo_write_ff2nf
     * @brief Writes FF2NF files from parsed C-SUMO settings and the received far-field data.
     *
     * Writes one FF2NF file for each configured diffuser. If `csumoSettings` holds an error, no files are written.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     * @param csumo_2d_mesh 2D mesh data received from preCICE.
     * @param csumo_3d_mesh 3D mesh data received from preCICE.
     * @param current_time_seconds Current coupling time in seconds.
     */
    void writeFF2NFFiles(const CSumoSettingsReader& csumoSettings, Mesh& csumo_2d_mesh, Mesh& csumo_3d_mesh,
                         double current_time_seconds);

    /**
     * @anchor pre_c_sumo_wait_nf2ff
     * @brief Waits until the required NF2FF files are available.
     *
     * For each diffuser configured in `csumoSettings`, this function waits for the corresponding NF2FF file to appear.
     * If `csumoSettings` contains a parse error, the function returns immediately without waiting.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     * @param current_time_seconds Current coupling time in seconds.
     * @return True on successful wait, false on timeout.
     */
    bool waitForNF2FFFiles(const CSumoSettingsReader& csumoSettings, double current_time_seconds);

    /**
     * @anchor pre_c_sumo_read_nf2ff
     * @brief Reads NF2FF files and extracts the data required for conversion.
     *
     * This reads the NF2FF files referenced by `csumoSettings` and returns the parsed snapshots used to produce
     * source/sink exchanges.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     * @param current_time_seconds Current coupling time in seconds.
     * @return Parsed NF2FF readers for all relevant diffusers.
     */
    const std::vector<NF2FFReader> readNF2FFFiles(const CSumoSettingsReader& csumoSettings,
                                                  double current_time_seconds);

    /**
     * @anchor pre_c_sumo_convert_nf_to_sources_sinks
     * @brief Converts near-field data to source/sink entries for preCICE communication.
     *
     * Uses the data referenced by `csumoSettings` to perform the conversion.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
    void convertNFToSourcesSinks(const CSumoSettingsReader& csumoSettings);

    /**
     * @anchor pre_c_sumo_convert_nf
     * @brief Converts NF2FF data to connected source/sink pairs.
     *
     * Uses the data from `nf2ff_readers` and the parsed C-SUMO configuration to construct the source/sink pairs that
     * are written to preCICE.
     *
     * @param csumoSettings Parsed C-SUMO settings.
     * @param nf2ff_readers NF2FF snapshots containing the latest near-field data.
     * @return Connected source and sink pairs ready for preCICE output.
     */
    pre_c_sumo::ConnectedSinkSources convertNFtoConnectedSinkSources(
        const pre_c_sumo::CSumoSettingsReader& csumoSettings, const std::vector<NF2FFReader>& nf2ff_readers);

    /**
     * @anchor pre_c_sumo_send_sources_sinks_to_ff
     * @brief Sends the computed source/sink data to the far-field model.
     *
     * This function dispatches the converted source/sink values to the far-field participant. The demo implementation
     * logs an informational message instead of performing additional data processing.
     *
     * @param participant preCICE participant used for writing values.
     * @param sources_sinks Connected source/sink data prepared for writing.
     */
    void sendSourcesSinksToFF(precice::Participant& participant, SourcesSinks& sources_sinks);

    /**
     * @anchor pre_c_sumo_is_diffuser_modelled
     * @brief Returns whether the diffuser is modeled explicitly.
     * @param diffuser NF2FF reader for the diffuser under inspection.
     * @return True if modeled explicitly, false otherwise.
     */
    bool isDiffuserModelled(const NF2FFReader& diffuser);

    /**
     * @anchor pre_c_sumo_process_source_locations
     * @brief Processes explicit source locations from near-field data.
     *
     * Converts NF source information into the format required by the far-field component.
     */
    void processSourceLocations();

    /**
     * @anchor pre_c_sumo_create_diffuser_model
     * @brief Creates an approximate diffuser model from NF source data.
     *
     * When diffusers are not modeled explicitly, this function creates the simplified source representation used to
     * feed the far-field component.
     *
     * @param diffuser NF2FF data for the diffuser being approximated.
     * @return Approximate source/sink records for the diffuser model.
     */
    std::vector<SourceOrSinkData> createDiffuserModel(const NF2FFReader& diffuser);

} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_COUPLING_STEPS_HPP
