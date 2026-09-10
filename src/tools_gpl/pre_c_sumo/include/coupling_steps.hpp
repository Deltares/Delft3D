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
     * @brief Internal coupling helper functions for the preC-SUMO tool.
     *
     * These functions are internal implementation helpers used by the
     * preC-SUMO library. They handle timestepping control, configuration and settings
     * file parsing and the conversion/communication of NF/FF data.
     */
    // TODO?: Move/fold into class(es)?
    constexpr std::string_view water_levels_id = "sea_surface_height";
    constexpr std::string_view bed_levels_id = "sea_floor_depth_below_geoid";
    constexpr std::string_view water_depth_id = "sea_floor_depth_below_sea_surface";
    constexpr std::string_view densities_id = "sea_water_potential_density";

    /**
     * @brief Mapping information for a diffuser's ambient and intake points.
     *
     * The mapping links a diffuser entry in the C-SUMO model to the corresponding
     * near-field intake definition and the related ambient point indices used when
     * creating the far-field input data.
     */
    struct DiffuserMapping
    {
        std::size_t diffuser_index;               ///< Index of the diffuser in the configured mesh.
        bool has_intake;                          ///< True when the diffuser has an associated intake point.
        std::size_t intake_index;                 ///< Index of the intake point in the intake mesh.
        std::size_t number_of_ambient_points;     ///< Number of ambient points associated with this diffuser.
        std::size_t first_ambient_point_index;    ///< First index of the ambient point range in the mesh.
    };

    /**
     * @brief Container for a mesh used in the preC-SUMO coupling workflow.
     *
     * Each mesh stores the coordinates, topology and scalar quantities exchanged
     * with preCICE for the 2D and 3D coupling steps. The forward mapping links
     * diffuser entries to their corresponding ambient and intake point data.
     */
    struct Mesh
    {
        std::string name;                                             ///< Mesh name as used in the preCICE configuration.
        std::vector<double> coordinates;                              ///< XYZ coordinates for each mesh node.
        std::vector<int> vertex_ids;                                  ///< Vertex identifiers used by the coupled mesh.
        std::vector<DiffuserMapping> forward_map;                     ///< Mapping from diffuser entries to intake/ambient data.
        std::size_t number_of_nodes;                                  ///< Total number of mesh nodes.
        std::size_t number_of_zcoordinates;                           ///< Number of vertical coordinates in the 3D mesh.
        std::unordered_map<std::string_view, std::vector<double>> quantities; ///< Per-quantity values associated with the mesh.
    };

    /**
     * @anchor pre_c_sumo_read_csumo_settings_file
     * @brief Read and parse the C-SUMO settings file.
     *
     * Attempts to read the C-SUMO settings from the given file.
     * On success returns a populated `CSumoSettingsReader`. On failure
     * returns a `ParseError` describing the problem.
     *
     * @param csumo_settings_file_name Path or name of the C-SUMO settings file.
     * @return std::expected containing `CSumoSettingsReader` on success or `ParseError` on failure.
     */
    std::expected<pre_c_sumo::CSumoSettingsReader, parsing_utils::ParseError> readCsumoSettingsFile(
        const std::string_view csumo_settings_file_name);

    /**
     * @anchor pre_c_sumo_receive_ff_data
     * @brief Receive farfield (FF) data from external sources via preCICE.
     *
     * This is the preCICE read step in the main coupling cycle. The function reads the
     * ambient and density data for the registered 2D and 3D meshes before the adapter emits
     * the FF2NF files for the near-field solver.
     */
    void receiveFFData(precice::Participant& participant, Mesh& csumo_2d_mesh, Mesh& csumo_3d_mesh,
                       double coupling_time_step);

    /**
     * @anchor pre_c_sumo_write_ff2nf
     * @brief Write FF2NF files based on parsed C-SUMO settings and received farfield data.
     *
     * Writes a FF2NF file for each configured diffuser.
     * If `csumo_settings` holds an error, no files are written.
     *
     * @param csumo_settings Parsed C-SUMO settings used to determine which diffuser files to write.
     * @param csumo_2d_mesh 2D mesh containing the ambient and coupling data for the far-field exchange.
     * @param csumo_3d_mesh 3D mesh containing the vertical data used when writing the FF2NF content.
     * @param current_time_seconds Current simulation time in seconds used to resolve time-dependent file names.
     */
    void writeFF2NFFiles(const CSumoSettingsReader& csumo_settings, Mesh& csumo_2d_mesh, Mesh& csumo_3d_mesh,
                         double current_time_seconds);

    /**
     * @anchor pre_c_sumo_wait_nf2ff
     * @brief Wait until NF2FF files become available.
     *
     * For each diffuser configured in `csumo_settings` this will wait for
     * the corresponding NF2FF file to appear. If `csumo_settings` contains
     * a parse error, the function returns immediately without waiting.
     * 
     * Note: If any diffuser is configured, this function will wait
     *       10 seconds for file(s) to appear. If the time elapses, it will fail with error.
     *
     * @param csumo_settings Expected C-SUMO settings or a parse error.
     * @param current_time_seconds Current time in seconds.
     * @returns true on successful wait, false on timeout.
     */
    bool waitForNF2FFFiles(const CSumoSettingsReader& csumo_settings, double current_time_seconds);

    /**
     * @anchor pre_c_sumo_read_nf2ff
     * @brief Read NF2FF files and extract the required data.
     *
     * Reads NF2FF files referenced in `csumo_settings` and extracts the
     * data that will be converted to sources/sinks.
     *
     * @param csumo_settings Expected C-SUMO settings or a parse error.
     * @param current_time_seconds Current simulation time in seconds used to locate the matching NF2FF files.
     * @returns std::vector<NF2FFReader> with the content of all NF2FF files of all
     * diffusers in the settings.
     */
    const std::vector<NF2FFReader> readNF2FFFiles(const CSumoSettingsReader& csumo_settings,
                                                  double current_time_seconds);

    /**
     * @anchor pre_c_sumo_convert_nf_to_sources_sinks
     * @brief Convert NF data to sources and sinks to be communicated via preCICE.
     *
     * Uses the data referenced in `csumo_settings` to perform the conversion.
     *
     * @param csumo_settings Expected C-SUMO settings or a parse error.
     */
    void convertNFToSourcesSinks(const CSumoSettingsReader& csumo_settings);

    /**
     * @anchor pre_c_sumo_convert_nf_to_connected_sink_sources
     * @brief Convert NF2FF results into connected source/sink entries for the FM adapter.
     *
     * Uses the data referenced in @p nf2ff_readers and @p csumo_settings to perform the conversion.
     *
     * @param csumo_settings Parsed C-SUMO settings.
     * @param nf2ff_readers NF2FF snapshots containing the latest near-field data.
     *
     * @return Connected source/sink pairs to be written to preCICE.
     */
    [[nodiscard]] std::expected<pre_c_sumo::ConnectedSinkSources, pre_c_sumo::ConnectedSinkSourcesError>
    convertNFtoConnectedSinkSources(const pre_c_sumo::CSumoSettingsReader& csumo_settings,
                                    const std::vector<NF2FFReader>& nf2ff_readers);
    /**
     * @anchor pre_c_sumo_send_sources_sinks_to_ff
     * @brief Send computed sources/sinks to the farfield model.
     *
     * Sends the converted sources and sinks to the farfield component.
     * The demo implementation logs an informational message.
     *
     * @param participant Active preCICE participant used to write the exchange data.
     * @param sources_sinks Source and sink exchange data prepared for the far-field send.
     */
    void sendSourcesSinksToFF(precice::Participant& participant, SourcesSinks& sources_sinks);

    /**
     * @anchor pre_c_sumo_is_diffuser_modelled
     * @brief Query whether the diffuser is modelled explicitly.
     *
     * @return true if the diffuser is modelled, false otherwise.
     */
    bool isDiffuserModelled(const NF2FFReader& diffuser);

    /**
     * @anchor pre_c_sumo_process_source_locations
     * @brief Process explicit source locations from NF data.
     *
     * Converts NF source information into the format required by the farfield component.
     */
    void processSourceLocations();

    /**
     * @anchor pre_c_sumo_create_diffuser_model
     * @brief Create an approximate diffuser model from NF source data.
     *
     * When diffusers are not modelled explicitly this function creates
     * the sources for a simplified diffuser representation that can be used
     * to create the farfield component.
     */
    std::vector<SourceOrSinkData> createDiffuserModel(const NF2FFReader& diffuser);

} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_COUPLING_STEPS_HPP
