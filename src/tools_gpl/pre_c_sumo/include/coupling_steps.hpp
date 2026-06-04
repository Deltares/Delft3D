#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_COUPLING_STEPS_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_COUPLING_STEPS_HPP

#include <precice/precice.hpp>
#include <expected>
#include <optional>
#include <string_view>
#include <vector>
#include <unordered_map>

#include "csumo_settings_reader.hpp"
#include "NF2FF_reader.hpp"
#include "parsing_types.hpp"
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
     * a parse error, the behaviour is undefined in the demo implementation.
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     */
    void waitForNF2FFFiles(const CSumoSettingsReader& csumoSettings);

    /**
     * @brief Read NF2FF files and extract the raw data needed for conversion.
     *
     * Reads NF2FF files referenced in `csumoSettings` and extracts the raw
     * discharge, intake, source, sink, and constituent data.
     * No lumping, no point aggregation, and no conversion from x/y coordinates
     * to FM cell ids happens here.
     *
     * @param csumoSettings Parsed C-SUMO settings.
     */
    void readNF2FFFiles(const CSumoSettingsReader& csumoSettings);

    /**
     * @brief Convert raw NF2FF input to preCICE-ready source and sink data.
     *
     * This is the pre-C-SUMO side of the handoff. It preserves the raw
     * source/sink geometry from NF2FFReader and produces data that the FM side
     * can consume via preCICE. The FM side remains responsible for mapping
     * x/y positions to cell ids and for the nearfield::dischargeToSrc step that
     * turns discharge information into FM source-sink entries.
     *
     * Current scope:
     * - convert S factors into the entrainment/discharge representation used by FM
     * - keep intake, discharge, source, and sink points as separate records
     * - do not lump points together yet
     * - do not derive FM cell ids here
     *
     * Future scope:
     * - aggregation of points, when enabled
     * - DESA-based processing, when enabled
     *
     * @param csumoSettings Expected C-SUMO settings or a parse error.
     * @note The converted data must remain compatible with nearfield::dischargeToSrc
     *       on the FM side.
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
    void sendSourcesSinksToFF(precice::Participant& participant, SourcesSinks& sources_sinks);

    /**
     * @brief Convert NF sink records to the far-field-facing sink stream.
     *
     * The conversion keeps the sink records separate and preserves their
     * coordinates and plume parameters. Any FM cell lookup is deferred to the
     * FM side. This is where the S-factor based entrainment logic is translated
     * into the source/sink representation required by preCICE and by the FM
     * nearfield coupling.
     */
    void convertNFSinksToFF();

    /**
     * @brief Convert one parsed NF2FF diffuser result to preCICE source/sink records.
     *
     * This conversion mirrors the FM nearfield intent while keeping FM-specific
     * cell mapping out of pre-C-SUMO:
        * - Entrainment records are derived from the ordered sink sequence, not read
        *   as explicit pairs from the NF2FF file.
        * - Discharge records are created from Qsource at source points.
        * - Optional intake record is created from Qintake at a provided intake point.
        *
        * Pairing semantics
        * -----------------
        * The NF2FF file only distinguishes raw `sinks`, `sources`, and optional
        * `intakes`.
        *
        * This function derives that structure as follows:
        * - For each sink index `i >= 2`, compute `delta_s = S_i - S_(i-1)`.
        * - For each source point, create one entrainment sink record and one linked
        *   entrainment source record.
        * - The sink-side entrainment record receives discharge
        *   `-delta_s * Qsource * normalized_weight`.
        * - The source-side entrainment record receives discharge
        *   `+delta_s * Qsource * normalized_weight`.
        * - Those two records are marked as a pair by reciprocal `connected_id`
        *   values.
        *
        * As a result, output records with `connected_id != 0` are derived
        * entrainment pairs created by this conversion. Output records with
        * `connected_id == 0` are unpaired records representing the source discharge
        * itself or the optional intake sink.
     *
     * Source weighting follows these rules:
        * - If a source line has an explicit weight, that weight is used.
     * - Otherwise, a default weight of 1.0 is used.
        * - All source weights are normalized by their sum before applying them.
        * - The normalized weight scales both entrainment-derived discharge and the
        *   final discharge-at-source record for that source.
        *
     *
     * Record ids are assigned sequentially starting at `first_record_id`.
     * Paired entrainment sink/source records are connected by `connected_id`.
     *
     * @param nf2ff_reader Parsed NF2FF input for one diffuser.
     * @param sources_sinks Output container receiving appended records.
     * @param first_record_id First logical record id to assign.
     * @param intake_point Optional intake position used to add an intake sink record.
     * @return Next available record id after all appended records.
     */
    double convertNFSinksToFF(const NF2FFReader& nf2ff_reader, SourcesSinks& sources_sinks, double first_record_id,
                              const std::optional<parsing_utils::Point2D>& intake_point = std::nullopt);

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
