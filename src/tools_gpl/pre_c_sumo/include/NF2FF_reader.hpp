#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_NF2FF_READER_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_NF2FF_READER_HPP

#include "csumo_settings_reader.hpp" // For Discharge, ConstituentsOperator
#include "parsing_types.hpp"         // For parsing_utils::Point2D

#include <expected>
#include <filesystem>
#include <pugixml.hpp>
#include <string>
#include <string_view>
#include <vector>

/*** Example input file.
 * <NF2FF>
 *    <fileVersion>0.3</fileVersion>
 *    <discharge>
 *       <Qintake>10.0</Qintake>
 *       <Qsource>10.0</Qsource>
 *       <!--  Constituents in fixed order: Temperature, Salinity, Sediments, Tracers
 *             Operator: "absolute" values or "excess" (dT,dS,d..)  -->
 *       <constituentsOperator>excess</constituentsOperator>
 *       <constituents>10.0 0.0</constituents>
 *    </discharge>
 *    <NFResult>
 *       <sinks>
 *          250.000 350.087 9.700 1.000 0.000 0.000
 *          252.500 350.048 9.700 5     0.250 0.380
 *       </sinks>
 *       <sources>
 *          1050.000 350.365 5.000 5.000 5 15.000
 *          1050.500 350.365 5.000 5.000 5 15.000
 *       </sources>
 *    </NFResult>
 * </NF2FF>
 ***/

namespace pre_c_sumo
{
    // Raw NF2FF point data as stored in the XML file.
    // The reader does not interpret these coordinates into FM cell indices.
    // That mapping stays on the FM side. This type is intended as the input
    // to the pre-C-SUMO conversion step that creates the source/sink data
    // communicated via preCICE.
    struct SourceOrSinkData
    {
        double x_coordinate;      ///< Horizontal X coordinate [m].
        double y_coordinate;      ///< Horizontal Y coordinate [m].
        double z_coordinate;      ///< Vertical coordinate [m].
        double entrainment;       ///< S-factor used for entrainment / discharge scaling.
        double half_plume_height; ///< Half plume height [m].
        double half_plume_width;  ///< Half plume width [m].
        double u_magnitude;       ///< Optional momentum magnitude [m/s].
        double u_direction;       ///< Optional momentum direction [degrees].
        double weight;            ///< Optional aggregation weight for later use.
        bool has_u;               ///< True when momentum fields were present in the XML.
        bool has_weight;          ///< True when an explicit weight value was present.
    };

    // Reader for NF2FF XML files.
    // The parsed data is intentionally kept close to the XML structure so the
    // conversion layer can decide how to map it to preCICE-ready source and sink
    // records. In particular, no lumping and no coordinate-to-cell mapping is
    // performed here.
    class NF2FFReader
    {
    public:
        // Reads NF2FF XML content from a file.
        // Expected input format:
        // - &lt;fileVersion&gt; must match the supported NF2FF version.
        // - &lt;discharge&gt; contains Qintake, Qsource, constituentsOperator and constituents.
        // - &lt;NFResult&gt; contains &lt;sources&gt; and &lt;sinks&gt; blocks.
        //
        // The returned reader exposes the raw point vectors so a later conversion
        // step can turn them into source and sink data for preCICE.
        [[nodiscard]] static std::expected<NF2FFReader, parsing_utils::ParseError> fromFile(
            const std::filesystem::path& file_path);

        // Reads NF2FF XML content from a string.
        // This overload is useful for tests and for callers that already have the
        // XML payload in memory.
        [[nodiscard]] static std::expected<NF2FFReader, parsing_utils::ParseError> fromString(
            const std::string_view xml);

        // Supported NF2FF file version.
        std::string_view fileVersion() const;
        // Total intake flow rate declared in &lt;Qintake&gt;.
        double intakeFlowRate() const;
        // Total source flow rate declared in &lt;Qsource&gt;.
        double sourceFlowRate() const;
        // Constituent operator declared in &lt;constituentsOperator&gt;.
        ConstituentsOperator constituentsOperator() const;
        // Constituents declared in &lt;constituents&gt;.
        std::vector<double> constituents() const;
        // Raw source points from &lt;NFResult&gt;/&lt;sources&gt;.
        std::vector<pre_c_sumo::SourceOrSinkData> sources() const;
        // Raw sink points from &lt;NFResult&gt;/&lt;sinks&gt;.
        std::vector<pre_c_sumo::SourceOrSinkData> sinks() const;

    private:
        explicit NF2FFReader(std::string file_version, pugi::xml_document document, double intake_flow_rate,
                             double source_flow_rate, ConstituentsOperator constituents_operator,
                             std::vector<double> constituents, std::vector<pre_c_sumo::SourceOrSinkData> sources,
                             std::vector<pre_c_sumo::SourceOrSinkData> sinks);

        constexpr static std::string_view root_element_name = "NF2FF";
        constexpr static std::string_view current_file_version = "0.3";
        std::string file_version_;
        pugi::xml_document document_;
        double intake_flow_rate_;
        double source_flow_rate_;
        pre_c_sumo::ConstituentsOperator constituents_operator_;
        std::vector<double> constituents_;
        std::vector<pre_c_sumo::SourceOrSinkData> sources_;
        std::vector<pre_c_sumo::SourceOrSinkData> sinks_;
    };

} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_NF2FF_READER_HPP
