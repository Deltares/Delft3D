#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_NF2FF_READER_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_NF2FF_READER_HPP

#include "csumo_settings_reader.hpp" // For Discharge, ConstituentsOperator
#include "parsing_types.hpp"         // For parsing_utils::Point2D

#include <expected>
#include <pugixml.hpp>
#include <string>
#include <string_view>
#include <vector>

/*** Example input file.
 * &lt;NF2FF&gt;
 *    &lt;fileVersion&gt;0.3&lt;/fileVersion&gt;
 *    &lt;discharge&gt;
 *       &lt;Qintake&gt;10.0&lt;/Qintake&gt;
 *       &lt;Qsource&gt;10.0&lt;/Qsource&gt;
 *       <!--  Constituents in fixed order: Temperature, Salinity, Sediments, Tracers
 *             Operator: "absolute" values or "excess" (dT,dS,d..)  -->
 *       &lt;constituentsOperator&gt;excess&lt;/constituentsOperator&gt;
 *       &lt;constituents&gt;10.0 0.0&lt;/constituents&gt;
 *    &lt;/discharge&gt;
 *    &lt;NFResult&gt;
 *       &lt;sinks&gt;
 *          250.000 350.087 9.700 1.000 0.000 0.000
 *          252.500 350.048 9.700 5     0.250 0.380
 *       &lt;/sinks&gt;
 *       &lt;sources&gt;
 *          1050.000 350.365 5.000 5.000 5 15.000
 *          1050.500 350.365 5.000 5.000 5 15.000
 *       &lt;/sources&gt;
 *    &lt;/NFResult&gt;
 * &lt;/NF2FF&gt;
 ***/

namespace pre_c_sumo
{
    /**
     * @anchor pre_c_sumo_source_or_sink_data
     * @brief Source or sink data parsed from NF2FF content.
     */
    struct SourceOrSinkData
    {
        double x_coordinate;
        double y_coordinate;
        double z_coordinate;
        double entrainment;
        double half_plume_height;
        double half_plume_width;
        double u_magnitude;
        double u_direction;
        double weight;
        bool has_u;
        bool has_weight;
    };

    /**
     * @anchor pre_c_sumo_intake_data
     * @brief Intake point data parsed from NF2FF content.
     */
    struct IntakeData
    {
        double x_coordinate;
        double y_coordinate;
        double z_coordinate;
        double weight;
        bool has_weight;
    };

    /**
     * @anchor pre_c_sumo_nf2ff_reader
     * @brief Reader for NF2FF XML files.
     */
    class NF2FFReader
    {
    public:
        /**
         * @anchor pre_c_sumo_nf2ff_reader_from_file
         * @brief Reads NF2FF XML content from a file.
         * @param file_path Path to the input file.
         * @return Parsed reader on success, or a parsing_utils::ParseError on failure.
         */
        [[nodiscard]] static std::expected<NF2FFReader, parsing_utils::ParseError> fromFile(
            const std::filesystem::path& file_path);

        /**
         * @anchor pre_c_sumo_nf2ff_reader_from_string
         * @brief Reads NF2FF XML content from a string.
         * @param xml XML input string.
         * @return Parsed reader on success, or a parsing_utils::ParseError on failure.
         */
        [[nodiscard]] static std::expected<NF2FFReader, parsing_utils::ParseError> fromString(
            const std::string_view xml);

        /**
         * @anchor pre_c_sumo_nf2ff_reader_file_version
         * @brief Returns the NF2FF file version.
         * @return File version string.
         */
        std::string_view fileVersion() const;

        /**
         * @anchor pre_c_sumo_nf2ff_reader_intake_flow_rate
         * @brief Returns the intake flow rate from the XML.
         * @return Intake flow rate [m³/s].
         */
        double intakeFlowRate() const;

        /**
         * @anchor pre_c_sumo_nf2ff_reader_source_flow_rate
         * @brief Returns the source flow rate from the XML.
         * @return Source flow rate [m³/s].
         */
        double sourceFlowRate() const;

        /**
         * @anchor pre_c_sumo_nf2ff_reader_constituents_operator
         * @brief Returns the constituent operator used in the NF2FF file.
         * @return Concentration operator for the constituent values.
         */
        ConstituentsOperator constituentsOperator() const;

        /**
         * @anchor pre_c_sumo_nf2ff_reader_constituents
         * @brief Returns the constituent concentrations included in the XML.
         * @return Vector of constituent values.
         */
        std::vector<double> constituents() const;

        /**
         * @anchor pre_c_sumo_nf2ff_reader_intakes
         * @brief Returns all intake entries parsed from the XML.
         * @return Intake records.
         */
        std::vector<pre_c_sumo::IntakeData> intakes() const;

        /**
         * @anchor pre_c_sumo_nf2ff_reader_sources
         * @brief Returns all source entries parsed from the XML.
         * @return Source records.
         */
        std::vector<pre_c_sumo::SourceOrSinkData> sources() const;

        /**
         * @anchor pre_c_sumo_nf2ff_reader_sinks
         * @brief Returns all sink entries parsed from the XML.
         * @return Sink records.
         */
        std::vector<pre_c_sumo::SourceOrSinkData> sinks() const;

    private:
        explicit NF2FFReader(std::string file_version, pugi::xml_document document, double intake_flow_rate,
                             double source_flow_rate, ConstituentsOperator constituents_operator,
                             std::vector<double> constituents, std::vector<pre_c_sumo::IntakeData> intakes,
                             std::vector<pre_c_sumo::SourceOrSinkData> sources,
                             std::vector<pre_c_sumo::SourceOrSinkData> sinks);

        constexpr static std::string_view root_element_name = "NF2FF";
        constexpr static std::string_view current_file_version = "0.3";
        std::string file_version_;
        pugi::xml_document document_;
        double intake_flow_rate_;
        double source_flow_rate_;
        pre_c_sumo::ConstituentsOperator constituents_operator_;
        std::vector<double> constituents_;
        std::vector<pre_c_sumo::IntakeData> intakes_;
        std::vector<pre_c_sumo::SourceOrSinkData> sources_;
        std::vector<pre_c_sumo::SourceOrSinkData> sinks_;
    };

} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_NF2FF_READER_HPP
