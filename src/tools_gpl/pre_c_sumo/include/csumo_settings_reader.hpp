#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_SETTINGS_READER_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_SETTINGS_READER_HPP

#include <expected>
#include <filesystem>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "parsing_utils.hpp"

namespace pre_c_sumo
{
    /**
     * @brief Operator applied to constituent concentrations.
     *
     * Corresponds to the &lt;constituentsOperator&gt; element inside &lt;discharge&gt;.
     * - Absolute: values are absolute concentrations.
     * - Excess: values are excess (delta) concentrations relative to ambient.
     */
    enum class ConstituentsOperator
    {
        Absolute, ///< Absolute concentration values
        Excess,   ///< Excess (delta) concentration values
    };

    /**
     * @brief Discharge characteristics of a near-field diffuser.
     *
     * Corresponds to the &lt;discharge&gt; element inside &lt;data&gt;.
     */
    struct Discharge
    {
        double flow_rate{}; ///< Volume flow rate [m³/s] (&lt;M3s&gt;)
        ConstituentsOperator
            constituents_operator{}; ///< Operator for constituent values (&lt;constituentsOperator&gt;)
        std::vector<double>
            constituents; ///< Concentrations: temperature, salinity, sediments, tracers (&lt;constituents&gt;)
    };

    /**
     * @brief Settings for a single near-field diffuser.
     *
     * Corresponds to one &lt;settings&gt; block in the COSUMO XML.
     */
    struct DiffuserSettings
    {
        // --- general section ---
        std::optional<std::string> id;              ///< Diffuser identifier (&lt;ID&gt;, optional)
        std::optional<std::string> sub_grid_model;  ///< Sub-grid model type (&lt;subGridModel&gt;, optional)
        std::optional<std::string> far_field_model; ///< Far-field model name (&lt;farFieldModel&gt;, optional)

        // --- data section ---
        parsing_utils::Point2D position; ///< Diffuser position in the flow grid (&lt;XYdiff&gt;)
        std::vector<parsing_utils::Point2D>
            ambient_positions; ///< Ambient condition sample points (&lt;XYambient&gt;, zero or more)
        std::optional<parsing_utils::Point2D> intake; ///< Intake location (&lt;XYintake&gt;, optional)
        Discharge discharge;                          ///< Discharge characteristics (&lt;discharge&gt;)
        double nozzle_diameter{};                     ///< Nozzle diameter [m] (&lt;D0&gt;)
        double nozzle_elevation{};                    ///< Height above the bed [m] (&lt;H0&gt;)
        double vertical_angle{};                      ///< Vertical discharge angle [degrees] (&lt;Theta0&gt;)
        double horizontal_angle{}; ///< Horizontal discharge angle, 0=east, 90=north [degrees] (&lt;Sigma0&gt;)
        std::optional<std::string> nf2ff_file; ///< Path to the NF2FF definition file (&lt;NF2FFFile&gt;, optional)

        // --- comm section ---
        std::filesystem::path ff2nf_dir;  ///< Directory for FF2NF communication files (&lt;FF2NFdir&gt;)
        std::filesystem::path ff_run_dir; ///< Far-field model run directory (&lt;FFrundir&gt;)
    };

    /**
     * @brief Reads C-SUMO settings from a configuration XML file.
     *
     * Expected XML format:
     * @include docs/full_settings_example.xml
     *
     * Use @ref fromFile to construct from a path, or @ref fromString to construct
     * directly from XML text (useful in tests).
     */
    class CSumoSettingsReader
    {
    public:
        /**
         * @brief Create by reading and parsing an XML file.
         * @param csumo_config_file Path to the C-SUMO configuration xml file.
         * @return The reader on success, or a @ref ParseError describing the failure.
         */
        [[nodiscard]] static std::expected<CSumoSettingsReader, parsing_utils::ParseError> fromFile(
            const std::filesystem::path& csumo_config_file);

        /**
         * @brief Create by parsing XML from an in-memory string.
         *
         * This overload does not touch the filesystem and is well-suited for
         * unit tests.
         *
         * @param xml Raw UTF-8 XML content.
         * @return The reader on success, or a @ref ParseError describing the failure.
         */
        [[nodiscard]] static std::expected<CSumoSettingsReader, parsing_utils::ParseError> fromString(
            std::string_view xml);

        /**
         * @brief The file format version (value of &lt;fileVersion&gt;).
         */
        [[nodiscard]] std::string_view fileVersion() const;

        /**
         * @brief All diffuser settings blocks read from the XML, in document order.
         */
        [[nodiscard]] const std::vector<DiffuserSettings>& diffusers() const;

    private:
        explicit CSumoSettingsReader(std::string file_version, std::vector<DiffuserSettings> diffusers);

        std::string file_version_;
        std::vector<DiffuserSettings> diffusers_;
    };
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_SETTINGS_READER_HPP
