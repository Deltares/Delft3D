#ifndef SRC_TOOLS_GPL_CSUMO_PRECICE_CSUMO_SETTINGS_READER_HPP
#define SRC_TOOLS_GPL_CSUMO_PRECICE_CSUMO_SETTINGS_READER_HPP

#include <expected>
#include <filesystem>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace csumo_precice
{
    /**
     * @brief Error returned when a csumo settings XML cannot be parsed.
     */
    struct ParseError
    {
            std::string message;
    };

    /**
     * @brief A 2-D coordinate pair (x, y).
     */
    struct Point2D
    {
            double x{};
            double y{};
    };

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
            Point2D position; ///< Diffuser position in the flow grid (&lt;XYdiff&gt;)
            std::vector<Point2D>
                ambient_positions;     ///< Ambient condition sample points (&lt;XYambient&gt;, zero or more)
            Point2D intake;            ///< Intake location (&lt;XYintake&gt;)
            Discharge discharge;       ///< Discharge characteristics (&lt;discharge&gt;)
            double nozzle_diameter{};  ///< Nozzle diameter [m] (&lt;D0&gt;)
            double nozzle_elevation{}; ///< Height above the bed [m] (&lt;H0&gt;)
            double vertical_angle{};   ///< Vertical discharge angle [degrees] (&lt;Theta0&gt;)
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
     * @code{.xml}
     * <?xml version="1.0" encoding="utf-8"?>
     * <COSUMO>
     *   <fileVersion>0.3</fileVersion>
     *   <settings>
     *     <general>
     *       <ID>Diffusor_1</ID>
     *       <subGridModel>fixedNFSolution</subGridModel>
     *       <farFieldModel>Delft3D</farFieldModel>
     *     </general>
     *     <comm>
     *       <FF2NFdir>FF2NF\</FF2NFdir>
     *       <FFrundir>rundir</FFrundir>
     *     </comm>
     *     <data>
     *       <XYdiff>550.0 350.0</XYdiff>
     *       <XYambient>823.0 344.8</XYambient>
     *       <XYintake>567.0 350.0</XYintake>
     *       <discharge><M3s>10.0</M3s><constituents>10.0 0.0 0.0</constituents></discharge>
     *       <D0>0.5</D0><H0>3.2</H0><Theta0>15.0</Theta0><Sigma0>180</Sigma0>
     *     </data>
     *   </settings>
     * </COSUMO>
     * @endcode
     *
     * Use @ref fromFile to construct from a path, or @ref fromXml to construct
     * directly from XML text (useful in tests).
     */
    class CSumoSettingsReader
    {
        public:
            /**
             * @brief Create by reading and parsing an XML file.
             * @param csumoConfigFile Path to the C-SUMO configuration xml file.
             * @return The reader on success, or a @ref ParseError describing the failure.
             */
            [[nodiscard]] static std::expected<CSumoSettingsReader, ParseError> fromFile(
                const std::filesystem::path& csumoConfigFile);

            /**
             * @brief Create by parsing XML from an in-memory string.
             *
             * This overload does not touch the filesystem and is well-suited for
             * unit tests.
             *
             * @param xml Raw UTF-8 XML content.
             * @return The reader on success, or a @ref ParseError describing the failure.
             */
            [[nodiscard]] static std::expected<CSumoSettingsReader, ParseError> fromXml(std::string_view xml);

            /**
             * @brief The file format version (value of &lt;fileVersion&gt;).
             */
            [[nodiscard]] std::string_view fileVersion() const noexcept;

            /**
             * @brief All diffuser settings blocks read from the XML, in document order.
             */
            [[nodiscard]] const std::vector<DiffuserSettings>& diffusers() const noexcept;

        private:
            explicit CSumoSettingsReader(std::string file_version, std::vector<DiffuserSettings> diffusers);

            std::string file_version_;
            std::vector<DiffuserSettings> diffusers_;
    };
} // namespace csumo_precice

#endif // SRC_TOOLS_GPL_CSUMO_PRECICE_CSUMO_SETTINGS_READER_HPP
