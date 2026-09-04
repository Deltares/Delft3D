#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_FF2NF_WRITER_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_FF2NF_WRITER_HPP

#include <expected>
#include <filesystem>
#include <optional>
#include <pugixml.hpp>
#include <string>
#include <string_view>
#include <vector>

#include "parsing_utils.hpp"

namespace pre_c_sumo
{
    /**
     * @anchor pre_c_sumo_write_error
     * @brief Error information returned when FF2NF generation or writing fails.
     */
    struct WriteError
    {
        std::string message;
    };

    /**
     * @anchor pre_c_sumo_far_field_layer
     * @brief Far-field layer state at a single horizontal point.
     */
    struct FarFieldLayer
    {
        double z_coordinate{};            ///< Depth of the layer from the water surface [m].
        double x_velocity{};              ///< X velocity in this layer [m/s].
        double y_velocity{};              ///< Y velocity in this layer [m/s].
        double density{};                 ///< Density at this layer [kg/m³].
        std::vector<double> constituents; ///< Constituent concentrations at this layer.
    };

    /**
     * @anchor pre_c_sumo_far_field_point_2d
     * @brief Far-field state at a single horizontal point (diffuser, intake, or ambient).
     */
    struct FarFieldPoint2D
    {
        parsing_utils::Point2D position;   ///< Horizontal position of the point [m].
        double water_depth{};              ///< Total water depth at this point [m] (written to &lt;waterDepth&gt;).
        std::vector<FarFieldLayer> layers; ///< Per-layer structure (velocity, density, constituents).
    };

    /**
     * @anchor pre_c_sumo_ff2nf_config
     * @brief Configuration for generating an FF2NF XML file.
     */
    struct FF2NFConfig
    {
        std::string ff2nf_filename;
        std::string wait_for_file;
        std::string ff_run_directory;
        std::string run_id;
        std::string unique_id;
        int subgrid_model_nr{};
        double current_time_seconds{};
        std::vector<std::string> constituent_names;
        FarFieldPoint2D diffuser;
        std::optional<FarFieldPoint2D> intake;
        std::vector<FarFieldPoint2D> ambient_points;
        /// Optional copy of the raw &lt;settings&gt; XML node from the C-SUMO configuration file.
        /// When set, the node is copied verbatim as a &lt;settings&gt; child of &lt;COSUMO&gt;
        /// in the generated FF2NF file. The source document must remain alive until generate()
        /// or toFile() has returned.
        pugi::xml_node settings_xml_node;
    };

    /**
     * @anchor pre_c_sumo_ff2nf_writer
     * @brief Writer for FF2NF XML files.
     */
    class FF2NFWriter
    {
    public:
        /**
         * @anchor pre_c_sumo_ff2nf_writer_constructor
         * @brief Constructs a writer from a configuration object.
         * @param config FF2NF content and metadata to be serialized.
         */
        explicit FF2NFWriter(FF2NFConfig config);

        /**
         * @anchor pre_c_sumo_ff2nf_writer_generate
         * @brief Generates the FF2NF XML content as a string.
         * @return Generated XML text or a WriteError when validation fails.
         * @note Validation checks that the data is consistent, for example constituent counts and unique ID length.
         */
        [[nodiscard]] std::expected<std::string, WriteError> generate() const;

        /**
         * @anchor pre_c_sumo_ff2nf_writer_to_file
         * @brief Writes the generated FF2NF XML content to a file.
         * @param file_path Destination path for the output file.
         * @return Empty result on success, or a WriteError on failure.
         * @note The function first calls generate() and returns any validation error from that step.
         */
        [[nodiscard]] std::expected<void, WriteError> toFile(const std::filesystem::path& file_path) const;

    private:
        constexpr static std::string_view root_element_name = "COSUMO";
        constexpr static std::string_view file_version = "0.3";
        FF2NFConfig config_;

        /**
         * @anchor pre_c_sumo_ff2nf_writer_validate
         * @brief Validates the writer configuration before serialization.
         * @return Empty result on success, or a WriteError on invalid data.
         */
        [[nodiscard]] std::expected<void, WriteError> validate() const;

        /**
         * @anchor pre_c_sumo_ff2nf_writer_create_root_element
         * @brief Creates the root `<COSUMO>` XML element.
         * @param document XML document used to own the created root element.
         * @return Root XML node for the generated output.
         */
        pugi::xml_node createRootElement(pugi::xml_document& document) const;

        /**
         * @anchor pre_c_sumo_ff2nf_writer_create_file_version_section
         * @brief Adds the file version section to the XML tree.
         * @param root Root XML node to populate.
         */
        void createFileVersionSection(pugi::xml_node& root) const;

        /**
         * @anchor pre_c_sumo_ff2nf_writer_create_comm_section
         * @brief Adds the communication section to the XML tree.
         * @param root Root XML node to populate.
         */
        void createCommSection(pugi::xml_node& root) const;

        /**
         * @anchor pre_c_sumo_ff2nf_writer_create_subgrid_model_section
         * @brief Adds the subgrid model section to the XML tree.
         * @param root Root XML node to populate.
         */
        void createSubgridModelSection(pugi::xml_node& root) const;

        /**
         * @anchor pre_c_sumo_ff2nf_writer_create_settings_section
         * @brief Adds the settings section to the XML tree.
         * @param root Root XML node to populate.
         */
        void createSettingsSection(pugi::xml_node& root) const;
    };
} // namespace pre_c_sumo
#endif // SRC_TOOLS_GPL_PRE_C_SUMO_FF2NF_WRITER_HPP
