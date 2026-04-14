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
    struct WriteError
    {
        std::string message;
    };

    /**
     * @brief Far-field layer at a single horizontal point.
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
     * @brief Far-field state at a single horizontal point (diffuser, intake, or ambient).
     */
    struct FarFieldPoint2D
    {
        parsing_utils::Point2D position;   ///< Horizontal position of the point [m].
        double water_depth{};              ///< Total water depth at this point [m] (written to <waterDepth>).
        std::vector<FarFieldLayer> layers; ///< Per-layer structure (velocity, density, constituents).
    };

    /**
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
    };

    /**
     * @brief Writer for FF2NF XML files.
     */
    class FF2NFWriter
    {
    public:
        explicit FF2NFWriter(FF2NFConfig config);

        /**
         * @brief Generates the FF2NF XML content as a string.
         * @return The generated XML content or a WriteError if validation fails.
         * @note Validation checks that the data is consistent (e.g., constituent counts match,
         *       unique ID length). If validation fails, the returned WriteError contains a message
         *       describing the issue.
         */
        [[nodiscard]] std::expected<std::string, WriteError> generate() const;

        /**
         * @brief Writes the generated FF2NF XML content to a file.
         * @param file_path The path to the output file.
         * @return std::expected containing void on success or WriteError on failure.
         * @note This function first calls generate() to create the XML content. If generation fails, it returns the
         *       WriteError from generate(). If writing to the file system fails, it returns a WriteError with an
         *       appropriate message.
         */
        [[nodiscard]] std::expected<void, WriteError> toFile(const std::filesystem::path& file_path) const;

    private:
        constexpr static std::string_view root_element_name = "COSUMO";
        constexpr static std::string_view file_version = "0.3";
        FF2NFConfig config_;

        [[nodiscard]] std::expected<void, WriteError> validate() const;

        pugi::xml_node createRootElement(pugi::xml_document& document) const;
        void createFileVersionSection(pugi::xml_node& root) const;
        void createCommSection(pugi::xml_node& root) const;
        void createSubgridModelSection(pugi::xml_node& root) const;
    };
} // namespace pre_c_sumo
#endif // SRC_TOOLS_GPL_PRE_C_SUMO_FF2NF_WRITER_HPP
