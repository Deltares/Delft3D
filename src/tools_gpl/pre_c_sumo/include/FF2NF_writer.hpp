#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_FF2NF_WRITER_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_FF2NF_WRITER_HPP

#include <expected>
#include <optional>
#include <pugixml.hpp>
#include <string>
#include <string_view>
#include <vector>

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
        double depth_from_surface{}; ///< Depth of the layer from the water surface [m].
        double x_velocity{};         ///< X velocity in this layer [m/s].
        double y_velocity{};         ///< Y velocity in this layer [m/s].
    };

    /**
     * @brief Far-field state at a single horizontal point (diffuser, intake, or ambient).
     *
     * @note density and constituents are depth-averaged scalars (per node, not per layer);
     *       the writer repeats them for every layer in the output.
     */
    struct FarFieldPoint2D
    {
        double x{};                        ///< X coordinate of the point [m].
        double y{};                        ///< Y coordinate of the point [m].
        double water_depth{};              ///< Total water depth at this point [m] (written to <waterDepth>).
        double density{};                  ///< Depth-averaged density [kg/m³] (repeated for every layer in <rho>).
        std::vector<double> constituents;  ///< Depth-averaged constituent concentrations (repeated per layer).
        std::vector<FarFieldLayer> layers; ///< Layered velocity structure at this point.
    };

    /**
     * @brief Writer for FF2NF XML files.
     */
    class FF2NFWriter
    {
    public:
        [[nodiscard]] std::expected<std::string, WriteError> generate() const;

        // --- Setters: all must be called; validate() (invoked by generate()) checks for omissions ---

        FF2NFWriter& setFF2NFFilename(std::string_view filename);
        FF2NFWriter& setWaitForFile(std::string_view filename);
        FF2NFWriter& setFFRunDirectory(std::string_view run_directory);
        FF2NFWriter& setRunId(std::string_view run_id);
        FF2NFWriter& setUniqueId(std::string_view unique_id);
        FF2NFWriter& setSubgridModelNumber(int number);
        FF2NFWriter& setCurrentTimeSeconds(double seconds);
        FF2NFWriter& setConstituentNames(const std::vector<std::string>& names);
        FF2NFWriter& setDiffusers(const std::vector<FarFieldPoint2D>& diffusers);
        FF2NFWriter& setIntakes(const std::vector<FarFieldPoint2D>& intakes);
        FF2NFWriter& setAmbientPoints(const std::vector<FarFieldPoint2D>& ambient_points);

    private:
        constexpr static std::string_view root_element_name = "COSUMO";
        constexpr static std::string_view file_version = "0.3";
        std::string ff2nf_filename_;
        std::string wait_for_file_;
        std::string ff_run_directory_;
        std::string run_id_;
        std::optional<std::string> unique_id_; // Empty string is considered a valid unique ID
        std::optional<int> subgrid_model_nr_;
        std::optional<double> current_time_seconds_;
        std::vector<std::string> constituent_names_;
        std::vector<FarFieldPoint2D> diffusers_;
        std::vector<FarFieldPoint2D> intakes_;
        std::vector<FarFieldPoint2D> ambient_points_;

        /// Returns an error if any setter was not called.
        [[nodiscard]] std::expected<void, WriteError> validate() const;

        pugi::xml_node createRootElement(pugi::xml_document& document) const;
        void createFileVersionSection(pugi::xml_node& root) const;
        void createCommSection(pugi::xml_node& root) const;
        void createSubgridModelSection(pugi::xml_node& root) const;
    };
} // namespace pre_c_sumo
#endif // SRC_TOOLS_GPL_PRE_C_SUMO_FF2NF_WRITER_HPP
