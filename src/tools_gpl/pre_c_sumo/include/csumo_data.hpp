#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_DATA_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_DATA_HPP

#include <filesystem>
#include <optional>
#include <pugixml.hpp>
#include <string>
#include <vector>

namespace pre_c_sumo
{
    /**
     * @anchor pre_c_sumo_point_2d
     * @brief Two-dimensional coordinate pair containing an x/y position.
     */
    struct Point2D
    {
        double x_coordinate{};
        double y_coordinate{};
    };

    enum class ConstituentsOperator
    {
        Absolute,
        Excess,
    };

    struct Discharge
    {
        double flow_rate{}; ///< Volume flow rate [m3/s].
        ConstituentsOperator constituents_operator{};
        std::vector<double> constituents;
    };

    struct DiffuserSettings
    {
        std::optional<std::string> id;
        std::optional<std::string> sub_grid_model;
        std::optional<std::string> far_field_model;
        Point2D position;
        std::vector<Point2D> ambient_positions;
        std::optional<Point2D> intake;
        Discharge discharge;
        double nozzle_diameter{}; ///< Nozzle diameter [m].
        double nozzle_elevation{}; ///< Height above the bed [m].
        double vertical_angle{}; ///< Discharge angle [degrees].
        double horizontal_angle{}; ///< Discharge angle, 0=east, 90=north [degrees].
        std::filesystem::path ff2nf_dir;
        std::filesystem::path ff_run_dir;
        pugi::xml_node settings_xml_node; ///< Borrowed from CSumoSettings::document.
    };

    /// Owns the XML document referenced by the diffuser settings nodes.
    struct CSumoSettings
    {
        std::string file_version;
        std::vector<DiffuserSettings> diffusers;
        pugi::xml_document document;
    };

    struct FarFieldLayer
    {
        double z_coordinate{};
        double x_velocity{}; ///< Horizontal velocity [m/s].
        double y_velocity{}; ///< Horizontal velocity [m/s].
        double density{}; ///< Density [kg/m3].
        std::vector<double> constituents;
    };

    struct FarFieldPoint2D
    {
        Point2D position;
        double water_depth{}; ///< Total water depth [m].
        std::vector<FarFieldLayer> layers;
    };

    /// Complete FF2NF content. The settings node must remain alive until XML generation completes.
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
        pugi::xml_node settings_xml_node;
    };

    struct SourceOrSinkData
    {
        double x_coordinate{};
        double y_coordinate{};
        double z_coordinate{}; ///< NF2FF vertical coordinate [m].
        double entrainment{};
        double half_plume_height{};
        double half_plume_width{};
        double u_magnitude{}; ///< Source velocity magnitude [m/s].
        double u_direction{}; ///< Towards bearing, clockwise from north [degrees].
        double weight{};
        bool has_u{};
        bool has_weight{};
    };

    struct IntakeData
    {
        double x_coordinate{};
        double y_coordinate{};
        double z_coordinate{};
        double weight{};
        bool has_weight{};
    };

    /// Near-field results independent of the XML reader and its input document.
    struct NF2FFData
    {
        std::string file_version{};
        double intake_flow_rate{};
        double source_flow_rate{};
        ConstituentsOperator constituents_operator{};
        std::vector<double> constituents{};
        std::vector<IntakeData> intakes{};
        std::vector<SourceOrSinkData> sources{};
        std::vector<SourceOrSinkData> sinks{};
    };
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_DATA_HPP
