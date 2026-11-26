#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_DATA_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_DATA_HPP

#include <filesystem>
#include <optional>
#include <string>
#include <vector>

namespace pre_c_sumo::c_sumo
{
    /**
     * @anchor pre_c_sumo_point_2d
     * @brief Two-dimensional horizontal position in model coordinates.
     */
    struct Point2D
    {
        double x_coordinate{}; ///< X coordinate [m].
        double y_coordinate{}; ///< Y coordinate [m].
    };

    /**
     * @brief Interpretation of constituent values in a discharge or NF2FF result.
     */
    enum class ConstituentsOperator
    {
        Absolute, ///< Constituent values are absolute concentrations.
        Excess,   ///< Constituent values are excess concentrations relative to ambient water.
    };

    /**
     * @brief Settings for one C-SUMO diffuser, including its locations and FF2NF exchange paths.
     */
    struct DiffuserSettings
    {
        Point2D position{};                       ///< Diffuser discharge location.
        std::vector<Point2D> ambient_positions{}; ///< Locations sampled to describe ambient conditions.
        std::optional<Point2D> intake{};          ///< Optional water intake location.
        std::filesystem::path ff2nf_dir{};        ///< Directory for FF2NF exchange files.
        std::filesystem::path ff_run_dir{};       ///< Far-field model run directory.
        std::string settings_xml{};               ///< Serialized diffuser settings element passed through to FF2NF.
    };

    /**
     * @brief C-SUMO file version and settings for all configured diffusers.
     */
    struct Settings
    {
        std::string file_version{};                ///< Version read from the settings file.
        std::vector<DiffuserSettings> diffusers{}; ///< Configured diffusers in file order.
    };

    /**
     * @brief Far-field data at one vertical layer of a sampled point.
     */
    struct FarFieldLayer
    {
        double z_coordinate{};              ///< Vertical coordinate [m].
        double x_velocity{};                ///< Horizontal velocity [m/s].
        double y_velocity{};                ///< Horizontal velocity [m/s].
        double density{};                   ///< Density [kg/m3].
        std::vector<double> constituents{}; ///< Constituent values in the associated constituent order.
    };

    /**
     * @brief Far-field conditions at one horizontal sampling location.
     */
    struct FarFieldPoint2D
    {
        Point2D position{};                  ///< Horizontal sampling location.
        double water_depth{};                ///< Total water depth [m].
        std::vector<FarFieldLayer> layers{}; ///< Vertical layers at this location.
    };

    /**
     * @brief Complete FF2NF input, including far-field samples and settings XML to pass through.
     */
    struct FF2NFConfig
    {
        std::string ff2nf_filename{};   ///< Path and filename of the FF2NF exchange file.
        std::string wait_for_file{};    ///< Filename written to FF2NF's waitForFile setting.
        std::string ff_run_directory{}; ///< Directory containing the far-field model run.
        std::string run_id{};           ///< Run identifier used to form the far-field model input filename.
        std::string unique_id{};        ///< Optional unique identifier written to FF2NF (at most 6 characters).
        int subgrid_model_nr{};         ///< Numeric identifier of the sub-grid model.
        double current_time_seconds{};  ///< Simulation time represented by these samples [s].
        std::vector<std::string> constituent_names{};  ///< Names corresponding by index to layer constituent values.
        FarFieldPoint2D diffuser{};                    ///< Far-field conditions sampled at the diffuser location.
        std::optional<FarFieldPoint2D> intake{};       ///< Far-field conditions at the intake, when configured.
        std::vector<FarFieldPoint2D> ambient_points{}; ///< Far-field conditions at ambient sampling locations.
        std::string settings_xml{}; ///< Serialized diffuser settings element appended to the FF2NF document.
    };

    /**
     * @brief A sink or source record parsed from an NF2FF result.
     *
     * Optional velocity and weight columns are indicated by @ref has_u and
     * @ref has_weight; fields for omitted columns remain zero.
     */
    struct SinkOrSourceData
    {
        double x_coordinate{};      ///< X coordinate [m].
        double y_coordinate{};      ///< Y coordinate [m].
        double z_coordinate{};      ///< NF2FF vertical coordinate [m].
        double entrainment{};       ///< Entrainment value reported by NF2FF.
        double half_plume_height{}; ///< Half-height of the plume [m].
        double half_plume_width{};  ///< Half-width of the plume [m].
        double u_magnitude{};       ///< Source velocity magnitude [m/s].
        double u_direction{};       ///< Towards bearing, clockwise from north [degrees].
        double weight{};            ///< Optional record weight, when supplied by NF2FF.
        bool has_u{};               ///< Whether the record includes velocity magnitude and direction.
        bool has_weight{};          ///< Whether the record includes a weight.
    };

    /**
     * @brief An intake location parsed from an NF2FF result.
     */
    struct IntakeData
    {
        double x_coordinate{}; ///< X coordinate [m].
        double y_coordinate{}; ///< Y coordinate [m].
        double z_coordinate{}; ///< NF2FF vertical coordinate [m].
        double weight{};       ///< Optional intake weight, when supplied by NF2FF.
        bool has_weight{};     ///< Whether the record includes a weight.
    };

    /**
     * @brief Near-field results parsed from NF2FF, independent of the input XML document.
     */
    struct NF2FFData
    {
        double intake_flow_rate{};                    ///< Intake volume flow rate [m3/s], zero when not provided.
        double source_flow_rate{};                    ///< Source volume flow rate [m3/s].
        ConstituentsOperator constituents_operator{}; ///< Interpretation of @ref constituents values.
        std::vector<double> constituents{};           ///< Discharge constituent values in constituent order.
        std::vector<IntakeData> intakes{};            ///< Intake records reported by NF2FF.
        std::vector<SinkOrSourceData> sources{};      ///< Source records reported by NF2FF.
        std::vector<SinkOrSourceData> sinks{};        ///< Sink records reported by NF2FF.
    };
} // namespace pre_c_sumo::c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_DATA_HPP
