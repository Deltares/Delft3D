#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_DATA_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_DATA_HPP

#include <array>
#include <cstddef>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace pre_c_sumo
{
    inline constexpr std::string_view water_levels_id = "sea_surface_height";
    inline constexpr std::string_view bed_levels_id = "sea_floor_depth_below_geoid";
    inline constexpr std::string_view water_depth_id = "sea_floor_depth_below_sea_surface";
    inline constexpr std::string_view densities_id = "sea_water_potential_density";
    inline constexpr std::string_view flow_velocities_id = "flow_velocity_3d";
    inline constexpr int max_number_of_constituents = 10;
    inline constexpr auto constituent_ids =
        std::to_array<std::string_view>({"C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10"});
    static_assert(constituent_ids.size() == max_number_of_constituents);

    struct DiffuserMapping
    {
        std::size_t diffuser_index{};
        bool has_intake{};
        std::size_t intake_index{};
        std::size_t number_of_ambient_points{};
        std::size_t first_ambient_point_index{};
    };

    /// Interleaved coordinates and per-quantity buffers exchanged with preCICE.
    struct PreciceMesh
    {
        std::string name;
        std::vector<double> coordinates;
        std::vector<int> vertex_ids;
        std::vector<DiffuserMapping> forward_map;
        std::size_t number_of_nodes{};
        std::size_t number_of_zcoordinates{};
        std::unordered_map<std::string_view, std::vector<double>> quantities;
    };

    struct ZSpecification
    {
        bool is_2d;
        double z_min;
        double z_step;
        double z_max;

        std::size_t numberOfDimensions() const;
        std::size_t numberOfZCoordinates() const;
        double zCoordinateAt(std::size_t z_index) const;
    };

    /// Registration buffers for the preCICE source/sink mesh.
    struct SourcesSinks
    {
        std::vector<double> coordinates;
        std::vector<int> precice_ids;
        std::vector<double> sinks_x;
        std::vector<double> sinks_y;
        std::vector<double> sinks_z_min;
        std::vector<double> sinks_z_max;
        std::vector<double> sources_x;
        std::vector<double> sources_y;
        std::vector<double> sources_z_min;
        std::vector<double> sources_z_max;
        std::vector<double> discharges;

        void clear();
        void clearData();
        void setVertexCount(std::size_t vertex_count);
        void addData(double new_sink_x, double new_sink_y, double new_sink_z_min, double new_sink_z_max,
                     double new_source_x, double new_source_y, double new_source_z_min, double new_source_z_max,
                     double new_discharge);
    };

    struct ConnectedSinkSourcesError
    {
        std::string message;
    };

    /// Attribute-wise source/sink buffers in the preCICE wire format.
    struct ConnectedSinkSourcesData
    {
        std::vector<double> sink_x_vector;
        std::vector<double> sink_y_vector;
        std::vector<double> sink_z_bottom_vector;
        std::vector<double> sink_z_top_vector;
        std::vector<double> source_x_vector;
        std::vector<double> source_y_vector;
        std::vector<double> source_z_bottom_vector;
        std::vector<double> source_z_top_vector;
        std::vector<double> discharge_vector;
        std::vector<double> momentum_magnitude_weighted_vector;
        std::vector<double> momentum_direction_vector;
        std::array<std::vector<double>, max_number_of_constituents> constituents_vectors;
    };
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_DATA_HPP
