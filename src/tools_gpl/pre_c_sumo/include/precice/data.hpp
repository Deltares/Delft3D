#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_DATA_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_DATA_HPP

#include <array>
#include <cstddef>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace pre_c_sumo::precice
{
    inline constexpr std::string_view water_levels_id = "sea_surface_height";
    inline constexpr std::string_view bed_levels_id = "sea_floor_depth_below_geoid";
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
        std::size_t first_ambient_point_index{};
    };

    /// Interleaved coordinates and per-quantity buffers exchanged with preCICE.
    struct Mesh
    {
        std::string name{};
        std::vector<double> coordinates{};
        std::vector<int> vertex_ids{};
        std::vector<DiffuserMapping> forward_map{};
        std::size_t number_of_nodes{};
        std::size_t number_of_zcoordinates{};
        std::unordered_map<std::string_view, std::vector<double>> quantities{};
    };

    struct ZSpecification
    {
        bool is_2d{};
        double z_min{};
        double z_step{};
        double z_max{};

        std::size_t numberOfDimensions() const;
        std::size_t numberOfZCoordinates() const;
        double zCoordinateAt(std::size_t z_index) const;
    };

    /// Registration buffers for the preCICE sink/source mesh.
    struct SinkSourceConnectionsMesh
    {
        std::vector<double> coordinates{};
        std::vector<int> precice_ids{};
        void setVertexCount(std::size_t vertex_count);
    };
} // namespace pre_c_sumo::precice

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_DATA_HPP
