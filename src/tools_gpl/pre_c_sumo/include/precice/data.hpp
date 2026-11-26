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
    inline constexpr std::string_view water_levels_id = "sea_surface_height"; ///< preCICE water-level quantity name.
    inline constexpr std::string_view bed_levels_id =
        "sea_floor_depth_below_geoid"; ///< preCICE bed-level quantity name.
    inline constexpr std::string_view densities_id =
        "sea_water_potential_density"; ///< preCICE water-density quantity name.
    inline constexpr std::string_view flow_velocities_id =
        "flow_velocity_3d";                               ///< preCICE three-dimensional flow-velocity quantity name.
    inline constexpr int max_number_of_constituents = 10; ///< Maximum number of constituent quantities supported.
    /// preCICE quantity names for the supported constituent slots, in slot order.
    inline constexpr auto constituent_ids =
        std::to_array<std::string_view>({"C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10"});
    static_assert(constituent_ids.size() == max_number_of_constituents);

    /**
     * @brief Maps one diffuser's sampling points to their positions in a preCICE mesh.
     */
    struct DiffuserMapping
    {
        std::size_t diffuser_index{};            ///< Index of the diffuser sample in the mesh.
        bool has_intake{};                       ///< Whether this diffuser has an intake sample.
        std::size_t intake_index{};              ///< Index of the intake sample when @ref has_intake is true.
        std::size_t first_ambient_point_index{}; ///< Index of the first ambient sample.
    };

    /**
     * @brief Coordinates, mappings, and per-quantity buffers for a preCICE mesh.
     *
     * Coordinates are interleaved by vertex, and quantity values use the same vertex order.
     */
    struct Mesh
    {
        std::string name{};                         ///< Registered preCICE mesh name.
        std::vector<double> coordinates{};          ///< Interleaved x/y(/z) coordinates for each vertex.
        std::vector<int> vertex_ids{};              ///< preCICE vertex IDs in coordinate order.
        std::vector<DiffuserMapping> forward_map{}; ///< Sampling-point indices, one mapping per diffuser.
        std::size_t number_of_nodes{};              ///< Number of mesh vertices.
        std::size_t number_of_zcoordinates{};       ///< Number of vertical samples per horizontal location.
        std::unordered_map<std::string_view, std::vector<double>>
            quantities{}; ///< Input buffers keyed by quantity name.
    };

    /**
     * @brief Specifies whether the mesh is 2D or the vertical coordinates for a 3D mesh.
     */
    struct ZSpecification
    {
        bool is_2d{};    ///< Whether to create a 2D mesh with no vertical coordinates.
        double z_min{};  ///< First vertical coordinate for a 3D mesh.
        double z_step{}; ///< Positive spacing between 3D vertical coordinates.
        double z_max{};  ///< Last vertical coordinate for a 3D mesh.

        /// @return Number of coordinate dimensions: 2 for 2D, otherwise 3.
        std::size_t numberOfDimensions() const;

        /**
         * @brief Returns the number of vertical coordinates in the specification.
         * @return One for 2D; for 3D, the count of z_min + i * z_step values not exceeding z_max.
         * @pre For 3D, z_step must be positive and z_max must be greater than or equal to z_min.
         */
        std::size_t numberOfZCoordinates() const;

        /**
         * @brief Computes a vertical coordinate by its zero-based index.
         * @param z_index Zero-based vertical-coordinate index.
         * @return Zero for 2D, otherwise z_min plus z_index times z_step.
         */
        double zCoordinateAt(std::size_t z_index) const;
    };

    /**
     * @brief Coordinates and vertex IDs used to register the preCICE sink/source mesh.
     */
    struct SinkSourceConnectionsMesh
    {
        std::string name{};                ///< Registered preCICE mesh name.
        std::vector<double> coordinates{}; ///< Interleaved x/y coordinates for each registered vertex.
        std::vector<int> precice_ids{};    ///< preCICE vertex IDs in coordinate order.

        /**
         * @brief Resizes the coordinate and ID buffers for a given number of vertices.
         *
         * One placeholder slot is kept when @p vertex_count is zero so the mesh can still be registered.
         * @param vertex_count Number of actual sink/source vertices.
         */
        void setVertexCount(std::size_t vertex_count);
    };
} // namespace pre_c_sumo::precice

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_DATA_HPP
