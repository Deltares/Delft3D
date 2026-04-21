#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_STATE_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_STATE_HPP

#include <memory>
#include <string_view>
#include <vector>

namespace precice
{
    class Participant;
}

namespace pre_c_sumo
{
    /**
     * @brief Runtime coupling state shared across preC-SUMO preCICE steps.
     *
     * Stores participant handle, mesh/data names, mesh vertex IDs and
     * coordinate/data buffers for both 2D and 3D coupling meshes.
     */
    struct PreCICEState
    {
        std::unique_ptr<precice::Participant> participant; ///< preCICE participant instance.

        constexpr static std::string_view csumo_2d_nodes_name = "csumo_2d_nodes"; ///< 2D mesh name in preCICE config.
        constexpr static std::string_view csumo_3d_nodes_name = "csumo_3d_nodes"; ///< 3D mesh name in preCICE config.
        constexpr static std::string_view bed_level_name = "bed_level_z";          ///< 2D scalar data name for bed level.
        constexpr static std::string_view water_level_name = "water_level_z";      ///< 2D scalar data name for water level.
        constexpr static std::string_view rho_name = "rho";                        ///< 3D scalar data name for density.

        std::vector<double> base_2d_coordinates; ///< Flat 2D XY coordinates [x0,y0,x1,y1,...].
        std::vector<int> csumo_2d_nodes_ids;     ///< Vertex IDs returned by setMeshVertices for 2D mesh.
        std::vector<double> bed_level_z;         ///< 2D bed level values per 2D vertex.
        std::vector<double> water_level_z;       ///< 2D water level values per 2D vertex.

        std::vector<double> csumo_3d_coordinates; ///< Flat 3D XYZ coordinates [x0,y0,z0,x1,y1,z1,...].
        std::vector<int> csumo_3d_node_ids;       ///< Vertex IDs returned by setMeshVertices for 3D mesh.
        std::vector<double> rho_3d;               ///< 3D density values per 3D vertex.
        int current_3d_layer_count{};             ///< Number of z-layers currently represented per 2D point.

        ~PreCICEState();
    };
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_STATE_HPP
