#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_MESH_LAYOUT_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_MESH_LAYOUT_HPP

#include <vector>

namespace pre_c_sumo
{
    /**
     * @brief Mapping from C-SUMO diffuser roles to flat preCICE 3D buffer indices.
     *
     * Built once by @ref build2DMeshPointsFromSettings and used by
     * @ref writeFF2NFFiles to decode 3D buffer data back into per-role
     * `FarFieldPoint2D` structures without re-counting points.
     *
     * Points are stored in the same order they appear in the flat preCICE
     * coordinate/data buffers: for each diffuser in settings order —
     * diffuser position, optional intake, then ambients in document order.
     */
    struct CsumoMeshLayout
    {
        enum class PointRole
        {
            Diffuser, ///< Diffuser outlet position.
            Intake,   ///< Intake position (optional per diffuser).
            Ambient,  ///< Ambient background point.
        };

        struct PointInfo
        {
            int flat_index;     ///< Index into flat preCICE 3D coordinate/data buffers.
            int diffuser_index; ///< Zero-based index of the parent diffuser in C-SUMO settings.
            PointRole role;     ///< Role of this coupling point.
            int ambient_index;  ///< Zero-based ambient index within diffuser (-1 for non-ambient).
        };

        std::vector<PointInfo> points; ///< All coupling points in flat buffer order.
    };
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_MESH_LAYOUT_HPP
