#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PRE_C_SUMO_LIB_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PRE_C_SUMO_LIB_HPP
#include <expected>

#include <string_view>
#include "csumo_settings_reader.hpp"

namespace pre_c_sumo
{
    /**
     * @struct ZSpecification
     * @brief Describes whether the mesh is 2D or 3D and provides z-coordinate helpers.
     *
     * When `is_2d` is true the mesh is treated as a single layer at z == 0. For 3D
     * meshes the z range is defined by `z_min`, `z_max` and `z_step`.
     */
    struct ZSpecification
    {
        bool is_2d;
        double z_min;
        double z_step;
        double z_max;

        /**
         * @brief Return the number of spatial dimensions represented by this spec.
         */
        int numberOfDimensions() const
        {
            if (is_2d)
            {
                return 2;
            }
            else
            {
                return 3;
            }
        }

        /**
         * @brief Return the number of discrete z coordinates / layers.
         */
        int numberOfZCoordinates() const
        {
            if (is_2d)
            {
                return 1; // for 2d mesh, we only have one layer of nodes at z=0
            }
            else
            {
                return static_cast<int>((z_max - z_min) / z_step) + 1;
            }
        }

        /**
         * @brief Get the z coordinate at a given layer index.
         */
        double zCoordinateAt(int z_index) const
        {
            if (is_2d)
            {
                return 0.0; // for 2d mesh, all nodes are at z=0
            }
            else
            {
                return z_min + z_index * z_step;
            }
        }
    };

    /**
     * @brief Entry point into the C-SUMO preCICE library.
     *
     * @param csumoConfigFileName Path and filename of C-SUMO configuration XML file.
     * @param adapterConfigFileName Path and filename of preCICE adapter configuration file.
     * @return int Returns 0 on success, non-zero on failure.
     */
    int run(std::string_view csumoConfigFileName, std::string_view adapterConfigFileName);

    /**
     * @brief Legacy entry point for testing without preCICE. Not intended for production use.
     *
     * @return int Returns 0 on success.
     */
    int run();

} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PRE_C_SUMO_LIB_HPP
