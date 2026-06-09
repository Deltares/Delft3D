#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PRE_C_SUMO_LIB_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PRE_C_SUMO_LIB_HPP
#include <expected>

#include <string_view>
#include <assert.h>
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
        std::size_t numberOfDimensions() const
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
        std::size_t numberOfZCoordinates() const
        {
            if (is_2d)
            {
                return 1; // for 2d mesh, we only have one layer of nodes at z=0
            }
            else
            {
                assert(z_step > 0);
                assert(z_max >= z_min);
                return static_cast<std::size_t>((z_max - z_min) / z_step) + 1;
            }
        }

        /**
         * @brief Get the z coordinate at a given layer index.
         */
        double zCoordinateAt(std::size_t z_index) const
        {
            if (is_2d)
            {
                return 0.0; // for 2d mesh, all nodes are at z=0
            }
            else
            {
                return z_min + static_cast<double>(z_index) * z_step;
            }
        }
    };

    struct SourcesSinks
    {
        std::vector<double> coordinates;
        std::vector<int> precice_ids;
        std::vector<double> ids;
        std::vector<double> connected_ids;
        std::vector<double> z_mins;
        std::vector<double> z_maxs;
        std::vector<double> discharges;

        void clear()
        {
            coordinates.clear();
            precice_ids.clear();
            ids.clear();
            connected_ids.clear();
            z_mins.clear();
            z_maxs.clear();
            discharges.clear();
        }

        void clearData()
        {
            ids.clear();
            connected_ids.clear();
            z_mins.clear();
            z_maxs.clear();
            discharges.clear();
        }

        void addCoordinates(const std::vector<double>& new_coordinates)
        {
            coordinates.insert(coordinates.end(), new_coordinates.begin(), new_coordinates.end());
            precice_ids.resize(coordinates.size() / 2); // Assuming 2D coordinates (x, y)
        }

        void addData(const double new_id, const double new_connected_id, const double new_z_min, const double new_z_max,
                     const double new_discharge)
        {
            ids.push_back(new_id);
            connected_ids.push_back(new_connected_id);
            z_mins.push_back(new_z_min);
            z_maxs.push_back(new_z_max);
            discharges.push_back(new_discharge);
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
