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
        std::vector<double> sinks_x;
        std::vector<double> sinks_y;
        std::vector<double> sinks_z_min;
        std::vector<double> sinks_z_max;
        std::vector<double> sources_x;
        std::vector<double> sources_y;
        std::vector<double> sources_z_min;
        std::vector<double> sources_z_max;
        std::vector<double> discharges;

        void clear()
        {
            coordinates.clear();
            precice_ids.clear();
            sinks_x.clear();
            sinks_y.clear();
            sinks_z_min.clear();
            sinks_z_max.clear();
            sources_x.clear();
            sources_y.clear();
            sources_z_min.clear();
            sources_z_max.clear();
            discharges.clear();
        }

        void clearData()
        {
            sinks_x.clear();
            sinks_y.clear();
            sinks_z_min.clear();
            sinks_z_max.clear();
            sources_x.clear();
            sources_y.clear();
            sources_z_min.clear();
            sources_z_max.clear();
            discharges.clear();
        }

        void setCoordinatesDimension(const int dimension)
        {
            coordinates.resize(dimension * 2, 0.0);
            precice_ids.resize(dimension, 0); // Assuming 2D coordinates (x, y)
        }

        void addData(const double new_sink_x, const double new_sink_y, const double new_sink_z_min,
                     const double new_sink_z_max, const double new_source_x, const double new_source_y,
                     const double new_source_z_min, const double new_source_z_max, const double new_discharge)
        {
            sinks_x.push_back(new_sink_x);
            sinks_y.push_back(new_sink_y);
            sinks_z_min.push_back(new_sink_z_min);
            sinks_z_max.push_back(new_sink_z_max);
            sources_x.push_back(new_source_x);
            sources_y.push_back(new_source_y);
            sources_z_min.push_back(new_source_z_min);
            sources_z_max.push_back(new_source_z_max);
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
