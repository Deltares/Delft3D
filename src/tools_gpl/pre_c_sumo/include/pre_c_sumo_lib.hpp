#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PRE_C_SUMO_LIB_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PRE_C_SUMO_LIB_HPP
#include <expected>

#include <string_view>
#include <assert.h>
#include "csumo_settings_reader.hpp"

namespace pre_c_sumo
{
    /**
     * @anchor pre_c_sumo_z_specification
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
         * @anchor pre_c_sumo_z_specification_number_of_dimensions
         * @brief Returns the number of spatial dimensions represented by this specification.
         * @return Number of active model dimensions: 2 for 2D and 3 for 3D.
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
         * @anchor pre_c_sumo_z_specification_number_of_z_coordinates
         * @brief Returns the number of discrete z coordinates or layers in the mesh.
         * @return Number of z levels for the current specification.
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
         * @anchor pre_c_sumo_z_specification_z_coordinate_at
         * @brief Returns the z coordinate for a specific layer index.
         * @param z_index Zero-based layer index.
         * @return The z coordinate for the requested layer.
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

    /**
     * @anchor pre_c_sumo_sources_sinks
     * @brief Stores source/sink coordinates and associated preCICE data for a coupling exchange.
     *
     * The vectors are kept aligned to represent one source/sink entry per index.
     */
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

        /**
         * @anchor pre_c_sumo_sources_sinks_clear
         * @brief Clears all stored source/sink data and registered preCICE IDs.
         */
        void clear()
        {
            coordinates.clear();
            precice_ids.clear();
            clearData();
        }

        /**
         * @anchor pre_c_sumo_sources_sinks_clear_data
         * @brief Clears only the per-entry source/sink arrays while leaving coordinate metadata intact.
         */
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

        /**
         * @anchor pre_c_sumo_sources_sinks_set_coordinates_dimension
         * @brief Resizes the coordinate and ID vectors for a given number of entries.
         * @param dimension Number of source/sink pairs to reserve capacity for.
         */
        void setCoordinatesDimension(const std::size_t dimension)
        {
            coordinates.resize(dimension * 2, 0.0);
            precice_ids.resize(dimension, 0); // Assuming 2D coordinates (x, y)
        }

        /**
         * @anchor pre_c_sumo_sources_sinks_add_data
         * @brief Appends one source/sink pair and its discharge to the stored data.
         * @param new_sink_x Sink x position.
         * @param new_sink_y Sink y position.
         * @param new_sink_z_min Lower sink z-boundary.
         * @param new_sink_z_max Upper sink z-boundary.
         * @param new_source_x Source x position.
         * @param new_source_y Source y position.
         * @param new_source_z_min Lower source z-boundary.
         * @param new_source_z_max Upper source z-boundary.
         * @param new_discharge Discharge value for the pair.
         */
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
     * @anchor pre_c_sumo_run_main
     * @brief Entry point into the C-SUMO preCICE library.
     *
     * Main workflow overview:
     * @dotfile preC_SUMO_Swimlanes.dot
     * @image html preC_SUMO_Swimlanes.svg "preC-SUMO coupling workflow" width=900px
     *
     * The core sequence is: @ref pre_c_sumo_read_settings -> @ref pre_c_sumo_receive_ff_data ->
     * @ref pre_c_sumo_write_ff2nf -> @ref pre_c_sumo_wait_nf2ff -> @ref pre_c_sumo_read_nf2ff ->
     * @ref pre_c_sumo_convert_nf -> @ref pre_c_sumo_connected_sink_sources.
     *
     * @param csumoConfigFileName Path and filename of the C-SUMO configuration XML file.
     * @param adapterConfigFileName Path and filename of the preCICE adapter configuration file.
     * @return Returns 0 on success and a non-zero value on failure.
     */
    int run(std::string_view csumoConfigFileName, std::string_view adapterConfigFileName);

    /**
     * @anchor pre_c_sumo_run_default
     * @brief Legacy entry point used for test scenarios without preCICE integration.
     *
     * This convenience overload is intended for non-production testing workflows.
     *
     * @return Returns 0 on success.
     */
    int run();

} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PRE_C_SUMO_LIB_HPP
