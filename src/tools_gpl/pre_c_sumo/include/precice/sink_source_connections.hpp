#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_SINK_SOURCE_CONNECTIONS_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_SINK_SOURCE_CONNECTIONS_HPP

#include <array>
#include <cstddef>
#include <string>
#include <vector>

#include "precice/data.hpp"

namespace pre_c_sumo::precice
{
    /**
     * @brief Describes a failure while preparing or writing sink/source data.
     */
    struct SinkSourceConnectionsError
    {
        std::string message; ///< Error message.
    };

    /**
     * @brief Attribute-wise sink/source buffers in the preCICE wire format.
     *
     * All vectors are aligned by connection index. The constituent array has one vector
     * for each supported constituent quantity.
     */
    struct SinkSourceConnectionBuffers
    {
        std::vector<double> sink_x_vector;                      ///< Sink x coordinates.
        std::vector<double> sink_y_vector;                      ///< Sink y coordinates.
        std::vector<double> sink_z_bottom_vector;               ///< Lower z-boundaries of sinks.
        std::vector<double> sink_z_top_vector;                  ///< Upper z-boundaries of sinks.
        std::vector<double> source_x_vector;                    ///< Source x coordinates.
        std::vector<double> source_y_vector;                    ///< Source y coordinates.
        std::vector<double> source_z_bottom_vector;             ///< Lower z-boundaries of sources.
        std::vector<double> source_z_top_vector;                ///< Upper z-boundaries of sources.
        std::vector<double> discharge_vector;                   ///< Discharge values [m3/s].
        std::vector<double> momentum_magnitude_weighted_vector; ///< Effective source speeds [m/s].
        std::vector<double> momentum_direction_vector;          ///< Towards bearings, clockwise from north [degrees].
        std::array<std::vector<double>, max_number_of_constituents>
            constituents_vectors; ///< Constituent values, one vector per constituent ID.
    };

    /**
     * @brief Sink/source connections exchanged through preCICE.
     * Each connection has one sink and one source slot; either may represent an unpaired sink or source.
     * preCICE handles communication per attribute, so data is stored as a struct of vectors instead of a vector of
     * structs. The member functions keep all vectors aligned.
     */
    class SinkSourceConnections
    {
    public:
        /**
         * @anchor pre_c_sumo_sink_source_connections_add_entry
         * @brief Adds one connected sink/source entry to the instance.
         * @param sink_x Sink x-coordinate [m].
         * @param sink_y Sink y-coordinate [m].
         * @param sink_z_bottom Lower z-boundary of the sink [m].
         * @param sink_z_top Upper z-boundary of the sink [m].
         * @param source_x Source x-coordinate [m].
         * @param source_y Source y-coordinate [m].
         * @param source_z_bottom Lower z-boundary of the source [m].
         * @param source_z_top Upper z-boundary of the source [m].
         * @param discharge Discharge value [m³/s].
         * @param momentum_magnitude_weighted Unweighted source speed [m/s], used by FM to reconstruct source area.
         * @param momentum_direction Source direction [degrees clockwise from north, pointing towards].
         * @param constituents Constituent values, up to the supported maximum; missing entries are zero-filled.
         */
        void add_entry(double sink_x, double sink_y, double sink_z_bottom, double sink_z_top, double source_x,
                       double source_y, double source_z_bottom, double source_z_top, double discharge,
                       double momentum_magnitude_weighted, double momentum_direction, std::vector<double> constituents);

        /**
         * @anchor pre_c_sumo_sink_source_connections_clear
         * @brief Clears all stored data from the instance.
         */
        void clear();

        /**
         * @anchor pre_c_sumo_sink_source_connections_get_number_of_entries
         * @brief Returns the number of stored sink/source entries.
         * @return Number of entries in the aligned vectors.
         */
        std::size_t get_number_of_entries() const;

        /**
         * @anchor pre_c_sumo_sink_source_connections_get_discharge_value
         * @brief Returns the discharge values in read-only form.
         * @return Discharge values in the aligned vector.
         */
        const std::vector<double>& get_discharge_value() const;

        /**
         * @brief Returns all aligned sink/source buffers in read-only form.
         * @return The attribute-wise buffers owned by this instance.
         */
        const SinkSourceConnectionBuffers& data() const;

    private:
        SinkSourceConnectionBuffers data_;
    };
} // namespace pre_c_sumo::precice

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_SINK_SOURCE_CONNECTIONS_HPP
