#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_SINK_SOURCE_CONNECTIONS_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_SINK_SOURCE_CONNECTIONS_HPP

#include <array>
#include <cstddef>
#include <string>
#include <vector>

#include "precice/data.hpp"

namespace pre_c_sumo::precice
{
    struct SinkSourceConnectionsError
    {
        std::string message;
    };

    /// Attribute-wise sink/source buffers in the preCICE wire format.
    struct SinkSourceConnectionBuffers
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
         * @param sink_x Sink x-coordinate.
         * @param sink_y Sink y-coordinate.
         * @param sink_z_bottom Lower z-boundary of the sink.
         * @param sink_z_top Upper z-boundary of the sink.
         * @param source_x Source x-coordinate.
         * @param source_y Source y-coordinate.
         * @param source_z_bottom Lower z-boundary of the source.
         * @param source_z_top Upper z-boundary of the source.
         * @param discharge Discharge value [m³/s].
         * @param momentum_magnitude_weighted Effective source speed [m/s], used by FM to reconstruct source area.
         * @param momentum_direction Source direction [degrees clockwise from north, pointing towards].
         * @param constituents Constituents (maximum 10)
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
        const SinkSourceConnectionBuffers& data() const;

    private:
        SinkSourceConnectionBuffers data_;
    };
} // namespace pre_c_sumo::precice

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_SINK_SOURCE_CONNECTIONS_HPP
