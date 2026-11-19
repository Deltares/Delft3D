#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CONNECTED_SINKS_SOURCES_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CONNECTED_SINKS_SOURCES_HPP

#include "precice_data.hpp"

namespace pre_c_sumo
{
    /**
     * @brief Connected Sinks and Sources administration
     * This class collects the sinks and sources data that is sent through preCICE.
     * In the context of preC-SUMO, these connected sinks and sources are handled by sink and/or source
     * pairs. preCICE handles communications per attribute. This is why data is stored here as a struct of vectors
     * instead of a vector of structs. The member functions assure that all vectors have consistent lengths.
     */
    class ConnectedSinkSources
    {
    public:
        /**
         * @anchor pre_c_sumo_connected_sink_sources_add_entry
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
         * @anchor pre_c_sumo_connected_sink_sources_clear
         * @brief Clears all stored data from the instance.
         */
        void clear();

        /**
         * @anchor pre_c_sumo_connected_sink_sources_get_number_of_entries
         * @brief Returns the number of stored source/sink entries.
         * @return Number of entries in the aligned vectors.
         */
        std::size_t get_number_of_entries() const;

        /**
         * @anchor pre_c_sumo_connected_sink_sources_get_discharge_value
         * @brief Returns the discharge values in read-only form.
         * @return Discharge values in the aligned vector.
         */
        const std::vector<double>& get_discharge_value() const;
        const ConnectedSinkSourcesData& data() const;

    private:
        ConnectedSinkSourcesData data_;
    }; // ConnectedSinksSources
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CONNECTED_SINKS_SOURCES_HPP
