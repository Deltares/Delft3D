#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CONNECTED_SINKS_SOURCES_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CONNECTED_SINKS_SOURCES_HPP

#include <precice/precice.hpp>
#include <vector>
#include <string>
#include <expected>

namespace pre_c_sumo
{
    /**
     * @brief Error returned when we entounted a runtime problem.
     */
    struct ConnectedSinkSourcesError
    {
        std::string message;
    };

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
         * @param momentum_magnitude_weighted Weighted momentum magnitude.
         * @param momentum_direction Momentum direction [rad].
         */
        void add_entry(double sink_x, double sink_y, double sink_z_bottom, double sink_z_top, double source_x,
                       double source_y, double source_z_bottom, double source_z_top, double discharge,
                       double momentum_magnitude_weighted, double momentum_direction);

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
         * @anchor pre_c_sumo_connected_sink_sources_write_to_precice
         * @brief Writes all accrued data to preCICE for the specified participant and mesh.
         *
         * After writing the data, the stored values are cleared.
         *
         * @param participant preCICE participant used for the write operation.
         * @param mesh_name Name of the mesh on which vertices are registered.
         * @param precice_ids Vertex IDs registered on the provided mesh.
         * @return std::expected containing void on success or pre_C_sumo::ConnectedSinkSourcesError on failure;
         */
        [[nodiscard]] std::expected<void, pre_c_sumo::ConnectedSinkSourcesError> write_to_precice(
            precice::Participant& participant, std::string_view mesh_name, const std::vector<int>& precice_ids);

        /**
         * @anchor pre_c_sumo_connected_sink_sources_get_discharge_value
         * @brief Returns the discharge values in read-only form.
         * @return Vector of discharge values used by unit tests and diagnostics.
         */
        const std::vector<double>& get_discharge_value() const { return discharge_vector; }

    private:
        // attributes
        std::vector<double> sink_x_vector;                      ///< X coordinates of sinks.
        std::vector<double> sink_y_vector;                      ///< Y coordinates of sinks.
        std::vector<double> sink_z_bottom_vector;               ///< Lowest z-boundary of sink extents.
        std::vector<double> sink_z_top_vector;                  ///< Highest z-boundary of sink extents.
        std::vector<double> source_x_vector;                    ///< X coordinates of sources.
        std::vector<double> source_y_vector;                    ///< Y coordinates of sources.
        std::vector<double> source_z_bottom_vector;             ///< Lowest z-boundary of source extents.
        std::vector<double> source_z_top_vector;                ///< Highest z-boundary of source extents.
        std::vector<double> discharge_vector;                   ///< Discharges [m³/s].
        std::vector<double> momentum_magnitude_weighted_vector; ///< Weighted momentum magnitude [kg m/s].
        std::vector<double> momentum_direction_vector;          ///< Momentum direction [rad].
    }; // ConnectedSinksSources
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CONNECTED_SINKS_SOURCES_HPP
