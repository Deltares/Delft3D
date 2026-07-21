#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CONNECTED_SINKS_SOURCES_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CONNECTED_SINKS_SOURCES_HPP

#include <precice/precice.hpp>
#include <vector>

namespace pre_c_sumo
{
    /**
     * @brief Connected Sinks and Sources administration
     * This class collects the sinks and sources data that is to be sent trough preCICE.
     * In the context of preC-SUMO, these connected sinks and sources are handled by sink and/or source
     * preCICE handles communications per attribute. This is why data is stored here as a struct of vectors
     * instead of a vector of structs. The member functions assure that all vectors have consistent lengths.
     */
    class ConnectedSinkSources
    {
    public:
        /**
         * @brief Adds a connected sink and source entry to this instance.
         * @param double sink_x Sink X coordinates
         * @param double sink_y Sink Y coordninate
         * @param double sink_z_top Sink Z extent highest point
         * @param double sink_z_bottom Sink Z extent lowest point
         * @param double source_x Source X coordinate
         * @param double source_y Source Y coordinate
         * @param double source_z_top Source Z extent highest point
         * @param double source_z_bottom Source Z extent lowest point
         * @param double discharge Discharge value
         * @param double momentum_magnitude Momentum magnitude value
         * @param double momentum_direction Momentum direction
         */
        void add_entry(double sink_x, double sink_y, double sink_z_bottom, double sink_z_top, double source_x,
                       double source_y, double source_z_bottom, double source_z_top, double discharge,
                       double momentum_magnitude, double momentum_direction);

        /**
         * @brief Clear all data from this class instance.
         */
        void clear();

        /**
         * @brief Get the number of entries stored.
         */
        std::size_t size();

        /**
         * @brief Writes all accrued data to preCICE as the specified participant on the specified
         * mesh and accompanying vertices. After writing the data, the accrued data is cleared.
         * @param participant preCICE participant of the connection
         * @param mesh_name Provided mesh name
         * @param precice_ids Vertex ID's registered on the provided mesh.
         */
        void write_to_precice(precice::Participant& participant, std::string_view mesh_name,
                              std::vector<int> precice_ids);

    private:
        // attributes
        std::vector<double> sink_x_vector;             //< X coordinates of sinks
        std::vector<double> sink_y_vector;             //< Y coordinates of sinks
        std::vector<double> sink_z_bottom_vector;      //< Lowest Z coordinate of sink extents
        std::vector<double> sink_z_top_vector;         //< Hightest Z coodinate of sink extents
        std::vector<double> source_x_vector;           //< X coordinates of sources
        std::vector<double> source_y_vector;           //< Y coordinates of sources
        std::vector<double> source_z_bottom_vector;    //< Lowest Z coordinate of source extents
        std::vector<double> source_z_top_vector;       //< Highest Z coordinate of source extents
        std::vector<double> discharge_vector;          //< Discharges [m^3/s]
        std::vector<double> momentum_magnitude_vector; //< Momentum magnitude [kg m/s]
        std::vector<double> momentum_direction_vector; //< Momentum direction [rad]
    }; // ConnectedSinksSources
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CONNECTED_SINKS_SOURCES_HPP
