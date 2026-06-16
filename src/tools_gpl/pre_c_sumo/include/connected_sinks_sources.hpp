#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CONNECTED_SINKS_SOURCES_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CONNECTED_SINKS_SOURCES_HPP

#include <precice/precice.hpp>
#include <vector>

namespace pre_c_sumo
{
    /**
     * @brief Connected Sinks and Sources administration
     */
    class ConnectedSinkSources
    {
    public:
        // constructor with coordinates etc?

        // add an entry
        void add_entry(double sink_x, double sink_y, double sink_z_bottom, double sink_z_top, double source_x,
                       double source_y, double source_z_bottom, double source_z_top, double q, double u_magnitude,
                       double u_direction);
        // clear all entries
        void clear();
        // send to preCICE
        void write_to_precice(precice::Participant& participant, std::string_view mesh_name,
                              std::vector<int> precice_ids);

    private:
        // attributes
        std::vector<double> sink_x_vector;
        std::vector<double> sink_y_vector;
        std::vector<double> sink_z_bottom_vector;
        std::vector<double> sink_z_top_vector;
        std::vector<double> source_x_vector;
        std::vector<double> source_y_vector;
        std::vector<double> source_z_bottom_vector;
        std::vector<double> source_z_top_vector;
        std::vector<double> q_vector;
        std::vector<double> u_magnitude_vector;
        std::vector<double> u_direction_vector;
        std::vector<double> u_sin_vector; // ??
        std::vector<double> u_cos_vector; // ??

    }; // ConnectedSinksSources
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CONNECTED_SINKS_SOURCES_HPP