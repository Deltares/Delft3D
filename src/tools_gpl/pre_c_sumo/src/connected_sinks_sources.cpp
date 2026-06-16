
#include <connected_sinks_sources.hpp>

namespace pre_c_sumo
{
    void ConnectedSinkSources::add_entry(double sink_x, double sink_y, double sink_z_bottom, double sink_z_top,
                                         double source_x, double source_y, double source_z_bottom, double source_z_top,
                                         double q, double u_magnitude, double u_direction)
    {
        sink_x_vector.push_back(sink_x);
        sink_y_vector.push_back(sink_y);
        sink_z_bottom_vector.push_back(sink_z_bottom);
        sink_z_top_vector.push_back(sink_z_top);
        source_x_vector.push_back(source_x);
        source_y_vector.push_back(source_y);
        source_z_bottom_vector.push_back(source_z_bottom);
        source_z_top_vector.push_back(source_z_top);
        q_vector.push_back(q);
        u_magnitude_vector.push_back(u_magnitude);
        u_direction_vector.push_back(u_direction);
        // ?? sin/cos
    };

    void ConnectedSinkSources::clear()
    {
        sink_x_vector.clear();
        sink_y_vector.clear();
        sink_z_bottom_vector.clear();
        sink_z_top_vector.clear();
        source_x_vector.clear();
        source_y_vector.clear();
        source_z_bottom_vector.clear();
        source_z_top_vector.clear();
        q_vector.clear();
        u_magnitude_vector.clear();
        u_direction_vector.clear();
        u_sin_vector.clear();
        u_cos_vector.clear();
    };

    void ConnectedSinkSources::write_to_precice() {
        // TODO: Implement preCICE writes
    };

} // namespace pre_c_sumo
