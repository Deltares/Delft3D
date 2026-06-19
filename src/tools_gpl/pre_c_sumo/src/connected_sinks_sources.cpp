
#include <precice/precice.hpp>
#include <connected_sinks_sources.hpp>

namespace pre_c_sumo
{
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
    void ConnectedSinkSources::add_entry(double sink_x, double sink_y, double sink_z_bottom, double sink_z_top,
                                         double source_x, double source_y, double source_z_bottom, double source_z_top,
                                         double discharge, double momentum_magnitude, double momentum_direction)
    {
        sink_x_vector.push_back(sink_x);
        sink_y_vector.push_back(sink_y);
        sink_z_bottom_vector.push_back(sink_z_bottom);
        sink_z_top_vector.push_back(sink_z_top);
        source_x_vector.push_back(source_x);
        source_y_vector.push_back(source_y);
        source_z_bottom_vector.push_back(source_z_bottom);
        source_z_top_vector.push_back(source_z_top);
        discharge_vector.push_back(discharge);
        momentum_magnitude_vector.push_back(momentum_magnitude);
        momentum_direction_vector.push_back(momentum_direction);
        // TODO: convert moment to sin/cos?
    }

    /**
     * @brief Clear all data from this class instance.
     */
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
        discharge_vector.clear();
        momentum_magnitude_vector.clear();
        momentum_direction_vector.clear();
        momentum_sin_vector.clear();
        momentum_cos_vector.clear();
    }

    /**
     * @brief Get the number of entries stored.
     */
    const std::size_t ConnectedSinkSources::size() { return sink_x_vector.size(); }

    /**
     * @brief Writes all accrued data to preCICE as the specified participant on the specified
     * mesh and accompanying vertices. After writing the data, the accrued data is cleared.
     * @param participant preCICE participant of the connection
     * @param mesh_name Provided mesh name
     * @param precice_ids Vertex ID's registered on the provided mesh.
     */
    void ConnectedSinkSources::write_to_precice(precice::Participant& participant, std::string_view mesh_name,
                                                std::vector<int> precice_ids)
    {
        participant.writeData(mesh_name, "sinks_x", precice_ids, sink_x_vector);
        participant.writeData(mesh_name, "sinks_y", precice_ids, sink_y_vector);
        participant.writeData(mesh_name, "sinks_z_min", precice_ids, sink_z_bottom_vector);
        participant.writeData(mesh_name, "sinks_z_max", precice_ids, sink_z_top_vector);
        participant.writeData(mesh_name, "sources_x", precice_ids, source_x_vector);
        participant.writeData(mesh_name, "sources_y", precice_ids, source_y_vector);
        participant.writeData(mesh_name, "sources_z_min", precice_ids, source_z_bottom_vector);
        participant.writeData(mesh_name, "sources_z_max", precice_ids, source_z_top_vector);
        participant.writeData(mesh_name, "sources_sinks_discharge", precice_ids, discharge_vector);
        // TODO: Momentum.

        // After the write, we can clear the list.
        clear();
    }

} // namespace pre_c_sumo
