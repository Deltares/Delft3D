
#include <precice/precice.hpp>
#include <connected_sinks_sources.hpp>

#include <format>
#include <print>
#include <stdexcept>

namespace pre_c_sumo
{
    /**
     * @brief Adds a connected sink and source entry to this instance.
        * @param sink_x Sink X coordinate.
        * @param sink_y Sink Y coordinate.
        * @param sink_z_bottom Sink lower Z extent.
        * @param sink_z_top Sink upper Z extent.
        * @param source_x Source X coordinate.
        * @param source_y Source Y coordinate.
        * @param source_z_bottom Source lower Z extent.
        * @param source_z_top Source upper Z extent.
        * @param discharge Discharge value.
        * @param momentum_magnitude Momentum magnitude value.
        * @param momentum_direction Momentum direction.
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
    }

    /**
     * @brief Get the number of entries stored.
     */
    std::size_t ConnectedSinkSources::size() const { return sink_x_vector.size(); }

    /**
     * @brief Writes all accrued data to preCICE as the specified participant on the specified
     * mesh and accompanying vertices. After writing the data, the accrued data is cleared.
     * @param participant preCICE participant of the connection
     * @param mesh_name Provided mesh name
     * @param precice_ids Vertex ID's registered on the provided mesh.
     */
    void ConnectedSinkSources::write_to_precice(precice::Participant& participant, std::string_view mesh_name,
                                                const std::vector<int>& precice_ids)
    {
        const std::size_t registered_vertex_count = precice_ids.size();
        const std::size_t entry_count = size();

        if (registered_vertex_count == 0)
        {
            throw std::runtime_error("Cannot write sources/sinks to an empty preCICE mesh.");
        }

        if (entry_count != 0 && entry_count != registered_vertex_count)
        {
            throw std::runtime_error(std::format(
                "Connected source/sink count changed from the registered preCICE mesh size {} to {}. "
                "Remeshing is not implemented.",
                registered_vertex_count, entry_count));
        }

        const std::vector<double> zero_values(registered_vertex_count, 0.0);

        auto write_or_zero = [&](std::string_view data_name, const std::vector<double>& values) {
            if (values.empty())
            {
                participant.writeData(mesh_name, data_name, precice_ids, zero_values);
                return;
            }

            participant.writeData(mesh_name, data_name, precice_ids, values);
        };

        write_or_zero("sinks_x", sink_x_vector);
        write_or_zero("sinks_y", sink_y_vector);
        write_or_zero("sinks_z_min", sink_z_bottom_vector);
        write_or_zero("sinks_z_max", sink_z_top_vector);
        write_or_zero("sources_x", source_x_vector);
        write_or_zero("sources_y", source_y_vector);
        write_or_zero("sources_z_min", source_z_bottom_vector);
        write_or_zero("sources_z_max", source_z_top_vector);
        write_or_zero("sources_sinks_discharge", discharge_vector);
        write_or_zero("sources_momentum_magnitude", momentum_magnitude_vector);
        write_or_zero("sources_momentum_direction", momentum_direction_vector);
        // TODO: Send Momentum.

        // After the write, we can clear the list.
        clear();
    }

} // namespace pre_c_sumo
