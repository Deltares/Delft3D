
#include <precice/precice.hpp>
#include <connected_sinks_sources.hpp>

#include <format>
#include <print>
#include <stdexcept>

namespace pre_c_sumo
{
    void ConnectedSinkSources::add_entry(double sink_x, double sink_y, double sink_z_bottom, double sink_z_top,
                                         double source_x, double source_y, double source_z_bottom, double source_z_top,
                                         double discharge, double momentum_magnitude_weighted,
                                         double momentum_direction, std::vector<double> constituents)
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
        momentum_magnitude_weighted_vector.push_back(momentum_magnitude_weighted);
        momentum_direction_vector.push_back(momentum_direction);
        for (std::size_t constituent_index = 0; constituent_index < max_number_of_consituents; constituent_index++)
        {
            if (constituent_index < constituents.size())
            {
                constituents_vectors[constituent_index].push_back(constituents[constituent_index]);
            }
            else
            {
                constituents_vectors[constituent_index].push_back(0.0);
            }
        }
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
        momentum_magnitude_weighted_vector.clear();
        momentum_direction_vector.clear();
        for (int constituent_index = 0; constituent_index < max_number_of_consituents; constituent_index++)
        {
            constituents_vectors[constituent_index].clear();
        }
    }

    /**
     * @brief Get the number of entries stored.
     */
    std::size_t ConnectedSinkSources::get_number_of_entries() const { return sink_x_vector.size(); }

    /**
     * @brief Writes all accrued data to preCICE as the specified participant on the specified
     * mesh and accompanying vertices. After writing the data, the accrued data is cleared.
     * @param participant preCICE participant of the connection
     * @param mesh_name Provided mesh name
     * @param precice_ids Vertex ID's registered on the provided mesh.
     */
    std::expected<void, pre_c_sumo::ConnectedSinkSourcesError> ConnectedSinkSources::write_to_precice(
        precice::Participant& participant, std::string_view mesh_name, const std::vector<int>& precice_ids)
    {
        const std::size_t registered_vertex_count = precice_ids.size();
        const std::size_t entry_count = get_number_of_entries();

        if (registered_vertex_count == 0)
        {
            return std::unexpected(
                pre_c_sumo::ConnectedSinkSourcesError{"Cannot write sources/sinks to an empty preCICE mesh."});
        }

        if (entry_count != 0 && entry_count != registered_vertex_count)
        {
            return std::unexpected(pre_c_sumo::ConnectedSinkSourcesError{
                std::format("Connected source/sink count changed from the registered preCICE mesh size {} to {}. "
                            "Remeshing is not implemented.",
                            registered_vertex_count, entry_count)});
        }

        const std::vector<double> zero_values(registered_vertex_count, 0.0);

        // preCICE expects values for all registered vertices each step.
        // When no entries are present, write explicit zeros with fixed mesh length.
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
        write_or_zero("sources_momentum_magnitude_weighted", momentum_magnitude_weighted_vector);
        write_or_zero("sources_momentum_direction", momentum_direction_vector);
        write_or_zero("C01", constituents_vectors[0]);
        write_or_zero("C02", constituents_vectors[1]);
        write_or_zero("C03", constituents_vectors[2]);
        write_or_zero("C04", constituents_vectors[3]);
        write_or_zero("C05", constituents_vectors[4]);
        write_or_zero("C06", constituents_vectors[5]);
        write_or_zero("C07", constituents_vectors[6]);
        write_or_zero("C08", constituents_vectors[7]);
        write_or_zero("C09", constituents_vectors[8]);
        write_or_zero("C10", constituents_vectors[9]);

        // After the write, we can clear the list.
        clear();
        return {};
    }

} // namespace pre_c_sumo
