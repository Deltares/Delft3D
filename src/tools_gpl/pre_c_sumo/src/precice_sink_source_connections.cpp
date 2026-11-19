#include "precice_sink_source_connections.hpp"

namespace pre_c_sumo::precice
{
    void SinkSourceConnections::add_entry(double sink_x, double sink_y, double sink_z_bottom, double sink_z_top,
                                          double source_x, double source_y, double source_z_bottom, double source_z_top,
                                          double discharge, double momentum_magnitude_weighted,
                                          double momentum_direction, std::vector<double> constituents)
    {
        data_.sink_x_vector.push_back(sink_x);
        data_.sink_y_vector.push_back(sink_y);
        data_.sink_z_bottom_vector.push_back(sink_z_bottom);
        data_.sink_z_top_vector.push_back(sink_z_top);
        data_.source_x_vector.push_back(source_x);
        data_.source_y_vector.push_back(source_y);
        data_.source_z_bottom_vector.push_back(source_z_bottom);
        data_.source_z_top_vector.push_back(source_z_top);
        data_.discharge_vector.push_back(discharge);
        data_.momentum_magnitude_weighted_vector.push_back(momentum_magnitude_weighted);
        data_.momentum_direction_vector.push_back(momentum_direction);
        for (std::size_t constituent_index = 0; constituent_index < max_number_of_constituents; constituent_index++)
        {
            if (constituent_index < constituents.size())
            {
                data_.constituents_vectors[constituent_index].push_back(constituents[constituent_index]);
            }
            else
            {
                data_.constituents_vectors[constituent_index].push_back(0.0);
            }
        }
    }

    void SinkSourceConnections::clear()
    {
        data_.sink_x_vector.clear();
        data_.sink_y_vector.clear();
        data_.sink_z_bottom_vector.clear();
        data_.sink_z_top_vector.clear();
        data_.source_x_vector.clear();
        data_.source_y_vector.clear();
        data_.source_z_bottom_vector.clear();
        data_.source_z_top_vector.clear();
        data_.discharge_vector.clear();
        data_.momentum_magnitude_weighted_vector.clear();
        data_.momentum_direction_vector.clear();
        for (int constituent_index = 0; constituent_index < max_number_of_constituents; constituent_index++)
        {
            data_.constituents_vectors[constituent_index].clear();
        }
    }

    std::size_t SinkSourceConnections::get_number_of_entries() const { return data_.sink_x_vector.size(); }

    const std::vector<double>& SinkSourceConnections::get_discharge_value() const { return data_.discharge_vector; }

    const SinkSourceConnectionBuffers& SinkSourceConnections::data() const { return data_; }
} // namespace pre_c_sumo::precice
