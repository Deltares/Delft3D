#include "precice_data.hpp"

#include <cassert>

namespace pre_c_sumo
{
    std::size_t ZSpecification::numberOfDimensions() const { return is_2d ? 2 : 3; }

    std::size_t ZSpecification::numberOfZCoordinates() const
    {
        if (is_2d)
        {
            return 1;
        }
        assert(z_step > 0);
        assert(z_max >= z_min);
        return static_cast<std::size_t>((z_max - z_min) / z_step) + 1;
    }

    double ZSpecification::zCoordinateAt(std::size_t z_index) const
    {
        return is_2d ? 0.0 : z_min + static_cast<double>(z_index) * z_step;
    }

    void SourcesSinks::clear()
    {
        coordinates.clear();
        precice_ids.clear();
        clearData();
    }

    void SourcesSinks::clearData()
    {
        sinks_x.clear();
        sinks_y.clear();
        sinks_z_min.clear();
        sinks_z_max.clear();
        sources_x.clear();
        sources_y.clear();
        sources_z_min.clear();
        sources_z_max.clear();
        discharges.clear();
    }

    void SourcesSinks::setCoordinatesDimension(std::size_t dimension)
    {
        coordinates.resize(dimension * 2, 0.0);
        precice_ids.resize(dimension, 0);
    }

    void SourcesSinks::addData(double new_sink_x, double new_sink_y, double new_sink_z_min, double new_sink_z_max,
                               double new_source_x, double new_source_y, double new_source_z_min,
                               double new_source_z_max, double new_discharge)
    {
        sinks_x.push_back(new_sink_x);
        sinks_y.push_back(new_sink_y);
        sinks_z_min.push_back(new_sink_z_min);
        sinks_z_max.push_back(new_sink_z_max);
        sources_x.push_back(new_source_x);
        sources_y.push_back(new_source_y);
        sources_z_min.push_back(new_source_z_min);
        sources_z_max.push_back(new_source_z_max);
        discharges.push_back(new_discharge);
    }
} // namespace pre_c_sumo
