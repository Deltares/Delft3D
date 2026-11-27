#include "precice/data.hpp"

#include <cassert>
#include <cstddef>

namespace pre_c_sumo::precice
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

    void SinkSourceConnectionsMesh::setVertexCount(std::size_t vertex_count)
    {
        // Keep one placeholder so the preCICE mesh can be registered when there are no entries.
        const auto registered_vertex_count = vertex_count == 0 ? 1 : vertex_count;
        coordinates.resize(registered_vertex_count * 2, 0.0);
        precice_ids.resize(registered_vertex_count, 0);
    }

} // namespace pre_c_sumo::precice
