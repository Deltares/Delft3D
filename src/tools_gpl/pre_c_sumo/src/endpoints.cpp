#include "endpoints.hpp"

namespace pre_c_sumo
{
    Endpoint makeEndpoint(const int id, const int connected_id, const double coordinate_x, const double coordinate_y,
                          const double vertical_boundary_lower, const double vertical_boundary_upper,
                          const double discharge)
    {
        return Endpoint{.id = id,
                        .connected_id = connected_id,
                        .coordinate_x = coordinate_x,
                        .coordinate_y = coordinate_y,
                        .vertical_boundary_lower = vertical_boundary_lower,
                        .vertical_boundary_upper = vertical_boundary_upper,
                        .discharge = discharge};
    }

    bool addMomentum(Source& source, const Momentum& momentum)
    {
        if (source.endpoint.discharge < 0.0)
        {
            return false;
        }

        source.momentum = momentum;
        return true;
    }

    bool addConstituents(Source& source, const Constituents& constituents)
    {
        if (source.endpoint.discharge < 0.0)
        {
            return false;
        }

        source.constituents = constituents;
        return true;
    }
} // namespace pre_c_sumo
