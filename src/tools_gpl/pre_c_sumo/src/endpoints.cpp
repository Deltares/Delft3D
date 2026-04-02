#include "endpoints.hpp"

namespace pre_c_sumo
{
    void makeEndpoint(
        Endpoint& endpoint,
        int id,
        double coordinate_x,
        double coordinate_y,
        double vertical_boundary_lower,
        double vertical_boundary_upper,
        double discharge,
        int connected_id)
    {
        endpoint.id = id;
        endpoint.connected_id = connected_id;
        endpoint.coordinate_x = coordinate_x;
        endpoint.coordinate_y = coordinate_y;
        endpoint.vertical_boundary_lower = vertical_boundary_lower;
        endpoint.vertical_boundary_upper = vertical_boundary_upper;
        endpoint.discharge = discharge;
    }

    Momentum makeMomentum(double velocity_magnitude, double velocity_direction_deg)
    {
        return Momentum{velocity_magnitude, velocity_direction_deg};
    }

    Constituents makeConstituents(
        double temperature,
        double salinity,
        const std::array<double, constituent_count>& additional_constituents)
    {
        return Constituents{temperature, salinity, additional_constituents};
    }

    bool addMomentum(Source& source, const Momentum& momentum)
    {
        if (source.discharge < 0.0)
        {
            return false;
        }

        source.momentum = momentum;
        return true;
    }

    bool addConstituents(Source& source, const Constituents& constituents)
    {
        if (source.discharge < 0.0)
        {
            return false;
        }

        source.constituents = constituents;
        return true;
    }
} // namespace pre_c_sumo
