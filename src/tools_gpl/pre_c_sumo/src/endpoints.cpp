#include "endpoints.hpp"

namespace pre_c_sumo
{
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
