#include "csumo_diffuser.hpp"

#include <cassert>
#include <cmath>
#include <numbers>

namespace pre_c_sumo
{
    bool isDiffuserModelled(const NF2FFData& diffuser)
    {
        // A diffuser is modelled when it has multiple source points or no sinks.
        return diffuser.sources.size() > 1 || diffuser.sinks.size() == 0;
    }

    // Determine the flow nodes over which to distribute the diluted discharge:
    // Both sink and source point needed for direction connection line.
    // Define the line through the source point, perpendicular to the line connecting the
    // last sink point with the source point. Define the line piece on this line, using
    // the specified source-width. Walk with 1000 steps over this line piece
    std::vector<SourceOrSinkData> createDiffuserModel(const NF2FFData& diffuser)
    {
        std::vector<SourceOrSinkData> new_sources;
        constexpr int num_steps = 1000;
        new_sources.reserve(num_steps);

        const auto& sources = diffuser.sources;
        const auto& sinks = diffuser.sinks;

        assert(sources.size() == 1);
        assert(sinks.size() > 0);

        double ang_end = atan2(sources[0].y_coordinate - sinks[sinks.size() - 1].y_coordinate,
                               sources[0].x_coordinate - sinks[sinks.size() - 1].x_coordinate);
        double unit_perpendicular_x = -sin(ang_end);
        double unit_perpendicular_y = cos(ang_end);
        double ds = 2.0 * sources[0].half_plume_width / (num_steps - 1);
        double dx = ds * unit_perpendicular_x;
        double dy = ds * unit_perpendicular_y;
        double x_start = sources[0].x_coordinate + sources[0].half_plume_width * unit_perpendicular_x;
        double y_start = sources[0].y_coordinate + sources[0].half_plume_width * unit_perpendicular_y;

        for (int i = 0; i < num_steps; i++)
        {
            new_sources.emplace_back(SourceOrSinkData{.x_coordinate = x_start - i * dx,
                                                      .y_coordinate = y_start - i * dy,
                                                      .z_coordinate = sources[0].z_coordinate,
                                                      .entrainment = sources[0].entrainment,
                                                      .half_plume_height = sources[0].half_plume_height,
                                                      .half_plume_width = 0,
                                                      .u_magnitude = sources[0].u_magnitude,
                                                      .u_direction = sources[0].u_direction,
                                                      .weight = 1.0 / num_steps,
                                                      .has_u = sources[0].has_u,
                                                      .has_weight = true});
        }

        return new_sources;
    }

} // namespace pre_c_sumo
