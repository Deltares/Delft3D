#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_STATE_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_STATE_HPP

#include <memory>
#include <string_view>
#include <vector>

namespace precice
{
    class Participant;
}

namespace pre_c_sumo
{
    struct PreCICEState
    {
        std::unique_ptr<precice::Participant> participant;
        constexpr static std::string_view csumo_2d_nodes_name = "csumo_2d_nodes";
        constexpr static std::string_view water_depths_name = "water_depths";
        std::vector<int> csumo_2d_nodes_ids;

        ~PreCICEState();
    };
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_STATE_HPP
