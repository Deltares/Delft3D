#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_DIFFUSER_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_DIFFUSER_HPP

#include <vector>

#include "csumo/data.hpp"

namespace pre_c_sumo::c_sumo
{
    /**
     * @brief Determines whether NF2FF already represents the diffuser as multiple source points.
     * @param diffuser NF2FF result to inspect.
     * @return True when it has multiple sources or no sinks; false when it has one source and at least one sink.
     */
    [[nodiscard]] bool isDiffuserModelled(const NF2FFData& diffuser);

    /**
     * @anchor pre_c_sumo_create_diffuser_model
     * @brief Expand a single NF2FF source into a DESA track using the last sink's position.
     * @param diffuser NF2FF result containing exactly one source and at least one sink.
     * @return Generated source records distributed across the original source width.
     * @pre The diffuser has exactly one source and at least one sink.
     */
    [[nodiscard]] std::vector<SinkOrSourceData> createDiffuserModel(const NF2FFData& diffuser);
} // namespace pre_c_sumo::c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_DIFFUSER_HPP
