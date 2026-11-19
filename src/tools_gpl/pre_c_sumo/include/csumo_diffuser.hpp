#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_DIFFUSER_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_DIFFUSER_HPP

#include "csumo_data.hpp"

namespace pre_c_sumo::c_sumo
{
    [[nodiscard]] bool isDiffuserModelled(const NF2FFData& diffuser);

    /**
     * @anchor pre_c_sumo_create_diffuser_model
     * @brief Expand a single NF2FF source into a DESA track using the last sink's position.
     * @pre The diffuser has exactly one source and at least one sink.
     */
    [[nodiscard]] std::vector<SinkOrSourceData> createDiffuserModel(const NF2FFData& diffuser);
} // namespace pre_c_sumo::c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_DIFFUSER_HPP
