#ifndef SRC_TOOLS_GPL_preC-SUMO_preC-SUMO_LIB_HPP
#define SRC_TOOLS_GPL_preC-SUMO_preC-SUMO_LIB_HPP

#include <string_view>

namespace preC-SUMO
{
    /**
     * @brief Entry point into the C-SUMO preCICE library.
     *
     * @param csumoConfigFileName Path and filename of C-SUMO configuration xml file.
     * @param adapterConfigFileName Path and filename of preCICE adapter configuration file.
     * @return int Return 0 on success, non-zero on failure
     */
    int run(std::string_view csumoConfigFileName, std::string_view adapterConfigFileName);

    /**
     * @brief Legacy entry point for testing without preCICE. Not intended for production use.
     *
     * @return int Return 0 on success
     */
    int run();
} // namespace preC-SUMO

#endif // SRC_TOOLS_GPL_preC-SUMO_preC-SUMO_LIB_HPP
