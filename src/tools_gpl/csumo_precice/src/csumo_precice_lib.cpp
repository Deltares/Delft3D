#include "csumo_precice_lib.hpp"

#include <precice/precice.hpp>
#include <print>
#include <string_view>
#include <vector>

namespace csumo_precice
{
    /**
     * @details Entry point into the C-SUMO preCICE library.
     * @param csumoConfigFileName Path and filename of C-SUMO configuration xml file.
     * @param preciceConfigFileName Path and filename of preCICE configuration xml file.
     */
    int csumo_precice(const std::string_view csumoConfigFileName, const std::string_view preciceConfigFileName)
    {
        (void)csumoConfigFileName; // Unused parameter, avoid compiler warning
        constexpr int mpiRank = 0;
        constexpr int mpiSize = 1;
        precice::Participant participant{"C-SUMO", preciceConfigFileName, mpiRank, mpiSize};
        return 0;
    }

    /**
     * @details This function prints a greeting message to the console using C++23's std::println.
     */
    int csumo_precice()
    {
        std::println("Hello, world from C-SUMO PreCICE library!");
        return 0;
    }
} // namespace csumo_precice
