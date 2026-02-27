#include <cstdlib>
#include <print>
#include <string_view>

#include "csumo_precice_lib.hpp"

int main(int argc, char** argv)
{
    if (argc != 3)
    {
        std::println("The csumo_precice solver was called with an incorrect number of arguments.");
        std::println("Usage: ./csumo_precice configFile solverName\n");
        std::println("Parameter description");
        std::println("  configFile: Path and filename of preCICE configuration");
        std::println("  solverName: Participant name in preCICE configuration");
        return EXIT_FAILURE;
    }

    const std::string_view configFileName(argv[1]);
    const std::string_view solverName(argv[2]);

    return csumo_precice::csumo_precice(configFileName, solverName);
}
