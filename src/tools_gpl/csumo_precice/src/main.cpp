#include "csumo_precice_lib.hpp"
#include <iostream>
#include <cstdlib>

int main(int argc, char** argv) 
{
    if (argc != 3) {
        std::cout << "The csumo_precice solver was called with an incorrect number of arguments.\n";
        std::cout << "Usage: ./csumo_precice configFile solverName\n\n";
        std::cout << "Parameter description\n";
        std::cout << "  configFile: Path and filename of preCICE configuration\n";
        std::cout << "  solverName: Participant name in preCICE configuration\n";
        return EXIT_FAILURE;
    }

    std::string configFileName(argv[1]);
    std::string solverName(argv[2]);

    return csumo_precice::csumo_precice(configFileName, solverName);
}
