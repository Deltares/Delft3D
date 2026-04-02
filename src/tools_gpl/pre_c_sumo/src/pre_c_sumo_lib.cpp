#include "pre_c_sumo_lib.hpp"

#include <precice/precice.hpp>
#include <print>
#include <string_view>
#include <vector>

#include "csumo_settings_reader.hpp"
#include "coupling_steps.hpp"

namespace pre_c_sumo
{
    /**
     * @details Entry point into the C-SUMO preCICE library.
     */
    int run(const std::string_view csumo_config_file_name, const std::string_view adapter_config_file_name)
    {
        constexpr int mpi_rank = 0;
        constexpr int mpi_size = 1;
        precice::Participant participant{"C-SUMO", adapter_config_file_name, mpi_rank, mpi_size};

        while (doTimeloop())
        {
            const auto csumo_settings = readCsumoConfigFile(csumo_config_file_name);
            receiveFFData();
            writeFF2NFFiles(csumo_settings.value());
            waitForNF2FFFiles(csumo_settings.value());
            readNF2FFFiles(csumo_settings.value());
            convertNFToSourcesSinks(csumo_settings.value());
            sendSourcesSinksToFF(csumo_settings.value());
        }
        return 0;
    }

    /**
     * @details This function prints a greeting message to the console using C++23's std::println.
     */
    int run()
    {
        std::println("Hello, world from C-SUMO PreCICE library!");
        return 0;
    }

} // namespace pre_c_sumo
