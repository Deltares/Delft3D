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
    int run(const std::string_view csumoConfigFileName, const std::string_view adapterConfigFileName)
    {
        constexpr int mpiRank = 0;
        constexpr int mpiSize = 1;
        precice::Participant participant{"C-SUMO", adapterConfigFileName, mpiRank, mpiSize};

        while (do_timeloop())
        {
            const auto csumoSettings = read_csumo_config_file(csumoConfigFileName);
            receive_ff_data();
            write_ff2nf_files(csumoSettings);
            wait_for_nf2ff_files(csumoSettings);
            read_nf2ff_files(csumoSettings);
            convert_nf_to_sources_sinks(csumoSettings);
            send_sources_sinks_to_ff(csumoSettings);
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
